# Can phase 3 undo one move of its own?
#
# The question that prompted this: phase 3 fails on scrambles of two or three
# moves. If it does, no amount of prune table or node budget is the story --
# at that depth a search with no heuristic at all finds the answer in a
# fraction of a second. Something about the goal or the coordinate is wrong.
#
# The test is built so that a solution is guaranteed to exist and its length is
# known in advance. Scramble the solved cube with k moves drawn from phase 3's
# OWN generators -- <Uw2, U, L, Fw2, F2, Rw2, R, B2, D>, seventeen in the
# metric -- and the inverse word is a k-move solution by construction. Phase 3
# searching that set cannot be barred from finding one at depth k or less.
#
# So the readings mean:
#
#   solved at <= k        the phase works at this depth
#   solved but longer     it works; the scramble was reducible
#   not solved at all     the goal or the coordinate is wrong, not the budget.
#                         Depth 1 failing is the sharpest form: one generator
#                         away from a goal and unable to see it.
#
# A move that fails at k = 1 names the generator to look at. The set has been
# wrong here before -- the wide half turns are spelled as words, and which
# inner layer belongs to which face does not follow from the letter (Uw takes
# layer 2, Dw layer 1); writing them by hand once put layer 1 under Uw2 and
# Fw2, so the phase searched a set other than the one it documents.

suppressMessages(library(cayleyR))

N  <- 4L
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

solved <- seq_len(96L)

# Phase 3's generators, as words in the package's move names. Copied from
# phase3_gens4() in src/kociemba4.h rather than retyped from the block
# notation, because that spelling is exactly what was wrong before.
gens <- list(
  "U"   = c("U"), "U'" = c("U'"), "U2" = c("U", "U"),
  "D"   = c("D"), "D'" = c("D'"), "D2" = c("D", "D"),
  "L"   = c("L"), "L'" = c("L'"), "L2" = c("L", "L"),
  "R"   = c("R"), "R'" = c("R'"), "R2" = c("R", "R"),
  "F2"  = c("F", "F"), "B2" = c("B", "B"),
  "Uw2" = c("U", "2y", "U", "2y"),
  "Rw2" = c("R", "2x", "R", "2x"),
  "Fw2" = c("F", "2z", "F", "2z"))

budget <- 5e6

# `found` is the only honest test of success. On failure the phase still
# returns a path -- the branch that came closest, from best_names -- so
# length(path) > 0 says nothing about whether it solved anything.
solve3 <- function(state, max_depth = 8L) {
  r <- try(cayleyR:::cube_kociemba4_phase3_cpp(state, max_depth3 = max_depth,
                                               node_budget = budget),
           silent = TRUE)
  if (inherits(r, "try-error")) {
    return(list(ok = FALSE, n = NA_integer_, nodes = NA_real_, err = TRUE,
                outcome = "error", cut = NA_real_))
  }
  list(ok = isTRUE(r$found), n = length(r$path), nodes = r$nodes, err = FALSE,
       outcome = r$outcome, cut = r$cut_ratio, raw = r)
}

# ---- Is the solved cube already at the goal? -------------------------------
#
# If this is not zero moves, nothing below means anything.
hr("the solved cube itself")
r0 <- solve3(solved)
str(r0$raw)

# ---- One generator away ----------------------------------------------------
hr("one move of phase 3's own generators")

cat(sprintf("  %-5s %-8s %6s %12s  %-12s %s\n",
            "move", "result", "moves", "nodes", "outcome", "cut_ratio"))
rows <- list()
for (nm in names(gens)) {
  st <- replay(solved, gens[[nm]])
  r  <- solve3(st)
  cat(sprintf("  %-5s %-8s %6s %12s  %-12s %.3f\n", nm,
              if (r$err) "ERROR" else if (r$ok) "solved" else "FAILED",
              if (is.na(r$n)) "-" else r$n,
              if (is.na(r$nodes)) "-" else
                format(r$nodes, big.mark = ",", scientific = FALSE),
              r$outcome, r$cut))
  rows[[length(rows) + 1L]] <- data.frame(move = nm, ok = r$ok, n = r$n,
                                          outcome = r$outcome,
                                          stringsAsFactors = FALSE)
}
t1 <- do.call(rbind, rows)
cat(sprintf("\n  solved : %d of %d\n", sum(t1$ok, na.rm = TRUE), nrow(t1)))
if (any(!t1$ok)) {
  cat(sprintf("  FAILED : %s\n",
              paste(t1$move[!t1$ok], collapse = ", ")))
  cat("  A generator that cannot be undone at depth 1 is the defect. Either\n")
  cat("  the move is not in the set the search uses, or the coordinate does\n")
  cat("  not see the difference it makes.\n")
}

# ---- Deeper, one level at a time -------------------------------------------
#
# Where the cost starts to run away, and whether the prune table is doing
# anything by the time it does.
#
# Two readings from the k = 1 pass shape what to expect here. Fourteen of the
# seventeen generators -- every outer turn -- were solved in zero moves and one
# node: they carry one goal to another, so the coordinate does not see them at
# all. Only Uw2, Rw2 and Fw2 move the phase, and each cost four moves to undo
# rather than one. So k drawn from the whole set is not a depth: a word of k
# moves may be worth anything from nothing to 4k, and the useful figure is not
# the count but the node curve against it.
#
# cut_ratio is the column to watch. Zero means the table was consulted and
# pruned at nothing -- and at these depths fill_to = limit/2 finally puts real
# levels in it, so a zero here is the heuristic failing where it should work,
# not the trivial zero of a depth-1 search.
hr("deeper, on phase 3's own generators")

set.seed(2026)

n_rep <- 8L
deep <- list()

cat(sprintf("  %-4s %8s %8s %10s %12s %10s  %s\n",
            "k", "solved", "longest", "mean nodes", "max nodes",
            "cut_ratio", "outcomes"))

for (k in c(2:7, 10L, 15L, 20L, 25L)) {
  ok <- 0L; worst <- NA_integer_; bad <- character(0)
  nodes <- numeric(0); cuts <- numeric(0); outs <- character(0)

  for (rep in seq_len(n_rep)) {
    w  <- sample(names(gens), k, replace = TRUE)
    st <- replay(solved, unlist(gens[w], use.names = FALSE))
    # Deep enough that a failure means the search could not do it, rather than
    # that it was not allowed to look. A word of 25 generators can land further
    # out than twelve moves even though the same word undone is a solution of
    # at most 25, and a ceiling hit reads as a defect if it is not ruled out.
    r  <- solve3(st, max_depth = 20L)

    nodes <- c(nodes, r$nodes)
    cuts  <- c(cuts, r$cut)
    outs  <- c(outs, r$outcome)

    if (isTRUE(r$ok)) {
      ok <- ok + 1L
      worst <- max(worst, r$n, na.rm = TRUE)
    } else {
      bad <- c(bad, paste(w, collapse = " "))
    }
  }

  cat(sprintf("  %-4d %8s %8s %10s %12s %10.3f  %s\n",
              k, sprintf("%d/%d", ok, n_rep),
              if (is.na(worst)) "-" else as.character(worst),
              format(round(mean(nodes)), big.mark = ",", scientific = FALSE),
              format(max(nodes), big.mark = ",", scientific = FALSE),
              mean(cuts),
              paste(names(table(outs)), table(outs), sep = "=", collapse = " ")))

  if (length(bad) > 0) {
    cat("       failed on:\n")
    for (b in bad) cat("         ", b, "\n", sep = "")
  }

  deep[[length(deep) + 1L]] <- data.frame(
    k = k, solved = ok, of = n_rep,
    longest = worst, mean_nodes = mean(nodes), max_nodes = max(nodes),
    cut_ratio = mean(cuts), stringsAsFactors = FALSE)
}

dt <- do.call(rbind, deep)

hr("the node curve")

# The ratio between consecutive levels is the branching factor the search is
# actually paying. With a heuristic that works it should stay well under the
# seventeen generators; at seventeen the table is buying nothing.
cat(sprintf("  %-4s %12s  %s\n", "k", "mean nodes", "x previous"))
for (i in seq_len(nrow(dt))) {
  cat(sprintf("  %-4d %12s  %s\n", dt$k[i],
              format(round(dt$mean_nodes[i]), big.mark = ",",
                     scientific = FALSE),
              if (i == 1) "-" else
                sprintf("%.1f", dt$mean_nodes[i] / dt$mean_nodes[i - 1])))
}

hr("what this says")
cat("  Every scramble above is built from phase 3's own generators, so a\n")
cat("  solution of at most that many moves exists in every case. Anything\n")
cat("  not solved is a phase that cannot undo its own moves -- a goal or\n")
cat("  coordinate fault, which no budget or prune table will fix.\n")
cat("\n")
cat("  If everything solves and the node curve stays flat, phase 3 is sound\n")
cat("  and the trouble in the full reduction is what phases 1 and 2 hand it,\n")
cat("  not the phase itself. If the curve multiplies by about seventeen per\n")
cat("  level while cut_ratio sits at zero, the prune table is not pruning and\n")
cat("  that is the thing to fix.\n")
