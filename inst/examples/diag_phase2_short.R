# Can phase 2 undo its own moves? The phase 3 test, run against phase 2.
#
# diag_phase3_short.R settled phase 3: seventeen of seventeen generators undone
# at depth 1, everything solved out to words of twenty-five generators, and a
# prune table cutting nine nodes in ten by the time the search is deep. Its
# only failures were `exhausted` -- out of budget on solutions past twenty
# moves -- never `no_solution`. So phase 3 is sound, and what remains is what
# it is handed.
#
# Same construction here. The solved cube is at phase 2's goal, phase 2's own
# twenty-eight generators take it away, and the inverse word is a solution of
# at most that length. A failure is therefore the phase failing, not a scramble
# it had no answer to.
#
# The readings differ from phase 3's in one way worth stating up front. Phase 2
# has no standalone entry point -- there is cube_kociemba4_phase3_cpp but no
# phase2 equivalent -- so it is run through cube_kociemba4_phase12_cpp, which
# gives phase 3 no depth and returns the moves phases 1 and 2 contributed. The
# test of success is then cube_at_phase_goal_cpp(state, 2) on the result rather
# than a `found` flag, and there is no node count or cut_ratio to read. What it
# can still answer is the question that matters: does phase 2 reach its goal,
# and how long a word does it need.

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

# Phase 2's generators, from phase2_gens4() in src/kociemba4.h. Every face by
# quarters, plus the inner layers: x and y by halves only, z freely. The z axis
# is F/B, which phase 1 built and phase 2 must not disturb -- so the inner z
# turns quarter, the others only half.
gens <- list(
  "U"  = c("U"),  "U'" = c("U'"), "U2" = c("U","U"),
  "D"  = c("D"),  "D'" = c("D'"), "D2" = c("D","D"),
  "L"  = c("L"),  "L'" = c("L'"), "L2" = c("L","L"),
  "R"  = c("R"),  "R'" = c("R'"), "R2" = c("R","R"),
  "F"  = c("F"),  "F'" = c("F'"), "F2" = c("F","F"),
  "B"  = c("B"),  "B'" = c("B'"), "B2" = c("B","B"),
  "1x2" = c("1x","1x"), "2x2" = c("2x","2x"),
  "1y2" = c("1y","1y"), "2y2" = c("2y","2y"),
  "1z"  = c("1z"), "1z'" = c("1z'"), "1z2" = c("1z","1z"),
  "2z"  = c("2z"), "2z'" = c("2z'"), "2z2" = c("2z","2z"))

budget <- 5e6

# Run phases 1 and 2 and report what phase 2 achieved. `path` is everything the
# two phases contributed together, so the length is not phase 2's alone -- the
# figure to compare across rows, not against the scramble length.
solve2 <- function(state, max_depth2 = 12L) {
  p <- try(cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 2L,
                                                max_depth2 = max_depth2,
                                                node_budget = budget),
           silent = TRUE)
  if (inherits(p, "try-error")) {
    return(list(ok = FALSE, n = NA_integer_, err = TRUE, g1 = NA, g2 = NA))
  }
  out <- replay(state, p)
  list(ok = cayleyR:::cube_at_phase_goal_cpp(out, 2L),
       g1 = cayleyR:::cube_at_phase_goal_cpp(out, 1L),
       n = length(p), err = FALSE, state = out)
}

# ---- Is the solved cube already at the goal? -------------------------------
hr("the solved cube itself")
cat(sprintf("  at phase 1 goal : %s\n",
            cayleyR:::cube_at_phase_goal_cpp(solved, 1L)))
cat(sprintf("  at phase 2 goal : %s\n",
            cayleyR:::cube_at_phase_goal_cpp(solved, 2L)))
r0 <- solve2(solved)
cat(sprintf("  phases 1+2 on it: %d moves, goal %s\n", r0$n, r0$ok))

# ---- One generator away ----------------------------------------------------
hr("one move of phase 2's own generators")

cat(sprintf("  %-5s %-8s %6s  %s\n", "move", "result", "moves", "p1 goal"))
rows <- list()
for (nm in names(gens)) {
  st <- replay(solved, gens[[nm]])
  # Whether one generator even leaves the goal: a move the coordinate cannot
  # see is solved in zero moves, exactly as fourteen of phase 3's seventeen
  # were. That is not a fault, but it changes what the row means.
  moved <- !cayleyR:::cube_at_phase_goal_cpp(st, 2L)
  r  <- solve2(st)
  cat(sprintf("  %-5s %-8s %6s  %-5s %s\n", nm,
              if (r$err) "ERROR" else if (isTRUE(r$ok)) "solved" else "FAILED",
              if (is.na(r$n)) "-" else r$n, r$g1,
              if (!moved) "(already at goal)" else ""))
  rows[[length(rows) + 1L]] <- data.frame(move = nm, ok = isTRUE(r$ok),
                                          n = r$n, moved = moved,
                                          stringsAsFactors = FALSE)
}
t1 <- do.call(rbind, rows)
cat(sprintf("\n  solved : %d of %d   (%d of them actually left the goal)\n",
            sum(t1$ok, na.rm = TRUE), nrow(t1), sum(t1$moved)))
if (any(!t1$ok)) {
  cat(sprintf("  FAILED : %s\n", paste(t1$move[!t1$ok], collapse = ", ")))
  cat("  A generator phase 2 cannot undo at depth 1 is the defect, and names\n")
  cat("  itself. The inner-layer entries are the ones to look at first: which\n")
  cat("  layer belongs to which face does not follow from the letter, and that\n")
  cat("  spelling has been wrong in this file before.\n")
}

# ---- Deeper ----------------------------------------------------------------
hr("deeper, on phase 2's own generators")

set.seed(2026)
n_rep <- 8L
deep <- list()

cat(sprintf("  %-4s %8s %8s %9s  %s\n", "k", "solved", "longest", "mean len",
            "phase 1 also at goal"))

for (k in c(2:7, 10L, 15L, 20L, 25L)) {
  ok <- 0L; worst <- NA_integer_; lens <- numeric(0); g1ok <- 0L
  bad <- character(0)

  for (rep in seq_len(n_rep)) {
    w  <- sample(names(gens), k, replace = TRUE)
    st <- replay(solved, unlist(gens[w], use.names = FALSE))
    r  <- solve2(st, max_depth2 = 14L)

    lens <- c(lens, r$n)
    if (isTRUE(r$g1)) g1ok <- g1ok + 1L
    if (isTRUE(r$ok)) {
      ok <- ok + 1L
      worst <- max(worst, r$n, na.rm = TRUE)
    } else {
      bad <- c(bad, paste(w, collapse = " "))
    }
  }

  cat(sprintf("  %-4d %8s %8s %9.1f  %d/%d\n", k, sprintf("%d/%d", ok, n_rep),
              if (is.na(worst)) "-" else as.character(worst),
              mean(lens, na.rm = TRUE), g1ok, n_rep))

  if (length(bad) > 0) {
    cat("       failed on:\n")
    for (b in bad) cat("         ", b, "\n", sep = "")
  }

  deep[[length(deep) + 1L]] <- data.frame(
    k = k, solved = ok, of = n_rep, longest = worst,
    mean_len = mean(lens, na.rm = TRUE), stringsAsFactors = FALSE)
}

hr("what this says")
cat("  Every scramble is built from phase 2's own generators, so a solution\n")
cat("  of at most that length exists in each case. Anything not solved is a\n")
cat("  phase that cannot undo its own moves.\n")
cat("\n")
cat("  If phase 2 solves everything here, it is sound in the same sense phase\n")
cat("  3 is, and neither phase is individually at fault. What would remain is\n")
cat("  the handover: phase 2 reaching its own goal by a route that leaves the\n")
cat("  cube far away in phase 3's metric. That is a different measurement --\n")
cat("  the true phase 3 distance of a real phase 2 output -- and this test\n")
cat("  does not make it.\n")
