# Can a cube say, before the search, which orientation to search in?
#
# The measurement that prompted this. One cube of a prefilled batch, budget 5e7,
# four orientations tried in the order the cascade tries them:
#
#     (-)   exhausted  50,000,001 nodes   40.5 s
#     1y    exhausted  50,000,001 nodes   38.7 s
#     1x    found      39,469,964 nodes   30.8 s
#     1z    (never reached)
#
# 95 of that cube's 127 seconds went on proving that two orientations were not
# the one. The answer was in the third, and nothing about the first two said so
# until the budget was gone. If some property of the handed-over state -- one
# that costs microseconds rather than forty seconds -- ranks 1x above the other
# three, then that whole 95 seconds is avoidable and the fix is a sort.
#
# So this script does not search. For every cube and every orientation it runs
# phases 1 and 2 (a fifth of a second for all four), reads every cheap property
# of the state phase 3 would have been handed, and then -- once -- runs phase 3
# to find out what the truth was. The question is whether any column predicts
# the last one.
#
# What "predicts" has to mean here is worth stating before any number appears,
# because a weaker reading of it would let this script claim success on noise.
# The cascade needs a ranking, not a correlation: it will try orientations in
# the order some column sorts them, and it stops at the first that works. So the
# test is whether the best orientation by that column is a good one to search --
# measured as how often the column's top pick is the cheapest of the four, and
# how much the column's ordering costs against the best possible ordering. A
# column that correlates well on average but puts a hopeless orientation first
# on one cube in four has not solved the problem.
#
# Usage:
#   Rscript inst/examples/diag_orientation_predictors.R [n_cubes] [budget]

suppressMessages(library(cayleyR))

args <- commandArgs(trailingOnly = TRUE)
n_cubes <- if (length(args) >= 1) as.integer(args[[1]]) else 6L
budget <- if (length(args) >= 2) as.numeric(args[[2]]) else 5e7
scramble_len <- 20L
orientations <- c("", "1y", "1x", "1z")
table_dir <- "/mnt/Data2/DS_projects/phase3"

hr <- function(t) cat(sprintf("\n== %s %s\n", t,
                              strrep("-", max(0, 58 - nchar(t)))))
fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

moves <- cube_moves(4)
names(moves) <- cube_move_names(4)
replay <- function(s, p) { for (m in p) s <- s[moves[[m]]]; s }

# ---- Is the installed package the one this script needs? --------------------
#
# `centre_dist` was added to cube_phase3_coord_cpp for this script. Without it
# the probe below hands data.frame() a NULL, and what comes back is "arguments
# imply differing number of rows: 1, 0" -- an error that says nothing about the
# actual problem, which is a .so older than the source. Checked once, here,
# where it can be said plainly.
local({
  probe0 <- cayleyR:::cube_phase3_coord_cpp(cube_identity(4))
  missing <- setdiff(c("centre_dist", "prune_bound"), names(probe0))
  if (length(missing)) {
    stop("the installed cayleyR is missing ", paste(missing, collapse = ", "),
         " from cube_phase3_coord_cpp -- rebuild and reinstall the package ",
         "before running this script", call. = FALSE)
  }
})

hr("setup")
cat(sprintf("cubes        : %d, scrambled %d moves\n", n_cubes, scramble_len))
cat(sprintf("budget       : %s nodes per orientation\n", fmt(budget)))
cat(sprintf("orientations : %s\n",
            paste(ifelse(nzchar(orientations), orientations, "(-)"),
                  collapse = ", ")))

set.seed(20260816)
states <- lapply(seq_len(n_cubes), function(i) {
  s <- cube_identity(4)
  replay(s, sample(names(moves), scramble_len, TRUE))
})

# ---- The prefilled tables ---------------------------------------------------
#
# Loaded for the same reason bench_prefilled_solve.R loads them, and for one
# more that belongs to this script in particular: `prune_bound` is a reading
# taken FROM the phase-3 table. Against an empty table every state reads zero,
# and the column being tested here as a predictor would be a constant produced
# by not having loaded a file -- a null result that says nothing about the cubes.
#
# The depth the table was filled to is printed because it is the ceiling on what
# that column can say: a table filled to 7 cannot distinguish a state 9 moves
# out from one 13 moves out, and both read 7.
hr("the tables")
for (ph in 1:3) {
  cand <- Sys.glob(file.path(table_dir, sprintf("phase%d_d*.bin", ph)))
  if (length(cand) == 0) {
    cat(sprintf("  phase %d: no file in %s -- fills lazily\n", ph, table_dir))
    next
  }
  d <- as.integer(sub("^.*_d([0-9]+)\\.bin$", "\\1", cand))
  cand <- cand[order(d, decreasing = TRUE)]
  ld <- cayleyR:::cube_kociemba4_load_phase_cpp(cand[1], ph)
  if (isTRUE(ld$ok)) {
    cat(sprintf("  phase %d: %s, depth %d, %s entries, %.1f%% full\n",
                ph, basename(cand[1]), ld$built_depth, fmt(ld$n_writes),
                100 * ld$n_writes / ld$size))
  } else {
    cat(sprintf("  phase %d: %s refused (%s)\n", ph, basename(cand[1]),
                ld$reason))
  }
}

# ---- One orientation of one cube -------------------------------------------
#
# The properties are read from the state phase 3 is handed -- after phases 1
# and 2 have run -- because that is the state phase 3 actually searches from.
# Reading them from the cube before phase 2 would be measuring a different
# state than the one the prediction is about.
probe <- function(state, rot) {
  rot_word <- if (nzchar(rot)) cube_expand_word(rot, 4L) else character(0)
  turned <- replay(state, rot_word)

  t12 <- proc.time()[["elapsed"]]
  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(turned, upto_phase = 2L,
                                              node_budget = budget)
  secs12 <- proc.time()[["elapsed"]] - t12

  # Phase 2 failing is itself a prediction, and a free one: there is nothing for
  # phase 3 to search and no budget can change that.
  if (!length(p12)) {
    return(list(ok12 = FALSE, secs12 = secs12))
  }
  handed <- replay(turned, p12)
  if (!isTRUE(cayleyR:::cube_at_phase_goal_cpp(handed, 2L))) {
    return(list(ok12 = FALSE, secs12 = secs12))
  }

  co <- cayleyR:::cube_phase3_coord_cpp(handed)
  # A field the C++ side did not return would otherwise enter the data.frame as
  # a zero-length column and take the whole run down several cubes later; NA
  # keeps the row the right shape and shows up as a gap in the output.
  num <- function(x) if (is.null(x) || !length(x)) NA_integer_ else as.integer(x)
  list(ok12 = TRUE, secs12 = secs12, handed = handed,
       p12_moves = length(p12),
       prune_bound = num(co$prune_bound),
       centre_dist = num(co$centre_dist),
       centre_mismatch = num(co$centre_mismatch),
       wing_mismatch = num(co$wing_mismatch),
       parity_ok = isTRUE(num(co$n_goals_matching_bit) > 0),
       n_goals = num(co$n_goals_matching_bit))
}

# ---- The truth, which is what costs the time -------------------------------
truth <- function(handed) {
  t3 <- proc.time()[["elapsed"]]
  r3 <- cayleyR:::cube_kociemba4_phase3_cpp(handed, node_budget = budget,
                                            use_exact_centres = TRUE,
                                            progress_every = 0)
  list(found = isTRUE(r3$found),
       nodes = r3$nodes,
       secs = proc.time()[["elapsed"]] - t3,
       outcome = r3$outcome)
}

hr("per cube, per orientation")
rows <- list()
for (i in seq_len(n_cubes)) {
  cat(sprintf("\n  cube %d\n", i))
  cat(sprintf("  %-5s %6s %6s %6s %6s %5s | %9s %13s %8s\n",
              "rot", "bound", "cdist", "cmis", "wmis", "par",
              "outcome", "nodes", "secs"))
  for (rot in orientations) {
    lbl <- if (nzchar(rot)) rot else "(-)"
    p <- probe(states[[i]], rot)
    if (!isTRUE(p$ok12)) {
      cat(sprintf("  %-5s %6s %6s %6s %6s %5s | %9s %13s %8s\n",
                  lbl, "-", "-", "-", "-", "-", "no-phase2", "-", "-"))
      rows[[length(rows) + 1L]] <- data.frame(
        cube = i, rot = lbl, ok12 = FALSE, prune_bound = NA_integer_,
        centre_dist = NA_integer_, centre_mismatch = NA_integer_,
        wing_mismatch = NA_integer_, parity_ok = NA, p12_moves = NA_integer_,
        found = FALSE, nodes = NA_real_, secs = 0, stringsAsFactors = FALSE)
      next
    }
    tr <- truth(p$handed)
    ip <- function(x) if (is.na(x)) "     -" else sprintf("%6d", x)
    cat(sprintf("  %-5s %s %s %s %s %5s | %9s %13s %8.1f\n",
                lbl, ip(p$prune_bound), ip(p$centre_dist),
                ip(p$centre_mismatch), ip(p$wing_mismatch),
                if (p$parity_ok) "ok" else "NO",
                tr$outcome, fmt(tr$nodes), tr$secs))
    flush.console()
    rows[[length(rows) + 1L]] <- data.frame(
      cube = i, rot = lbl, ok12 = TRUE, prune_bound = p$prune_bound,
      centre_dist = p$centre_dist, centre_mismatch = p$centre_mismatch,
      wing_mismatch = p$wing_mismatch, parity_ok = p$parity_ok,
      p12_moves = p$p12_moves,
      found = tr$found, nodes = tr$nodes, secs = tr$secs,
      stringsAsFactors = FALSE)
  }
}
df <- do.call(rbind, rows)

# ---- Does any column separate the cheap orientations from the dear ones? ----
hr("does any column separate them")

live <- df[df$ok12, ]
cat(sprintf("  orientations with a phase-2 handover : %d of %d\n",
            nrow(live), nrow(df)))
cat(sprintf("  of those, phase 3 found a reduction  : %d\n", sum(live$found)))

# A column that never varies cannot rank anything, and this is the first thing
# to check rather than the last: the phase-3 prune table is filled to depth 7
# against searches that run to 14, so its bound may well read 7 for every state
# here -- in which case it is not a weak predictor, it is a constant.
cat("\n  spread of each column (a constant cannot rank anything):\n")
for (nm in c("prune_bound", "centre_dist", "centre_mismatch", "wing_mismatch",
             "p12_moves")) {
  v <- live[[nm]][!is.na(live[[nm]])]
  if (!length(v)) {
    cat(sprintf("    %-16s no readings\n", nm))
    next
  }
  cat(sprintf("    %-16s min %3d  max %3d  distinct %2d%s\n",
              nm, min(v), max(v), length(unique(v)),
              if (length(unique(v)) < 2) "   <- constant, ranks nothing" else ""))
}

# Correlation against the cost, on the orientations that finished. Spearman
# rather than Pearson: what matters is the order, not the size of the gaps, and
# an exhausted orientation's node count is a censored measurement -- it says
# "at least this many" -- which a rank handles honestly and a mean does not.
fin <- live[live$found, ]
if (nrow(fin) >= 3) {
  cat("\n  rank correlation with nodes, on orientations that finished:\n")
  for (nm in c("prune_bound", "centre_dist", "centre_mismatch",
               "wing_mismatch", "p12_moves")) {
    v <- fin[[nm]]
    rho <- if (length(unique(v)) < 2) NA_real_
           else suppressWarnings(cor(v, fin$nodes, method = "spearman"))
    cat(sprintf("    %-16s rho %s\n", nm,
                if (is.na(rho)) "  n/a (constant)" else sprintf("%+.2f", rho)))
  }
}

# ---- The test that actually matters: does its top pick work? ---------------
#
# The cascade does not need a correlation, it needs to know what to try first.
# So each column is scored by what a cascade sorting on it would have paid:
# try orientations in that column's order, stop at the first that reduces, and
# add up the seconds. Against two reference points -- the order the cascade uses
# today, and the best any ordering could do.
hr("what each column would have cost as a sort key")

order_cost <- function(sub, key_order) {
  # `sub` is one cube's four orientations; `key_order` the order to try them in.
  # An orientation that phase 2 could not hand over costs nothing and is skipped
  # -- the cascade already excludes those before the sweep.
  total <- 0
  for (k in key_order) {
    r <- sub[k, ]
    if (!isTRUE(r$ok12)) next
    total <- total + r$secs
    if (isTRUE(r$found)) return(list(secs = total, solved = TRUE))
  }
  list(secs = total, solved = FALSE)
}

keys <- c("prune_bound", "centre_dist", "centre_mismatch", "wing_mismatch",
          "p12_moves")
res <- list()
for (nm in c(keys, "cascade order", "best possible")) {
  tot <- 0; n_solved <- 0; n_top <- 0
  for (i in seq_len(n_cubes)) {
    sub <- df[df$cube == i, ]
    rownames(sub) <- NULL
    ord <- if (nm == "cascade order") {
      seq_len(nrow(sub))
    } else if (nm == "best possible") {
      # Cheapest first among those that work, which is the floor no sort key
      # can beat: it is chosen knowing the answer.
      c(order(ifelse(sub$found, sub$secs, Inf)))
    } else {
      # Ties broken by the cascade's own order, so a key that cannot separate
      # two orientations is scored as no better than not sorting -- not as
      # luckily right.
      order(ifelse(is.na(sub[[nm]]), Inf, sub[[nm]]), seq_len(nrow(sub)))
    }
    oc <- order_cost(sub, ord)
    tot <- tot + oc$secs
    n_solved <- n_solved + oc$solved
    # Did this key's first live pick turn out to be a working one?
    live_ord <- ord[sub$ok12[ord]]
    if (length(live_ord) && isTRUE(sub$found[live_ord[1]])) n_top <- n_top + 1
  }
  res[[length(res) + 1L]] <- data.frame(
    key = nm, secs = tot, solved = n_solved, top_pick_works = n_top,
    stringsAsFactors = FALSE)
}
rr <- do.call(rbind, res)
base <- rr$secs[rr$key == "cascade order"]

cat(sprintf("  %-16s %9s %8s %14s %10s\n",
            "sort key", "secs", "solved", "top pick works", "vs now"))
for (k in seq_len(nrow(rr))) {
  cat(sprintf("  %-16s %9.1f %5d/%-2d %11d/%-2d %9s\n",
              rr$key[k], rr$secs[k], rr$solved[k], n_cubes,
              rr$top_pick_works[k], n_cubes,
              if (rr$key[k] == "cascade order") "--"
              else sprintf("%+.0f%%", 100 * (rr$secs[k] - base) / base)))
}

hr("how to read this")
cat("
  A key is worth using when its top pick works on nearly every cube AND its
  total is near the best-possible row. Those are not the same test: a key can
  pick a working orientation every time and still be slow, by picking the
  dearest of the ones that work.

  If every key sits at the cascade's own number, the cheap properties do not
  separate these states and the ordering cannot be fixed by sorting -- which
  makes interleaving the orientations by depth the thing to try instead, since
  it needs no predictor at all.

  Watch `prune_bound` in the spread table above. It is a lower bound clipped by
  the depth the hash table was filled to; if it reads the same for every state
  here, that is the ceiling talking, not the cubes being alike.
")
