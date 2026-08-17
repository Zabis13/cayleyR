#!/usr/bin/env Rscript
# Does a deeper prune table pay for itself in phase 3?
#
# The companion to bench_phase3_width.R. That one holds the depth fixed and
# varies the width; this one holds the width fixed and varies the depth. Read
# together they separate the two explanations for phase 3 getting stuck, and
# neither can separate them alone.
#
# What is already measured, and does not need measuring again:
#
#   Six cubes in twenty-four fail (diag_phase3_seeds.R): seeds 4, 8, 12, 20,
#   23, 24. They are not special -- they are the tail. Every one of them shows
#   the same two things:
#
#     best_bound reaches 0     on all six. A bound of zero on a cube that was
#                              never solved cannot be that state's own
#                              distance; it is another state's entry read out
#                              of the same slot. Solved cubes average 0.22.
#
#     the pairing never moves  five of the six end the branch the table rated
#                              closest with exactly the pairing count they
#                              started with: 2->2, 0->0, 8->8, 4->4, 3->3. The
#                              sixth goes backwards, 3->1. Solved cubes reach
#                              12 every time.
#
#   A heuristic that leads the search somewhere the phase's own measure never
#   improves is not steering. The question is why.
#
# The two candidates, and what each predicts here:
#
#   the table is too shallow    Filled to depth 5 while the hard cubes start at
#                               bound 5 or 6, the bound saturates at the fill
#                               depth and cannot tell "six away" from "sixteen
#                               away". Then deepening moves best_bound up
#                               towards the real distance and the cubes start
#                               solving -- and the bonus levels below are the
#                               fix.
#
#   the table is too narrow     Entries collide before depth matters, so a
#                               deeper fill writes more entries into the same
#                               slots and makes the collisions worse. Then
#                               best_bound stays pinned at 0, cut_ratio does
#                               not improve, and the answer is width --
#                               bench_phase3_width.R's question, not this one.
#
# A third outcome is possible and is the one worth naming in advance: neither
# helps within any affordable size. Phase 3's coordinate is about 1e11 states
# and no addressable table holds it, so widening trades one collision rate for
# a slightly lower one. That would say the work is the coordinate itself --
# what phase 3 needs to know about a state, rather than where every piece in it
# sits.
#
# What to read, in order: best_bound first (is it still pinned at 0), then
# cut_ratio and mean_bound (is the table firing at all), then paired_after
# (did the search go anywhere useful), then nodes. Nodes last, because on this
# phase node counts have twice been identical to the last digit across settings
# that differ -- which is what a heuristic that never fires looks like, and
# nothing like one that got weaker.
#
# Run with:
#   Rscript inst/examples/bench_phase3_prune_depth.R
#   Rscript inst/examples/bench_phase3_prune_depth.R 3 12   # 3 seeds, depth 12

library(cayleyR)

args   <- commandArgs(trailingOnly = TRUE)
worker <- length(args) >= 1L && args[[1]] == "--run"

N <- 4L

# The six that fail, from diag_phase3_seeds.R. Named rather than re-derived so
# this run and that one are looking at the same cubes; the scramble is
# reproduced below by the same set.seed/sample pair both scripts use.
FAILING <- c(4L, 8L, 12L, 20L, 23L, 24L)

n_seeds <- if (!worker && length(args) >= 1L) as.integer(args[[1]]) else 3L
DEPTH   <- if (!worker && length(args) >= 2L) as.integer(args[[2]]) else 12L

# Extra table levels beyond the depth/2 rule.
#
# Measured before this was cut down: filling to depth 6 takes 7.5 s and to
# depth 7 takes 177.6 s, a factor of 24 for one level. Depth 8 is then about an
# hour per cube and depth 9 the better part of a day, so bonuses 2 and 3 do not
# produce data -- they produce timeouts that cost fifteen minutes each. Two
# levels is the range that can actually be compared.
BONUSES    <- 0:1
BUDGET     <- 1e6
P12_BUDGET <- 2e6
# Enough for a depth-7 fill (178 s) with room to spare, and short enough that a
# run which is not going to finish says so promptly.
TIMEOUT_S  <- 400L

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

# ---- the pairing measure --------------------------------------------------

# The measure phase 3 exists to improve, and the one that showed the search was
# not improving it. Wings occupy slots 9:32 of the 56-piece vector and are
# numbered from the same offset, so the 8 comes off before the geometry vectors
# -- which are indexed from zero -- are touched.
.wing_geom <- cayleyR:::cube_wing_geometry_cpp()
n_paired <- function(state) {
  perm <- cayleyR:::cube_to_pieces4_cpp(state)$perm
  w <- perm[9:32] - 8L
  paired <- 0L
  seen <- logical(24)
  for (slot in seq_len(24)) {
    if (seen[[slot]]) next
    piece <- w[[slot]]
    mate_piece <- .wing_geom$partner[[piece + 1L]]
    mate_slot <- which(w == mate_piece)
    if (!length(mate_slot)) next
    seen[[slot]] <- TRUE
    seen[[mate_slot[[1]]]] <- TRUE
    if (.wing_geom$dedge[[slot]] == .wing_geom$dedge[[mate_slot[[1]]]]) {
      paired <- paired + 1L
    }
  }
  paired
}

# Checked in every process that uses it, including the workers. This measure
# has been wrong twice -- once crossing pieces with stickers and returning 2 on
# every cube, once reading wing numbers in the wrong index space and running
# off the end of `partner` -- and both times it was a control state that caught
# it rather than the code looking right.
check_measures <- function() {
  chk <- n_paired(cube_identity(N))
  if (chk != 12L) {
    stop("n_paired() reports ", chk, " on a solved cube, expected 12",
         call. = FALSE)
  }
}
check_measures()

handed_state <- function(seed) {
  set.seed(seed)
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(s, upto_phase = 2L,
                                              node_budget = P12_BUDGET)
  list(state = replay(s, p12), word = w, p12_moves = length(p12),
       p2ok = isTRUE(cayleyR:::cube_at_phase_goal_cpp(replay(s, p12), 2L)))
}

# ---- worker ---------------------------------------------------------------

# One bonus level on one seed, in its own process. Same structure as
# bench_phase3_width.R, and for the same reason: a fill that runs long cannot
# be interrupted from inside R, so it is given a timeout from outside.
if (worker) {
  seed  <- as.integer(args[[2]])
  bonus <- as.integer(args[[3]])
  out   <- args[[4]]

  h <- handed_state(seed)
  paired_before <- n_paired(h$state)

  t0 <- proc.time()[["elapsed"]]
  r <- cayleyR:::cube_kociemba4_phase3_cpp(h$state, max_depth3 = DEPTH,
                                           node_budget = BUDGET,
                                           prune_depth_bonus = bonus)
  secs <- proc.time()[["elapsed"]] - t0

  tb <- cayleyR:::cube_kociemba4_tables_cpp()$phase3
  co <- cayleyR:::cube_phase3_coord_cpp(h$state)
  paired_after <- n_paired(replay(h$state, r$path))

  # waste_ratio is collisions over visits during the fill -- the table's own
  # account of how often two states landed in one slot. It belongs next to
  # best_bound because the two are the same claim measured from opposite ends:
  # one counts collisions as they happen, the other sees their effect on the
  # bound the search reads back.
  writeLines(sprintf(
    "RESULT\t%s\t%.0f\t%.2f\t%.0f\t%.0f\t%d\t%d\t%d\t%d\t%d\t%.0f\t%.6f\t%.2f\t%d\t%d\t%.6f",
    r$outcome, r$nodes, secs, tb$size, tb$filled, tb$built_depth,
    r$best_bound, co$prune_bound, paired_before, paired_after,
    r$prune_lookups, r$cut_ratio, r$mean_bound, length(r$path),
    co$wing_mismatch, tb$waste_ratio), out)
  quit(save = "no")
}

# ---- driver ---------------------------------------------------------------

this_file <- sub("^--file=", "",
                 grep("^--file=", commandArgs(FALSE), value = TRUE)[1])

seeds <- head(FAILING, n_seeds)

na_row <- function(seed, bonus) {
  data.frame(seed = seed, bonus = bonus, outcome = "timeout",
             nodes = NA_real_, seconds = NA_real_, size = NA_real_,
             filled = NA_real_, built_depth = NA_integer_,
             best_bound = NA_integer_, bound_start = NA_integer_,
             paired_before = NA_integer_, paired_after = NA_integer_,
             lookups = NA_real_, cut_ratio = NA_real_, mean_bound = NA_real_,
             n_moves = NA_integer_, wing_mismatch = NA_integer_,
             waste_ratio = NA_real_,
             solved = FALSE, stringsAsFactors = FALSE)
}

run_one <- function(seed, bonus) {
  cat(sprintf("    bonus %d (table to depth %d) ... ", bonus,
              DEPTH %/% 2 + bonus))
  flush.console()

  res <- tempfile("p3d", fileext = ".tsv")
  on.exit(unlink(res), add = TRUE)
  system2(file.path(R.home("bin"), "Rscript"),
          c(shQuote(this_file), "--run", seed, bonus, shQuote(res)),
          stdout = NULL, stderr = NULL, timeout = TIMEOUT_S)

  line <- if (file.exists(res))
            grep("^RESULT\t", readLines(res, warn = FALSE), value = TRUE)
          else character(0)
  if (length(line) != 1L) {
    cat(sprintf("no result (over %d s)\n", TIMEOUT_S))
    return(na_row(seed, bonus))
  }

  f <- strsplit(line[[1]], "\t", fixed = TRUE)[[1]]
  o <- data.frame(seed = seed, bonus = bonus, outcome = f[[2]],
                  nodes = as.numeric(f[[3]]), seconds = as.numeric(f[[4]]),
                  size = as.numeric(f[[5]]), filled = as.numeric(f[[6]]),
                  built_depth = as.integer(f[[7]]),
                  best_bound = as.integer(f[[8]]),
                  bound_start = as.integer(f[[9]]),
                  paired_before = as.integer(f[[10]]),
                  paired_after = as.integer(f[[11]]),
                  lookups = as.numeric(f[[12]]),
                  cut_ratio = as.numeric(f[[13]]),
                  mean_bound = as.numeric(f[[14]]),
                  n_moves = as.integer(f[[15]]),
                  wing_mismatch = as.integer(f[[16]]),
                  waste_ratio = as.numeric(f[[17]]),
                  solved = f[[2]] == "found", stringsAsFactors = FALSE)

  cat(sprintf("%-10s best %2d  cuts %6.2f%%  mean %.2f  pairs %2d->%2d  %9s nodes  %5.1fs\n",
              o$outcome, o$best_bound, 100 * o$cut_ratio, o$mean_bound,
              o$paired_before, o$paired_after,
              format(o$nodes, big.mark = ",", scientific = FALSE), o$seconds))
  o
}

hr("setup")
cat("seeds       : ", paste(seeds, collapse = ", "),
    "   (the ones diag_phase3_seeds.R found failing)\n", sep = "")
cat("search depth: ", DEPTH, "\n", sep = "")
cat("bonuses     : ", paste(BONUSES, collapse = ", "),
    "   (bonus 0 is the standard depth/2 rule, table to depth ",
    DEPTH %/% 2, ")\n", sep = "")
cat("node budget : ", format(BUDGET, scientific = FALSE, big.mark = ","),
    "\n", sep = "")
cat("\nbest_bound is the column this run exists for. It stays 0 on every one\n")
cat("of these cubes at bonus 0, and a 0 without a solution is a collision\n")
cat("rather than a distance. If deepening lifts it towards the real distance\n")
cat("the table was too shallow; if it stays pinned, the table is too narrow\n")
cat("and the depth is not what to spend on.\n")
cat("\nEach bonus level costs about 24x the last in fill time -- 7.5 s to\n")
cat("depth 6, 177.6 s to depth 7. The seconds column is mostly the fill, not\n")
cat("the search, and it is why this stops at bonus 1.\n")

rows <- list()
for (sd in seeds) {
  h <- handed_state(sd)
  hr(sprintf("seed %d  (phases 1+2: %d moves%s, %d paired)", sd, h$p12_moves,
             if (h$p2ok) "" else ", PHASE 2 DID NOT ARRIVE",
             n_paired(h$state)))
  if (!h$p2ok) {
    cat("    phase 2 did not reach its goal, so phase 3 is being handed a cube\n")
    cat("    outside its domain and these rows say nothing about the table.\n")
  }
  for (b in BONUSES) rows[[length(rows) + 1L]] <- run_one(sd, b)
}
tab <- do.call(rbind, rows)

# ---- what the table says --------------------------------------------------

hr("by table depth")

print(do.call(rbind, lapply(split(tab, tab$bonus), function(d) data.frame(
  bonus = d$bonus[1],
  table_depth = DEPTH %/% 2 + d$bonus[1],
  best_bound = round(mean(d$best_bound, na.rm = TRUE), 2),
  zero_bounds = sprintf("%d/%d", sum(d$best_bound == 0L, na.rm = TRUE),
                        sum(!is.na(d$best_bound))),
  cut_pct = round(100 * mean(d$cut_ratio, na.rm = TRUE), 3),
  mean_bound = round(mean(d$mean_bound, na.rm = TRUE), 2),
  waste_pct = round(100 * mean(d$waste_ratio, na.rm = TRUE), 1),
  fill_pct = round(100 * mean(d$filled / d$size, na.rm = TRUE), 1),
  paired_moved = sprintf("%d/%d",
                         sum(d$paired_after != d$paired_before, na.rm = TRUE),
                         sum(!is.na(d$paired_after))),
  solved = sprintf("%d/%d", sum(d$solved), nrow(d)),
  nodes = round(mean(d$nodes, na.rm = TRUE)),
  secs = round(mean(d$seconds, na.rm = TRUE), 1),
  stringsAsFactors = FALSE))), row.names = FALSE)

hr("verdict")

fin <- tab[!is.na(tab$best_bound), ]
if (!nrow(fin)) {
  cat("No run finished inside the timeout. Lower the depth or raise it.\n")
} else {
  b0 <- fin[fin$bonus == min(fin$bonus), ]
  bN <- fin[fin$bonus == max(fin$bonus), ]

  cat(sprintf("table to depth %d: best bound %.2f, %d of %d pinned at 0, %d solved\n",
              DEPTH %/% 2 + b0$bonus[1], mean(b0$best_bound),
              sum(b0$best_bound == 0L), nrow(b0), sum(b0$solved)))
  cat(sprintf("table to depth %d: best bound %.2f, %d of %d pinned at 0, %d solved\n",
              DEPTH %/% 2 + bN$bonus[1], mean(bN$best_bound),
              sum(bN$best_bound == 0L), nrow(bN), sum(bN$solved)))
  cat("\n")

  gained <- sum(bN$solved) - sum(b0$solved)
  unpinned <- sum(b0$best_bound == 0L) - sum(bN$best_bound == 0L)

  if (gained > 0 && unpinned > 0) {
    cat("Deepening lifted the bound off zero and solved cubes that were\n")
    cat("failing. The table was too shallow, and the depth/2 rule is the\n")
    cat("thing to change -- read the seconds too, since a level costs about\n")
    cat("as much to build as it saves.\n")
  } else if (unpinned <= 0 && mean(bN$cut_ratio) <= mean(b0$cut_ratio) * 1.1) {
    cat("Deepening did not move the bound off zero and did not improve the\n")
    cat("cut rate. The entries are colliding before the depth matters, so a\n")
    cat("deeper fill is writing more of them into the same slots. Width is\n")
    cat("the variable -- bench_phase3_width.R -- not depth.\n")
  } else {
    cat("Deepening moved the numbers without settling the question. Compare\n")
    cat("mean_bound against the depths being searched: a bound that stays far\n")
    cat("below the remaining depth is an honest weak table rather than a\n")
    cat("broken one, and neither width nor depth repairs that -- the\n")
    cat("coordinate does.\n")
  }
}

out_file <- file.path(dirname(tempdir()), "cayleyR_phase3_depth.rds")
saveRDS(tab, out_file)
cat("\nrows written to ", out_file, "\n", sep = "")
