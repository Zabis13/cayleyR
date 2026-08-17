# Where does a phase 3 solve actually spend its time?
#
# The batch target is ten seconds a cube. On a table prefilled to depth 7 the
# first three cubes of a twenty-move sample took 33.9, 61.0 and 216.1 seconds,
# so the prefill alone does not get there and the question is what the time is
# made of before trying to buy more of anything.
#
# Four candidates, wanting four different fixes:
#
#   the tree        genuinely many nodes, and the table already cuts what it
#                   can. A bigger table then buys a little more.
#   cache misses    the table is 256 MB. Nothing that size fits in any level of
#                   cache, so every prune lookup is a trip to main memory at
#                   about a hundred nanoseconds. At millions of nodes that is
#                   the whole budget -- and a BIGGER table makes it worse, not
#                   better, however many collisions it saves.
#   rebuilding      a search that calls grow_to() reallocates and discards, and
#                   pays to fill the levels again. n_grows counts it.
#   orientations    the reduction used to try all 24 and keep the shortest,
#                   paying a full node budget per failure. stop_at_first was
#                   added today; this checks it is in force.
#
# The counters separate them. nodes and prune_lookups say how much work there
# was; secs per million nodes says how expensive each unit of it was, which is
# the cache question; n_grows says whether the table survived; and running the
# reduction with one orientation against many says what the loop costs.
#
# The table is loaded from a file rather than filled, so the same table is
# measured every time and the 85-second fill is paid once, ever.
#
# Run with:  Rscript inst/examples/bench_where_time_goes.R [table.bin] [cubes]
#            Rscript inst/examples/bench_where_time_goes.R --save table.bin

suppressMessages(library(cayleyR))

args <- commandArgs(trailingOnly = TRUE)

# ---- --save: build the table once and write it out -------------------------
if (length(args) >= 1 && identical(args[[1]], "--save")) {
  path  <- if (length(args) >= 2) args[[2]] else "phase3_d7.bin"
  force <- length(args) >= 3 && identical(args[[3]], "--force")

  # An existing table is the point of having one. Rebuilding it costs 85
  # seconds and produces the same bytes, so the file is left alone unless
  # --force says otherwise -- and it is checked by loading rather than by its
  # name, since a file from a different generator set would be refused and is
  # worth finding out about here rather than mid-benchmark.
  if (file.exists(path) && !force) {
    ld <- cayleyR:::cube_kociemba4_load_phase3_cpp(path)
    if (isTRUE(ld$ok)) {
      cat(sprintf("%s already exists and loads: depth %d, %s entries, %s MB\n",
                  path, ld$built_depth,
                  format(ld$n_writes, big.mark = ",", scientific = FALSE),
                  format(round(ld$size / 1024 / 1024), big.mark = ",")))
      cat("nothing to do -- pass --force after the path to rebuild it\n")
      quit(save = "no")
    }
    cat(sprintf("%s exists but will not load (%s); rebuilding\n",
                path, ld$reason))
  }

  cat(sprintf("filling to depth 7 at 2^28 slots, then writing %s\n", path))
  t0 <- proc.time()[["elapsed"]]
  f  <- cayleyR:::cube_kociemba4_fill_phase3_cpp(7L, table_size = 2^28,
                                                 breadth_first = TRUE)
  cat(sprintf("  filled to depth %d in %.1f s, %s entries\n",
              f$built_depth, proc.time()[["elapsed"]] - t0,
              format(f$n_writes, big.mark = ",", scientific = FALSE)))

  t0 <- proc.time()[["elapsed"]]
  ok <- cayleyR:::cube_kociemba4_save_phase3_cpp(path)
  cat(sprintf("  wrote %s in %.1f s: %s\n", path,
              proc.time()[["elapsed"]] - t0, if (ok) "ok" else "FAILED"))
  cat(sprintf("  file is %s\n",
              format(file.size(path), big.mark = ",", scientific = FALSE)))
  quit(save = "no")
}

path     <- if (length(args) >= 1) args[[1]] else "phase3_d7.bin"
n_states <- if (length(args) >= 2) as.integer(args[[2]]) else 3L
n_moves  <- 20L

N  <- 4L
g  <- cube_group(N)
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

# ---- the table -------------------------------------------------------------
hr("the table")

if (!file.exists(path)) {
  cat(sprintf("  %s does not exist.\n", path))
  cat("  Build it first:  Rscript this-script.R --save phase3_d7.bin\n")
  quit(save = "no", status = 1)
}

t0 <- proc.time()[["elapsed"]]
ld <- cayleyR:::cube_kociemba4_load_phase3_cpp(path)
el <- proc.time()[["elapsed"]] - t0

cat(sprintf("  %s: %s (%.1f s)\n", path, ld$reason, el))
if (!isTRUE(ld$ok)) quit(save = "no", status = 1)

cat(sprintf("  depth %d, %s entries in %s slots (%.1f%% full)\n",
            ld$built_depth, fmt(ld$n_writes), fmt(ld$size),
            100 * ld$n_writes / ld$size))
cat(sprintf("  %s MB resident\n", fmt(ld$size / 1024 / 1024)))

set.seed(2026)
states <- lapply(seq_len(n_states),
                 function(i) generate_state(group = g, n_moves = n_moves))

# ---- phase 3 alone, with its counters --------------------------------------
#
# cube_kociemba4_phase3_cpp reports nodes, prune_lookups, prune_cuts and
# cut_ratio, which the whole-reduction entry point does not. So the handover is
# produced separately and phase 3 measured on it, and the two timings are kept
# apart -- a figure that mixes them cannot answer which is expensive.
hr("phases 1+2, then phase 3, separately")

cat(sprintf("  %4s %9s %9s %12s %14s %9s %8s\n",
            "cube", "p12 secs", "p3 secs", "p3 nodes", "prune lookups",
            "cut", "s per Mn"))

rows <- list()
for (i in seq_len(n_states)) {
  s <- states[[i]]

  t0 <- proc.time()[["elapsed"]]
  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(s, upto_phase = 2L,
                                              node_budget = 5e7)
  t_p12 <- proc.time()[["elapsed"]] - t0
  handover <- replay(s, p12)

  t0 <- proc.time()[["elapsed"]]
  r3 <- cayleyR:::cube_kociemba4_phase3_cpp(handover, max_depth3 = 20L,
                                            node_budget = 5e7,
                                            use_exact_centres = TRUE)
  t_p3 <- proc.time()[["elapsed"]] - t0

  per_mn <- if (r3$nodes > 0) t_p3 / (r3$nodes / 1e6) else NA_real_

  cat(sprintf("  %4d %9.1f %9.1f %12s %14s %9.3f %8.2f\n", i, t_p12, t_p3,
              fmt(r3$nodes), fmt(r3$prune_lookups), r3$cut_ratio, per_mn))
  flush.console()

  rows[[i]] <- data.frame(cube = i, p12_secs = t_p12, p3_secs = t_p3,
                          nodes = r3$nodes, lookups = r3$prune_lookups,
                          cuts = r3$prune_cuts, cut_ratio = r3$cut_ratio,
                          found = isTRUE(r3$found), secs_per_mn = per_mn,
                          stringsAsFactors = FALSE)
}
tab <- do.call(rbind, rows)

# ---- did the table survive? ------------------------------------------------
hr("the table after the solves")

t2 <- cayleyR:::cube_kociemba4_tables_cpp()$phase3
cat(sprintf("  built_depth %d, %s entries, n_grows %d\n",
            t2$built_depth, fmt(t2$n_writes), t2$n_grows))
if (t2$n_grows > 0) {
  cat("  IT DID NOT. grow_to() reallocated, which discards the contents and\n")
  cat("  sets built_depth to zero -- every level was walked again. Loading\n")
  cat("  pins min_size and max_size to the file's size to prevent exactly\n")
  cat("  this, so a non-zero count here means something else moved them.\n")
} else if (t2$built_depth < ld$built_depth) {
  cat("  Shallower than it was loaded at, which should not happen.\n")
} else {
  cat("  Intact: the solves used the loaded table and did not rebuild it.\n")
}

# ---- what one orientation costs against many -------------------------------
#
# stop_at_first was made the default today. This is the check that it holds --
# if one orientation and twenty-four cost the same, it does not.
hr("orientations")

s <- states[[1]]
t0 <- proc.time()[["elapsed"]]
r1 <- cube_kociemba4_reduce(s, node_budget = 5e7, max_orientations = 1L)
t_one <- proc.time()[["elapsed"]] - t0

t0 <- proc.time()[["elapsed"]]
rd <- cube_kociemba4_reduce(s, node_budget = 5e7)
t_def <- proc.time()[["elapsed"]] - t0

cat(sprintf("  one orientation      : %.1f s, %d moves\n", t_one, length(r1)))
cat(sprintf("  defaults             : %.1f s, %d moves\n", t_def, length(rd)))
cat(sprintf("  ratio                : %.2fx\n", t_def / max(t_one, 1e-9)))
cat("\n  Near 1.0 means stop_at_first is working and the loop is not the cost.\n")
cat("  Much above it means orientations are still being tried after one has\n")
cat("  succeeded, and that is the first thing to fix.\n")

# ---- the verdict -----------------------------------------------------------
hr("what this says")

cat(sprintf("  phase 3 is %.0f%% of the solve time\n",
            100 * sum(tab$p3_secs) / sum(tab$p3_secs + tab$p12_secs)))
cat(sprintf("  seconds per million nodes : %s\n",
            paste(sprintf("%.2f", tab$secs_per_mn), collapse = ", ")))
cat(sprintf("  lookups per node          : %.2f\n",
            sum(tab$lookups) / max(sum(tab$nodes), 1)))

cat("\n  Seconds per million nodes is the cache question. A node is a few\n")
cat("  dozen instructions plus one lookup into 256 MB; if the figure sits\n")
cat("  around 0.1 s per million the lookups are landing in memory every time\n")
cat("  and the table's SIZE is the cost, not its contents. Enlarging it would\n")
cat("  then make solves slower while making the table more complete -- the\n")
cat("  opposite of the trade wanted, and worth knowing before paying for it.\n")
cat("\n  If instead the figure is well under that, the time is the tree itself\n")
cat("  and a deeper or larger table is the lever that remains.\n")
