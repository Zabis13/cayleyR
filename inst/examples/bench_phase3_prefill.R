# What does it cost to fill phase 3's prune table to each depth?
#
# The plan being tested: fill the table once, deeply, and let a thousand cubes
# share it. kociemba4::solver4() is a singleton whose init() begins
# `if (ready) return`, so a table filled once stays filled for the life of the
# process -- that part needs no new machinery.
#
# What did need it was the fill. The original walks WORDS: it enumerates every
# allowed word up to the target length and writes at each node. The canonical
# FSM removes reorderings of commuting moves, but nothing removes two different
# words arriving at the same state, and on this coordinate nearly every word
# does. Measured to depth 6: 150,089,448 visits produced 8,310,614 writes, with
# the table three per cent full. Ninety-four per cent of the walk was landing
# on states already recorded.
#
# fill_prune_table_bfs walks STATES instead, expanding a frontier one level at
# a time and adding a successor only when the table has no entry for it -- so
# the table doubles as the visited set. Each state is visited once.
#
# The trade is memory. The frontier holds full PieceStates, and phase 3's
# levels grow by about twelve per level:
#
#     1, 6, 83, 938, 11044, 131150, 1555236          (levels 0 to 6, measured)
#     ~18,000,000                                     (level 7, extrapolated)
#     ~220,000,000                                    (level 8, extrapolated)
#
# against a table ceiling of 1<<28 = 268 million entries. Level 7 should fit;
# level 8 should not, and watching it fail to is the point of asking for it.
#
# Each depth is measured in a FRESH PROCESS. The breadth-first fill is not
# additive -- it always starts from the goals, so against a table already
# filled to depth d it meets nothing but occupied slots and returns having done
# nothing. Building to depth 7 means building to depth 7, not adding a level to
# a table already at 6, and the figure that matters is the whole cost either
# way.
#
# Run with:  Rscript inst/examples/bench_phase3_prefill.R
#            Rscript inst/examples/bench_phase3_prefill.R 7      # one depth
#            Rscript inst/examples/bench_phase3_prefill.R 7 dfs  # the old fill

args <- commandArgs(trailingOnly = TRUE)

# ---- the child process: one depth, one fill, one line of output ------------
if (length(args) >= 1 && nzchar(args[[1]])) {
  suppressMessages(library(cayleyR))

  depth <- as.integer(args[[1]])
  bfs   <- !(length(args) >= 2 && identical(args[[2]], "dfs"))
  size  <- 2^28

  t0 <- proc.time()[["elapsed"]]
  r  <- cayleyR:::cube_kociemba4_fill_phase3_cpp(depth, table_size = size,
                                                 breadth_first = bfs)
  el <- proc.time()[["elapsed"]] - t0

  # Written as a parseable line so the parent can read it back without the
  # child having to know how the table is formatted.
  cat(sprintf("RESULT %d %.2f %.0f %.0f %.0f %.0f %d\n",
              depth, el, r$n_visits, r$n_writes, r$n_collisions, r$size,
              r$built_depth))
  quit(save = "no")
}

# ---- the parent: drive one child per depth ---------------------------------
hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

levels <- c(5L, 6L, 7L, 8L)
self   <- sub("^--file=", "",
              grep("^--file=", commandArgs(FALSE), value = TRUE)[1])

hr("setup")
cat(sprintf("table size : %s entries (2^28, the phase 3 ceiling)\n", fmt(2^28)))
cat(sprintf("levels     : %s, each in a fresh process\n",
            paste(levels, collapse = ", ")))
cat("fill       : breadth first, one visit per state\n")
cat("\nlevel sizes measured by breadth-first from the goals:\n")
cat("  0-6      1, 6, 83, 938, 11044, 131150, 1555236\n")
cat("  7       ~18,000,000    (extrapolated at x12)\n")
cat("  8      ~220,000,000    (extrapolated; the ceiling is 268M)\n")

hr("filling")

cat(sprintf("  %6s %10s %14s %14s %14s %9s %7s\n",
            "level", "secs", "visits", "writes", "collisions", "fill", "built"))

rows <- list()

for (d in levels) {
  out <- system2("Rscript", c(self, as.character(d)), stdout = TRUE,
                 stderr = FALSE)
  line <- grep("^RESULT ", out, value = TRUE)

  if (length(line) == 0) {
    cat(sprintf("  %6d %10s  (no result -- out of memory, or the fill died)\n",
                d, "-"))
    flush.console()
    next
  }

  f <- as.numeric(strsplit(line[[1]], " ")[[1]][-1])
  names(f) <- c("depth", "secs", "visits", "writes", "collisions", "size",
                "built")

  cat(sprintf("  %6d %10.1f %14s %14s %14s %9.3f %7d\n",
              d, f[["secs"]], fmt(f[["visits"]]), fmt(f[["writes"]]),
              fmt(f[["collisions"]]), f[["writes"]] / f[["size"]],
              as.integer(f[["built"]])))
  flush.console()

  rows[[length(rows) + 1L]] <- as.data.frame(as.list(f))

  # Past twenty minutes the plan stops being "fill it once at startup".
  if (f[["secs"]] > 1200) {
    cat("\n  (past twenty minutes for a single depth; stopping here)\n")
    break
  }
}

if (length(rows) == 0) {
  cat("\nnothing completed\n")
  quit(save = "no")
}

tab <- do.call(rbind, rows)

hr("every level")
print(tab[, c("depth", "secs", "visits", "writes", "collisions", "built")],
      row.names = FALSE)

hr("what this says")

deepest <- tab[nrow(tab), ]
cat(sprintf("  deepest depth built : %d in %.1f s (%.1f min)\n",
            as.integer(deepest[["built"]]), deepest[["secs"]],
            deepest[["secs"]] / 60))
cat(sprintf("  entries held        : %s of %s slots (%.1f%%)\n",
            fmt(deepest[["writes"]]), fmt(deepest[["size"]]),
            100 * deepest[["writes"]] / deepest[["size"]]))

cat("\n  visits per write, by level -- one is perfect, and the depth-first\n")
cat("  fill managed eighteen at level 6:\n")
for (i in seq_len(nrow(tab))) {
  cat(sprintf("    level %d : %.2f\n", as.integer(tab$depth[i]),
              tab$visits[i] / max(tab$writes[i], 1)))
}

cat("\n  A ratio near one means the walk is not repeating itself and the time\n")
cat("  went into states the table kept. A `built` below the level asked for\n")
cat("  means the frontier hit its cap and the fill stopped early -- the table\n")
cat("  is complete to the depth shown and usable, just shallower than asked.\n")

cat("\n  If a useful depth lands inside twenty minutes, the next measurement is\n")
cat("  what it saves per cube: solve once in a fresh process, then fill and\n")
cat("  solve again in the same one, and compare.\n")
