# Build the prune tables for all three phases and write them to disk.
#
# Why all three rather than phase 3 alone, which is the one that got a file
# first: phase 3's fill is the expensive one -- 88 seconds -- so it looked like
# the one worth saving. Profiling a solve says otherwise. On the first cube of
# the standard seed, solved in 36.2 seconds:
#
#     phase 1+2        30.6 s   (84.6%)   17 moves
#     phase 3           5.5 s   (15.2%)   25 moves
#     3x3x3 finish      0.1 s   ( 0.2%)  238 moves
#
# Phase 3, with its 256 MB table loaded, was the cheap part. The 30.6 seconds
# went to phases 1 and 2, whose tables are not prepared at all: init() calls
# build_prune_table(..., max_depth = 0, ...), which stores the goals and stops,
# and the levels are then filled lazily inside the search -- to limit/2, once
# per iterative-deepening level, with a grow_to() before each fill that discards
# the table when it reallocates. A cube that needs depth 9 pays for that
# repeatedly, and a cube that needs depth 6 does not, which is why the same
# stage costs 0.05 s on one cube and 30.6 s on another.
#
# A table built once here and loaded at the start of a run pays neither cost.
#
# Usage:
#   Rscript inst/examples/build_phase_tables.R [flags] [dir] [d1] [d2] [d3]
#
#   dir          where the files go, default /mnt/Data2/DS_projects/phase3
#   d1, d2, d3   depth per phase, default 7 7 7
#   --phase N    only these phases, comma separated: --phase 2
#   --force      refill even when a usable file is there
#   --dfs        fill by walking words rather than states
#
# Rebuilding one phase is the common case, not all three: phase 3's fill is 88
# seconds and has nothing to do with a question about phase 2.
#
#   Rscript inst/examples/build_phase_tables.R --phase 2 --force --dfs . 7 5 7
#
# Files are named phase<N>_d<depth>.bin, matching what the benches already
# look for. An existing file at the right depth is left alone: the fill is
# deterministic, so rebuilding it produces the same bytes for the same time.

suppressMessages(library(cayleyR))

args <- commandArgs(trailingOnly = TRUE)

# --dfs fills with extend_prune_table (walks words) instead of
# fill_prune_table_bfs (walks states). The breadth-first fill is normally the
# cheap one -- a hundred times cheaper on phase 3 -- but it stopped at depth 1
# on phase 2, storing 12 entries, while the search filling the same table
# lazily reached depth 5 and 116,538 entries. Both fills are here so that is a
# measurement rather than an argument.
use_dfs <- "--dfs" %in% args
args <- args[args != "--dfs"]

# --force refills even when a usable file is already there, which is what
# comparing two fills needs.
force <- "--force" %in% args
args <- args[args != "--force"]

# --phase 2, or --phase 1,2 -- which phases to touch at all. Rebuilding phase 3
# to answer a question about phase 2 costs 88 seconds and answers nothing, so
# the default of all three is a convenience rather than something to accept
# when one phase is what is being measured.
phases <- 1:3
ix <- which(args == "--phase")
if (length(ix) == 1 && length(args) > ix) {
  phases <- as.integer(strsplit(args[[ix + 1L]], ",")[[1]])
  if (any(is.na(phases)) || any(phases < 1 | phases > 3)) {
    stop("--phase takes 1, 2 or 3, comma separated")
  }
  args <- args[-c(ix, ix + 1L)]
}

dir  <- if (length(args) >= 1) args[[1]] else "/mnt/Data2/DS_projects/phase3"
depths <- c(
  if (length(args) >= 2) as.integer(args[[2]]) else 7L,
  if (length(args) >= 3) as.integer(args[[3]]) else 7L,
  if (length(args) >= 4) as.integer(args[[4]]) else 7L)

if (!dir.exists(dir)) {
  dir.create(dir, recursive = TRUE)
  cat(sprintf("created %s\n", dir))
}

fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

cat(sprintf("target dir : %s\n", dir))
cat(sprintf("phases     : %s\n", paste(phases, collapse = ", ")))
cat(sprintf("depths     : %s\n",
            paste(sprintf("phase %d = %d", phases, depths[phases]),
                  collapse = ", ")))
cat(sprintf("fill       : %s%s\n",
            if (use_dfs) "depth first (extend_prune_table)"
            else "breadth first (fill_prune_table_bfs)",
            if (force) ", forced" else ""))

# Phases 1 and 2 are capped at 1<<24 slots by init() and phase 3 at 1<<28, so
# the sizes below are what those caps allow rather than a choice made here.
# Asking grow_to() for more than the cap gets the cap.
sizes <- c(2^24, 2^24, 2^28)

rows <- list()

for (ph in phases) {
  depth <- depths[ph]
  path  <- file.path(dir, sprintf("phase%d_d%d.bin", ph, depth))

  hr(sprintf("phase %d, depth %d", ph, depth))

  if (file.exists(path) && !force) {
    # Loading it is also the check that it is usable: the signature over the
    # generators and goals is verified on the way in, so a file left over from
    # a differently built package is rejected here rather than silently used.
    ld <- cayleyR:::cube_kociemba4_load_phase_cpp(path, ph)
    if (isTRUE(ld$ok)) {
      cat(sprintf("  %s exists: %s\n", basename(path), ld$reason))
      cat(sprintf("  depth %d, %s entries in %s slots (%.1f%% full)\n",
                  ld$built_depth, fmt(ld$n_writes), fmt(ld$size),
                  100 * ld$n_writes / ld$size))
      rows[[ph]] <- data.frame(phase = ph, depth = ld$built_depth,
                               entries = ld$n_writes, slots = ld$size,
                               secs = 0, action = "loaded",
                               stringsAsFactors = FALSE)
      next
    }
    cat(sprintf("  %s exists but was refused (%s) -- rebuilding\n",
                basename(path), ld$reason))
  }

  t0 <- proc.time()[["elapsed"]]
  f  <- cayleyR:::cube_kociemba4_fill_phase_cpp(depth, ph,
                                                table_size = sizes[ph],
                                                breadth_first = !use_dfs)
  el <- proc.time()[["elapsed"]] - t0

  cat(sprintf("  filled to depth %d in %.1f s\n", f$built_depth, el))
  cat(sprintf("  %s entries in %s slots (%.1f%% full), %s visits\n",
              fmt(f$n_writes), fmt(f$size), 100 * f$n_writes / f$size,
              fmt(f$n_visits)))

  # Visits per write is the saturation reading: at 1.0 every state found was
  # stored, and as it climbs the table is losing states to collisions and never
  # expanding their successors. Phase 3 measured 1.87 rising to 2.95 over the
  # depth 7 fill, which is the figure that says its ceiling binds.
  if (f$n_writes > 0) {
    cat(sprintf("  %.2f visits per write\n", f$n_visits / f$n_writes))
  }

  if (f$built_depth < depth) {
    cat(sprintf("  (asked for %d; the frontier hit its cap, so the table is\n",
                depth))
    cat("   complete to the depth shown and usable, just shallower)\n")
  }

  # The file is named for the depth actually reached, not the one asked for.
  # A file called d7 holding six levels is the kind of thing that is believed
  # later and wastes an afternoon.
  out <- file.path(dir, sprintf("phase%d_d%d.bin", ph, f$built_depth))
  if (cayleyR:::cube_kociemba4_save_phase_cpp(out, ph)) {
    cat(sprintf("  saved %s (%s bytes)\n", basename(out),
                fmt(file.size(out))))
  } else {
    cat(sprintf("  FAILED to write %s\n", out))
  }

  rows[[ph]] <- data.frame(phase = ph, depth = f$built_depth,
                           entries = f$n_writes, slots = f$size,
                           secs = el, action = "filled",
                           stringsAsFactors = FALSE)
}

hr("summary")

tab <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
cat(sprintf("  %5s %6s %14s %14s %8s %9s\n",
            "phase", "depth", "entries", "slots", "full", "secs"))
for (i in seq_len(nrow(tab))) {
  cat(sprintf("  %5d %6d %14s %14s %7.1f%% %9.1f\n",
              tab$phase[i], tab$depth[i], fmt(tab$entries[i]),
              fmt(tab$slots[i]), 100 * tab$entries[i] / tab$slots[i],
              tab$secs[i]))
}
cat(sprintf("\n  total fill time: %.1f s\n", sum(tab$secs)))
cat(sprintf("  files in %s\n", dir))

cat("\n  These are files, not a configuration: nothing loads them on its own.\n")
cat("  A run that wants them has to call cube_kociemba4_load_phase_cpp() for\n")
cat("  each phase before it solves anything, and loading pins the table's size\n")
cat("  so no search can grow it and throw the contents away.\n")
