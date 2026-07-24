# Compare cycle_shortcut() against short_path_bfs() on a path from
# human_algorithm_to().
#
# The two shorteners look for the same thing -- a stretch of path that some
# other route covers in fewer operations -- but reach for it differently.
# short_path_bfs() sweeps the BFS neighbourhood a few steps deep, so it sees
# only nearby rejoin points. cycle_shortcut() spins combo words into cycles
# that wander far from where they started, which costs much more but can reach
# rejoin points BFS depth cannot.
#
# Every row is verified by applying the shortened path to the start state and
# checking it still lands on the target; an unverified row is reported rather
# than quietly counted as a win.

library(cayleyR)

n <- 2000L
k <- 4L
n_moves <- 500L
#seed <- 2L

cycle_points <- c(20L)
#cycle_points <- c(20L, 60L)   # n_points values to try
bfs_depths <- c(6L)           # short_path_bfs depths to try

# How the cycles themselves are built. These are the knobs that decide what
# cycle_shortcut() actually gets to look at: n_samples combo words of
# combo_length operations each are drawn at every point, ranked by sort_by,
# and the best n_top are unrolled until they close or hit max_cycle_len.
moves <- c("1", "2", "3")
combo_length <- 15L
n_samples <- 400L
n_top <- 100L
max_cycle_len <- 2000000L

# OpenMP threads for the combo scoring and search. NULL leaves cycle_shortcut()
# to its default of two below the core count; set a number to pin it. The
# points themselves stay sequential -- each searches the path the one before it
# shortened -- so this only widens the per-point work.

n_threads <- NULL

# Ranking has to unroll every one of the n_samples candidates to score it, so
# it is worth seeing what it buys against taking combos as they come. NULL is
# the no-ranking case.
sort_variants <- list(c("longest", "most_unique"), NULL)

#set.seed(seed)
start <- generate_state(n, k, n_moves = n_moves)
target <- seq_len(n)

cat(sprintf("n = %d, k = %d, scramble = %d moves\n", n, k, n_moves))
cat(sprintf("cycles: combo_length = %d, n_samples = %d, n_top = %d, max_cycle_len = %d\n",
            combo_length, n_samples, n_top, max_cycle_len))
cat(sprintf("threads: %s of %d cores\n",
            if (is.null(n_threads)) sprintf("%d (default)", max(1L, openmp_threads() - 2L))
            else as.character(n_threads),
            openmp_threads()))

solved <- human_algorithm_to(start, target, k = k, simplify = FALSE)
if (!solved$found) stop("human_algorithm_to failed to solve the start state")
path <- solved$path
cat(sprintf("human_algorithm_to: %d operations\n", length(path)))

# Time one shortener and check what it returns actually works.
run_case <- function(label, shorten) {
  t0 <- proc.time()[["elapsed"]]
  res <- shorten(path)
  elapsed <- proc.time()[["elapsed"]] - t0

  reached <- apply_operations(start, res$path, k, compute_coords = FALSE)$state
  ok <- identical(as.integer(reached), as.integer(target))

  cat(sprintf("  %-52s done in %.1fs\n", label, elapsed))

  list(
    label = label,
    original = length(path),
    new = length(res$path),
    savings = length(path) - length(res$path),
    elapsed = elapsed,
    verified = ok
  )
}

cat("\nrunning:\n")

# One row per (n_points, sort_by) pair, then the BFS shortener for reference.
cycle_cases <- expand.grid(np = cycle_points, sv = seq_along(sort_variants))

cases <- c(
  lapply(seq_len(nrow(cycle_cases)), function(i) {
    np <- cycle_cases$np[i]
    sb <- sort_variants[[cycle_cases$sv[i]]]
    label <- sprintf("cycle_shortcut, n_points=%d, %s", np,
                     if (is.null(sb)) "no ranking" else paste(sb, collapse = "+"))
    run_case(label, function(p) {
      set.seed(42)
      cycle_shortcut(p, start, k = k, n_points = np,
                     moves = moves, combo_length = combo_length,
                     n_samples = n_samples, n_top = n_top, sort_by = sb,
                     max_cycle_len = max_cycle_len, n_threads = n_threads)
    })
  }),
  lapply(bfs_depths, function(d) {
    run_case(sprintf("short_path_bfs, depth=%d", d), function(p) {
      short_path_bfs(p, start, k, depth = d)
    })
  })
)

# Box-drawing table: pad every cell to its column width, then draw a rule
# between rows. Widths come from the header and the cells themselves, so the
# frame stays aligned whatever the numbers turn out to be.
#
# The frame characters are written as \u escapes rather than literally. Rscript
# takes the locale of whatever shell starts it, and a literal multi-byte
# character in the source breaks the parser in a non-UTF-8 one -- the script
# would run to completion and then die on this very print. Escaped, the file
# stays plain ASCII on disk and R builds the characters at run time.
print_boxed <- function(header, cells) {
  widths <- pmax(nchar(header), apply(nchar(cells), 2, max))

  rule <- function(left, mid, right) {
    cat(left, paste(vapply(widths + 2L, strrep, character(1), x = "\u2500"),
                    collapse = mid), right, "\n", sep = "")
  }
  line <- function(values) {
    padded <- paste0(values, strrep(" ", widths - nchar(values)))
    cat("\u2502 ", paste(padded, collapse = " \u2502 "), " \u2502\n", sep = "")
  }

  rule("\u250c", "\u252c", "\u2510")
  line(header)
  for (i in seq_len(nrow(cells))) {
    rule("\u251c", "\u253c", "\u2524")
    line(cells[i, ])
  }
  rule("\u2514", "\u2534", "\u2518")
}

# Sub-second timings are reported as such: printing "0.0s" for short_path_bfs
# next to three minutes of cycle_shortcut would understate the gap.
format_time <- function(s) {
  if (s < 1) "under 1s" else sprintf("%.1fs", s)
}

cat("\n")
print_boxed(
  c("label", "raw", "short", "saved", "sec"),
  cbind(
    vapply(cases, function(x) x$label, character(1)),
    vapply(cases, function(x) as.character(x$original), character(1)),
    vapply(cases, function(x) as.character(x$new), character(1)),
    vapply(cases, function(x) as.character(x$savings), character(1)),
    vapply(cases, function(x) format_time(x$elapsed), character(1))
  )
)

failed <- Filter(function(x) !x$verified, cases)
if (length(failed) > 0) {
  cat("\nVERIFICATION FAILED:\n")
  for (f in failed) cat("  ", f$label, "\n", sep = "")
} else {
  cat("\nall paths verified\n")
}
