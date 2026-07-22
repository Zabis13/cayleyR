library(cayleyR)

# === Benchmark: phase-1 navigation + search, versus the human algorithm ===
#
# Solves the same random states three ways, one row per state, and reports
# operations and time for each:
#
#   A. "nav+search" -- phase 1 of the human algorithm drives the state to the
#      tail, then find_path_iterative closes the tail with distance_method
#      "human", so bridge selection uses the same criterion as the navigator.
#
#   B. "search"     -- find_path_iterative with "human" on its own, no phase 1:
#      the search builds the whole run itself, guided by the same criterion.
#      Isolates what phase 1 contributes.
#
#   C. "human"      -- human_algorithm() end to end: phase 1 plus the memorised
#      3-cycles of phase 2.
#
# All three solve final -> 1:n and all three get the same post-processing
# (short_path_bfs at the same depth), so the lengths are comparable. Timings
# include that post-processing; the raw columns show the length before it.
#
# Note that C is not a search: phase 2 places the tail from a lookup table.
# A and B are searches guided by a human criterion, which is the point of
# comparing. A minus B is what phase 1 buys.

n <- 20
k <- 4
solved_state <- 1:n

n_points <- 10   # states to solve; each is solved by all three methods
n_moves <- 1000
depth <- 9L

# Search budget, shared by methods A and B.
tail_args <- list(
  moves = c("1", "2", "3"),
  combo_length = 25,
  n_samples = 400,
  n_top = 100,
  max_iterations = 150,
  potc = 1,
  ptr = 3,
  opd = TRUE,
  reuse_combos = FALSE,
  distance_method = "human",
  sort_by = c("longest", "most_unique"),
  verbose = FALSE
)

set.seed(2024)
points <- lapply(seq_len(n_points), function(i) generate_state(n, k, n_moves = n_moves))

# --- Methods A and B: search, with phase 1 optionally in front ----------------

solve_nav <- function(state, use_phase1 = TRUE) {
  t0 <- Sys.time()

  nav <- if (use_phase1) {
    human_phase1_navigate(state, k = k)
  } else {
    list(state = as.integer(state), path = character(0),
         run = run_length(state))
  }

  tail_res <- do.call(find_path_iterative,
                      c(list(nav$state, solved_state, k = k), tail_args))

  if (!tail_res$found) {
    return(list(found = FALSE, raw = NA_integer_, len = NA_integer_,
                sec = as.numeric(difftime(Sys.time(), t0, units = "secs")),
                run = nav$run, cycles = tail_res$cycles))
  }

  path <- c(nav$path, tail_res$path)
  short <- short_path_bfs(path, state, k, depth = depth)
  sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  ok <- identical(
    as.integer(apply_operations(state, short$path, k, compute_coords = FALSE)$state),
    as.integer(solved_state)
  )

  list(found = ok, raw = length(path), len = short$new_length,
       sec = sec, run = nav$run, cycles = tail_res$cycles)
}

# --- Method C: the human algorithm end to end ---------------------------------

solve_human <- function(state) {
  t0 <- Sys.time()

  res <- human_algorithm(state, k = k, simplify = FALSE)
  if (!res$found) {
    return(list(found = FALSE, raw = NA_integer_, len = NA_integer_,
                sec = as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }

  short <- short_path_bfs(res$path, state, k, depth = depth)
  sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  ok <- identical(
    as.integer(apply_operations(state, short$path, k, compute_coords = FALSE)$state),
    as.integer(solved_state)
  )

  list(found = ok, raw = length(res$path), len = short$new_length, sec = sec)
}

# --- Run ----------------------------------------------------------------------

cat("Benchmark: n =", n, " k =", k, " points =", n_points,
    " n_moves =", n_moves, " short depth =", depth, "\n")
cat("  A = phase1 + search   B = search only   C = human_algorithm\n")
cat("  ops are after short_path_bfs; sec includes it\n\n")

# One row per state: ops and seconds for each of the three methods.
hdr <- sprintf("%5s | %8s %7s | %8s %7s | %8s %7s",
               "point", "A ops", "A sec", "B ops", "B sec", "C ops", "C sec")
cat(hdr, "\n")
cat(strrep("-", nchar(hdr)), "\n")

# A = phase1 + search, B = search only, C = human_algorithm
ops_of <- function(r) if (isTRUE(r$found)) format(r$len) else "FAIL"

rows <- vector("list", n_points)

for (i in seq_len(n_points)) {
  st <- points[[i]]

  a <- solve_nav(st, use_phase1 = TRUE)
  s2 <- solve_nav(st, use_phase1 = FALSE)
  b <- solve_human(st)

  cat(sprintf("%5d | %8s %7.2f | %8s %7.2f | %8s %7.2f\n",
              i, ops_of(a), a$sec, ops_of(s2), s2$sec, ops_of(b), b$sec))
  flush.console()

  rows[[i]] <- data.frame(
    point = i,
    state = paste(st, collapse = " "),
    nav_found = a$found,
    nav_run = a$run,
    nav_cycles = a$cycles,
    nav_raw_ops = a$raw,
    nav_ops = a$len,
    nav_sec = round(a$sec, 3),
    search_found = s2$found,
    search_cycles = s2$cycles,
    search_raw_ops = s2$raw,
    search_ops = s2$len,
    search_sec = round(s2$sec, 3),
    human_found = b$found,
    human_raw_ops = b$raw,
    human_ops = b$len,
    human_sec = round(b$sec, 3),
    stringsAsFactors = FALSE
  )
}

stats <- do.call(rbind, rows)

# --- Summary ------------------------------------------------------------------

cat("\n")
cat("=====================================================\n")
cat("SUMMARY\n")
cat("=====================================================\n")

fmt <- function(label, found, ops, sec) {
  cat(sprintf("%-12s solved %d/%d | ops mean %7.1f  median %6.0f  min %5.0f  max %5.0f | sec mean %6.2f  total %7.2f\n",
              label, sum(found), length(found),
              mean(ops, na.rm = TRUE), median(ops, na.rm = TRUE),
              min(ops, na.rm = TRUE), max(ops, na.rm = TRUE),
              mean(sec, na.rm = TRUE), sum(sec, na.rm = TRUE)))
}

fmt("A phase1+srch", stats$nav_found, stats$nav_ops, stats$nav_sec)
fmt("B search", stats$search_found, stats$search_ops, stats$search_sec)
fmt("C human", stats$human_found, stats$human_ops, stats$human_sec)

both <- stats$nav_found & stats$human_found
if (any(both)) {
  cat("\nOn the", sum(both), "points both A and C solved:\n")
  cat(sprintf("  ops   A / C : %.2fx\n",
              mean(stats$nav_ops[both]) / mean(stats$human_ops[both])))
  cat(sprintf("  time  A / C : %.2fx\n",
              mean(stats$nav_sec[both]) / mean(stats$human_sec[both])))
  cat(sprintf("  A shorter on: %d / %d points\n",
              sum(stats$nav_ops[both] < stats$human_ops[both]), sum(both)))
}

cat("\nWhat phase 1 contributes (A vs B, points both solved):\n")
bs <- stats$nav_found & stats$search_found
if (any(bs)) {
  cat(sprintf("  ops   A / B : %.2fx\n",
              mean(stats$nav_ops[bs]) / mean(stats$search_ops[bs])))
  cat(sprintf("  time  A / B : %.2fx\n",
              mean(stats$nav_sec[bs]) / mean(stats$search_sec[bs])))
  cat(sprintf("  cycles mean  A %.1f | B %.1f\n",
              mean(stats$nav_cycles[bs]), mean(stats$search_cycles[bs])))
}

cat("\nBefore short_path_bfs (raw ops mean):\n")
cat(sprintf("  A %.1f | B %.1f | C %.1f\n",
            mean(stats$nav_raw_ops, na.rm = TRUE),
            mean(stats$search_raw_ops, na.rm = TRUE),
            mean(stats$human_raw_ops, na.rm = TRUE)))

# --- Write CSV ----------------------------------------------------------------

out_dir <- file.path(system.file("examples", package = "cayleyR"), "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, "benchmark_human_nav.csv")
write.table(stats, file = out_file, sep = ",", row.names = FALSE)

cat("\nCSV written:", out_file, "\n")
