library(cayleyR)

# === Solving a state, navigated by phase 1 of the human algorithm ===
#
# Differences from find_path_bfs:
#   - no BFS highways; the search runs in one direction only, final -> 1:n
#   - the next move is chosen not by Manhattan distance but by phase 1 of
#     human_algorithm: maximise run_length (the sorted run 1,2,...,r), and on
#     ties minimise gap_cost (|distance(m-1, m) - k|)
#
# Candidates are composite moves "rotate the ring, then X" -- exactly the ones
# phase 1 weighs. Ranking single L/R/X gives no signal: a one-position rotation
# changes neither the run nor the gap.
#
# Layout: find_path_iterative with distance_method "human" does the searching,
# and short_path_bfs shortens whatever it produces.
#
# use_phase1 decides whether phase 1 of the human algorithm runs first:
#   TRUE  -- phase 1 drives the state to the tail, then the search closes the
#            tail. Fewer ops to search for, but the run phase 1 built is fixed
#            and the search cannot revisit it.
#   FALSE -- the search runs from final_state all the way to 1:n on its own,
#            guided only by the "human" bridge criterion.
# Either way the scoring is the same; the switch is only about who builds the
# run, the human rule or the search.
#
# The whole search runs one way, final_state -> 1:n: that is the direction the
# navigator works in, since it can only build the sorted run up, never take it
# apart. The solving path is the answer; it is not turned back around.

n <- 21
k <- 4
solved_state <- 1:n   # the goal: the sorted ring

use_phase1 <- TRUE    # run phase 1 of the human algorithm before searching
#use_phase1 <- FALSE  


#set.seed(42)
final_state <- generate_state(n, k, n_moves = 997)
#final_state <- convert_digits("1 3 19 18 4 20 2 7 5 6 8 9 10 11 12 13 14 15 16 17")

cat("solved:", solved_state, "\n")
cat("final:", final_state, "\n\n")

# === Step 1: phase 1 as navigation (optional) ===

if (use_phase1) {
  cat("Step 1. Phase 1 (navigating from final):\n")
  t0 <- Sys.time()
  nav <- human_phase1_navigate(final_state, k = k, trace = TRUE)
  nav_elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  cat("  run built:", nav$run, "of", n, "\n")
  cat("  operations spent:", length(nav$path), "\n")
  cat("  state after phase 1:", nav$state, "\n")
  cat("  elapsed:", round(nav_elapsed, 2), "sec\n\n")
} else {
  cat("Step 1. Phase 1 skipped (use_phase1 = FALSE)\n\n")
  nav <- list(state = as.integer(final_state), path = character(0),
              run = run_length(final_state), trace = NULL)
  nav_elapsed <- 0
}

# === Step 2: search the remainder with find_path_iterative ===
#
# With phase 1 on this is just the tail: phase 1 cannot act on the last few
# tiles (it needs three intermediate tiles and there are none left). With it
# off this is the whole problem.
#
# distance_method = "human" scores bridges by how much of the sorted run is
# still missing (n - run_length) rather than by Manhattan displacement, so the
# search is guided by the same criterion phase 1 uses. The target is 1:n, which
# is what that method assumes.

cat("Step 2. Search to 1:n (find_path_iterative", 
    if (use_phase1) "- tail only" else "- full", "):\n")
t1 <- Sys.time()
tail_res <- find_path_iterative(
  nav$state, solved_state, k = k,
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
  verbose = FALSE   # TRUE also dumps the whole path, thousands of ops long
)
tail_elapsed <- as.numeric(difftime(Sys.time(), t1, units = "secs"))

if (!tail_res$found) {
  cat("  not found (found = FALSE), cycles:", tail_res$cycles, "\n")
} else {
  cat("  operations from the search:", length(tail_res$path), "\n")
  cat("  cycles:", tail_res$cycles, "\n")
}
cat("  elapsed:", round(tail_elapsed, 2), "sec\n\n")

# === Step 3: assemble the solving path ===
#
# nav$path + tail$path lead final_state -> 1:n, which is the path we want.

found <- FALSE
path_full <- NULL

if (tail_res$found) {
  path_full <- c(nav$path, tail_res$path)

  chk <- as.integer(
    apply_operations(final_state, path_full, k, compute_coords = FALSE)$state
  )
  found <- identical(chk, as.integer(solved_state))

  cat("Step 3. Full path:", length(path_full), "operations\n")
  cat("  verification (final -> 1:n):", if (found) "OK" else "FAILED", "\n\n")
}

# === Step 4: shorten via short_path_bfs ===

depth <- 9L
shortened <- NULL
short_elapsed <- NA
if (found) {
  cat("Step 4. Shortening (short_path_bfs, depth =", depth, "):\n")
  t2 <- Sys.time()
  shortened <- short_path_bfs(path_full, final_state, k, depth = depth)
  short_elapsed <- as.numeric(difftime(Sys.time(), t2, units = "secs"))
  cat("  now:", shortened$new_length, "operations (saved",
      shortened$savings, ")\n")
  cat("  elapsed:", round(short_elapsed, 2), "sec\n\n")
}

# === For comparison: human_algorithm on its own ===
#
# Given the same short_path_bfs treatment, so the two lengths are comparable.

t3 <- Sys.time()
plain <- human_algorithm(final_state, k = k, simplify = FALSE)
plain_raw <- if (plain$found) length(plain$path) else NA_integer_
plain_short <- NULL
if (plain$found) {
  plain_short <- short_path_bfs(plain$path, final_state, k, depth = depth)
}
plain_elapsed <- as.numeric(difftime(Sys.time(), t3, units = "secs"))

# === Comparison ===

cat("=========================================================\n")
cat("COMPARISON  (n =", n, " k =", k, ")\n")
cat("=========================================================\n")

row <- function(label, raw, short, sec) {
  cat(sprintf("  %-22s %8s %8s %8.2f\n", label,
              if (is.na(raw)) "-" else format(raw),
              if (is.na(short)) "-" else format(short), sec))
}

cat(sprintf("  %-22s %8s %8s %8s\n", "", "raw", "short", "sec"))
row(if (use_phase1) "phase1 + search" else "search only",
    if (found) length(path_full) else NA_integer_,
    if (!is.null(shortened)) shortened$new_length else NA_integer_,
    nav_elapsed + tail_elapsed + (if (is.na(short_elapsed)) 0 else short_elapsed))
row("human_algorithm", plain_raw,
    if (!is.null(plain_short)) plain_short$new_length else NA_integer_,
    plain_elapsed)

if (found && !is.null(plain_short)) {
  cat(sprintf("\n  ratio (ours / human): %.2fx ops\n",
              shortened$new_length / plain_short$new_length))
}

if (use_phase1) {
  cat(sprintf("\n  phase 1 built a run of %d/%d in %d ops; the search closed the rest\n",
              nav$run, n, length(nav$path)))
}
cat("\n")

# === Write results to CSV ===

out_dir <- file.path(system.file("examples", package = "cayleyR"), "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

info_list <- list(
  n = n,
  k = k,
  solved = paste(solved_state, collapse = " "),
  final = paste(final_state, collapse = " "),
  found = found,
  use_phase1 = use_phase1,
  nav_run = nav$run,
  nav_ops = length(nav$path),
  nav_elapsed_sec = round(nav_elapsed, 2),
  tail_found = tail_res$found,
  tail_ops = if (tail_res$found) length(tail_res$path) else NA,
  tail_cycles = tail_res$cycles,
  tail_elapsed_sec = round(tail_elapsed, 2),
  path_length = if (found) length(path_full) else NA,
  short_path_length = if (!is.null(shortened)) shortened$new_length else NA,
  short_savings = if (!is.null(shortened)) shortened$savings else NA,
  short_elapsed_sec = if (!is.null(shortened)) round(short_elapsed, 2) else NA,
  plain_path_length = plain_raw,
  plain_short_length = if (!is.null(plain_short)) plain_short$new_length else NA,
  plain_elapsed_sec = round(plain_elapsed, 2)
)
info_df <- data.frame(
  key = names(info_list),
  value = as.character(info_list),
  stringsAsFactors = FALSE
)
write.table(info_df, file = file.path(out_dir, "human_nav_info.csv"),
            sep = ",", row.names = FALSE)

if (found) {
  path_df <- data.frame(
    key = "path",
    value = paste(path_full, collapse = ""),
    stringsAsFactors = FALSE
  )
  write.table(path_df, file = file.path(out_dir, "human_nav_path.csv"),
              sep = ",", row.names = FALSE)
}

if (!is.null(shortened)) {
  short_df <- data.frame(
    key = "path",
    value = paste(shortened$path, collapse = ""),
    stringsAsFactors = FALSE
  )
  write.table(short_df, file = file.path(out_dir, "human_nav_path_short.csv"),
              sep = ",", row.names = FALSE)
}

# Navigator trace: shows the run growing step by step
if (!is.null(nav$trace)) {
  write.table(nav$trace, file = file.path(out_dir, "human_nav_trace.csv"),
              sep = ",", row.names = FALSE)
}

cat("Done. Files in", out_dir, ":\n")
cat("  human_nav_info.csv         - parameters and statistics\n")
cat("  human_nav_path.csv         - full path\n")
cat("  human_nav_path_short.csv   - shortened path\n")
cat("  human_nav_trace.csv        - navigator trace, step by step\n")
