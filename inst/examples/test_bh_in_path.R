library(cayleyR)

# === find_path_bfs test ===

n <- 20
k <- 4
start_state <- 1:n

final_state <- generate_state(n, k, n_moves = 1000)
#final_state <- convert_digits("1 3 19 18 4 20 2 7 5 6 8 9 10 11 12 13 14 15 16 17")

start_time <- Sys.time()
result <- find_path_bfs(
  start_state, final_state, k = k,
  bfs_levels = 200, bfs_n_hubs = 7, bfs_n_random = 3,
  highway_distance_method = "manhattan",   # pairs the two highway ends
  iterative_distance_method = "human",     # guides the search between the hubs
  verbose = TRUE,
  # parameters forwarded to find_path_iterative (via ...)
  moves = c("1", "2", "3"),
  combo_length = 25,
  n_samples = 400,
  n_top = 100,
  max_iterations = 150,
  potc = 1,
  ptr = 3,
  opd = TRUE,
  reuse_combos = FALSE,
  sort_by = c("longest", "most_unique")
)
elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

# === Shorten the path with short_path_bfs ===
shortened <- NULL
short_elapsed <- NA
if (result$found) {
  depth <- 9L
  short_start <- Sys.time()
  shortened <- short_path_bfs(result$path, start_state, k, depth = depth)
  short_elapsed <- as.numeric(difftime(Sys.time(), short_start, units = "secs"))
}

# === Report ===
cat("\n=========================================================\n")
cat("RESULT  (n =", n, " k =", k, ")\n")
cat("=========================================================\n")
if (!result$found) {
  cat("  path not found (cycles:", result$cycles, ") in",
      round(elapsed, 2), "sec\n")
} else {
  raw <- length(result$path)
  short <- if (!is.null(shortened)) shortened$new_length else NA_integer_
  total_sec <- elapsed + (if (is.na(short_elapsed)) 0 else short_elapsed)

  cat(sprintf("  %-22s %8s %8s %8s\n", "", "raw", "short", "sec"))
  cat(sprintf("  %-22s %8d %8s %8.2f\n", "find_path_bfs", raw,
              if (is.na(short)) "-" else format(short), total_sec))
  if (!is.null(shortened)) {
    cat(sprintf("\n  short_path_bfs (depth %d): saved %d ops (%.0f%%)\n",
                depth, shortened$savings, 100 * shortened$savings / raw))
  }
  cat(sprintf("  cycles: %d\n", result$cycles))
}
cat("\n")

# === Write results to CSV ===
out_dir <- file.path(system.file("examples", package = "cayleyR"), "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 1. Main info (key/value, one per row)
info_list <- list(
  n = n,
  k = k,
  start = paste(start_state, collapse = " "),
  final = paste(final_state, collapse = " "),
  found = result$found,
  cycles = result$cycles,
  path_length = if (result$found) length(result$path) else NA,
  elapsed_sec = round(elapsed, 2),
  short_path_length = if (!is.null(shortened)) shortened$new_length else NA,
  short_savings = if (!is.null(shortened)) shortened$savings else NA,
  short_elapsed_sec = if (!is.null(shortened)) round(short_elapsed, 2) else NA
)
info_df <- data.frame(
  key = names(info_list),
  value = as.character(info_list),
  stringsAsFactors = FALSE
)
write.table(info_df, file = file.path(out_dir, "path_info.csv"), sep = ",", row.names = FALSE)

# 2. Full path (two columns: key=path, value=operations run together)
if (result$found) {
  path_df <- data.frame(
    key = "path",
    value = paste(result$path, collapse = ""),
    stringsAsFactors = FALSE
  )
  write.table(path_df, file = file.path(out_dir, "path_full.csv"), sep = ",", row.names = FALSE)
}

# 3. Shortened path (two columns: key=path, value=operations run together)
if (!is.null(shortened)) {
  short_df <- data.frame(
    key = "path",
    value = paste(shortened$path, collapse = ""),
    stringsAsFactors = FALSE
  )
  write.table(short_df, file = file.path(out_dir, "path_short.csv"), sep = ",", row.names = FALSE)
}

# 4. BFS info
if (length(result$bfs_info) > 0) {
  bfs_df <- data.frame(
    key = names(result$bfs_info),
    value = as.character(result$bfs_info),
    stringsAsFactors = FALSE
  )
  write.table(bfs_df, file = file.path(out_dir, "path_bfs_info.csv"), sep = ",", row.names = FALSE)
}

# 5. Bridge states (start) - always written (bridges exist even if no path was found)
bs_start <- do.call(rbind, lapply(result$bridge_states_start, function(b) {
  data.frame(
    state = paste(b$state, collapse = " "),
    cycle = if (length(b$cycle) > 0) b$cycle else NA,
    label = if (length(b$label) > 0) b$label else NA,
    stringsAsFactors = FALSE
  )
}))
write.table(bs_start, file = file.path(out_dir, "bridge_states_start.csv"), sep = ",", row.names = FALSE)

# 6. Bridge states (final) - always written
bs_final <- do.call(rbind, lapply(result$bridge_states_final, function(b) {
  data.frame(
    state = paste(b$state, collapse = " "),
    cycle = if (length(b$cycle) > 0) b$cycle else NA,
    label = if (length(b$label) > 0) b$label else NA,
    stringsAsFactors = FALSE
  )
}))
write.table(bs_final, file = file.path(out_dir, "bridge_states_final.csv"), sep = ",", row.names = FALSE)

cat("Done. Files in", out_dir, ":\n")
cat("  path_info.csv              - parameters and statistics\n")
cat("  path_full.csv              - full path\n")
cat("  path_short.csv             - shortened path\n")
cat("  path_bfs_info.csv          - BFS info\n")
cat("  bridge_states_start.csv    - bridges from start\n")
cat("  bridge_states_final.csv    - bridges from final\n")
