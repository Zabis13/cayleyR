library(cayleyR)

# === Human-style solver ===
# Solves the ring the way a person does: grow a sorted run one value at a
# time, then finish the remaining tail with local cycle primitives.

n <- 20
k <- 4
start_state <- 1:n

final_state <- generate_state(n, k, n_moves = 1000)

cat("Start:", paste(start_state, collapse = " "), "\n")
cat("Final:", paste(final_state, collapse = " "), "\n\n")

# === Sorting: bring a scrambled ring back to 1..n ===

t0 <- Sys.time()
sorted <- human_algorithm(final_state, k = k)
sort_elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

cat("--- Sorting", paste(final_state, collapse = " "), "-> 1..n ---\n")
cat("Found:", sorted$found, "\n")
cat("Length:", sorted$length, "\n")
cat("Time:", round(sort_elapsed, 2), "sec\n\n")

# === Arbitrary target: start_state -> final_state ===
# Internally both states are sorted and one path is inverted.

t0 <- Sys.time()
result <- human_algorithm(start_state, final_state, k = k)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

cat("--- Path start -> final ---\n")
cat("Found:", result$found, "\n")
cat("Length:", result$length, "\n")
cat("Time:", round(elapsed, 2), "sec\n")

# Verify the path really lands on the target
if (result$found) {
  check <- apply_operations(start_state, result$path, k, compute_coords = FALSE)$state
  cat("Verified:", identical(as.integer(check), as.integer(final_state)), "\n")
}

# === Shorten the result ===
# human_algorithm() already runs short_position() on its output, so feeding
# result$path straight into short_path_bfs() has little left to cancel. Ask
# for the raw path instead to see what the shortener actually does.

raw <- human_algorithm(start_state, final_state, k = k, simplify = FALSE)

shortened <- NULL
short_elapsed <- NA
if (raw$found) {
  t0 <- Sys.time()
  shortened <- short_path_bfs(raw$path, start_state, k, depth = 9)
  short_elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  cat("\n--- Shortening ---\n")
  cat("Raw path:      ", raw$length, "\n")
  cat("After simplify:", result$length, "(built-in short_position)\n")
  cat("After BFS:     ", shortened$new_length,
      "(saved", shortened$savings, "from raw)\n")
  cat("Time:", round(short_elapsed, 2), "sec\n")
}

# === Save results ===

out_dir <- file.path(system.file("examples", package = "cayleyR"), "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

info_list <- list(
  n = n,
  k = k,
  start = paste(start_state, collapse = " "),
  final = paste(final_state, collapse = " "),
  found = result$found,
  path_length = if (result$found) result$length else NA,
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
write.table(info_df, file = file.path(out_dir, "human_info.csv"),
            sep = ",", row.names = FALSE)

if (result$found) {
  path_df <- data.frame(
    key = "path",
    value = paste(result$path, collapse = ""),
    stringsAsFactors = FALSE
  )
  write.table(path_df, file = file.path(out_dir, "human_path.csv"),
              sep = ",", row.names = FALSE)
}

if (!is.null(shortened)) {
  short_df <- data.frame(
    key = "path",
    value = paste(shortened$path, collapse = ""),
    stringsAsFactors = FALSE
  )
  write.table(short_df, file = file.path(out_dir, "human_path_short.csv"),
              sep = ",", row.names = FALSE)
}

cat("\nDone. Files in", out_dir, ":\n")
cat("  human_info.csv        - parameters and statistics\n")
cat("  human_path.csv        - full path\n")
cat("  human_path_short.csv  - shortened path\n")
