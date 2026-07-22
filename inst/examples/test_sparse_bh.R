library(cayleyR)

# === Sparse BFS: Look-ahead + Hybrid Selection ===

n <- 10
k <- 4
start_state <- 1:n

cat("=== Sparse BFS ===\n")
cat("n =", n, " k =", k, "\n")
cat("Start:", paste(start_state, collapse = " "), "\n\n")

# --- Run (unbounded: goes until it dies out) ---
t1 <- system.time({
  result <- sparse_bfs(start_state, k = k, n_hubs = 7, n_random = 3)
})

cat("Elapsed:", t1["elapsed"], "sec\n")
cat("Edges:", nrow(result), "\n")
cat("Unique states:", length(unique(c(result$parent_key, result$child_key))), "\n")
cat("Max level:", max(result$level), "\n\n")

# --- States per level (first and last) ---
lvl_table <- table(result$level)
cat("States per level (first 10):\n")
print(head(lvl_table, 10))
cat("...\n")
cat("Last 5 levels:\n")
print(tail(lvl_table, 5))
cat("\n")

# --- Transition table ---
cat("Transition table (first 15 rows):\n")
print(head(result, 15))
cat("\n")

# --- Path reconstruction ---
target_key <- result$child_key[nrow(result)]
path <- reconstruct_bfs_path(result, target_key)

cat("=== Path reconstruction ===\n")
cat("Target:", target_key, "\n")
cat("Path:", paste(path, collapse = " -> "), "\n")
cat("Length:", length(path), "\n\n")

# --- Path verification ---
res <- apply_operations(start_state, path, k = k)
result_key <- paste(res$state, collapse = "_")
cat("Result:", result_key, "\n")
cat("Match:", result_key == target_key, "\n")

# === Integration with find_path_bfs ===
cat("\n\n=== find_path_bfs ===\n")
set.seed(123)
final_state <- generate_state(n, k)
cat("Start:", paste(start_state, collapse = " "), "\n")
cat("Final:", paste(final_state, collapse = " "), "\n\n")

t2 <- system.time({
  res2 <- find_path_bfs(start_state, final_state, k = k,
    bfs_levels = 500,
    combo_length = 20, n_samples = 200, n_top = 50,
    max_iterations = 20)
})
cat("\nElapsed:", t2["elapsed"], "sec\n")
cat("Found:", res2$found, " Cycles:", res2$cycles, "\n")
if (res2$found) cat("Path length:", length(res2$path), "\n")
