library(cayleyR)

# === Тест find_path_bfs ===

n <- 20
k <- 4
start_state <- 1:n

final_state <- generate_state(n, k, n_moves = 20)
#final_state <- convert_digits("1 10 2 4 3 11 6 5 7 8 9")

cat("Start:", paste(start_state, collapse = " "), "\n")
cat("Final:", paste(final_state, collapse = " "), "\n\n")

start_time <- Sys.time()
result <- find_path_bfs(
  start_state, final_state, k = k,
  bfs_levels = 200, bfs_n_hubs = 7, bfs_n_random = 3,
  distance_method = "manhattan",
  # параметры для find_path_iterative (середина)
  combo_length = 25, n_samples = 400, n_top = 100,
  max_iterations = 150, potc = 1, ptr = 3, opd = TRUE
)
elapsed <- difftime(Sys.time(), start_time, units = "secs")

cat("\nВремя:", round(elapsed, 1), "сек\n")
cat("Найден:", result$found, "\n")
cat("Циклов:", result$cycles, "\n")
if (result$found) cat("Длина пути:", length(result$path), "\n")
cat("BFS info:", paste(names(result$bfs_info), result$bfs_info, sep = "=", collapse = ", "), "\n")

