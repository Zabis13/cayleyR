library(cayleyR)

# === Тест find_path_bfs ===

n <- 20
k <- 4
start_state <- 1:n

final_state <- generate_state(n, k, n_moves = 20)
#final_state <- convert_digits("1 3 19 18 4 20 2 7 5 6 8 9 10 11 12 13 14 15 16 17")

cat("Start:", paste(start_state, collapse = " "), "\n")
cat("Final:", paste(final_state, collapse = " "), "\n\n")

start_time <- Sys.time()
result <- find_path_bfs(
  start_state, final_state, k = k,
  bfs_levels = 200, bfs_n_hubs = 7, bfs_n_random = 3,
  distance_method = "manhattan",
  # параметры для find_path_iterative (через ...)
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
elapsed <- difftime(Sys.time(), start_time, units = "secs")

cat("\nВремя:", round(elapsed, 1), "сек\n")
cat("Найден:", result$found, "\n")
cat("Циклов:", result$cycles, "\n")
if (result$found) cat("Длина пути:", length(result$path), "\n")
cat("BFS info:", paste(names(result$bfs_info), result$bfs_info, sep = "=", collapse = ", "), "\n")

# === Сокращение пути через short_path_bfs ===
if (result$found) {
  depth <- 9L
  cat("\n--- short_path_bfs (depth =", depth, ") ---\n")
  short_start <- Sys.time()
  shortened <- short_path_bfs(result$path, start_state, k, depth = depth)
  short_elapsed <- difftime(Sys.time(), short_start, units = "secs")
  cat("До:", shortened$original_length, "\n")
  cat("После:", shortened$new_length, "\n")
  cat("Экономия:", shortened$savings, "\n")
  cat("Время:", round(short_elapsed, 1), "сек\n")
  cat("Путь:", paste(shortened$path, collapse = " "), "\n")
}
