library(cayleyR)

# === Бенчмарк: растущий n (от 5 до 30), фиксированный n_moves ===
# Усложнение через увеличение размера перестановки, сложность - n_moves = 100

k <- 4
n_moves <- 20
depth <- 8L

n_seq <- 10:16
results <- vector("list", length(n_seq))

for (i in seq_along(n_seq)) {
  n <- n_seq[i]
  start_state <- 1:n
  cat("\n========================================\n")
  cat(sprintf("n = %d  (%d / %d)\n", n, i, length(n_seq)))
  cat("========================================\n")

  final_state <- generate_state(n, k, n_moves = n_moves)
  cat("final_state:", paste(final_state, collapse = " "), "\n\n")

  # --- find_path_bfs ---
  t0 <- Sys.time()
  result <- find_path_bfs(
    start_state, final_state, k = k,
    bfs_levels = 200, bfs_n_hubs = 7, bfs_n_random = 3,
    distance_method = "manhattan",
    verbose = TRUE,
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
  bfs_elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  # --- short_path_bfs ---
  short_len <- NA
  short_savings <- NA
  short_elapsed <- NA
  if (result$found) {
    t1 <- Sys.time()
    shortened <- short_path_bfs(result$path, start_state, k, depth = depth)
    short_elapsed <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
    short_len <- shortened$new_length
    short_savings <- shortened$savings
  }

  results[[i]] <- data.frame(
    n = n,
    k = k,
    n_moves = nm,
    final_state = paste(final_state, collapse = " "),
    found = result$found,
    cycles = result$cycles,
    path_length = if (result$found) length(result$path) else NA,
    short_path_length = short_len,
    short_savings = short_savings,
    bfs_time_sec = round(bfs_elapsed, 2),
    short_time_sec = if (!is.na(short_elapsed)) round(short_elapsed, 2) else NA,
    total_time_sec = round(bfs_elapsed + ifelse(is.na(short_elapsed), 0, short_elapsed), 2),
    stringsAsFactors = FALSE
  )
}

# === Итоговая таблица ===
stats <- do.call(rbind, results)

cat("\n\n========================================\n")
cat("ИТОГО: бенчмарк по n_moves\n")
cat(sprintf("n = %d, k = %d, n_moves от %d до %d\n", n, k, min(n_moves_seq), max(n_moves_seq)))
cat("========================================\n")
print(stats[, c("n_moves", "found", "cycles", "path_length",
                 "short_path_length", "short_savings",
                 "bfs_time_sec", "short_time_sec", "total_time_sec")])

cat(sprintf("\nНайдено: %d / %d\n", sum(stats$found), nrow(stats)))
cat(sprintf("Средняя длина пути: %.1f\n", mean(stats$path_length, na.rm = TRUE)))
cat(sprintf("Средняя длина после short: %.1f\n", mean(stats$short_path_length, na.rm = TRUE)))
cat(sprintf("Среднее время поиска: %.2f сек\n", mean(stats$bfs_time_sec, na.rm = TRUE)))
cat(sprintf("Среднее время short: %.2f сек\n", mean(stats$short_time_sec, na.rm = TRUE)))

# === CSV ===
out_dir <- file.path(system.file("examples", package = "cayleyR"), "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, "benchmark_n_moves.csv")
write.csv(stats, file = out_file, row.names = FALSE)
cat("\nCSV сохранён:", out_file, "\n")
