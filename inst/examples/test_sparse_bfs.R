library(cayleyR)

# === Sparse BFS: Look-ahead + Hybrid Selection ===

n <- 10
k <- 4
start_state <- 1:n

cat("=== Sparse BFS ===\n")
cat("n =", n, " k =", k, "\n")
cat("Start:", paste(start_state, collapse = " "), "\n\n")

# --- Запуск (без ограничений — работает до затухания) ---
t1 <- system.time({
  result <- sparse_bfs(start_state, k = k, n_hubs = 7, n_random = 3)
})

cat("Время:", t1["elapsed"], "сек\n")
cat("Рёбер:", nrow(result), "\n")
cat("Уникальных состояний:", length(unique(c(result$parent_key, result$child_key))), "\n")
cat("Макс уровень:", max(result$level), "\n\n")

# --- Состояний по уровням (первые и последние) ---
lvl_table <- table(result$level)
cat("Состояний на уровень (первые 10):\n")
print(head(lvl_table, 10))
cat("...\n")
cat("Последние 5 уровней:\n")
print(tail(lvl_table, 5))
cat("\n")

# --- Таблица переходов ---
cat("Таблица переходов (первые 15 строк):\n")
print(head(result, 15))
cat("\n")

# --- Восстановление пути ---
target_key <- result$child_key[nrow(result)]
path <- reconstruct_bfs_path(result, target_key)

cat("=== Восстановление пути ===\n")
cat("Цель:", target_key, "\n")
cat("Путь:", paste(path, collapse = " -> "), "\n")
cat("Длина:", length(path), "\n\n")

# --- Проверка пути ---
res <- apply_operations(start_state, path, k = k)
result_key <- paste(res$state, collapse = "_")
cat("Результат:", result_key, "\n")
cat("Совпадение:", result_key == target_key, "\n")

# === Интеграция с find_path_bfs ===
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
cat("\nВремя:", t2["elapsed"], "сек\n")
cat("Найден:", res2$found, " Циклов:", res2$cycles, "\n")
if (res2$found) cat("Длина пути:", length(res2$path), "\n")
