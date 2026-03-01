library(cayleyR)

# === Тест BFS-интеграции в find_path_iterative ===

n <- 11
k <- 4
start_state <- 1:n
#set.seed(42)
#final_state <- generate_state(n)
final_state <- convert_digits("1 10 2 4 3 11 6 5 7 8 9")


cat("Start:", paste(start_state, collapse = " "), "\n")
cat("Final:", paste(final_state, collapse = " "), "\n\n")

start_time <- Sys.time()
result <- find_path_iterative(
  start_state, final_state, k = k,
  combo_length = 25, n_samples = 200, n_top = 120,
  max_iterations = 50, potc = 1, ptr = 3, opd = TRUE,
  reuse_combos = FALSE
)
elapsed <- difftime(Sys.time(), start_time, units = "secs")

cat("\nВремя:", round(elapsed, 1), "сек\n")
cat("Найден:", result$found, "\n")
cat("Циклов:", result$cycles, "\n")
if (result$found) cat("Длина пути:", length(result$path), "\n")

# === Bridge states для отладки ===
cat("\n--- Bridge states START ---\n")
for (i in seq_along(result$bridge_states_start)) {
  bs <- result$bridge_states_start[[i]]
  cat("  [", i, "] cycle:", bs$cycle,
      " state:", paste(bs$state, collapse = " "), "\n")
}

cat("\n--- Bridge states FINAL ---\n")
for (i in seq_along(result$bridge_states_final)) {
  bs <- result$bridge_states_final[[i]]
  cat("  [", i, "] cycle:", bs$cycle,
      " state:", paste(bs$state, collapse = " "), "\n")
}

# Manhattan между последними bridge-ами
last_s <- result$bridge_states_start[[length(result$bridge_states_start)]]$state
last_f <- result$bridge_states_final[[length(result$bridge_states_final)]]$state
cat("\nПоследние bridge states Manhattan:", sum(abs(last_s - last_f)), "\n")
