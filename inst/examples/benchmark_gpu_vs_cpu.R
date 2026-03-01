#!/usr/bin/env Rscript
# Benchmark: store_analyze_combos (CPU/C++) vs store_analyze_combos_gpu (GPU/R+ggml)
#
# Usage:
#   Rscript inst/examples/benchmark_gpu_vs_cpu.R
#   Rscript inst/examples/benchmark_gpu_vs_cpu.R --n 14 --k 4 --n_top 10 --reps 3

library(cayleyR)

# --- Parse args ---
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default) {
  idx <- match(flag, args)
  if (!is.na(idx) && idx < length(args)) return(args[idx + 1])
  default
}

n     <- as.integer(get_arg("--n", 20))
k     <- as.integer(get_arg("--k", 4))
n_top <- as.integer(get_arg("--n_top", 100))
reps  <- as.integer(get_arg("--reps", 3))

cat("========================================\n")
cat("Benchmark: CPU vs GPU store_analyze_combos\n")
cat("========================================\n")
cat("n =", n, " k =", k, " n_top =", n_top, " reps =", reps, "\n")

# --- Check GPU ---
gpu_available <- tryCatch(cayley_gpu_available(), error = function(e) FALSE)
cat("GPU available:", gpu_available, "\n")
if (gpu_available) {
  cayley_gpu_status()
}
cat("========================================\n\n")

if (!gpu_available) {
  cat("GPU not available, cannot run comparison. Exiting.\n")
  q(status = 0)
}

# --- Prepare data ---
set.seed(42)
start_state <- 1:n

# Generate combos once (same for both)
top_combos <- find_best_random_combinations(
  moves = c("1", "2", "3"),
  combo_length = 10,
  n_samples = 200,
  n_top = n_top,
  start_state = start_state,
  k = k
)

cat("Top combos:\n")
print(top_combos[, c("combination", "total_moves", "unique_states_count")])
cat("\n")

# --- Benchmark function ---
run_bench <- function(label, func, reps) {
  times <- numeric(reps)
  sizes <- integer(reps)
  for (r in seq_len(reps)) {
    store <- cayleyR:::create_state_store(n)
    gc(verbose = FALSE)
    t0 <- proc.time()["elapsed"]
    func(store, top_combos, start_state, k, 1L)
    t1 <- proc.time()["elapsed"]
    times[r] <- t1 - t0
    sizes[r] <- cayleyR:::state_store_size(store)
  }
  list(label = label, times = times, size = sizes[1])
}

# --- Run CPU ---
cat("--- Running CPU (C++) ---\n")
flush.console()
res_cpu <- run_bench("CPU (C++)", cayleyR:::store_analyze_combos, reps)
cat(sprintf("  Times: %s\n", paste(round(res_cpu$times, 4), collapse = ", ")))
cat(sprintf("  Mean: %.4f sec\n", mean(res_cpu$times)))
cat(sprintf("  Store size: %d states\n\n", res_cpu$size))
flush.console()

# --- Run GPU ---
cat("--- Running GPU (R + ggml Vulkan) ---\n")
flush.console()
# Warm up GPU
cayley_gpu_init()
res_gpu <- run_bench("GPU (R+ggml)", cayleyR:::store_analyze_combos_gpu, reps)
cat(sprintf("  Times: %s\n", paste(round(res_gpu$times, 4), collapse = ", ")))
cat(sprintf("  Mean: %.4f sec\n", mean(res_gpu$times)))
cat(sprintf("  Store size: %d states\n\n", res_gpu$size))
flush.console()

# --- Summary ---
cpu_mean <- mean(res_cpu$times)
gpu_mean <- mean(res_gpu$times)
ratio <- gpu_mean / cpu_mean

cat("========================================\n")
cat("RESULTS\n")
cat("========================================\n")
cat(sprintf("%-20s %10s %10s %10s\n", "Method", "Mean(sec)", "Min(sec)", "Max(sec)"))
cat(paste(rep("-", 55), collapse = ""), "\n")
cat(sprintf("%-20s %10.4f %10.4f %10.4f\n", "CPU (C++)",
            cpu_mean, min(res_cpu$times), max(res_cpu$times)))
cat(sprintf("%-20s %10.4f %10.4f %10.4f\n", "GPU (R+ggml)",
            gpu_mean, min(res_gpu$times), max(res_gpu$times)))
cat(paste(rep("-", 55), collapse = ""), "\n")
cat(sprintf("GPU/CPU ratio: %.2fx\n", ratio))
if (ratio > 1) {
  cat(sprintf("=> GPU is %.1fx SLOWER than CPU\n", ratio))
} else {
  cat(sprintf("=> GPU is %.1fx FASTER than CPU\n", 1/ratio))
}
cat(sprintf("Store sizes: CPU=%d, GPU=%d (match: %s)\n",
            res_cpu$size, res_gpu$size,
            ifelse(res_cpu$size == res_gpu$size, "YES", "NO")))
cat("========================================\n")
