#!/usr/bin/env Rscript
# Benchmark: compare sort_by strategies for find_path_iterative
#
# Usage:
#   Rscript inst/examples/benchmark_sort_by.R
#   Rscript inst/examples/benchmark_sort_by.R --n 15 --k 5 --timeout 60

library(cayleyR)

# --- Parse args ---
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default) {
  idx <- match(flag, args)
  if (!is.na(idx) && idx < length(args)) return(args[idx + 1])
  default
}

n <- 14
k <- 4
timeout <- 30

# --- Generate states (no fixed seed — each run is different) ---
start_state <- 1:n
final_state <- generate_state(n, k, n_moves = 100)

cat("========================================\n")
cat("Benchmark: sort_by strategies\n")
cat("========================================\n")
cat("n =", n, " k =", k, " timeout =", timeout, "sec\n")
cat("Start:", paste(start_state, collapse = " "), "\n")
cat("Final:", paste(final_state, collapse = " "), "\n")
cat("========================================\n\n")

# --- Sort_by variants ---
sort_by_variants <- list(
  "longest"             = c("longest"),
  "most_unique"         = c("most_unique"),
  "most_repeated"       = c("most_repeated"),
  "least_repeated"      = c("least_repeated"),
  "longest+most_unique" = c("longest", "most_unique"),
  "most_unique+longest" = c("most_unique", "longest")
)

# --- Run benchmarks ---
results <- data.frame(
  sort_by     = character(),
  found       = logical(),
  time_sec    = numeric(),
  cycles      = integer(),
  path_length = integer(),
  stringsAsFactors = FALSE
)

for (name in names(sort_by_variants)) {
  cat("--- Running:", name, "---\n")
  flush.console()

  elapsed <- system.time({
    res <- tryCatch({
      setTimeLimit(elapsed = timeout)
      on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
      find_path_iterative(
        start_state = start_state,
        final_state = final_state,
        k = k,
        sort_by = sort_by_variants[[name]],
        max_iterations = 200,
        opd = TRUE,
        verbose = FALSE
      )
    },
    error = function(e) {
      list(found = FALSE, path = NULL, cycles = NA)
    })
  })["elapsed"]

  path_len <- if (!is.null(res$path)) length(res$path) else NA_integer_

  results <- rbind(results, data.frame(
    sort_by     = name,
    found       = res$found,
    time_sec    = round(elapsed, 1),
    cycles      = ifelse(is.na(res$cycles), NA_integer_, as.integer(res$cycles)),
    path_length = path_len,
    stringsAsFactors = FALSE
  ))

  cat("  Found:", res$found,
      " Time:", round(elapsed, 1), "s",
      " Cycles:", res$cycles,
      " Path:", path_len, "\n\n")
  flush.console()
}

# --- Summary table ---
cat("\n========================================\n")
cat("RESULTS SUMMARY\n")
cat("========================================\n")
cat(sprintf("%-25s %6s %8s %6s %8s\n", "sort_by", "found", "time(s)", "cycles", "path_len"))
cat(paste(rep("-", 60), collapse = ""), "\n")
for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf("%-25s %6s %8.1f %6s %8s\n",
    r$sort_by,
    ifelse(r$found, "YES", "NO"),
    r$time_sec,
    ifelse(is.na(r$cycles), "-", as.character(r$cycles)),
    ifelse(is.na(r$path_length), "-", as.character(r$path_length))
  ))
}
cat(paste(rep("-", 60), collapse = ""), "\n")
