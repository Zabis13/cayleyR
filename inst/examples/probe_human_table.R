library(cayleyR)

# === What governs build_table cost? ===
#
# build_table walks backwards from the solved ring. Its cost tracks the table
# size, which this sweep shows is set by k, not by the ring: the key space is
# TAIL! with TAIL = tail_size(k), so build time jumps with k (TAIL 8 -> 9 -> 10
# for k 4 -> 5 -> 6) while staying flat as n varies at fixed k. The block width
# bs = n - TAIL barely moves it -- an early guess that it did, which the sweep
# disproved.
#
# Run it to re-measure after touching build_table or the table caps. A row that
# hits `capped` built a partial table; a long `sec` row is an expensive k.
#
# Timings are per (n, k) build, so the sweep costs roughly the sum of the
# `sec` column; start with SMALL and widen once the shape is clear.

SMALL <- TRUE   # FALSE widens the sweep (slower)

ks <- if (SMALL) c(3, 4, 5) else c(3, 4, 5, 6, 7)
ns <- if (SMALL) 12:20 else 12:26

budget_sec <- 20   # give up on a (n, k) build past this, and skip wider ones

rows <- list()
for (k in ks) {
  tail_k <- max(8L, k + 4L)
  for (n in ns) {
    bs <- n - tail_k
    if (bs < 1) next   # no block at all; nothing for the check to prune on

    t0 <- Sys.time()
    # Internal Rcpp export: a measurement hook, not part of the public API.
    res <- cayleyR:::human_table_probe_cpp(n, k)
    sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    cat(sprintf("k=%d n=%2d TAIL=%2d bs=%2d | entries=%9.0f capped=%-5s %7.2f sec\n",
                k, n, res$tail, res$bs, res$entries, res$capped, sec))
    flush.console()

    rows[[length(rows) + 1]] <- data.frame(
      k = k, n = n, tail = res$tail, bs = res$bs,
      entries = res$entries, capped = res$capped, sec = sec
    )

    # Cost climbs with n at fixed k, so once one build blows the budget the
    # wider rings for this k will too.
    if (sec > budget_sec) {
      cat(sprintf("  -- over %d sec, skipping larger n for k=%d\n", budget_sec, k))
      break
    }
  }
}

df <- do.call(rbind, rows)

cat("\n=== summary ===\n")
print(df, row.names = FALSE)

# Cost per k: one figure per k (worst n in range), since k -- through TAIL --
# is what sets it. bs / n is shown only to confirm it does not move the cost.
cat("\n=== build cost by k (worst case over n in range) ===\n")
for (k in unique(df$k)) {
  sub <- df[df$k == k, ]
  i <- which.max(sub$sec)
  cat(sprintf("  k=%d (TAIL=%d): up to %6.2f sec, entries=%.0f, over n=%d..%d\n",
              k, sub$tail[1], sub$sec[i], max(sub$entries),
              min(sub$n), max(sub$n)))
}

out <- file.path(system.file("examples", package = "cayleyR"), "output")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
write.csv(df, file.path(out, "human_table_probe.csv"), row.names = FALSE)
cat("\nWrote", file.path(out, "human_table_probe.csv"), "\n")
