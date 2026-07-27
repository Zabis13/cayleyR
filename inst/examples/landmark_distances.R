#!/usr/bin/env Rscript
# Distance from the identity to ten structural landmark states.
#
# For each n in N_VALUES the ten permutations built by landmark_states() are
# solved with the human TopSpin algorithm and the resulting path is shortened
# with short_path_bfs(). The path length is the estimated distance d(id, sigma).
#
# When the true diameter of the graph is known (small n, computed here by BFS
# or supplied in KNOWN_DIAMETER) the ratio d / diameter is reported. Those
# ratios are the point of the experiment: if a landmark keeps a stable ratio
# across the small graphs, the diameter of a large graph can be estimated as
# d_large / ratio.
#
# Run with:  Rscript inst/examples/landmark_distances.R

library(cayleyR)

# ---------------------------------------------------------------- PARAMETERS

N_VALUES <- c(6, 7, 8, 9, 10)   # permutation sizes to probe
K <- 4L                         # reverse-prefix length for the X operation
DEPTH <- 5L                     # BFS depth used by short_path_bfs
ROUNDS <- 3L                    # how many times to re-apply the shortener

# Exact diameter by full BFS over the graph. Cheap up to n = 8, painful beyond.
DIAMETER_MAX_N <- 8L

# Diameters you already know, as "n" = value. Used instead of computing them.
KNOWN_DIAMETER <- c()

OUT_CSV <- ""                   # path to write the table to, "" = do not write

# --------------------------------------------------------------- HELPERS

shorten <- function(path, start_state, k, depth, rounds) {
  for (i in seq_len(rounds)) {
    before <- length(path)
    res <- short_path_bfs(path, start_state, k, depth)
    path <- res$path
    if (length(path) >= before) break
  }
  path
}

distance_to <- function(target, k, depth, rounds) {
  n <- length(target)
  start <- seq_len(n)
  res <- human_algorithm(start, final_state = target, k = k)
  if (!isTRUE(res$found)) {
    return(list(len = NA_integer_, raw = NA_integer_, ok = FALSE))
  }
  raw <- res$length
  path <- shorten(res$path, start, k, depth, rounds)
  reached <- as.integer(
    apply_operations(start, path, k, compute_coords = FALSE)$state
  )
  list(len = length(path), raw = raw, ok = identical(reached, target))
}

graph_diameter <- function(n, k, max_n, known) {
  key <- as.character(n)
  if (key %in% names(known)) return(as.numeric(known[[key]]))
  if (n > max_n) return(NA_real_)
  bfs <- cayley_bfs_full(seq_len(n), k = k, moves = c("L", "R", "X"))
  max(bfs$dist)
}

# --------------------------------------------------------------------- RUN

cat(sprintf("Landmark distances: k = %d, depth = %d, rounds = %d\n",
            K, DEPTH, ROUNDS))
cat("n values:", paste(N_VALUES, collapse = ", "), "\n\n")

rows <- list()

for (n in N_VALUES) {
  lm <- landmark_states(n)

  t0 <- Sys.time()
  diam <- graph_diameter(n, K, DIAMETER_MAX_N, KNOWN_DIAMETER)
  t_diam <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  cat(sprintf("--- n = %d ---\n", n))
  if (is.na(diam)) {
    cat("diameter: unknown (n above DIAMETER_MAX_N and not in KNOWN_DIAMETER)\n")
  } else {
    cat(sprintf("diameter: %g  (%.2f s)\n", diam, t_diam))
  }

  for (i in seq_len(nrow(lm))) {
    target <- lm$state[[i]]
    t1 <- Sys.time()
    d <- distance_to(target, K, DEPTH, ROUNDS)
    el <- as.numeric(difftime(Sys.time(), t1, units = "secs"))

    rows[[length(rows) + 1L]] <- data.frame(
      n = n,
      id = lm$id[i],
      name = lm$name[i],
      state_str = lm$state_str[i],
      raw_len = d$raw,
      dist = d$len,
      valid = d$ok,
      diameter = diam,
      ratio = if (is.na(diam)) NA_real_ else d$len / diam,
      secs = round(el, 3),
      stringsAsFactors = FALSE
    )

    cat(sprintf("  %-15s %-28s raw %4s -> %4s  ratio %s%s\n",
                lm$name[i],
                lm$state_str[i],
                ifelse(is.na(d$raw), "NA", d$raw),
                ifelse(is.na(d$len), "NA", d$len),
                ifelse(is.na(diam), "-", sprintf("%.3f", d$len / diam)),
                ifelse(isTRUE(d$ok), "", "  [PATH DID NOT REACH TARGET]")))
  }
  cat("\n")
}

res <- do.call(rbind, rows)

# ------------------------------------------------------------------ REPORT

cat("--- Full table ---\n")
print(res[, c("n", "name", "raw_len", "dist", "diameter", "ratio", "valid")],
      row.names = FALSE, digits = 4)

known <- res[!is.na(res$ratio), ]
if (nrow(known) > 0L) {
  cat("\n--- Ratio d/diameter per landmark, across n ---\n")
  wide <- reshape(
    known[, c("n", "name", "ratio")],
    idvar = "name", timevar = "n", direction = "wide"
  )
  names(wide) <- sub("^ratio\\.", "n=", names(wide))
  print(wide, row.names = FALSE, digits = 3)

  cat("\n--- Stability of each ratio (lower sd = better predictor) ---\n")
  agg <- do.call(rbind, lapply(split(known, known$name), function(g) {
    data.frame(name = g$name[1],
               n_obs = nrow(g),
               mean_ratio = mean(g$ratio),
               sd_ratio = if (nrow(g) > 1L) stats::sd(g$ratio) else NA_real_,
               min_ratio = min(g$ratio),
               max_ratio = max(g$ratio),
               stringsAsFactors = FALSE)
  }))
  agg <- agg[order(agg$sd_ratio, na.last = TRUE), ]
  print(agg, row.names = FALSE, digits = 3)

  # Extrapolation: for every n whose diameter is unknown, divide the measured
  # distance by the mean ratio of the same landmark over the known graphs.
  unknown <- res[is.na(res$diameter) & !is.na(res$dist), ]
  if (nrow(unknown) > 0L) {
    cat("\n--- Estimated diameter for graphs with unknown diameter ---\n")
    est <- merge(unknown[, c("n", "name", "dist")],
                 agg[, c("name", "mean_ratio", "sd_ratio")], by = "name")
    est$diam_est <- est$dist / est$mean_ratio
    est <- est[order(est$n, est$sd_ratio, na.last = TRUE), ]
    print(est[, c("n", "name", "dist", "mean_ratio", "diam_est")],
          row.names = FALSE, digits = 4)

    cat("\nPer-n summary of the estimates:\n")
    smry <- do.call(rbind, lapply(split(est, est$n), function(g) {
      data.frame(n = g$n[1],
                 median_est = stats::median(g$diam_est),
                 mean_est = mean(g$diam_est),
                 min_est = min(g$diam_est),
                 max_est = max(g$diam_est),
                 stringsAsFactors = FALSE)
    }))
    print(smry, row.names = FALSE, digits = 4)
  }
} else {
  cat("\nNo graph with a known diameter -- no ratios to report.\n")
}

if (nzchar(OUT_CSV)) {
  utils::write.csv(res, OUT_CSV, row.names = FALSE)
  cat("\nWritten to", OUT_CSV, "\n")
}
