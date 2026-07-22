library(cayleyR)

# === find_path_bfs + celestial coordinates at every transition point ===
# Finds a path as in test_bh_in_path.R, then for each step of the path
# accumulates the L/R/X counters and maps them to celestial coordinates
# (convert_LRX_to_celestial). Each coordinate goes to its own CSV field.

n <- 10
k <- 4
start_state <- 1:n

final_state <- generate_state(n, k, n_moves = 20)

start_time <- Sys.time()
result <- find_path_bfs(
  start_state, final_state, k = k,
  bfs_levels = 200, bfs_n_hubs = 7, bfs_n_random = 3,
  highway_distance_method = "manhattan",
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
elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

cat("\nFound:", result$found, " Path length:",
    if (result$found) length(result$path) else NA, "\n")

# === Coordinates for every transition point of the full path ===
# path is a vector of operations "1"/"2"/"3" (L/R/X). We keep running counters
# and compute celestial coordinates at each point (including the start, step 0).
out_dir <- file.path(system.file("examples", package = "cayleyR"), "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (result$found) {
  path <- as.character(result$path)
  n_steps <- length(path)

  # Rows: step 0 (start, before any move) plus one per operation.
  # Alongside the counters we track the state itself (for graph embedding).
  rows <- vector("list", n_steps + 1L)

  nL <- 0L; nR <- 0L; nX <- 0L
  state <- start_state
  for (i in 0:n_steps) {
    if (i > 0) {
      mv <- path[i]
      if (mv == "1") { nL <- nL + 1L; state <- shift_left(state)$state }
      else if (mv == "2") { nR <- nR + 1L; state <- shift_right(state)$state }
      else if (mv == "3") { nX <- nX + 1L; state <- reverse_prefix(state, k)$state }
    }
    co <- convert_LRX_to_celestial(nL, nR, nX)
    rows[[i + 1L]] <- data.frame(
      step            = i,
      move            = if (i > 0) path[i] else NA_character_,
      state           = paste(state, collapse = " "),
      n               = n,
      k               = k,
      nL              = nL,
      nR              = nR,
      nX              = nX,
      theta           = co$theta,
      phi             = co$phi,
      omega_conformal = co$omega_conformal,
      z_re            = Re(co$z),
      z_im            = Im(co$z),
      stringsAsFactors = FALSE
    )
  }
  coords_df <- do.call(rbind, rows)

  out_file <- file.path(out_dir, "path_coords.csv")
  write.csv(coords_df, file = out_file, row.names = FALSE)

  cat("Done. Path coordinates (", nrow(coords_df), "points) in:\n  ", out_file, "\n", sep = "")
} else {
  cat("No path found - coordinates not written.\n")
}
