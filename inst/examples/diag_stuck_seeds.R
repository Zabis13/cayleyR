#!/usr/bin/env Rscript
# Seeds 8 and 12 do not reduce in any of the 24 orientations, at any budget.
# Every other seed in the sample does. What is different about them?
#
# From bench_orientations.R, all four cells NOT reduced:
#
#     seed  8   2x' 2z U' 1y B' 2z    51.3 s at the largest budget
#     seed 12   U' 1y' 2z 1x' F F     41.4 s
#
# Two candidate causes, and they call for different work:
#
#   the orientation sweep cannot help them   every orientation leaves phase 3
#       a state it cannot finish, so the fix has to be in phase 3's coordinate
#       -- the collision-bound table measured earlier today.
#   an earlier phase is what fails          phase 1 or 2 never reaches its own
#       goal, and phase 3 never runs at all. Then the orientation sweep is
#       irrelevant to these cubes and so is phase 3's table.
#
# The published report cannot distinguish them: cube_kociemba4_reduce() returns
# an empty path either way. So this asks each phase separately, in every
# orientation, and reports which one stops.
#
# What comes out per orientation:
#
#   p1, p2   moves each of the first two phases spent, or "-" if it failed
#   p3       what phase 3 then did: moves, or the outcome that stopped it
#   pairs    dedges already paired when phase 3 started -- the quantity phase 3
#            exists to fix, and a rough measure of how hard its job is
#
# Run with:  Rscript inst/examples/diag_stuck_seeds.R
#            Rscript inst/examples/diag_stuck_seeds.R 8,12,4   # pick seeds

library(cayleyR)

args   <- commandArgs(trailingOnly = TRUE)
worker <- length(args) >= 1L && args[[1]] == "--run"

seeds <- if (!worker && length(args) >= 1L) {
  as.integer(strsplit(args[[1]], ",", fixed = TRUE)[[1]])
} else {
  c(8L, 12L)
}

N          <- 4L
budget     <- 2e6
timeout_s  <- 600L

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

scramble_state <- function(seed) {
  set.seed(seed)
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  list(state = s, word = w)
}

# How many of the twelve dedges are already paired. Phase 3's whole job is to
# make this twelve, so it says more about the size of its task than the move
# count does.
#
# A dedge is paired when the two wings that belong together sit in two slots
# that themselves form a dedge. Both halves of that come from the geometry:
# `partner` maps a wing to the wing it pairs with, and `dedge` maps a slot to
# the dedge it belongs to. The state is read as pieces rather than stickers,
# because `partner` numbers wings and a sticker vector numbers stickers -- an
# earlier version of this crossed the two and returned 2 for every cube,
# including a solved one, which is how the mistake was caught.
.wing_geom <- cayleyR:::cube_wing_geometry_cpp()
n_paired <- function(state) {
  perm <- cayleyR:::cube_to_pieces4_cpp(state)$perm
  # Wings occupy the middle block of the piece vector: 8 corners, then 24
  # wings, then the centres.
  w <- perm[9:32]
  paired <- 0L
  seen <- logical(24)
  for (slot in seq_len(24)) {
    if (seen[[slot]]) next
    piece <- w[[slot]]                       # which wing sits here, 0-based
    mate_piece <- .wing_geom$partner[[piece + 1L]]
    mate_slot <- which(w == mate_piece)
    if (!length(mate_slot)) next
    seen[[slot]] <- TRUE
    seen[[mate_slot[[1]]]] <- TRUE
    if (.wing_geom$dedge[[slot]] == .wing_geom$dedge[[mate_slot[[1]]]]) {
      paired <- paired + 1L
    }
  }
  paired
}

# Checked rather than trusted, in every process that uses it: the measure has
# to say 12 on a solved cube or it is not measuring pairing at all.
local({
  chk <- n_paired(cube_identity(N))
  if (chk != 12L) {
    stop("n_paired() reports ", chk, " on a solved cube, expected 12",
         call. = FALSE)
  }
})

if (worker) {
  seed  <- as.integer(args[[2]])
  ridx  <- as.integer(args[[3]])
  out   <- args[[4]]

  sc <- scramble_state(seed)
  rot <- cayleyR:::.cube4_orientations[[ridx]]

  st <- sc$state
  if (nzchar(rot)) st <- st[cube_wide_word(rot, N)]

  # Phase 1 alone, then phases 1 and 2, then phase 3 on what they left. Asking
  # them one at a time is the only way to see which one stops: the combined
  # call reports the same empty path whichever it was.
  p1 <- cayleyR:::cube_kociemba4_phase12_cpp(st, upto_phase = 1L,
                                             node_budget = budget)
  r1 <- cayleyR:::cube_kociemba4_last_cpp()$phase1
  after1 <- replay(st, p1)

  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(st, upto_phase = 2L,
                                              node_budget = budget)
  r2 <- cayleyR:::cube_kociemba4_last_cpp()$phase2
  after2 <- replay(st, p12)

  pairs <- if (identical(r2, "found")) n_paired(after2) else NA_real_

  p3n <- -1L; r3 <- "not run"
  if (identical(r2, "found")) {
    res3 <- cayleyR:::cube_kociemba4_phase3_cpp(after2, node_budget = budget)
    r3 <- res3$outcome
    if (isTRUE(res3$found)) p3n <- length(res3$path)
  }

  writeLines(sprintf("RESULT\t%d\t%s\t%d\t%s\t%d\t%s\t%d\t%.1f",
                     ridx, r1, length(p1), r2, length(p12), r3, p3n,
                     if (is.na(pairs)) -1 else pairs), out)
  quit(save = "no")
}

this_file <- sub("^--file=", "",
                 grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
rots <- cayleyR:::.cube4_orientations

run_one <- function(seed, ridx) {
  res <- tempfile("stuck", fileext = ".tsv")
  on.exit(unlink(res), add = TRUE)
  system2(file.path(R.home("bin"), "Rscript"),
          c(shQuote(this_file), "--run", seed, ridx, shQuote(res)),
          stdout = NULL, stderr = NULL, timeout = timeout_s)
  line <- if (file.exists(res))
            grep("^RESULT\t", readLines(res, warn = FALSE), value = TRUE)
          else character(0)
  if (length(line) != 1L) {
    return(data.frame(seed = seed, rot = rots[[ridx]], r1 = "timeout",
                      p1 = NA_integer_, r2 = "-", p12 = NA_integer_,
                      r3 = "-", p3 = NA_integer_, pairs = NA_real_,
                      stringsAsFactors = FALSE))
  }
  f <- strsplit(line[[1]], "\t", fixed = TRUE)[[1]]
  p3 <- as.integer(f[[8]]); pr <- as.numeric(f[[9]])
  data.frame(seed = seed, rot = rots[[ridx]], r1 = f[[3]],
             p1 = as.integer(f[[4]]), r2 = f[[5]],
             p12 = as.integer(f[[6]]), r3 = f[[7]],
             p3 = if (p3 < 0L) NA_integer_ else p3,
             pairs = if (pr < 0) NA_real_ else pr,
             stringsAsFactors = FALSE)
}

hr("setup")
cat("seeds  : ", paste(seeds, collapse = ", "), "\n", sep = "")
cat("budget : ", format(budget, scientific = FALSE, big.mark = ","), "\n", sep = "")
cat("\nEach phase is asked separately, in all 24 orientations. The combined\n")
cat("call returns the same empty path whichever phase stopped, so the only\n")
cat("way to see which one it was is to run them apart.\n")

all_rows <- list()
for (sd in seeds) {
  sc <- scramble_state(sd)
  hr(paste0("seed ", sd, "  (", paste(sc$word, collapse = " "), ")"))
  cat(sprintf("  %-7s %-10s %-4s %-10s %-4s %-12s %-5s %s\n",
              "rot", "phase1", "p1", "phase2", "p12", "phase3", "p3", "pairs"))
  rows <- list()
  for (i in seq_along(rots)) {
    r <- run_one(sd, i)
    cat(sprintf("  %-7s %-10s %-4s %-10s %-4s %-12s %-5s %s\n",
                if (nzchar(r$rot)) r$rot else "-", r$r1,
                if (is.na(r$p1)) "?" else r$p1, r$r2,
                if (is.na(r$p12)) "?" else r$p12, r$r3,
                if (is.na(r$p3)) "-" else r$p3,
                if (is.na(r$pairs)) "-" else r$pairs))
    flush.console()
    rows[[length(rows) + 1L]] <- r
  }
  d <- do.call(rbind, rows)
  all_rows[[length(all_rows) + 1L]] <- d

  cat("\n  where it stops, over the 24 orientations:\n")
  cat(sprintf("    phase 1 finished : %d\n", sum(d$r1 == "found")))
  cat(sprintf("    phase 2 finished : %d\n", sum(d$r2 == "found")))
  cat(sprintf("    phase 3 finished : %d\n", sum(d$r3 == "found", na.rm = TRUE)))
  ok <- d[!is.na(d$pairs), ]
  if (nrow(ok)) {
    cat(sprintf("    dedges paired when phase 3 started: %.1f to %.1f of 12\n",
                min(ok$pairs), max(ok$pairs)))
  }
}

tab <- do.call(rbind, all_rows)

hr("verdict")
for (sd in seeds) {
  d <- tab[tab$seed == sd, ]
  n1 <- sum(d$r1 == "found"); n2 <- sum(d$r2 == "found")
  n3 <- sum(d$r3 == "found", na.rm = TRUE)
  cat(sprintf("\nseed %d: phase 1 %d/24, phase 2 %d/24, phase 3 %d/24\n",
              sd, n1, n2, n3))
  if (n2 == 0L) {
    cat("  Phase 2 never finishes, so phase 3 never runs and the orientation\n")
    cat("  sweep has nothing to choose between. This is not a phase 3 problem\n")
    cat("  at all -- raise phase 2's depth or budget and look again.\n")
  } else if (n3 == 0L) {
    cat("  Phases 1 and 2 finish and phase 3 never does, in any orientation.\n")
    cat("  The sweep cannot help this cube: there is no orientation that\n")
    cat("  leaves phase 3 a state it can handle. This is the cube to take to\n")
    cat("  the coordinate work -- a table bounded by collisions cannot score\n")
    cat("  states this far out, whatever the route in.\n")
  } else {
    cat("  Some orientation does get through. Then the sweep should have\n")
    cat("  found it, and the reason it did not is worth chasing: check that\n")
    cat("  cube_kociemba4_reduce() is passing the same budget this run used.\n")
  }
}
