#!/usr/bin/env Rscript
# Where does greedy descent on the model stop working?
#
# The model was trained on random walks of 2..45 moves. This walks a solved
# cube away by a known number of moves and asks the model to walk it back, one
# best-scoring move at a time, at several depths. What comes out is the depth
# at which the value function stops pointing downhill.
#
# The scramble is made with the model's OWN generators, so the depth reported
# is the depth the model was trained to talk about -- a walk in the package's
# alphabet would be a different number of its moves.
#
# Run with:  Rscript inst/examples/demo_cube4_model_depth.R

suppressMessages(library(ggmlR))

ARCHIVE <- "/mnt/Data2/DS_projects/444/archive"
DEPTHS  <- c(10L, 20L, 30L, 45L)
N_EACH  <- 10L        # cubes per depth
BUDGET  <- function(k) max(200L, 8L * k)   # greedy steps allowed

model <- pt_transformer_load(file.path(ARCHIVE, "model/model.pth"))

# The generators the model scores, in its own action order.
gen    <- paste(readLines(file.path(ARCHIVE, "generators/p002.json"),
                          warn = FALSE), collapse = "")
mn_txt <- regmatches(gen, regexpr("\"move_names\"\\s*:\\s*\\[[^]]*\\]", gen))
moves  <- gsub("\"", "", regmatches(mn_txt, gregexpr("\"-?[a-z][0-9]\"",
                                                     mn_txt))[[1]])
mv_txt <- regmatches(gen, regexpr("\"moves\"\\s*:\\s*\\[.*\\]\\s*,\\s*\"move_names\"",
                                  gen))
nums   <- as.integer(regmatches(mv_txt, gregexpr("-?[0-9]+", mv_txt))[[1]])
G      <- lapply(seq_len(24L),
                 function(i) nums[((i - 1L) * 96L + 1L):(i * 96L)] + 1L)
names(G) <- moves

solved <- rep(0:5, each = 16)

solved_pattern <- function(c)
  all(vapply(0:5, function(f)
    length(unique(c[f * 16L + 1:16])) == 1L, logical(1)))

inverse_of <- function(m)
  if (startsWith(m, "-")) sub("^-", "", m) else paste0("-", m)

# Greedy descent, guarded only against undoing the move just made.
model_solve <- function(colours, budget) {
  banned <- ""
  for (step in seq_len(budget)) {
    if (solved_pattern(colours)) return(step - 1L)
    q <- pt_forward(model, colours)
    ord <- order(q)
    pick <- if (identical(moves[ord[1L]], banned)) ord[2L] else ord[1L]
    banned <- inverse_of(moves[pick])
    colours <- colours[G[[moves[pick]]]]
  }
  NA_integer_
}

set.seed(2026)

cat("\n", strrep("-", 66), "\n", sep = "")
cat(sprintf("%6s  %8s  %9s  %9s  %9s  %8s\n",
            "depth", "solved", "median", "min", "max", "sec/cube"))

rows <- list()
for (k in DEPTHS) {
  got <- integer(0)
  t0 <- proc.time()[["elapsed"]]
  for (i in seq_len(N_EACH)) {
    s <- solved
    for (j in seq_len(k)) s <- s[G[[sample(moves, 1L)]]]
    got <- c(got, model_solve(s, BUDGET(k)))
  }
  secs <- (proc.time()[["elapsed"]] - t0) / N_EACH
  ok <- !is.na(got)

  rows[[length(rows) + 1L]] <- data.frame(
    depth = k, solved = sum(ok), n = N_EACH,
    median = if (any(ok)) stats::median(got[ok]) else NA_real_,
    sec = secs)

  cat(sprintf("%6d  %8s  %9s  %9s  %9s  %8.1f\n", k,
              paste0(sum(ok), "/", N_EACH),
              if (any(ok)) as.character(stats::median(got[ok])) else "-",
              if (any(ok)) as.character(min(got[ok])) else "-",
              if (any(ok)) as.character(max(got[ok])) else "-",
              secs))
}

res <- do.call(rbind, rows)

cat("\n", strrep("=", 66), "\n", sep = "")
cat(N_EACH, "cubes per depth, scrambled with the model's own generators\n\n")
print(res, row.names = FALSE)

# The reading that matters: a solve of k moves undone in k moves is the value
# function pointing straight down; more than k means it wanders on the way.
ok <- !is.na(res$median)
if (any(ok)) {
  cat("\nmoves taken against moves needed:\n")
  for (i in which(ok))
    cat(sprintf("  depth %2d : %5.1f moves, %.2fx the scramble\n",
                res$depth[i], res$median[i], res$median[i] / res$depth[i]))
}

worst <- res$depth[!ok]
if (length(worst))
  cat("\nnothing solved at depth:", paste(worst, collapse = ", "), "\n")
