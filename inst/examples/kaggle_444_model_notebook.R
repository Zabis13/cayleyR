# ---------------------------------------------------------------------------
# cayley-py-444-cube -- solving a 4x4x4 with a trained model, and reduction
# behind it
#
# Kaggle notebook, R. Two solvers, in order:
#
#   the model    a transformer scores all 24 moves on a state; a beam keeps the
#                best `WIDTH` states it has reached and expands all of them.
#                Short paths -- tens of moves rather than hundreds -- but it
#                does not finish every cube.
#   reduction    cube_solve4(), which finishes every cube in about three
#                hundred quarter turns. Whatever the model leaves, this takes.
#
# So every cube gets solved, and the ones the model reached get a much shorter
# path. Nothing here needs Python or torch: ggmlR reads the PyTorch checkpoint
# directly.
#
# Set MODEL_DIR to the dataset holding model.pth and p002.json.
# ---------------------------------------------------------------------------

library(cayleyR)
library(ggmlR)

DATA      <- "/kaggle/input/competitions/cayley-py-444-cube"
MODEL_DIR <- "/kaggle/input/datasets/trydotatwo/cube4-full-transformer-inference/model"
GEN_DIR   <- "/kaggle/input/datasets/trydotatwo/cube4-full-transformer-inference/generators"

WIDTH     <- 10L      # beam width; 1 is greedy, wider is slower and stronger
STEPS     <- 40L      # give the model this many steps before reduction takes over

if (!dir.exists(DATA)) DATA <- "."            # running outside Kaggle

MODEL_PTH <- file.path(MODEL_DIR, "model.pth")
GEN_JSON  <- file.path(GEN_DIR, "p002.json")

# Say which file is missing and what is actually there, rather than let
# normalizePath() report a path with no hint as to what was expected.
for (f in c(MODEL_PTH, GEN_JSON))
  if (!file.exists(f)) {
    cat("missing:", f, "\n\nwhat is under /kaggle/input:\n")
    print(utils::head(list.files("/kaggle/input", recursive = TRUE,
                                 full.names = TRUE), 50))
    stop("set MODEL_DIR / GEN_DIR to match", call. = FALSE)
  }

test <- read.csv(file.path(DATA, "test.csv"), stringsAsFactors = FALSE)
cat("puzzles:", nrow(test), "\n")

# ---- 1. The model and its alphabet -----------------------------------------
#
# The model speaks Kaggle's own moves (f0, -f0, r1 ...) and Kaggle's own face
# order, which is why the state needs no conversion here and the path needs no
# translation on the way out. Reduction, below, is the one that needs both.
#
# The action order matters: the model's output is 24 numbers and nothing in
# them says which move each belongs to. It comes from the generator file.

model <- pt_transformer_load(MODEL_PTH)
print(model)

gen    <- paste(readLines(GEN_JSON, warn = FALSE), collapse = "")
mn_txt <- regmatches(gen, regexpr("\"move_names\"\\s*:\\s*\\[[^]]*\\]", gen))
moves  <- gsub("\"", "", regmatches(mn_txt, gregexpr("\"-?[a-z][0-9]\"",
                                                     mn_txt))[[1]])
mv_txt <- regmatches(gen, regexpr("\"moves\"\\s*:\\s*\\[.*\\]\\s*,\\s*\"move_names\"",
                                  gen))
nums   <- as.integer(regmatches(mv_txt, gregexpr("-?[0-9]+", mv_txt))[[1]])
G      <- lapply(seq_len(24L),
                 function(i) nums[((i - 1L) * 96L + 1L):(i * 96L)] + 1L)
names(G) <- moves

cat("actions:", length(G), paste0("(", paste(head(moves, 6), collapse = " "),
                                  " ...)\n"))

is_solved <- function(c)
  all(vapply(0:5, function(f)
    length(unique(c[f * 16L + 1:16])) == 1L, logical(1)))

key_of <- function(c) paste(c, collapse = ",")

# ---- 2. Beam search --------------------------------------------------------
#
# Greedy descent -- width 1 -- stalls: some fifteen moves from the solved cube
# the values flatten out, the right move stops being the first one, and a
# search keeping only the first loses it. A beam keeps enough of the ordering
# that the right move survives without being on top.
#
# Visited states are shared across the whole beam rather than per branch. Two
# branches arriving at the same cube are one branch, and keeping both spends
# the width on a single line of play. It also covers cycles of every length --
# banning the move that undoes the last one does not, since four of the same
# quarter turn come back to where they started too.

beam_solve <- function(start, width = WIDTH, steps = STEPS) {
  if (is_solved(start)) return(character(0))

  seen <- new.env(hash = TRUE, parent = emptyenv())
  assign(key_of(start), TRUE, envir = seen)

  # The beam as a matrix, one state per row, with the paths alongside. Scoring
  # the whole beam in one call rather than one call per state is most of what
  # makes the search affordable: the model batches, and a tall matmul is what
  # BLAS is for.
  beam  <- matrix(start, 1L)
  paths <- list(character(0))

  for (step in seq_len(steps)) {
    q <- pt_forward(model, beam)               # [beam, 24], one call
    if (is.null(dim(q))) q <- matrix(q, 1L)

    n_beam <- nrow(beam)
    n_mv   <- length(moves)
    total  <- n_beam * n_mv

    # Every successor of every state in the beam, built in one go: row
    # (b-1)*n_mv + j is state b after move j.
    nxt <- matrix(0L, total, ncol(beam))
    for (j in seq_len(n_mv))
      nxt[seq.int(j, by = n_mv, length.out = n_beam), ] <-
        beam[, G[[moves[j]]], drop = FALSE]

    # A solved successor ends it, and the cheapest one is checked first.
    vals <- as.vector(t(q))
    ord  <- order(vals)
    for (i in ord) {
      if (!is_solved(nxt[i, ])) next
      b <- (i - 1L) %/% n_mv + 1L
      return(c(paths[[b]], moves[(i - 1L) %% n_mv + 1L]))
    }

    keep_rows <- integer(0)
    keep_path <- list()
    for (i in ord) {
      if (length(keep_rows) >= width) break
      k2 <- key_of(nxt[i, ])
      if (exists(k2, envir = seen, inherits = FALSE)) next
      assign(k2, TRUE, envir = seen)
      keep_rows <- c(keep_rows, i)
      b <- (i - 1L) %/% n_mv + 1L
      keep_path[[length(keep_path) + 1L]] <-
        c(paths[[b]], moves[(i - 1L) %% n_mv + 1L])
    }
    if (!length(keep_rows)) return(NULL)       # every branch a dead end

    beam  <- nxt[keep_rows, , drop = FALSE]
    paths <- keep_path
  }
  NULL                                       # out of steps
}

# ---- 3. Solve --------------------------------------------------------------
#
# The model first; reduction for whatever it does not reach. The submission is
# written as we go, so a run cut short by the clock still leaves a valid file
# covering the cubes it got to.

paths  <- character(nrow(test))
solver <- character(nrow(test))
secs   <- numeric(nrow(test))

write_submission <- function()
  write.csv(data.frame(initial_state_id = test$initial_state_id,
                       path = paths, stringsAsFactors = FALSE),
            "submission.csv", row.names = FALSE, quote = TRUE)

t_start <- proc.time()[["elapsed"]]

for (i in seq_len(nrow(test))) {
  t0 <- proc.time()[["elapsed"]]
  colours <- as.integer(strsplit(test$initial_state[i], ",", fixed = TRUE)[[1]])

  p <- try(beam_solve(colours), silent = TRUE)
  if (inherits(p, "try-error")) p <- NULL

  if (!is.null(p)) {
    paths[i]  <- paste(p, collapse = ".")
    solver[i] <- "model"
  } else {
    # Reduction works in the package's alphabet and sticker order, so the
    # state goes across and the path comes back.
    st <- suppressWarnings(cube_colour_state(cube_santa_state(colours, 4), 4))
    r  <- try(cube_solve4(st), silent = TRUE)
    if (inherits(r, "try-error") || !length(r$path)) {
      paths[i]  <- ""
      solver[i] <- "failed"
    } else {
      paths[i]  <- cube_santa_path_out(r$path, 4)
      solver[i] <- if (isTRUE(r$found)) "reduction" else "partial"
    }
  }
  secs[i] <- proc.time()[["elapsed"]] - t0

  if (i %% 25 == 0 || i == nrow(test)) {
    write_submission()
    el <- proc.time()[["elapsed"]] - t_start
    cat(sprintf("%d of %d  |  model %d, reduction %d  |  %.0f s, ~%.0f s left\n",
                i, nrow(test), sum(solver == "model"),
                sum(solver == "reduction"), el, el / i * (nrow(test) - i)))
  }
}

# ---- 4. What came out ------------------------------------------------------

lens <- vapply(strsplit(paths, ".", fixed = TRUE), length, integer(1))
done <- solver != "failed"

cat("\n", strrep("=", 58), "\n", sep = "")
cat("solved      :", sum(done), "of", nrow(test), "\n\n")

for (who in c("model", "reduction", "partial")) {
  k <- solver == who
  if (!any(k)) next
  cat(sprintf("%-10s %4d cubes  median %5g moves  %6.1f s each\n",
              who, sum(k), stats::median(lens[k]), mean(secs[k])))
}

cat(sprintf("\ntotal       : %.0f s\n", sum(secs)))

# What the model bought, where it worked at all.
if (any(solver == "model") && any(solver == "reduction"))
  cat(sprintf("the model's paths are %.0f%% shorter than reduction's\n",
              100 * (1 - stats::median(lens[solver == "model"]) /
                         stats::median(lens[solver == "reduction"]))))

# ---- 5. The submission -----------------------------------------------------

write_submission()
cat("\nwrote submission.csv --", nrow(test), "rows\n")
