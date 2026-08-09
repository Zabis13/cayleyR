#!/usr/bin/env Rscript
# Solving the 4x4x4 with a trained model.
#
# A transformer reads the cube and scores all 24 moves at once; the solver
# takes the best one and asks again. No search and no table -- one forward pass
# per move, and the move is whichever the model likes best.
#
# The model reads a cube as PIECES, not stickers: 96 stickers make 56 pieces --
# 8 corners of three stickers, 24 wings of two, 24 centres of one -- and a
# piece is the thing that moves. It speaks Kaggle's alphabet (f0, -f0, r1 ...)
# and Kaggle's face order, so the state goes across before the solve and the
# path comes back after.
#
# The weights are read straight from the PyTorch checkpoint by ggmlR. No
# Python, no torch, CPU throughout.
#
# Run with:  Rscript inst/examples/demo_cube4_model.R

library(cayleyR)

ARCHIVE  <- "/mnt/Data2/DS_projects/444/archive"
N_STATES <- 10L
N_MOVES  <- 60L    # quarter turns walked away from solved
BUDGET   <- 400L   # greedy steps before a cube is given up on

# ---- 1. The model ----------------------------------------------------------

# The reader and the model live in ggmlR. Once it is installed with them,
# library(ggmlR) is all this needs; sourcing the two files is the way in while
# they are newer than the installed package.
GGMLR <- "/mnt/Data2/DS_projects/ggmlR"
if (requireNamespace("ggmlR", quietly = TRUE) &&
    exists("pt_transformer_load", asNamespace("ggmlR"))) {
  suppressMessages(library(ggmlR))
} else {
  source(file.path(GGMLR, "R/pth.R"))
  source(file.path(GGMLR, "R/piece_transformer.R"))
}

model <- pt_transformer_load(file.path(ARCHIVE, "model/model.pth"))
print(model)

# The generators the model was trained on, in the model's own action order.
# The order matters: the model's output is 24 numbers and nothing in them says
# which move each belongs to.
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

cat("actions     :", length(G), paste0("(", paste(head(moves, 6),
                                                  collapse = " "), " ...)\n"))

# ---- 2. The solver ---------------------------------------------------------

solved_pattern <- function(colours)
  all(vapply(0:5, function(f)
    length(unique(colours[f * 16L + 1:16])) == 1L, logical(1)))

inverse_of <- function(m)
  if (startsWith(m, "-")) sub("^-", "", m) else paste0("-", m)

# Greedy descent on the model's own values, refusing to revisit a state.
#
# Banning the move that undoes the last one is not enough: four of the same
# quarter turn also come back to where they started, and the descent will sit
# in that loop for as long as it is allowed to. So every state the walk has
# stood on is remembered, and the best move leading somewhere new is taken --
# which is the same rule, stated once and covering every length of cycle.
#
# Away from the solved cube the values flatten out, and the move taken there is
# often uphill. That is the point at which this stops being a solver and starts
# being a walk; the budget is what ends it.
model_solve <- function(colours, budget = BUDGET) {
  path <- character(0)
  seen <- new.env(hash = TRUE, parent = emptyenv())
  assign(paste(colours, collapse = ","), TRUE, envir = seen)

  for (step in seq_len(budget)) {
    if (solved_pattern(colours))
      return(list(path = path, found = TRUE))

    q <- pt_forward(model, colours)
    move <- NULL
    for (j in order(q)) {
      cand <- colours[G[[moves[j]]]]
      key <- paste(cand, collapse = ",")
      if (!exists(key, envir = seen, inherits = FALSE)) {
        move <- moves[j]
        colours <- cand
        assign(key, TRUE, envir = seen)
        break
      }
    }
    # Every one of the 24 leads somewhere already stood on: a dead end, and
    # nothing short of stepping back would get out of it.
    if (is.null(move))
      return(list(path = path, found = FALSE, stuck = "dead end"))

    path <- c(path, move)
  }
  list(path = path, found = solved_pattern(colours))
}

# ---- 3. Ten cubes ----------------------------------------------------------

g <- cube_group(4)
set.seed(2026)

cat("\n", strrep("-", 62), "\n", sep = "")
cat(sprintf("%5s  %7s  %7s  %8s   %9s %8s\n",
            "cube", "model", "sec", "steps/s", "reduction", "sec"))

rows <- vector("list", N_STATES)

for (i in seq_len(N_STATES)) {
  state <- generate_state(group = g, n_moves = N_MOVES)

  # A package state is sticker numbers; the model wants colours in Kaggle's
  # face order. Colours first, then the relabelling.
  colours <- cube_santa_state_out(cube_colours(state, 4), 4)

  t0 <- proc.time()[["elapsed"]]
  mres <- model_solve(colours)
  t_model <- proc.time()[["elapsed"]] - t0

  # The same cube by reduction, for a figure to read the model's against.
  t0 <- proc.time()[["elapsed"]]
  rres <- cube_solve4(suppressWarnings(
    cube_colour_state(cube_santa_state(colours, 4), 4)))
  t_red <- proc.time()[["elapsed"]] - t0

  n_model <- if (isTRUE(mres$found)) length(mres$path) else NA_integer_
  n_red   <- if (isTRUE(rres$found)) length(rres$path) else NA_integer_

  rows[[i]] <- data.frame(cube = i, model = n_model, model_sec = t_model,
                          reduction = n_red, red_sec = t_red)

  cat(sprintf("%5d  %7s  %7.1f  %8.1f   %9s %8.2f\n", i,
              if (is.na(n_model)) "stuck" else n_model, t_model,
              length(mres$path) / t_model,
              if (is.na(n_red)) "-" else n_red, t_red))
}

res <- do.call(rbind, rows)

# ---- 4. Statistics ---------------------------------------------------------

cat("\n", strrep("=", 62), "\n", sep = "")
cat(sprintf("%d cubes, %d quarter turns from solved\n\n", N_STATES, N_MOVES))

summary_row <- function(label, n, secs) {
  ok <- !is.na(n)
  data.frame(
    method     = label,
    solved     = sprintf("%d/%d", sum(ok), length(n)),
    mean_moves = if (any(ok)) round(mean(n[ok]), 1) else NA_real_,
    median     = if (any(ok)) round(stats::median(n[ok]), 1) else NA_real_,
    min        = if (any(ok)) min(n[ok]) else NA_integer_,
    max        = if (any(ok)) max(n[ok]) else NA_integer_,
    mean_sec   = round(mean(secs), 2),
    stringsAsFactors = FALSE)
}

print(rbind(summary_row("model", res$model, res$model_sec),
            summary_row("reduction", res$reduction, res$red_sec)),
      row.names = FALSE)

both <- !is.na(res$model) & !is.na(res$reduction)
if (any(both)) {
  cat(sprintf("\non the %d cube(s) both solved:\n", sum(both)))
  cat(sprintf("  model     %6.1f moves   %7.1f s\n",
              mean(res$model[both]), mean(res$model_sec[both])))
  cat(sprintf("  reduction %6.1f moves   %7.1f s\n",
              mean(res$reduction[both]), mean(res$red_sec[both])))
  cat(sprintf("  the model's path is %.2fx the length, for %.0fx the time\n",
              mean(res$model[both]) / mean(res$reduction[both]),
              mean(res$model_sec[both]) / mean(res$red_sec[both])))
}

stuck <- which(is.na(res$model))
if (length(stuck))
  cat("\nthe model did not finish", length(stuck), "cube(s) within", BUDGET,
      "steps:", paste(stuck, collapse = ", "), "\n")
