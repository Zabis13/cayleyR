#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Is the piece encoding paying to compute, or paying to arrive?
#
# Two hypotheses have already been ruled out by measurement. Input width is not
# it: the piece input is 1200 values against the sticker input's 1728, and it is
# the narrower one that is slower. Per-call overhead is not it either: cutting
# the calls eightfold (batch 256 -> 2048) bought piece 1.4x while it bought
# sticker 2.4x, so the calls are what sticker pays for, not piece.
#
# What is left is what the two encodings do differently at the boundary. The
# sticker input leaves R as 54 integers per state and is turned into numbers by
# an embedding lookup that happens inside the graph, on the device. The piece
# input leaves R as 1200 doubles per state -- 230 MB for a batch of 24000 --
# and every one of them has to cross into video memory before anything is
# computed. That would make the difference a transfer, not a computation.
#
# This does not argue it; it times it. Two tests, because either alone can be
# explained away:
#
#   1. linearity   score the same network on 1000, 2000, 4000, 8000 states at a
#                  fixed batch. Cost proportional to data gives a straight line
#                  through the origin. A large intercept would mean a fixed
#                  cost per call -- already doubted, and this either buries it
#                  or revives it.
#
#   2. H2D alone   allocate a tensor of each encoding's shape on the Vulkan
#                  backend and copy a batch into it with no graph, no forward
#                  pass, nothing else. This is the transfer and only the
#                  transfer. If the sticker/piece gap in `score` shows up here
#                  at the same scale, the transfer is the answer outright.
#
# The second test needs ggml_backend_synchronize. Vulkan copies are queued, and
# an unsynchronised timer measures how fast R can ask for a copy rather than how
# long the copy takes -- which would show transfers as free and send the
# investigation somewhere else entirely.
#
# Usage:  Rscript diag_score_transfer.R [reps]
#   e.g.  Rscript diag_score_transfer.R 5
# ---------------------------------------------------------------------------

suppressMessages({
  library(cayleyR)
  library(ggmlR)
})

args <- commandArgs(trailingOnly = TRUE)
REPS <- if (length(args) >= 1) as.integer(args[1]) else 5L
BATCH <- 1024L
SIZES <- c(1000L, 2000L, 4000L, 8000L)

FACES <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
g   <- cube_group(3, moves = FACES)
lay <- cube_piece_layout(g)

set.seed(5)
sc   <- cayleyR:::cube_adi_scramble(g$ptr, max(SIZES), 14L)
pool <- sc$states

nets <- lapply(c("sticker", "piece"), function(e) {
  set.seed(99)
  cube_adi_model(g, embed_dim = 32L, hidden = c(512L), n_blocks = 3L,
                 arch = "resnet", encoding = e)
})
names(nets) <- c("sticker", "piece")

## ---- test 1: does cost track the number of states? ------------------------
cat("== 1. scoring cost against batch of states ================\n\n")
cat(sprintf("  fixed batch %d, %d reps\n\n", BATCH, REPS))
cat(sprintf("%8s %12s %12s %10s %10s\n",
            "states", "sticker s", "piece s", "st us/st", "pc us/st"))

lin <- list()
for (e in names(nets)) {
  net <- nets[[e]]
  invisible(cayleyR:::adi_value_of(net$value, pool[seq_len(BATCH), , drop = FALSE],
                                   BATCH, net$arch, net$layout))
}
for (n in SIZES) {
  st <- pool[seq_len(n), , drop = FALSE]
  tt <- vapply(names(nets), function(e) {
    net <- nets[[e]]
    t0 <- proc.time()[["elapsed"]]
    for (r in seq_len(REPS))
      invisible(cayleyR:::adi_value_of(net$value, st, BATCH, net$arch,
                                       net$layout))
    (proc.time()[["elapsed"]] - t0) / REPS
  }, numeric(1))
  cat(sprintf("%8d %12.3f %12.3f %10.1f %10.1f\n", n, tt[["sticker"]],
              tt[["piece"]], 1e6 * tt[["sticker"]] / n, 1e6 * tt[["piece"]] / n))
  flush(stdout())
  lin[[length(lin) + 1L]] <- data.frame(n = n, sticker = tt[["sticker"]],
                                        piece = tt[["piece"]])
}
lin <- do.call(rbind, lin)

## A per-state cost is a line through the origin; a per-call cost lifts the
## intercept off it. Reported as a share of the largest measurement so the two
## encodings can be compared without minding their different scales.
for (e in c("sticker", "piece")) {
  fit <- stats::lm(lin[[e]] ~ lin$n)
  icpt <- unname(stats::coef(fit)[1L])
  cat(sprintf("\n  %-7s fixed cost %.3f s (%.0f%% of the largest run), %.1f us per state",
              e, icpt, 100 * icpt / max(lin[[e]]),
              1e6 * unname(stats::coef(fit)[2L])))
}
cat("\n\n")

## ---- test 2: the transfer with nothing attached to it ---------------------
cat("== 2. host-to-device copy alone ===========================\n\n")

## A backend to copy into. cayley_gpu_init holds one already; asking ggmlR for
## the device directly keeps this test independent of that.
##
## The device is found by walking the list and taking the first one that is not
## the CPU, rather than by asking for a type by number: the enumeration reports
## what is actually present, and on this machine that is a Vulkan GPU at index
## 0 with the CPU behind it at 1.
backend <- tryCatch({
  devs <- lapply(seq_len(ggml_backend_dev_count()) - 1L, ggml_backend_dev_get)
  types <- vapply(devs, ggml_backend_dev_type, integer(1))
  gpu <- which(types != 0L)
  if (!length(gpu)) NULL else
    ggml_backend_dev_init(devs[[gpu[1L]]], NULL)
}, error = function(e) NULL)
if (is.null(backend)) {
  cat("  No Vulkan device to copy into; skipping.\n")
} else {
  copy_cost <- function(shape, mode) {
    ## One context, one tensor, allocated on the device: the copy is then the
    ## only thing being timed.
    ctx <- ggml_init(mem_size = 64L * 1024L * 1024L, no_alloc = TRUE)
    ten <- ggml_new_tensor_2d(ctx,
                              if (mode == "i32") GGML_TYPE_I32 else GGML_TYPE_F32,
                              shape[1L], shape[2L])
    buf <- ggml_backend_alloc_ctx_tensors(ctx, backend)
    payload <- if (mode == "i32")
      as.integer(rep_len(0:53, prod(shape)))
    else
      as.numeric(rep_len(c(0, 1), prod(shape)))

    ggml_backend_tensor_set_data(ten, payload)
    ggml_backend_synchronize(backend)     # queued, not done, without this

    t0 <- proc.time()[["elapsed"]]
    for (r in seq_len(REPS)) {
      ggml_backend_tensor_set_data(ten, payload)
      ggml_backend_synchronize(backend)
    }
    el <- (proc.time()[["elapsed"]] - t0) / REPS
    ggml_backend_buffer_free(buf)
    el
  }

  n <- BATCH
  st_shape <- c(54L, n)                                  # tokens per state
  pc_shape <- c(lay$n_piece * lay$n_piece * lay$width, n)  # one-hot per state
  st_bytes <- prod(st_shape) * 4
  pc_bytes <- prod(pc_shape) * 4

  ## The error is printed rather than swallowed: a test that quietly reports
  ## nothing looks like a test that found nothing.
  why <- NULL
  a <- tryCatch(copy_cost(st_shape, "i32"),
                error = function(e) { why <<- conditionMessage(e); NA_real_ })
  b <- tryCatch(copy_cost(pc_shape, "f32"),
                error = function(e) { why <<- conditionMessage(e); NA_real_ })

  if (is.na(a) || is.na(b)) {
    cat("  Could not copy directly: ", why, "\n", sep = "")
    cat("  Test 1 above still separates per-state from per-call cost.\n")
  } else {
    cat(sprintf("  %d states per copy\n\n", n))
    cat(sprintf("%10s %10s %12s %12s\n", "input", "values", "MB", "seconds"))
    cat(sprintf("%10s %10d %12.1f %12.4f\n", "sticker", prod(st_shape),
                st_bytes / 2^20, a))
    cat(sprintf("%10s %10d %12.1f %12.4f\n", "piece", prod(pc_shape),
                pc_bytes / 2^20, b))
    cat(sprintf("\n  ratio %.1fx on %.1fx the bytes\n", b / max(a, 1e-9),
                pc_bytes / st_bytes))
    cat("\n  If this ratio is near the one `score` showed, the gap is the\n")
    cat("  transfer, and the fix is to send less: indices rather than a dense\n")
    cat("  one-hot, expanded on the device the way the embedding already is.\n")
  }
}

cat("\n== for scale =============================================\n\n")
cat(sprintf("  sticker sends %d values per state, piece sends %d\n",
            54L, lay$n_piece * lay$n_piece * lay$width))
cat(sprintf("  a training iteration scores %d states, a solve step scores %d\n",
            2000L * length(FACES), length(FACES)))
