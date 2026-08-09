#!/usr/bin/env Rscript
# Where does the time go in a forward pass?
#
# The beam spends nearly all of its time in the model -- 240 permutations, 240
# hash keys and 240 solved-checks together come to a few percent of one step,
# and ten forward passes to the rest. So the question is what inside the
# forward pass costs what, and whether any of it would go faster somewhere
# other than R.
#
# The pass is timed twice over: once by section, by re-running each piece on
# its own, and once by Rprof, which says the same thing from the other side.
#
# Run with:  Rscript inst/examples/profile_cube4_model.R [reps]

suppressMessages(library(ggmlR))

ARCHIVE <- Sys.getenv("CUBE4_ARCHIVE", "/mnt/Data2/DS_projects/444/archive")
args <- commandArgs(trailingOnly = TRUE)
REPS <- if (length(args)) as.integer(args[1]) else 30L

model <- pt_transformer_load(file.path(ARCHIVE, "model/model.pth"))
lay   <- model$layout
state <- rep(0:5, each = 16)

d        <- model$d_model
n_pieces <- model$n_pieces
max_slot <- model$max_slot
n_tok    <- n_pieces + 1L          # the CLS token leads

cat(sprintf("model : %d pieces, d_model %d, %d layers, ff %d\n",
            n_pieces, d, model$n_layers, model$ff_dim))
cat(sprintf("tokens: %d   reps: %d\n\n", n_tok, REPS))

time_it <- function(label, expr, reps = REPS) {
  force(label)
  t0 <- proc.time()[["elapsed"]]
  for (i in seq_len(reps)) v <- eval.parent(substitute(expr))
  el <- (proc.time()[["elapsed"]] - t0) / reps
  cat(sprintf("  %-26s %8.2f ms\n", label, el * 1000))
  invisible(el)
}

# ---- the whole pass, for a baseline ----------------------------------------

cat("whole pass\n")
total <- time_it("pt_forward", pt_forward(model, state))

# ---- the pieces, in the order the pass runs them ---------------------------
#
# Each is timed on its own with the inputs it would really see, so the numbers
# add up to roughly the whole rather than measuring something else.

cat("\nby section\n")

values <- matrix(state[lay$positions + 1L], n_pieces, max_slot)
idx <- values + rep(seq_len(max_slot) - 1L, each = n_pieces) * lay$num_classes

t_emb <- time_it("embed + mask + flatten", {
  emb <- array(t(model$value_embed[, as.vector(idx) + 1L, drop = FALSE]),
               dim = c(n_pieces, max_slot, d))
  emb <- emb * array(lay$mask, dim = dim(emb))
  matrix(aperm(emb, c(1L, 3L, 2L)), n_pieces, max_slot * d)
})

flat <- matrix(aperm(array(t(model$value_embed[, as.vector(idx) + 1L, drop = FALSE]),
                           dim = c(n_pieces, max_slot, d)) *
                     array(lay$mask, dim = c(n_pieces, max_slot, d)),
                     c(1L, 3L, 2L)), n_pieces, max_slot * d)

t_proj <- time_it("piece_projection", {
  sweep(flat %*% t(model$proj_w), 2L, model$proj_b, "+")
})

h0 <- rbind(model$cls_token,
            sweep(flat %*% t(model$proj_w), 2L, model$proj_b, "+") +
              t(model$pos_embed) +
              t(model$type_embed[, lay$types + 1L, drop = FALSE]))

t_ln <- time_it("layer_norm (one)", {
  mu <- rowMeans(h0); xc <- h0 - mu
  sd <- sqrt(rowMeans(xc * xc) + 1e-5)
  sweep(sweep(xc / sd, 2L, model$input_norm$g, "*"), 2L, model$input_norm$b, "+")
})

blk <- model$blocks[[1L]]

# The attention, split into the two things it does: four big projections, and
# the per-head scores-and-weights that R loops over.
t_attn_proj <- time_it("attn projections (4)", {
  q <- sweep(h0 %*% t(blk$attn$w_q), 2L, blk$attn$b_q, "+")
  k <- sweep(h0 %*% t(blk$attn$w_k), 2L, blk$attn$b_k, "+")
  v <- sweep(h0 %*% t(blk$attn$w_v), 2L, blk$attn$b_v, "+")
  sweep(q %*% t(blk$attn$w_o), 2L, blk$attn$b_o, "+")
})

q <- sweep(h0 %*% t(blk$attn$w_q), 2L, blk$attn$b_q, "+")
k <- sweep(h0 %*% t(blk$attn$w_k), 2L, blk$attn$b_k, "+")
v <- sweep(h0 %*% t(blk$attn$w_v), 2L, blk$attn$b_v, "+")
dk <- d %/% model$n_heads

t_heads <- time_it(sprintf("attn heads (%d, in R)", model$n_heads), {
  out <- matrix(0.0, n_tok, d)
  for (i in seq_len(model$n_heads)) {
    cols <- seq.int((i - 1L) * dk + 1L, i * dk)
    sc <- (q[, cols, drop = FALSE] %*% t(k[, cols, drop = FALSE])) / sqrt(dk)
    m <- apply(sc, 1L, max); e <- exp(sc - m)
    out[, cols] <- (e / rowSums(e)) %*% v[, cols, drop = FALSE]
  }
  out
})

t_ff <- time_it("feed-forward (2 matmul)", {
  x <- pmax(sweep(h0 %*% t(blk$ff1$w), 2L, blk$ff1$b, "+"), 0)
  sweep(x %*% t(blk$ff2$w), 2L, blk$ff2$b, "+")
})

t_out <- time_it("output layer", {
  as.vector(model$out_w %*% h0[1L, ]) + model$out_b
})

# ---- what that adds up to --------------------------------------------------

nl <- model$n_layers
per_block <- t_ln * 2 + t_attn_proj + t_heads + t_ff
accounted <- t_emb + t_proj + t_ln + per_block * nl + t_out

cat("\nadding up\n")
cat(sprintf("  %-26s %8.2f ms\n", "one block", per_block * 1000))
cat(sprintf("  %-26s %8.2f ms\n", sprintf("%d blocks", nl), per_block * nl * 1000))
cat(sprintf("  %-26s %8.2f ms\n", "everything above", accounted * 1000))
cat(sprintf("  %-26s %8.2f ms\n", "pt_forward, measured", total * 1000))

cat("\nshare of the pass\n")
share <- function(label, v)
  cat(sprintf("  %-26s %5.1f%%\n", label, 100 * v / total))
share("embed + flatten", t_emb)
share("piece_projection", t_proj)
share("attn projections", t_attn_proj * nl)
share(sprintf("attn heads (R loop)"), t_heads * nl)
share("feed-forward", t_ff * nl)
share("layer_norm", t_ln * (2 * nl + 1))

# ---- the same thing from Rprof ---------------------------------------------
#
# Section timings can mislead -- an expression run on its own is not always the
# expression run in place -- so this is the same question asked of the real
# pass, sampled rather than reconstructed.

prof <- tempfile(fileext = ".out")
Rprof(prof, interval = 0.002, line.profiling = FALSE)
for (i in seq_len(REPS * 2L)) invisible(pt_forward(model, state))
Rprof(NULL)

cat("\nRprof, self time\n")
s <- summaryRprof(prof)$by.self
print(utils::head(s[, c("self.time", "self.pct")], 12))
unlink(prof)

# ---- and what BLAS is doing ------------------------------------------------
#
# The matmuls go through BLAS, so a slow BLAS is felt everywhere at once. The
# reference implementation R ships with is single-threaded and several times
# slower than OpenBLAS on the same machine.

cat("\nBLAS in use:", La_library(), "\n")
n <- 800L
a <- matrix(runif(n * n), n, n)
t0 <- proc.time()[["elapsed"]]
invisible(a %*% a)
gf <- 2 * n^3 / (proc.time()[["elapsed"]] - t0) / 1e9
cat(sprintf("%dx%d matmul: %.1f GFLOP/s", n, n, gf))
cat(if (gf < 10) "  -- reference BLAS; OpenBLAS would be several times this\n"
    else "\n")
