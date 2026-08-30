#!/usr/bin/env Rscript
# A transformer for the 3x3x3 cube -- training and saving.
#
# The cube as a SEQUENCE of pieces rather than a flat vector. A dense network
# sees fifteen hundred numbers and has to work out for itself that one stretch
# of them is a single piece; the transformer is told that by the shape of its
# input, and attention compares pieces against each other directly. A corner is
# not twisted on its own but relative to its neighbours -- which is exactly the
# relation attention computes.
#
# The alphabet is all 18 moves of the group, slices included: the same one the
# classical methods in demo_cube3_solve.R work on. Anything else and there is
# nothing to compare. Slices move the centres, so cube_piece_layout() keeps 26
# pieces rather than 20 -- the six centres stop being fixed and become pieces
# like any corner or edge.
#
# arch = "transformer" is built inside from the newer ggmlR API:
#   ggml_layer_transformer_block()      a whole pre-LN block in one call
#   ggml_layer_positional_embedding()   or slot 3 is the same as slot 7
#   ggml_layer_sequence_pooling()       collapse the positions to one vector
#   ggml_layer_dense(time_distributed)  project a one-hot piece into d_model
#
# The trained network is saved as a directory: value.ggml, policy.ggml and
# meta.rds. Testing it is the other script, test_cube3_transformer.R.
#
# Run with:  Rscript inst/examples/train_cube3_transformer.R [name=value ...]
#
# Parameters go on the command line, so the file needs no editing:
#   depth=20 iters=200 states=4000 batch=128 thresh=0.15 out=/some/dir
#   d_model=64 heads=4 blocks=3 ff=256 backend=auto seed=42
#
# A quick check that the thing learns at all (~30s):
#   Rscript inst/examples/train_cube3_transformer.R iters=5
# Training proper:
#   Rscript inst/examples/train_cube3_transformer.R iters=400 thresh=0.15
#
# init= carries on from a model already trained instead of starting from random
# weights. What ADI learns is an area around the solved state where the value is
# accurate, and each run pushes its edge further out; a fresh start spends half
# its budget re-learning an interior that was already exact. The result is saved
# beside the model it grew from, named with the running total of iterations:
#   Rscript inst/examples/train_cube3_transformer.R depth=10 states=8000 \
#     iters=200 init=/mnt/Data2/DS_projects/333/cube3_tr_m64_h4_b3_d10_n8000_i100_s42

library(cayleyR)

stopifnot(requireNamespace("ggmlR", quietly = TRUE))

# ---------------------------------------------------------------------------
# Hyperparameters
# ---------------------------------------------------------------------------

defaults <- list(
  d_model = 64L,     # model width; must divide by heads
  heads   = 4L,
  blocks  = 3L,
  ff      = 256L,    # 4 * d_model, the usual ratio
  backend = "auto",  # "cpu" | "vulkan" | "auto"
  states  = 4000L,   # states per ADI iteration
  depth   = 20L,     # scramble depth
  iters   = 200L,
  batch   = 128L,
  thresh  = 0.15,    # loss_thresh: below it the frozen copy is refreshed
  refresh = 0L,      # refresh the frozen copy every N iterations; 0 = use thresh
  seed    = 42L,
  out     = "/mnt/Data2/DS_projects/333",  # where model directories live
  name    = "",      # model directory name; built from the settings if empty
  init    = ""       # carry on from this saved model instead of fresh weights
)

# Arguments are name=value. The type comes from the default, so depth=20 stays
# an integer and thresh=0.15 stays a double.
opt <- defaults
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1L]]
  if (length(kv) != 2L) stop("argument must be name=value: ", a)
  key <- kv[[1L]]
  if (is.null(defaults[[key]])) {
    stop("unknown parameter: ", key, "\navailable: ",
         paste(names(defaults), collapse = ", "))
  }
  opt[[key]] <- if (is.character(defaults[[key]])) kv[[2L]]
                else if (is.integer(defaults[[key]])) as.integer(kv[[2L]])
                else as.numeric(kv[[2L]])
  if (!is.character(opt[[key]]) && is.na(opt[[key]]))
    stop("not a number: ", a)
}

set.seed(opt$seed)

cat("parameters:",
    paste(sprintf("%s=%s", names(opt), unlist(opt)), collapse = " "), "\n\n")

# ---------------------------------------------------------------------------
# 1. The group
# ---------------------------------------------------------------------------

# The whole alphabet: 12 quarter face turns plus 6 slices. Exactly what
# Kociemba and CFOP solve, so the network learns on the same states.
g   <- cube_group(3)
lay <- cube_piece_layout(g)

cat("group    :", g$name, "--", g$n, "positions,",
    length(g$moves), "moves\n")
cat("moves    :", paste(g$moves, collapse = " "), "\n")
cat("pieces   :", lay$n_piece, "of width", lay$width, "\n")
cat("input    : [n,", lay$n_piece, ",", lay$n_piece * lay$width, "]\n\n")

# ---------------------------------------------------------------------------
# 2. The model
# ---------------------------------------------------------------------------

# encoding = "piece" is required: the transformer's sequence is the pieces. The
# sticker path would give an embedding with axes [dim, seq_len], and attention
# reads the first axis as the sequence -- cube_adi_model() rejects that rather
# than building a graph that would train along the wrong axis.
#
# init= carries on from weights already trained rather than starting over. What
# ADI learns is an area around the solved state inside which the value is
# accurate, and each run pushes its edge outward; starting fresh spends the
# first half of the budget re-learning an interior that was already exact. The
# architecture then comes from the saved model, so d_model and friends are
# ignored -- weights only fit the shape they were trained in.
if (nzchar(opt$init)) {
  init_dir <- path.expand(opt$init)
  if (!file.exists(file.path(init_dir, "meta.rds"))) {
    found <- list.files(init_dir, full.names = TRUE)
    found <- found[file.exists(file.path(found, "meta.rds"))]
    if (length(found) == 0L) stop("no saved network in ", init_dir)
    init_dir <- found[[which.max(file.mtime(file.path(found, "meta.rds")))]]
  }

  net <- cube_adi_load(init_dir, backend = opt$backend)
  cat("continuing from:", basename(init_dir), "\n")

  # A model trained on another move set has a policy head of the wrong width and
  # an input shaped for other pieces. Training would carry on regardless and the
  # damage would only show up as a network that never improves.
  if (net$n_moves != length(g$moves)) {
    stop("saved model has ", net$n_moves, " moves, this group has ",
         length(g$moves), " -- different alphabets, cannot continue")
  }
  if (net$state_len != g$n) {
    stop("saved model has state length ", net$state_len, ", this group has ",
         g$n)
  }
  if (!identical(net$layout$n_piece, lay$n_piece)) {
    stop("saved model was built for ", net$layout$n_piece, " pieces, this ",
         "group gives ", lay$n_piece)
  }
} else {
  net <- cube_adi_model(g, arch = "transformer", encoding = "piece",
                        d_model = opt$d_model, n_heads = opt$heads,
                        n_blocks = opt$blocks, ff_dim = opt$ff,
                        backend = opt$backend)
}
print(net)
cat("\n")

# ---------------------------------------------------------------------------
# 3. Shapes, before any training
# ---------------------------------------------------------------------------

sc0 <- cayleyR:::cube_adi_scramble(g$ptr, opt$batch, opt$depth)
v0  <- cayleyR:::adi_value_of(net$value, sc0$states, opt$batch,
                              net$arch, net$layout)
p0  <- cayleyR:::adi_policy_of(net$policy, sc0$states, opt$batch,
                               net$n_moves, net$arch, net$layout)

cat("forward  : value", length(v0), "values | policy",
    paste(dim(p0), collapse = " x "), "\n")
stopifnot(ncol(p0) == net$n_moves)
cat("softmax row sums to:", sprintf("%.4f", sum(p0[1L, ])), "\n\n")

# ---------------------------------------------------------------------------
# 4. Training
# ---------------------------------------------------------------------------

# ADI has no labels to start from: a state's target is 1 + the smallest value
# among its children, and it is exact wherever a child is already solved. The
# frozen copy supplies the values, the live network learns from them. The
# horizon grows in steps -- each refresh of the copy pushes it out by about half
# a move -- which makes thresh matter more than the iteration count.
cat("training : ", opt$iters, " iterations of ", opt$states,
    " states, depth up to ", opt$depth, "\n\n", sep = "")

# cube_adi_train() starts a fresh history and replaces whatever the network
# carried, so a continued run would forget how far it had come and the next
# continuation after that would name itself wrongly. The old rows are kept here
# and the new ones appended, renumbered to run straight through.
prior <- if (nzchar(opt$init) && !is.null(net$history)) net$history else NULL

t0  <- Sys.time()
net <- cube_adi_train(net, iterations = opt$iters, batch_states = opt$states,
                      max_depth = opt$depth, batch_size = opt$batch,
                      loss_thresh = opt$thresh,
                      refresh_every = if (opt$refresh > 0L) opt$refresh else NULL,
                      verbose = TRUE)
train_secs <- as.numeric(Sys.time() - t0, units = "secs")
cat(sprintf("\ntime     : %.1fs\n", train_secs))

if (!is.null(prior)) {
  net$history$iteration <- net$history$iteration + nrow(prior)
  net$history <- rbind(prior, net$history)
  cat("history  :", nrow(prior), "earlier iterations kept, ",
      nrow(net$history), "in total\n")
}

# ---------------------------------------------------------------------------
# 5. Does it learn at all
# ---------------------------------------------------------------------------

# The only proof is the loss falling. The graph builds and stays quiet even when
# some node has no backward pass, so this is checked with a number.
#
# Only this run's iterations count. A continuation starts from a loss that is
# already low, and measuring it against the very first iteration of the original
# run would report a large fall for a round that achieved nothing.
vl   <- net$history$value_loss
if (!is.null(prior)) vl <- vl[-seq_len(nrow(prior))]
drop <- (vl[[1L]] - vl[[length(vl)]]) / abs(vl[[1L]])
cat(sprintf("\nvalue loss %.4f -> %.4f  (%+.0f%%)%s\n",
            vl[[1L]], vl[[length(vl)]], -100 * drop,
            if (!is.null(prior)) "  (this run only)" else ""))
cat(if (is.finite(drop) && drop > 0.10) "OK: the thing learns\n"
    else if (!is.null(prior)) "loss flat -- expected when continuing; the value spread below is what matters\n"
    else "FAIL: loss is not falling\n")

# ---------------------------------------------------------------------------
# 6. Does the value grow with scramble depth
# ---------------------------------------------------------------------------

# A separate check, because a falling loss is also what a network that learnt
# one constant produces. The value has to grow with depth: the further a state
# is from solved, the more moves it takes. If the last two numbers have merged,
# the horizon has stalled, and what helps then is refreshing the frozen copy
# more often (a higher thresh) or more capacity -- not more iterations.
cat("\nmean value by scramble depth:\n")
probe <- unique(c(1L, 3L, 6L, 10L, 15L, 20L))
probe <- probe[probe <= opt$depth]
for (d in probe) {
  s <- cayleyR:::cube_adi_scramble(g$ptr, opt$batch, d)
  v <- cayleyR:::adi_value_of(net$value, s$states, opt$batch,
                              net$arch, net$layout)
  cat(sprintf("  depth %2d : %6.2f\n", d, mean(v)))
}

# ---------------------------------------------------------------------------
# 7. Saving
# ---------------------------------------------------------------------------

# ggmlR reads ONNX (onnx_load, onnx_run) but cannot write it, so the format is
# the native one: the two models as their own files plus meta.rds. The group is
# an external pointer and cannot go into an RDS, so cube_adi_save() writes the
# move permutations and rebuilds the group on load.
#
# The directory name carries the settings that shape the weights, so a model
# trained to one depth cannot be picked up by mistake as one trained to another
# -- the same reason train_cube4_cnn.R names its file cube4_cnn_d..._n..._e....
# Everything that changes what the weights become goes in: the architecture
# (d_model, heads, blocks), the training regime (depth, states, iterations) and
# the seed. Anything that does not -- the backend, the batch size -- stays out.
#
# A continued run writes beside the model it started from rather than over it:
# the total iteration count goes in the name, so carrying 100 on by 200 lands in
# ..._i300_..., and the 100 it grew from is still there to fall back to. The
# architecture comes from the loaded model in that case, not from the arguments.
# The width is not recoverable from a saved model -- meta.rds keeps the layout
# and the group, not the hyperparameters -- so a continued run names itself with
# the architecture arguments it was given. Leave them at their defaults when
# continuing, or the name will describe a model it is not.
model_name <- if (nzchar(opt$name)) opt$name else {
  done <- if (nzchar(opt$init) && !is.null(prior)) nrow(prior) else 0L
  sprintf("cube3_tr_m%d_h%d_b%d_d%d_n%d_i%d%s_s%d",
          opt$d_model, opt$heads, opt$blocks,
          opt$depth, opt$states, done + opt$iters,
          if (opt$refresh > 0L) sprintf("_r%d", opt$refresh) else "",
          opt$seed)
}

out <- file.path(path.expand(opt$out), model_name)
cube_adi_save(net, out)

cat("\nsaved to", out, "\n")
for (f in list.files(out, full.names = FALSE)) {
  cat(sprintf("  %-14s %8.1f KB\n", f,
              file.size(file.path(out, f)) / 1024))
}

cat("\nnext:  Rscript inst/examples/test_cube3_transformer.R net=", out,
    "\n", sep = "")
