#!/usr/bin/env Rscript
# A Q-model for the 3x3x3, trained on states that are never seen twice.
#
# train_cube3_q.R builds one fixed set of walks and passes over it `epochs`
# times. That is what a Keras-shaped fit does, and here it is the wrong shape.
# Two of the eighteen outputs carry a label, and that label is noisy: p-1 is an
# upper bound on the distance, overstating it by an amount that depends on the
# state. Averaging the overstatement away needs MANY DIFFERENT states, not the
# same states many times -- and re-reading a fixed set is how a network learns
# the noise instead.
#
# What that looked like: at depth=20 the mean prediction settled on ~11, at
# depth=10 on ~5.8. Both are the middle of their own range, which is what a net
# answers once it has stopped telling states apart. pair_accuracy fell to chance
# past depth ~8 in the first and past ~6 in the second -- narrowing the training
# depth made the middle depths WORSE, so the fixed sample, not the depth range,
# was the binding constraint.
#
# Raising `states` does not fix it and cannot: the encoded input is
# [states, 26, 78] doubles, so 600k walks is 9.7 GB and the run is killed before
# it trains. The memory holds the whole sample at once precisely because the
# sample is fixed.
#
# So the sample is not fixed here. Each step draws a fresh batch, trains on it
# once, and discards it -- the shape cayleypy-training-core uses (256 steps of
# 2048 per "epoch", 65536 epochs, no state repeated). Memory is then one batch
# rather than the whole run, and the states seen is limited by time instead of
# by RAM.
#
# The pivot comes from generate_walk(pivot = TRUE): the cut is drawn inside the
# walk, so `next_move` is the move the walk actually went on to make rather than
# a neighbour drawn at random, and `prev_move` is the inverse of the move that
# arrived. Those are the only two moves whose value is known at that state.
#
# Training goes through ggmlR::ggml_trainer(), one context for the whole run.
# An earlier version called ggml_fit() once per batch, and that was wrong twice
# over: every call built a fresh optimizer context, so each step was Adam's
# first (roughly lr * sign(g)) and the run plateaued early; and the functional
# ggml_fit() dropped `sample_weight` without a word, so the mask was never
# applied and all eighteen outputs were pulled towards the fill value.
#
# The learning rate follows `sched`: "cosine" warms up linearly over `warmup`
# steps and then decays to `lr_min`; "const" keeps `lr` throughout, for
# comparing the two.
#
# Run with:  Rscript inst/examples/train_cube3_q_online.R [name=value ...]
#
#   depth=20 steps=20000 batch=512 report=250
#   d_model=64 heads=4 blocks=3 ff=256 backend=auto seed=42
#   lr=0.001 lr_min=0.00001 warmup=500 sched=cosine
#
# `steps` x `batch` is the number of states the run will see; nothing is
# repeated, so raising `steps` is how this run is made longer, not `epochs`.
#
# ckpt=N writes the model and meta.rds into the run's directory every N steps,
# with the step reached and the random-number state. resume=<that directory>
# carries on from there: the schedule, data and architecture are all taken from
# its meta.rds (only backend, report and ckpt may be given anew), the learning
# rate picks up the same curve at the next step, and the training stream
# continues where it stopped. What is NOT carried over is Adam's moments --
# ggmlR does not save them yet -- so the first steps after a resume are Adam's
# first, and the result is close to an uninterrupted run, not identical to it.
#
#   Rscript inst/examples/train_cube3_q_online.R steps=20000 ckpt=1000
#   Rscript inst/examples/train_cube3_q_online.R resume=/mnt/Data2/DS_projects/333/<run>

library(cayleyR)

stopifnot(requireNamespace("ggmlR", quietly = TRUE))

# ---------------------------------------------------------------------------
# Parameters
# ---------------------------------------------------------------------------

defaults <- list(
  d_model = 64L,
  heads   = 4L,
  blocks  = 3L,
  ff      = 256L,
  backend = "auto",
  steps   = 20000L,  # fresh batches; states seen = steps * batch
  batch   = 512L,
  val     = 4000L,   # held-out walks, drawn once and kept fixed
  depth   = 20L,     # longest walk; the cut falls inside it
  k_min   = 2L,      # shallowest pivot; 1 is one move from solved
  no_cancel = 1L,    # 1: walks refuse U D U', U U U and the like; 0: no_undo only
  report  = 250L,    # steps between progress lines
  lr      = 1e-3,    # peak learning rate
  lr_min  = 1e-5,    # where the cosine ends
  warmup  = 500L,    # linear warm-up steps
  sched   = "cosine",  # "cosine" or "const"
  seed    = 42L,
  init    = "",      # model directory to continue from; "" trains from scratch
  resume  = "",      # run directory to carry on from its last checkpoint
  ckpt    = 0L,      # steps between checkpoints; 0 saves only at the end
  out     = "/mnt/Data2/DS_projects/333",
  name    = ""
)

opt <- defaults
given <- character(0)
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1L]]
  if (length(kv) != 2L) stop("argument must be name=value: ", a)
  key <- kv[[1L]]
  given <- c(given, key)
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

# resume= takes the run as it was set up. Everything that shapes the data, the
# schedule or the net comes from its meta.rds; giving any of it again would
# make the continuation a different run, so that is refused rather than mixed.
resume_meta <- NULL
if (nzchar(opt$resume)) {
  if (nzchar(opt$init)) stop("resume= and init= do not go together: resume carries its own start")
  opt$resume <- path.expand(opt$resume)
  resume_meta <- readRDS(file.path(opt$resume, "meta.rds"))
  if (is.null(resume_meta$step_done))
    stop("resume: ", opt$resume, " was saved before checkpoints existed; ",
         "use init= to train on from it")
  if (resume_meta$step_done >= resume_meta$steps)
    stop(sprintf("resume: the run finished all %d steps; use init= to train on from it",
                 resume_meta$steps))
  own  <- c("backend", "report", "ckpt", "resume")
  bad  <- setdiff(given, own)
  if (length(bad))
    stop("resume: these come from the run itself and cannot be given: ",
         paste(bad, collapse = ", "))
  for (k in c("steps", "batch", "val", "depth", "k_min", "lr", "lr_min",
              "warmup", "sched", "seed", "d_model", "heads", "blocks", "ff"))
    opt[[k]] <- resume_meta[[k]]
  opt$no_cancel <- as.integer(isTRUE(resume_meta$no_cancel))
  if (!"ckpt" %in% given && !is.null(resume_meta$ckpt)) opt$ckpt <- resume_meta$ckpt
  if (!"report" %in% given && !is.null(resume_meta$report)) opt$report <- resume_meta$report
}

if (opt$depth < 2L) stop("depth must be at least 2: a pivot needs a step each side")
if (!opt$sched %in% c("cosine", "const")) stop("sched must be cosine or const")

lr_at <- function(step) {
  if (opt$sched == "const") return(opt$lr)
  if (step <= opt$warmup) return(opt$lr * step / opt$warmup)
  t <- (step - opt$warmup) / max(1, opt$steps - opt$warmup)
  opt$lr_min + 0.5 * (opt$lr - opt$lr_min) * (1 + cos(pi * t))
}

set.seed(opt$seed)

cat("parameters:",
    paste(sprintf("%s=%s", names(opt), unlist(opt)), collapse = " "), "\n\n")

g   <- cube_group(3)
lay <- cube_piece_layout(g)
mv  <- g$moves
n_a <- length(mv)
mv_index <- setNames(seq_along(mv), mv)

cat("group    :", g$name, "--", g$n, "positions,", n_a, "moves\n")
cat("pieces   :", lay$n_piece, "of width", lay$width, "\n")
cat("head     :", n_a, "outputs, one per move\n")
cat(sprintf("plan     : %d steps x %d = %s states, none repeated\n\n",
            opt$steps, opt$batch,
            format(as.numeric(opt$steps) * opt$batch, big.mark = ",",
                   scientific = FALSE)))

# Scaled by numbers that follow from `depth` alone, so the predicting side can
# undo it knowing only that -- no extra file travels beside the weights.
y_mu <- (1 + opt$depth) / 2
y_sd <- stats::sd(seq_len(opt$depth))

# init= continues from a saved model. Its weights were trained on its own
# scaling, so that scaling is kept rather than re-derived from the new depth:
# otherwise every value the net has learnt would shift with the label scale.
# The architecture is the saved one too; d_model and the rest are taken from
# meta so that what is saved below describes the net actually trained.
init_model <- NULL
if (nzchar(opt$init)) {
  init_dir  <- path.expand(opt$init)
  init_meta <- readRDS(file.path(init_dir, "meta.rds"))
  if (!identical(as.character(init_meta$moves), as.character(mv)))
    stop("init: the model was trained on a different move set")
  init_model <- ggmlR::ggml_load_model(file.path(init_dir, "model.ggml"),
                                       backend = opt$backend)
  y_mu <- init_meta$y_mu
  y_sd <- init_meta$y_sd
  for (k in c("d_model", "heads", "blocks", "ff")) opt[[k]] <- init_meta[[k]]
  cat(sprintf("init     : %s  (depth %d, pair %.1f%%, top1 %.1f%%)\n",
              basename(init_dir), init_meta$depth,
              100 * init_meta$pair_accuracy, 100 * init_meta$top1_accuracy))
  cat(sprintf("           architecture taken from it: d_model=%d heads=%d blocks=%d ff=%d\n",
              opt$d_model, opt$heads, opt$blocks, opt$ff))
} else if (!is.null(resume_meta)) {
  if (!identical(as.character(resume_meta$moves), as.character(mv)))
    stop("resume: the model was trained on a different move set")
  init_model <- ggmlR::ggml_load_model(file.path(opt$resume, "model.ggml"),
                                       backend = opt$backend)
  y_mu <- resume_meta$y_mu
  y_sd <- resume_meta$y_sd
  cat(sprintf("resume   : %s  at step %d of %d  (pair %.1f%%, top1 %.1f%%)\n",
              basename(opt$resume), resume_meta$step_done, opt$steps,
              100 * resume_meta$pair_accuracy, 100 * resume_meta$top1_accuracy))
  cat("           schedule, data and architecture taken from it;",
      "Adam's moments start from zero\n")
}
cat(sprintf("target scaling: mean %.1f, sd %.1f%s\n\n", y_mu, y_sd,
            if (is.null(init_model)) ""
            else if (!is.null(resume_meta)) "  (kept from the run)"
            else "  (kept from init)"))

# ---------------------------------------------------------------------------
# A batch
# ---------------------------------------------------------------------------

# generate_walk(pivot = TRUE) returns the cut point, its label, and the two
# moves that meet there. Everything else about the batch is shape: the encoder
# turns states into the transformer's input, and the mask says which two of the
# eighteen outputs the loss may look at.
#
# The fill on the sixteen unlabelled outputs is 0 in scaled units -- the mean
# depth. The mask zeroes their gradient exactly (checked on a linear net: two
# runs whose masked targets differ by 200 give bit-identical trained outputs),
# but a shared trunk still carries their VALUE into the trained head, and a wild
# fill costs accuracy there:
#
#   inactive target    0 -> active head MAE 0.027
#   inactive target    3 -> active head MAE 0.080
#   inactive target  100 -> active head MAE 0.201
#
# so the blandest available value is the right one.
make_batch <- function(n) {
  w <- generate_walk(g, n = n, n_moves = opt$depth,
                     exact = FALSE, no_undo = TRUE, pivot = TRUE,
                     k_min = opt$k_min, no_cancel = opt$no_cancel == 1L)
  p      <- as.integer(w$depth)
  prev_a <- unname(mv_index[w$prev_move])
  next_a <- unname(mv_index[w$next_move])

  y <- matrix(0, n, n_a)
  m <- matrix(0, n, n_a)
  ip <- cbind(seq_len(n), prev_a)
  ix <- cbind(seq_len(n), next_a)
  y[ip] <- (p - 1L - y_mu) / y_sd
  y[ix] <- (p + 1L - y_mu) / y_sd
  m[ip] <- 1
  m[ix] <- 1

  list(x = cayleyR:::adi_encode(w$states, "transformer", lay),
       y = y, mask = m, p = p, prev_a = prev_a, next_a = next_a)
}

# The validation set is drawn once and then left alone: the point of it is that
# the same states are scored at every report, so the numbers are comparable
# down the run. Only the training stream is fresh.
cat("drawing the held-out set\n"); flush(stdout())
va <- make_batch(opt$val)

# ---------------------------------------------------------------------------
# The network
# ---------------------------------------------------------------------------

# The same transformer the depth script and ADI use, so the three stay
# comparable on architecture; the only change is the width of the last dense.
# silu rather than gelu inside the blocks: gelu has no backward rule in ggml.
build_net <- function() {
  depth_dim <- lay$n_piece * lay$width
  inp <- ggmlR::ggml_input(shape = c(lay$n_piece, depth_dim))
  h   <- ggmlR::ggml_layer_dense(inp, opt$d_model, time_distributed = TRUE)
  h   <- ggmlR::ggml_layer_positional_embedding(h)
  for (b in seq_len(opt$blocks)) {
    h <- ggmlR::ggml_layer_transformer_block(
      h, opt$d_model, n_heads = opt$heads, ff_dim = opt$ff,
      activation = "silu", norm = "rms", name = paste0("block", b))
  }
  h   <- ggmlR::ggml_layer_sequence_pooling(h, mode = "mean")
  out <- ggmlR::ggml_layer_dense(h, n_a)
  ggmlR::ggml_compile(ggmlR::ggml_model(inputs = inp, outputs = out),
                      optimizer = "adam", loss = "mse", metrics = NULL,
                      backend = opt$backend)
}

# What a report line says. pair_accuracy is the question a descent actually
# asks -- of two moves whose order is known, did the net put the better one
# lower -- and top1 is its harder form, where the helping move has to beat all
# seventeen others. MAE is reported too but it is not what decides a search.
# Reports and the final score run the multiplies in f32. On Vulkan the default
# f16 tiles put ~1e-3 of batch-size-dependent noise on every q, so the same
# held-out set would score slightly differently from run to run and between
# a resumed run and its original. Training itself stays on the fast path.
predict_f32 <- function(x) tr$predict(x, matmul_precision = "f32")

score <- function(predict_fn, d) {
  vp <- predict_fn(d$x) * y_sd + y_mu
  rows <- seq_len(nrow(vp))
  q_prev <- vp[cbind(rows, d$prev_a)]
  q_next <- vp[cbind(rows, d$next_a)]
  list(vp = vp, q_prev = q_prev, q_next = q_next,
       mae_prev = mean(abs(q_prev - (d$p - 1L))),
       mae_next = mean(abs(q_next - (d$p + 1L))),
       pair = mean(q_prev < q_next),
       top1 = mean(max.col(-vp, ties.method = "first") == d$prev_a))
}

# ---------------------------------------------------------------------------
# Where the run is written
# ---------------------------------------------------------------------------

# Decided before training, not after, so that checkpoints land in the same
# directory as the finished model and resume= can be pointed at it mid-run.
out <- if (!is.null(resume_meta)) opt$resume else {
  model_name <- if (nzchar(opt$name)) opt$name else
    sprintf("cube3_qon_m%d_h%d_b%d_d%d_s%dk_b%d_%s%s%s_s%d",
            opt$d_model, opt$heads, opt$blocks, opt$depth,
            round(opt$steps / 1000), opt$batch,
            if (opt$sched == "cosine") "cos" else opt$sched,
            if (opt$no_cancel == 1L) "_nc" else "",
            if (nzchar(opt$init)) "_init" else "",
            opt$seed)
  file.path(path.expand(opt$out), model_name)
}
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# A checkpoint and the final save are the same write. step_done and the random
# state are what resume= needs to pick up the schedule and the training stream
# at the next step. Each file goes to a temporary name first and is renamed
# into place, so a run killed mid-write leaves the previous checkpoint whole;
# meta.rds goes last, so the step it records is never ahead of the model.
save_run <- function(model, step_done, s, secs) {
  tmp_model <- file.path(out, "model.ggml.tmp")
  tmp_meta  <- file.path(out, "meta.rds.tmp")
  ggmlR::ggml_save_model(model, tmp_model)
  saveRDS(list(version = 2L, kind = "q_online", y_mu = y_mu, y_sd = y_sd,
               moves = mv, n_out = n_a, depth = opt$depth,
               d_model = opt$d_model, heads = opt$heads, blocks = opt$blocks,
               ff = opt$ff, steps = opt$steps, batch = opt$batch,
               val = opt$val, k_min = opt$k_min,
               step_done = step_done,
               states_seen = as.numeric(step_done) * opt$batch,
               lr = opt$lr, lr_min = opt$lr_min, warmup = opt$warmup,
               sched = opt$sched, no_cancel = opt$no_cancel == 1L,
               ckpt = opt$ckpt, report = opt$report,
               init = init_path, resumed_at = resumed_at,
               seed = opt$seed, rng = .Random.seed,
               mae_prev = s$mae_prev, mae_next = s$mae_next,
               pair_accuracy = s$pair, top1_accuracy = s$top1,
               history = do.call(rbind, history),
               train_secs = secs),
          tmp_meta)
  file.rename(tmp_model, file.path(out, "model.ggml"))
  file.rename(tmp_meta,  file.path(out, "meta.rds"))
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Training
# ---------------------------------------------------------------------------

# One trainer for the whole run: weights, Adam moments and the graph live
# across every step. loss_mask = n_a makes the loss a per-output weighted MSE,
# so b$mask leaves the sixteen unlabelled outputs out of it.
tr <- ggmlR::ggml_trainer(if (is.null(init_model)) build_net() else init_model,
                          batch_size = opt$batch,
                          loss_mask = n_a)

# A fresh run starts at step 1. A resumed one starts after its checkpoint, with
# the random state it saved there: the held-out set above was drawn from the
# same seed as before, and from here the training batches are the ones the
# uninterrupted run would have drawn.
if (is.null(resume_meta)) {
  first_step <- 1L
  history    <- list()
  secs_before <- 0
  init_path  <- if (nzchar(opt$init)) path.expand(opt$init) else NULL
  resumed_at <- integer(0)
} else {
  first_step <- resume_meta$step_done + 1L
  history    <- if (is.null(resume_meta$history)) list() else
    split(resume_meta$history, seq_len(nrow(resume_meta$history)))
  secs_before <- resume_meta$train_secs
  init_path  <- resume_meta$init
  resumed_at <- c(resume_meta$resumed_at, resume_meta$step_done)
  assign(".Random.seed", resume_meta$rng, envir = globalenv())
}

cat(sprintf("training steps %d to %d, batch %d\n\n", first_step, opt$steps, opt$batch))
if (opt$ckpt > 0L)
  cat(sprintf("checkpoint every %d steps into %s\n\n", opt$ckpt, out))
cat("    step        seen   pair    top1   MAE(prev)       lr   gen_s   fit_s\n")
flush(stdout())

t_start <- proc.time()[["elapsed"]]
gen_secs <- 0; fit_secs <- 0

for (step in seq.int(first_step, opt$steps)) {
  t0 <- proc.time()[["elapsed"]]
  b  <- make_batch(opt$batch)
  t1 <- proc.time()[["elapsed"]]
  lr <- lr_at(step)
  tr$set_lr(lr)
  tr$step(b$x, b$y, b$mask)
  t2 <- proc.time()[["elapsed"]]
  gen_secs <- gen_secs + (t1 - t0)
  fit_secs <- fit_secs + (t2 - t1)

  if (step %% opt$report == 0L || step == opt$steps) {
    s <- score(predict_f32, va)
    history[[length(history) + 1L]] <- data.frame(
      step = step, seen = as.numeric(step) * opt$batch, lr = lr,
      pair = s$pair, top1 = s$top1, mae_prev = s$mae_prev)
    cat(sprintf("  %6d  %10s  %5.1f%%  %5.1f%%   %7.2f   %.1e  %6.0f  %6.0f\n",
                step,
                format(as.numeric(step) * opt$batch, big.mark = ",",
                       scientific = FALSE),
                100 * s$pair, 100 * s$top1, s$mae_prev, lr,
                gen_secs, fit_secs))
    flush(stdout())
  }

  if (opt$ckpt > 0L && step %% opt$ckpt == 0L && step < opt$steps) {
    save_run(tr$model(), step, score(predict_f32, va),
             secs_before + proc.time()[["elapsed"]] - t_start)
    cat(sprintf("  %6d  checkpoint saved\n", step))
    flush(stdout())
  }
}

model <- tr$model()
tr$free()

train_secs <- proc.time()[["elapsed"]] - t_start
cat(sprintf("\ntrained in %.0f s  (generating %.0f s, fitting %.0f s)\n\n",
            train_secs, gen_secs, fit_secs))

# ---------------------------------------------------------------------------
# What it learnt
# ---------------------------------------------------------------------------

s <- score(function(x) ggmlR::ggml_predict(model, x, batch_size = opt$batch,
                                           matmul_precision = "f32"), va)

cat("== the Q-model ==========================================\n\n")
cat(sprintf("  states seen      %s (none twice)\n",
            format(as.numeric(opt$steps) * opt$batch, big.mark = ",",
                   scientific = FALSE)))
cat(sprintf("  MAE on previous  %.2f moves   (label p-1)\n", s$mae_prev))
cat(sprintf("  MAE on next      %.2f moves   (label p+1)\n", s$mae_next))
cat(sprintf("\n  pair_accuracy    %.1f%%   (previous ranked below next)\n",
            100 * s$pair))
cat(sprintf("  top1_accuracy    %.1f%%   (previous is the argmin of all %d)\n",
            100 * s$top1, n_a))
cat(sprintf("\n  chance is %.1f%% for pair, %.1f%% for top1.\n", 50, 100 / n_a))

# The table that replaces the `step` column of the depth script. Where
# pair_accuracy reaches chance the net can no longer order the neighbours, and
# that is the horizon a search hits -- not where the absolute number flattens.
cat("\n  by depth (where the ordering survives):\n")
for (d in sort(unique(va$p))) {
  k <- va$p == d
  if (!any(k)) next
  cat(sprintf("    %3d moves  n=%4d   pair %5.1f%%   top1 %5.1f%%   q(prev) %6.2f\n",
              d, sum(k), 100 * mean(s$q_prev[k] < s$q_next[k]),
              100 * mean(max.col(-s$vp[k, , drop = FALSE],
                                 ties.method = "first") == va$prev_a[k]),
              mean(s$q_prev[k])))
}
cat("\n  Read pair_accuracy, not the MAE, and read q(prev) for saturation:\n")
cat("  once it stops rising with depth the net has stopped telling those\n")
cat("  depths apart, whatever the MAE says.\n")

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

save_run(model, opt$steps, s, secs_before + train_secs)

cat("\nsaved to", out, "\n")
for (f in list.files(out)) {
  cat(sprintf("  %-14s %8.1f KB\n", f, file.size(file.path(out, f)) / 1024))
}
