# One training step of the Q network, recorded once and replayed.
#
# Everything the step computes is on the device: the walk generator
# (cube_q_walk_device.R), the net (cube_q_device.R), the loss, its backward and
# Adam -- recorded by ggmlR::ag_capture_step(). Its only input is the random
# key, two numbers from the host; a replay uploads those 8 bytes and downloads
# the loss.
#
# (A key kept and advanced on the device would upload nothing, but a recording
# without inputs is refused by ag_capture, so something crosses the bus either
# way; the key itself is the cheapest honest input.)
#
# The loss is the online script's: ggml_trainer(loss_mask = n_a) computes
# sum(mask * (pred - y)^2) / (batch * n_a) -- normalised by the batch's size,
# not by the number of labelled outputs -- and ag_mse_loss(pred * mask, y) is
# exactly that, y being zero wherever the mask is. Every state has two labelled
# outputs, so the two normalisations differ by a constant, not by the batch.
#
# The key is (step counter, seed). The counter wraps at 2^24 and the seed then
# moves on, so its first number stays an exact f32 however long the run.

.q_key_limit <- 2^24

.q_train_state <- function(net, dev, lr = 1e-3, seed = 1) {
  .q_device_require()
  spec <- net$spec; B <- dev$batch
  consts <- .q_net_consts(spec, B)

  # No warm-up forward before recording. Weights are resident from creation
  # (ag_param uploads to the persistent pool on the GPU), and the constants are
  # declared in `params`, which ag_capture_step moves to the persistent pool
  # itself -- but only those that hold no device handle yet. A forward run here
  # gave them pass-pool handles, so they were skipped, the pass pool was reset
  # when recording began, and the recording had to upload them: refused.
  opt <- ggmlR::optimizer_adam(.q_net_params(net), lr = lr)

  fn <- function(key) {
    b    <- .q_walk_batch(dev, key)
    pred <- .q_net_forward(net, b$x, consts)              # [n_a, B]
    ggmlR::ag_mse_loss(ggmlR::ag_mul(pred, b$mask), b$y)
  }
  params <- c(.q_walk_params(dev), list(consts$tile, consts$pool))
  step <- ggmlR::ag_capture_step(fn, params = params, optimizer = opt)

  st <- new.env(parent = emptyenv())
  st$net <- net; st$dev <- dev; st$consts <- consts; st$opt <- opt
  st$step <- step; st$seed <- seed; st$n <- 0
  st
}

# The key step number `n` (0-based) of a run with this seed.
.q_train_key <- function(st, n = st$n) {
  c(n %% .q_key_limit, st$seed + n %/% .q_key_limit)
}

# One step at learning rate lr; returns the loss of the batch it trained on
# (computed before the update).
.q_train_step <- function(st, lr = NULL) {
  if (!is.null(lr)) st$opt$lr <- lr
  loss <- st$step(key = matrix(.q_train_key(st), 2L, 1L))
  st$n <- st$n + 1
  as.numeric(loss)
}

# ---------------------------------------------------------------------------
# The training run
# ---------------------------------------------------------------------------

#' Train the Cube Q-Network Entirely on the GPU
#'
#' Trains the same transformer Q-network as
#' \code{inst/examples/train_cube3_q_online.R} on the same data -- pivot points
#' of random walks labelled with the two moves that meet there -- but with
#' every step done on the device: the walks, their encoding, the labels, the
#' forward and backward pass and Adam form one recorded graph
#' (\code{ggmlR::ag_capture_step}), whose only input is a two-number random key.
#' The result is written as an ordinary \code{ggml_model} file with the same
#' \code{meta.rds} the online script writes, so \code{ggml_load_model()} and the
#' beam search read it as they read that script's models.
#'
#' @section Requirements:
#' Needs a ggmlR with batched \code{ag_flash_attention()}, \code{ag_silu()} and
#' \code{ag_rms_norm()} (0.8.9.2 or later); with an older one it stops at once
#' and names what is missing. Models it writes load with any ggmlR that reads
#' the online script's models.
#'
#' @section Data:
#' A step draws \code{batch} pivots: a cut uniform on \code{k_min..depth-1},
#' a walk long enough to contain it that never undoes its last move and, with
#' \code{no_cancel}, never cancels against its tail through commuting moves,
#' and the two moves meeting at the cut with labels \code{cut - 1} and
#' \code{cut + 1}. These are the rules of \code{\link{generate_walk}}; the
#' random numbers are the device's, so batches match it in distribution, not
#' bit for bit. The held-out set is drawn on the host with
#' \code{generate_walk()} from \code{seed}, as in the online script, so scores
#' are comparable across both.
#'
#' @section Loss and schedule:
#' Masked MSE normalised by \code{batch * n_moves}, as
#' \code{ggml_trainer(loss_mask = )} computes it in the online script; learning
#' rate \code{"cosine"} (linear warm-up over \code{warmup} steps, then cosine
#' decay to \code{lr_min}) or \code{"const"}.
#'
#' @section Checkpoints and continuing:
#' With \code{ckpt > 0} the model, \code{meta.rds} and the Adam state are
#' written every \code{ckpt} steps (each file to a temporary name, then renamed;
#' \code{meta.rds} last). \code{resume} continues such a run from its last
#' checkpoint with everything taken from its \code{meta.rds}; unlike the online
#' script it restores Adam's moments too, and the training stream continues
#' exactly where it stopped (step \code{n} always draws with key
#' \code{(n - 1, seed)}). \code{init} starts a new run from the weights of a
#' saved model -- this function's or the online script's -- keeping its
#' architecture and target scaling.
#'
#' @param group A \code{perm_group} from \code{\link{cube_group}}; every move
#'   needs its inverse in the alphabet.
#' @param steps Training steps; \code{steps * batch} states are seen.
#' @param batch States per step.
#' @param depth Longest walk; cuts fall on \code{k_min..depth-1}.
#' @param k_min Shallowest cut.
#' @param no_cancel Refuse moves that cancel against the walk's tail.
#' @param val Size of the held-out set.
#' @param d_model,heads,blocks,ff Transformer width, attention heads, blocks
#'   and feed-forward width.
#' @param lr,lr_min,warmup,sched Learning-rate schedule (see Details).
#' @param seed Seed of the held-out set, the initial weights and the training
#'   stream.
#' @param report Steps between progress lines (and history rows).
#' @param ckpt Steps between checkpoints; 0 writes only at the end.
#' @param out Directory the run is written into (created); \code{NULL} writes
#'   nothing.
#' @param init Directory of a saved model to start from, or \code{NULL}.
#' @param resume Directory of a checkpointed run to continue, or \code{NULL}.
#'   Everything but \code{report} and \code{ckpt} comes from the run itself.
#' @param device \code{"gpu"} (default) or \code{"cpu"}; the CPU runs the same
#'   step op by op, for checking rather than training.
#' @param verbose Print progress.
#' @return Invisibly, a list with \code{dir} (where the run was written, or
#'   \code{NULL}), \code{history} (a data frame of the reports) and
#'   \code{score} (the final held-out scores: \code{mae_prev}, \code{mae_next},
#'   \code{pair_accuracy}, \code{top1_accuracy}).
#' @seealso \code{\link{generate_walk}}, \code{\link{cube_piece_layout}}
#' @export
#' @examples
#' \dontrun{
#' res <- cube_q_train_device(cube_group(3), steps = 2000, out = tempdir())
#' res$score$pair_accuracy
#' }
cube_q_train_device <- function(group = cube_group(3), steps = 20000L,
                                batch = 512L, depth = 20L, k_min = 2L,
                                no_cancel = TRUE, val = 4000L,
                                d_model = 64L, heads = 4L, blocks = 3L,
                                ff = 256L, lr = 1e-3, lr_min = 1e-5,
                                warmup = 500L, sched = c("cosine", "const"),
                                seed = 42L, report = 250L, ckpt = 0L,
                                out = NULL, init = NULL, resume = NULL,
                                device = c("gpu", "cpu"), verbose = TRUE) {
  .q_device_require()
  sched  <- match.arg(sched)
  device <- match.arg(device)
  say <- function(...) if (verbose) { cat(sprintf(...)); utils::flush.console() }

  # ---- a run to continue takes everything from itself --------------------
  resume_meta <- NULL
  if (!is.null(resume)) {
    if (!is.null(init)) stop("resume and init do not go together", call. = FALSE)
    resume <- path.expand(resume)
    resume_meta <- readRDS(file.path(resume, "meta.rds"))
    if (!identical(resume_meta$trainer, "device"))
      stop("resume: ", resume, " was not written by cube_q_train_device(); ",
           "use init = to train on from it", call. = FALSE)
    if (resume_meta$step_done >= resume_meta$steps)
      stop("resume: the run finished all its steps; use init = to train on",
           call. = FALSE)
    for (k in c("steps", "batch", "depth", "k_min", "val", "d_model", "heads",
                "blocks", "ff", "lr", "lr_min", "warmup", "sched", "seed"))
      assign(k, resume_meta[[k]])
    no_cancel <- isTRUE(resume_meta$no_cancel)
    if (!identical(as.character(resume_meta$moves), as.character(group$moves)))
      stop("resume: the run was trained on a different move set", call. = FALSE)
    out <- resume
  }
  steps <- as.integer(steps); batch <- as.integer(batch); report <- as.integer(report)
  ckpt <- as.integer(ckpt)

  old_dev <- ggmlR::ag_device(device)
  old_dt  <- ggmlR::ag_dtype("f32")
  on.exit({ ggmlR::ag_dtype(old_dt); ggmlR::ag_device(old_dev) }, add = TRUE)

  lay <- cube_piece_layout(group)
  mv  <- group$moves; n_a <- length(mv)
  y_mu <- (1 + depth) / 2; y_sd <- stats::sd(seq_len(depth))

  init_meta <- NULL; init_file <- NULL
  if (!is.null(init)) {
    init <- path.expand(init)
    init_meta <- readRDS(file.path(init, "meta.rds"))
    if (!identical(as.character(init_meta$moves), as.character(mv)))
      stop("init: the model was trained on a different move set", call. = FALSE)
    for (k in c("d_model", "heads", "blocks", "ff")) assign(k, init_meta[[k]])
    y_mu <- init_meta$y_mu; y_sd <- init_meta$y_sd
    init_file <- file.path(init, "model.ggml")
  }
  if (!is.null(resume_meta)) {
    y_mu <- resume_meta$y_mu; y_sd <- resume_meta$y_sd
    init_file <- file.path(resume, "model.ggml")
  }

  tab  <- .q_walk_tables(group, lay, depth, k_min = k_min, no_cancel = no_cancel,
                         y_mu = y_mu, y_sd = y_sd)
  spec <- .q_net_spec(lay$n_piece, tab$depth_dim, n_a, d_model = d_model,
                      heads = heads, blocks = blocks, ff = ff)

  # ---- the held-out set, on the host, as the online script draws it ------
  set.seed(seed)
  w  <- generate_walk(group, n = val, n_moves = depth, exact = FALSE,
                      no_undo = TRUE, pivot = TRUE, k_min = k_min,
                      no_cancel = no_cancel)
  va <- list(x = adi_encode(w$states, "transformer", lay),
             p = as.integer(w$depth),
             prev_a = match(w$prev_move, mv), next_a = match(w$next_move, mv))

  # ---- the net --------------------------------------------------------------
  init_fn <- NULL
  if (!is.null(init_file)) {
    wts <- .q_net_weights_from_file(init_file, spec)
    init_fn <- function(name, nr, nc) {
      m <- wts[[name]]
      if (is.null(m) || !identical(dim(m), c(as.integer(nr), as.integer(nc))))
        stop("init: weight '", name, "' does not fit this architecture",
             call. = FALSE)
      m
    }
  }
  net <- .q_net_ag(spec, init = init_fn)
  dev <- .q_walk_device(tab, batch)
  st  <- .q_train_state(net, dev, lr = lr, seed = seed)

  first_step <- 1L; history <- list(); secs_before <- 0; resumed_at <- integer(0)
  if (!is.null(resume_meta)) {
    first_step  <- resume_meta$step_done + 1L
    st$n        <- resume_meta$step_done           # the key continues from here
    history     <- if (is.null(resume_meta$history)) list() else
      split(resume_meta$history, seq_len(nrow(resume_meta$history)))
    secs_before <- resume_meta$train_secs
    resumed_at  <- c(resume_meta$resumed_at, resume_meta$step_done)
    adam <- file.path(resume, "adam.rds")
    # The saved lr is the schedule's at that step, not the base one the
    # optimizer was built with; the schedule sets it again every step, so that
    # one difference is expected and silenced. Any other still warns.
    if (file.exists(adam))
      withCallingHandlers(st$opt$load_state(readRDS(adam)),
        ggmlR_opt_hyper_warning = function(w) {
          if (identical(names(w$changed), "lr")) invokeRestart("muffleWarning")
        })
  }

  lr_at <- function(step) {
    if (sched == "const") return(lr)
    if (step <= warmup) return(lr * step / warmup)
    t <- (step - warmup) / max(1, steps - warmup)
    lr_min + 0.5 * (lr - lr_min) * (1 + cos(pi * t))
  }

  # ---- scoring: the online script's numbers, from an f32 forward ----------
  # Eager, outside the recording, with constants of its own; the precision is
  # switched only around it, so the recorded step is not affected.
  score_consts <- .q_net_consts(spec, batch)
  predict_va <- function() {
    old_p <- ggmlR::ag_matmul_precision("f32")
    on.exit(ggmlR::ag_matmul_precision(old_p))
    n <- dim(va$x)[1L]; outp <- matrix(0, n, n_a)
    for (lo in seq(1L, n, by = batch)) {
      idx <- lo:min(n, lo + batch - 1L)
      take <- c(idx, rep(idx[1L], batch - length(idx)))  # pad a short chunk
      # A private pass pool, freed after each chunk: without a tape nothing
      # else frees it, and every score() kept ~2.7 GB of VRAM until the
      # driver refused submission (cube3 run, step ~2000). Only the host
      # matrix leaves the scope.
      pr <- ggmlR::ag_pass_scope({
        xin <- ggmlR::ag_tensor(.q_net_input(va$x[take, , , drop = FALSE]))
        t(as.matrix(.q_net_forward(net, xin, score_consts)))
      })
      outp[idx, ] <- pr[seq_along(idx), ]
    }
    outp
  }
  score <- function() {
    vp <- predict_va() * y_sd + y_mu
    rows <- seq_len(nrow(vp))
    q_prev <- vp[cbind(rows, va$prev_a)]; q_next <- vp[cbind(rows, va$next_a)]
    list(vp = vp, q_prev = q_prev, q_next = q_next,
         mae_prev = mean(abs(q_prev - (va$p - 1L))),
         mae_next = mean(abs(q_next - (va$p + 1L))),
         pair = mean(q_prev < q_next),
         top1 = mean(max.col(-vp, ties.method = "first") == va$prev_a))
  }

  # ---- where it is written ----------------------------------------------------
  if (!is.null(out) && is.null(resume_meta)) {
    out <- file.path(path.expand(out), sprintf(
      "cube3_qdev_m%d_h%d_b%d_d%d_s%dk_b%d_%s%s%s_s%d", d_model, heads, blocks,
      depth, round(steps / 1000), batch,
      if (sched == "cosine") "cos" else sched, if (no_cancel) "_nc" else "",
      if (!is.null(init)) "_init" else "", seed))
  }
  if (!is.null(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

  save_run <- function(step_done, s, secs) {
    tmp <- function(f) file.path(out, paste0(f, ".tmp"))
    n_chk <- min(64L, dim(va$x)[1L])
    .q_net_export(net, tmp("model.ggml"), backend = "cpu",
                  check_x = va$x[seq_len(n_chk), , , drop = FALSE], tol = 2e-3)
    saveRDS(st$opt$state(), tmp("adam.rds"))
    saveRDS(list(version = 2L, kind = "q_online", trainer = "device",
                 y_mu = y_mu, y_sd = y_sd, moves = mv, n_out = n_a,
                 depth = depth, d_model = d_model, heads = heads,
                 blocks = blocks, ff = ff, steps = steps, batch = batch,
                 val = val, k_min = k_min, step_done = step_done,
                 states_seen = as.numeric(step_done) * batch,
                 lr = lr, lr_min = lr_min, warmup = warmup, sched = sched,
                 no_cancel = no_cancel, ckpt = ckpt, report = report,
                 init = init, resumed_at = resumed_at, seed = seed,
                 mae_prev = s$mae_prev, mae_next = s$mae_next,
                 pair_accuracy = s$pair, top1_accuracy = s$top1,
                 history = do.call(rbind, history), train_secs = secs),
            tmp("meta.rds"))
    for (f in c("model.ggml", "adam.rds", "meta.rds"))
      file.rename(tmp(f), file.path(out, f))
    invisible(NULL)
  }

  # ---- training --------------------------------------------------------------
  say("cube_q_train_device: %s, %d moves, %d pieces; steps %d to %d, batch %d\n",
      group$name, n_a, lay$n_piece, first_step, steps, batch)
  say("    step        seen   pair    top1   MAE(prev)       lr    secs\n")
  t_start <- proc.time()[["elapsed"]]
  for (step in seq.int(first_step, steps)) {
    lr_s <- lr_at(step)
    loss <- .q_train_step(st, lr_s)
    if (!is.finite(loss)) stop("the loss became ", loss, " at step ", step,
                               call. = FALSE)
    if (step %% report == 0L || step == steps) {
      s <- score()
      history[[length(history) + 1L]] <- data.frame(
        step = step, seen = as.numeric(step) * batch, lr = lr_s, loss = loss,
        pair = s$pair, top1 = s$top1, mae_prev = s$mae_prev)
      say("  %6d  %10s  %5.1f%%  %5.1f%%   %7.2f   %.1e  %6.0f\n", step,
          format(as.numeric(step) * batch, big.mark = ",", scientific = FALSE),
          100 * s$pair, 100 * s$top1, s$mae_prev, lr_s,
          proc.time()[["elapsed"]] - t_start)
    }
    if (!is.null(out) && ckpt > 0L && step %% ckpt == 0L && step < steps) {
      save_run(step, score(), secs_before + proc.time()[["elapsed"]] - t_start)
      say("  %6d  checkpoint saved\n", step)
    }
  }
  train_secs <- secs_before + proc.time()[["elapsed"]] - t_start

  s <- score()
  say("\ntrained in %.0f s;  pair %.1f%%  top1 %.1f%%  MAE(prev) %.2f  MAE(next) %.2f\n",
      train_secs, 100 * s$pair, 100 * s$top1, s$mae_prev, s$mae_next)
  say("  by depth:\n")
  for (d in sort(unique(va$p))) {
    k <- va$p == d
    say("    %3d moves  n=%4d   pair %5.1f%%   top1 %5.1f%%   q(prev) %6.2f\n",
        d, sum(k), 100 * mean(s$q_prev[k] < s$q_next[k]),
        100 * mean(max.col(-s$vp[k, , drop = FALSE], ties.method = "first") ==
                     va$prev_a[k]),
        mean(s$q_prev[k]))
  }
  if (!is.null(out)) {
    save_run(steps, s, train_secs)
    say("saved to %s\n", out)
  }
  invisible(list(dir = out, history = do.call(rbind, history),
                 score = list(mae_prev = s$mae_prev, mae_next = s$mae_next,
                              pair_accuracy = s$pair, top1_accuracy = s$top1)))
}
