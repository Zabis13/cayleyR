# The Q-network of inst/examples/train_cube3_q_online.R, in two forms.
#
# The online Q script trains a transformer through ggmlR's Keras-like API
# (ggml_model / ggml_trainer), and beam search reads it back with
# ggml_load_model / ggml_predict. Training it entirely on the device -- walks,
# encoding, labels and the step in one recorded graph -- needs ggmlR's ag_*
# engine instead, which builds a different object. This file holds ONE
# description of the architecture and builds both forms from it, plus the map
# that carries ag_* weights into a ggml_model file beam search already reads.
#
# The two forms compute the same function:
#
#   x  [n_piece, depth_dim] per state  ->  dense (per token)  ->  + pos
#   -> blocks x { h + attn(rms1(h));  h + ff2(silu(ff1(rms2(h)))) }
#   -> mean over tokens  ->  dense n_a
#
# ag_* is two-dimensional, so a batch of N states is [features, n_piece * N]
# with each state's tokens in adjacent columns -- byte for byte the ggml tensor
# [features, n_piece, N] that ggml_predict builds from the R array
# aperm(x, c(3, 2, 1)). Attention runs over all N sequences in one call
# (ag_flash_attention(batch = N)); adding the position table and pooling over
# tokens are products with constant 0/1 matrices, which keeps every op one the
# graph backward can emit.
#
# Weight layouts (ggml order, flat):
#   dense       weight [in, out] = t(W) for an ag W [out, in]; bias [out]
#   attention   W_q/W_k/W_v/W_o likewise t(W); b_o [d]; no q/k/v biases;
#               heads are contiguous row blocks; scale 1/sqrt(d_head)
#   rms_norm    gamma [d], beta [d], eps 1e-5
#   pos         [d_model, n_piece], as is

# ---------------------------------------------------------------------------
# What the installed ggmlR has to offer
# ---------------------------------------------------------------------------

# Checked by capability, not by version number: the functions this needs, and
# the batch argument of ag_flash_attention. Stops with the list of what is
# missing, so an old ggmlR is a clear error rather than a failure deep inside.
.q_device_require <- function() {
  if (!requireNamespace("ggmlR", quietly = TRUE))
    stop("the on-device Q network needs the ggmlR package", call. = FALSE)
  ns   <- asNamespace("ggmlR")
  need <- c("ag_rms_norm", "ag_silu", "ag_flash_attention", "ag_param",
            "ag_tensor", "ag_matmul", "ag_add", "ggml_layer_transformer_block",
            "ggml_layer_positional_embedding", "ggml_layer_sequence_pooling",
            "ag_buffer", "ag_buffer_write", "ag_get_rows",
            "ag_sample_categorical", "ag_capture_step",
            "optimizer_adam", "ag_mse_loss")
  missing <- need[!vapply(need, exists, logical(1), envir = ns, inherits = FALSE)]
  has_arg <- function(f, a)
    f %in% missing || a %in% names(formals(get(f, envir = ns)))
  if (!has_arg("ag_flash_attention", "batch"))
    missing <- c(missing, "ag_flash_attention(batch = )")
  if (!has_arg("ag_sample_categorical", "key"))
    missing <- c(missing, "ag_sample_categorical(key = )")
  if (length(missing))
    stop("the on-device Q network needs a newer ggmlR (installed ",
         as.character(utils::packageVersion("ggmlR")), "); missing: ",
         paste(missing, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# The architecture, once
# ---------------------------------------------------------------------------

.q_net_spec <- function(n_piece, depth_dim, n_a, d_model = 64L, heads = 4L,
                        blocks = 3L, ff = 256L) {
  spec <- list(n_piece = as.integer(n_piece), depth_dim = as.integer(depth_dim),
               n_a = as.integer(n_a), d_model = as.integer(d_model),
               heads = as.integer(heads), blocks = as.integer(blocks),
               ff = as.integer(ff))
  if (spec$d_model %% spec$heads != 0L)
    stop("d_model must be a multiple of heads", call. = FALSE)
  spec
}

# The ggml_model form. Same layers and options as build_net() in
# train_cube3_q_online.R; the weighted layers are named so the exporter can
# find them (the blocks name their own parts block<b>_norm1, _attn, _norm2,
# _ff1, _ff2).
.q_net_ggml <- function(spec, backend = "auto") {
  inp <- ggmlR::ggml_input(shape = c(spec$n_piece, spec$depth_dim))
  h   <- ggmlR::ggml_layer_dense(inp, spec$d_model, time_distributed = TRUE,
                                 name = "embed")
  h   <- ggmlR::ggml_layer_positional_embedding(h, name = "pos")
  for (b in seq_len(spec$blocks)) {
    h <- ggmlR::ggml_layer_transformer_block(
      h, spec$d_model, n_heads = spec$heads, ff_dim = spec$ff,
      activation = "silu", norm = "rms", name = paste0("block", b))
  }
  h   <- ggmlR::ggml_layer_sequence_pooling(h, mode = "mean")
  out <- ggmlR::ggml_layer_dense(h, spec$n_a, name = "head")
  ggmlR::ggml_compile(ggmlR::ggml_model(inputs = inp, outputs = out),
                      optimizer = "adam", loss = "mse", metrics = NULL,
                      backend = backend)
}

# ---------------------------------------------------------------------------
# The ag_* form
# ---------------------------------------------------------------------------

# Parameters on the current ag device. Initialised as ggmlR initialises the
# ggml_model form (glorot-uniform kernels, zero biases, unit gamma, zero beta,
# N(0, 0.02) positions), unless `init` is given: function(name, nr, nc)
# returning the matrix to use, or NULL for the default -- the tests use it to
# make every parameter, biases and beta included, random and non-zero.
.q_net_ag <- function(spec, init = NULL) {
  d <- spec$d_model
  mat <- function(name, nr, nc, default) {
    m <- if (is.null(init)) NULL else init(name, nr, nc)
    if (is.null(m)) m <- default(nr, nc)
    ggmlR::ag_param(matrix(as.numeric(m), nr, nc))
  }
  glorot <- function(nr, nc) {
    a <- sqrt(6 / (nr + nc))
    matrix(stats::runif(nr * nc, -a, a), nr, nc)
  }
  zeros <- function(nr, nc) matrix(0, nr, nc)
  norm <- function(name) {
    m <- ggmlR::ag_rms_norm(d, eps = 1e-5)
    m$gamma <- mat(paste0(name, "_gamma"), d, 1L, function(nr, nc) matrix(1, nr, nc))
    m$beta  <- mat(paste0(name, "_beta"),  d, 1L, zeros)
    m
  }

  net <- list(spec = spec)
  net$embed_W <- mat("embed_W", d, spec$depth_dim, glorot)
  net$embed_b <- mat("embed_b", d, 1L, zeros)
  net$pos     <- mat("pos", d, spec$n_piece,
                     function(nr, nc) matrix(stats::rnorm(nr * nc, 0, 0.02), nr, nc))
  net$blocks <- lapply(seq_len(spec$blocks), function(b) {
    p <- paste0("block", b, "_")
    list(norm1 = norm(paste0(p, "norm1")),
         W_q = mat(paste0(p, "W_q"), d, d, glorot),
         W_k = mat(paste0(p, "W_k"), d, d, glorot),
         W_v = mat(paste0(p, "W_v"), d, d, glorot),
         W_o = mat(paste0(p, "W_o"), d, d, glorot),
         b_o = mat(paste0(p, "b_o"), d, 1L, zeros),
         norm2 = norm(paste0(p, "norm2")),
         ff1_W = mat(paste0(p, "ff1_W"), spec$ff, d, glorot),
         ff1_b = mat(paste0(p, "ff1_b"), spec$ff, 1L, zeros),
         ff2_W = mat(paste0(p, "ff2_W"), d, spec$ff, glorot),
         ff2_b = mat(paste0(p, "ff2_b"), d, 1L, zeros))
  })
  net$head_W <- mat("head_W", spec$n_a, d, glorot)
  net$head_b <- mat("head_b", spec$n_a, 1L, zeros)
  net
}

# Every trainable tensor, for the optimizer, named as `init` names them -- so a
# saved optimizer state is matched by name, not by position.
.q_net_params <- function(net) {
  blk <- lapply(seq_along(net$blocks), function(b) {
    k <- net$blocks[[b]]
    stats::setNames(
      list(k$norm1$gamma, k$norm1$beta, k$W_q, k$W_k, k$W_v, k$W_o, k$b_o,
           k$norm2$gamma, k$norm2$beta, k$ff1_W, k$ff1_b, k$ff2_W, k$ff2_b),
      paste0("block", b, "_",
             c("norm1_gamma", "norm1_beta", "W_q", "W_k", "W_v", "W_o", "b_o",
               "norm2_gamma", "norm2_beta", "ff1_W", "ff1_b", "ff2_W", "ff2_b")))
  })
  c(list(embed_W = net$embed_W, embed_b = net$embed_b, pos = net$pos),
    unlist(blk, recursive = FALSE),
    list(head_W = net$head_W, head_b = net$head_b))
}

# The weights of a saved ggml_model (a file from ggml_save_model: this
# package's export or the online script's own models), keyed as `init` asks
# for them -- the reverse of .q_net_weights_data. The blocks are found by name
# (block<b>_...), the rest by place, because the online script left them
# unnamed: the dense fed by the input is the embedding, the dense fed by the
# pooling is the head.
.q_net_weights_from_file <- function(path, spec) {
  data <- readRDS(path)
  nwd  <- data$node_weights_data
  if (is.null(nwd)) stop("init: ", path, " holds no weights", call. = FALSE)
  nodes <- list(); seen <- character()
  walk <- function(nd) {
    if (nd$id %in% seen) return(invisible())
    seen <<- c(seen, nd$id); nodes[[nd$id]] <<- nd
    for (p in nd$parents) walk(p)
  }
  for (o in data$outputs) walk(o)
  ptype <- function(nd) if (length(nd$parents)) nd$parents[[1]]$node_type else ""
  find <- function(pred, what) {
    hit <- Filter(pred, nodes)
    if (length(hit) != 1L)
      stop("init: cannot find the ", what, " layer in ", path, call. = FALSE)
    nwd[[hit[[1]]$id]]
  }
  by_name <- function(nm) find(function(nd) identical(nd$config$name, nm), nm)

  d <- spec$d_model; out <- list()
  dense <- function(prefix, w) {
    n_out <- length(w$bias)
    out[[paste0(prefix, "_W")]] <<- t(matrix(w$weight, ncol = n_out))
    out[[paste0(prefix, "_b")]] <<- matrix(w$bias, n_out, 1L)
  }
  dense("embed", find(function(nd) nd$node_type == "dense" && ptype(nd) == "input",
                      "embedding"))
  out$pos <- matrix(find(function(nd) nd$node_type == "positional_embedding",
                         "position")$pos, d, spec$n_piece)
  for (b in seq_len(spec$blocks)) {
    p <- paste0("block", b, "_")
    for (nm in c("norm1", "norm2")) {
      w <- by_name(paste0(p, nm))
      out[[paste0(p, nm, "_gamma")]] <- matrix(w$gamma, d, 1L)
      out[[paste0(p, nm, "_beta")]]  <- matrix(w$beta,  d, 1L)
    }
    a <- by_name(paste0(p, "attn"))
    for (k in c("W_q", "W_k", "W_v", "W_o"))
      out[[paste0(p, k)]] <- t(matrix(a[[k]], d, d))
    out[[paste0(p, "b_o")]] <- matrix(if (is.null(a$b_o)) 0 else a$b_o, d, 1L)
    dense(paste0(p, "ff1"), by_name(paste0(p, "ff1")))
    dense(paste0(p, "ff2"), by_name(paste0(p, "ff2")))
  }
  dense("head", find(function(nd) nd$node_type == "dense" &&
                       ptype(nd) == "sequence_pooling", "head"))
  out
}

# The constant matrices for a batch of n states of n_piece tokens:
#   tile [n_piece, n_piece * n]   pos %*% tile repeats the table for every state
#   pool [n_piece * n, n]         h %*% pool is each state's mean over tokens
# Data-independent, so they are made once per batch size and, when recorded,
# passed in as constants rather than rebuilt.
.q_net_consts <- function(spec, n) {
  s <- spec$n_piece; cols <- seq_len(s * n)
  tile <- matrix(0, s, s * n)
  tile[cbind(rep(seq_len(s), n), cols)] <- 1
  pool <- matrix(0, s * n, n)
  pool[cbind(cols, rep(seq_len(n), each = s))] <- 1 / s
  list(n = as.integer(n), tile = ggmlR::ag_tensor(tile),
       pool = ggmlR::ag_tensor(pool))
}

# An encoded batch, as adi_encode(states, "transformer", layout) returns it
# ([n, n_piece, depth_dim]), in the ag layout [depth_dim, n_piece * n].
.q_net_input <- function(x) {
  dm <- dim(x)
  matrix(as.numeric(aperm(x, c(3L, 2L, 1L))), dm[3L], dm[2L] * dm[1L])
}

# Pieces of the forward, kept separate so the tests can check them one layer
# at a time against the matching ggml layer.
.q_lin <- function(W, b, h) ggmlR::ag_add(ggmlR::ag_matmul(W, h), b)

.q_attn <- function(blk, a, spec, n) {
  q <- ggmlR::ag_matmul(blk$W_q, a)
  k <- ggmlR::ag_matmul(blk$W_k, a)
  v <- ggmlR::ag_matmul(blk$W_v, a)
  o <- ggmlR::ag_flash_attention(q, k, v, n_heads = spec$heads, batch = n)
  .q_lin(blk$W_o, blk$b_o, o)
}

.q_block <- function(blk, h, spec, n) {
  h <- ggmlR::ag_add(h, .q_attn(blk, blk$norm1$forward(h), spec, n))
  f <- ggmlR::ag_silu(.q_lin(blk$ff1_W, blk$ff1_b, blk$norm2$forward(h)))
  ggmlR::ag_add(h, .q_lin(blk$ff2_W, blk$ff2_b, f))
}

# x: ag tensor or matrix [depth_dim, n_piece * n]. Returns [n_a, n].
.q_net_forward <- function(net, x, consts) {
  spec <- net$spec; n <- consts$n
  h <- .q_lin(net$embed_W, net$embed_b, x)
  h <- ggmlR::ag_add(h, ggmlR::ag_matmul(net$pos, consts$tile))
  for (blk in net$blocks) h <- .q_block(blk, h, spec, n)
  .q_lin(net$head_W, net$head_b, ggmlR::ag_matmul(h, consts$pool))
}

# Forward on an encoded batch, as ggml_predict would answer it: [n, n_a].
.q_net_predict_ag <- function(net, x) {
  consts <- .q_net_consts(net$spec, dim(x)[1L])
  t(as.matrix(.q_net_forward(net, ggmlR::ag_tensor(.q_net_input(x)), consts)))
}

# ---------------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------------

# Weighted nodes of a ggml_model, by name, found by walking up from the output.
.q_ggml_nodes <- function(model) {
  # Each residual add has two parents, so without the visited set the walk
  # would double at every block.
  found <- list(); seen <- character()
  walk <- function(nd) {
    if (nd$id %in% seen) return(invisible())
    seen <<- c(seen, nd$id)
    nm <- nd$config$name
    if (!is.null(nm)) found[[nm]] <<- nd
    for (p in nd$parents) walk(p)
  }
  for (o in model$outputs) walk(o)
  found
}

# One layer's weights in the flat ggml layout node_weights_data holds.
.q_w_dense <- function(W, b) list(weight = as.numeric(t(as.matrix(W))),
                                  bias   = as.numeric(as.matrix(b)))
.q_w_norm  <- function(m) list(gamma = as.numeric(as.matrix(m$gamma)),
                               beta  = as.numeric(as.matrix(m$beta)))
.q_w_pos   <- function(P) list(pos = as.numeric(as.matrix(P)))
.q_w_attn  <- function(blk) list(W_q = as.numeric(t(as.matrix(blk$W_q))),
                                 W_k = as.numeric(t(as.matrix(blk$W_k))),
                                 W_v = as.numeric(t(as.matrix(blk$W_v))),
                                 W_o = as.numeric(t(as.matrix(blk$W_o))),
                                 b_o = as.numeric(as.matrix(blk$b_o)))

# node id -> weights, for every weighted node of `model` (built by .q_net_ggml).
.q_net_weights_data <- function(net, model) {
  nodes <- .q_ggml_nodes(model)
  out <- list()
  put <- function(name, w) {
    nd <- nodes[[name]]
    if (is.null(nd)) stop("export: the ggml model has no layer '", name, "'",
                          call. = FALSE)
    out[[nd$id]] <<- w
  }
  put("embed", .q_w_dense(net$embed_W, net$embed_b))
  put("pos", .q_w_pos(net$pos))
  for (b in seq_along(net$blocks)) {
    blk <- net$blocks[[b]]; p <- paste0("block", b, "_")
    put(paste0(p, "norm1"), .q_w_norm(blk$norm1))
    put(paste0(p, "attn"),  .q_w_attn(blk))
    put(paste0(p, "norm2"), .q_w_norm(blk$norm2))
    put(paste0(p, "ff1"),   .q_w_dense(blk$ff1_W, blk$ff1_b))
    put(paste0(p, "ff2"),   .q_w_dense(blk$ff2_W, blk$ff2_b))
  }
  put("head", .q_w_dense(net$head_W, net$head_b))
  out
}

# Relative error of a against a reference, by its largest and its mean entry.
.q_rel_err <- function(a, ref) {
  c(max  = max(abs(a - ref)) / max(abs(ref)),
    mean = mean(abs(a - ref)) / mean(abs(ref)))
}

# Write the ag_* net as a ggml_model file that ggml_load_model() and beam search
# read like any other. With `check_x` (an encoded batch) the ggml model's
# predictions are compared with the ag forward before anything is written, and
# a relative error above `tol` stops the export: a wrong layout must not reach
# a file. The default tol is f16-sized, the accuracy of Vulkan's attention.
.q_net_export <- function(net, path, backend = "auto", check_x = NULL,
                          tol = 2e-3) {
  .q_device_require()
  model <- .q_net_ggml(net$spec, backend = backend)
  model$node_weights_data <- .q_net_weights_data(net, model)

  err <- NULL
  if (!is.null(check_x)) {
    pg  <- ggmlR::ggml_predict(model, check_x, batch_size = dim(check_x)[1L])
    pa  <- .q_net_predict_ag(net, check_x)
    err <- .q_rel_err(pg, pa)
    if (any(err > tol))
      stop(sprintf(paste0("export: the ggml model disagrees with the ag net ",
                          "(relative max %.3g, mean %.3g; tol %.3g) -- not written"),
                   err[["max"]], err[["mean"]], tol), call. = FALSE)
  }

  ggmlR::ggml_save_model(model, path)
  # An older ggml_save_model wrote a never-fitted functional model without its
  # weights; reading the file back is the only reliable way to know.
  if (is.null(readRDS(path)$node_weights_data)) {
    unlink(path)
    stop("export: the installed ggmlR saved the model without its weights ",
         "(ggml_save_model ignores node_weights_data); update ggmlR",
         call. = FALSE)
  }
  invisible(list(path = path, err = err))
}
