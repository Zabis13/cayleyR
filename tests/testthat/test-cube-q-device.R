# The ag_* form of the Q network against its ggml_model form, layer by layer.
#
# Each check builds a small ggml_model -- per-token dense, then the layer under
# test, then mean pooling -- gives it the ag net's weights through the export
# map, and compares ggml_predict with the ag forward. Done one layer at a time
# so that a wrong transpose or head order shows up in the layer that has it,
# not as an unexplained difference at the end.
#
# On the CPU both sides compute in f32 and must agree to ~1e-6; a relative
# error at 1e-5 there is a layout bug, not arithmetic. On Vulkan attention runs
# with f16 operands, so the whole-net GPU check gets an f16-sized tolerance.
#
# Every parameter is random and non-zero -- biases, beta and the attention
# output bias included. A default-initialised net has zero biases and beta, and
# would agree even if those were never exported.

skip_if_no_q_device <- function() {
  skip_if_not_installed("ggmlR")
  ok <- tryCatch(cayleyR:::.q_device_require(), error = function(e) FALSE)
  skip_if_not(isTRUE(ok), "ggmlR lacks the on-device Q-network functions")
}

rand_init <- function(name, nr, nc) {
  if (grepl("gamma$", name)) matrix(stats::runif(nr * nc, 0.5, 1.5), nr, nc)
  else matrix(stats::runif(nr * nc, -0.5, 0.5), nr, nc)
}

spec_small <- function() {
  cayleyR:::.q_net_spec(n_piece = 5L, depth_dim = 12L, n_a = 3L,
                        d_model = 8L, heads = 2L, blocks = 2L, ff = 16L)
}

rand_x <- function(spec, n) {
  array(stats::runif(n * spec$n_piece * spec$depth_dim, -1, 1),
        dim = c(n, spec$n_piece, spec$depth_dim))
}

# ggml side: input -> dense(embed) -> `layer` -> mean pooling.
mini_ggml <- function(spec, layer) {
  inp <- ggmlR::ggml_input(shape = c(spec$n_piece, spec$depth_dim))
  h <- ggmlR::ggml_layer_dense(inp, spec$d_model, time_distributed = TRUE,
                               name = "embed")
  h <- switch(layer,
    embed = h,
    pos   = ggmlR::ggml_layer_positional_embedding(h, name = "pos"),
    norm  = ggmlR::ggml_layer_rms_norm(h, eps = 1e-5, name = "block1_norm1"),
    attn  = ggmlR::ggml_layer_attention(h, spec$d_model, n_heads = spec$heads,
                                        name = "block1_attn"),
    block = ggmlR::ggml_layer_transformer_block(
              h, spec$d_model, n_heads = spec$heads, ff_dim = spec$ff,
              activation = "silu", norm = "rms", name = "block1"))
  out <- ggmlR::ggml_layer_sequence_pooling(h, mode = "mean")
  ggmlR::ggml_compile(ggmlR::ggml_model(inputs = inp, outputs = out),
                      optimizer = "adam", loss = "mse", metrics = NULL,
                      backend = "cpu")
}

# The export map, restricted to the layers a mini model has.
mini_weights <- function(net, model) {
  nodes <- cayleyR:::.q_ggml_nodes(model)
  blk <- net$blocks[[1]]
  all <- list(
    embed        = cayleyR:::.q_w_dense(net$embed_W, net$embed_b),
    pos          = cayleyR:::.q_w_pos(net$pos),
    block1_norm1 = cayleyR:::.q_w_norm(blk$norm1),
    block1_attn  = cayleyR:::.q_w_attn(blk),
    block1_norm2 = cayleyR:::.q_w_norm(blk$norm2),
    block1_ff1   = cayleyR:::.q_w_dense(blk$ff1_W, blk$ff1_b),
    block1_ff2   = cayleyR:::.q_w_dense(blk$ff2_W, blk$ff2_b))
  have <- intersect(names(all), names(nodes))
  stats::setNames(all[have], vapply(have, function(n) nodes[[n]]$id, ""))
}

# ag side of the same mini model, as [n, d_model].
mini_ag <- function(net, x, layer) {
  spec <- net$spec; n <- dim(x)[1L]
  consts <- cayleyR:::.q_net_consts(spec, n)
  xa <- ggmlR::ag_tensor(cayleyR:::.q_net_input(x))
  blk <- net$blocks[[1]]
  h <- cayleyR:::.q_lin(net$embed_W, net$embed_b, xa)
  h <- switch(layer,
    embed = h,
    pos   = ggmlR::ag_add(h, ggmlR::ag_matmul(net$pos, consts$tile)),
    norm  = blk$norm1$forward(h),
    attn  = cayleyR:::.q_attn(blk, h, spec, n),
    block = cayleyR:::.q_block(blk, h, spec, n))
  t(as.matrix(ggmlR::ag_matmul(h, consts$pool)))
}

expect_close <- function(a, ref, tol, label) {
  err <- cayleyR:::.q_rel_err(a, ref)
  expect_lt(err[["max"]],  tol, label = paste(label, "relative max"))
  expect_lt(err[["mean"]], tol, label = paste(label, "relative mean"))
}

for (layer in c("embed", "pos", "norm", "attn", "block")) {
  test_that(paste("ag and ggml agree on the CPU:", layer), {
    skip_if_no_q_device()
    old <- ggmlR::ag_device("cpu"); on.exit(ggmlR::ag_device(old), add = TRUE)
    set.seed(31L)
    spec <- spec_small()
    net  <- cayleyR:::.q_net_ag(spec, init = rand_init)
    x    <- rand_x(spec, 4L)

    model <- mini_ggml(spec, layer)
    model$node_weights_data <- mini_weights(net, model)
    pg <- ggmlR::ggml_predict(model, x, batch_size = 4L)
    expect_close(pg, mini_ag(net, x, layer), 1e-5, layer)
  })
}

test_that("ag and ggml agree on the CPU: whole net, and the export reloads", {
  skip_if_no_q_device()
  old <- ggmlR::ag_device("cpu"); on.exit(ggmlR::ag_device(old), add = TRUE)
  set.seed(37L)
  spec <- spec_small()
  net  <- cayleyR:::.q_net_ag(spec, init = rand_init)
  x    <- rand_x(spec, 6L)
  pa   <- cayleyR:::.q_net_predict_ag(net, x)
  expect_equal(dim(pa), c(6L, spec$n_a))

  f <- tempfile(fileext = ".ggml"); on.exit(unlink(f), add = TRUE)
  res <- cayleyR:::.q_net_export(net, f, backend = "cpu", check_x = x, tol = 1e-5)
  expect_lt(res$err[["max"]], 1e-5)

  m2 <- ggmlR::ggml_load_model(f, backend = "cpu")
  expect_close(ggmlR::ggml_predict(m2, x, batch_size = 6L), pa, 1e-5, "reloaded")
})

test_that("the comparison notices heads in the wrong order", {
  # The bug the per-layer checks exist for: an export that hands ggml the
  # attention heads in a different order than ag uses them. Simulated by
  # exporting W_q with its two head blocks swapped and W_k likewise (so each
  # head's scores are still self-consistent -- the subtle case), while the ag
  # net keeps the originals. The difference has to stand far above the
  # tolerance, or the checks above prove nothing.
  skip_if_no_q_device()
  old <- ggmlR::ag_device("cpu"); on.exit(ggmlR::ag_device(old), add = TRUE)
  set.seed(41L)
  spec <- spec_small()
  net  <- cayleyR:::.q_net_ag(spec, init = rand_init)
  x    <- rand_x(spec, 4L)

  hd <- spec$d_model / spec$heads
  swap <- c((hd + 1):(2 * hd), seq_len(hd))
  wrong <- net
  wrong$blocks[[1]]$W_q <- ggmlR::ag_param(as.matrix(net$blocks[[1]]$W_q)[swap, ])
  wrong$blocks[[1]]$W_k <- ggmlR::ag_param(as.matrix(net$blocks[[1]]$W_k)[swap, ])

  model <- cayleyR:::.q_net_ggml(spec, backend = "cpu")
  model$node_weights_data <- cayleyR:::.q_net_weights_data(wrong, model)
  pg  <- ggmlR::ggml_predict(model, x, batch_size = 4L)
  err <- cayleyR:::.q_rel_err(pg, cayleyR:::.q_net_predict_ag(net, x))
  expect_gt(err[["max"]], 1e-3)
})

test_that("ag on Vulkan agrees with the ggml model to f16 accuracy", {
  skip_if_no_q_device()
  skip_if_not(ggmlR::ggml_vulkan_available(), "Vulkan not available")
  old <- ggmlR::ag_device("gpu")
  on.exit(ggmlR::ag_device(old), add = TRUE)
  ggmlR::ag_dtype("f32")
  set.seed(43L)
  spec <- spec_small()
  net  <- cayleyR:::.q_net_ag(spec, init = rand_init)
  x    <- rand_x(spec, 8L)

  f <- tempfile(fileext = ".ggml"); on.exit(unlink(f), add = TRUE)
  res <- cayleyR:::.q_net_export(net, f, backend = "cpu", check_x = x, tol = 2e-3)
  expect_lt(res$err[["max"]], 2e-3)
  expect_lt(res$err[["mean"]], 2e-3)
})
