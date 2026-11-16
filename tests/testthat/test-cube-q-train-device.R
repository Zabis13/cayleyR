# The recorded training step: batch, forward, loss, backward and Adam on the
# device, its only input the key (step counter, seed) from the host.
#
# The loss a step returns is the loss of the batch drawn with its key, under
# the weights before its update. That is recomputed here
# on the host from the same key and compared, for two consecutive steps -- the
# second one checks that the key moved on AND that the update happened (its
# reference forward uses the weights the first step left).

skip_if_no_q_device <- function() {
  skip_if_not_installed("ggmlR")
  ok <- tryCatch(cayleyR:::.q_device_require(), error = function(e) FALSE)
  skip_if_not(isTRUE(ok), "ggmlR lacks the on-device Q-network functions")
}

small_setup <- function(batch = 16L, depth = 6L, d = 16L, blocks = 1L) {
  g   <- cube_group(3)
  lay <- cube_piece_layout(g)
  tab <- cayleyR:::.q_walk_tables(g, lay, depth, k_min = 2L, no_cancel = TRUE)
  spec <- cayleyR:::.q_net_spec(lay$n_piece, tab$depth_dim, tab$n_a,
                                d_model = d, heads = 2L, blocks = blocks,
                                ff = 2L * d)
  list(tab = tab, spec = spec, dev = cayleyR:::.q_walk_device(tab, batch))
}

# The loss the step should report for `key`, under the net's current weights.
# On its OWN copies of the constants: running the step's constants eagerly
# before the first step would give them pass-pool handles, and the recording
# would then have to upload them (refused). The weights are shared -- they are
# resident from creation and are what the reference has to see.
host_loss <- function(st, key) {
  dev    <- cayleyR:::.q_walk_device(st$dev$tab, st$dev$batch)
  consts <- cayleyR:::.q_net_consts(st$net$spec, st$dev$batch)
  b <- cayleyR:::.q_walk_batch(dev, key)
  pred <- as.matrix(cayleyR:::.q_net_forward(st$net, b$x, consts))
  mask <- as.matrix(b$mask); y <- as.matrix(b$y)
  sum((pred * mask - y)^2) / length(y)
}

check_two_steps <- function(st, seed, tol) {
  ref1 <- host_loss(st, c(0, seed))
  l1   <- cayleyR:::.q_train_step(st)
  expect_equal(l1, ref1, tolerance = tol)
  expect_equal(cayleyR:::.q_train_key(st), c(1, seed))

  ref2 <- host_loss(st, c(1, seed))
  l2   <- cayleyR:::.q_train_step(st)
  expect_equal(l2, ref2, tolerance = tol)
  expect_equal(cayleyR:::.q_train_key(st), c(2, seed))
}

test_that("a step trains on the batch its key draws, and moves the key on (CPU)", {
  skip_if_no_q_device()
  old <- ggmlR::ag_device("cpu"); on.exit(ggmlR::ag_device(old), add = TRUE)
  set.seed(51L)
  s  <- small_setup()
  st <- cayleyR:::.q_train_state(cayleyR:::.q_net_ag(s$spec), s$dev,
                                 lr = 1e-3, seed = 3)
  check_two_steps(st, 3, 1e-6)
})

test_that("the step is recorded on Vulkan and agrees with the host", {
  skip_if_no_q_device()
  skip_if_not(ggmlR::ggml_vulkan_available(), "Vulkan not available")
  old <- ggmlR::ag_device("gpu"); on.exit(ggmlR::ag_device(old), add = TRUE)
  ggmlR::ag_dtype("f32")
  set.seed(53L)
  s  <- small_setup()
  st <- cayleyR:::.q_train_state(cayleyR:::.q_net_ag(s$spec), s$dev,
                                 lr = 1e-3, seed = 7)
  check_two_steps(st, 7, 2e-3)
  # Replayed from a recording, not run op by op.
  expect_false(is.null(attr(st$step, "captures")$step))
})

test_that("a few hundred recorded steps bring the loss down (Vulkan)", {
  skip_if_no_q_device()
  skip_if_not(ggmlR::ggml_vulkan_available(), "Vulkan not available")
  skip_on_cran()
  old <- ggmlR::ag_device("gpu"); on.exit(ggmlR::ag_device(old), add = TRUE)
  ggmlR::ag_dtype("f32")
  set.seed(57L)
  s  <- small_setup(batch = 128L, depth = 8L, d = 32L, blocks = 2L)
  st <- cayleyR:::.q_train_state(cayleyR:::.q_net_ag(s$spec), s$dev,
                                 lr = 1e-3, seed = 11)
  losses <- vapply(seq_len(300L), function(i) cayleyR:::.q_train_step(st), 0)
  expect_true(all(is.finite(losses)))
  expect_lt(mean(utils::tail(losses, 50L)), 0.7 * mean(utils::head(losses, 50L)))
})
