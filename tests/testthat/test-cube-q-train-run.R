# cube_q_train_device() end to end, on the CPU where every step is
# deterministic: what it writes, that a resumed run continues EXACTLY where it
# stopped (keys, weights and Adam's moments), and that init starts from a saved
# model. Tiny sizes; the point is the bookkeeping, not the learning.

skip_if_no_q_device <- function() {
  skip_if_not_installed("ggmlR")
  ok <- tryCatch(cayleyR:::.q_device_require(), error = function(e) FALSE)
  skip_if_not(isTRUE(ok), "ggmlR lacks the on-device Q-network functions")
}

tiny <- function(steps, out, ...) {
  cube_q_train_device(cube_group(3), steps = steps, batch = 16L, depth = 6L,
                      val = 32L, d_model = 16L, heads = 2L, blocks = 1L,
                      ff = 32L, lr = 1e-3, sched = "const", seed = 5L,
                      report = 10L, out = out, device = "cpu",
                      verbose = FALSE, ...)
}

predict_saved <- function(dir, x) {
  m <- ggmlR::ggml_load_model(file.path(dir, "model.ggml"), backend = "cpu")
  ggmlR::ggml_predict(m, x, batch_size = dim(x)[1L])
}

probe_x <- function() {
  g <- cube_group(3); lay <- cube_piece_layout(g)
  set.seed(9L)
  w <- generate_walk(g, n = 8L, n_moves = 6L, exact = FALSE, pivot = TRUE)
  cayleyR:::adi_encode(w$states, "transformer", lay)
}

test_that("a run writes model, meta and Adam state the online script's readers expect", {
  skip_if_no_q_device()
  root <- tempfile("qdev"); on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- tiny(20L, root)

  expect_true(all(file.exists(file.path(res$dir,
                                        c("model.ggml", "meta.rds", "adam.rds")))))
  meta <- readRDS(file.path(res$dir, "meta.rds"))
  expect_identical(meta$trainer, "device")
  expect_identical(meta$kind, "q_online")
  expect_equal(meta$step_done, 20L)
  expect_identical(as.character(meta$moves), cube_group(3)$moves)
  for (k in c("y_mu", "y_sd", "d_model", "heads", "blocks", "ff",
              "pair_accuracy", "top1_accuracy", "mae_prev", "history"))
    expect_false(is.null(meta[[k]]), info = k)
  expect_equal(nrow(res$history), 2L)
  expect_equal(meta$pair_accuracy, res$score$pair_accuracy)

  p <- predict_saved(res$dir, probe_x())
  expect_equal(dim(p), c(8L, 18L))
  expect_true(all(is.finite(p)))
})

test_that("resuming continues exactly where the run stopped", {
  skip_if_no_q_device()
  root <- tempfile("qdev"); on.exit(unlink(root, recursive = TRUE), add = TRUE)

  straight <- tiny(40L, file.path(root, "a"))

  # 20 steps, then the same run told it has 40 and resumed: steps 21..40 must
  # draw the same batches from the same weights and the same Adam moments.
  half <- tiny(20L, file.path(root, "b"))
  mp <- file.path(half$dir, "meta.rds")
  meta <- readRDS(mp); meta$steps <- 40L; saveRDS(meta, mp)
  resumed <- cube_q_train_device(cube_group(3), resume = half$dir,
                                 device = "cpu", verbose = FALSE)

  expect_equal(resumed$score, straight$score, tolerance = 1e-6)
  expect_equal(utils::tail(resumed$history$loss, 1L),
               utils::tail(straight$history$loss, 1L), tolerance = 1e-6)
  x <- probe_x()
  expect_equal(predict_saved(half$dir, x), predict_saved(straight$dir, x),
               tolerance = 1e-5)
  expect_equal(readRDS(mp)$resumed_at, 20L)
})

test_that("init starts a new run from a saved model's weights", {
  skip_if_no_q_device()
  root <- tempfile("qdev"); on.exit(unlink(root, recursive = TRUE), add = TRUE)
  base <- tiny(20L, file.path(root, "base"))

  # Zero steps of training are not allowed, so compare the starting point: a
  # run from init has, before its first update, the saved model's predictions.
  # One step at lr ~ 0 keeps the weights where they started.
  res <- cube_q_train_device(cube_group(3), steps = 1L, batch = 16L, depth = 6L,
                             val = 32L, lr = 1e-12, sched = "const", seed = 5L,
                             report = 1L, init = base$dir,
                             out = file.path(root, "init"), device = "cpu",
                             verbose = FALSE)
  meta <- readRDS(file.path(res$dir, "meta.rds"))
  expect_equal(meta$d_model, 16L)               # architecture from the init
  x <- probe_x()
  expect_equal(predict_saved(res$dir, x), predict_saved(base$dir, x),
               tolerance = 1e-4)
})
