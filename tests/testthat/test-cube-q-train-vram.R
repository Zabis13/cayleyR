# Regression: score() in cube_q_train_device() must not grow the pass pool.
#
# score() runs eager forwards between captured steps. Nothing outside a tape
# frees the pass pool, so before ggmlR::ag_pass_scope() every call kept its
# activations on the device: ~2.7 GB of VRAM per call on the full-size cube3
# run, until the driver refused command submission (step ~2000) and the
# desktop went down. Here the pass pool is compared between a run that scores
# once and a run that scores at every step; the extra calls must leave
# nothing behind.

skip_if_no_q_vulkan <- function() {
  skip_if_not_installed("ggmlR")
  ok <- tryCatch(cayleyR:::.q_device_require(), error = function(e) FALSE)
  skip_if_not(isTRUE(ok), "ggmlR lacks the on-device Q-network functions")
  skip_if_not(ggmlR::ggml_vulkan_available() &&
                ggmlR::ggml_vulkan_device_count() >= 1L, "no Vulkan device")
}

pass_bytes <- function() ggmlR:::.ag_tape_mem()$buffer_bytes

# val = 64 with batch 16: four chunks per score() call.
tiny_gpu <- function(report) {
  cube_q_train_device(cube_group(3), steps = 10L, batch = 16L, depth = 6L,
                      val = 64L, d_model = 16L, heads = 2L, blocks = 1L,
                      ff = 32L, lr = 1e-3, sched = "const", seed = 5L,
                      report = report, out = NULL, device = "gpu",
                      verbose = FALSE)
}

test_that("repeated score() calls leave the pass pool as they found it (Vulkan)", {
  skip_if_no_q_vulkan()
  old <- ggmlR::ag_device("gpu")
  on.exit(ggmlR::ag_device(old), add = TRUE)

  b0 <- pass_bytes()
  tiny_gpu(report = 10L)              # score() at step 10 and at the end
  once <- pass_bytes() - b0

  b1 <- pass_bytes()
  tiny_gpu(report = 1L)               # score() at every step and at the end
  every <- pass_bytes() - b1

  expect_identical(every, once)
})
