test_that("human_algorithm sorts a scrambled state", {
  set.seed(42)
  state <- generate_state(14, k = 4, n_moves = 60)

  result <- human_algorithm(state, k = 4)

  expect_true(result$found)
  expect_equal(result$length, length(result$path))

  final <- apply_operations(state, result$path, 4, compute_coords = FALSE)$state
  expect_equal(as.integer(final), 1:14)
})

test_that("human_algorithm returns operations in digit form", {
  set.seed(7)
  state <- generate_state(14, k = 4, n_moves = 40)

  result <- human_algorithm(state, k = 4)

  expect_type(result$path, "character")
  expect_true(all(result$path %in% c("1", "2", "3")))
})

test_that("human_algorithm solves an already sorted state", {
  result <- human_algorithm(1:14, k = 4)

  expect_true(result$found)
  final <- apply_operations(1:14, result$path, 4, compute_coords = FALSE)$state
  expect_equal(as.integer(final), 1:14)
})

test_that("human_algorithm reaches an arbitrary target state", {
  set.seed(11)
  start <- generate_state(14, k = 4, n_moves = 50)
  target <- generate_state(14, k = 4, n_moves = 50)

  result <- human_algorithm(start, target, k = 4)

  expect_true(result$found)
  final <- apply_operations(start, result$path, 4, compute_coords = FALSE)$state
  expect_equal(as.integer(final), as.integer(target))
})

# Ring sizes and flipper widths are spelled out one case per test_that, not
# looped: a case that hangs then names itself in the reporter instead of
# stalling an anonymous iteration.
#
# The finish needs a tail of eight tiles plus room for the block, so 12 is
# about as small as a ring can get here.

.expect_solves <- function(state, k, n) {
  result <- human_algorithm(state, k = k)
  expect_true(result$found)
  final <- apply_operations(state, result$path, k, compute_coords = FALSE)$state
  expect_equal(as.integer(final), seq_len(n))
}

test_that("human_algorithm works on a ring of 12", {
  set.seed(12)
  .expect_solves(generate_state(12, k = 4, n_moves = 60), k = 4, n = 12)
})

test_that("human_algorithm works on a ring of 13", {
  set.seed(13)
  .expect_solves(generate_state(13, k = 4, n_moves = 60), k = 4, n = 13)
})

test_that("human_algorithm works on a ring of 14", {
  set.seed(14)
  .expect_solves(generate_state(14, k = 4, n_moves = 60), k = 4, n = 14)
})

test_that("human_algorithm works on a ring of 15", {
  set.seed(15)
  .expect_solves(generate_state(15, k = 4, n_moves = 60), k = 4, n = 15)
})

test_that("human_algorithm works for flipper width three", {
  set.seed(103)
  .expect_solves(generate_state(14, k = 3, n_moves = 60), k = 3, n = 14)
})

test_that("human_algorithm works for flipper width five", {
  set.seed(105)
  .expect_solves(generate_state(14, k = 5, n_moves = 60), k = 5, n = 14)
})

test_that("human_algorithm works for flipper width six", {
  set.seed(106)
  .expect_solves(generate_state(14, k = 6, n_moves = 60), k = 6, n = 14)
})

test_that("human_algorithm rejects mismatched state lengths", {
  expect_error(human_algorithm(1:14, 1:10, k = 4), "equal length")
})

test_that("human_algorithm rejects rings too small for the tail phase", {
  expect_error(human_algorithm(1:6, k = 4))
})
