# Tests for both human solvers: human_algorithm(), which routes an arbitrary
# target through the identity state, and human_algorithm_to(), which relabels
# the ring so the target becomes 1:n and solves once.
#
# They live in one file because the C++ core caches its finish table per
# (n, k), and building that table is what the runtime is made of -- a solve
# itself is instant. Split across two files the same (n, k) pairs were built
# twice over, once per file.
#
# For the same reason the ring sizes below are deliberately few. Phase 1 is
# insensitive to n, so sweeping 12/13/14/15 mostly re-pays the table cost;
# what genuinely differs is k (odd k needs different primitives, wider k a
# wider tail) and the parity of n. k = 6 is left out entirely: its table takes
# about 45 seconds to build, against well under a second for every other
# width, and it exercises no path the narrower widths miss.

.expect_solves <- function(state, k, n) {
  result <- human_algorithm(state, k = k)
  expect_true(result$found)
  final <- apply_operations(state, result$path, k, compute_coords = FALSE)$state
  expect_equal(as.integer(final), seq_len(n))
}

.expect_reaches <- function(start, target, k) {
  result <- human_algorithm_to(start, target, k = k)
  expect_true(result$found)
  final <- apply_operations(start, result$path, k, compute_coords = FALSE)$state
  expect_equal(as.integer(final), as.integer(target))
}

# --- human_algorithm ------------------------------------------------------

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

# --- human_algorithm_to ---------------------------------------------------

test_that("human_algorithm_to reaches an arbitrary target state", {
  set.seed(11)
  start <- generate_state(14, k = 4, n_moves = 50)
  target <- generate_state(14, k = 4, n_moves = 50)

  result <- human_algorithm_to(start, target, k = 4)

  expect_true(result$found)
  expect_equal(result$length, length(result$path))

  final <- apply_operations(start, result$path, 4, compute_coords = FALSE)$state
  expect_equal(as.integer(final), as.integer(target))
})

test_that("human_algorithm_to defaults to sorting when no target is given", {
  set.seed(42)
  state <- generate_state(14, k = 4, n_moves = 60)

  result <- human_algorithm_to(state, k = 4)

  expect_true(result$found)
  final <- apply_operations(state, result$path, 4, compute_coords = FALSE)$state
  expect_equal(as.integer(final), 1:14)
})

test_that("human_algorithm_to returns operations in digit form", {
  set.seed(7)
  start <- generate_state(14, k = 4, n_moves = 40)
  target <- generate_state(14, k = 4, n_moves = 40)

  result <- human_algorithm_to(start, target, k = 4)

  expect_type(result$path, "character")
  expect_true(all(result$path %in% c("1", "2", "3")))
})

test_that("human_algorithm_to returns an empty path when start equals target", {
  set.seed(21)
  state <- generate_state(14, k = 4, n_moves = 50)

  result <- human_algorithm_to(state, state, k = 4)

  expect_true(result$found)
  expect_equal(result$length, 0L)
})

test_that("human_algorithm_to travels from the sorted state to a target", {
  set.seed(22)
  target <- generate_state(14, k = 4, n_moves = 50)

  .expect_reaches(1:14, target, k = 4)
})

# Solving one scramble instead of two is the whole point of the relabelling,
# so the word should come out shorter than the concatenate-through-identity
# route -- on the same pair of states, or the comparison means nothing.

test_that("human_algorithm_to beats routing through the identity state", {
  set.seed(23)
  start <- generate_state(14, k = 4, n_moves = 60)
  target <- generate_state(14, k = 4, n_moves = 60)

  direct <- human_algorithm_to(start, target, k = 4)
  through_identity <- human_algorithm(start, target, k = 4)

  expect_true(direct$found)
  expect_true(through_identity$found)
  expect_lt(direct$length, through_identity$length)
})

# --- ring sizes and flipper widths ----------------------------------------

# Both solvers share the C++ core, so each (n, k) below is covered once, by
# whichever of the two is more natural to state it with.

test_that("the solvers work on a ring of 12", {
  set.seed(12)
  .expect_solves(generate_state(12, k = 4, n_moves = 60), k = 4, n = 12)
  .expect_reaches(generate_state(12, k = 4, n_moves = 60),
                  generate_state(12, k = 4, n_moves = 60), k = 4)
})

test_that("the solvers work on a ring of 13", {
  set.seed(13)
  .expect_solves(generate_state(13, k = 4, n_moves = 60), k = 4, n = 13)
  .expect_reaches(generate_state(13, k = 4, n_moves = 60),
                  generate_state(13, k = 4, n_moves = 60), k = 4)
})

test_that("the solvers work for flipper width three", {
  set.seed(103)
  .expect_solves(generate_state(14, k = 3, n_moves = 60), k = 3, n = 14)
  .expect_reaches(generate_state(14, k = 3, n_moves = 60),
                  generate_state(14, k = 3, n_moves = 60), k = 3)
})

test_that("the solvers work for flipper width five", {
  set.seed(105)
  .expect_solves(generate_state(14, k = 5, n_moves = 60), k = 5, n = 14)
  .expect_reaches(generate_state(14, k = 5, n_moves = 60),
                  generate_state(14, k = 5, n_moves = 60), k = 5)
})

# --- argument checking ----------------------------------------------------

test_that("human_algorithm rejects mismatched state lengths", {
  expect_error(human_algorithm(1:14, 1:10, k = 4), "equal length")
})

test_that("human_algorithm rejects rings too small for the tail phase", {
  expect_error(human_algorithm(1:6, k = 4))
})

test_that("human_algorithm_to rejects mismatched state lengths", {
  expect_error(human_algorithm_to(1:14, 1:10, k = 4), "equal length")
})

test_that("human_algorithm_to rejects states that are not permutations", {
  expect_error(human_algorithm_to(c(1:13, 13L), 1:14, k = 4), "permutations")
  expect_error(human_algorithm_to(1:14, c(1:13, 13L), k = 4), "permutations")
})
