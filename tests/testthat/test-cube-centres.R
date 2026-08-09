test_that("the identity has all six centres built", {
  expect_equal(cube_centre_counts(cube_identity(4)), rep(4L, 6))
})

test_that("centre counts fall when the cube is scrambled", {
  set.seed(1)
  s <- generate_state(group = cube_group(4), n_moves = 40L)
  expect_lt(sum(cube_centre_counts(s)), 24L)
})

# The colours of a face's four centre pieces. Read from the state directly
# rather than through the package, so the test can fail when the package is
# wrong -- checking a solver with the counter it is built on proves nothing.
centre_colours <- function(state, face) {
  idx <- face * 16L + c(5L, 6L, 9L, 10L) + 1L
  (state[idx] - 1L) %/% 16L
}

all_centres_built <- function(state) {
  all(vapply(0:5, function(f) length(unique(centre_colours(state, f))) == 1L,
             logical(1)))
}

test_that("cube_solve_centres builds all six centres", {
  set.seed(7)
  n <- 25L
  built <- 0L
  for (i in seq_len(n)) {
    s <- generate_state(group = cube_group(4), n_moves = 60L)
    res <- cube_solve_centres(s)
    if (isTRUE(res$found)) built <- built + 1L
  }
  expect_equal(built, n)
})

test_that("the path really produces the state the solver reports", {
  set.seed(11)
  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)

  for (i in 1:5) {
    s <- generate_state(group = cube_group(4), n_moves = 60L)
    res <- cube_solve_centres(s)
    expect_true(res$found)

    # replay the path move by move, independently of the solver
    cur <- s
    for (m in res$path) cur <- cur[moves[[m]]]

    expect_identical(cur, res$states[[length(res$states)]])
    expect_true(all_centres_built(cur))
  }
})

test_that("a solved cube needs no moves", {
  res <- cube_solve_centres(cube_identity(4))
  expect_true(res$found)
  expect_length(res$path, 0L)
})

test_that("cube_solve_centres rejects states that are not 4x4x4", {
  expect_error(cube_solve_centres(cube_identity(3)), "96")
  expect_error(cube_solve_centres(rep(1L, 96)), "permutation")
})

test_that("the solver leaves the cube a valid permutation", {
  set.seed(3)
  s <- generate_state(group = cube_group(4), n_moves = 60L)
  res <- cube_solve_centres(s)
  final <- res$states[[length(res$states)]]
  expect_setequal(final, 1:96)
})
