# Read the cube directly, not through the package -- a solver checked with the
# counter it is built on proves nothing.
face_is_one_colour <- function(state, face) {
  idx <- face * 16L + c(5L, 6L, 9L, 10L) + 1L
  length(unique((state[idx] - 1L) %/% 16L)) == 1L
}

test_that("a solved cube needs no moves", {
  res <- cube_solve4(cube_identity(4))
  expect_true(res$found)
  expect_length(res$path, 0L)
})

test_that("cube_solve4 solves the cube outright", {
  set.seed(4)
  n <- 10L
  solved <- 0L
  for (i in seq_len(n)) {
    s <- generate_state(group = cube_group(4), n_moves = 60L)
    res <- cube_solve4(s)
    if (isTRUE(res$found)) solved <- solved + 1L
  }
  expect_equal(solved, n)
})

test_that("the path really solves the cube when replayed", {
  set.seed(9)
  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)

  for (i in 1:4) {
    s <- generate_state(group = cube_group(4), n_moves = 60L)
    res <- cube_solve4(s)
    expect_true(res$found)

    cur <- s
    for (m in res$path) cur <- cur[moves[[m]]]

    expect_true(cube_is_colour_solved(cur))
    expect_setequal(cur, 1:96)
  }
})

test_that("reduction leaves every face one colour before the 3x3x3 stage", {
  set.seed(13)
  s <- generate_state(group = cube_group(4), n_moves = 60L)
  res <- cube_solve4(s)
  expect_true(res$found)

  # the state after the reduction stage, whatever else follows
  i <- which(res$stages$name == "reduction")[1]
  after <- res$states[[i]]
  expect_true(all(vapply(0:5, function(f) face_is_one_colour(after, f),
                         logical(1))))
})

test_that("the squeeze carries a turn to the matching 3x3x3 turn", {
  # The reduction is only sound if squeezing commutes with turning. Checked on
  # every face, since one wrong face would show up as a solver that almost
  # works.
  id4 <- cube_identity(4)
  id3 <- cube_identity(3)
  m4 <- cube_moves(4); names(m4) <- cube_move_names(4)
  m3 <- cube_moves(3); names(m3) <- cube_move_names(3)

  for (f in c("U", "R", "F", "D", "L", "B")) {
    lhs <- cayleyR:::cube_squeeze_cpp(id4[m4[[f]]])
    rhs <- (id3[m3[[f]]] - 1L) %/% 9L
    expect_identical(as.integer(lhs), as.integer(rhs))
  }
})

test_that("lifting a 3x3x3 slice takes both inner layers", {
  # A single inner layer looks right to the squeeze -- it cannot tell one half
  # of an edge from the other -- but it tears the pairing apart. Measured: 1y
  # alone drops a reduced cube from twelve pairs to eight.
  expect_identical(cayleyR:::cube_lift_path_cpp("E")$path,
                   c("1y'", "2y'"))
  expect_identical(cayleyR:::cube_lift_path_cpp("M")$path,
                   c("1x'", "2x'"))
  expect_identical(cayleyR:::cube_lift_path_cpp("S")$path,
                   c("1z", "2z"))
})

test_that("the parity algorithms leave the reduction intact", {
  # Both are applied to an already reduced cube, so neither may spend a centre
  # or a pair. The OLL word changes orientation only; the PLL word does the
  # same once r is read as the inner slice rather than the wide turn.
  id <- cube_identity(4)
  for (kind in c("OLL", "PLL")) {
    out <- as.integer(cayleyR:::cube_parity_fix_cpp(id, kind)$state)
    expect_true(all(vapply(0:5, function(f) face_is_one_colour(out, f),
                           logical(1))))
  }
})
