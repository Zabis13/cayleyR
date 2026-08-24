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

test_that("centre counts work at any size", {
  # A solved cube has every centre home, and how many that is per face follows
  # from the size rather than being written down.
  for (n in 2:7) {
    cs <- cube_centre_structure(n)
    per_face <- if (nrow(cs)) sum(cs$face == 0L) else 0L
    expect_equal(cube_centre_counts(cube_identity(n)), rep(per_face, 6))
  }
  expect_equal(cube_centre_counts(cube_identity(3)), rep(1L, 6))
  expect_equal(cube_centre_counts(cube_identity(4)), rep(4L, 6))
  expect_equal(cube_centre_counts(cube_identity(5)), rep(9L, 6))
})

test_that("the generic count agrees with the 4x4x4 C++ it replaces", {
  # The 4x4x4 keeps its own path, so this checks the two do not drift.
  set.seed(11)
  g <- cube_group(4)
  for (trial in 1:20) {
    s <- group_apply(g, group_identity(g),
                     sample(cube_move_names(4), 30, replace = TRUE))
    cs <- cube_centre_structure(4)
    home <- (s[cs$sticker] - 1L) %/% 16L == cs$face
    generic <- vapply(0:5, function(f) sum(home[cs$face == f]), integer(1))
    expect_equal(cube_centre_counts(s), generic)
  }
})

test_that("counting by orbit splits the centres a size actually has", {
  # A 5x5x5 solves its two moving orbits and leaves the fixed six alone, so a
  # single total would hide which of the three is finished.
  by <- cube_centre_counts(cube_identity(5), by_orbit = TRUE)
  expect_equal(nrow(by), 3 * 6)
  expect_true(all(by$home == by$of))
  expect_setequal(unique(by$of), c(4L, 1L))

  # the totals agree with the flat count
  flat <- cube_centre_counts(cube_identity(5))
  per_face <- tapply(by$home, by$face, sum)
  expect_equal(as.integer(per_face), flat)

  # and on a scramble too
  set.seed(5)
  g <- cube_group(5)
  s <- group_apply(g, group_identity(g),
                   sample(cube_move_names(5), 40, replace = TRUE))
  by_s <- cube_centre_counts(s, by_orbit = TRUE)
  expect_equal(as.integer(tapply(by_s$home, by_s$face, sum)),
               cube_centre_counts(s))
})

test_that("centre counts infer n, and reject a length that is no cube", {
  expect_equal(cube_centre_counts(cube_identity(5), n = 5),
               cube_centre_counts(cube_identity(5)))
  expect_error(cube_centre_counts(1:50), "no cube")
  expect_error(cube_centre_counts(cube_identity(5), n = 4), "stickers")
})

# ---- reducing from every face --------------------------------------------

test_that("every starting face reduces the cube", {
  # The pipeline used to build its first centre on L and nowhere else; the
  # other five faces had no layer table and could not get past step 2. This is
  # what says they can now -- and it replays each path rather than believing
  # what the solver reports.
  set.seed(3)
  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)

  for (i in 1:3) {
    s <- generate_state(group = cube_group(4), n_moves = 12L)
    for (f in 0:5) {
      res <- cube_reduce_cpp(s, f)
      expect_true(res$found)

      cur <- s
      for (m in res$path) cur <- cur[moves[[m]]]
      expect_true(cube_is_reduced(cur, 4L))
    }
  }
})

test_that("cube_reduce_best picks the shortest verified face", {
  set.seed(19)
  s <- generate_state(group = cube_group(4), n_moves = 12L)
  res <- cube_reduce_best(s)

  expect_true(res$found)
  expect_true(res$face %in% 0:5)
  expect_equal(nrow(res$tried), 6L)

  # the reported best really is the shortest of the verified ones
  ok <- res$tried$verified
  expect_true(any(ok))
  expect_equal(length(res$path), min(res$tried$n_moves[ok]))

  # and it is no worse than the old fixed default
  expect_lte(length(res$path), length(cube_reduce_cpp(s)$path))

  # the path it returns reduces the cube when replayed
  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)
  cur <- s
  for (m in res$path) cur <- cur[moves[[m]]]
  expect_true(cube_is_reduced(cur, 4L))
})

test_that("cube_reduce_best honours a shorter list of faces", {
  set.seed(23)
  s <- generate_state(group = cube_group(4), n_moves = 10L)
  res <- cube_reduce_best(s, faces = c(4L, 0L))

  expect_equal(nrow(res$tried), 2L)
  expect_equal(sort(res$tried$face), c(0L, 4L))
  expect_true(res$face %in% c(0L, 4L))

  expect_error(cube_reduce_best(s, faces = 7L), "0 to 5")
  expect_error(cube_reduce_best(s, faces = integer(0)), "0 to 5")
})
