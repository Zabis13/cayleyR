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

test_that("start_face: all three settings solve, and full is never worse", {
  set.seed(31)
  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)

  # One scramble, not three: "full" solves from all six faces, so a single
  # cube already costs six solves and a second one settles nothing a third
  # would not. The measurement that says "full" is shorter on average lives in
  # the documentation, where it was made over 40 scrambles.
  for (i in 1:1) {
    s <- generate_state(group = cube_group(4), n_moves = 12L)

    res <- lapply(c("fixed", "reduction", "full"),
                  function(sf) cube_solve4(s, start_face = sf))
    names(res) <- c("fixed", "reduction", "full")

    for (nm in names(res)) {
      expect_true(res[[nm]]$found)
      cur <- s
      for (m in res[[nm]]$path) cur <- cur[moves[[m]]]
      expect_true(cube_is_colour_solved(cur))
    }

    # "full" takes the shortest of six solves and the fixed face is one of the
    # six, so it cannot come out longer. "reduction" optimises the stage rather
    # than the answer and carries no such guarantee -- which is the reason
    # "full" is the default.
    expect_lte(length(res$full$path), length(res$fixed$path))
  }
})

test_that("start_face rejects a name it does not know", {
  expect_error(cube_solve4(cube_identity(4), start_face = "cheapest"))
})

test_that("cube_solve4 solves the cube, and the path replays to the solve", {
  # This was two tests -- one counting solves, one replaying the path -- on the
  # same scrambles. Replaying is the stronger of the two and answers
  # both, so the scrambles are solved once rather than twice.
  set.seed(9)
  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)

  for (i in 1:3) {
    s <- generate_state(group = cube_group(4), n_moves = 12L)
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
  s <- generate_state(group = cube_group(4), n_moves = 12L)
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

test_that("reduction is a question that can be asked at any size", {
  # A solved cube is reduced whatever its size.
  for (n in 2:7) expect_true(cube_is_reduced(cube_identity(n)))

  # An outer face turn moves whole pieces and so keeps the cube reduced; an
  # inner slice cuts through them and does not.
  for (n in c(4, 5, 6)) {
    m <- cube_moves(n)
    names(m) <- cube_move_names(n)
    s <- cube_identity(n)
    expect_true(cube_is_reduced(s[m[["U"]]]))
    inner <- grep("^[0-9]+x$", names(m), value = TRUE)[1]
    expect_false(cube_is_reduced(s[m[[inner]]]))
  }
})

test_that("outer turns alone can never break reduction", {
  # The property the 3x3x3 stage depends on: once reduced, a cube stays
  # reduced under the moves a 3x3x3 method uses.
  set.seed(42)
  outer <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
  for (n in c(4, 5, 6)) {
    g <- cube_group(n)
    id <- group_identity(g)
    for (trial in 1:15) {
      s <- group_apply(g, id, sample(outer, 12, replace = TRUE))
      expect_true(cube_is_reduced(s))
    }
  }
})

test_that("a 3x3x3 is reduced in every state", {
  # There is nothing to reduce: its centres are single and its edges have no
  # wings. Saying TRUE is the right answer rather than a degenerate one.
  set.seed(3)
  g <- cube_group(3)
  for (trial in 1:20) {
    s <- group_apply(g, group_identity(g),
                     sample(cube_move_names(3), 15, replace = TRUE))
    expect_true(cube_is_reduced(s))
  }
})

test_that("cube_is_reduced infers n and rejects a length that is no cube", {
  g <- cube_group(4)
  set.seed(7)
  s <- group_apply(g, group_identity(g),
                   sample(cube_move_names(4), 10, replace = TRUE))
  expect_identical(cube_is_reduced(s), cube_is_reduced(s, n = 4))
  expect_error(cube_is_reduced(s, n = 5), "stickers")
  expect_error(cube_is_reduced(1:50), "no cube")
})

test_that("every method returns a result, and the path it returns solves", {
  # cube_kociemba returns the word itself, a character vector, where the other
  # four return a list with $path and $found. Reading $found off a character
  # vector is an error, not FALSE, so the kociemba method used to fail on every
  # cube -- and the failure looked like a solver bug rather than a shape
  # mismatch. Each method is called here so that a return shape changing again
  # is caught at once.
  #
  # One fixed cube for all five, two moves deep and not reduced -- an inner
  # slice is one of the two -- so every method still walks its reduction stage.
  # That they solve hard cubes is the business of the tests above; what is
  # asked here is the shape of the answer and that the path replays.
  #
  # Fixed rather than drawn per method because the kociemba search costs
  # anywhere from 0.03s to 4s depending which cube it lands on, with no
  # relation to how deep the scramble was, and a test of the return shape
  # should not pay that lottery five times over.
  g <- cube_group(4)
  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)
  s <- group_apply(g, group_identity(g), c("1x", "R"))

  for (m in c("cfop", "kociemba", "lbl", "m2", "pochmann")) {
    r <- cube_solve4(s, method = m)
    expect_type(r, "list")
    expect_true(all(c("path", "found", "failure") %in% names(r)))
    expect_type(r$found, "logical")

    # A method that says it solved the cube has to have solved the cube.
    if (isTRUE(r$found)) {
      cur <- s
      for (mv in r$path) cur <- cur[moves[[mv]]]
      expect_true(cube_is_colour_solved(cur),
                  info = paste(m, "-- path does not solve the cube"))
    }
  }
})
