# The cube's foundations: the alphabet, and the reading of a state into pieces.
#
# Everything above these -- the predicates, the algorithm tables, the solvers --
# is stated in their terms, so a fault here shows up as nonsense higher up and
# nowhere near its cause. These tests are what makes the difference between
# "the solver failed" and "the solver failed because F is not the front face".

test_that("a quarter turn has order 4, a half turn order 2", {
  for (m in c("U", "R", "F", "D", "L", "B")) {
    expect_equal(cube_word_order(m), 4L, info = m)
    expect_equal(cube_word_order(paste0(m, "2")), 2L, info = m)
  }
})

test_that("a move and its inverse cancel", {
  s <- cube_identity(3)
  for (m in c("U", "R", "F", "D", "L", "B", "M", "E", "S")) {
    expect_identical(cube_apply_word(s, paste(m, paste0(m, "'"))), s, info = m)
  }
})

test_that("face turns never move the centres", {
  centres <- cube_centre_positions()
  expect_equal(centres, c(5L, 14L, 23L, 32L, 41L, 50L))

  set.seed(1)
  g <- cube_group(3)
  faces <- g$moves[!grepl("^[MES]", g$moves)]
  for (i in 1:5) {
    s <- generate_state(group = g, n_moves = 30, moves = faces)
    expect_equal(s[centres], centres)
  }
})

test_that("opposite faces commute, adjacent ones do not", {
  s <- cube_identity(3)
  # U and D turn different layers of the same axis: order does not matter
  expect_identical(cube_apply_word(s, "U D"), cube_apply_word(s, "D U"))
  expect_identical(cube_apply_word(s, "R L"), cube_apply_word(s, "L R"))
  expect_identical(cube_apply_word(s, "F B"), cube_apply_word(s, "B F"))
  # U and R share no axis: order matters
  expect_false(identical(cube_apply_word(s, "U R"), cube_apply_word(s, "R U")))
})

test_that("the sexy move has order 6", {
  # R U R' U' is the first sequence anyone learns; it is famous for coming
  # back to the identity after six repetitions.
  expect_equal(cube_word_order("R U R' U'"), 6L)
})

test_that("a solved cube reads as the identity permutation", {
  c <- cube_read_state(cube_identity(3))
  expect_equal(c$cp, 0:7)
  expect_equal(c$ep, 0:11)
  expect_equal(c$co, rep(0L, 8))
  expect_equal(c$eo, rep(0L, 12))
})

parity <- function(p) {
  n <- length(p)
  seen <- rep(FALSE, n)
  swaps <- 0L
  for (i in seq_len(n)) {
    if (seen[i]) next
    j <- i
    len <- 0L
    while (!seen[j]) { seen[j] <- TRUE; j <- p[j] + 1L; len <- len + 1L }
    swaps <- swaps + len - 1L
  }
  swaps %% 2L
}

test_that("parity() itself is right", {
  expect_equal(parity(0:7), 0L)
  expect_equal(parity(c(1L, 0L, 2:7)), 1L)          # one transposition
  expect_equal(parity(c(1L, 2L, 0L, 3:7)), 0L)      # one 3-cycle
})

test_that("face turns keep the invariants of the cube group", {
  # Three facts hold of every state reachable by turning faces: corner twists
  # sum to a multiple of 3, edge flips sum to an even number, and the corner
  # and edge permutations have the same parity. A reading that got orientation
  # or slot order wrong would break one of them.
  set.seed(42)
  g <- cube_group(3)
  faces <- g$moves[!grepl("^[MES]", g$moves)]

  for (i in 1:20) {
    s <- generate_state(group = g, n_moves = 25, moves = faces)
    c <- cube_read_state(s)

    expect_equal(sum(c$co) %% 3L, 0L, info = paste("corner twist, state", i))
    expect_equal(sum(c$eo) %% 2L, 0L, info = paste("edge flip, state", i))
    expect_equal(parity(c$cp), parity(c$ep), info = paste("parity, state", i))

    expect_setequal(c$cp, 0:7)
    expect_setequal(c$ep, 0:11)
  }
})

test_that("a slice move breaks the parity invariant, and that is correct", {
  # M turns four edges in a cycle and no corners at all, so it changes edge
  # parity while leaving corner parity alone. On a real cube that state is
  # unreachable: it amounts to having turned the centres, and this model holds
  # the centres fixed. The invariant is a statement about the face-turn
  # subgroup, not about every word in the alphabet.
  #
  # This matters to the solvers. Both are written for the cube a person holds,
  # so a scramble handed to them must come from faces only --- see
  # cube_solve_cfop() and cube_solve_lbl().
  c <- cube_read_state(cube_apply_word(cube_identity(3), "M"))
  expect_equal(parity(c$cp), 0L)
  expect_equal(parity(c$ep), 1L)

  # and the centres have indeed moved, which is the same fact seen directly
  s <- cube_apply_word(cube_identity(3), "M")
  expect_false(all(s[cube_centre_positions()] == cube_centre_positions()))
})

test_that("Sune twists exactly three corners and moves nothing else", {
  # R U R' U R U2 R' is the standard three-corner twist. It leaves every piece
  # in its own slot, twists three corners, and leaves the fourth alone --- the
  # sharpest single check that orientation is being read correctly.
  s <- cube_apply_word(cube_identity(3), "R U R' U R U2 R'")
  c <- cube_read_state(s)

  expect_equal(sum(c$co != 0L), 3L)
  expect_equal(sum(c$co) %% 3L, 0L)
  expect_equal(c$eo, rep(0L, 12))
})
