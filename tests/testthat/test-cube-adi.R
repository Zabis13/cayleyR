# The C++ side of autodidactic iteration. None of this needs ggmlR: the
# networks are what ADI trains, but the scrambling, the child expansion and the
# target rule are all decided before a network is involved, and they are what a
# wrong answer would come from most quietly.

test_that("scrambles are uniform over depth and never the identity", {
  set.seed(42)
  g  <- cube_group(4)
  sc <- cayleyR:::cube_adi_scramble(g$ptr, 2000L, 12L)

  expect_equal(dim(sc$states), c(2000L, 96L))
  expect_equal(range(sc$depth), c(1L, 12L))

  # Uniform in depth is what lets accuracy spread outward from the goal without
  # any weighting in the loss, so it is worth asserting rather than assuming.
  counts <- table(sc$depth)
  expect_length(counts, 12L)
  expect_gt(min(counts) / max(counts), 0.6)

  # Depth >= 1 with no immediate undo means no scramble lands back on solved.
  identity <- seq_len(96L)
  is_id <- apply(sc$states, 1L, function(r) identical(as.integer(r), identity))
  expect_false(any(is_id))
})

test_that("children match cube_moves order exactly", {
  set.seed(1)
  g  <- cube_group(4)
  m  <- cube_moves(4)
  sc <- cayleyR:::cube_adi_scramble(g$ptr, 5L, 6L)
  ch <- cayleyR:::cube_adi_children(g$ptr, sc$states)

  expect_equal(nrow(ch$children), 5L * 24L)

  # Row (i-1)*24 + a must be child a of state i, in the move order R sees.
  # Everything downstream reads the targets back by this arithmetic, so a
  # transposed layout here would train the policy on the wrong labels while
  # every loss curve still looked healthy.
  for (i in 1:5) {
    for (a in 1:24) {
      expect_identical(as.integer(ch$children[(i - 1L) * 24L + a, ]),
                       as.integer(sc$states[i, ][m[[a]]]))
    }
  }
})

test_that("a state one move from solved has exactly one solved child", {
  set.seed(7)
  g  <- cube_group(4)
  sc <- cayleyR:::cube_adi_scramble(g$ptr, 200L, 1L)
  ch <- cayleyR:::cube_adi_children(g$ptr, sc$states)

  per_state <- tapply(ch$solved, rep(seq_len(200L), each = 24L), sum)
  expect_true(all(per_state == 1L))
})

test_that("targets ignore what the network says about a solved child", {
  set.seed(9)
  g  <- cube_group(4)
  sc <- cayleyR:::cube_adi_scramble(g$ptr, 100L, 1L)
  ch <- cayleyR:::cube_adi_children(g$ptr, sc$states)

  # A network that is confidently wrong everywhere. Depth-1 states must still
  # come out at exactly 1: this anchoring is the whole reason ADI converges
  # instead of drifting on its own guesses.
  liar <- runif(nrow(ch$children), 5, 9)
  tg   <- cayleyR:::cube_adi_targets(liar, ch$solved, 24L)

  expect_true(all(tg$value == 1))
  solved_child <- vapply(seq_len(100L),
                         function(i) ch$solved[(i - 1L) * 24L + tg$policy[i]],
                         logical(1))
  expect_true(all(solved_child))
})

test_that("targets take the minimum over children", {
  # Two states, four moves, no solved children: the rule is 1 + min(v).
  values <- c(3, 1, 4, 2,
              9, 8, 6, 7)
  tg <- cayleyR:::cube_adi_targets(values, rep(FALSE, 8L), 4L)

  expect_equal(tg$value, c(2, 7))
  expect_equal(tg$policy, c(2L, 3L))
})

test_that("adi helpers reject mismatched input", {
  g <- cube_group(4)
  expect_error(cayleyR:::cube_adi_scramble(g$ptr, 10L, 0L), "max_depth")
  expect_error(cayleyR:::cube_adi_scramble(g$ptr, 0L, 5L), "n must be positive")
  expect_error(cayleyR:::cube_adi_children(g$ptr, matrix(1L, 2L, 20L)),
               "columns")
  expect_error(cayleyR:::cube_adi_targets(c(1, 2, 3), rep(FALSE, 3L), 2L),
               "multiple")
})

test_that("scrambling works for cubes of other sizes", {
  # Nothing in the C++ is 4x4x4-specific; it goes through PermGroup.
  set.seed(5)
  g3 <- cube_group(3)
  sc <- cayleyR:::cube_adi_scramble(g3$ptr, 50L, 5L)
  expect_equal(ncol(sc$states), 54L)

  ch <- cayleyR:::cube_adi_children(g3$ptr, sc$states)
  expect_equal(nrow(ch$children), 50L * length(g3$moves))
})
