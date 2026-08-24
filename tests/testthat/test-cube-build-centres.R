test_that("a build returns the moves that produced the state", {
  # The one thing a solver must never get wrong: the word it hands back has to
  # take the cube it was given to the cube it claims. Short scrambles, because
  # a full build costs about ten seconds and what is being checked here is the
  # bookkeeping, not the method.
  set.seed(3)
  for (n in c(4, 5)) {
    g <- cube_group(n)
    id <- group_identity(g)
    s <- group_apply(g, id, sample(cube_move_names(n), 5, replace = TRUE))

    lay <- cube_build_lslice(s)
    expect_equal(group_apply(g, s, lay$path), lay$state)
    expect_lte(lay$count, lay$target)

    down <- group_apply(g, lay$state, cube_expand_word("z'", n))
    sh <- cube_empty_u_slice(down, n)
    expect_equal(group_apply(g, down, sh$path), sh$state)
  }
})

test_that("a solved cube is already built and needs no moves", {
  for (n in c(2, 3, 4, 5, 6)) {
    lay <- cube_build_lslice(cube_identity(n))
    expect_true(lay$built)
    expect_equal(lay$count, lay$target)
    expect_equal(lay$target, nrow(cube_lslice_cells(n)))
    expect_length(lay$path, 0)
    expect_equal(lay$rounds, 0L)

    sh <- cube_empty_u_slice(cube_identity(n))
    expect_equal(sh$shots, 0L)
    expect_length(sh$path, 0)
  }
})

test_that("the shots leave the built bottom one colour", {
  # The whole point of the stage: what has been built and turned down survives
  # it. Checked by COLOUR rather than by sticker index -- on an even cube the
  # centres of a face are indistinguishable, so a shot may permute them within
  # the face without disturbing anything that matters.
  set.seed(2)
  for (n in c(4, 5)) {
    g <- cube_group(n)
    cs <- cube_centre_structure(n)
    per <- table(cs$orbit)
    fixed <- if (n %% 2L == 1L) as.integer(names(per)[per == 6L])[1] else NA
    d_sk <- cs$sticker[cs$face == 3L & (is.na(fixed) | cs$orbit != fixed)]
    colours <- function(st) sort((st[d_sk] - 1L) %/% (n * n))

    s <- group_apply(g, group_identity(g),
                     sample(cube_move_names(n), 5, replace = TRUE))
    lay <- cube_build_lslice(s, n)
    down <- group_apply(g, lay$state, cube_expand_word("z'", n))

    r <- cube_empty_u_slice(down, n)
    expect_equal(colours(r$state), colours(down))
  }
})

test_that("an odd cube reads its frame off the fixed centres", {
  # A three-cycle turns the cube to reach its triple and turns it back, and
  # while it is turned "the colour that belongs on face f" is not f. On an odd
  # cube the fixed centre of each face says what belongs there whatever the
  # rotation, so a bodily turned cube is still built and needs no moves.
  #
  # An even cube has no such sticker and cannot tell: with nothing to read the
  # frame from, the face keeps its own number, and a rotated cube genuinely
  # looks unbuilt. That is a real limit rather than a bug, and stating it here
  # keeps it from being mistaken for one later.
  g <- cube_group(5)
  for (rot in c("x", "y", "z")) {
    turned <- group_apply(g, cube_identity(5), cube_expand_word(rot, 5))
    r <- cube_build_lslice(turned)
    expect_true(r$built)
    expect_length(r$path, 0)
  }

  g4 <- cube_group(4)
  turned4 <- group_apply(g4, cube_identity(4), cube_expand_word("x", 4))
  r4 <- cube_build_lslice(turned4)
  expect_false(r4$built && length(r4$path) == 0)
})

test_that("max_rounds bounds the cycles", {
  # Only that the cap is obeyed and the path still holds. Whether the ladder
  # alone finishes is a fact about the method, measured properly and recorded
  # in the documentation; re-measuring it here would cost ten seconds a build
  # for something a single scramble cannot settle anyway.
  set.seed(1)
  g <- cube_group(5)
  s <- group_apply(g, group_identity(g),
                   sample(cube_move_names(5), 8, replace = TRUE))

  r <- cube_build_lslice(s, max_rounds = 0L)
  expect_equal(r$rounds, 0L)
  expect_lte(r$count, r$target)
  expect_equal(group_apply(g, s, r$path), r$state)

  sh <- cube_empty_u_slice(s, max_shots = 3L)
  expect_lte(sh$shots, 3L)
})

test_that("both stages infer n and reject a bad length", {
  s <- cube_identity(5)
  expect_equal(cube_build_lslice(s)$target, cube_build_lslice(s, n = 5)$target)
  expect_error(cube_build_lslice(s, n = 4), "stickers")
  expect_error(cube_build_lslice(1:50), "no cube")

  expect_equal(cube_empty_u_slice(s)$target, cube_empty_u_slice(s, n = 5)$target)
  expect_error(cube_empty_u_slice(s, n = 4), "stickers")
  expect_error(cube_empty_u_slice(1:50), "no cube")
})
