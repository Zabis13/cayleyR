## Wide turns and rotations are a vocabulary on top of the alphabet, not an
## addition to it. These tests hold that line: what the names expand to, that
## the permutation is the expansion and not a second derivation of it, and
## that the alphabet itself is unchanged.

test_that("the expansion agrees with the table the solvers already use", {
  # src/cube_algs.h has expanded 3x3x3 notation since before these functions
  # existed. It was written by hand; this one comes from the geometry. If they
  # disagree, one is wrong -- and the old one is the one already trusted.
  ref <- list(
    x = "R M' L'",  `x'` = "R' M L",   x2 = "R2 M2 L2",
    y = "U E' D'",  `y'` = "U' E D",   y2 = "U2 E2 D2",
    z = "F S B'",   `z'` = "F' S' B",  z2 = "F2 S2 B2",
    r = "R M'",     `r'` = "R' M",     r2 = "R2 M2",
    l = "L M",      `l'` = "L' M'",    l2 = "L2 M2",
    u = "U E'",     `u'` = "U' E",     u2 = "U2 E2",
    d = "D E",      `d'` = "D' E'",    d2 = "D2 E2",
    f = "F S",      `f'` = "F' S'",    f2 = "F2 S2",
    b = "B S'",     `b'` = "B' S",     b2 = "B2 S2"
  )
  g <- cube_group(3)
  for (nm in names(ref)) {
    # compared by what they do: disjoint layers commute, so the spelling may
    # differ while the permutation may not
    expect_identical(group_compose(g, cube_expand_move(nm, 3L)),
                     group_compose(g, cube_expand_word(ref[[nm]], 3L)),
                     info = nm)
  }
})

test_that("a wide turn is the face plus the layers behind it", {
  for (n in 3:6) {
    g <- cube_group(n)

    expect_identical(cube_expand_move("R", n), "R")
    expect_length(cube_expand_move("Rw", n), 2L)
    expect_length(cube_expand_move("3Rw", n), 3L)

    # Rw is R together with the next layer in
    expect_identical(group_compose(g, cube_expand_move("Rw", n)),
                     group_compose(g, c("R", cube_expand_move("2R", n))))

    # and kRw is the first k layers, so it grows one layer at a time
    for (k in 2:min(n, 4L)) {
      wide <- cube_expand_move(paste0(k, "Rw"), n)
      expect_length(wide, k)
      expect_identical(group_compose(g, wide),
                       group_compose(g, c(cube_expand_move(paste0(k - 1L, "Rw"), n),
                                          cube_expand_move(paste0(k, "R"), n))))
    }
  }
})

test_that("a single numbered layer is one move, not several", {
  for (n in 4:6) {
    for (k in seq_len(n)) {
      one <- cube_expand_move(paste0(k, "R"), n)
      expect_length(one, 1L)
      expect_true(one %in% cube_move_names(n))
    }
    # 1R is the face itself
    expect_identical(group_compose(cube_group(n), cube_expand_move("1R", n)),
                     group_compose(cube_group(n), "R"))
  }
})

test_that("a whole-cube rotation turns every layer and keeps the cube solved", {
  for (n in 2:6) {
    g <- cube_group(n)
    s <- cube_identity(n)
    for (rot in c("x", "y", "z")) {
      w <- cube_expand_move(rot, n)
      expect_length(w, n)

      # a solved cube stays solved when turned bodily, though its stickers move
      moved <- group_apply(g, s, w)
      expect_true(cube_is_colour_solved(moved))
      if (n > 2L) expect_false(identical(moved, s))

      # and four of them is the identity
      expect_identical(group_apply(g, s, rep(w, 4L)), s)
    }
    # turning n layers wide is turning the whole cube
    expect_identical(group_compose(g, cube_expand_move(paste0(n, "Rw"), n)),
                     group_compose(g, cube_expand_move("x", n)))
  }
})

test_that("the permutation is the composition of the expansion", {
  # The point of the design: cube_wide_move does not derive anything of its
  # own, so it cannot drift away from what the words say.
  for (n in c(2L, 3L, 4L, 5L)) {
    g <- cube_group(n)
    names_to_try <- c("R", "Rw", "x", "y'", "z2", "Lw", "Uw'")
    if (n >= 3L) names_to_try <- c(names_to_try, "3Rw", "2R")
    for (nm in names_to_try) {
      expect_identical(cube_wide_move(nm, n),
                       group_compose(g, cube_expand_move(nm, n)), info = nm)
    }
  }
})

test_that("primes invert and 2 doubles", {
  for (n in c(3L, 4L, 5L)) {
    g <- cube_group(n)
    s <- cube_identity(n)
    for (nm in c("R", "Rw", "x", "y", "z", "Lw", "Dw")) {
      fwd <- cube_expand_move(nm, n)
      inv <- cube_expand_move(paste0(nm, "'"), n)
      expect_identical(group_apply(g, group_apply(g, s, fwd), inv), s, info = nm)
      expect_identical(group_apply(g, s, cube_expand_move(paste0(nm, "2"), n)),
                       group_apply(g, s, c(fwd, fwd)), info = nm)
    }
  }
})

test_that("lower case is the older spelling of a wide turn", {
  for (n in c(3L, 4L)) {
    g <- cube_group(n)
    for (pair in list(c("r", "Rw"), c("l", "Lw"), c("u", "Uw"),
                      c("d", "Dw"), c("f", "Fw"), c("b", "Bw"))) {
      expect_identical(group_compose(g, cube_expand_move(pair[1L], n)),
                       group_compose(g, cube_expand_move(pair[2L], n)))
    }
  }
})

test_that("a whole word expands, in either spelling", {
  n <- 4L
  a <- cube_expand_word("Rw U Rw'", n)
  b <- cube_expand_word(c("Rw", "U", "Rw'"), n)
  expect_identical(a, b)
  expect_identical(cube_wide_word("Rw U Rw'", n),
                   group_compose(cube_group(n), a))

  # a word of ordinary moves passes through unchanged
  expect_identical(cube_expand_word("R U R' U'", 3L), c("R", "U", "R'", "U'"))
})

test_that("the alphabet itself is untouched", {
  # The whole reason for keeping wide turns outside the generating set: the
  # metric, and every number measured in it, must not move.
  for (n in 2:6) {
    expect_length(cube_move_names(n), 6L * n)
    expect_false(any(grepl("w", cube_move_names(n))))
    expect_false(any(c("x", "y", "z") %in% cube_move_names(n)))
  }
})

test_that("slices are refused where they have no meaning", {
  expect_identical(cube_expand_move("M", 3L), "M")
  expect_error(cube_expand_move("M", 4L), "3x3x3 slice")
  expect_error(cube_expand_move("E", 5L), "3x3x3 slice")
})

test_that("bad names are refused", {
  expect_error(cube_expand_move("Q", 4L), "not a move name")
  expect_error(cube_expand_move("5R", 4L), "asks for layer 5")
  expect_error(cube_expand_move("9Rw", 4L), "asks for layer 9")
  expect_error(cube_expand_move(c("R", "U"), 4L), "one name at a time")
  expect_error(cube_expand_move("R", 1L), "at least 2")
})

test_that("expansions stay inside the alphabet", {
  for (n in 2:6) {
    alphabet <- cube_move_names(n)
    for (nm in c("R", "Rw", "x", "y'", "z2", "Uw", "Lw'")) {
      expect_true(all(cube_expand_move(nm, n) %in% alphabet), info = nm)
    }
  }
})
