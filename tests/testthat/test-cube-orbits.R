test_that("orbits partition the pieces, and pieces the stickers", {
  for (n in 2:7) {
    o <- cube_orbits(n)
    p <- cube_pieces(n)

    # every piece belongs to exactly one orbit, and the orbit sizes add up
    expect_equal(sum(o$n_pieces), nrow(p))
    expect_equal(as.vector(table(p$orbit)), o$n_pieces)

    # every sticker is carried by exactly one piece
    all_st <- unlist(strsplit(p$stickers, ","))
    expect_equal(length(all_st), 6 * n * n)
    expect_equal(length(unique(all_st)), 6 * n * n)
    expect_setequal(as.integer(all_st), seq_len(6 * n * n))

    # a piece carries one sticker per face it touches
    expect_equal(p$n_stickers, lengths(strsplit(p$stickers, ",")))
  }
})

test_that("a cube of any size has eight corners", {
  for (n in 2:8) {
    o <- cube_orbits(n)
    expect_equal(sum(o$n_pieces[o$kind == "corner"]), 8)
  }
})

test_that("orbit sizes are 24 apart from the pieces on a symmetry axis", {
  # The exceptions are the 8 corners, and on an odd cube the 6 fixed centres
  # and 12 middle edges.
  for (n in 4:8) {
    o <- cube_orbits(n)
    odd <- n %% 2 == 1
    for (i in seq_len(nrow(o))) {
      expected <- if (o$kind[i] == "corner") {
        8L
      } else if (odd && o$kind[i] == "centre" &&
                 o$depth_a[i] == (n - 1) / 2 && o$depth_b[i] == (n - 1) / 2) {
        6L
      } else if (odd && o$kind[i] == "edge" && o$depth_b[i] == (n - 1) / 2) {
        12L
      } else {
        24L
      }
      expect_equal(o$n_pieces[i], expected,
                   info = paste0("n = ", n, ", orbit ", o$label[i]))
    }
  }
})

test_that("the 3x3x3 comes out as corners, edges and fixed centres", {
  o <- cube_orbits(3)
  expect_equal(nrow(o), 3L)
  expect_equal(o$n_pieces[o$kind == "corner"], 8L)
  expect_equal(o$n_pieces[o$kind == "edge"], 12L)
  expect_equal(o$n_pieces[o$kind == "centre"], 6L)
  expect_equal(nrow(cube_pieces(3)), 26L)
})

test_that("mirror-pair orbits are labelled, and only where they are pairs", {
  # A 6x6x6 splits f1:0,1,2 into two mirror orbits; a 5x5x5 does not, because
  # its faces have a true centre to carry a piece round through.
  o6 <- cube_orbits(6)
  pair <- o6[o6$depth_a == 1 & o6$depth_b == 2 & o6$kind == "centre", ]
  expect_equal(nrow(pair), 2L)
  expect_setequal(pair$chirality, c(-1L, 1L))
  expect_setequal(pair$label, c("f1:0,1,2-", "f1:0,1,2+"))

  o5 <- cube_orbits(5)
  single <- o5[o5$depth_a == 1 & o5$depth_b == 2 & o5$kind == "centre", ]
  expect_equal(nrow(single), 1L)
  expect_equal(single$chirality, 0L)
  expect_equal(single$label, "f1:0,1,2")

  # chirality is non-zero only when the label carries a sign
  for (n in 2:8) {
    o <- cube_orbits(n)
    expect_equal(o$chirality != 0, grepl("[+-]$", o$label))
  }
})

test_that("no move takes a piece out of its orbit", {
  # The defining property. Applying any single move must leave each orbit's
  # membership unchanged, which shows up as the solved count being invariant
  # when the whole orbit is home.
  for (n in c(3L, 4L, 5L)) {
    g <- cube_group(n)
    id <- cube_identity(n)
    orb <- cube_orbits(n)
    for (mv in cube_move_names(n)) {
      s <- group_apply(g, id, mv)
      pr <- cube_progress(s)
      expect_equal(pr$label, orb$label)
      expect_equal(pr$total, orb$n_pieces)
    }
  }
})

test_that("progress is total on a solved cube and read from the state's size", {
  for (n in 2:6) {
    pr <- cube_progress(cube_identity(n))
    expect_true(all(pr$solved == pr$total))
    expect_true(all(pr$fraction == 1))

    h <- cube_pieces_home(cube_identity(n))
    expect_equal(h[["home"]], h[["total"]])
    expect_equal(h[["total"]], nrow(cube_pieces(n)))
  }
})

test_that("a scramble leaves fewer pieces home", {
  set.seed(42)
  for (n in c(3L, 4L, 5L)) {
    s <- generate_state(group = cube_group(n), n_moves = 40L)
    h <- cube_pieces_home(s)
    expect_lt(h[["home"]], h[["total"]])
    expect_equal(sum(cube_progress(s)$solved), h[["home"]])
  }
})

test_that("a cube solved relative to its centres counts as solved", {
  # Slice turns move the centres, so a cube turned bodily in space has its
  # stickers elsewhere while every piece is still home.
  g <- cube_group(3)
  s <- group_apply(g, cube_identity(3), c("R", "M'", "L'"))
  expect_false(identical(s, cube_identity(3)))
  expect_true(cube_is_colour_solved(s))
  h <- cube_pieces_home(s)
  expect_equal(h[["home"]], h[["total"]])
})

test_that("a state that is not a cube is refused", {
  expect_error(cube_progress(1:50), "not a cube of any size")
  expect_error(cube_pieces_home(1:100), "not a cube of any size")
  expect_error(cube_orbits(1), "at least 2")
})
