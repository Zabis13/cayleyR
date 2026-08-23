test_that("centre structure covers exactly the centre stickers", {
  for (n in 2:7) {
    cs <- cube_centre_structure(n)
    o <- cube_orbits(n)

    # one row per centre piece, and centre pieces carry one sticker each
    expect_equal(nrow(cs), sum(o$n_pieces[o$kind == "centre"]))
    expect_equal(length(unique(cs$sticker)), nrow(cs))
    expect_true(all(cs$sticker >= 1 & cs$sticker <= 6 * n * n))

    # faces and locals are consistent with the sticker index
    expect_equal(cs$face, (cs$sticker - 1L) %/% (n * n))
    expect_equal(cs$local, (cs$sticker - 1L) %% (n * n))
    expect_true(all(cs$face %in% 0:5))

    # every orbit named is a centre orbit
    expect_true(all(cs$orbit %in% o$orbit[o$kind == "centre"]))
  }
})

test_that("slots number each face-and-orbit group from one, with no gaps", {
  for (n in 2:7) {
    cs <- cube_centre_structure(n)
    expect_false(anyNA(cs$slot))

    groups <- split(cs$slot, list(cs$face, cs$orbit), drop = TRUE)
    for (g in groups) expect_equal(sort(g), seq_along(g))

    # an orbit contributes the same number of stickers to every face, so each
    # of its groups is the orbit split six ways
    for (ob in unique(cs$orbit)) {
      per_face <- table(cs$face[cs$orbit == ob])
      expect_equal(length(per_face), 6L)
      expect_equal(length(unique(as.integer(per_face))), 1L)
    }
  }
})

test_that("a face turn steps every slot of its own face by one", {
  # What numbering along the turn's own cycle buys: on the face being turned,
  # a quarter turn is a constant step in slot number, so a setup turn can be
  # computed rather than searched for.
  #
  # The step is minus one rather than plus one, and that is a convention
  # meeting a convention. Slots are numbered by cube_layer_move with turns = 1,
  # which turns clockwise seen from the positive end of the axis; the named
  # move U turns clockwise seen from U itself. For the three faces at the
  # negative end of their axis those are opposite senses. What matters here is
  # that the step is the same for every slot, which is what makes the
  # arithmetic work.
  for (n in c(4, 5, 6)) {
    cs <- cube_centre_structure(n)
    for (face_move in c("U", "R", "F", "D", "L", "B")) {
      m <- cube_slice_map(n, face_move, cs)
      moved <- m[m$from_face == m$to_face, , drop = FALSE]
      if (!nrow(moved)) next

      # only the turned face has stickers staying on their own face
      expect_equal(length(unique(moved$from_face)), 1L)

      len <- ave(moved$from_slot, moved$from_orbit, FUN = length)
      step <- (moved$to_slot - moved$from_slot) %% len
      # one step, the same one, for every slot of every orbit on that face
      expect_equal(length(unique(step[len > 1])), 1L)
    }
  }
})

test_that("no move mixes two centre orbits", {
  # The premise of solving orbit by orbit. If it ever failed, a method that
  # finished one orbit could not trust it to stay finished.
  for (n in c(4, 5, 6, 7)) {
    cs <- cube_centre_structure(n)
    for (mv in cube_move_names(n)) {
      m <- cube_slice_map(n, mv, cs)
      if (nrow(m)) expect_true(all(m$from_orbit == m$to_orbit))
    }
  }
})

test_that("the generated slice table reproduces the hand table of cube_centres.h", {
  # The six blocks of slice_maps() in src/cube_centres.h, measured by hand for
  # a 4x4x4. Each row is from_face, from_slot, to_face, to_slot.
  #
  # That file numbers a face's four slots by reading order -- locals 5, 6, 9,
  # 10 are slots 1 to 4 -- where cube_centre_structure numbers them along the
  # face-turn cycle. The two conventions differ, so the comparison converts,
  # and what is being checked is the geometry rather than the numbering.
  hand <- list(
    "1x" = c(0,1,5,4, 0,3,5,2, 2,1,0,1, 2,3,0,3,
             3,1,2,1, 3,3,2,3, 5,2,3,3, 5,4,3,1),
    "2x" = c(0,2,5,3, 0,4,5,1, 2,2,0,2, 2,4,0,4,
             3,2,2,2, 3,4,2,4, 5,1,3,4, 5,3,3,2),
    "1z" = c(0,1,1,2, 0,2,1,4, 1,2,3,4, 1,4,3,3,
             3,3,4,1, 3,4,4,3, 4,1,0,2, 4,3,0,1),
    "2z" = c(0,3,1,1, 0,4,1,3, 1,1,3,2, 1,3,3,1,
             3,1,4,2, 3,2,4,4, 4,2,0,4, 4,4,0,3),
    "1y" = c(1,3,2,3, 1,4,2,4, 2,3,4,3, 2,4,4,4,
             4,3,5,3, 4,4,5,4, 5,3,1,3, 5,4,1,4),
    "2y" = c(1,1,2,1, 1,2,2,2, 2,1,4,1, 2,2,4,2,
             4,1,5,1, 4,2,5,2, 5,1,1,1, 5,2,1,2)
  )

  cs <- cube_centre_structure(4)
  # the hand file's slot for a sticker: its rank among locals 5, 6, 9, 10
  hand_slot <- match(cs$local, c(5L, 6L, 9L, 10L))
  expect_false(anyNA(hand_slot))

  key <- function(a, b, c, d) sort(paste(a, b, c, d, sep = ","))

  for (mv in names(hand)) {
    m <- cube_slice_map(4, mv, cs)
    from_h <- hand_slot[match(paste(m$from_face, m$from_slot),
                              paste(cs$face, cs$slot))]
    to_h <- hand_slot[match(paste(m$to_face, m$to_slot),
                            paste(cs$face, cs$slot))]

    got <- key(m$from_face, from_h, m$to_face, to_h)
    want <- matrix(hand[[mv]], ncol = 4, byrow = TRUE)
    expect_equal(nrow(m), nrow(want))
    expect_equal(got, key(want[, 1], want[, 2], want[, 3], want[, 4]))
  }
})

test_that("the structure of the centres follows from the size", {
  # What the sizes come out as, so a change in the derivation is noticed.
  sizes <- function(n) {
    cs <- cube_centre_structure(n)
    as.integer(sort(table(cs$orbit), decreasing = TRUE))
  }
  expect_equal(sizes(3), 6L)                      # fixed centres only
  expect_equal(sizes(4), 24L)                     # one moving orbit
  expect_equal(sizes(5), c(24L, 24L, 6L))         # two orbits and the fixed six
  expect_equal(sizes(6), rep(24L, 4))
  expect_equal(sizes(7), c(rep(24L, 6), 6L))
})

test_that("a slice map is empty when the move leaves the centres alone", {
  # A 3x3x3 face turn moves no centre: the one centre of a face is its axis.
  for (mv in c("U", "R", "F", "D", "L", "B")) {
    expect_equal(nrow(cube_slice_map(3, mv)), 0)
  }
  # and the slices of a 3x3x3 do move them
  expect_gt(nrow(cube_slice_map(3, "M")), 0)
})

test_that("cube_slice_map rejects a move the cube does not have", {
  expect_error(cube_slice_map(4, "M"), "no move")
})

test_that("central moves are exactly the ones that disturb the fixed centres", {
  # An odd cube has a middle layer per axis and an even one does not, so this
  # is a fact about parity -- but it is found by asking the permutations, and
  # the test states the expected answer so a change in the derivation shows up.
  expect_equal(cube_central_moves(4), character(0))
  expect_equal(cube_central_moves(6), character(0))
  expect_setequal(cube_central_moves(5),
                  c("2x", "2x'", "2y", "2y'", "2z", "2z'"))
  expect_setequal(cube_central_moves(7),
                  c("3x", "3x'", "3y", "3y'", "3z", "3z'"))

  # and on an odd cube they are the only moves that touch the fixed six
  for (n in c(5, 7)) {
    cs <- cube_centre_structure(n)
    per <- table(cs$orbit)
    fixed <- cs$sticker[cs$orbit == as.integer(names(per)[per == 6L])]
    central <- cube_central_moves(n)
    moves <- cube_moves(n)
    for (mv in cube_move_names(n)) {
      disturbs <- any(moves[[mv]][fixed] != fixed)
      expect_equal(disturbs, mv %in% central)
    }
  }
})

test_that("the generated shots are the eight of cube_centres5.h", {
  # The hand-written table of src/cube_centres5.h: eight conjugations, two per
  # side face, each leaving the D centres alone. Reproduced exactly, words and
  # destinations both.
  hand <- data.frame(
    word = c("1x U 1x'", "3x U 3x'", "1x' U 1x", "3x' U 3x",
             "1z U 1z'", "3z U 3z'", "1z' U 1z", "3z' U 3z"),
    to_face = c(2L, 2L, 5L, 5L, 4L, 4L, 1L, 1L),
    stringsAsFactors = FALSE
  )

  got <- cube_centre_shots(5)
  expect_setequal(got$word, hand$word)
  expect_equal(got$to_face[match(hand$word, got$word)], hand$to_face)

  # each moves both of the moving orbits together, and not the fixed one
  expect_true(all(got$orbits == "4,5"))
  expect_true(all(got$n_moved == 8))
})

test_that("a shot never disturbs the face it is meant to spare", {
  # The invariant the method rests on: fire any number of shots in any order,
  # with any turns of the working face between, and the kept face is untouched.
  set.seed(4)
  for (n in c(4, 5, 6)) {
    cs <- cube_centre_structure(n)
    shots <- cube_centre_shots(n)
    keep_face <- 3L                      # D, the default of cube_centre_shots
    keep <- cs$sticker[cs$face == keep_face]

    g <- cube_group(n)
    id <- group_identity(g)

    # each shot on its own
    for (w in shots$word) {
      s <- group_apply(g, id, strsplit(w, " ", fixed = TRUE)[[1]])
      expect_equal(s[keep], keep)
    }

    # and long random sequences of them
    for (trial in 1:20) {
      word <- character(0)
      for (k in 1:8) {
        word <- c(word, strsplit(sample(shots$word, 1), " ", fixed = TRUE)[[1]])
        turns <- sample(0:3, 1)
        if (turns) word <- c(word, rep("U", turns))
      }
      s <- group_apply(g, id, word)
      expect_equal(s[keep], keep)
    }
  }
})

test_that("shots reach every side face", {
  # Two per side face on the smaller cubes, four on the larger -- but what the
  # method needs is only that no side face is unreachable.
  for (n in c(4, 5, 6, 7)) {
    s <- cube_centre_shots(n)
    expect_setequal(unique(s$to_face), c(1L, 2L, 4L, 5L))
  }
})

test_that("no shot uses a central move", {
  # Turning the middle layer of an odd cube turns the cube, which would carry
  # the kept face away with it.
  for (n in c(5, 7)) {
    central <- cube_central_moves(n)
    for (w in cube_centre_shots(n)$word) {
      expect_false(any(strsplit(w, " ", fixed = TRUE)[[1]] %in% central))
    }
  }
})

test_that("a size with no moving centres has no shots", {
  # A 3x3x3 centre is the axis of its face and cannot be carried anywhere.
  expect_equal(nrow(cube_centre_shots(3)), 0)
  expect_equal(nrow(cube_centre_shots(2)), 0)
})

test_that("the l-slice matches the hand-measured cells of cube_centres.h", {
  # That file measures the layer a wide L turn carries and writes it out:
  #
  #   L   all four slots
  #   U   slots 1 and 3        F   slots 1 and 3
  #   D   slots 1 and 3        B   slots 2 and 4
  #
  # Twelve cells. The slot numbers are in that file's reading-order convention
  # rather than this one, so what is compared is the shape: which faces take
  # part, and how many centres each contributes.
  cells <- cube_lslice_cells(4)
  expect_equal(nrow(cells), 12L)

  per_face <- table(cells$face)
  expect_equal(as.integer(per_face[["4"]]), 4L)          # L, all of it
  for (f in c("0", "2", "3", "5"))                        # U, F, D, B
    expect_equal(as.integer(per_face[[f]]), 2L)
  expect_false("1" %in% names(per_face))                  # R takes no part
})

test_that("the free moves match the ones cube_centres.h names", {
  # "Genuinely free are R, R', (Rr), (Rr)' and 2x" -- in this package's
  # alphabet, R, R' and the inner layer 2x with its inverse.
  expect_setequal(cube_free_moves(4), c("R", "R'", "2x", "2x'"))
})

test_that("a free move really does leave the whole l-slice standing", {
  # The property the name claims, checked rather than trusted, at every size.
  for (n in c(4, 5, 6)) {
    cells <- cube_lslice_cells(n)$sticker
    g <- cube_group(n)
    id <- group_identity(g)
    free <- cube_free_moves(n)
    expect_gt(length(free), 0)
    for (mv in free) expect_equal(group_apply(g, id, mv)[cells], cells)

    # and every move not called free disturbs it, so the set is exact
    for (mv in setdiff(cube_move_names(n), free)) {
      expect_false(all(group_apply(g, id, mv)[cells] == cells))
    }
  }
})

test_that("the l-slice grows with the size the way the geometry says", {
  # One face entire plus a column of each of four side faces: (n-2)^2 centres
  # on L and (n-2) on each of U, F, D and B.
  #
  # On an odd cube L's own fixed centre sits on the turn's axis and so does not
  # move with it. Leaving it out is right rather than a miss: a piece that no
  # move can shift is built already, and the layer is the set that has to be
  # put there. The side faces lose nothing --- their columns run across the
  # axis rather than through it, so all n-2 of each take part at every size.
  for (n in c(4, 5, 6, 7)) {
    cells <- cube_lslice_cells(n)
    inner <- (n - 2L)
    on_l <- inner * inner - as.integer(n %% 2L == 1L)

    expect_equal(nrow(cells), on_l + 4L * inner)
    per_face <- table(cells$face)
    expect_equal(as.integer(per_face[["4"]]), on_l)
    for (f in c("0", "2", "3", "5"))
      expect_equal(as.integer(per_face[[f]]), inner)
  }
})

test_that("the D-sparing shots are no use once the l-slice is built", {
  # Why the layer stage needs its own vocabulary: every shot that spares D
  # breaks the l-slice, which is what makes cube_free_moves necessary rather
  # than a convenience.
  n <- 4
  cells <- cube_lslice_cells(n)$sticker
  g <- cube_group(n)
  id <- group_identity(g)
  for (w in cube_centre_shots(n)$word) {
    s <- group_apply(g, id, strsplit(w, " ", fixed = TRUE)[[1]])
    expect_false(all(s[cells] == cells))
  }
})

test_that("the three-cycle reproduces the one in cube_centres.h", {
  # That file carries a single hand-written commutator for a 4x4x4:
  #   1x U 2x' U' 1x' U 2x U'   cycling a centre between faces 0, 2 and 5.
  # The generic search finds it, word for word.
  cyc <- cube_centre_cycles(4)
  expect_true("1x U 2x' U' 1x' U 2x U'" %in% cyc$word)
  expect_equal(cyc$faces[cyc$word == "1x U 2x' U' 1x' U 2x U'"], "0,2,5")
})

test_that("a three-cycle moves exactly three centres, and nothing else", {
  # The property that makes it usable: placing one piece must not disturb the
  # ones already placed.
  for (n in c(4, 5, 6, 7)) {
    cyc <- cube_centre_cycles(n)
    if (!nrow(cyc)) next
    cs <- cube_centre_structure(n)
    g <- cube_group(n)
    id <- group_identity(g)

    for (i in seq_len(nrow(cyc))) {
      s <- group_apply(g, id, strsplit(cyc$word[i], " ", fixed = TRUE)[[1]])
      moved <- cs[s[cs$sticker] != cs$sticker, , drop = FALSE]
      expect_equal(nrow(moved), 3L)
      expect_equal(length(unique(moved$orbit)), 1L)
      expect_equal(unique(moved$orbit), cyc$orbit[i])

      # three distinct faces, and it really is a cycle rather than a swap
      expect_equal(length(unique(moved$face)), 3L)
      dest <- vapply(moved$sticker, function(sk) which(s == sk), integer(1))
      expect_setequal(dest, moved$sticker)
    }
  }
})

test_that("applying a three-cycle three times is the identity", {
  # What "three-cycle" means, checked rather than assumed.
  for (n in c(4, 5, 6)) {
    cyc <- cube_centre_cycles(n)
    if (!nrow(cyc)) next
    g <- cube_group(n)
    id <- group_identity(g)
    for (w in cyc$word) {
      mv <- strsplit(w, " ", fixed = TRUE)[[1]]
      s <- group_apply(g, id, rep(mv, 3))
      cs <- cube_centre_structure(n)
      expect_equal(s[cs$sticker], cs$sticker)
    }
  }
})

test_that("every moving centre orbit gets three-cycles, on every face triple", {
  # A method has to cycle whichever three faces the cube presents, and has to
  # reach every orbit -- leaving one out means the pieces of that orbit cannot
  # be placed at all, however good the rest of the method is.
  #
  # The 5x5x5 is the case that caught this: its plus centres are reached only
  # through the central slice, and while that was excluded from the search the
  # orbit had no tool and builds stalled on it.
  for (n in c(4, 5, 6, 7)) {
    cs <- cube_centre_structure(n)
    per <- table(cs$orbit)
    moving <- as.integer(names(per)[per != 6L])
    cyc <- cube_centre_cycles(n)

    expect_setequal(unique(cyc$orbit), moving)
    # all twenty triples of six faces, for each orbit
    for (ob in moving)
      expect_equal(length(unique(cyc$faces[cyc$orbit == ob])), 20L)
    # one kept per (orbit, triple) by default
    expect_equal(nrow(cyc), length(moving) * 20L)
  }
})

test_that("a three-cycle may use a central move, and some must", {
  # This test used to assert the opposite, on the reasoning that a central
  # move turns the whole cube. That holds for the move alone; a commutator
  # built from it puts the frame back, and the check that matters is the one
  # below -- the fixed centres end where they started. Asserting the reasoning
  # instead of the measurement cost the 5x5x5 its plus-centre orbit.
  for (n in c(5, 7)) {
    cs <- cube_centre_structure(n)
    per <- table(cs$orbit)
    fixed <- cs$sticker[cs$orbit == as.integer(names(per)[per == 6L])]
    g <- cube_group(n)
    id <- group_identity(g)
    for (w in cube_centre_cycles(n)$word) {
      s <- group_apply(g, id, strsplit(w, " ", fixed = TRUE)[[1]])
      expect_equal(s[fixed], fixed)
    }
  }
})

test_that("the central slice is part of the three-cycle search", {
  # A 5x5x5's plus centres are cycled by 1x U 2x' U' 1x' U 2x U' and by no
  # other word of this shape -- one character different from the corner
  # centres' own word, and that character is the central slice 2x.
  #
  # Excluding central moves from the search on the grounds that they turn the
  # whole cube is the mistake this guards. It is true of the move alone and
  # false of a commutator built from it: measured on a 5x5x5, 1644 of 2592
  # candidate words leave the fixed centres exactly where they stood.
  cyc <- cube_centre_cycles(5)
  central <- cube_central_moves(5)

  uses_central <- vapply(cyc$word, function(w)
    any(strsplit(w, " ", fixed = TRUE)[[1]] %in% central), logical(1))
  expect_true(any(uses_central))

  # and specifically: the plus-centre orbit is reached only that way
  cs <- cube_centre_structure(5)
  per <- table(cs$orbit)
  moving <- as.integer(names(per)[per != 6L])
  plus <- max(moving)
  expect_true(all(uses_central[cyc$orbit == plus]))
})

test_that("a three-cycle never disturbs the fixed centres", {
  # A word that appears to cycle the fixed six has turned the cube instead,
  # which is not a three-cycle of anything and must not be returned.
  for (n in c(5, 7)) {
    cs <- cube_centre_structure(n)
    per <- table(cs$orbit)
    fixed <- cs$sticker[cs$orbit == as.integer(names(per)[per == 6L])]
    g <- cube_group(n)
    id <- group_identity(g)
    for (w in cube_centre_cycles(n)$word) {
      s <- group_apply(g, id, strsplit(w, " ", fixed = TRUE)[[1]])
      expect_equal(s[fixed], fixed)
    }
  }
})

test_that("cube_centre_cycles(all = TRUE) keeps every commutator found", {
  a <- cube_centre_cycles(5)
  b <- cube_centre_cycles(5, all = TRUE)
  expect_gt(nrow(b), nrow(a))
  expect_true(all(a$word %in% b$word))
  # the default is one per (orbit, faces) pair
  expect_equal(nrow(a), nrow(unique(a[, c("orbit", "faces")])))
})
