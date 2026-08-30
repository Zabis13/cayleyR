# How a cube state reaches the network. The encoding is a claim about the cube
# -- that these three stickers are one corner, that a slot names the piece in it
# -- and a wrong claim here does not crash: it trains a slightly worse network
# and looks like bad luck. So the claims are checked directly.

test_that("the layout drops exactly the pieces no move disturbs", {
  faces <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")

  # Face turns leave the six centres where they are, so DeepCubeA's 20 pieces
  # are what is left: 8 corners and 12 edges.
  lay <- cube_piece_layout(cube_group(3, moves = faces))
  expect_equal(lay$n_piece, 20L)
  expect_equal(lay$width, 3L)
  expect_equal(sum(rowSums(lay$slots > 0L) == 3L), 8L)   # corners
  expect_equal(sum(rowSums(lay$slots > 0L) == 2L), 12L)  # edges

  # Add a slice and the centres it turns start carrying information. M turns
  # four of the six, leaving the two on its own axis fixed, so the layout keeps
  # 8 + 12 + 4 = 24 pieces -- not all 26. Which centres a slice moves is a fact
  # about the cube that the layout measures; counting them from the move names
  # is how one gets 26 here and is wrong.
  lay_m <- cube_piece_layout(cube_group(3, moves = c(faces, "M", "M'")))
  expect_equal(lay_m$n_piece, 24L)

  # All three slices between them move every centre, and then nothing is
  # dropped.
  lay_all <- cube_piece_layout(cube_group(3))
  expect_equal(lay_all$n_piece, 26L)
})

test_that("the layout agrees with cube_pieces", {
  g   <- cube_group(3, moves = c("U", "U'", "R", "R'", "F", "F'",
                                 "D", "D'", "L", "L'", "B", "B'"))
  lay <- cube_piece_layout(g)

  # Every sticker of a kept piece is claimed by that piece and no other.
  kept <- as.integer(lay$slots[lay$slots > 0L])
  expect_equal(anyDuplicated(kept), 0L)
  expect_length(kept, 8L * 3L + 12L * 2L)

  # home/turn are the inverse of slots, so the round trip has to close. Read
  # the slots out piece by piece -- rows, in order -- and compare the whole
  # round trip in one go rather than two expectations a piece.
  n_st <- rowSums(lay$slots > 0L)
  by_row <- unlist(lapply(seq_len(lay$n_piece),
                          function(p) lay$slots[p, seq_len(n_st[p])]))
  expect_equal(lay$home[by_row], rep(seq_len(lay$n_piece), n_st))
  expect_equal(lay$turn[by_row], unlist(lapply(n_st, seq_len)))
})

test_that("a slot's stickers always come from one piece", {
  # This is what lets the encoder read the piece off the slot's first sticker
  # alone. If a move could split a piece across two slots the encoding would be
  # quietly wrong, so it is checked on scrambled cubes rather than argued.
  faces <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
  g     <- cube_group(3, moves = faces)
  lay   <- cube_piece_layout(g)
  tbl   <- cube_moves(3)

  set.seed(7)
  # One expectation per scramble rather than one per piece: the check is the
  # same, and 20 expect_ calls a scramble cost more than the cube does.
  slots <- lapply(seq_len(lay$n_piece), function(p) {
    slot <- lay$slots[p, ]
    slot[slot > 0L]
  })

  s <- group_identity(g)
  for (i in 1:50) {
    s <- s[tbl[[sample(faces, 1L)]]]
    one_piece <- vapply(slots,
                        function(slot) length(unique(lay$home[s[slot]])) == 1L,
                        logical(1))
    expect_true(all(one_piece))
  }
})

test_that("the piece encoding is one-hot and loses nothing", {
  faces <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
  g     <- cube_group(3, moves = faces)
  lay   <- cube_piece_layout(g)
  tbl   <- cube_moves(3)

  set.seed(11)
  states <- t(vapply(1:50, function(i) {
    s <- group_identity(g)
    for (m in sample(faces, 15L, replace = TRUE)) s <- s[tbl[[m]]]
    s
  }, integer(54L)))

  # Flat, not [states, pieces, bits]: the network input is declared flat
  # because a 2-d one runs some twenty-five times slower per state.
  enc <- cayleyR:::adi_encode_pieces(states, lay)
  expect_equal(dim(enc), c(50L, 20L * 20L * 3L))
  expect_true(all(enc %in% c(0, 1)))

  # Exactly one bit per piece slot: the piece in it, turned the way it is.
  # The slots are still there in the flat layout, 60 columns apart.
  per_slot <- array(enc, dim = c(50L, 20L, 20L * 3L))
  expect_true(all(apply(per_slot, c(1L, 2L), sum) == 1))

  # Distinct cubes must encode distinctly, or the network is being asked to
  # tell apart things it cannot see. Face turns fix the centres, so dropping
  # them costs nothing here -- which is the whole argument for dropping them.
  expect_equal(anyDuplicated(asplit(enc, 1L)), 0L)
})

test_that("the C++ encoder matches the obvious R one", {
  # The encoder was moved to C++ for speed, and a speed change that alters the
  # answer is the kind that shows up as a slightly worse network months later.
  # So the plain R version it replaced is written out here and compared.
  faces <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
  g     <- cube_group(3, moves = faces)
  lay   <- cube_piece_layout(g)
  tbl   <- cube_moves(3)

  set.seed(23)
  states <- t(vapply(1:40, function(i) {
    s <- group_identity(g)
    for (m in sample(faces, 12L, replace = TRUE)) s <- s[tbl[[m]]]
    s
  }, integer(54L)))

  in_r <- local({
    P <- lay$n_piece; W <- lay$width
    out <- array(0, dim = c(nrow(states), P, P * W))
    for (p in seq_len(P)) {
      here <- states[, lay$slots[p, 1L]]
      ix <- (lay$home[here] - 1L) * W + lay$turn[here]
      out[cbind(seq_len(nrow(states)), p, ix)] <- 1
    }
    # The C++ version returns the same values flat, so the comparison is made
    # against the same view rather than by reshaping its answer -- flattening
    # the expectation is what checks the ORDER survived, not just the count.
    matrix(out, nrow = nrow(states))
  })

  expect_equal(cayleyR:::adi_encode_pieces(states, lay), in_r)
})

test_that("state hashing partitions states exactly as pasting does", {
  g   <- cube_group(3)
  tbl <- cube_moves(3)
  set.seed(3)
  states <- t(vapply(1:2000, function(i) {
    s <- group_identity(g)
    for (m in sample(names(tbl), 8L, replace = TRUE)) s <- s[tbl[[m]]]
    s
  }, integer(54L)))

  keys  <- cayleyR:::cube_adi_keys(states)
  paste <- apply(states, 1L, paste, collapse = ",")

  # Same number of distinct values, and the same grouping of rows: a hash that
  # merged two states would show up as fewer keys than pasted strings.
  expect_equal(length(unique(keys)), length(unique(paste)))
  expect_equal(unname(split(seq_len(2000L), match(keys, unique(keys)))),
               unname(split(seq_len(2000L), match(paste, unique(paste)))))

  # And it must not depend on how the rows were batched.
  expect_equal(cayleyR:::cube_adi_keys(states[1:10, , drop = FALSE]),
               keys[1:10])
})
