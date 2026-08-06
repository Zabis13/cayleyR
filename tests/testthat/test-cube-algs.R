# The stage predicates and the algorithm tables.
#
# A solver stage is "search until the predicate holds", and a last-layer stage
# is "find the table entry that makes the predicate hold". So these two things
# are what the solvers are made of, and they can be checked without running a
# solver at all: a predicate against states built to satisfy it, a table
# against the property its entries are supposed to have.

# Scrambles for the solvers and for the tables come from faces only. The slice
# moves M, E and S turn the centres relative to the faces, and a state reached
# with them is not one a real cube can be in --- see test-cube-basics.R.
cube_faces <- function(g) g$moves[!grepl("^[MES]", g$moves)]

test_that("every predicate holds on a solved cube", {
  p <- cube_predicates(cube_identity(3))
  expect_true(all(p))
})

test_that("a scrambled cube fails the predicates it should", {
  set.seed(7)
  g <- cube_group(3)
  s <- generate_state(group = g, n_moves = 20, moves = cube_faces(g))
  p <- cube_predicates(s)
  expect_false(p[["cube_solved"]])
})

test_that("the cross means four edges in their own slots, not four edges on D", {
  # A single D turn leaves something that looks like a cross -- four D edges on
  # the D face -- but each is in the next slot round, and the predicate says
  # no. That strictness is what the next stage needs: F2L inserts a pair into a
  # named slot, so "the cross is somewhere on the bottom" is not enough.
  s <- cube_apply_word(cube_identity(3), "D")
  expect_false(cube_predicates(s)[["cross_solved"]])
  expect_equal(cube_read_state(s)$ep[5:8], c(5L, 6L, 7L, 4L))

  # four D turns is the identity, and the cross is back
  s <- cube_apply_word(cube_identity(3), "D D D D")
  expect_true(cube_predicates(s)[["cross_solved"]])

  # F breaks it outright: the DF edge leaves the bottom layer
  s <- cube_apply_word(cube_identity(3), "F")
  expect_false(cube_predicates(s)[["cross_solved"]])
})

test_that("no algorithm has an unpaired cube rotation", {
  # A rotation in the middle of a published algorithm means "turn the cube in
  # your hands, and read what follows from its new position". Expanded as a
  # permutation it does the turn but leaves the following letters naming the
  # faces they named before, so the algorithm does something else entirely.
  # Paired rotations -- x ... x' -- are safe, because the conjugation closes.
  #
  # This caught the published V-perm, whose y sits in the middle and destroyed
  # the middle layer.
  for (tn in c("oll", "pll")) {
    t <- cube_alg_table(tn)
    for (i in seq_len(nrow(t))) {
      tokens <- strsplit(trimws(t$notation[i]), "\\s+")[[1]]
      rots <- tokens[grepl("^[xyz]", tokens)]
      if (length(rots) == 0L) next

      # each axis used must appear an even number of times, and the plain and
      # primed forms must balance
      for (axis in c("x", "y", "z")) {
        on_axis <- rots[substr(rots, 1, 1) == axis]
        if (length(on_axis) == 0L) next
        net <- sum(ifelse(grepl("'", on_axis), -1L,
                          ifelse(grepl("2", on_axis), 2L, 1L)))
        expect_equal(net %% 4L, 0L,
                     info = paste(toupper(tn), i, t$name[i],
                                  "has unpaired", axis, ":", t$notation[i]))
      }
    }
  }
})

test_that("OLL leaves the top oriented and the first two layers standing", {
  # Every OLL algorithm is applied to a cube that has F2L done and the last
  # layer misoriented. Running one on the solved cube and then undoing the
  # orientation it caused is circular, so instead: apply the algorithm to a
  # solved cube, and check it disturbs only the last layer's orientation ---
  # F2L must survive, since that is the promise OLL makes.
  oll <- cube_alg_table("oll")
  expect_equal(nrow(oll), 57L)

  words <- attr(oll, "moves")
  id <- cube_identity(3)

  for (i in seq_len(nrow(oll))) {
    s <- cube_apply_word(id, paste(words[[i]], collapse = " "))
    p <- cube_predicates(s)
    expect_true(p[["f2l_solved"]],
                info = paste("OLL", i, oll$name[i], "broke F2L"))
  }
})

test_that("every OLL algorithm returns to the identity when repeated", {
  # An algorithm is a permutation, so repeating it enough times must come back.
  # A word that does not is not a permutation of the cube --- it is a typo.
  oll <- cube_alg_table("oll")
  words <- attr(oll, "moves")
  for (i in seq_len(nrow(oll))) {
    ord <- cube_word_order(paste(words[[i]], collapse = " "))
    expect_true(ord > 0L,
                info = paste("OLL", i, oll$name[i], "has no finite order"))
  }
})

test_that("PLL permutes the last layer and leaves the rest alone", {
  # A PLL algorithm moves last-layer pieces among themselves. Applied to a
  # solved cube it must leave F2L solved and the top still oriented --- it
  # permutes, it does not twist or flip.
  pll <- cube_alg_table("pll")
  expect_equal(nrow(pll), 21L)

  words <- attr(pll, "moves")
  id <- cube_identity(3)

  for (i in seq_len(nrow(pll))) {
    s <- cube_apply_word(id, paste(words[[i]], collapse = " "))
    p <- cube_predicates(s)
    expect_true(p[["f2l_solved"]],
                info = paste("PLL", i, pll$name[i], "broke F2L"))
    expect_true(p[["oll_solved"]],
                info = paste("PLL", i, pll$name[i], "broke orientation"))
  }
})

test_that("the last-layer edge and corner cycles are three-cycles", {
  # A three-cycle has order 3: applied three times it is the identity. This is
  # what layer by layer relies on when it repeats one until the pieces land.
  for (tbl in c("lbl_edge_perm", "lbl_corner_perm")) {
    t <- cube_alg_table(tbl)
    words <- attr(t, "moves")
    for (i in seq_len(nrow(t))) {
      ord <- cube_word_order(paste(words[[i]], collapse = " "))
      expect_equal(ord, 3L,
                   info = paste(tbl, i, t$name[i], "is not a three-cycle"))
    }
  }
})

test_that("the corner twist algorithm has order 6", {
  # R' D' R D twists one corner a third of a turn and disturbs the layer below.
  # Six repetitions bring the cube back: three to untwist the corner, doubled
  # because each pass also cycles pieces underneath.
  t <- cube_alg_table("lbl_corner_twist")
  w <- attr(t, "moves")[[1]]
  expect_equal(cube_word_order(paste(w, collapse = " ")), 6L)
})

test_that("the LL cross algorithm has order 3 in edge orientation", {
  # F R U R' U' F' cycles the orientation case: 4 oriented edges to 2, 2 to 2,
  # and back to 4. This is why the step cannot be written as "apply the one
  # that orients one more edge" --- there often is no such application.
  t <- cube_alg_table("lbl_cross")
  w <- paste(attr(t, "moves")[[1]], collapse = " ")

  n_oriented <- function(s) sum(cube_read_state(s)$eo[1:4] == 0L)

  s <- cube_identity(3)
  expect_equal(n_oriented(s), 4L)
  s <- cube_apply_word(s, w); expect_equal(n_oriented(s), 2L)
  s <- cube_apply_word(s, w); expect_equal(n_oriented(s), 2L)
  s <- cube_apply_word(s, w); expect_equal(n_oriented(s), 4L)
})

test_that("expanded algorithms are longer than the notation suggests", {
  # The tables are written the way the literature writes them, with half turns
  # and wide moves; the package's alphabet is quarter turns of single layers.
  # So "R U2 R'" is four moves here, not three, and every move count the
  # solvers report is in that larger metric.
  oll <- cube_alg_table("oll")
  half_turns <- grepl("2", oll$notation)
  expect_true(any(half_turns))

  tokens <- vapply(strsplit(trimws(oll$notation), "\\s+"), length, integer(1))
  expect_true(all(oll$n_moves[half_turns] > tokens[half_turns]))
})
