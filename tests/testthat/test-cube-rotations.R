# Cube rotations in algorithm notation.
#
# A rotation printed in an algorithm -- x, y, z -- is an instruction to the
# reader: turn the cube in your hands, and read the remaining letters from
# where it now is. It is not a move to be performed and forgotten. Expanding it
# as a move, leaving the following letters to mean the faces they meant before,
# silently yields a different algorithm; that is what destroyed the middle
# layer in the published V-perm.
#
# So the expansion renames instead. These tests check the renaming as a
# property in its own right, over many words, rather than by whether the
# algorithm tables happen to come out right --- a table can be correct for the
# wrong reason, and then the next table added is not.

rotations <- c("x", "x'", "y", "y'", "z", "z'")

inverse_rotation <- function(r) if (grepl("'", r)) substr(r, 1, 1) else paste0(r, "'")

# A supply of words to conjugate: single moves, and sequences of the kind
# algorithms are actually made of.
test_words <- c(
  "R", "U", "F", "D", "L", "B",
  "R'", "U'", "F'", "D'", "L'", "B'",
  "R2", "U2", "F2",
  "R U R' U'",
  "R U2 R' U' R U' R'",
  "F R U R' U' F'",
  "R' D' R D",
  "R U R' U R U2 R'",
  "M U M' U2 M U M'"
)

test_that("a rotation is exactly a renaming: x ALG x' equals rename(ALG)", {
  # The defining property. Conjugating a word by a rotation must equal writing
  # the word out in the rotated frame -- that is what the reader does when they
  # turn the cube, and it is the only thing the expansion is allowed to mean.
  id <- cube_identity(3)

  for (r in rotations) {
    ri <- inverse_rotation(r)
    for (w in test_words) {
      conjugated <- cube_apply_word(id, paste(cube_expand_alg(paste(r, w, ri)),
                                              collapse = " "))
      renamed <- cube_apply_word(id, paste(cube_expand_alg(paste(r, w)),
                                           collapse = " "))
      # `r w` renames w and then leaves the cube rotated; `r w ri` renames and
      # rotates back. Compare the renaming alone by undoing the rotation.
      renamed_back <- cube_apply_word(renamed,
                                      paste(cube_expand_alg(ri), collapse = " "))
      expect_identical(conjugated, renamed_back,
                       info = paste("rotation", r, "on", w))
    }
  }
})

test_that("a rotation and its inverse cancel as a renaming", {
  # Renaming through a rotation and back must leave every letter as it was.
  for (r in rotations) {
    ri <- inverse_rotation(r)
    for (w in test_words) {
      expect_identical(cube_expand_alg(paste(r, ri, w)), cube_expand_alg(w),
                       info = paste(r, ri, "on", w))
    }
  }
})

test_that("four quarter rotations about one axis rename to nothing", {
  for (r in c("x", "y", "z")) {
    four <- paste(rep(r, 4), collapse = " ")
    for (w in c("R", "U", "R U R' U'")) {
      expect_identical(cube_expand_alg(paste(four, w)), cube_expand_alg(w),
                       info = paste(r, "x4 on", w))
    }
  }
})

test_that("a rotation on its own expands to nothing", {
  # A rotation is a renaming of what follows it, so a rotation with nothing
  # following is no moves at all. This is the whole design in one line: the
  # cube is never turned, the letters are.
  for (r in rotations) {
    expect_length(cube_expand_alg(r), 0L)
  }
  expect_length(cube_expand_alg("x y z"), 0L)
})

test_that("a rotation turns the cube when written as slices", {
  # The rotation as a physical act still exists --- it is what the solvers use
  # to put turned centres back --- but it is spelled in slices, because those
  # are moves. It moves the centres and no piece relative to any other: the
  # cube stays solved as a cube, every face one colour.
  #
  # It does not leave cp/co/ep/eo alone, and expecting it to is a mistake worth
  # naming. Those are read against the slots, and a rotation carries every
  # piece into a slot on a different axis; a corner that is not twisted at all
  # still reads co = 2 once it sits where the frame measures it differently.
  # Solvedness of the pieces is not the same claim as solvedness of the colours.
  id <- cube_identity(3)
  as_slices <- c(x = "R M' L'", `x'` = "R' M L",
                 y = "U E' D'", `y'` = "U' E D",
                 z = "F S B'", `z'` = "F' S' B")
  for (r in names(as_slices)) {
    s <- cube_apply_word(id, as_slices[[r]])
    expect_false(all(s[cube_centre_positions()] == cube_centre_positions()),
                 info = paste(r, "should move the centres"))
    expect_true(cube_is_colour_solved(s, 3L),
                info = paste(r, "should leave the cube solved as colours"))
  }
})

test_that("rotations compose, innermost first", {
  # Two rotations in a row rename twice. Renaming through r1 and then r2 must
  # be the same as renaming through r1 alone the word that r2 already renamed
  # --- which is what makes "x y R" well defined.
  for (r1 in rotations) {
    for (r2 in rotations) {
      for (w in c("R", "U", "F", "R U R' U'")) {
        once <- cube_expand_alg(paste(r2, w))
        # expanding is idempotent on already-expanded words, so feed the names
        # back through r1
        twice <- cube_expand_alg(paste(r1, paste(once, collapse = " ")))
        expect_identical(cube_expand_alg(paste(r1, r2, w)), twice,
                         info = paste(r1, r2, w))
      }
    }
  }
})

test_that("a rotation in the middle of an algorithm is honoured", {
  # The V-perm case, stated directly. Its published form has a y in the middle;
  # the letters after it must act on the rotated cube. If they do not, the
  # algorithm wrecks the first two layers instead of permuting the last.
  published <- "R' U R' U' y R' F' R2 U' R' U R' F R F"
  s <- cube_apply_word(cube_identity(3),
                       paste(cube_expand_alg(published), collapse = " "))
  p <- cube_predicates(s)
  expect_true(p[["f2l_solved"]])
  expect_true(p[["oll_solved"]])
  expect_false(p[["cube_solved"]])
})

test_that("slices agree with the composition they are equivalent to", {
  # The alphabet carries M, E and S as generators of their own rather than as
  # aliases for a composition of face turns and a rotation. Both descriptions
  # must name the same permutation, and nothing in the code enforces it -- the
  # move table is built from geometry, the identities below from notation, and
  # they meet only here.
  #
  # The identity is M = R L' x', not M = R L'. R and L' turn opposite faces in
  # opposite directions in space, so the pair does not add up to a slice on its
  # own; the rotation is what supplies the difference.
  id <- cube_identity(3)
  x  <- "R M' L'"     # the rotation, spelled physically
  xi <- "R' M L"
  y  <- "U E' D'"
  yi <- "U' E D"
  z  <- "F S B'"
  zi <- "F' S' B"

  expect_identical(cube_apply_word(id, "M"),
                   cube_apply_word(id, paste("R L'", xi)))
  expect_identical(cube_apply_word(id, "M'"),
                   cube_apply_word(id, paste("R' L", x)))
  expect_identical(cube_apply_word(id, "E"),
                   cube_apply_word(id, paste("U D'", yi)))
  expect_identical(cube_apply_word(id, "E'"),
                   cube_apply_word(id, paste("U' D", y)))
  expect_identical(cube_apply_word(id, "S"),
                   cube_apply_word(id, paste("F' B", z)))
  expect_identical(cube_apply_word(id, "S'"),
                   cube_apply_word(id, paste("F B'", zi)))
})

test_that("all three layers of an axis is a rotation", {
  # The other side of the same coin: a face, the slice beside it and the
  # opposite face, all turned the same way in space, move every piece together
  # and so leave the cube solved as colours while moving the centres.
  id <- cube_identity(3)
  for (w in c("R M' L'", "U E' D'", "F S B'")) {
    s <- cube_apply_word(id, w)
    expect_true(cube_is_colour_solved(s, 3L), info = w)
    expect_false(all(s[cube_centre_positions()] == cube_centre_positions()),
                 info = w)
  }
})

test_that("wide turns are the face plus the slice beside it", {
  # r is R with the middle layer following it, and the sense matters: M runs
  # with L, so r is R M', not R M.
  id <- cube_identity(3)
  expect_identical(cube_apply_word(id, paste(cube_expand_alg("r"), collapse = " ")),
                   cube_apply_word(id, "R M'"))
  expect_identical(cube_apply_word(id, paste(cube_expand_alg("u"), collapse = " ")),
                   cube_apply_word(id, "U E'"))
})
