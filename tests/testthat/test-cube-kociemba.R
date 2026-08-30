test_that("the solved cube needs no moves", {
  res <- cube_kociemba(cube_identity(3))
  expect_length(res$path, 0L)
  expect_true(res$found)
})

# Not "in one move". Two phases mean phase 1 stops as soon as the cube is in
# G1, without regard for how far that leaves phase 2 to go: a cube one quarter
# turn from solved comes back as R R R rather than R', because R also lands in
# G1 and R2 finishes from there. Short, not shortest, is what the method
# promises.
test_that("a state one move from solved comes back solved and short", {
  moves <- cube_moves(3)
  names(moves) <- cube_move_names(3)
  s <- cube_identity(3)[moves[["R"]]]
  path <- cube_kociemba(s)$path
  for (mv in path) s <- s[moves[[mv]]]
  expect_identical(s, cube_identity(3))
  expect_lte(length(path), 4L)
})

# The point of the whole file: the path really returns the cube to solved.
# Scrambles include slice moves, which turn the centres -- the case that was
# silently wrong before the solver learned to turn the cube back first.
test_that("solutions solve the cube, slices included", {
  moves <- cube_moves(3)
  names(moves) <- cube_move_names(3)
  apply_path <- function(s, path) {
    for (mv in path) s <- s[moves[[mv]]]
    s
  }

  set.seed(11)
  for (n in c(3, 8, 12)) {
    s <- cube_identity(3)
    for (mv in sample(cube_move_names(3), n, replace = TRUE)) s <- s[moves[[mv]]]
    path <- cube_kociemba(s)$path
    expect_gt(length(path), 0L)
    expect_identical(apply_path(s, path), cube_identity(3))
  }
})

test_that("phase 1 lands in G1", {
  moves <- cube_moves(3)
  names(moves) <- cube_move_names(3)
  set.seed(3)
  s <- cube_identity(3)
  for (mv in sample(cube_move_names(3), 15, replace = TRUE)) s <- s[moves[[mv]]]

  # The solved cube is in G1; a scrambled one generally is not.
  expect_true(cayleyR:::cube_in_g1_cpp(cube_identity(3)))
  expect_false(cayleyR:::cube_in_g1_cpp(s[moves[["F"]]]))
})

# The piece reading is what the search runs on, so a disagreement between it
# and the sticker layer would be silent everywhere else.
test_that("the piece reading of the solved cube is the identity", {
  p <- cayleyR:::cube_cubie_pieces_cpp(cube_identity(3))
  expect_identical(p$corner_perm, 1:8)
  expect_identical(p$edge_perm, 1:12)
  expect_true(all(p$corner_ori == 0))
  expect_true(all(p$edge_ori == 0))
})

test_that("F flips four edges and twists four corners, U neither", {
  moves <- cube_moves(3)
  names(moves) <- cube_move_names(3)
  pF <- cayleyR:::cube_cubie_pieces_cpp(cube_identity(3)[moves[["F"]]])
  expect_equal(sum(pF$edge_ori), 4L)
  expect_equal(sum(pF$corner_ori != 0), 4L)

  pU <- cayleyR:::cube_cubie_pieces_cpp(cube_identity(3)[moves[["U"]]])
  expect_equal(sum(pU$edge_ori), 0L)
  expect_equal(sum(pU$corner_ori), 0L)
})

test_that("the report says which phase did what", {
  moves <- cube_moves(3)
  names(moves) <- cube_move_names(3)
  set.seed(5)
  s <- cube_identity(3)
  for (mv in sample(cube_move_names(3), 10, replace = TRUE)) s <- s[moves[[mv]]]
  cube_kociemba(s)
  r <- cube_kociemba_report()
  expect_identical(r$phase1, "found")
  expect_identical(r$phase2, "found")
})

test_that("a state that is not 54 stickers is refused", {
  expect_error(cube_kociemba(1:53), "54 stickers")
})
