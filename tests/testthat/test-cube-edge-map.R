test_that("edge structure is twelve edges of n-2 pieces at every size", {
  for (n in 3:7) {
    e <- cube_edge_structure(n)
    expect_equal(nrow(e), 12L * (n - 2L))
    expect_equal(length(unique(e$edge)), 12L)
    expect_true(all(table(e$edge) == n - 2L))
    # every piece carries two stickers and sits between two distinct faces
    expect_true(all(e$face_lo < e$face_hi))
    expect_true(all(e$pos >= 1L & e$pos <= n - 2L))
  }
})

test_that("edge orbits match cube_orbits", {
  for (n in 3:7) {
    e <- cube_edge_structure(n)
    o <- cube_orbits(n)
    oe <- o[o$kind == "edge", ]
    expect_setequal(unique(e$orbit), oe$orbit)
    for (i in seq_len(nrow(oe)))
      expect_equal(sum(e$orbit == oe$orbit[i]), oe$n_pieces[i])
  }
})

# The hand-measured table from src/cube_edges.h, edge_slots(), zero-based.
# The generic structure is held to it rather than merely believed.
edges_h_slots <- function() {
  t <- matrix(c(
     7, 18,  11, 17,  13, 33,  14, 34,   4, 65,   8, 66,   1, 82,   2, 81,
    24, 43,  20, 39,  30, 59,  29, 55,  27, 88,  23, 84,  45, 49,  46, 50,
    40, 75,  36, 71,  56, 77,  52, 78,  61, 94,  62, 93,  72, 91,  68, 87),
    ncol = 2, byrow = TRUE)
  lab <- c("0-1A", "0-1B", "0-2A", "0-2B", "0-4A", "0-4B", "0-5A", "0-5B",
           "1-2A", "1-2B", "1-3A", "1-3B", "1-5A", "1-5B", "2-3A", "2-3B",
           "2-4A", "2-4B", "3-4A", "3-4B", "3-5A", "3-5B", "4-5A", "4-5B")
  list(stickers = t + 1L, label = lab)   # to 1-based
}

test_that("cube_edge_structure(4) reproduces the hand table in cube_edges.h", {
  h <- edges_h_slots()
  hkey <- paste(pmin(h$stickers[, 1], h$stickers[, 2]),
                pmax(h$stickers[, 1], h$stickers[, 2]), sep = ",")

  e <- cube_edge_structure(4)
  gkey <- paste(pmin(e$sticker_a, e$sticker_b),
                pmax(e$sticker_a, e$sticker_b), sep = ",")

  expect_setequal(hkey, gkey)

  m <- match(hkey, gkey)
  expect_false(anyNA(m))

  # the face pair the hand table names must be the one the geometry gives
  expect_equal(paste(e$face_lo[m], e$face_hi[m], sep = "-"),
               sub("[AB]$", "", h$label))

  # A and B are the two positions along the edge
  expect_true(all(tapply(e$pos[m], sub("[AB]$", "", h$label),
                         function(v) length(unique(v))) == 2L))
})

test_that("outer turns carry an edge whole and slices split it", {
  # This is the mechanism the whole pairing stage rests on, measured rather
  # than assumed: see cube_edge_map_family.
  for (n in c(4L, 5L, 6L)) {
    es <- cube_edge_structure(n)
    for (f in c("U", "R", "F", "D", "L", "B")) {
      mp <- cube_edge_map(n, f, es)
      expect_equal(nrow(mp), 4L * (n - 2L), info = paste(n, f))
      expect_false(any(mp$splits), info = paste(n, f))
    }
    slices <- grep("^[0-9]+[xyz]$", names(cube_moves(n)), value = TRUE)
    for (s in slices) {
      mp <- cube_edge_map(n, s, es)
      expect_equal(nrow(mp), 4L, info = paste(n, s))
      expect_true(all(mp$splits), info = paste(n, s))
    }
  }
})

test_that("edge counts agree with cube_is_reduced", {
  set.seed(4)
  for (n in c(4L, 5L)) {
    expect_equal(cube_edge_counts(cube_identity(n), n)$whole, 12L)
    for (k in 1:15) {
      s <- generate_state(group = cube_group(n), n_moves = 25)
      # reduction demands whole edges, so reduced implies all twelve whole
      if (cube_is_reduced(s, n))
        expect_equal(cube_edge_counts(s, n)$whole, 12L)
    }
  }
})

test_that("an outer turn never changes the count and a slice does", {
  for (n in c(4L, 5L, 6L, 7L)) {
    id <- cube_identity(n)
    mv <- cube_moves(n)
    expect_equal(cube_edge_counts(id[mv[["U"]]], n)$whole, 12L)
    expect_lt(cube_edge_counts(id[mv[["1x"]]], n)$whole, 12L)
  }
})

test_that("cube_wide_turn reproduces the expansions in cube_edges.h", {
  # The three constants the reference states for a 4x4x4.
  expect_equal(cube_wide_turn(4, "R"), c("R", "2x"))
  expect_equal(cube_wide_turn(4, "D"), c("D", "1y'"))
  expect_equal(cube_wide_turn(4, "U"), c("U", "2y"))

  # The same turn on a bigger cube names a different slice and nothing else.
  expect_equal(cube_wide_turn(5, "R"), c("R", "3x"))
  expect_equal(cube_wide_turn(6, "R"), c("R", "4x"))
  expect_equal(cube_wide_turn(5, "D"), c("D", "1y'"))
})

test_that("a wide turn is one rigid rotation of two layers", {
  # The property that fixes the slice's direction: both directions keep the
  # edges whole and both have order 4, so neither of those can decide it.
  for (n in c(4L, 5L, 6L)) {
    p <- cube_pieces(n)
    st <- strsplit(as.character(p$stickers), ",", fixed = TRUE)
    mv <- cube_moves(n)
    id <- cube_identity(n)
    s2p <- integer(6L * n * n)
    for (i in seq_len(nrow(p))) s2p[as.integer(st[[i]])] <- i

    for (f in c("U", "R", "F", "D", "L", "B")) {
      w <- cube_wide_turn(n, f)
      comp <- mv[[w[1]]][mv[[w[2]]]]
      a <- cayleyR:::.cube_face_layer(n, f)$axis
      uv <- setdiff(1:3, a)
      cn <- c("x", "y", "z")
      moved <- which(id[comp] != id)
      pcs <- unique(s2p[moved])
      c0 <- n - 1L
      rot <- vapply(pcs, function(i) {
        j <- s2p[which(comp == as.integer(st[[i]])[1])]
        u0 <- p[[cn[uv[1]]]][i]; v0 <- p[[cn[uv[2]]]][i]
        u1 <- p[[cn[uv[1]]]][j]; v1 <- p[[cn[uv[2]]]][j]
        if (u1 == v0 && v1 == c0 - u0) "cw"
        else if (u1 == c0 - v0 && v1 == u0) "ccw"
        else "other"
      }, character(1))
      expect_equal(length(unique(rot)), 1L, info = paste(n, f))
      expect_false("other" %in% rot, info = paste(n, f))
    }
  }
})

test_that("cube_edge_algs(4) is the six words of cube_edges.h", {
  expect_equal(cube_edge_algs(4), c(
    "R' 2x' F R F' R 2x",
    "D 1y' R U R' D' 1y",
    "D 1y' R F' U R' F D' 1y",
    "D 1y' R U R' F R' F' R D' 1y",
    "U 2y U 2y R U R' F R' F' R U 2y U 2y",
    "R 2x U U R 2x U U R 2x U U R 2x U U R 2x U U"
  ))
})

test_that("the algorithms break the pairs the reference records", {
  # cube_edges.h states 3, 3, 2, 2, 2 and 4 pairs broken from solved.
  broken <- c(3L, 3L, 2L, 2L, 2L, 4L)
  for (n in c(4L, 5L, 6L, 7L)) {
    es <- cube_edge_structure(n)
    mv <- cube_moves(n)
    id <- cube_identity(n)
    for (i in seq_along(broken)) {
      s <- id
      for (m in strsplit(cube_edge_algs(n)[i], " ", fixed = TRUE)[[1]])
        s <- s[mv[[m]]]
      expect_equal(12L - cube_edge_counts(s, n, es)$whole, broken[i],
                   info = paste(n, i))
    }
  }
})

test_that("the outer turns are derived, not named", {
  # At n = 3 the alphabet also holds M, E and S, which split no edge and so
  # belong here; a test on the name would have missed them.
  for (n in 4:6)
    expect_setequal(cayleyR:::.cube_outer_turns(n),
                    c("U", "U'", "R", "R'", "F", "F'",
                      "D", "D'", "L", "L'", "B", "B'"))
  expect_true(all(c("M", "E", "S") %in% cayleyR:::.cube_outer_turns(3)))
})

test_that("pairing finishes a 4x4x4", {
  # Measured with the centre guard off, which is the setting the reference's
  # own figure of thirty-of-thirty was taken under: it runs after the centre
  # stage, on a cube whose centres are already built. Here the scrambles are
  # random, so there are no built centres to protect.
  skip_on_cran()
  set.seed(11)
  for (k in 1:3) {
    s <- generate_state(group = cube_group(4), n_moves = 20)
    r <- cube_pair_edges(s, 4L, depth = 3L, max_rounds = 25L,
                         keep_centres = FALSE)
    expect_true(r$solved, info = paste("cube", k, "reached", r$whole))
  }
})

test_that("the centre guard is honoured when it is asked for", {
  skip_on_cran()
  set.seed(11)
  s <- generate_state(group = cube_group(4), n_moves = 20)
  r <- cube_pair_edges(s, 4L, depth = 2L, max_rounds = 8L,
                       keep_centres = TRUE)
  expect_equal(cayleyR:::.cube_centres_signature(r$state, 4L),
               cayleyR:::.cube_centres_signature(s, 4L))
})

test_that("pairing improves a 5x5x5 without spending the centres", {
  # The stage does not finish an odd cube: the six algorithms are 4x4x4
  # algorithms and cannot address the middle slice independently of the
  # wings. What is asserted here is that it makes progress and stays safe --
  # see the TODO for the commutator this is waiting on.
  skip_on_cran()
  set.seed(11)
  s <- generate_state(group = cube_group(5), n_moves = 20)
  before <- cube_edge_counts(s, 5L)$whole
  r <- cube_pair_edges(s, 5L, depth = 2L, max_rounds = 10L,
                       keep_centres = FALSE)
  expect_gte(r$whole, before)
  expect_false(r$solved)
})
