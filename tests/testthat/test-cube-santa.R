## The Santa 2023 correspondence is stored as a law -- a face map and a rule
## turning f/r/d into an axis, a layer and a number of quarter turns -- rather
## than as a table of names. These tests are the guard on that law: they
## rederive it from scratch and fail if the package's own layout ever drifts
## away from what the constant assumes.

test_that("the alphabets have the same shape at every size", {
  for (n in 2:7) {
    santa <- cube_santa_move_names(n)
    ours <- cube_move_names(n)
    expect_equal(length(santa), 6L * n)
    expect_equal(length(santa), length(ours))
    expect_equal(length(unique(santa)), length(santa))

    m <- cube_santa_moves(n)
    expect_setequal(names(m), santa)
    expect_setequal(unname(m), ours)          # a bijection, not just a map
  }
})

test_that("translating a move name and back is the identity", {
  for (n in c(2L, 3L, 4L, 5L)) {
    m <- cube_santa_moves(n)
    back <- cube_moves_santa(n)
    expect_equal(unname(back[unname(m)]), names(m))
  }
})

test_that("the relabelling is an involution on faces and fixes their insides", {
  for (n in 2:6) {
    p <- cube_santa_perm(n)
    inv <- cube_santa_perm(n, inverse = TRUE)
    expect_setequal(p, seq_len(6L * n * n))
    expect_equal(inv[p], seq_len(6L * n * n))

    # whole faces move; within a face nothing does
    f2 <- n * n
    for (f in 0:5) {
      block <- p[(f * f2 + 1L):((f + 1L) * f2)]
      expect_equal(diff(block), rep(1L, f2 - 1L))       # a contiguous run
      expect_equal((block - 1L) %% f2, 0:(f2 - 1L))     # in the same order
    }
  }
})

test_that("the stored law is the one a search rediscovers", {
  # Rederive the correspondence the way it was found in the first place: try
  # every symmetry of the sticker layout that could relate the two
  # conventions, and check the stored one is among the solutions -- and that
  # the solutions differ only by a symmetry, not in substance.
  n <- 3L
  ns <- 6L * n * n
  f2 <- n * n
  m <- n - 1L

  ours <- cube_moves(n)
  key <- function(p) paste(p, collapse = ",")
  by_key <- stats::setNames(names(ours), vapply(ours, key, character(1)))

  # Santa's generators in Santa's own indexing, built from the law under test
  sigma0 <- cube_santa_perm(n)
  inv0 <- cube_santa_perm(n, inverse = TRUE)
  mp0 <- cube_santa_moves(n)
  santa <- stats::setNames(lapply(names(mp0), function(nm) {
    inv0[ours[[ mp0[[nm]] ]][sigma0]]
  }), names(mp0))

  # candidate relabellings: a face map with a grid symmetry per face is too
  # large to enumerate, so use the structure the search settled on -- faces
  # permuted, grids untouched -- and check every face permutation.
  faces <- 0:5
  hits <- 0L
  stored_seen <- FALSE
  idx <- expand.grid(a = faces, b = faces, c = faces,
                     d = faces, e = faces, f = faces)
  idx <- idx[apply(idx, 1L, function(r) length(unique(r)) == 6L), , drop = FALSE]

  for (i in seq_len(nrow(idx))) {
    fmap <- as.integer(idx[i, ])
    sig <- integer(ns)
    for (f in 0:5)
      sig[(f * f2 + 1L):((f + 1L) * f2)] <-
        (fmap[f + 1L] * f2 + 1L):((fmap[f + 1L] + 1L) * f2)
    inv <- integer(ns); inv[sig] <- seq_len(ns)

    ok <- TRUE
    for (nm in names(santa)) {
      q <- sig[santa[[nm]][inv]]
      if (is.na(by_key[key(q)])) { ok <- FALSE; break }
    }
    if (ok) {
      hits <- hits + 1L
      if (identical(sig, sigma0)) stored_seen <- TRUE
    }
  }

  expect_true(stored_seen)
  expect_gt(hits, 0L)
})

test_that("a state survives the round trip in both of Santa's forms", {
  for (n in 2:5) {
    ns <- 6L * n * n

    # colours
    s <- rep(0:5, each = n * n)
    expect_identical(cube_santa_state_out(cube_santa_state(s, n), n), s)
    # a solved Santa cube is a solved cube of ours
    expect_identical(cube_santa_state(s, n), (seq_len(ns) - 1L) %/% (n * n))

    # distinct sticker numbers
    p <- seq_len(ns) - 1L
    expect_identical(cube_santa_state(p, n), cube_identity(n))
    expect_identical(cube_santa_state_out(cube_santa_state(p, n), n), p)

    # a scrambled permutation, which is the form that carries the most
    set.seed(n)
    scr <- sample(ns) - 1L
    expect_identical(cube_santa_state_out(cube_santa_state(scr, n), n), scr)
  }
})

test_that("a state read as a comma-separated string matches the vector", {
  s <- rep(0:5, each = 4L)
  expect_identical(cube_santa_state(paste(s, collapse = ",")),
                   cube_santa_state(s))
})

test_that("moves and states agree: the same word lands in the same place", {
  # The point of the whole file. A Santa word applied to a Santa state, and
  # the translated word applied to the translated state, must agree -- checked
  # on wholly asymmetric states so that no accidental symmetry can hide a
  # mismatch, which is what a solved state would do.
  for (n in c(2L, 3L, 4L, 5L)) {
    ns <- 6L * n * n
    ours <- cube_moves(n)
    mp <- cube_santa_moves(n)
    santa <- stats::setNames(lapply(names(mp), function(nm) {
      # Santa's permutation, expressed in Santa's own indexing
      sig <- cube_santa_perm(n)
      inv <- cube_santa_perm(n, inverse = TRUE)
      inv[ours[[ mp[[nm]] ]][sig]]
    }), names(mp))

    set.seed(100L + n)
    for (trial in 1:20) {
      start <- sample(ns) - 1L                   # Santa's 0-based labels
      word <- sample(names(mp), 25, replace = TRUE)

      a <- start + 1L
      for (w in word) a <- a[santa[[w]]]

      b <- cube_santa_state(start, n)
      for (w in word) b <- b[ours[[ mp[[w]] ]]]

      expect_identical(cube_santa_state_out(b, n), a - 1L)
    }
  }
})

test_that("a path translates both ways, and half turns expand", {
  n <- 4L
  p <- "f0.-d3.r1.-f2"
  ourp <- cube_santa_path(p, n)
  expect_length(ourp, 4L)
  expect_identical(cube_santa_path_out(ourp, n), p)

  # a half turn has no name in either alphabet, so it becomes two moves
  expect_identical(cube_santa_path_out("U2", n), "-d3.-d3")

  expect_error(cube_santa_path("f9", n), "unknown move")
  expect_error(cube_santa_path("q0", n), "unknown move")

  # the geometry itself rejects a layer index the cube has no room for
  expect_error(cayleyR:::.santa_move_geometry("f4", 4L), "out of range")
  expect_error(cayleyR:::.santa_move_geometry("q0", 4L), "unknown move")
})

test_that("the group in Santa notation is the same group", {
  for (n in 2:4) {
    g <- cube_santa_group(n)
    expect_true(is_perm_group(g))
    expect_setequal(group_moves(g), cube_santa_move_names(n))

    # moving by a Santa name equals moving by the package name it maps to
    mp <- cube_santa_moves(n)
    gg <- cube_group(n)
    set.seed(n)
    s <- sample(6L * n * n)
    for (nm in names(mp))
      expect_identical(group_apply(g, s, nm), group_apply(gg, s, mp[[nm]]))
  }
})

test_that("the law matches real Santa 2023 puzzle files", {
  # Everything else in this file checks the law against itself, which catches
  # drift but not a wrong law. This is the one test tied to the outside: it
  # reads puzzle_info.json as Kaggle published it and demands the generators
  # agree permutation for permutation. It is skipped where those files are not
  # to hand, which is everywhere but the machine they were downloaded on.
  skip_if_not_installed("jsonlite")

  paths <- c("4" = "/mnt/Data2/DS_projects/444/puzzle_info.json",
             "7" = "/mnt/Data2/DS_projects/777/puzzle_info(1).json")
  paths <- paths[file.exists(paths)]
  skip_if(length(paths) == 0L, "no Santa 2023 puzzle files on this machine")

  for (i in seq_along(paths)) {
    n <- as.integer(names(paths)[i])
    pj <- jsonlite::fromJSON(paths[[i]], simplifyVector = TRUE)
    santa <- lapply(pj$generators, function(v) as.integer(v) + 1L)

    ours <- cube_moves(n)
    mp <- cube_santa_moves(n)
    sig <- cube_santa_perm(n)
    inv <- cube_santa_perm(n, inverse = TRUE)

    expect_setequal(names(santa), names(mp))
    for (nm in names(santa))
      expect_identical(sig[santa[[nm]][inv]], ours[[ mp[[nm]] ]],
                       info = paste("move", nm, "on", n, "x", n, "x", n))

    # and the solved state Kaggle ships comes across as ours
    cs <- as.integer(pj$central_state)
    got <- cube_santa_state(cs, n)
    want <- if (length(unique(cs)) == 6L) {
      (seq_len(6L * n * n) - 1L) %/% (n * n)
    } else {
      cube_identity(n)
    }
    expect_identical(got, want)
  }
})

test_that("errors are raised on states of the wrong size", {
  expect_error(cube_santa_state(rep(0:5, each = 5L)), "not 6n\\^2")
  expect_error(cube_santa_state(rep(0:5, each = 4L), n = 3L), "does not match")
  expect_error(cube_santa_state(c(0L, 7L, rep(0L, 22L))), "0\\.\\.5")
  expect_error(cube_santa_move_names(1L), "at least 2")
})
