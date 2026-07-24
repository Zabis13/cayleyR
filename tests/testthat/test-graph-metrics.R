test_that("cayley_bfs_full enumerates the reachable component", {
  d <- cayley_bfs_full(1:5, k = 3)

  expect_s3_class(d, "data.frame")
  expect_true(all(c("state_str", "dist", "nL", "nR", "nX",
                    "theta", "phi", "omega") %in% names(d)))

  # L, R, X with k = 3 generate all of S5
  expect_equal(nrow(d), 120)
  expect_false(any(duplicated(d$state_str)))

  # start is its own first row, at distance 0
  expect_equal(d$state_str[1], "1_2_3_4_5")
  expect_equal(d$dist[1], 0)
  expect_true(all(d$dist >= 0))

  # BFS discovers vertices in non-decreasing distance order
  expect_false(is.unsorted(d$dist))

  # operation counts along the BFS path sum to the distance
  expect_equal(d$nL + d$nR + d$nX, d$dist)
})

test_that("restricting moves restricts the graph", {
  # L and R alone only rotate, giving a single 6-cycle
  d <- cayley_bfs_full(1:6, k = 3, moves = c("L", "R"))
  expect_equal(nrow(d), 6)
  expect_equal(max(d$dist), 3)
  expect_true(all(d$nX == 0))
})

test_that("integer and letter operation names agree", {
  a <- cayley_bfs_full(1:5, k = 3, moves = c("L", "R", "X"))
  b <- cayley_bfs_full(1:5, k = 3, moves = c("1", "2", "3"))
  expect_equal(a, b)
})

test_that("unknown operations are rejected", {
  expect_error(cayley_bfs_full(1:5, k = 3, moves = c("L", "Q")), "Unknown operation")
  expect_error(cayley_bfs_full(1:5, k = 3, moves = character(0)), "at least one")
})

test_that("cayley_graph_diameter finds the diameter of S5", {
  res <- cayley_graph_diameter(1:5, k = 3, method = "all_pairs")

  expect_equal(res$n_vertices, 120)
  expect_equal(res$diameter, 10)
  expect_equal(res$method, "all_pairs")
  expect_false(res$truncated)

  # every pair listed sits at exactly the diameter
  expect_true(all(res$pairs_df$dist == res$diameter))
  expect_equal(nrow(res$pairs_df), res$n_pairs)

  # both endpoints carry coordinates
  expect_true(all(c("from_state_str", "from_theta", "from_phi",
                    "to_state_str", "to_theta", "to_phi") %in%
                    names(res$pairs_df)))

  # the distance histogram accounts for every vertex
  expect_equal(sum(res$dist_hist$count), res$n_vertices)
})

test_that("diametral pairs really are that far apart", {
  res <- cayley_graph_diameter(1:5, k = 3)

  # verify a few pairs against an independent shortest-path search
  set.seed(42)
  for (i in sample(nrow(res$pairs_df), 3)) {
    s1 <- as.integer(strsplit(res$pairs_df$from_state_str[i], "_")[[1]])
    s2 <- as.integer(strsplit(res$pairs_df$to_state_str[i], "_")[[1]])
    path <- bidirectional_bfs(5, s1, s2, max_level = 8,
                              moves = c("1", "2", "3"), k = 3)
    expect_equal(length(path), res$diameter)
  }
})

test_that("from_start reports the start eccentricity", {
  all_pairs <- cayley_graph_diameter(1:5, k = 3, method = "all_pairs")
  from_start <- cayley_graph_diameter(1:5, k = 3, method = "from_start")

  # S5 under L/R/X is vertex-transitive, so the two agree here
  expect_equal(from_start$diameter, all_pairs$diameter)

  # from_start only knows the eccentricity of the start vertex
  expect_false(is.na(from_start$ecc$ecc[1]))
  expect_true(all(is.na(from_start$ecc$ecc[-1])))

  # its pairs all start at the start vertex
  expect_true(all(from_start$pairs_df$from_state_str == "1_2_3_4_5"))
  expect_equal(from_start$n_pairs, sum(from_start$bfs$dist == from_start$diameter))
})

test_that("all_pairs eccentricities bracket the diameter", {
  res <- cayley_graph_diameter(1:6, k = 3, method = "all_pairs")

  expect_false(any(is.na(res$ecc$ecc)))
  expect_equal(max(res$ecc$ecc), res$diameter)
  expect_true(min(res$ecc$ecc) <= res$diameter)   # radius
  expect_equal(nrow(res$ecc), res$n_vertices)
})

test_that("max_pairs caps the output but not the count", {
  res <- cayley_graph_diameter(1:5, k = 3, max_pairs = 7)

  expect_equal(nrow(res$pairs_df), 7)
  expect_true(res$truncated)
  expect_equal(res$n_pairs, 60)   # honest total, uncapped
})

test_that("operations that do not generate the group give a subgraph", {
  # with k = 3 the three operations do not reach all of S6
  res <- cayley_graph_diameter(1:6, k = 3)
  expect_lt(res$n_vertices, factorial(6))
  expect_gt(res$n_vertices, 1)
})
