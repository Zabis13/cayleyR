# The on-device batch generator for the Q network against the host.
#
# Not bit for bit -- its random numbers are the device's -- so what is checked
# is everything that has to hold for every batch:
#   * the replay used as reference applies moves as the C++ does (checked on
#     generate_walk's own words, 1, 2, 3 moves and a full walk);
#   * every word obeys the walk's rules up to the move taken at the cut;
#   * ban/cnt stay whole numbers and some move is always allowed;
#   * the encoded input is exactly the pivot state's encoding -- which pins the
#     composition order and the cut's off-by-one: the pivot is the state after
#     moves 0..cut-1, before the move at the cut;
#   * targets and mask match the moves that meet at the cut;
# and, over a large batch, the cut is uniform and the labelled moves are
# distributed like the C++ generator's.

skip_if_no_q_device <- function() {
  skip_if_not_installed("ggmlR")
  ok <- tryCatch(cayleyR:::.q_device_require(), error = function(e) FALSE)
  skip_if_not(isTRUE(ok), "ggmlR lacks the on-device Q-network functions")
}

setup_tab <- function(depth = 12L, no_cancel = TRUE) {
  g   <- cube_group(3)
  lay <- cube_piece_layout(g)
  list(g = g, lay = lay,
       tab = cayleyR:::.q_walk_tables(g, lay, depth, k_min = 2L,
                                      no_cancel = no_cancel))
}

draw <- function(s, batch, key) {
  dev <- cayleyR:::.q_walk_device(s$tab, batch)
  b   <- cayleyR:::.q_walk_batch(dev, key, debug = TRUE)
  lapply(b, as.matrix)
}

# Every per-batch property; returns nothing, fails through expectations.
check_batch <- function(s, b) {
  tab <- s$tab; B <- ncol(b$words); depth <- tab$depth
  words <- round(b$words); cut <- round(as.numeric(b$cut))
  expect_equal(b$words, words, tolerance = 0)
  expect_true(all(words >= 0 & words < tab$n_a))
  expect_true(all(cut >= tab$k_min & cut <= depth - 1L))

  expect_true(all(cayleyR:::.q_walk_rules_ok(tab, words, cut)))
  expect_equal(b$ban, round(b$ban), tolerance = 0)
  expect_equal(b$cnt, round(b$cnt), tolerance = 0)
  expect_true(all(b$ban %in% c(0, 1)))
  expect_gte(min(b$nallow), 1)

  # The input: the encoding of the state after exactly `cut` moves.
  states <- t(vapply(seq_len(B), function(i)
    cayleyR:::.q_walk_replay(tab, words[, i] + 1L, cut[i]), integer(tab$n)))
  enc <- cayleyR:::adi_encode(states, "transformer", s$lay)
  expect_equal(b$x, cayleyR:::.q_net_input(enc), tolerance = 0)

  # Targets: next = the move at the cut, prev = inverse of the one before.
  nxt <- words[cbind(cut + 1L, seq_len(B))] + 1L
  prv <- tab$inv[words[cbind(cut, seq_len(B))] + 1L]
  y <- matrix(0, tab$n_a, B); m <- y
  y[cbind(prv, seq_len(B))] <- (cut - 1 - tab$y_mu) / tab$y_sd
  y[cbind(nxt, seq_len(B))] <- (cut + 1 - tab$y_mu) / tab$y_sd
  m[cbind(prv, seq_len(B))] <- 1; m[cbind(nxt, seq_len(B))] <- 1
  expect_true(all(prv != nxt))
  expect_equal(b$y, y, tolerance = 1e-6)
  expect_equal(b$mask, m, tolerance = 0)
}

test_that("the host replay applies moves as the C++ walk does", {
  s <- setup_tab()
  set.seed(3L)
  for (k in c(1L, 2L, 3L, s$tab$depth)) {
    w <- generate_walk(s$g, n = 40L, n_moves = k, exact = TRUE, no_undo = TRUE)
    for (i in seq_len(40L)) {
      word <- match(w$moves[[i]], s$g$moves)
      expect_identical(cayleyR:::.q_walk_replay(s$tab, word, length(word)),
                       as.integer(w$states[i, ]), info = paste("k", k, "row", i))
    }
  }
})

test_that("the tables say what the C++ rules need (cube: order 4 / 2, inverses)", {
  s <- setup_tab()
  tab <- s$tab; mv <- s$g$moves
  expect_identical(mv[tab$inv[match("U", mv)]], "U'")
  expect_equal(unname(tab$order[match("U", mv)]), 4L)
  if ("U2" %in% mv) expect_equal(unname(tab$order[match("U2", mv)]), 2L)
  expect_equal(tab$comm[match("U", mv), match("D", mv)], 1)
  expect_equal(tab$comm[match("U", mv), match("R", mv)], 0)
})

test_that("an on-device batch obeys every rule (CPU, no_cancel)", {
  skip_if_no_q_device()
  old <- ggmlR::ag_device("cpu"); on.exit(ggmlR::ag_device(old), add = TRUE)
  s <- setup_tab(no_cancel = TRUE)
  check_batch(s, draw(s, 64L, c(1, 2)))
})

test_that("an on-device batch obeys every rule (CPU, no_undo only)", {
  skip_if_no_q_device()
  old <- ggmlR::ag_device("cpu"); on.exit(ggmlR::ag_device(old), add = TRUE)
  s <- setup_tab(no_cancel = FALSE)
  check_batch(s, draw(s, 64L, c(5, 6)))
})

test_that("cuts are uniform and labelled moves are distributed like the C++ walk's", {
  skip_if_no_q_device()
  skip_on_cran()
  old <- ggmlR::ag_device("cpu"); on.exit(ggmlR::ag_device(old), add = TRUE)
  s <- setup_tab(no_cancel = TRUE)
  tab <- s$tab; B <- 3000L
  b <- draw(s, B, c(11, 12))
  cut <- round(as.numeric(b$cut)); words <- round(b$words)

  lv <- tab$k_min:(tab$depth - 1L)
  expect_gt(stats::chisq.test(table(factor(cut, levels = lv)))$p.value, 1e-3)

  set.seed(13L)
  w <- generate_walk(s$g, n = B, n_moves = tab$depth, exact = FALSE,
                     no_undo = TRUE, pivot = TRUE, k_min = tab$k_min,
                     no_cancel = TRUE)
  dev_next <- words[cbind(cut + 1L, seq_len(B))] + 1L
  cpp_next <- match(w$next_move, s$g$moves)
  tbl <- rbind(tabulate(dev_next, tab$n_a), tabulate(cpp_next, tab$n_a))
  expect_gt(stats::chisq.test(tbl)$p.value, 1e-3)
  dev_cut <- table(factor(cut, levels = lv))
  cpp_cut <- table(factor(w$depth, levels = lv))
  expect_gt(stats::chisq.test(rbind(dev_cut, cpp_cut))$p.value, 1e-3)
})

test_that("an on-device batch obeys every rule (Vulkan)", {
  skip_if_no_q_device()
  skip_if_not(ggmlR::ggml_vulkan_available(), "Vulkan not available")
  old <- ggmlR::ag_device("gpu"); on.exit(ggmlR::ag_device(old), add = TRUE)
  ggmlR::ag_dtype("f32")
  s <- setup_tab(no_cancel = TRUE)
  check_batch(s, draw(s, 64L, c(21, 22)))
})
