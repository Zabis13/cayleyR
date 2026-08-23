#!/usr/bin/env Rscript
# Which short scrambles the four-phase search solves, and which it does not.
#
# Six cubes, five and six moves from solved, split three and three: the search
# finished half of them and spent its whole budget on the other half. A cube
# six quarter turns from solved has a solution at most six moves long, so the
# failures are not hard cubes. Something about their SHAPE is what the search
# cannot handle, and six examples hinted at what:
#
#   failed   U F' 1x 1y B' R'        inner turns on x and y
#            1y' F B' U' 2z' 2x      inner turns on y, z, x
#            1y 1x' 1z B' L' 2y'     inner turns on y, x, z, y
#   solved   1z' U' F B' 2z' F       inner turns on z only
#            1y' R B' R R L'         one inner turn
#            R' D' 1z' U' 1x L       inner turns on z and x
#
# The reading that suggests itself is inner turns spread across several axes.
# It is a plausible mechanism -- phases 1 and 2 exist precisely to get the
# centres back onto their axes, and inner turns are what takes them off -- but
# six cubes cannot tell a pattern from a coincidence, and the last row already
# breaks the simplest version of it.
#
# So: draw a few hundred short scrambles, record for each one what it is made
# of, run the reduction, and see which features separate the two groups.
# Features are cheap to compute and it is not obvious in advance which one
# matters, so all of them are recorded:
#
#   n_inner        how many inner-layer turns
#   n_inner_axes   how many distinct axes those inner turns use
#   inner_parity   odd or even count -- the classic source of 4x4x4 parity
#   n_outer        how many outer face turns
#   n_axes         distinct axes over the whole word
#   has_inner      whether there are any inner turns at all
#
# Run with:  Rscript inst/examples/diag_scramble_shape.R
#            Rscript inst/examples/diag_scramble_shape.R 200    # 200 cubes
#            Rscript inst/examples/diag_scramble_shape.R 200 5  # 5 moves each

library(cayleyR)

args     <- commandArgs(trailingOnly = TRUE)
n_states <- if (length(args) >= 1) as.integer(args[[1]]) else 120L
n_moves  <- if (length(args) >= 2) as.integer(args[[2]]) else 6L

N           <- 4L
node_budget <- 1e6   # small on purpose: these cubes are a handful of moves
                     # from solved, and a phase needing a million nodes for one
                     # has already answered the question

set.seed(2026)

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

# Same draw as generate_state(): moves independently with replacement, a word
# landing on the identity rejected. The word is kept, which generate_state()
# does not return and which is needed to describe the scramble's shape.
scramble <- function(n, max_attempts = 100L) {
  names_all <- cube_move_names(N)
  id <- cube_identity(N)
  for (i in seq_len(max_attempts)) {
    word <- sample(names_all, n, replace = TRUE)
    state <- replay(id, word)
    if (!identical(state, id)) return(list(state = state, word = word))
  }
  stop("scramble: failed to produce a non-identity state", call. = FALSE)
}

# What a move is made of. An inner-layer move is written "1x", "2y" and so on:
# a layer number then an axis. An outer move is a face letter, and its axis is
# the one that face turns about.
face_axis <- c(U = "y", D = "y", R = "x", L = "x", F = "z", B = "z")

move_axis <- function(m) {
  base <- sub("'$", "", m)
  if (grepl("^[12]", base)) substr(base, 2, 2) else face_axis[[base]]
}
is_inner <- function(m) grepl("^[12]", m)

features <- function(word) {
  inner <- word[vapply(word, is_inner, logical(1))]
  axes  <- vapply(word, move_axis, character(1))
  inner_axes <- vapply(inner, move_axis, character(1))
  list(n_inner      = length(inner),
       n_inner_axes = length(unique(inner_axes)),
       inner_parity = length(inner) %% 2L,
       n_outer      = length(word) - length(inner),
       n_axes       = length(unique(axes)),
       has_inner    = length(inner) > 0L)
}

hr("setup")
cat("cubes       : ", n_states, " scrambles of ", n_moves, " quarter turns\n",
    sep = "")
cat("node budget : ", format(node_budget, scientific = FALSE, big.mark = ","),
    " per phase\n", sep = "")
cat("\nEvery one of these has a solution at most ", n_moves,
    " moves long, so a failure\n", sep = "")
cat("is the search looking in the wrong place, not a hard cube.\n")

rows <- list()
for (i in seq_len(n_states)) {
  sc <- scramble(n_moves)
  f  <- features(sc$word)

  # How long each of the first two phases had to work. This is the control:
  # more inner axes means more centres off their axis, which means a longer
  # phase 1 -- so a solve rate that falls with the axis count might be nothing
  # but a solve rate that falls with the length of phase 1 and 2, with the axis
  # count riding along as a marker. Recorded per cube so the two can be told
  # apart in the summary rather than argued about.
  p1 <- cayleyR:::cube_kociemba4_phase12_cpp(sc$state, upto_phase = 1L,
                                             node_budget = node_budget)
  p2 <- cayleyR:::cube_kociemba4_phase12_cpp(sc$state, upto_phase = 2L,
                                             node_budget = node_budget)

  red <- cube_kociemba4_reduce(sc$state, node_budget = node_budget)$path
  rep <- cube_kociemba4_report()
  ok  <- length(red) > 0 && cube_is_reduced(replay(sc$state, red))

  rows[[i]] <- data.frame(
    cube = i, word = paste(sc$word, collapse = " "),
    n_inner = f$n_inner, n_inner_axes = f$n_inner_axes,
    inner_parity = f$inner_parity, n_outer = f$n_outer,
    n_axes = f$n_axes, has_inner = f$has_inner,
    p1_len = length(p1), p2_len = length(p2) - length(p1),
    solved = ok,
    stopped_at = if (ok) "" else
      if (rep$phase1 != "found") "phase 1"
      else if (rep$phase2 != "found") "phase 2" else "phase 3",
    p1_nodes = rep$phase1_nodes, p2_nodes = rep$phase2_nodes,
    p3_nodes = rep$phase3_nodes,
    stringsAsFactors = FALSE)

  if (i %% 10L == 0L) { cat("."); flush.console() }
}
cat("\n")

tab <- do.call(rbind, rows)

hr("overall")
cat(sprintf("solved %d of %d (%.0f%%)\n", sum(tab$solved), nrow(tab),
            100 * mean(tab$solved)))

if (any(!tab$solved)) {
  cat("\nwhere the failures stopped:\n")
  print(table(tab$stopped_at[!tab$solved]))
}

# Each feature against the outcome. A feature that matters shows a solve rate
# that changes down the column; one that does not is flat.
by_feature <- function(name) {
  hr(paste("by", name))
  d <- split(tab, tab[[name]])
  out <- do.call(rbind, lapply(names(d), function(k) {
    g <- d[[k]]
    data.frame(value = k, cubes = nrow(g),
               solved = sum(g$solved),
               rate = sprintf("%.0f%%", 100 * mean(g$solved)),
               stringsAsFactors = FALSE)
  }))
  print(out, row.names = FALSE)
}

for (f in c("n_inner", "n_inner_axes", "inner_parity", "n_outer", "n_axes",
            "has_inner")) {
  by_feature(f)
}

hr("the two features together")

# n_inner and n_inner_axes are not independent -- more inner turns can use more
# axes -- so the cross-tabulation says which of the two is doing the work.
cnt <- table(tab$n_inner, tab$n_inner_axes)
rate <- tapply(tab$solved, list(tab$n_inner, tab$n_inner_axes), mean)
cat("solve rate, rows = inner turns, columns = distinct inner axes\n")
cat("(blank where no cube of that shape was drawn)\n\n")
print(round(100 * rate))
cat("\ncubes in each cell:\n\n")
print(cnt)

hr("axes, controlled for how much work phases 1 and 2 had")

# The objection this answers: inner turns on more axes leave more centres off
# their axis, so phase 1 and phase 2 have further to go. If the solve rate
# falls with the axis count only because it falls with the length of those
# phases, then the axis count is a marker and not a cause -- and reading the
# derivers for an axis bug would be chasing a shadow.
#
# The way to tell: hold the phase lengths roughly fixed and see whether the
# axis count still moves the rate. If it does, axes are doing work of their
# own. If the columns go flat, the length was the whole story.
cat("solve rate by inner axes, split by how long phases 1+2 ran\n")
cat("(a column that still falls means the axes matter on their own)\n\n")

work <- tab$p1_len + tab$p2_len
band <- cut(work, breaks = c(-1, 4, 8, Inf),
            labels = c("short (<=4)", "medium (5-8)", "long (>8)"))

rate <- tapply(tab$solved, list(band, tab$n_inner_axes), mean)
cnt  <- table(band, tab$n_inner_axes)
cat("solve rate %, rows = phase 1+2 length, columns = distinct inner axes\n\n")
print(round(100 * rate))
cat("\ncubes in each cell:\n\n")
print(cnt)

cat("\nmean phase 1+2 length by axis count:\n")
for (k in sort(unique(tab$n_inner_axes))) {
  d <- tab[tab$n_inner_axes == k, ]
  cat(sprintf("  %d axes : phase 1 %.1f moves, phase 2 %.1f moves  (%d cubes)\n",
              k, mean(d$p1_len), mean(d$p2_len), nrow(d)))
}

hr("verdict")

rate_by_axes <- tapply(tab$solved, tab$n_inner_axes, mean)
cat("solve rate by number of distinct inner axes (uncontrolled):\n")
for (k in names(rate_by_axes)) {
  cat(sprintf("  %s axes : %.0f%%  (%d cubes)\n", k, 100 * rate_by_axes[[k]],
              sum(tab$n_inner_axes == as.integer(k))))
}

cat("\nsolve rate by how long phases 1 and 2 ran:\n")
rate_by_work <- tapply(tab$solved, band, mean)
for (k in names(rate_by_work)) {
  if (is.na(rate_by_work[[k]])) next
  cat(sprintf("  %-13s : %.0f%%  (%d cubes)\n", k, 100 * rate_by_work[[k]],
              sum(band == k, na.rm = TRUE)))
}

# Which of the two is the cause. The axis count and the phase length rise
# together -- more axes means more centres to put back -- so the raw rates
# cannot separate them. The controlled table can: whichever variable still
# moves the rate when the other is held fixed is the one doing the work.
#
# Read within a row (axes varying, length fixed) and down a column (length
# varying, axes fixed), over cells with enough cubes to mean anything.
enough <- cnt >= 5
axis_effect <- c()   # spread across a row: does the axis count still matter?
work_effect <- c()   # spread down a column: does the length still matter?
for (r in seq_len(nrow(rate))) {
  v <- rate[r, ][enough[r, ]]
  if (length(v) > 1) axis_effect <- c(axis_effect, max(v) - min(v))
}
for (cc in seq_len(ncol(rate))) {
  v <- rate[, cc][enough[, cc]]
  if (length(v) > 1) work_effect <- c(work_effect, max(v) - min(v))
}

cat("\nwith the other held fixed, the rate moves by:\n")
cat(sprintf("  axes, within a length band : %s\n",
            if (length(axis_effect)) sprintf("%.0f points", 100 * mean(axis_effect))
            else "not enough cubes to say"))
cat(sprintf("  length, within an axis count: %s\n",
            if (length(work_effect)) sprintf("%.0f points", 100 * mean(work_effect))
            else "not enough cubes to say"))

cat("\n")
if (length(work_effect) && length(axis_effect) &&
    mean(work_effect) > mean(axis_effect)) {
  cat("The length of phases 1 and 2 is what decides it, not the axis count.\n")
  cat("The two rise together -- more inner axes means more centres to put\n")
  cat("back, hence a longer phase 1 -- so the raw rate by axes was the\n")
  cat("length showing through. Held fixed, the axis count stops mattering.\n")
  cat("\nThat is the thing to chase: a cube six quarter turns from solved\n")
  cat("whose first two phases spend nine moves has been carried a long way\n")
  cat("from where it started, and phase 3 is left to undo it. Why those\n")
  cat("phases go so far on so short a scramble is the question.\n")
} else if (length(axis_effect)) {
  cat("The axis count still moves the rate with the phase length held fixed,\n")
  cat("so it is doing work of its own rather than standing in for length.\n")
  cat("Phases 1 and 2 are where to look: putting centres back on their axes\n")
  cat("is their job, and they handle some spreads worse than others.\n")
}

hr("a few of each")
cat("failed:\n")
f <- head(tab[!tab$solved, c("word", "n_inner", "n_inner_axes", "stopped_at")], 10)
if (nrow(f) > 0) print(f, row.names = FALSE) else cat("  (none)\n")
cat("\nsolved:\n")
s <- head(tab[tab$solved, c("word", "n_inner", "n_inner_axes")], 10)
if (nrow(s) > 0) print(s, row.names = FALSE) else cat("  (none)\n")
