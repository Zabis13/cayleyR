#!/usr/bin/env Rscript
# Surface area and volume of the landmark solid, swept over a range of n.
#
# Companion to demo_landmark_network.R, which draws one graph size at a time.
# This one draws nothing: it walks a range of ring sizes, measures the figure
# the landmarks span at each, and prints a table.
#
# For every n the same twelve landmarks are used -- the endpoints of PAIRS, the
# disjoint far-apart pairs found at n = 20. Their positions come from the
# celestial coordinates of the identity -> landmark path, i.e. the (nL, nR, nX)
# counts of the route the solver took. Both hulls are reported: the convex one,
# which leaves interior landmarks off the surface, and the enclosing one, which
# dents inwards until every landmark is a corner.
#
# A caveat that applies to every number below: those routes are not guaranteed
# shortest, so the figures describe a particular set of walks under a particular
# solver. They are comparable to each other -- across n, or across a change to
# the solver -- but they are not invariants of the graph.
#
# Run with:  Rscript inst/examples/landmark_hull_scan.R

library(cayleyR)

# ---------------------------------------------------------------- PARAMETERS

N_FROM <- 10L               # smallest ring size (human_algorithm needs n >= k+6)
N_TO <- 30L                 # largest
N_BY <- 1L                  # step

K <- 4L                     # reverse-prefix length
DEPTH <- 5L                 # BFS depth used by short_path_bfs
ROUNDS <- 3L                # how many times to re-apply the shortener

# The twelve landmarks to measure, as the six disjoint pairs from
# demo_landmark_network.R. Only the names matter here; the pairing itself is not
# used, since the figure is spanned by all twelve at once.
PAIRS <- rbind(
  c("reverse_first",  "pair_shift"),
  c("cycles3",        "block_reverse_pairs"),
  c("alt_pairs",      "full_reverse"),
  c("block_rotate3",  "zigzag"),
  c("spiral",         "adjacent_swaps"),
  c("reverse_second", "two_cycles")
)

OUT_CSV <- "landmark_hull_scan.csv"   # "" to skip writing
DIGITS <- 2                 # decimals in the printed table

# The solver fails on some landmarks at odd n -- for odd n the tail parity works
# out so that the finish table cannot reach every arrangement, which is a known
# limitation, not a bug in this script. A figure spanned by four points is not
# comparable with one spanned by twelve, so by default a size is dropped unless
# every landmark was solved. Set to FALSE to measure whatever was reached.
REQUIRE_ALL <- TRUE

# ----------------------------------------------------------------- HELPERS

# Print a data.frame as a ruled table. Box-drawing characters need a UTF-8
# locale; ASCII is used instead when the console cannot render them, since
# Rscript under a non-UTF-8 locale turns them into mojibake.
print_table <- function(df, title = NULL, digits = 2) {
  utf8 <- isTRUE(l10n_info()$`UTF-8`)
  gl <- if (utf8) {
    list(h = "─", v = "│",
         tl = "┌", tm = "┬", tr = "┐",
         ml = "├", mm = "┼", mr = "┤",
         bl = "└", bm = "┴", br = "┘")
  } else {
    list(h = "-", v = "|", tl = "+", tm = "+", tr = "+",
         ml = "+", mm = "+", mr = "+", bl = "+", bm = "+", br = "+")
  }

  cells <- vapply(df, function(col) {
    if (is.numeric(col)) {
      formatC(col, format = "f",
              digits = if (all(col == round(col))) 0 else digits,
              big.mark = " ")
    } else {
      as.character(col)
    }
  }, character(nrow(df)))
  if (is.null(dim(cells))) cells <- matrix(cells, nrow = nrow(df))

  head_txt <- names(df)
  w <- pmax(nchar(head_txt), apply(nchar(cells), 2, max))

  rule <- function(left, mid, right) {
    paste0(left, paste(vapply(w, function(x) strrep(gl$h, x + 2), ""),
                       collapse = mid), right)
  }
  # formatC() takes a single width, so the padding is done per column. Right
  # alignment throughout: pre-formatted numbers arrive as character columns and
  # would otherwise sit flush left, out of line with the numeric ones.
  line <- function(vals) {
    padded <- vapply(seq_along(vals),
                     function(i) formatC(vals[i], width = w[i], flag = ""),
                     character(1))
    paste0(gl$v, " ", paste(padded, collapse = paste0(" ", gl$v, " ")),
           " ", gl$v)
  }

  if (!is.null(title)) cat("\n", title, "\n", sep = "")
  cat(rule(gl$tl, gl$tm, gl$tr), "\n", sep = "")
  cat(line(head_txt), "\n", sep = "")
  for (i in seq_len(nrow(cells))) {
    cat(rule(gl$ml, gl$mm, gl$mr), "\n", sep = "")
    cat(line(cells[i, ]), "\n", sep = "")
  }
  cat(rule(gl$bl, gl$bm, gl$br), "\n", sep = "")
}

shorten <- function(path, start_state, k, depth, rounds) {
  for (i in seq_len(rounds)) {
    before <- length(path)
    path <- short_path_bfs(path, start_state, k, depth)$path
    if (length(path) >= before) break
  }
  path
}

# Celestial position of a state, taken from the route that reaches it from the
# identity: the L/R/X counts of that word map to (theta, phi, omega).
landmark_position <- function(target, k, depth, rounds) {
  n <- length(target)
  res <- human_algorithm_to(seq_len(n), target, k = k)
  if (!isTRUE(res$found)) return(NULL)
  p <- shorten(res$path, seq_len(n), k, depth, rounds)
  ops <- c("1" = "L", "2" = "R", "3" = "X")[p]
  cel <- convert_LRX_to_celestial(sum(ops == "L"), sum(ops == "R"),
                                  sum(ops == "X"))
  list(
    xyz = c(cel$omega_conformal * sin(cel$theta) * cos(cel$phi),
            cel$omega_conformal * sin(cel$theta) * sin(cel$phi),
            cel$omega_conformal * cos(cel$theta)),
    len = length(p)
  )
}

# --------------------------------------------------------------------- SCAN

names_wanted <- unique(as.vector(PAIRS))
n_values <- seq(N_FROM, N_TO, by = N_BY)

cat(sprintf("TopSpin(k = %d), %d landmarks, n from %d to %d by %d\n",
            K, length(names_wanted), N_FROM, N_TO, N_BY))
cat(sprintf("Landmarks: %s\n\n", paste(names_wanted, collapse = ", ")))

rows <- list()

for (n in n_values) {
  t0 <- Sys.time()
  lm <- landmark_states(n)

  idx <- match(names_wanted, lm$name)
  if (anyNA(idx)) {
    cat(sprintf("n = %3d  skipped: %s not in landmark_states(%d)\n",
                n, paste(names_wanted[is.na(idx)], collapse = ", "), n))
    next
  }

  pos <- lapply(idx, function(i)
    landmark_position(lm$state[[i]], K, DEPTH, ROUNDS))
  ok <- !vapply(pos, is.null, logical(1))
  if (REQUIRE_ALL && !all(ok)) {
    cat(sprintf("n = %3d  skipped: %d of %d landmarks unsolved (%s)\n",
                n, sum(!ok), length(ok),
                paste(names_wanted[!ok], collapse = ", ")))
    next
  }
  if (sum(ok) < 4L) {
    cat(sprintf("n = %3d  skipped: only %d landmarks solved, need 4\n",
                n, sum(ok)))
    next
  }

  xyz <- t(vapply(pos[ok], function(z) z$xyz, numeric(3)))
  path_len <- vapply(pos[ok], function(z) z$len, numeric(1))

  ch <- convex_hull_3d(xyz)
  eh <- enclosing_hull_3d(xyz)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  rows[[length(rows) + 1L]] <- data.frame(
    n = n,
    landmarks = sum(ok),
    mean_path = mean(path_len),
    max_path = max(path_len),
    convex_vertices = length(ch$vertices),
    convex_faces = nrow(ch$faces),
    convex_area = ch$area,
    convex_volume = ch$volume,
    enclosing_vertices = length(eh$vertices),
    enclosing_faces = nrow(eh$faces),
    enclosing_area = eh$area,
    enclosing_volume = eh$volume,
    secs = elapsed,
    stringsAsFactors = FALSE
  )

  cat(sprintf("n = %3d  paths %6.1f avg  convex %8.2f / %10.2f  enclosing %8.2f / %10.2f  (%.1fs)\n",
              n, mean(path_len), ch$area, ch$volume, eh$area, eh$volume,
              elapsed))
}

if (!length(rows)) {
  cat("\nNothing measured.\n")
  quit(save = "no")
}

res <- do.call(rbind, rows)

# ------------------------------------------------------------------ REPORT

# The convex hull is measured too and written to the CSV, but not printed: it
# differs from the enclosing one only by the landmarks it swallows, and that
# difference is a few percent of area at most.
#
# The x-columns are the ratio to the previous row rather than a difference,
# since both quantities grow by orders of magnitude over the range. The first
# row has no predecessor, hence the dash.
ratio <- function(v) c("-", sprintf("%.2f", v[-1L] / v[-length(v)]))

tab <- data.frame(
  n = res$n,
  path = round(res$mean_path),
  verts = res$enclosing_vertices,
  area = formatC(res$enclosing_area, format = "f", digits = DIGITS,
                 big.mark = " "),
  x_area = ratio(res$enclosing_area),
  volume = formatC(res$enclosing_volume, format = "f", digits = DIGITS,
                   big.mark = " "),
  x_vol = ratio(res$enclosing_volume),
  stringsAsFactors = FALSE
)
print_table(tab, "--- Enclosing hull (every landmark is a corner) ---", DIGITS)

if (nrow(res) > 1L) {
  first <- 1L
  last <- nrow(res)
  cat(sprintf("\nOver the whole range (n %d -> %d, x%.1f):",
              res$n[first], res$n[last], res$n[last] / res$n[first]))
  cat(sprintf("  path x%.1f   area x%.1f   volume x%.1f\n",
              res$mean_path[last] / res$mean_path[first],
              res$enclosing_area[last] / res$enclosing_area[first],
              res$enclosing_volume[last] / res$enclosing_volume[first]))
}

cat(sprintf("\nTotal time: %.1fs\n", sum(res$secs)))

if (nzchar(OUT_CSV)) {
  utils::write.csv(res, OUT_CSV, row.names = FALSE)
  cat("Written to", OUT_CSV, "\n")
}
