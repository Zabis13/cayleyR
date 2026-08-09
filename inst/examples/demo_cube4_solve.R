#!/usr/bin/env Rscript
# Solving the 4x4x4 by reduction, stage by stage.
#
# An even cube cannot be solved the way a 3x3x3 is. Its centres are four pieces
# that move relative to each other, its edges are two halves apiece, and until
# both are gathered there is no "the U face" to speak of. Reduction is the
# standard answer: build the centres, pair the edges, and what is left behaves
# as a 3x3x3 with fat pieces, which the solver already in this package
# finishes.
#
#   centres   Pochmann's layer-by-layer: build one, put it on the left, fill
#             the layer beside it, then shoot the rest down from the top
#   edges     slice-flip-slice: a slice brings two halves together, a face
#             turn flips one, the slice goes back
#   parity    two last-layer states a 3x3x3 never reaches, so its tables have
#             no entry for them -- repaired with inner-layer algorithms
#   3x3x3     CFOP on the squeezed cube, its solution lifted back
#
# Move counts are quarter turns, the metric the whole package uses. Expect
# around 300 -- reduction is a method a person can hold in their head, not a
# short solution. A search-based solver reaches the same cube in a fraction of
# the moves and a great deal more time. Each solve is also passed through a
# general shortener, which takes out the detours left where one stage joins the
# next without either method knowing about cubes.
#
# Every figure below -- the table, the parity counts, the summary -- is read off
# one pass of n_states solves, so a state is solved once and reported on many
# times.
#
# Run with:  Rscript inst/examples/demo_cube4_solve.R

library(cayleyR)

N        <- 4L
n_states <- 20L       # samples -- the table, the parity counts and the summary
                      # all read from these same solves
n_moves  <- 600L       # quarter turns walked away from the solved cube
shorten_depth <- 4L   # BFS depth for the shortener; see solve_and_report()

set.seed(2026)

g  <- cube_group(N)
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

# Replay a path move by move, independently of the solver that produced it.
# A solver checked with its own arithmetic proves nothing.
replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

# One solve, measured and checked. Everything the demo reports below comes from
# these rows -- the table, the parity counts and the summary alike -- so a state
# is solved once and read many times rather than solved once per question.
solve_and_report <- function(method_name, method, state) {
  # Timed from here to the end of the shortener: the reported figure is what it
  # costs to get the short word, not what the solver alone took.
  t0 <- proc.time()[["elapsed"]]
  r  <- try(method(state), silent = TRUE)

  if (inherits(r, "try-error")) {
    secs <- proc.time()[["elapsed"]] - t0
    msg  <- trimws(sub("^.*cube_solve[^:]*: ", "",
                       conditionMessage(attr(r, "condition"))))
    return(list(status = "error", moves = NA_integer_, short = NA_integer_,
                seconds = secs, parity = character(0), result = NULL,
                message = msg))
  }

  # The same solve with the detours taken out. The stages are written one after
  # another and none looks back at what the one before left, so the joins often
  # hold a turn and its inverse -- a general shortener finds those without
  # knowing anything about cubes. Depth 2: the 4x4x4 alphabet is 24 moves, so
  # the neighbourhood grows as 24^d.
  sh <- short_path_bfs(r$path, state, depth = shorten_depth, group = g)

  # Verified by replaying the path, not by the solver's own flag.
  ok <- isTRUE(r$found) && cube_is_colour_solved(replay(state, r$path))

  list(status  = if (ok) "solved" else "unsolved",
       moves   = length(r$path),
       short   = sh$new_length,
       seconds = proc.time()[["elapsed"]] - t0,
       parity  = r$stages$detail[r$stages$name == "parity"],
       result  = r,
       message = "")
}

hr(paste(n_states, "solves"))

# The one pass everything below reads from.
states <- vector("list", n_states)
runs   <- vector("list", n_states)
for (i in seq_len(n_states)) {
  states[[i]] <- generate_state(group = g, n_moves = n_moves)
  runs[[i]]   <- solve_and_report("reduction", cube_solve4, states[[i]])
}

rows <- do.call(rbind, lapply(seq_len(n_states), function(i) {
  r <- runs[[i]]
  data.frame(state = i, method = "reduction", status = r$status,
             moves = r$moves, short = r$short, seconds = round(r$seconds, 2),
             stringsAsFactors = FALSE)
}))

cat(sprintf("%5s  %8s  %8s  %8s  %9s  %s\n",
            "state", "moves", "short", "seconds", "status", "stages"))
for (i in seq_len(n_states)) {
  r <- runs[[i]]
  cat(sprintf("%5d  %8s  %8s  %8.2f  %9s  %s\n", i,
              ifelse(is.na(r$moves), "-", r$moves),
              ifelse(is.na(r$short), "-", r$short),
              r$seconds, r$status,
              if (is.null(r$result)) r$message
              else paste(r$result$stages$name, collapse = " + ")))
}

hr("one solve, in detail")

state <- states[[1L]]
res   <- runs[[1L]]$result

cat("scramble      : ", n_moves, " quarter turns\n", sep = "")
cat("solved        : ", res$found, "\n", sep = "")
cat("total moves   : ", length(res$path), "\n", sep = "")
cat("after shorten : ", runs[[1L]]$short, "\n\n", sep = "")

print(res$stages)

cat("\nthe first twenty moves:\n  ",
    paste(head(res$path, 20), collapse = " "), " ...\n", sep = "")

hr("what each stage leaves behind")

# Read off the state, not taken on trust -- and read as COLOUR, not as face
# number. cube_centre_counts() compares a piece against the face it started on,
# which stops meaning anything once the solve has turned the cube; a face whose
# four centre pieces all match each other is built wherever it now sits.
faces_built <- function(s) {
  sum(vapply(0:5, function(f) {
    idx <- f * 16L + c(5L, 6L, 9L, 10L) + 1L
    length(unique((s[idx] - 1L) %/% 16L)) == 1L
  }, logical(1)))
}

pairs_made <- function(s) {
  p <- cube_pieces(4)
  e <- p[p$n_stickers == 2L, ]
  st <- lapply(strsplit(e$stickers, ","), as.integer)
  key <- vapply(st, function(i) paste(sort((i - 1L) %/% 16L), collapse = "-"),
                character(1))
  sum(vapply(split(st, key), function(g2) {
    a <- sort((s[g2[[1]]] - 1L) %/% 16L)
    b <- sort((s[g2[[2]]] - 1L) %/% 16L)
    identical(a, b)
  }, logical(1)))
}

for (i in seq_len(nrow(res$stages))) {
  s <- res$states[[i]]
  cat(sprintf("  %-10s %-20s faces built: %d of 6   edges paired: %2d of 12\n",
              res$stages$name[i], res$stages$detail[i],
              faces_built(s), pairs_made(s)))
}

hr("parity, which is what makes an even cube different")

# Reduction can leave the last layer in a state no 3x3x3 reaches. The 3x3x3
# solver knows all 57 OLL and 21 PLL cases, so its refusal IS the detection --
# there is no separate detector here. Read off the solves already done: a
# parity repair is a stage of the solve, so counting them costs nothing.
seen <- unlist(lapply(runs, `[[`, "parity"), use.names = FALSE)
cat("over ", n_states, " solves, parity repairs applied:\n", sep = "")
print(table(factor(seen, levels = c("OLL", "PLL"))))

# What a parity costs, in the only terms that matter here.
with_parity <- vapply(runs, function(r) length(r$parity) > 0L, logical(1))
ok_all      <- rows$status == "solved"
if (any(ok_all & with_parity) && any(ok_all & !with_parity)) {
  cat(sprintf("\nmean moves  with parity: %6.1f   without: %6.1f\n",
              mean(rows$moves[ok_all & with_parity]),
              mean(rows$moves[ok_all & !with_parity])))
}
cat("\nneither is a defect of the solve -- an odd number of inner-layer turns\n",
    "during reduction is enough to produce one, and it has to be undone with\n",
    "an inner-layer algorithm, which a 3x3x3 has no way to express.\n", sep = "")

# ---------------------------------------------------------------- summary
#
# Averaged over the states that finished. A method that solved none has no mean
# to report, and saying so is more use than a NaN. One method for now; the shape
# is the one demo_cube3_solve.R uses for four, so another is a row, not a
# rewrite.

hr("summary")
cat(sprintf("%d states, %d moves from solved\n\n", n_states, n_moves))

summary_row <- function(nm) {
  d  <- rows[rows$method == nm, ]
  ok <- d$status == "solved"
  data.frame(
    method     = nm,
    solved     = sprintf("%d/%d", sum(ok), nrow(d)),
    mean_moves = if (any(ok)) round(mean(d$moves[ok]), 1) else NA_real_,
    mean_short = if (any(ok)) round(mean(d$short[ok]), 1) else NA_real_,
    mean_sec   = if (any(ok)) round(mean(d$seconds[ok]), 2) else NA_real_,
    errors     = sum(d$status == "error"),
    stringsAsFactors = FALSE)
}

print(do.call(rbind, lapply(unique(rows$method), summary_row)),
      row.names = FALSE)

ok_rows <- rows[rows$status == "solved", ]
if (nrow(ok_rows) > 0) {
  cat(sprintf("\nmoves : median %g, range %g-%g\n",
              median(ok_rows$moves), min(ok_rows$moves), max(ok_rows$moves)))
  cat(sprintf("short : median %g, range %g-%g   (%.1f%% off at depth %d)\n",
              median(ok_rows$short), min(ok_rows$short), max(ok_rows$short),
              100 * (1 - mean(ok_rows$short) / mean(ok_rows$moves)),
              shorten_depth))
}

fails <- rows[rows$status != "solved", ]
if (nrow(fails) > 0) {
  cat("\ndid not solve:\n")
  print(fails[, c("state", "method", "status", "seconds")], row.names = FALSE)
}
