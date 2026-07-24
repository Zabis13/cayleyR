# Benchmark human_algorithm_to() against human_algorithm() on the plain
# sorting task: from a random point back to the identity state 1:n.
#
# Both functions are given the same starting states, so the columns are
# directly comparable. Raw and shortened move counts are reported separately:
# the solvers are called with simplify = FALSE to time the search alone, then
# short_path_bfs() is applied on top of the raw word.
#


library(cayleyR)

n_values <- seq(50, 50, by = 1)
n_reps <- 3
k <- 4L
depth <- 4L

set.seed(2024)

rows <- lapply(n_values, function(n) {
  target <- seq_len(n)

  # Same starting states for both solvers.
  starts <- lapply(seq_len(n_reps), function(i) {
    generate_state(n, k, n_moves = sample(200:1000, 1))
  })

  measure <- function(solver) {
    time_s <- numeric(n_reps)
    short_time_s <- numeric(n_reps)
    raw <- integer(n_reps)
    shortened <- integer(n_reps)
    found <- logical(n_reps)

    for (i in seq_len(n_reps)) {
      state <- starts[[i]]

      t0 <- proc.time()[["elapsed"]]
      res <- solver(state, k)
      time_s[i] <- proc.time()[["elapsed"]] - t0

      found[i] <- res$found
      if (!res$found) {
        raw[i] <- NA_integer_
        shortened[i] <- NA_integer_
        short_time_s[i] <- NA_real_
        next
      }

      raw[i] <- length(res$path)

      t1 <- proc.time()[["elapsed"]]
      sh <- short_path_bfs(res$path, state, k, depth = depth)
      short_time_s[i] <- proc.time()[["elapsed"]] - t1
      shortened[i] <- sh$new_length
    }

    list(time_s = time_s, short_time_s = short_time_s,
         raw = raw, shortened = shortened, found = found)
  }

  to <- measure(function(state, k) {
    human_algorithm_to(state, target, k = k, simplify = FALSE)
  })

  cat(sprintf("n = %3d  done\n", n))

  data.frame(
    n = n,
    time = mean(to$time_s),
    short_time = mean(to$short_time_s, na.rm = TRUE),
    raw = mean(to$raw, na.rm = TRUE),
    shortened = mean(to$shortened, na.rm = TRUE),
    found = sum(to$found)
  )
})

stats <- do.call(rbind, rows)

cat("\nhuman_algorithm_to(), target = 1:n, k =", k,
    ", shortening via short_path_bfs(depth =", depth, "),",
    n_reps, "runs per size\n\n")

# Box-drawing table: pad every cell to its column width, then draw a rule
# between rows. Widths come from the header and the cells themselves, so the
# frame stays aligned whatever the numbers turn out to be.
#
# The frame characters are written as \u escapes rather than literally. Rscript
# takes the locale of whatever shell starts it, and a literal multi-byte
# character in the source breaks the parser in a non-UTF-8 one -- the script
# would run to completion and then die on this very print. Escaped, the file
# stays plain ASCII on disk and R builds the characters at run time.
print_boxed <- function(header, cells) {
  widths <- pmax(nchar(header), apply(nchar(cells), 2, max))

  rule <- function(left, mid, right) {
    cat(left, paste(vapply(widths + 2L, strrep, character(1), x = "\u2500"),
                    collapse = mid), right, "\n", sep = "")
  }
  line <- function(values) {
    padded <- paste0(values, strrep(" ", widths - nchar(values)))
    cat("\u2502 ", paste(padded, collapse = " \u2502 "), " \u2502\n", sep = "")
  }

  rule("\u250c", "\u252c", "\u2510")
  line(header)
  for (i in seq_len(nrow(cells))) {
    rule("\u251c", "\u253c", "\u2524")
    line(cells[i, ])
  }
  rule("\u2514", "\u2534", "\u2518")
}

print_boxed(
  c("n", "sec", "raw", "short", "found"),
  cbind(
    stats$n,
    formatC(stats$time, format = "f", digits = 3),
    formatC(stats$raw, format = "f", digits = 1),
    formatC(stats$shortened, format = "f", digits = 1),
    paste0(stats$found, "/", n_reps)
  )
)
