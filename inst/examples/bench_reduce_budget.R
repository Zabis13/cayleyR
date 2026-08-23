# What does phase 3 need, across thirty cubes?
#
# Settled by diag_phase3_ceiling.R: the failures were the node budget, not the
# depth ceiling and not reachability. Two cubes that had failed on 50,000,000
# nodes were solved on 55 million and 135 million, both within a ceiling of
# twenty generators. One cube -- number 2 of that sample -- survived three runs
# of 400,000,000 at ceilings of 20, 23 and 26 and is not explained.
#
# So the practical question is what to set, and the honest answer needs a
# distribution rather than a single success count. If the cost per cube grows
# steeply, no fixed constant serves forever and the useful output is the shape
# of the curve plus a defensible cut-off. Time matters as much as nodes, and
# the two do not track each other: at the measured ceiling one cube took 188.6
# seconds for 55 million nodes while another took 84.1 seconds for 135 million.
# The prune table's state at the time explains that -- it is a singleton and
# survives between calls, so a run that follows a deep fill is cheaper per node
# than the run that paid for the fill.
#
# The defaults being measured against, from R/cube_kociemba.R:
#
#   node_budget  5e7      (the diagnostics had been passing 2e6, which is what
#                          made phase 3 look broken rather than starved)
#   max_depth3   14L      generators, not expanded moves. Cubes 1 and 3 above
#                          needed solutions of 28 and 30 expanded moves, and a
#                          wide half turn is one generator and four moves, so
#                          14 may be the tighter of the two limits.
#
# Both are varied here, because raising the budget under a ceiling that is
# itself too low would measure nothing.

suppressMessages(library(cayleyR))

N  <- 4L
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

n_states <- 10L
n_moves  <- 6L

# Ordered so each setting is a single change from the one before, which is what
# makes the difference between two rows attributable: first the ceiling alone,
# then the budget alone.
#
# Ten cubes and three settings because the first full-size attempt was not
# affordable -- one cube took 580 seconds on the default setting alone, which
# put four settings over thirty cubes out of reach. That 580 seconds is itself
# worth knowing: the same phase had solved 55 million nodes in 188 seconds in
# the previous script, and the difference is the prune table, a singleton that
# survives between calls. The first cube of a session pays to fill it and the
# rest inherit it, so per-cube times early in a run overstate the steady-state
# cost.
settings <- list(
  list(name = "default",   budget = 5e7, depth = 14L),
  list(name = "deeper",    budget = 5e7, depth = 20L),
  list(name = "deeper+4x", budget = 2e8, depth = 20L)
)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

solved <- seq_len(96L)

hr("setup")
cat(sprintf("cubes    : %d scrambles of %d quarter turns\n", n_states, n_moves))
cat("settings :\n")
for (s in settings) {
  cat(sprintf("  %-10s budget %13s  max_depth3 %d\n", s$name,
              format(s$budget, big.mark = ",", scientific = FALSE), s$depth))
}
cat("\nmax_depth3 counts GENERATORS; a wide half turn is one generator and\n")
cat("four expanded moves, so a limit of 14 admits solutions of about 20 moves.\n")

# The same thirty cubes for every setting, so rows are comparable.
set.seed(2026)
scrambles <- lapply(seq_len(n_states),
                    function(i) sample(names(mv), n_moves, replace = TRUE))

rows <- list()

for (s in settings) {
  hr(sprintf("%s -- budget %s, depth %d", s$name,
             format(s$budget, big.mark = ",", scientific = FALSE), s$depth))

  n_ok <- 0L
  cat(sprintf("  %4s %8s %7s %14s %9s\n",
              "cube", "result", "moves", "p3 nodes", "secs"))

  for (i in seq_len(n_states)) {
    state <- replay(solved, scrambles[[i]])

    t0  <- proc.time()[["elapsed"]]
    red <- try(cube_kociemba4_reduce(state, max_depth3 = s$depth,
                                     node_budget = s$budget)$path, silent = TRUE)
    el  <- proc.time()[["elapsed"]] - t0

    if (inherits(red, "try-error")) {
      ok <- FALSE; nmv <- NA_integer_; rep3 <- list(phase3 = "error",
                                                    phase3_nodes = NA_real_)
    } else {
      rep3 <- cube_kociemba4_report()
      ok   <- length(red) > 0 && cube_is_reduced(replay(state, red))
      nmv  <- if (ok) length(red) else NA_integer_
    }
    if (ok) n_ok <- n_ok + 1L

    # Every cube prints. At minutes per cube a filtered view is
    # indistinguishable from a hung process, and flush.console() keeps the line
    # from sitting in a buffer until the setting finishes.
    cat(sprintf("  %4d %8s %7s %14s %9.1f\n", i,
                if (ok) "reduced" else "FAILED",
                if (is.na(nmv)) "-" else nmv,
                format(rep3$phase3_nodes, big.mark = ",",
                       scientific = FALSE), el))
    flush.console()

    rows[[length(rows) + 1L]] <- data.frame(
      setting = s$name, budget = s$budget, depth = s$depth, cube = i,
      reduced = ok, moves = nmv, p3_outcome = rep3$phase3,
      p3_nodes = rep3$phase3_nodes, secs = el, stringsAsFactors = FALSE)
  }

  cat(sprintf("\n  reduced %d of %d\n", n_ok, n_states))
}

tab <- do.call(rbind, rows)

hr("by setting")

cat(sprintf("  %-10s %8s %10s %10s %12s %12s\n",
            "setting", "reduced", "mean secs", "max secs", "mean moves",
            "total secs"))
for (s in settings) {
  d <- tab[tab$setting == s$name, ]
  cat(sprintf("  %-10s %8s %10.1f %10.1f %12.0f %12.0f\n", s$name,
              sprintf("%d/%d", sum(d$reduced), nrow(d)),
              mean(d$secs), max(d$secs),
              mean(d$moves, na.rm = TRUE), sum(d$secs)))
}

hr("what phase 3 actually cost, on the cubes it solved")

# The distribution, not the mean: a fixed constant has to cover the tail, and
# the tail is what decides whether one exists.
best <- tab[tab$setting == settings[[length(settings)]]$name, ]
solved_n <- best[best$reduced, ]
if (nrow(solved_n) > 0) {
  q <- quantile(solved_n$p3_nodes, c(0.5, 0.75, 0.9, 1.0))
  cat(sprintf("  median   : %s nodes\n",
              format(round(q[[1]]), big.mark = ",", scientific = FALSE)))
  cat(sprintf("  75th     : %s\n",
              format(round(q[[2]]), big.mark = ",", scientific = FALSE)))
  cat(sprintf("  90th     : %s\n",
              format(round(q[[3]]), big.mark = ",", scientific = FALSE)))
  cat(sprintf("  worst    : %s\n",
              format(round(q[[4]]), big.mark = ",", scientific = FALSE)))
  cat(sprintf("\n  a budget of 5e7 covers  : %d of %d solved cubes\n",
              sum(solved_n$p3_nodes <= 5e7), nrow(solved_n)))
  cat(sprintf("  a budget of 2e8 covers  : %d of %d\n",
              sum(solved_n$p3_nodes <= 2e8), nrow(solved_n)))
}

hr("the stubborn ones")

# Cubes that no setting reduced. Cube 2 of the earlier sample took 1.2 billion
# nodes across three ceilings without deciding anything, and whether it is one
# of a kind or one of many decides if it is worth a reachability proof.
never <- tapply(tab$reduced, tab$cube, any)
hard  <- as.integer(names(never)[!never])
cat(sprintf("  reduced under no setting : %d of %d  (%s)\n",
            length(hard), n_states,
            if (length(hard) == 0) "-" else paste(hard, collapse = ", ")))

if (length(hard) > 0) {
  cat("\n  These are the candidates for a reachability proof -- a breadth-first\n")
  cat("  sweep from phase 3's goals, which answers where a search cannot.\n")
  cat("  If there is one, it is an outlier and probably not worth the cost;\n")
  cat("  several sharing a shape would be a defect worth chasing.\n")
} else {
  cat("\n  Every cube reduced under some setting, so the earlier stubborn cube\n")
  cat("  was specific to its scramble rather than a class of them.\n")
}

hr("what this says")
cat("  Pick the setting from the coverage table and the tail above, not from\n")
cat("  the success count alone: a budget that reduces one more cube while\n")
cat("  doubling the mean time may or may not be the trade wanted, and the\n")
cat("  distribution is what makes that a decision rather than a guess.\n")
