#!/usr/bin/env Rscript
# Where the four-phase 4x4x4 search spends its time.
#
# demo_cube4_solve.R runs two methods side by side and reports one number each:
# seconds. That is enough to see that `kociemba` is slow and not enough to see
# why. This script takes the same method apart and times the pieces.
#
# cube_kociemba4() is four steps, and only the first three live in C++:
#
#   phase 1-3   cube_kociemba4_reduce() -- centres, then wings, then the pairing
#   squeeze     the reduced 96-sticker cube read as a 54-sticker 3x3x3
#   phase 4     cube_kociemba() on that, the ordinary two-phase solver
#   lift        its answer written back in 4x4x4 moves
#
# Each is timed separately here, and the two solvers' own reports supply the
# node counts, so a slow run can be read as "phase 3 visited 40M nodes" rather
# than "it took a minute". With progress_every set, the reduction also prints a
# line per depth of iterative deepening -- that is the shape worth watching:
# nodes per depth growing by a constant factor means the prune table is not
# cutting anything, and the depth where the growth starts is where to look.
#
# Short scrambles on purpose. A cube 20 moves from solved is the one that hangs;
# starting at 2 and walking up finds the length where the cost turns over, which
# is the thing to know before touching the search itself.
#
# Run with:  Rscript inst/examples/prof_cube4_kociemba.R
#            Rscript inst/examples/prof_cube4_kociemba.R 12      # up to 12 moves
#            Rscript inst/examples/prof_cube4_kociemba.R 12 3    # 3 per length

library(cayleyR)

args        <- commandArgs(trailingOnly = TRUE)
max_scramble <- if (length(args) >= 1) as.integer(args[[1]]) else 20L
per_length   <- if (length(args) >= 2) as.integer(args[[2]]) else 2L
step_scramble <- if (length(args) >= 3) as.integer(args[[3]]) else 4L

N            <- 4L
node_budget  <- 2e7      # the same cap demo_cube4_solve.R uses
progress_every <- 2e6    # a progress line every this many nodes; 0 turns it off

set.seed(2026)

g  <- cube_group(N)
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

secs <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  value <- force(expr)
  list(value = value, seconds = proc.time()[["elapsed"]] - t0)
}

# The state as it is stored: 96 position numbers on one line. Nothing is
# folded into faces or turned into colours -- what is printed is the vector the
# solver was handed, so a run can be replayed from the log by pasting it back.
show_state <- function(label, state) {
  cat("    ", label, ": ", paste(state, collapse = " "), "\n", sep = "")
}

# cube_kociemba4() opened up. The steps are the same ones and in the same
# order -- this is not a second implementation, it is the same calls with a
# clock between them, so a total here is comparable with a total there.
profile_one <- function(state, verbose) {
  show_state("scrambled ", state)

  # Phases 1-3. progress_every makes the C++ side print as it deepens; the
  # timing is unaffected by it beyond the cost of the printing itself.
  red <- secs(cube_kociemba4_reduce(state, node_budget = node_budget,
                                    progress_every = if (verbose) progress_every
                                                     else 0)$path)
  rep3 <- cube_kociemba4_report()

  reduced <- length(red$value) > 0 || cube_is_reduced(state)
  if (!reduced) {
    return(list(ok = FALSE, where = "reduction", reduce = red, report = rep3,
                squeeze = NULL, phase4 = NULL, lift = NULL,
                path = character(0)))
  }

  cur <- replay(state, red$value)
  show_state("reduced   ", cur)

  sq <- secs(cube_colour_state(cayleyR:::cube_squeeze_cpp(cur), 3))
  p4 <- secs(cube_kociemba(sq$value)$path)
  rep4 <- cube_kociemba_report()

  solved3 <- length(p4$value) > 0 || identical(sq$value, cube_identity(3))
  if (!solved3) {
    return(list(ok = FALSE, where = "phase 4", reduce = red, report = rep3,
                squeeze = sq, phase4 = p4, report4 = rep4, lift = NULL,
                path = red$value))
  }

  lf <- secs(cayleyR:::cube_lift_path_cpp(p4$value)$path)
  cur <- replay(cur, lf$value)
  show_state("solved    ", cur)

  list(ok = cube_is_colour_solved(cur), where = "",
       reduce = red, report = rep3, squeeze = sq, phase4 = p4, report4 = rep4,
       lift = lf, path = c(red$value, lf$value))
}

hr("setup")
cat("scramble lengths : 2 to ", max_scramble, ", ", per_length,
    " state(s) each\n", sep = "")
cat("node budget      : ", format(node_budget, scientific = FALSE, big.mark = ","),
    " per phase\n", sep = "")
cat("progress lines   : every ",
    format(progress_every, scientific = FALSE, big.mark = ","),
    " nodes, on the first state of each length\n", sep = "")

hr("warming the tables")

# The three prune tables are built on the first call to the reduction and kept
# for the rest of the session -- Solver4::init() runs once, behind a `ready`
# flag. So the first solve of a run is billed for the tables as well as for
# itself, which is why in demo_cube4_solve.R state 1 takes 122 s and every
# state after it takes 12. Building them here, on a solved cube that no phase
# has to search, separates the two: everything the table below reports is
# search time on warm tables.
warm <- secs(cube_kociemba4_reduce(cube_identity(N), node_budget = node_budget)$path)
cat(sprintf("  %.2f s to build the phase 1-3 prune tables (once per session)\n",
            warm$seconds))
cat("  every timing below is on warm tables\n")

rows <- list()

for (len in seq(2L, max_scramble, by = step_scramble)) {
  hr(paste("scramble of", len, "quarter turns"))

  for (k in seq_len(per_length)) {
    state <- generate_state(group = g, n_moves = len)

    # Only the first state of each length prints its deepening log. Every state
    # printing it would bury the table; one is enough to see the shape.
    verbose <- (k == 1L)
    cat("\n  state ", k, ":\n", sep = "")

    p <- profile_one(state, verbose)

    r3 <- p$report
    total <- p$reduce$seconds +
      (if (is.null(p$squeeze)) 0 else p$squeeze$seconds) +
      (if (is.null(p$phase4))  0 else p$phase4$seconds) +
      (if (is.null(p$lift))    0 else p$lift$seconds)

    cat(sprintf("\n  state %d  --  %s  (%.2f s total, %d moves)\n",
                k, if (p$ok) "solved" else paste("STOPPED at", p$where),
                total, length(p$path)))

    # Phases 1-3: seconds are the reduction's as a whole -- the C++ side does
    # not clock them apart -- but the nodes and the outcome are per phase, and
    # those are what say which phase is the expensive one.
    cat(sprintf("    %-12s %9s  %14s  %s\n",
                "step", "seconds", "nodes", "outcome"))
    cat(sprintf("    %-12s %9.2f  %14s  %s\n", "phases 1-3",
                p$reduce$seconds,
                format(r3$phase1_nodes + r3$phase2_nodes + r3$phase3_nodes,
                       scientific = FALSE, big.mark = ","),
                paste(r3$phase1, r3$phase2, r3$phase3, sep = "/")))
    for (ph in 1:3) {
      cat(sprintf("      %-10s %9s  %14s  %s\n", paste0("phase ", ph), "",
                  format(r3[[paste0("phase", ph, "_nodes")]],
                         scientific = FALSE, big.mark = ","),
                  r3[[paste0("phase", ph)]]))
    }

    if (!is.null(p$squeeze)) {
      cat(sprintf("    %-12s %9.2f\n", "squeeze", p$squeeze$seconds))
    }
    if (!is.null(p$phase4)) {
      r4 <- p$report4
      cat(sprintf("    %-12s %9.2f  %14s  %s\n", "phase 4",
                  p$phase4$seconds,
                  format(r4$phase1_nodes + r4$phase2_nodes,
                         scientific = FALSE, big.mark = ","),
                  paste(r4$phase1, r4$phase2, sep = "/")))
    }
    if (!is.null(p$lift)) {
      cat(sprintf("    %-12s %9.2f\n", "lift", p$lift$seconds))
    }

    # The one line worth reading when a solve fails. "unsolved" in
    # demo_cube4_solve.R does not say which phase stopped or why, and the two
    # reasons want opposite repairs:
    #
    #   exhausted    the phase spent its whole node budget and gave up. There
    #                may well be a solution just past where it stopped -- raise
    #                node_budget, or make the phase cheaper per node.
    #   no_solution  the phase searched its whole tree to max_depth and there
    #                was nothing there. A bigger budget changes nothing; raise
    #                max_depth -- or, if the depth is already generous, the
    #                phase's goal or coordinate is wrong.
    #
    # A phase that never ran reports no_solution with zero nodes, which is
    # neither of the above; the node count is what tells them apart.
    if (!p$ok) {
      stop_ph <- NA_integer_
      for (ph in 1:3) {
        if (r3[[paste0("phase", ph)]] != "found") { stop_ph <- ph; break }
      }
      if (!is.na(stop_ph)) {
        why <- r3[[paste0("phase", stop_ph)]]
        n   <- r3[[paste0("phase", stop_ph, "_nodes")]]
        cat(sprintf("    -> phase %d stopped: %s after %s nodes -- %s\n",
                    stop_ph, why, format(n, scientific = FALSE, big.mark = ","),
                    if (why == "exhausted")
                      "raise node_budget, or cut the cost per node"
                    else if (n == 0)
                      "the phase never ran"
                    else
                      "raise max_depth; if it is already deep, the goal or coordinate is wrong"))
      } else {
        cat("    -> phases 1-3 all found; the 3x3x3 phase is what stopped\n")
      }
    }
    flush.console()

    rows[[length(rows) + 1L]] <- data.frame(
      scramble = len, state = k,
      status   = if (p$ok) "solved" else p$where,
      moves    = length(p$path),
      total_s  = round(total, 2),
      reduce_s = round(p$reduce$seconds, 2),
      phase4_s = if (is.null(p$phase4)) NA_real_ else round(p$phase4$seconds, 2),
      p1_nodes = r3$phase1_nodes,
      p2_nodes = r3$phase2_nodes,
      p3_nodes = r3$phase3_nodes,
      stringsAsFactors = FALSE)
  }
}

tab <- do.call(rbind, rows)

hr("every run")
print(tab, row.names = FALSE)

hr("by scramble length")

# Averaged over the states of each length. What to read here is the shape of
# total_s down the column: a cost that climbs smoothly is a search doing more
# work for a harder cube, and one that jumps is a search falling off a table.
by_len <- do.call(rbind, lapply(split(tab, tab$scramble), function(d) {
  data.frame(
    scramble  = d$scramble[1],
    solved    = sprintf("%d/%d", sum(d$status == "solved"), nrow(d)),
    mean_s    = round(mean(d$total_s), 2),
    max_s     = round(max(d$total_s), 2),
    reduce_s  = round(mean(d$reduce_s), 2),
    phase4_s  = if (all(is.na(d$phase4_s))) NA_real_
                else round(mean(d$phase4_s, na.rm = TRUE), 2),
    p3_nodes  = round(mean(d$p3_nodes)),
    stringsAsFactors = FALSE)
}))
print(by_len, row.names = FALSE)

hr("where the time goes")

# Summed over everything that ran, failures included: a phase that spends its
# whole budget and gives up is exactly the time worth accounting for.
tot_reduce <- sum(tab$reduce_s)
tot_p4     <- sum(tab$phase4_s, na.rm = TRUE)
tot_all    <- sum(tab$total_s)

cat(sprintf("  %-12s %8.2f s  %5.1f%%\n", "phases 1-3", tot_reduce,
            100 * tot_reduce / tot_all))
cat(sprintf("  %-12s %8.2f s  %5.1f%%\n", "phase 4", tot_p4,
            100 * tot_p4 / tot_all))
cat(sprintf("  %-12s %8.2f s\n", "total", tot_all))

n1 <- sum(tab$p1_nodes); n2 <- sum(tab$p2_nodes); n3 <- sum(tab$p3_nodes)
n_all <- n1 + n2 + n3
cat("\n  nodes visited in phases 1-3:\n")
for (i in 1:3) {
  n <- c(n1, n2, n3)[i]
  cat(sprintf("    phase %d %16s  %5.1f%%\n", i,
              format(n, scientific = FALSE, big.mark = ","),
              if (n_all > 0) 100 * n / n_all else 0))
}
if (tot_reduce > 0) {
  cat(sprintf("\n  reduction throughput: %s nodes/second\n",
              format(round(n_all / tot_reduce), scientific = FALSE,
                     big.mark = ",")))
}

stalled <- tab[tab$status != "solved", ]
if (nrow(stalled) > 0) {
  hr("runs that did not finish")
  print(stalled[, c("scramble", "state", "status", "total_s",
                    "p1_nodes", "p2_nodes", "p3_nodes")], row.names = FALSE)
  cat("\n\"exhausted\" is a budget to raise; \"no_solution\" is a depth to raise.\n")
}
