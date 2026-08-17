# Where the batch actually stops.
#
# bench_prefilled_solve.R solves cube 1 in about six seconds and then hangs
# before printing anything about cube 2. That is all the benchmark can say: its
# per-cube line is printed after the solve returns, so a solve that never
# returns prints nothing at all, and the stage breakdown is read out of the
# solver afterwards and never reached either.
#
# So this prints before each step rather than after it. Every line is flushed as
# it is written, which makes the last line on screen the step that is still
# running -- the one piece of information the benchmark cannot give.
#
# The tables are loaded exactly as the benchmark loads them, because a hang that
# only happens with the files loaded is a different hang from one that happens
# without them.

suppressMessages(library(cayleyR))

args     <- commandArgs(trailingOnly = TRUE)
which_cube <- if (length(args) >= 1) as.integer(args[[1]]) else 2L
n_moves  <- 20L
node_budget <- if (length(args) >= 2) as.numeric(args[[2]]) else 5e7

say <- function(...) { cat(..., "\n", sep = ""); flush.console() }

N  <- 4L
g  <- cube_group(N)
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

apply_path <- function(s, p) { for (m in p) s <- s[mv[[m]]]; s }

# ---- The same tables the benchmark uses ------------------------------------
table_dir <- "/mnt/Data2/DS_projects/phase3"
for (ph in 1:3) {
  cand <- Sys.glob(file.path(table_dir, sprintf("phase%d_d*.bin", ph)))
  if (length(cand) == 0) { say("  phase ", ph, ": no file"); next }
  d <- as.integer(sub("^.*_d([0-9]+)\\.bin$", "\\1", cand))
  cand <- cand[order(d, decreasing = TRUE)]
  ld <- cayleyR:::cube_kociemba4_load_phase_cpp(cand[1], ph)
  say("  phase ", ph, ": ", basename(cand[1]),
      if (isTRUE(ld$ok)) sprintf(" loaded, depth %d", ld$built_depth)
      else paste0(" REFUSED (", ld$reason, ")"))
}

# ---- The same cubes, from the same seed -------------------------------------
# generate_state() is called once per cube in the same order as the benchmark,
# so cube k here is cube k there.
set.seed(2026)
states <- lapply(seq_len(which_cube),
                 function(i) generate_state(group = g, n_moves = n_moves))
s <- states[[which_cube]]

say("\n== cube ", which_cube, ", node_budget ", format(node_budget,
                                                       scientific = FALSE))

# ---- The cascade's own steps, one at a time ---------------------------------
# This is cube_solve4_cascade() unrolled: the same calls in the same order, each
# announced before it runs. When the screen stops, the last line names the call
# that did not come back.
orientations <- c("", "1y", "1x", "1z")

for (rot in orientations) {
  rot_word <- if (nzchar(rot)) strsplit(trimws(rot), " +")[[1]] else character(0)
  turned <- apply_path(s, rot_word)
  tag <- if (nzchar(rot)) rot else "(-)"

  say("\n  orientation ", tag)

  say("    phases 1+2 ... running")
  t0 <- proc.time()[["elapsed"]]
  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(turned, upto_phase = 2L,
                                              node_budget = node_budget)
  say("    phases 1+2 : ", length(p12), " moves, ",
      sprintf("%.1f s", proc.time()[["elapsed"]] - t0))

  handed <- apply_path(turned, p12)
  p2ok <- isTRUE(cayleyR:::cube_at_phase_goal_cpp(handed, 2L))
  say("    phase 2 goal: ", if (p2ok) "reached" else "NOT reached")
  if (!p2ok) next

  say("    phase 3 ... running")
  t0 <- proc.time()[["elapsed"]]
  r3 <- cayleyR:::cube_kociemba4_phase3_cpp(handed, node_budget = node_budget,
                                            use_exact_centres = TRUE)
  say("    phase 3    : ", r3$outcome, ", ",
      format(r3$nodes, big.mark = ","), " nodes, ",
      sprintf("%.1f s", proc.time()[["elapsed"]] - t0))
  if (!isTRUE(r3$found)) next

  reduced <- apply_path(handed, r3$path)

  # The step the benchmark's stage table never names. It is outside reduce(),
  # so it has no node budget and no line of its own -- it lands in "rest",
  # which was 3.7 of cube 1's 6.2 seconds.
  say("    cube_solve4 (3x3x3 finish) ... running")
  t0 <- proc.time()[["elapsed"]]
  tail_solve <- cube_solve4(reduced)
  say("    cube_solve4: found = ", isTRUE(tail_solve$found), ", ",
      length(tail_solve$path), " moves, ",
      sprintf("%.1f s", proc.time()[["elapsed"]] - t0))
  break
}

say("\n  done")
