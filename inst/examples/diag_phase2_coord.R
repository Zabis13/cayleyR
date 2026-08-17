# Is phase 2's coordinate too coarse?
#
# diag_phase2_short.R turned up something its own headline hid. Phase 2 solved
# all twenty-eight of its generators, but only FOUR of them -- 1z, 1z', 2z, 2z'
# -- took the solved cube off phase 2's goal at all. The other twenty-four,
# every outer turn and every inner half turn, left it "already at goal".
#
# That has two possible readings, and they call for opposite conclusions:
#
#   (a) the goal is a SET, as phase 3's is -- twenty-four goals there -- and
#       those moves carry one goal to another. Harmless: phase 3 behaved
#       exactly this way, fourteen of its seventeen generators solved in zero
#       moves, and it is sound.
#
#   (b) the coordinate cannot see those moves at all. Then phase 2 declares
#       victory on states that differ in ways it never measured, and hands
#       phase 3 whatever those twenty-four moves happened to do. That would be
#       the defect the whole investigation has been looking for.
#
# The goal-set reading is checked first, by counting. Then the two are told
# apart on a state that is NOT a goal: if a move is a permutation of the goal
# set (a), it still changes a non-goal state and the phase needs moves to
# recover; if the coordinate is blind to it (b), applying it changes nothing
# the phase can measure, and phase 2 solves the state in the same number of
# moves with or without it.
#
# This is the same trap the PLL bit set earlier in the day: a state compared
# against ONE goal looks unreachable when the goal is really a set. Counting
# first is what stops that mistake being made twice.

suppressMessages(library(cayleyR))

N  <- 4L
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

solved <- seq_len(96L)
budget <- 5e6

at2 <- function(s) cayleyR:::cube_at_phase_goal_cpp(s, 2L)
at1 <- function(s) cayleyR:::cube_at_phase_goal_cpp(s, 1L)

solve2 <- function(state, max_depth2 = 14L) {
  p <- try(cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 2L,
                                                max_depth2 = max_depth2,
                                                node_budget = budget),
           silent = TRUE)
  if (inherits(p, "try-error")) return(list(ok = FALSE, n = NA_integer_))
  out <- replay(state, p)
  list(ok = at2(out), n = length(p), state = out, path = p)
}

# ---- How many goals has each phase? ----------------------------------------
#
# distinct_keys below goals is the coordinate merging goals it cannot tell
# apart -- intended, and what makes the search cheap. The number to read here
# is goals: one goal makes reading (a) impossible.
hr("goals per phase")

cat(sprintf("  %-7s %7s  %s\n", "phase", "goals", "distinct keys"))
for (ph in 1:3) {
  k <- cayleyR:::cube_phase_goal_keys_cpp(ph)
  cat(sprintf("  %-7d %7d  %d\n", ph, k$goals, k$distinct_keys))
}

# ---- Which generators move the solved cube off the goal? -------------------
hr("phase 2 generators against the solved cube")

gens <- list(
  "U"  = c("U"),  "U'" = c("U'"), "U2" = c("U","U"),
  "D"  = c("D"),  "D'" = c("D'"), "D2" = c("D","D"),
  "L"  = c("L"),  "L'" = c("L'"), "L2" = c("L","L"),
  "R"  = c("R"),  "R'" = c("R'"), "R2" = c("R","R"),
  "F"  = c("F"),  "F'" = c("F'"), "F2" = c("F","F"),
  "B"  = c("B"),  "B'" = c("B'"), "B2" = c("B","B"),
  "1x2" = c("1x","1x"), "2x2" = c("2x","2x"),
  "1y2" = c("1y","1y"), "2y2" = c("2y","2y"),
  "1z"  = c("1z"), "1z'" = c("1z'"), "1z2" = c("1z","1z"),
  "2z"  = c("2z"), "2z'" = c("2z'"), "2z2" = c("2z","2z"))

leaves <- vapply(names(gens), function(nm) !at2(replay(solved, gens[[nm]])),
                 logical(1))
cat(sprintf("  leave the goal : %s\n",
            paste(names(gens)[leaves], collapse = ", ")))
cat(sprintf("  stay at goal   : %s\n",
            paste(names(gens)[!leaves], collapse = ", ")))

# ---- The test that separates (a) from (b) ----------------------------------
#
# Take a state phase 2 has real work to do on, and apply each "invisible"
# generator to it. Under (a) the move genuinely rearranges the cube and the
# work changes; under (b) the phase sees the same coordinate either way.
#
# Solution length is the measure. It is not a perfect one -- a search can find
# different words of equal length -- so the resulting STATE is compared too:
# identical phase 2 output from two different inputs is the strongest evidence
# the coordinate never saw the difference.
hr("the same generators on a state that is not a goal")

set.seed(2026)

# A scramble phase 2 must actually work on. Drawing four moves at random from
# the four that shift the coordinate is not enough: a word like 1z 1z' cancels
# itself and leaves the cube at the goal, which is what happened on the first
# run of this script -- the base sat at the goal, phase 2 solved it in zero
# moves, and the table below compared zero against zero on every row. Draw
# until the state is genuinely off-goal, and say so if it never is.
base <- NULL
for (attempt in 1:200) {
  w <- unlist(gens[sample(c("1z","1z'","2z","2z'"), 6L, replace = TRUE)],
              use.names = FALSE)
  cand <- replay(solved, w)
  if (!at2(cand)) { base <- cand; base_word <- w; break }
}
if (is.null(base)) {
  stop("no off-goal base found in 200 draws -- the four generators that ",
       "appeared to move the coordinate may not actually do so")
}

cat(sprintf("  base state: off phase 2 goal = %s, at phase 1 goal = %s\n",
            !at2(base), at1(base)))

rb <- solve2(base)
cat(sprintf("  phase 2 on the base alone : %d moves, goal %s\n", rb$n, rb$ok))

# Without real work on the base there is nothing for the rows below to differ
# in, and a table of zeroes reads like a result while saying nothing.
if (rb$n == 0) {
  stop("phase 2 solved the base in zero moves -- the comparison below would ",
       "be vacuous")
}

cat(sprintf("\n  %-5s %10s %10s  %s\n", "move", "base len", "with mv",
            "same output state"))

same_state <- 0L; tested <- 0L
for (nm in names(gens)[!leaves]) {
  st <- replay(base, gens[[nm]])
  r  <- solve2(st)
  ident <- identical(as.integer(r$state), as.integer(rb$state))
  same_state <- same_state + as.integer(ident)
  tested <- tested + 1L
  cat(sprintf("  %-5s %10d %10s  %s\n", nm, rb$n,
              if (is.na(r$n)) "-" else as.character(r$n),
              if (ident) "yes" else "no"))
}

hr("what this says")

cat(sprintf("  invisible generators tested         : %d\n", tested))
cat(sprintf("  produced phase 2's identical output : %d\n", same_state))

cat("\n")
if (same_state == tested) {
  cat("  Every one identical: the coordinate is blind to those moves. Phase 2\n")
  cat("  cannot distinguish states differing by them, so whatever they did to\n")
  cat("  the wings and centres reaches phase 3 unmeasured, and cubes phase 2\n")
  cat("  calls equally finished sit at widely different distances in phase 3's\n")
  cat("  metric. That is the shape of the failures in\n")
  cat("  diag_kociemba4_vs_solve4.R, and it would be the defect.\n")
} else if (same_state == 0) {
  cat("  None identical: every move changed what phase 2 had to do, so the\n")
  cat("  coordinate does see them. With twelve goals reported above, staying\n")
  cat("  at the goal on the solved cube means those moves permute the goal set\n")
  cat("  -- the harmless reading, and the one phase 3 already showed with\n")
  cat("  fourteen of its seventeen generators.\n")
} else {
  cat("  A split result, which neither reading predicts. The moves that came\n")
  cat("  back identical are the ones to look at; the coordinate sees the rest.\n")
}
