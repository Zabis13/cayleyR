# Is the PLL bit the quantity it claims to be?
#
# Two questions, asked separately, because the earlier diagnostics conflated
# them and the conflation cost a whole line of investigation.
#
#   (1) Does the bit respond to parity the way the puzzle does? A parity
#       algorithm is the one thing on a 4x4x4 that changes PLL parity and
#       nothing else a 3x3x3 can see. If the bit does not flip on it, the
#       formula is measuring something else.
#
#   (2) Do phase 3's goals all want the bit to be zero? diag_kociemba4_vs_solve4
#       prints "phase 3 goal wants 0" beside every cube. If the goal list is
#       split between the two values, that line is false and no state is ever
#       barred from phase 3 by its parity.
#
# Question (2) decides how to read every "exhausted" in the older output: a
# state with goals to aim at that ran out of budget is a search problem, and a
# state with none is a reachability problem. They need opposite fixes.

suppressMessages(library(cayleyR))

N  <- 4L
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

bit_of <- function(state) cayleyR:::cube_wing_parities_cpp(state)

solved <- seq_len(96L)

# ---- (1) the bit against a parity algorithm --------------------------------
#
# r2 U2 r2 Uw2 r2 u2 in this package's notation. Layers count from the L/D/B
# end, so the inner slice beside R is 2x and the one beside U is 2y; r is 2x,
# u is 2y, and Uw is U with 2y. There are no half turns in the move list, so
# each appears twice.
hr("a parity algorithm against the bit")

parity_alg <- c("2x", "2x", "U", "U", "2x", "2x",
                "U", "2y", "U", "2y", "2x", "2x", "2y", "2y")

# A second one, unrelated in shape, so a single lucky word cannot carry the
# conclusion: Rw U2 x Rw U2 Rw' U2 Rw U2 Rw' U2 Rw' -- written without the
# rotation by folding x into the wide turns it governs is error-prone, so the
# simpler well-known OLL-parity word is used instead: r U2 r U2 r' U2 r U2 r'.
parity_alg2 <- c("2x", "U", "U", "2x", "U", "U", "2x'", "U", "U",
                 "2x", "U", "U", "2x'")

show_state <- function(label, state) {
  p <- bit_of(state)
  cat(sprintf("  %-28s corner %d  dedge %d  PLL %d  prim-in-prim %d\n",
              label, p$corner_parity, p$dedge_parity, p$pll_bit,
              p$primary_in_primary))
}

show_state("solved", solved)

s1 <- replay(solved, parity_alg)
show_state("after r2 U2 r2 Uw2 r2 u2", s1)

s2 <- replay(s1, parity_alg)
show_state("after it twice", s2)

s3 <- replay(solved, parity_alg2)
show_state("after r U2 r U2 r' U2 r U2 r'", s3)

cat("\n  the bit flips on the first algorithm : ",
    if (bit_of(s1)$pll_bit != bit_of(solved)$pll_bit) "yes" else "NO", "\n", sep = "")
cat("  and returns when it is applied twice : ",
    if (bit_of(s2)$pll_bit == bit_of(solved)$pll_bit) "yes" else "NO", "\n", sep = "")

# A sanity contrast: an ordinary outer turn must not touch PLL parity on its
# own -- it moves corners and dedges together. If the bit flips here too, it is
# not PLL parity, it is just some parity.
hr("ordinary turns, which must not flip it")

for (m in c("U", "R", "F")) {
  s <- replay(solved, c(m, m))
  cat(sprintf("  %-4s twice : PLL %d\n", m, bit_of(s)$pll_bit))
}
for (m in c("U", "R", "F")) {
  s <- replay(solved, m)
  cat(sprintf("  %-4s once  : PLL %d\n", m, bit_of(s)$pll_bit))
}

# ---- (2) what phase 3's goals actually want --------------------------------
#
# Scramble a few cubes, run the reduction's own probe, and read the two numbers
# it reports beside the bit: the bit of the nearest goal, and how many of the
# twenty-four goals share the state's bit. The second is the one that matters --
# zero means no goal is reachable, anything else means there are goals to aim
# at and running out of budget was a search failure, not a parity wall.
hr("phase 3 goals, by their bit")

set.seed(2026)

probe <- cayleyR:::cube_phase3_coord_cpp

# The solved cube first, as the reference reading.
pr0 <- probe(solved)
cat(sprintf("  solved   PLL %d  nearest-goal bit %d  goals sharing it %d/24  prune %d\n",
            bit_of(solved)$pll_bit, pr0$goal_parity_bit,
            pr0$n_goals_matching_bit, pr0$prune_bound))

# And the two parity-algorithm states, which sit on the other side of the bit
# if question (1) came out as expected. If they still have goals to aim at,
# parity bars nothing.
for (nm in c("parity alg", "parity alg2")) {
  st <- if (nm == "parity alg") s1 else s3
  pr <- probe(st)
  cat(sprintf("  %-8s PLL %d  nearest-goal bit %d  goals sharing it %d/24  prune %d\n",
              nm, bit_of(st)$pll_bit, pr$goal_parity_bit,
              pr$n_goals_matching_bit, pr$prune_bound))
}

cat("\n")
for (i in 1:6) {
  scr <- sample(names(mv), 6L, replace = TRUE)
  st  <- replay(solved, scr)
  pr  <- probe(st)
  p   <- bit_of(st)
  cat(sprintf("  cube %d   PLL %d  nearest-goal bit %d  goals sharing it %d/24  prune %d\n",
              i, p$pll_bit, pr$goal_parity_bit, pr$n_goals_matching_bit,
              pr$prune_bound))
}

cat("\n  If the goals-sharing column is never zero, no state is barred from\n")
cat("  phase 3 by parity, and \"phase 3 goal wants 0\" in\n")
cat("  diag_kociemba4_vs_solve4.R is a false line: every \"exhausted\" in that\n")
cat("  output is a search that ran out of budget, not a goal out of reach.\n")
