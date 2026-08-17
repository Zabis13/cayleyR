# What does phase 3's coordinate do with the parity bit, and does the search
# plateau where the bit is wrong?
#
# The bit is corner_parity + dedge_parity, and it is NOT an invariant of phase
# 3: three of its seventeen generators -- the wide half turns Uw2, Rw2, Fw2 --
# flip it, which is exactly the `orientationDelta: [1]` twips gives them. So a
# cube whose bit differs from the goal's is not unreachable; it merely has to
# spend one of those three moves. The earlier suspicion that phase 3 might be
# hunting for something outside its own coset is closed: measured, it is not.
#
# What is left worth asking is narrower. If the bit is wrong, at least one of
# three specific moves is compulsory, and an admissible heuristic ought to see
# that. If the prune table does not, the search wanders -- and wandering is
# what the node counts look like: level 13 and level 14 cost about the same,
# where a healthy search grows by its branching factor each level.
#
# So this measures two things:
#
#   1. the bit after phases 1-2, against the goal's -- context, not a verdict;
#   2. how far the search actually gets, and whether the levels grow.
#
# A flat cost curve is the symptom to chase; the bit is only here to say
# whether it coincides with the bit being wrong.
#
# Run with:
#   Rscript inst/examples/diag_phase3_parity.R

library(cayleyR)

SEEDS <- c(8101, 8102, 8103, 8104)
MOVES <- 10

moves4 <- cube_moves(4)
names(moves4) <- cube_move_names(4)
apply_path <- function(state, path) {
  for (mv in path) state <- state[moves4[[mv]]]
  state
}

pieces  <- cube_pieces(4)
wing_st <- lapply(strsplit(pieces$stickers[pieces$n_stickers == 2], ","), as.integer)
corn_st <- lapply(strsplit(pieces$stickers[pieces$n_stickers == 3], ","), as.integer)

owner <- integer(96)
for (i in seq_along(wing_st)) for (s in wing_st[[i]]) owner[s] <- i

geometry <- cayleyR:::cube_wing_geometry_cpp()
is_primary <- as.logical(geometry$primary)
dedge_of   <- as.integer(geometry$dedge)

permutation_parity <- function(v) {
  v <- as.integer(v)
  seen <- rep(FALSE, length(v))
  parity <- 0L
  for (i in seq_along(v)) {
    if (seen[i]) next
    j <- i; len <- 0L
    while (!seen[j]) { seen[j] <- TRUE; j <- v[j]; len <- len + 1L }
    parity <- parity + (len - 1L)
  }
  parity %% 2L
}

wing_perm <- function(state) vapply(wing_st, function(v) owner[state[v[1]]], 0L)

# A corner is identified by the set of face colours it carries, which is what
# survives the piece being turned in place.
corner_perm <- function(state) {
  vapply(corn_st, function(v) {
    faces <- sort((state[v] - 1) %/% 16)
    for (k in seq_along(corn_st)) {
      if (identical(sort((corn_st[[k]] - 1) %/% 16), faces)) return(k)
    }
    NA_integer_
  }, 0L)
}

# One wing per dedge -- the one in a primary position -- so the twelve dedges
# are counted once each, as phase 3's deriver does.
dedge_perm <- function(state) dedge_of[wing_perm(state)[is_primary]] + 1L

parity_bit <- function(state) {
  (permutation_parity(corner_perm(state)) +
   permutation_parity(dedge_perm(state))) %% 2L
}

goal_bit <- parity_bit(cube_identity(4))
cat(sprintf("goal parity bit: %d\n", goal_bit))
cat("(three generators flip it: Uw2, Rw2, Fw2 -- so a mismatch costs a move,\n")
cat(" it does not make the goal unreachable)\n\n")

for (seed in SEEDS) {
  set.seed(seed)
  scramble <- generate_state(group = cube_group(4), n_moves = MOVES)

  phase12 <- cayleyR:::cube_kociemba4_phase12_cpp(scramble)
  handed_over <- apply_path(scramble, phase12)
  bit <- parity_bit(handed_over)

  cat(sprintf("seed %d: after phases 1-2, %d moves, parity bit %d %s\n",
              seed, length(phase12), bit,
              if (bit == goal_bit) "(matches goal)" else "(differs -- one flip owed)"))

  # How the levels grow. A search that is pruning properly costs more at each
  # level by roughly its branching factor; one that has stopped pruning costs
  # about the same each time, which is what was seen at levels 13 and 14.
  previous <- 0
  for (depth in 10:13) {
    cube_kociemba4_reduce(handed_over, max_depth3 = depth, node_budget = 3e7)
    nodes <- cube_kociemba4_report()$phase3_nodes
    level <- nodes - previous
    cat(sprintf("    depth %2d: %10d nodes this level%s\n", depth, level,
                if (previous > 0) sprintf(", ratio %.1f", level / previous) else ""))
    previous <- nodes
    if (cube_kociemba4_report()$phase3 == "exhausted") {
      cat("    (budget reached; deeper levels not measured)\n")
      break
    }
  }
  cat("\n")
}

cat("Read the ratios, not the bit. Levels that grow by a steady factor mean\n")
cat("the table is still pruning; levels that cost the same mean it has\n")
cat("stopped, and that is where the phase actually fails.\n")
