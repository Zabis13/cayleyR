# Which of phase 2's twelve goals actually get used, and does the reduction
# hold up on the ones that do.
#
# Eight of the twelve goal words were wrong: Uw2, Dw2 and Fw2 had the inner
# layer number transposed, so those goals named cubes twips does not mean.
# Fixing them changed nothing on the eight seeds in bench_kociemba4.R -- to the
# node. That looked like the fix had not taken, and it had: those seeds land on
# goals 1, 2, 3 and 6, and goals 1, 2 and 3 were the ones that were already
# right.
#
# So the fix needs a wider sample to show up at all. This script runs many
# scrambles, records which goal phase 2 stops on, and checks that the reduction
# still finishes from there. A goal that is never reached tells us the sample
# is too narrow; a goal that is reached and then fails to reduce is a goal that
# is still wrong.
#
# Run with:
#   Rscript inst/examples/bench_goals2_coverage.R

library(cayleyR)

N_SCRAMBLES <- 60
SCRAMBLE_MOVES <- 5
BUDGET <- 2e7

moves4 <- cube_moves(4)
names(moves4) <- cube_move_names(4)
apply_path <- function(state, path) {
  for (mv in path) state <- state[moves4[[mv]]]
  state
}

centre_stickers <- local({
  pieces <- cube_pieces(4)
  as.integer(vapply(strsplit(pieces$stickers[pieces$n_stickers == 1], ","),
                    `[`, "", 1))
})

# The classes phase 2's deriver reduces centres to, in this package's face
# order U R F D L B: opposite faces share a class.
CENTRE_CLASS <- c(0, 4, 8, 0, 4, 16)

goal_key <- function(state) {
  paste(CENTRE_CLASS[((state[centre_stickers] - 1L) %/% 16L) + 1L], collapse = ",")
}

GOALS <- c("", "y2", "Lw2", "Rw2", "Uw2", "Dw2",
           "Lw2 Fw2", "Rw2 Fw2", "Uw2 Fw2", "Dw2 Fw2",
           "Dw2 Fw2 Lw2", "Lw2 Fw2 Uw2")

expand_blocks <- function(spec) {
  if (!nzchar(spec)) return(character(0))
  unlist(lapply(strsplit(spec, " ")[[1]], cube_expand_word, n = 4),
         use.names = FALSE)
}

goal_keys <- vapply(GOALS,
                    function(g) goal_key(apply_path(cube_identity(4),
                                                    expand_blocks(g))), "")

cat(sprintf("%d goals, %d distinct coordinates\n\n",
            length(GOALS), length(unique(goal_keys))))

hits <- integer(length(GOALS))
reduced_from <- integer(length(GOALS))
failures <- 0L

for (i in seq_len(N_SCRAMBLES)) {
  set.seed(20000 + i)
  scramble <- generate_state(group = cube_group(4), n_moves = SCRAMBLE_MOVES)

  after2 <- apply_path(scramble,
                       cayleyR:::cube_kociemba4_phase12_cpp(scramble))
  which_goal <- match(goal_key(after2), goal_keys)

  if (is.na(which_goal)) {
    failures <- failures + 1L
    cat(sprintf("scramble %d: phase 2 stopped somewhere that is not a goal\n", i))
    next
  }
  hits[which_goal] <- hits[which_goal] + 1L

  # And from that goal, does the whole reduction still finish?
  path <- cube_kociemba4_reduce(scramble, node_budget = BUDGET)
  if (length(path) && cube_is_reduced(apply_path(scramble, path))) {
    reduced_from[which_goal] <- reduced_from[which_goal] + 1L
  }
  if (i %% 10 == 0) { cat(sprintf("  %d/%d\n", i, N_SCRAMBLES)); flush.console() }
}

cat("\ngoal                       reached  reduced\n")
for (g in seq_along(GOALS)) {
  cat(sprintf("%-2d %-22s %7d  %7d%s\n", g,
              if (nzchar(GOALS[g])) GOALS[g] else "(solved)",
              hits[g], reduced_from[g],
              if (hits[g] > 0 && reduced_from[g] < hits[g]) "   <-- some failed" else ""))
}

cat(sprintf("\nreached %d of %d goals; %d scrambles reduced of %d\n",
            sum(hits > 0), length(GOALS), sum(reduced_from), N_SCRAMBLES))
if (failures) {
  cat(sprintf("%d scrambles ended somewhere that is not a goal at all\n", failures))
}
cat("\nGoals never reached are untested, not proven right. Goals reached but\n")
cat("not reduced are the ones to look at: phase 2 stopped there and phase 3\n")
cat("could not carry on.\n")
