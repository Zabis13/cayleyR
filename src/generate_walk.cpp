// Random walks away from the solved state, reported with the word they walked.
//
// generate_state() in R produces a state and forgets how it got there, which is
// all a caller needs when the state is the subject. It is not enough when the
// walk itself is the subject: a walk that reports its word also reports a way
// home, since inverting the word solves the state. Anything measuring a solver
// against a reference path -- what rank did the estimator give the helping
// move, at which step did the search drop the trail -- needs that word.
//
// It also fixes what generate_state cannot say honestly. That one draws its
// word uniformly with replacement, so a pair like U U' can land next to itself
// and n_moves becomes an upper bound on the distance rather than the distance.
// Here that pair is refused, so the label means more, though it is still an
// upper bound: a shorter route may exist that the walk did not take.
//
// This lives apart from cube_adi.cpp deliberately. That file is the training
// loop's hot path and its scrambler is shaped for it -- depth drawn at random,
// word discarded, nothing returned that ADI does not read. This is a general
// generator for any group the package can build, and the two should be free to
// change without each other.

#include <Rcpp.h>
#include "perm_group.h"
using namespace Rcpp;

//' Random Walks From the Solved State, With the Moves They Took
//'
//' Walks away from the identity and reports the word it walked, which is what
//' separates this from \code{\link{generate_state}} and from the ADI
//' scrambler. Knowing the word means knowing a way home --- inverting it solves
//' the state --- and anything that measures a solver against a reference path
//' needs exactly that.
//'
//' Two switches cover what the callers want, because the two uses pull opposite
//' ways:
//'
//' \describe{
//'   \item{\code{exact}}{\code{TRUE} walks \code{n_moves} every time.
//'     \code{FALSE} draws a length uniformly from 1 to \code{n_moves}, which is
//'     the sampling training asks for: it spreads accuracy outward from the
//'     goal without any weighting in the loss. Measurement wants the opposite
//'     --- a fixed depth, so that a result is about that depth and not about a
//'     mixture dominated by states a move or two from solved.}
//'   \item{\code{no_undo}}{\code{TRUE} refuses a move that undoes the one
//'     before it. Such a pair returns the state to where it was and makes the
//'     walk shorter than its label. Longer cycles are left alone: on the cube
//'     four quarter turns of one face also come home, and refusing every way
//'     back would mean searching the group rather than walking it.}
//' }
//'
//' Even with \code{no_undo} the length is an upper bound on the distance to
//' solved, never a promise. Callers comparing a solver against the returned
//' word should read it as "no worse than this", which is the safe direction:
//' the solver is never credited with a shortcut the reference did not have.
//'
//' The walk is uniform over the alphabet, so a group given generators without
//' their inverses is walked with what it has; \code{no_undo} then has nothing
//' to refuse at those steps and allows them.
//'
//' @param group External pointer to a \code{perm_group}
//' @param n Number of walks to generate
//' @param n_moves Length of each walk, or the longest one when
//'   \code{exact = FALSE}
//' @param exact Walk \code{n_moves} exactly (default), or draw the length
//'   uniformly from 1 to \code{n_moves}
//' @param no_undo Refuse a move that immediately undoes the previous one
//'   (default \code{TRUE})
//' @return List with \code{states} (n x state_len integer matrix, one walk
//'   endpoint per row), \code{depth} (integer vector, the length of each walk)
//'   and \code{moves} (n x n_moves integer matrix of 1-based move indices, the
//'   word each row walked, padded with \code{NA} past that row's depth)
//' @keywords internal
// [[Rcpp::export]]
List generate_walk_cpp(SEXP group, int n, int n_moves,
                       bool exact = true, bool no_undo = true) {
  XPtr<PermGroup> g(group);
  if (n <= 0) stop("n must be positive");
  if (n_moves <= 0) stop("n_moves must be positive");

  const int state_len = g->state_length();
  const int n_alpha   = g->n_moves();
  if (n_alpha <= 0) stop("group has no moves");

  IntegerMatrix states(n, state_len);
  IntegerVector depth(n);
  // Ragged in principle -- with exact = FALSE the rows differ in length -- but
  // returned as one matrix padded with NA, because that is the shape R reads
  // back cheaply. n_moves is the longest any row can be either way.
  IntegerMatrix words(n, n_moves);
  std::fill(words.begin(), words.end(), NA_INTEGER);

  std::vector<int> cur;

  for (int i = 0; i < n; ++i) {
    cur = g->identity();

    int d = n_moves;
    if (!exact) {
      d = (int)(unif_rand() * n_moves) + 1;
      if (d > n_moves) d = n_moves;          // guards unif_rand() == 1
    }

    int last = -1;
    for (int step = 0; step < d; ++step) {
      int a;
      // When the alphabet does not name the inverse of the last move there is
      // nothing to refuse, and the loop must not spin looking for it. Nor may
      // it refuse the only move there is: a one-move group would never satisfy
      // the condition and the draw would not terminate.
      const int banned = (no_undo && last >= 0 && n_alpha > 1)
                         ? g->inverse_move(last) : -1;
      do {
        a = (int)(unif_rand() * n_alpha);
        if (a >= n_alpha) a = n_alpha - 1;
      } while (banned >= 0 && a == banned);
      g->apply(cur, a);
      words(i, step) = a + 1;                // 1-based, to index g$moves in R
      last = a;
    }

    for (int j = 0; j < state_len; ++j) states(i, j) = cur[j];
    depth[i] = d;
  }

  return List::create(_["states"] = states, _["depth"] = depth,
                      _["moves"]  = words);
}
