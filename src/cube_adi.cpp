// Autodidactic iteration: the parts that have to be fast.
//
// ADI trains a network on states it generates itself, and two steps of that
// loop are hot enough to belong in C++.
//
// The first is walking away from the solved cube. A scramble of length 1..K,
// done a few hundred thousand times per iteration, is nothing but permutation
// lookups; generate_state() does one state per call from R, which is the wrong
// shape for a batch this size, and it neither draws its depth at random nor
// reports it back.
//
// The second is expanding children. Every state in a batch has one child per
// move, and the ADI target is read off their values. The expansion returns all
// of them as a single matrix, state-major -- row (i-1)*n_moves + a is child a
// of state i -- because that is what the network scores in one pass.
//
// Everything here goes through PermGroup, so it is not 4x4x4-specific: the
// same three functions work for any group the package can build, cubes of
// other sizes included.

#include <Rcpp.h>
#include "perm_group.h"
using namespace Rcpp;

//' Random Scrambles From the Solved State
//'
//' Walks away from the identity by a uniformly random number of moves between
//' 1 and \code{max_depth}, \code{n} times over. Uniform in depth is the
//' sampling ADI asks for: it is what lets accuracy spread outward from the
//' states near the goal without any weighting in the loss.
//'
//' The walk never undoes its own last move. A move immediately followed by its
//' inverse returns the state to where it was and would quietly make the
//' scramble shorter than its label. Longer cycles are left alone -- they are
//' rarer, and ADI's targets do not depend on the label being exact anyway.
//'
//' @param group External pointer to a \code{perm_group}
//' @param n Number of states to generate
//' @param max_depth Longest scramble
//' @return List with \code{states} (n x state_len integer matrix) and
//'   \code{depth} (integer vector, the scramble length of each row)
//' @keywords internal
// [[Rcpp::export]]
List cube_adi_scramble(SEXP group, int n, int max_depth) {
  XPtr<PermGroup> g(group);
  if (n <= 0) stop("n must be positive");
  if (max_depth <= 0) stop("max_depth must be positive");

  const int state_len = g->state_length();
  const int n_moves   = g->n_moves();
  if (n_moves <= 0) stop("group has no moves");

  IntegerMatrix states(n, state_len);
  IntegerVector depth(n);
  std::vector<int> cur;

  for (int i = 0; i < n; ++i) {
    cur = g->identity();
    int d = (int)(unif_rand() * max_depth) + 1;
    if (d > max_depth) d = max_depth;          // guards unif_rand() == 1
    int last = -1;
    for (int step = 0; step < d; ++step) {
      int a;
      do {
        a = (int)(unif_rand() * n_moves);
        if (a >= n_moves) a = n_moves - 1;
      } while (last >= 0 && a == g->inverse_move(last));
      g->apply(cur, a);
      last = a;
    }
    for (int j = 0; j < state_len; ++j) states(i, j) = cur[j];
    depth[i] = d;
  }
  return List::create(_["states"] = states, _["depth"] = depth);
}

//' All Children of Each State
//'
//' Applies every move to every state. The result is one matrix of
//' \code{nrow(states) * n_moves} children, laid out state-major so that row
//' \code{(i - 1) * n_moves + a} is child \code{a} of state \code{i}. That is
//' the layout the value network scores in a single pass, and the layout
//' \code{cube_adi_targets} reads back.
//'
//' @param group External pointer to a \code{perm_group}
//' @param states Integer matrix, one state per row
//' @return List with \code{children} (integer matrix, state-major) and
//'   \code{solved} (logical vector, whether each child is the identity)
//' @keywords internal
// [[Rcpp::export]]
List cube_adi_children(SEXP group, IntegerMatrix states) {
  XPtr<PermGroup> g(group);
  const int state_len = g->state_length();
  const int n_moves   = g->n_moves();
  if (states.ncol() != state_len)
    stop("states have %d columns but the group works on length %d",
         (int)states.ncol(), state_len);

  const int n = states.nrow();
  IntegerMatrix out((R_xlen_t)n * n_moves, state_len);
  LogicalVector solved((R_xlen_t)n * n_moves);
  std::vector<int> base(state_len), cur(state_len);

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < state_len; ++j) base[j] = states(i, j);
    for (int a = 0; a < n_moves; ++a) {
      cur = base;
      g->apply(cur, a);
      const R_xlen_t row = (R_xlen_t)i * n_moves + a;
      for (int j = 0; j < state_len; ++j) out(row, j) = cur[j];
      solved[row] = g->is_identity(cur);
    }
  }
  return List::create(_["children"] = out, _["solved"] = solved);
}

//' ADI Targets From Children Values
//'
//' The value target of a state is \code{min_a (1 + v(child_a))} and its policy
//' target is the move attaining that minimum. Value is cost-to-go, so the
//' network learns a distance and the solved state is zero.
//'
//' Solved children are what anchors the scheme. A child that is already solved
//' contributes exactly 1 no matter what the network says about it, so states
//' one move from the goal get an exact target from the first iteration onward,
//' and deeper states inherit that accuracy through their neighbours as
//' training goes on. This is why ADI needs no weighting by depth.
//'
//' @param child_values Numeric vector of network values, one per child, in the
//'   state-major order \code{cube_adi_children} produces
//' @param child_solved Logical vector, whether each child is the solved state
//' @param n_moves Number of moves per state
//' @return List with \code{value} (numeric) and \code{policy} (integer, the
//'   1-based index of the best move)
//' @keywords internal
// [[Rcpp::export]]
List cube_adi_targets(NumericVector child_values, LogicalVector child_solved,
                      int n_moves) {
  if (n_moves <= 0) stop("n_moves must be positive");
  const R_xlen_t total = child_values.size();
  if (child_solved.size() != total)
    stop("child_values and child_solved must have the same length");
  if (total % n_moves != 0)
    stop("child_values length %lld is not a multiple of n_moves %d",
         (long long)total, n_moves);

  const R_xlen_t n = total / n_moves;
  NumericVector value(n);
  IntegerVector policy(n);

  for (R_xlen_t i = 0; i < n; ++i) {
    double best = R_PosInf;
    int best_a = 1;
    for (int a = 0; a < n_moves; ++a) {
      const R_xlen_t k = i * n_moves + a;
      // A solved child is worth 0 by definition, whatever the network thinks.
      const double v = child_solved[k] ? 0.0 : child_values[k];
      const double cand = 1.0 + v;
      if (cand < best) { best = cand; best_a = a + 1; }
    }
    value[i] = best;
    policy[i] = best_a;
  }
  return List::create(_["value"] = value, _["policy"] = policy);
}
