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

//' Hash a Batch of States to Keys
//'
//' A search has to recognise a state it has already reached, and the obvious
//' way to do that from R -- \code{paste(state, collapse = ",")} per row -- costs
//' more than the network forward pass once the open list runs to hundreds of
//' thousands of nodes. This does the same job in one pass over the matrix.
//'
//' The keys are 64-bit FNV-1a hashes returned as doubles. A double carries 53
//' bits exactly, so the hash is folded down to 53 bits rather than truncated:
//' the alternative is silently rounding two distinct hashes onto the same
//' double. Collisions are still possible in principle -- at 2^53 keys and a
//' search of 10^6 nodes the chance is around 10^-4 -- and the caller that
//' cannot afford one has to compare the states themselves.
//'
//' @param states Integer matrix, one state per row
//' @return Numeric vector of keys, one per row
//' @keywords internal
// [[Rcpp::export]]
NumericVector cube_adi_keys(IntegerMatrix states) {
  const int n = states.nrow();
  const int m = states.ncol();
  NumericVector out(n);
  // Reading down a column at a time follows R's column-major storage; the
  // hash is order-dependent, so the loop carries one accumulator per row.
  std::vector<uint64_t> h((size_t)n, 1469598103934665603ULL);
  for (int j = 0; j < m; ++j) {
    const int* col = &states[(R_xlen_t)j * n];
    for (int i = 0; i < n; ++i) {
      h[i] ^= (uint64_t)(uint32_t)col[i];
      h[i] *= 1099511628211ULL;
    }
  }
  for (int i = 0; i < n; ++i) {
    // Fold the top bits into the bottom 53 so nothing is lost to rounding.
    const uint64_t v = (h[i] ^ (h[i] >> 53)) & ((1ULL << 53) - 1ULL);
    out[i] = (double)v;
  }
  return out;
}

//' One-Hot Encoding of States by Piece
//'
//' The piece encoding of \code{cube_adi_model(encoding = "piece")}, built here
//' rather than in R because it runs on every batch of every iteration. The R
//' version allocates a \code{n x P x P*W} array and fills it with a loop per
//' piece, which on a batch of a few thousand costs more than the training step
//' it feeds.
//'
//' Each of the \code{P} piece slots gets \code{P * W} bits, one per (piece,
//' turning) pair, and exactly one of them is set: which piece is sitting in
//' that slot, and which way round. The piece is read off the slot's first
//' sticker, since every sticker of a slot comes from one piece.
//'
//' @param states Integer matrix, one state per row
//' @param first_slot Integer vector, the first sticker position of each slot
//' @param home Integer vector, the piece each sticker belongs to (1-based)
//' @param turn Integer vector, which of its piece's slots each sticker is
//' @param n_piece Number of pieces
//' @param width Slots per piece
//'
//' The result is a flat \code{n x (n_piece * n_piece * width)} matrix rather
//' than an \code{n x n_piece x (n_piece * width)} array, because the network
//' runs about twenty-five times faster per state on a flat input than on a
//' two-dimensional one with a flatten over it.
//'
//' Flat here means exactly what R's own flattening of that array means: the
//' array is column-major, so its \code{(i, p, d)} lands at column
//' \code{d * n_piece + p}, with the slots adjacent and the bits strided. That
//' is not the order one would choose writing this from scratch -- slot-major
//' would be the natural one -- but it is the order every other view of this
//' data already has, and having two orders in play is how the encoding and the
//' test that checks it come to disagree while both look right.
//'
//' @return Numeric matrix \code{n x (n_piece * n_piece * width)}
//' @keywords internal
// [[Rcpp::export]]
NumericVector cube_adi_encode_pieces(IntegerMatrix states,
                                     IntegerVector first_slot,
                                     IntegerVector home, IntegerVector turn,
                                     int n_piece, int width) {
  const int n = states.nrow();
  if (first_slot.size() != n_piece)
    stop("first_slot has %d entries but there are %d pieces",
         (int)first_slot.size(), n_piece);
  const int depth = n_piece * width;

  // Laid out as the [n, n_piece, depth] array flattens: column-major, so
  // (i, p, d) sits at i + n * (d * n_piece + p). Writing it slot-major instead
  // -- p * depth + d -- holds the same ones in different columns, which no
  // shape check catches and which trains a network on scrambled inputs.
  NumericVector out((R_xlen_t)n * n_piece * depth);
  out.attr("dim") = IntegerVector::create(n, n_piece * depth);

  for (int p = 0; p < n_piece; ++p) {
    const int slot = first_slot[p];          // 1-based sticker position
    if (slot < 1 || slot > states.ncol())
      stop("slot %d is outside a state of %d positions", slot,
           (int)states.ncol());
    for (int i = 0; i < n; ++i) {
      const int here = states(i, slot - 1);  // sticker now in this slot
      if (here < 1 || here > home.size())
        stop("state holds position %d, outside 1..%d", here,
             (int)home.size());
      // (piece, turning) -> one index into the slot's bits
      const int d = (home[here - 1] - 1) * width + (turn[here - 1] - 1);
      out[(R_xlen_t)i + (R_xlen_t)n * ((R_xlen_t)d * n_piece + p)] = 1.0;
    }
  }
  return out;
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
      // Every other child is a distance, so it is worth at least 0 too. The
      // value head is linear and starts out emitting negatives; without this
      // floor min_a picks whichever child the untrained net happens to score
      // most negative, the target goes below zero, and the frozen copy learns
      // from it next round. The solved-child anchor alone cannot hold, since
      // any negative beats the 1 it contributes.
      double v = child_solved[k] ? 0.0 : child_values[k];
      if (!(v > 0.0)) v = 0.0;      // also sends NaN to 0
      const double cand = 1.0 + v;
      if (cand < best) { best = cand; best_a = a + 1; }
    }
    value[i] = best;
    policy[i] = best_a;
  }
  return List::create(_["value"] = value, _["policy"] = policy);
}
