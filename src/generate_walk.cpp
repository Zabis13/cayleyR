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
//' \code{pivot = TRUE} changes what is reported without changing how a walk is
//' taken. A cut is drawn uniformly from \code{k_min} to \code{n_moves - 1} and
//' the state at that point is returned instead of the endpoint, together with
//' the two moves that meet there: the inverse of the move that arrived
//' (\code{prev_move}, a step back toward the identity) and the move the walk
//' went on to make (\code{next_move}, a step further away). \code{depth} then
//' reports the cut, which is that point's distance label.
//'
//' The cut is drawn BEFORE the walk, and the walk is then made long enough to
//' contain it. Walking first and cutting inside afterwards is the obvious
//' order, but it leans the labels hard on the shallow end --- a cut of 1 can
//' come out of every walk, a deep cut only out of the longest --- and the deep
//' end ends up too thin to read while the shallow end, where the task is
//' trivial, dominates the averages. Cutting first gives every depth equal
//' weight, which is what the depth scripts sample and what a by-depth table has
//' to have to be comparable between runs.
//'
//' This is what a Q-model is trained on. Of the whole alphabet only those two
//' moves have a known label at that state --- \code{depth - 1} and
//' \code{depth + 1} --- and both come out of the walk itself, with no prefix
//' replayed and no distance guessed. \code{next_move} is the move the walk
//' really took, not one drawn afresh, so it is a genuine step of a random walk
//' rather than an arbitrary neighbour. \code{k_min} defaults to 2 because a cut
//' of 1 puts the pivot one move from solved, where every method is right and
//' the row says nothing. \code{no_undo} keeps \code{next_move} from being the
//' inverse of the arriving move, which would otherwise carry both labels at
//' once.
//'
//' @param group External pointer to a \code{perm_group}
//' @param n Number of walks to generate
//' @param n_moves Length of each walk, or the longest one when
//'   \code{exact = FALSE}
//' @param exact Walk \code{n_moves} exactly (default), or draw the length
//'   uniformly from 1 to \code{n_moves}
//' @param no_undo Refuse a move that immediately undoes the previous one
//'   (default \code{TRUE})
//' @param pivot Report a point partway along each walk instead of its end
//'   (default \code{FALSE}). See Details.
//' @param k_min Shallowest cut to draw when \code{pivot = TRUE} (default 2)
//' @param no_cancel Refuse a move that cancels against the tail of the walk
//'   through moves it commutes with (default \code{FALSE}). See Details of
//'   \code{\link{generate_walk}}.
//' @return List with \code{states} (n x state_len integer matrix, one walk
//'   endpoint per row), \code{depth} (integer vector, the length of each walk)
//'   and \code{moves} (n x n_moves integer matrix of 1-based move indices, the
//'   word each row walked, padded with \code{NA} past that row's depth). With
//'   \code{pivot = TRUE} it also carries \code{prev_move} and \code{next_move}
//'   (integer vectors of 1-based move indices) and \code{states} holds the
//'   pivot point rather than the endpoint, with \code{depth} its distance
//'   label.
//' @keywords internal
// [[Rcpp::export]]
List generate_walk_cpp(SEXP group, int n, int n_moves,
                       bool exact = true, bool no_undo = true,
                       bool pivot = false, int k_min = 2,
                       bool no_cancel = false) {
  XPtr<PermGroup> g(group);
  if (n <= 0) stop("n must be positive");
  if (n_moves <= 0) stop("n_moves must be positive");
  if (pivot && n_moves < 2)
    stop("pivot needs n_moves >= 2: a midpoint must have a step on each side");
  if (pivot && k_min < 1)
    stop("k_min must be at least 1: the pivot is a step from the identity");
  if (pivot && k_min > n_moves - 1)
    stop("k_min must leave a move beyond the pivot: need k_min <= n_moves - 1");

  const int state_len = g->state_length();
  const int n_alpha   = g->n_moves();
  if (n_alpha <= 0) stop("group has no moves");
  if (pivot && n_alpha < 2)
    stop("pivot needs at least two moves to tell previous from next");

  IntegerMatrix states(n, state_len);
  IntegerVector depth(n);
  // Ragged in principle -- with exact = FALSE the rows differ in length -- but
  // returned as one matrix padded with NA, because that is the shape R reads
  // back cheaply. n_moves is the longest any row can be either way.
  IntegerMatrix words(n, n_moves);
  std::fill(words.begin(), words.end(), NA_INTEGER);

  // Only filled when pivot = TRUE; length 0 otherwise so the unused case costs
  // nothing and the R side can test for their presence.
  IntegerVector prev_move(pivot ? n : 0);
  IntegerVector next_move(pivot ? n : 0);

  std::vector<int> cur, at_pivot;

  // no_cancel reads two facts off the permutations themselves, so it needs no
  // knowledge of faces or axes: which pairs of moves commute, and the order of
  // each move. On the cube U, D and E commute, so U D U' walks three moves and
  // travels one; and U has order 4, so U U U is U'. no_undo sees neither.
  std::vector<std::vector<char> > commute;
  std::vector<int> order;
  if (no_cancel) {
    commute.assign(n_alpha, std::vector<char>(n_alpha, 0));
    for (int a = 0; a < n_alpha; ++a) {
      for (int b = a; b < n_alpha; ++b) {
        std::vector<int> ab = g->identity(), ba = g->identity();
        g->apply(ab, a); g->apply(ab, b);
        g->apply(ba, b); g->apply(ba, a);
        commute[a][b] = commute[b][a] = (ab == ba);
      }
    }
    // 0 where the order is not found within the cap; the count rule is then
    // skipped for that move and only the inverse rule applies.
    order.assign(n_alpha, 0);
    for (int a = 0; a < n_alpha; ++a) {
      std::vector<int> s = g->identity();
      for (int k = 1; k <= 1000; ++k) {
        g->apply(s, a);
        if (g->is_identity(s)) { order[a] = k; break; }
      }
    }
  }
  std::vector<int> allowed;
  allowed.reserve(n_alpha);

  for (int i = 0; i < n; ++i) {
    cur = g->identity();

    int d = n_moves;
    int cut = -1;

    if (pivot) {
      // Draw the CUT first, uniformly over k_min..n_moves-1, then the walk
      // long enough to contain it. Drawing the walk first and cutting inside
      // it -- which is the obvious order, and the one cayleypy-training-core
      // uses -- makes the label distribution lean hard on the shallow end: a
      // cut of 1 can come out of every walk, a cut of 9 only out of the
      // longest. Measured on 1000 draws over 1..10 that gave 411 states at
      // depth 1 and 16 at depth 9, so the deep end was too thin to read and
      // the shallow end, where the task is trivial, dominated the averages.
      //
      // Cutting first makes every depth equally represented, which is what the
      // by-depth table has to have to be comparable across runs, and matches
      // how the depth scripts sample.
      const int span = n_moves - k_min;          // cut in k_min .. n_moves-1
      cut = k_min + (int)(unif_rand() * (span));
      if (cut > n_moves - 1) cut = n_moves - 1;  // guards unif_rand() == 1

      // The walk must go at least one move past the cut; beyond that its
      // length does not matter, since only the cut point is reported. exact
      // walks the full n_moves, otherwise the tail is drawn.
      if (exact) {
        d = n_moves;
      } else {
        const int tail = n_moves - cut;          // at least 1
        d = cut + 1 + (int)(unif_rand() * tail);
        if (d > n_moves) d = n_moves;
      }
    } else if (!exact) {
      d = (int)(unif_rand() * n_moves) + 1;
      if (d > n_moves) d = n_moves;              // guards unif_rand() == 1
    }
    int last = -1;
    for (int step = 0; step < d; ++step) {
      int a;
      // When the alphabet does not name the inverse of the last move there is
      // nothing to refuse, and the loop must not spin looking for it. Nor may
      // it refuse the only move there is: a one-move group would never satisfy
      // the condition and the draw would not terminate.
      if (no_cancel) {
        // Walk back over the tail while it commutes with `a`. Meeting the
        // inverse of `a` there means `a` would cancel it; meeting `a` itself
        // often enough means one more would be shorter the other way round.
        allowed.clear();
        for (int c = 0; c < n_alpha; ++c) {
          const int inv_c = g->inverse_move(c);
          int  cnt = 0;
          bool ban = false;
          for (int j = step - 1; j >= 0; --j) {
            const int b = words(i, j) - 1;
            if (b == inv_c) { ban = true; break; }
            if (!commute[c][b]) break;
            if (b == c) ++cnt;
          }
          if (!ban && order[c] > 0 && 2 * (cnt + 1) > order[c]) ban = true;
          if (!ban) allowed.push_back(c);
        }
        if (allowed.empty())
          stop("no_cancel left no move to take at step %d", step + 1);
        int k = (int)(unif_rand() * allowed.size());
        if (k >= (int)allowed.size()) k = (int)allowed.size() - 1;
        a = allowed[k];
      } else {
        const int banned = (no_undo && last >= 0 && n_alpha > 1)
                           ? g->inverse_move(last) : -1;
        do {
          a = (int)(unif_rand() * n_alpha);
          if (a >= n_alpha) a = n_alpha - 1;
        } while (banned >= 0 && a == banned);
      }

      // The cut is taken BEFORE the move is applied, so `cur` is the pivot
      // point, the move about to be made leads away from the goal, and the
      // inverse of the move that arrived here leads back. Both labels come out
      // of the walk itself -- nothing is replayed and nothing is guessed.
      if (pivot && step == cut) {
        at_pivot = cur;
        prev_move[i] = g->inverse_move(last) + 1;   // 1-based, steps homeward
        next_move[i] = a + 1;                       // 1-based, steps away
      }

      g->apply(cur, a);
      words(i, step) = a + 1;                // 1-based, to index g$moves in R
      last = a;
    }

    if (pivot) {
      for (int j = 0; j < state_len; ++j) states(i, j) = at_pivot[j];
      depth[i] = cut;                        // the pivot's distance label
    } else {
      for (int j = 0; j < state_len; ++j) states(i, j) = cur[j];
      depth[i] = d;
    }
  }

  if (pivot)
    return List::create(_["states"] = states, _["depth"] = depth,
                        _["moves"]  = words,
                        _["prev_move"] = prev_move, _["next_move"] = next_move);
  return List::create(_["states"] = states, _["depth"] = depth,
                      _["moves"]  = words);
}
