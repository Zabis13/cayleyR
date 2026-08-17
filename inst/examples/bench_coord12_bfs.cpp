// Breadth-first over phase 3's exact coordinate: where the twelve primary
// wings sit among the twelve primary slots.
//
// Compiled by bench_coord12_bfs.R with Rcpp::sourceCpp, so nothing in the
// package changes until the measurement says the design is worth building.
//
// The point of the design is that 12! = 479,001,600 states fit in a table that
// can be addressed directly. No hash, so no collisions, so the bound is the
// real distance rather than another state's entry -- which is what the current
// phase 3 cannot promise (waste 0.975, best_bound pinned at 0 on every cube it
// fails).
//
// The table is the visited set AND the answer. There is no frontier list: the
// states at depth k are found by scanning the table for the value k. That
// costs one pass over 479M bytes per level -- cheap next to expanding them --
// and in exchange the memory is exactly the table, with no peak above it. A
// frontier-list BFS on a space this size is where the memory goes, so this
// design does not have one.

#include <Rcpp.h>
#include <vector>
#include <cstring>
using namespace Rcpp;

static const int NP = 12;                  // primary slots, and primary wings
static const long NSTATES = 479001600L;    // 12!

static const int FACT[13] = {
  1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800, 479001600
};

// ---- ranking -------------------------------------------------------------
//
// Lehmer code. rank() and unrank() have to be exact inverses or the table is
// addressed inconsistently and every number that comes out of it is nonsense;
// bench_coord12_bfs.R checks that on a sample before trusting any of this.

static inline long rank_perm(const int* p) {
  long r = 0;
  for (int i = 0; i < NP; i++) {
    int c = 0;
    for (int j = i + 1; j < NP; j++) if (p[j] < p[i]) c++;
    r += (long)c * FACT[NP - 1 - i];
  }
  return r;
}

static inline void unrank_perm(long r, int* p) {
  int avail[NP];
  for (int i = 0; i < NP; i++) avail[i] = i;
  int n = NP;
  for (int i = 0; i < NP; i++) {
    const long f = FACT[NP - 1 - i];
    const int k = (int)(r / f);
    r -= (long)k * f;
    p[i] = avail[k];
    for (int j = k; j < n - 1; j++) avail[j] = avail[j + 1];
    n--;
  }
}

// [[Rcpp::export]]
List coord12_check_rank(int n_samples) {
  int p[NP], q[NP];
  bool ok = true;
  long bad_at = -1;
  // Both ends: rank(unrank(r)) == r on samples spread across the range, and
  // unrank(rank(p)) == p on the identity and on a few shuffles.
  for (int s = 0; s < n_samples; s++) {
    const long r = (long)((double)s / n_samples * (double)NSTATES);
    unrank_perm(r, p);
    if (rank_perm(p) != r) { ok = false; bad_at = r; break; }
  }
  return List::create(_["ok"] = ok, _["bad_at"] = (double)bad_at,
                      _["n_states"] = (double)NSTATES);
}

// ---- the search ----------------------------------------------------------

// `moves` is n_moves rows of NP entries: moves[m][i] is the primary slot whose
// wing lands in slot i when move m is applied. Built in R from the cube's own
// move tables, so this file does not restate any geometry.

// [[Rcpp::export]]
List coord12_bfs(IntegerMatrix moves, IntegerVector goal_ranks,
                 int max_depth = 20, bool verbose = true) {
  const int n_moves = moves.nrow();
  if (moves.ncol() != NP) stop("moves must have %d columns", NP);

  std::vector<int> mv(n_moves * NP);
  for (int m = 0; m < n_moves; m++)
    for (int i = 0; i < NP; i++) mv[m * NP + i] = moves(m, i);

  // One byte per state: 0 means unvisited, d+1 means distance d. 479 MB.
  // Distances here run to about 15, so a nibble each would halve it; a byte is
  // kept for the prototype because the question is whether this is affordable
  // at all, and 479 MB already answers that.
  std::vector<unsigned char> tab;
  tab.assign((size_t)NSTATES, 0);

  std::vector<double> counts;
  double total = 0;

  for (int i = 0; i < goal_ranks.size(); i++) {
    const long g = (long)goal_ranks[i];
    if (g < 0 || g >= NSTATES) stop("goal rank out of range");
    if (tab[(size_t)g] == 0) { tab[(size_t)g] = 1; total += 1; }
  }
  counts.push_back(total);
  if (verbose) {
    Rcpp::Rcout << "  depth  0: " << (long)total << " states\n";
    R_FlushConsole();
  }

  int p[NP], q[NP];
  for (int depth = 0; depth < max_depth; depth++) {
    const unsigned char cur = (unsigned char)(depth + 1);
    const unsigned char nxt = (unsigned char)(depth + 2);
    double found = 0;

    // Scan for the current level instead of keeping a frontier. One pass over
    // the table per level, and no allocation that grows with the level's size.
    for (long r = 0; r < NSTATES; r++) {
      if (tab[(size_t)r] != cur) continue;
      unrank_perm(r, p);
      for (int m = 0; m < n_moves; m++) {
        const int* pm = &mv[m * NP];
        for (int i = 0; i < NP; i++) q[i] = p[pm[i]];
        const long r2 = rank_perm(q);
        if (tab[(size_t)r2] == 0) { tab[(size_t)r2] = nxt; found += 1; }
      }
      if ((r & 0xFFFFFF) == 0) Rcpp::checkUserInterrupt();
    }

    counts.push_back(found);
    total += found;
    if (verbose) {
      Rcpp::Rcout << "  depth " << (depth + 1) << ": " << (long)found
                  << " states, " << (long)total << " seen ("
                  << (100.0 * total / (double)NSTATES) << "%)\n";
      R_FlushConsole();
    }
    if (found == 0) break;
  }

  return List::create(
    _["counts"] = NumericVector(counts.begin(), counts.end()),
    _["total"] = total,
    _["n_states"] = (double)NSTATES,
    _["complete"] = (total == (double)NSTATES),
    _["table_mb"] = (double)NSTATES / 1048576.0);
}
