#include <Rcpp.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>
#include <algorithm>
#include "cayley_utils.h"

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// ---------------------------------------------------------------------------
// Shorten a path by cutting across cycles.
//
// A cycle is a combo word applied round and round from some point on the path
// until the state returns to where it started. If a state along that loop also
// occurs later in the path, the stretch between the two meeting points can be
// replaced by the stretch of the loop, and when the loop is shorter the path
// shrinks.
//
// Points are handled one at a time and a cut is applied the moment it is
// found, so every later point searches the already-shortened path. Points are
// carried as states, not positions: a cut renumbers everything downstream, but
// a state is either still in the path or it is not.
//
// This lives in C++ because the search is millions of single-operation steps
// each needing an index lookup -- in R the intermediate states and their hash
// keys dominated everything else.
// ---------------------------------------------------------------------------

namespace {

// FNV-1a over the state's bytes. The search tests a state against the index on
// every single step, and building the string key state_to_key() returns -- some
// two thousand characters for a ring of 500 -- cost far more than the operation
// being applied. A 64-bit hash costs no allocation at all.
//
// Collisions are therefore possible, and are resolved by keeping the state
// itself alongside each entry and comparing on a hit.
inline uint64_t state_hash(const std::vector<int>& st) {
  uint64_t h = 1469598103934665603ULL;
  const unsigned char* p = (const unsigned char*)st.data();
  const size_t bytes = st.size() * sizeof(int);
  for (size_t i = 0; i < bytes; i++) {
    h ^= (uint64_t)p[i];
    h *= 1099511628211ULL;
  }
  return h;
}

// state -> furthest position along the path where it occurs, plus the state, so
// a hash collision can be told from a real match. The furthest position is what
// matters: a cut is worth more the later it lands.
struct Entry {
  int pos;
  std::vector<int> state;
};
typedef std::unordered_map<uint64_t, std::vector<Entry> > PathIndex;

// Position of `st` in the index, or -1. Buckets hold every state sharing a
// hash, so the state is compared before a hit is believed.
inline int index_lookup(const PathIndex& index, const std::vector<int>& st) {
  PathIndex::const_iterator it = index.find(state_hash(st));
  if (it == index.end()) return -1;
  for (size_t i = 0; i < it->second.size(); i++) {
    if (it->second[i].state == st) return it->second[i].pos;
  }
  return -1;
}

// Record `st` at `pos`, keeping the furthest position when it repeats.
inline void index_put(PathIndex& index, const std::vector<int>& st, int pos) {
  std::vector<Entry>& bucket = index[state_hash(st)];
  for (size_t i = 0; i < bucket.size(); i++) {
    if (bucket[i].state == st) {
      if (pos > bucket[i].pos) bucket[i].pos = pos;
      return;
    }
  }
  Entry e;
  e.pos = pos;
  e.state = st;
  bucket.push_back(e);
}

// Walk a path, filling the index and reporting the state it ends on. The
// intermediate states are not kept -- only `want`ed positions are copied out,
// which is what the search needs to start from.
std::vector<int> walk_path(const std::vector<int>& start_state,
                           const std::vector<int>& ops,
                           int k,
                           PathIndex& index,
                           const std::vector<int>& want,
                           std::vector<std::vector<int> >& wanted) {
  index.clear();
  index.reserve(ops.size() * 2 + 1);
  wanted.assign(want.size(), std::vector<int>());

  std::vector<int> cur = start_state;
  index_put(index, cur, 0);
  for (size_t w = 0; w < want.size(); w++) {
    if (want[w] == 0) wanted[w] = cur;
  }

  for (size_t i = 0; i < ops.size(); i++) {
    apply_op_code_inplace(cur, ops[i], k);
    int pos = (int)i + 1;
    index_put(index, cur, pos);
    for (size_t w = 0; w < want.size(); w++) {
      if (want[w] == pos) wanted[w] = cur;
    }
  }
  return cur;
}

struct Cut {
  int from;
  int to;
  int cycle_len;
  int gain;
  std::vector<int> ops;
};

// Sort keys, matching find_best_random_combinations: 1 longest, 2 shortest,
// 3 most_unique, 4 least_unique, 5 most_repeated, 6 least_repeated.
struct Score {
  int total;      // steps until the cycle closes
  int distinct;   // distinct states seen on the way
  int idx;
};

// Order two scored combos by the criteria in `sort_by`, first key first.
struct ScoreLess {
  const std::vector<int>& sort_by;
  explicit ScoreLess(const std::vector<int>& sb) : sort_by(sb) {}

  double key(const Score& s, int criterion) const {
    double ratio = s.distinct > 0 ? (double)s.total / s.distinct
                                  : (double)s.total;
    switch (criterion) {
      case 1: return -(double)s.total;      // longest
      case 2: return  (double)s.total;      // shortest
      case 3: return -(double)s.distinct;   // most_unique
      case 4: return  (double)s.distinct;   // least_unique
      case 5: return -ratio;                // most_repeated
      case 6: return  ratio;                // least_repeated
    }
    return 0.0;
  }

  bool operator()(const Score& a, const Score& b) const {
    for (size_t i = 0; i < sort_by.size(); i++) {
      double ka = key(a, sort_by[i]), kb = key(b, sort_by[i]);
      if (ka != kb) return ka < kb;
    }
    return a.idx < b.idx;
  }
};

// Sample combo words, score each by unrolling its cycle, and return the best
// n_top of them. Scoring costs a full unroll per candidate, which is why the
// cycles are kept short by combo_length -- a short word closes quickly.
// max_cycle_len is a backstop for the occasional word that does not.
std::vector<std::vector<int> > sample_combos(const std::vector<int>& from_state,
                                             int k,
                                             const std::vector<int>& move_codes,
                                             int combo_length,
                                             int n_samples,
                                             int n_top,
                                             const std::vector<int>& sort_by,
                                             int max_cycle_len,
                                             int n_threads) {
  int n_moves = (int)move_codes.size();
  std::unordered_set<std::string> seen;
  std::vector<std::vector<int> > combos;

  int max_iter = n_samples * 10;
  while ((int)combos.size() < n_samples && max_iter-- > 0) {
    std::vector<int> combo(combo_length);
    std::string key;
    key.reserve(combo_length);
    for (int j = 0; j < combo_length; j++) {
      int idx = (int)(R::runif(0.0, 1.0) * n_moves);
      if (idx >= n_moves) idx = n_moves - 1;
      combo[j] = move_codes[idx];
      key += (char)('0' + combo[j]);
    }
    if (seen.insert(key).second) combos.push_back(combo);
  }

  // No criteria: take the first n_top as drawn. Worth a separate path rather
  // than an unsorted fall-through, because scoring is what makes ranking
  // expensive -- every candidate gets unrolled -- and with nothing to rank
  // none of that work is needed.
  if (sort_by.empty()) {
    if ((int)combos.size() > n_top) combos.resize(n_top);
    return combos;
  }

  // Scoring is the expensive half of ranking -- every candidate gets unrolled
  // -- and the candidates are independent: each walks its own state and writes
  // one preallocated slot. No R API is touched in here, which is what makes it
  // safe to thread; the sampling above uses R's RNG and stays on one thread.
  const int n_combos = (int)combos.size();
  std::vector<Score> scored(n_combos);

  #ifdef _OPENMP
  #pragma omp parallel for schedule(dynamic) num_threads(n_threads)
  #endif
  for (int ci = 0; ci < n_combos; ci++) {
    std::vector<int> cur = from_state;
    std::unordered_set<uint64_t> distinct;
    int total = 0;
    bool stop = false;
    while (!stop) {
      for (size_t j = 0; j < combos[ci].size(); j++) {
        apply_op_code_inplace(cur, combos[ci][j], k);
        total++;
        if (cur == from_state) { stop = true; break; }
        distinct.insert(state_hash(cur));
        if (total >= max_cycle_len) { stop = true; break; }
      }
    }
    scored[ci].total = total;
    scored[ci].distinct = (int)distinct.size();
    scored[ci].idx = ci;
  }

  std::sort(scored.begin(), scored.end(), ScoreLess(sort_by));
  if ((int)scored.size() > n_top) scored.resize(n_top);

  std::vector<std::vector<int> > top;
  top.reserve(scored.size());
  for (size_t i = 0; i < scored.size(); i++) top.push_back(combos[scored[i].idx]);
  return top;
}

// Spin cycles out of one point, returning its best cut. found stays false when
// nothing beats the path as it stands.
Cut best_cut_from(const std::vector<int>& from_state,
                  int from,
                  const PathIndex& index,
                  int k,
                  const std::vector<std::vector<int> >& combos,
                  int max_cycle_len,
                  int n_threads,
                  bool& found) {
  // Combos are independent, so each is searched on its own thread and keeps
  // its own best; the winners are reduced afterwards. The index is only read
  // here, never written, which is what makes sharing it safe.
  const int n_combos = (int)combos.size();
  std::vector<Cut> per_combo(n_combos);
  std::vector<char> hit(n_combos, 0);

  #ifdef _OPENMP
  #pragma omp parallel for schedule(dynamic) num_threads(n_threads)
  #endif
  for (int ci = 0; ci < n_combos; ci++) {
    const std::vector<int>& word = combos[ci];
    if (word.empty()) continue;

    Cut local;
    local.gain = 0;
    bool local_hit = false;

    std::vector<int> cur = from_state;
    std::vector<int> ops;
    ops.reserve(max_cycle_len);
    int m = 0;
    bool stop = false;

    while (!stop) {
      for (size_t j = 0; j < word.size(); j++) {
        apply_op_code_inplace(cur, word[j], k);
        m++;
        ops.push_back(word[j]);

        // Back where it started: the cycle has closed, and another lap would
        // only repeat the same states.
        if (cur == from_state) { stop = true; break; }

        int at = index_lookup(index, cur);
        // Landing behind the current point would run the path backwards.
        if (at > from) {
          int gain = (at - from) - m;
          if (gain > local.gain) {
            local.from = from;
            local.to = at;
            local.cycle_len = m;
            local.gain = gain;
            local.ops.assign(ops.begin(), ops.end());
            local_hit = true;
          }
        }
        if (m >= max_cycle_len) { stop = true; break; }
      }
    }

    if (local_hit) {
      per_combo[ci] = local;
      hit[ci] = 1;
    }
  }

  Cut best;
  best.gain = 0;
  found = false;
  // Reduced in combo order so the result does not depend on which thread
  // finished first.
  for (int ci = 0; ci < n_combos; ci++) {
    if (hit[ci] && per_combo[ci].gain > best.gain) {
      best = per_combo[ci];
      found = true;
    }
  }

  return best;
}

}  // namespace

// [[Rcpp::export]]
List cycle_shortcut_cpp(IntegerVector start_state,
                        IntegerVector path,
                        int k,
                        IntegerVector points,
                        IntegerVector moves,
                        int combo_length,
                        int n_samples,
                        int n_top,
                        IntegerVector sort_by,
                        int max_cycle_len,
                        int n_threads,
                        bool verbose) {
  std::vector<int> start(start_state.begin(), start_state.end());
  std::vector<int> ops(path.begin(), path.end());
  std::vector<int> move_codes(moves.begin(), moves.end());
  std::vector<int> want(points.begin(), points.end());
  std::vector<int> sort_codes(sort_by.begin(), sort_by.end());
  const int N = (int)ops.size();

  PathIndex index;
  std::vector<std::vector<int> > point_states;
  std::vector<int> final_state = walk_path(start, ops, k, index, want, point_states);

  std::vector<Cut> cuts;

  for (size_t pi = 0; pi < point_states.size(); pi++) {
    Rcpp::checkUserInterrupt();
    if (point_states[pi].empty()) continue;

    // Still in the path? An earlier cut may have removed the stretch this
    // point sat in, in which case there is nothing to search from.
    int from = index_lookup(index, point_states[pi]);
    if (from < 0) {
      if (verbose) Rcpp::Rcout << "  point " << (pi + 1) << ": gone, skipped\n";
      continue;
    }
    if (from >= (int)ops.size() - 1) continue;

    int cap = std::min(max_cycle_len, (int)ops.size());
    std::vector<std::vector<int> > combos = sample_combos(
      point_states[pi], k, move_codes, combo_length, n_samples, n_top,
      sort_codes, cap, n_threads);

    bool found = false;
    Cut best = best_cut_from(point_states[pi], from, index, k, combos, cap,
                             n_threads, found);
    if (!found) continue;

    // Apply straight away, then rebuild the index so every later point is
    // searched against the shortened path.
    std::vector<int> next;
    next.reserve(ops.size() - best.gain);
    next.insert(next.end(), ops.begin(), ops.begin() + best.from);
    next.insert(next.end(), best.ops.begin(), best.ops.end());
    if (best.to < (int)ops.size()) {
      next.insert(next.end(), ops.begin() + best.to, ops.end());
    }
    ops.swap(next);

    std::vector<int> no_want;
    std::vector<std::vector<int> > no_states;
    final_state = walk_path(start, ops, k, index, no_want, no_states);

    cuts.push_back(best);
    if (verbose) {
      Rcpp::Rcout << "  point " << (pi + 1) << ": " << best.from << " -> "
                  << best.to << " via " << best.cycle_len << " ops, gain "
                  << best.gain << ", path now " << ops.size() << "\n";
    }
  }

  IntegerVector out_path(ops.begin(), ops.end());
  IntegerVector cut_from(cuts.size()), cut_to(cuts.size());
  IntegerVector cut_len(cuts.size()), cut_gain(cuts.size());
  for (size_t i = 0; i < cuts.size(); i++) {
    cut_from[i] = cuts[i].from;
    cut_to[i] = cuts[i].to;
    cut_len[i] = cuts[i].cycle_len;
    cut_gain[i] = cuts[i].gain;
  }

  return List::create(
    _["path"] = out_path,
    _["final_state"] = IntegerVector(final_state.begin(), final_state.end()),
    _["original_length"] = N,
    _["new_length"] = (int)ops.size(),
    _["savings"] = N - (int)ops.size(),
    _["cut_from"] = cut_from,
    _["cut_to"] = cut_to,
    _["cut_len"] = cut_len,
    _["cut_gain"] = cut_gain
  );
}
