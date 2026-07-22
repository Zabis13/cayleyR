#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <string>
#include <algorithm>
#include <climits>
#include "cayley_utils.h"

using namespace Rcpp;

// ---------------------------------------------------------------------------
// Human-style solver for the TopSpin puzzle.
//
// Phase 1 grows a sorted run one value at a time: the ring is manoeuvred until
// the new value sits exactly k positions after its predecessor, and a single
// reverse-prefix drops it into place. The run is tracked as a contiguous range
// of ring positions, and auxiliary flips only ever use windows lying wholly
// inside the unsorted arc, so the run is never disturbed.
//
// Phase 2 finishes the last eight tiles with two local 3-cycles:
//
//   A = X L X L X R X R      cycles positions 1 <- 4 <- 6 <- 1
//   B = L X R X L X L X R R  cycles positions 1 <- 3 <- 6 <- 1
//
// Both leave the rest of the ring untouched. Conjugated by rotations they
// generate the full alternating group on the tail (measured: 20160 = 8!/2), so
// the finish is a table lookup rather than a search.
//
// 3-cycles are even, so odd tail arrangements are unreachable by them. Those
// are handled by firing one flip across the block boundary and rebuilding the
// run: shifting a tile between block and tail flips the parity of the split.
// ---------------------------------------------------------------------------

namespace {

// Tiles handed to the 3-cycle phase. The primitives reach position k+2, and
// conjugating shifts that a further two, so the tail must cover k+4.
const int MIN_TAIL = 8;
inline int tail_size(int k) { return std::max(MIN_TAIL, k + 4); }

// Search limits for deriving the 3-cycle primitives.
const int PRIM_MAX_LEN     = 17;       // longest word considered
const int PRIM_MAX_FRONTIER = 1200000; // states held per BFS level
const int PRIM_COUNT        = 60;      // distinct local moves to collect
const int PRIM_MAX_MOVED    = 5;       // widest window a primitive may disturb

// Depth of the tail table walk.
const int TABLE_DEPTH = 16;
// The block check below is what prunes this BFS, and it only inspects the
// bs = n - TAIL block tiles. As bs shrinks the pruning weakens while the key
// space (TAIL! in the worst case) stays large, so the table can run away.
// These caps bound the work regardless; the table is then partial and
// try_table simply fails to find some states.
const int TABLE_MAX_FRONTIER = 200000;  // states held per BFS level
const int TABLE_MAX_ENTRIES  = 2000000; // total stored tail positions

struct Solver {
  std::vector<int> state;
  std::vector<std::string> path;
  int n, k;

  Solver(const std::vector<int>& s, int k_) : state(s), n((int)s.size()), k(k_) {}

  void emit(const std::string& op) {
    apply_op_inplace(state, op, k);
    path.push_back(op);
  }

  void emit_word(const std::vector<std::string>& w) {
    for (const auto& op : w) emit(op);
  }

  void rotate(int t) {
    t = ((t % n) + n) % n;
    if (t == 0) return;
    if (t <= n - t) { for (int i = 0; i < t; i++) emit("L"); }
    else { for (int i = 0; i < n - t; i++) emit("R"); }
  }

  int pos_of(int v) const {
    for (int i = 0; i < n; i++) if (state[i] == v) return i;
    return -1;
  }

  int fwd_gap(int a, int b) const { return ((b - a) % n + n) % n; }
};

// Length of the run 1,2,...,r currently consecutive on the ring.
// Shared with the scoring path in state_store.cpp; see cayley_utils.h.
inline int run_length(const std::vector<int>& st) { return run_length_of(st); }

bool is_solved(const std::vector<int>& st) {
  return run_length(st) == (int)st.size();
}

// Derive the 3-cycle primitives for a given k.
//
// Rather than hard-coding words, the solver searches short sequences for ones
// that act as a plain 3-cycle confined to the first `span` positions once
// their net rotation is compensated. Two words are kept, and they must differ
// in geometry -- the gaps within the triple -- because two cycles on the same
// shape are conjugates of each other and generate too little on their own.
//
// For k = 4 this recovers X L X L X R X R and L X R X L X L X R R; k = 3 needs
// a different pair, which is why fixed words would not carry over.
std::vector<std::vector<std::string>> derive_primitives(int k, int span, int ring) {
  // A word's effect depends on the ring it runs on, because its rotations wrap
  // modulo n. Deriving on one size and applying on another silently produces a
  // different permutation, so the search must run on the actual ring.
  const int n = ring;
  std::vector<int> id(n);
  for (int i = 0; i < n; i++) id[i] = i + 1;

  std::vector<std::vector<std::string>> found;
  std::vector<std::string> profiles;

  struct Node {
    std::vector<int> s;
    std::vector<std::string> w;
    int sh;
  };

  std::unordered_map<std::string, bool> seen;
  seen[state_to_key(id)] = true;

  std::vector<Node> frontier;
  frontier.push_back({id, {}, 0});

  const char* ops[3] = {"L", "R", "X"};

  for (int d = 0; d < PRIM_MAX_LEN && !frontier.empty(); d++) {
    Rcpp::checkUserInterrupt();
    std::vector<Node> next;

    for (const auto& f : frontier) {
      for (int oi = 0; oi < 3; oi++) {
        std::vector<int> s2 = f.s;
        apply_op_inplace(s2, ops[oi], k);

        std::string key = state_to_key(s2);
        if (seen.count(key)) continue;
        seen[key] = true;

        int sh2 = f.sh + (oi == 0 ? 1 : (oi == 1 ? -1 : 0));
        std::vector<std::string> w2 = f.w;
        w2.push_back(ops[oi]);

        // Compensate the net rotation, then look at what actually moved.
        int shc = ((sh2 % n) + n) % n;
        std::vector<int> st = s2;
        for (int i = 0; i < shc; i++) apply_op_inplace(st, "R", k);

        std::vector<int> ch;
        for (int i = 0; i < n; i++) if (st[i] != id[i]) ch.push_back(i + 1);

        if ((int)ch.size() >= 3 && (int)ch.size() <= PRIM_MAX_MOVED &&
            ch.back() <= span) {
          // The moved values must be a permutation of the moved positions --
          // anything else would drag tiles in from outside the window.
          //
          // Cycles of length 3 are enough when k is even. For odd k a reverse
          // preserves the parity of a position, so 3-cycles alone leave the
          // tail split into two halves that never mix; the wider primitives
          // (4-cycles and up) are what bridge them.
          std::vector<int> vals, pos = ch;
          for (int c : ch) vals.push_back(st[c-1]);
          std::sort(vals.begin(), vals.end());
          if (vals == pos) {
            // Key on the triple itself, not on its shape: conjugation only
            // shifts a triple right, so the set has to include triples that
            // already sit near the far end of the tail, or the last positions
            // stay unreachable and the generated group collapses.
            std::string prof;
            for (size_t i = 0; i < ch.size(); i++) {
              if (i) prof += ',';
              prof += std::to_string(ch[i]);
            }
            if (std::find(profiles.begin(), profiles.end(), prof) == profiles.end()) {
              profiles.push_back(prof);
              std::vector<std::string> full = w2;
              for (int i = 0; i < shc; i++) full.push_back("R");
              found.push_back(full);
              if ((int)found.size() >= PRIM_COUNT) return found;
            }
          }
        }

        next.push_back({s2, w2, sh2});
      }
    }
    frontier = std::move(next);
    if ((int)frontier.size() > PRIM_MAX_FRONTIER) break;
  }
  return found;
}

std::vector<std::string> conjugate(const std::vector<std::string>& w, int r) {
  std::vector<std::string> out;
  for (int i = 0; i < r; i++) out.push_back("L");
  out.insert(out.end(), w.begin(), w.end());
  for (int i = 0; i < r; i++) out.push_back("R");
  return out;
}

std::vector<std::string> invert_word(const std::vector<std::string>& w) {
  std::vector<std::string> out;
  for (int i = (int)w.size() - 1; i >= 0; i--) {
    if (w[i] == "L") out.push_back("R");
    else if (w[i] == "R") out.push_back("L");
    else out.push_back("X");
  }
  return out;
}

// Pack the first TAIL tile numbers into one integer, six bits each. TAIL <= 10
// (tail_size caps at k+4 and rings stay small), so 60 bits fit a long long and
// tile numbers stay under 64. This replaces per-state string keys in the table
// BFS, where hashing and building millions of strings dominated the build.
typedef long long TailKey;

inline TailKey tail_key(const std::vector<int>& st, int TAIL) {
  TailKey key = 0;
  for (int i = 0; i < TAIL; i++) {
    key = (key << 6) | (st[i] & 0x3F);
  }
  return key;
}

// Lookup table: tail arrangement -> word that solves it.
// Built once per (n, k) by walking backwards from the solved ring.
typedef std::unordered_map<TailKey, std::vector<std::string>> Table;

Table build_table(int n, int k) {
  Table tbl;
  const int TAIL = tail_size(k);
  int bs = n - TAIL;

  std::vector<int> goal(n);
  for (int i = 0; i < TAIL; i++) goal[i] = bs + 1 + i;
  for (int i = 0; i < bs; i++) goal[TAIL + i] = i + 1;

  std::vector<std::vector<std::string>> prims = derive_primitives(k, TAIL, n);
  if (prims.empty()) return tbl;

  // Conjugating shifts a primitive's triple to the right, so the usable range
  // is whatever room is left between the triple's reach and the tail width.
  std::vector<std::vector<std::string>> gens;
  for (const auto& base : prims) {
    std::vector<int> probe(n);
    for (int i = 0; i < n; i++) probe[i] = i + 1;
    for (const auto& op : base) apply_op_inplace(probe, op, k);
    int reach = 0;
    for (int i = 0; i < n; i++) if (probe[i] != i + 1) reach = i + 1;

    int max_r = TAIL - reach;
    if (max_r < 0) max_r = 0;
    for (int r = 0; r <= max_r; r++) gens.push_back(conjugate(base, r));
  }

  // Pre-encode each generator to int op codes once. The BFS below applies these
  // words across ~1M states, and doing it by integer avoids the string
  // comparisons in the string apply -- the dominant cost of the build.
  std::vector<std::vector<int>> gen_codes(gens.size());
  for (size_t gi = 0; gi < gens.size(); gi++) {
    for (const auto& op : gens[gi]) {
      gen_codes[gi].push_back(op == "L" ? 1 : (op == "R" ? 2 : 3));
    }
  }

  // Each BFS node records only which generator reached it and from which parent,
  // never the accumulated word. Earlier the frontier carried the full path and
  // grew it at every step, so ~1M nodes each copied a word tens of ops long --
  // the dominant cost. The word for a key is rebuilt once, on demand, by walking
  // these parent links back to the root (see reconstruct below).
  struct Node { std::vector<int> state; int gen; int parent; };
  std::vector<Node> nodes;
  nodes.push_back({goal, -1, -1});

  // Walk parent links from `node` to the root, appending each step's inverse
  // generator. The root's word is empty, matching the solved goal.
  // BFS records only node links; words are reconstructed once at the end, not
  // per node inside the loop. key_of_node maps each key to its node so the final
  // pass can rebuild that key's word by walking parent links to the root.
  std::unordered_map<TailKey, int> key_of_node;
  key_of_node[tail_key(goal, TAIL)] = 0;

  std::vector<int> frontier;
  frontier.push_back(0);

  std::vector<int> s_buf;  // reused child-state scratch, see the loop
  bool capped = false;
  for (int d = 0; d < TABLE_DEPTH && !frontier.empty() && !capped; d++) {
    Rcpp::checkUserInterrupt();
    std::vector<int> next;

    for (int fi : frontier) {
      const std::vector<int> parent_state = nodes[fi].state;
      for (int gi = 0; gi < (int)gens.size(); gi++) {
        // Reused across generators so its buffer is allocated once, not per
        // child. Copied into nodes only when the child is a keeper.
        s_buf.assign(parent_state.begin(), parent_state.end());
        for (int op : gen_codes[gi]) apply_op_code_inplace(s_buf, op, k);

        bool block_ok = true;
        for (int i = 0; i < bs && block_ok; i++) {
          if (s_buf[TAIL + i] != i + 1) block_ok = false;
        }
        if (!block_ok) continue;

        TailKey key = tail_key(s_buf, TAIL);
        int idx = (int)nodes.size();
        // One hash lookup: inserts only if new, and tells us if it was.
        if (!key_of_node.emplace(key, idx).second) continue;

        nodes.push_back({s_buf, gi, fi});
        next.push_back(idx);
      }
      if ((int)key_of_node.size() > TABLE_MAX_ENTRIES) { capped = true; break; }
    }
    frontier = std::move(next);
    if ((int)frontier.size() > TABLE_MAX_FRONTIER) break;
  }

  // One reconstruction per stored key: walk parent links to the root, appending
  // each step's inverse generator (root first has an empty word).
  tbl.reserve(key_of_node.size());
  for (const auto& kv : key_of_node) {
    std::vector<std::string> w;
    for (int i = kv.second; i >= 0 && nodes[i].gen >= 0; i = nodes[i].parent) {
      const std::vector<std::string> inv = invert_word(gens[nodes[i].gen]);
      w.insert(w.end(), inv.begin(), inv.end());
    }
    tbl[kv.first] = std::move(w);
  }
  return tbl;
}

// Place m directly behind prev using rotations plus one flip.
bool place_value(Solver& S, int prev, int m, int blen) {
  const int n = S.n, k = S.k;

  for (int attempt = 0; attempt < 4 * n; attempt++) {
    int p_prev = S.pos_of(prev), p_m = S.pos_of(m);
    if (p_prev < 0 || p_m < 0) return false;

    int gap = S.fwd_gap(p_prev, p_m);
    if (gap == 1) return true;
    if (gap == k) {
      S.rotate((p_prev + 1) % n);
      S.emit("X");
      return true;
    }

    int best = -1, best_score = INT_MAX;
    for (int off = 0; off < k; off++) {
      int start = ((p_m - off) % n + n) % n;

      bool hits = false;
      for (int i = 0; i < k && !hits; i++) {
        int q = (start + i) % n;
        if (S.fwd_gap(q, p_prev) < blen) hits = true;
      }
      if (hits) continue;

      int newp = (start + (k - 1 - off)) % n;
      int ng = S.fwd_gap(p_prev, newp);
      if (ng == 0) continue;

      int sc = std::abs(ng - k);
      if (sc < best_score) { best_score = sc; best = start; }
    }

    if (best < 0) { S.emit("L"); continue; }
    S.rotate(best);
    S.emit("X");
  }
  return false;
}

// Phase 1: grow the run until only TAIL tiles are loose.
void phase1(Solver& S) {
  const int n = S.n;
  const int TAIL = tail_size(S.k);
  for (int pass = 0; pass < 6 * n; pass++) {
    int r = run_length(S.state);
    if (r >= n || (n - r) <= TAIL) return;
    if (!place_value(S, r, r + 1, r)) S.emit("L");
  }
}

// Try to finish from the current state via the table.
bool try_table(Solver& S, const Table& tbl) {
  const int n = S.n;
  const int TAIL = tail_size(S.k);
  int bs = n - TAIL;
  // Phase 1 can overshoot and leave a longer run than the table expects; the
  // surplus values are already in place, so treat the run as ending at bs.
  if (run_length(S.state) < bs) return false;

  Solver probe = S;
  int p = probe.pos_of(bs);
  probe.rotate((p + 1) % n);

  for (int i = 0; i < bs; i++) {
    if (probe.state[TAIL + i] != i + 1) return false;
  }

  auto it = tbl.find(tail_key(probe.state, TAIL));
  if (it == tbl.end()) return false;

  probe.emit_word(it->second);
  if (!is_solved(probe.state)) return false;

  S = probe;
  return true;
}

}  // namespace

// Build the finish table alone and report its shape: table size and whether the
// entry cap was hit, for a given (n, k). A diagnostic for build_table cost --
// measurement showed the table size, and so the build time, is governed by k
// (via TAIL = tail_size(k), the key space is TAIL!), not by the block width
// bs = n - TAIL, which barely moves it.
// [[Rcpp::export]]
List human_table_probe_cpp(int n, int k) {
  if (n > 63) stop("human_table_probe: ring size must be <= 63");
  const int TAIL = tail_size(k);
  Table tbl = build_table(n, k);
  return List::create(
    _["n"] = n,
    _["k"] = k,
    _["tail"] = TAIL,
    _["bs"] = n - TAIL,
    _["entries"] = (double)tbl.size(),
    _["capped"] = (bool)((int)tbl.size() > TABLE_MAX_ENTRIES)
  );
}


// [[Rcpp::export]]
int run_length_cpp(IntegerVector state) {
  std::vector<int> st(state.begin(), state.end());
  return run_length(st);
}

// Phase 1 seen as a navigator rather than a solver.
//
// Ranking the three raw operations gives almost no signal: a single rotation
// changes neither the run nor the gap, and phase 1 does not think in single
// operations. Its unit of work is a *composite* move -- rotate the ring so the
// flipper covers a chosen window, then flip -- which is what place_value()
// searches over. So the candidates offered here are those composite moves.
//
// For each of the k window offsets that place_value() would consider, the
// candidate is "rotate to `start`, then X", reported as the full operation
// word. Windows overlapping the finished run are dropped, exactly as in
// place_value(); the gap==k shortcut is offered as its own candidate, since
// that is the move phase 1 fires to actually place a value.
//
// Returned per candidate:
//   ops      -- the operation word, e.g. "2,2,3" (R R X)
//   len      -- its length in operations
//   run      -- run_length after applying it; longer is better
//   gap_cost -- |gap(prev, m) - k| afterwards, the quantity place_value drives
//               to zero before firing its flip; lower is better
//   places   -- TRUE if this move actually appends a value to the run
//
// [[Rcpp::export]]
DataFrame human_phase1_rank_cpp(IntegerVector state, int k) {
  std::vector<int> st(state.begin(), state.end());
  const int n = (int)st.size();
  if (k < 3 || k > n) stop("human_phase1_rank: need 3 <= k <= n");

  const int TAIL = tail_size(k);
  const int r0 = run_length(st);

  std::vector<std::string> r_ops;
  std::vector<int> r_len, r_run, r_gap;
  std::vector<bool> r_places;

  // Nothing for phase 1 to aim at once the tail is reached.
  if (r0 >= n || (n - r0) <= TAIL) {
    return DataFrame::create(
      Named("ops") = CharacterVector::create(),
      Named("len") = IntegerVector::create(),
      Named("run") = IntegerVector::create(),
      Named("gap_cost") = IntegerVector::create(),
      Named("places") = LogicalVector::create(),
      Named("stringsAsFactors") = false
    );
  }

  const int prev = r0, m = r0 + 1;

  // Evaluate a candidate word: apply it to a scratch solver and score.
  auto consider = [&](const std::vector<std::string>& word) {
    if (word.empty()) return;
    Solver T(st, k);
    T.emit_word(word);

    const int run = run_length(T.state);

    int gap_cost = NA_INTEGER;
    if (run < n && (n - run) <= TAIL) {
      gap_cost = 0;
    } else if (run < n) {
      int p_prev = T.pos_of(run), p_m = T.pos_of(run + 1);
      if (p_prev >= 0 && p_m >= 0) {
        int g = T.fwd_gap(p_prev, p_m);
        gap_cost = (g == 1) ? 0 : std::abs(g - k);
      }
    } else {
      gap_cost = 0;
    }

    std::string joined;
    for (size_t i = 0; i < word.size(); i++) {
      if (i) joined += ",";
      joined += (word[i] == "L") ? "1" : (word[i] == "R") ? "2" : "3";
    }

    r_ops.push_back(joined);
    r_len.push_back((int)word.size());
    r_run.push_back(run);
    r_gap.push_back(gap_cost);
    r_places.push_back(run > r0);
  };

  // Word for "rotate by t, then flip", matching Solver::rotate's direction choice.
  auto rotate_then_flip = [&](int t) {
    std::vector<std::string> w;
    t = ((t % n) + n) % n;
    if (t <= n - t) { for (int i = 0; i < t; i++) w.push_back("L"); }
    else { for (int i = 0; i < n - t; i++) w.push_back("R"); }
    w.push_back("X");
    return w;
  };

  Solver S(st, k);
  const int p_prev = S.pos_of(prev), p_m = S.pos_of(m);
  if (p_prev < 0 || p_m < 0) stop("human_phase1_rank: malformed state");

  const int gap = S.fwd_gap(p_prev, p_m);

  // The placing move: gap == k means one flip drops m behind prev.
  if (gap == k) consider(rotate_then_flip((p_prev + 1) % n));

  // Otherwise the nudges place_value() would weigh, one per window offset.
  for (int off = 0; off < k; off++) {
    int start = ((p_m - off) % n + n) % n;

    bool hits = false;
    for (int i = 0; i < k && !hits; i++) {
      int q = (start + i) % n;
      if (S.fwd_gap(q, p_prev) < r0) hits = true;
    }
    if (hits) continue;  // window would disturb the finished run

    int newp = (start + (k - 1 - off)) % n;
    if (S.fwd_gap(p_prev, newp) == 0) continue;

    consider(rotate_then_flip(start));
  }

  return DataFrame::create(
    Named("ops") = wrap(r_ops),
    Named("len") = wrap(r_len),
    Named("run") = wrap(r_run),
    Named("gap_cost") = wrap(r_gap),
    Named("places") = wrap(r_places),
    Named("stringsAsFactors") = false
  );
}

// [[Rcpp::export]]
List human_algorithm_cpp(IntegerVector start_state, int k, double max_ops,
                         bool final_rotate) {
  std::vector<int> st(start_state.begin(), start_state.end());
  int n = (int)st.size();

  if (n < tail_size(k) + 2 || k < 3 || k > n) {
    stop("human_algorithm: need n >= k + 6 and 3 <= k <= n");
  }
  // The finish table packs tile numbers into a 6-bit-per-slot key, so tile
  // values must stay under 64.
  if (n > 63) {
    stop("human_algorithm: ring size must be <= 63");
  }

  Solver S(st, k);
  phase1(S);

  // The table depends only on (n, k); building it dominates the runtime, so
  // cache one per (n, k). A single slot thrashed whenever callers alternated
  // sizes -- e.g. a test file cycling n and k rebuilt every time, and a large
  // TAIL (k >= 6) makes each rebuild cost seconds.
  static std::unordered_map<long long, Table> table_cache;
  // k <= n always, so shifting n by 32 bits keeps (n, k) pairs distinct.
  long long cache_key = ((long long)n << 32) | (unsigned)k;
  auto ct = table_cache.find(cache_key);
  if (ct == table_cache.end()) {
    ct = table_cache.emplace(cache_key, build_table(n, k)).first;
  }
  const Table& tbl = ct->second;

  bool ok = try_table(S, tbl);

  if (!ok) {
    // The tail arrangement is odd, which the 3-cycles cannot reach. Fire one
    // flip across the block boundary and rebuild: moving a tile between block
    // and tail changes the parity of the split.
    //
    // Each attempt restarts from the state phase 1 produced, and an attempt
    // only counts if the rebuild actually restored the run -- otherwise the
    // flip landed somewhere that phase 1 cannot recover from.
    const int need = n - tail_size(k);
    for (int sh = 0; sh < n && !ok; sh++) {
      for (int form = 0; form < 2 && !ok; form++) {
        Rcpp::checkUserInterrupt();
        Solver T = S;
        const char* out = form == 0 ? "R" : "L";
        const char* back = form == 0 ? "L" : "R";
        for (int i = 0; i < sh; i++) T.emit(out);
        T.emit("X");
        for (int i = 0; i < sh; i++) T.emit(back);

        phase1(T);
        if (run_length(T.state) < need) continue;
        if (try_table(T, tbl)) { S = T; ok = true; }
      }
    }
  }

  if (ok && final_rotate) {
    int p1 = S.pos_of(1);
    if (p1 > 0) S.rotate(p1);
  }

  bool solved = ok && is_solved(S.state);
  if (solved && final_rotate) {
    for (int i = 0; i < n; i++) if (S.state[i] != i + 1) { solved = false; break; }
  }

  CharacterVector cv(S.path.size());
  for (size_t i = 0; i < S.path.size(); i++) cv[i] = S.path[i];

  IntegerVector fs(S.state.begin(), S.state.end());

  return List::create(
    Named("found") = solved,
    Named("path") = cv,
    Named("length") = (int)S.path.size(),
    Named("final_state") = fs
  );
}
