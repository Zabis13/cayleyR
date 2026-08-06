#ifndef CAYLEYR_CUBE_SEARCH_H
#define CAYLEYR_CUBE_SEARCH_H

#include <vector>
#include <string>
#include <stdexcept>
#include "cube_cubie.h"

// ---- Searching for a stage, not for a solution -------------------------
//
// A human method is a chain of subgoals, and what makes it tractable is that
// each one is shallow: a cross is eight moves at worst, an F2L pair eleven.
// Depth-first iterative deepening finds those without a table, and without the
// memory a full BFS of the cube would need.
//
// The search here is deliberately not a whole-cube solver. It is given a
// predicate -- "the cross is done", "this slot is filled" -- and it returns a
// shortest word reaching it. Chaining those is what the solvers above do, and
// it is also why their results are longer than an optimal solution: each stage
// is optimal on its own and blind to the next, which is exactly the trade a
// human makes.
//
// ---- Why iterative deepening --------------------------------------------
//
// Breadth-first would visit the same states but hold a frontier of millions;
// IDA* holds a stack of depth d. Repeating shallow levels costs little because
// the branching factor is large: on a cube with 18 quarter-turn moves the last
// level is most of the work whatever order it is visited in.

namespace cube_search {

using namespace cube_cubie;
using namespace cube_nnn;

// The 3x3x3 alphabet, built once. Face turns and slices, 18 moves, in the
// package's own order and naming.
struct Cube3 {
  CubeAlphabet a;
  std::vector<std::vector<int> > perm;   // 0-based, new[i] = state[perm[i]]
  std::vector<int> axis_of;              // which axis each move turns
  std::vector<int> layer_of_move;

  Cube3() {
    a = build_alphabet(3);
    perm.resize(a.perms.size());
    for (size_t m = 0; m < a.perms.size(); m++) {
      perm[m].resize(54);
      for (int i = 0; i < 54; i++) perm[m][i] = a.perms[m][i] - 1;
    }
    // The alphabet is built face by face, two moves each: U U' R R' F F' D D'
    // L L' B B' then M M' E E' S S'. Axis and layer come from that order, and
    // they are what lets the search skip redundant sequences.
    static const int ax[9] = {1, 0, 2, 1, 0, 2, 0, 1, 2};   // U R F D L B M E S
    static const int ly[9] = {2, 2, 2, 0, 0, 0, 1, 1, 1};
    axis_of.resize(a.names.size());
    layer_of_move.resize(a.names.size());
    for (size_t m = 0; m < a.names.size(); m++) {
      axis_of[m] = ax[m / 2];
      layer_of_move[m] = ly[m / 2];
    }
  }

  int n_moves() const { return (int)a.names.size(); }
  const std::string& name(int m) const { return a.names[m]; }

  void apply(std::vector<int>& s, int m) const {
    std::vector<int> t(54);
    const std::vector<int>& p = perm[m];
    for (int i = 0; i < 54; i++) t[i] = s[p[i]];
    s.swap(t);
  }

  int move_index(const std::string& nm) const {
    for (size_t m = 0; m < a.names.size(); m++) if (a.names[m] == nm) return (int)m;
    return -1;
  }
};

inline const Cube3& cube3() {
  static Cube3 c;
  return c;
}

// A word as move indices, and the same word as text.
inline std::vector<std::string> word_names(const std::vector<int>& w) {
  const Cube3& C = cube3();
  std::vector<std::string> out;
  out.reserve(w.size());
  for (size_t i = 0; i < w.size(); i++) out.push_back(C.name(w[i]));
  return out;
}

// Parse "R U R' U'" into move indices. Half turns are written out, so "U2"
// becomes two moves -- the alphabet is quarter turns and this is where a
// literature algorithm crosses into it.
inline std::vector<int> parse_word(const std::string& text) {
  const Cube3& C = cube3();
  std::vector<int> out;
  std::string tok;
  for (size_t i = 0; i <= text.size(); i++) {
    if (i == text.size() || text[i] == ' ') {
      if (!tok.empty()) {
        bool half = (tok.size() > 1 && tok[tok.size() - 1] == '2');
        std::string base = half ? tok.substr(0, tok.size() - 1) : tok;
        int m = C.move_index(base);
        if (m < 0) {
          throw std::runtime_error("cube: '" + tok + "' is not a move of the "
                                   "3x3x3 alphabet");
        }
        out.push_back(m);
        if (half) out.push_back(m);
        tok.clear();
      }
    } else {
      tok += text[i];
    }
  }
  return out;
}

// ---- Move-set restrictions ---------------------------------------------
//
// Which moves a stage may use is part of the method. The cross may use
// anything; an OLL algorithm may not disturb what is below it. Passing a mask
// keeps the searches honest about that without a second copy of the alphabet.

inline std::vector<int> moves_all() {
  std::vector<int> v;
  for (int m = 0; m < cube3().n_moves(); m++) v.push_back(m);
  return v;
}

inline std::vector<int> moves_named(const std::vector<std::string>& names) {
  const Cube3& C = cube3();
  std::vector<int> v;
  for (size_t i = 0; i < names.size(); i++) {
    int m = C.move_index(names[i]);
    if (m < 0) throw std::runtime_error("cube: unknown move '" + names[i] + "'");
    v.push_back(m);
  }
  return v;
}

// The twelve face turns: no slices. This is the set a last-layer algorithm
// works in, and the one most human methods stay inside.
inline std::vector<int> moves_faces() {
  static const char* nm[12] = {"U","U'","R","R'","F","F'",
                               "D","D'","L","L'","B","B'"};
  std::vector<std::string> v(nm, nm + 12);
  return moves_named(v);
}

// ---- Iterative deepening -----------------------------------------------
//
// Two prunings, both cheap and both sound:
//
//   * a move may not be followed by its own inverse -- the pair is nothing;
//   * moves on one axis commute, so among the orderings of a run of them fix
//     one representative: a move may not follow a higher-numbered layer of its
//     own axis.
//
// Together they cut the effective branching factor from 18 to about 14.
//
// Two stronger-looking rules are wrong here, both because the alphabet is
// quarter turns rather than half ones. Forbidding the same layer twice running
// would forbid D D, and D D is D2 -- a half turn, which in this metric is
// genuinely two moves and cannot be shortened. Forbidding "layer A, other
// layer, layer A again" on one axis would forbid U E U, where the E between
// them is a different layer and the two U turns do not merge. Either rule
// makes the search return words longer than the shortest, which is the one
// thing a stage solver must not do.

struct SearchLimit {
  long long nodes;      // budget, counted down
  bool exhausted;       // true if the budget ran out before an answer
};

template <typename Pred>
static bool ida_dfs(std::vector<int>& state, int depth, int last,
                    const std::vector<int>& moves, Pred goal,
                    std::vector<int>& path, SearchLimit& lim) {
  if (lim.nodes <= 0) { lim.exhausted = true; return false; }
  lim.nodes--;

  if (depth == 0) return goal(state);

  const Cube3& C = cube3();
  for (size_t k = 0; k < moves.size(); k++) {
    const int m = moves[k];

    // a move undone by the next one: the pair is nothing at all. The alphabet
    // pairs each move with its inverse, so m ^ 1 is that inverse.
    if (last >= 0 && m == (last ^ 1)) continue;
    // moves on one axis commute, so fix a single order for each run of them.
    // Equal layers are left alone: on this axis that is the same layer turned
    // twice, which is a half turn and not reducible in the quarter-turn metric.
    if (last >= 0 && C.axis_of[m] == C.axis_of[last] &&
        C.layer_of_move[m] < C.layer_of_move[last]) continue;

    std::vector<int> next = state;
    C.apply(next, m);
    path.push_back(m);
    if (ida_dfs(next, depth - 1, m, moves, goal, path, lim)) return true;
    path.pop_back();
    if (lim.exhausted) return false;
  }
  return false;
}

// Shortest word taking `state` to something satisfying `goal`, searching to
// `max_depth`. Returns true and fills `path` on success.
template <typename Pred>
inline bool ida_solve(const std::vector<int>& state, Pred goal,
                      const std::vector<int>& moves, int max_depth,
                      std::vector<int>& path, long long node_budget = 200000000LL) {
  path.clear();
  if (goal(state)) return true;
  for (int d = 1; d <= max_depth; d++) {
    std::vector<int> s = state;
    SearchLimit lim = {node_budget, false};
    path.clear();
    if (ida_dfs(s, d, -1, moves, goal, path, lim)) return true;
    if (lim.exhausted) {
      throw std::runtime_error("cube: search budget exhausted at depth " +
                               std::to_string(d));
    }
  }
  path.clear();
  return false;
}

// The common case: a goal phrased on cubies.
template <typename CubiePred>
inline bool ida_solve_cubie(const std::vector<int>& state, CubiePred goal,
                            const std::vector<int>& moves, int max_depth,
                            std::vector<int>& path) {
  return ida_solve(state,
                   [&](const std::vector<int>& s) {
                     return goal(read_state(s));
                   },
                   moves, max_depth, path);
}

// Apply a word, returning the new state.
inline std::vector<int> apply_word(const std::vector<int>& state,
                                   const std::vector<int>& word) {
  const Cube3& C = cube3();
  std::vector<int> s = state;
  for (size_t i = 0; i < word.size(); i++) C.apply(s, word[i]);
  return s;
}

}  // namespace cube_search

#endif  // CAYLEYR_CUBE_SEARCH_H
