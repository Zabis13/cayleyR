#ifndef CAYLEYR_CUBE_SEARCH_H
#define CAYLEYR_CUBE_SEARCH_H

#include <vector>
#include <string>
#include <map>
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

// The alphabet of one cube size, built once and kept. Face turns and inner
// layers, 6n moves over 6n^2 stickers, in the package's own order and naming.
//
// Axis and layer come from `build_alphabet`, which knew both while it was
// laying the moves out. They are what lets the search skip redundant
// sequences, and taking them from the alphabet rather than from a table
// written out by hand is what makes this work at any n.
struct CubeN {
  CubeAlphabet a;
  int n;
  int n_stickers;
  std::vector<std::vector<int> > perm;   // 0-based, new[i] = state[perm[i]]

  explicit CubeN(int side) {
    n = side;
    n_stickers = 6 * side * side;
    a = build_alphabet(side);
    perm.resize(a.perms.size());
    for (size_t m = 0; m < a.perms.size(); m++) {
      perm[m].resize(n_stickers);
      for (int i = 0; i < n_stickers; i++) perm[m][i] = a.perms[m][i] - 1;
    }
  }

  int n_moves() const { return (int)a.names.size(); }
  const std::string& name(int m) const { return a.names[m]; }
  int axis_of(int m) const { return a.axis_of[m]; }
  int layer_of(int m) const { return a.layer_of[m]; }

  void apply(std::vector<int>& s, int m) const {
    std::vector<int> t(n_stickers);
    const std::vector<int>& p = perm[m];
    for (int i = 0; i < n_stickers; i++) t[i] = s[p[i]];
    s.swap(t);
  }

  int move_index(const std::string& nm) const {
    for (size_t m = 0; m < a.names.size(); m++) if (a.names[m] == nm) return (int)m;
    return -1;
  }
};

// One alphabet per size, built on first use. A 7x7x7 alphabet is 42 moves of
// 294 entries and there is no reason to build it twice; a map keeps each size
// for as long as the process lives.
inline const CubeN& cube_n(int n) {
  static std::map<int, CubeN*> cache;
  std::map<int, CubeN*>::iterator it = cache.find(n);
  if (it != cache.end()) return *it->second;
  if (n < 2) {
    throw std::runtime_error("cube: a cube has side 2 or more, got " +
                             std::to_string(n));
  }
  CubeN* c = new CubeN(n);
  cache[n] = c;
  return *c;
}

// The 3x3x3, which is what every method in the package solves today. Kept as
// its own name because the solvers read better for it, and because it is the
// size their piece tables are written for.
inline const CubeN& cube3() { return cube_n(3); }

// The side of a cube with this many stickers, or -1 if no cube has that many.
// A state vector is all a caller has, and 96 entries means 4x4x4 with nothing
// else to consult.
inline int cube_side_of(size_t n_stickers) {
  for (int n = 2; 6 * n * n <= (int)n_stickers; n++) {
    if ((size_t)(6 * n * n) == n_stickers) return n;
  }
  return -1;
}

// A word as move indices, and the same word as text.
inline std::vector<std::string> word_names(const std::vector<int>& w, int n = 3) {
  const CubeN& C = cube_n(n);
  std::vector<std::string> out;
  out.reserve(w.size());
  for (size_t i = 0; i < w.size(); i++) out.push_back(C.name(w[i]));
  return out;
}

// Parse "R U R' U'" into move indices. Half turns are written out, so "U2"
// becomes two moves -- the alphabet is quarter turns and this is where a
// literature algorithm crosses into it.
inline std::vector<int> parse_word(const std::string& text, int n = 3) {
  const CubeN& C = cube_n(n);
  std::vector<int> out;
  std::string tok;
  for (size_t i = 0; i <= text.size(); i++) {
    if (i == text.size() || text[i] == ' ') {
      if (!tok.empty()) {
        bool half = (tok.size() > 1 && tok[tok.size() - 1] == '2');
        std::string base = half ? tok.substr(0, tok.size() - 1) : tok;
        int m = C.move_index(base);
        if (m < 0) {
          throw std::runtime_error("cube: '" + tok + "' is not a move of the " +
                                   std::to_string(n) + "x" + std::to_string(n) +
                                   "x" + std::to_string(n) + " alphabet");
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

inline std::vector<int> moves_all(int n = 3) {
  std::vector<int> v;
  for (int m = 0; m < cube_n(n).n_moves(); m++) v.push_back(m);
  return v;
}

inline std::vector<int> moves_named(const std::vector<std::string>& names,
                                    int n = 3) {
  const CubeN& C = cube_n(n);
  std::vector<int> v;
  for (size_t i = 0; i < names.size(); i++) {
    int m = C.move_index(names[i]);
    if (m < 0) throw std::runtime_error("cube: unknown move '" + names[i] + "'");
    v.push_back(m);
  }
  return v;
}

// The twelve face turns: no inner layers. This is the set a last-layer
// algorithm works in, and the one most human methods stay inside. The six
// faces are named the same whatever the size, so this is the same twelve
// moves on a 7x7x7 as on a 3x3x3 -- they are just a smaller share of the
// alphabet.
inline std::vector<int> moves_faces(int n = 3) {
  static const char* nm[12] = {"U","U'","R","R'","F","F'",
                               "D","D'","L","L'","B","B'"};
  std::vector<std::string> v(nm, nm + 12);
  return moves_named(v, n);
}

// Everything that is not a face turn: the slices of a 3x3x3, and on a larger
// cube the inner layers that make up most of the alphabet. The alphabet puts
// the six faces first, so these are the moves from index 12 on.
inline std::vector<int> moves_inner(int n = 3) {
  std::vector<int> v;
  for (int m = 12; m < cube_n(n).n_moves(); m++) v.push_back(m);
  return v;
}

// ---- No search here -----------------------------------------------------
//
// There was an iterative-deepening search in this file, and it is gone. Every
// method in the package now places pieces by rule or by table: CFOP's cross
// uses solve_cross_edge() like LBL does, its F2L has a table of 41 cases, and
// the two blindfold methods never searched at all.
//
// Removing it was the point rather than a side effect. Enumeration does not
// survive the cube growing: a 3x3x3 branches 18 ways per move and a 7x7x7
// branches 42, and the stages that matter on a large cube are deeper than the
// ones that were being searched for here. CFOP's own F2L had already reached
// that conclusion and written a table -- see the note in cube_solve_cfop.h,
// where an exact search exhausted its budget at depth 7 and took over a minute
// to do it. What is left in this file is the alphabet and how to apply it,
// which is what the methods actually build on.

// Apply a word, returning the new state. The size comes from the state: a
// state of 96 entries is a 4x4x4 and there is nothing else it could be, so a
// caller that already holds one need not say so twice. The move indices are
// indices into that size's alphabet.
inline std::vector<int> apply_word(const std::vector<int>& state,
                                   const std::vector<int>& word) {
  const int side = cube_side_of(state.size());
  if (side < 0) {
    throw std::runtime_error("cube: " + std::to_string(state.size()) +
                             " stickers is not a cube of any size");
  }
  const CubeN& C = cube_n(side);
  std::vector<int> s = state;
  for (size_t i = 0; i < word.size(); i++) C.apply(s, word[i]);
  return s;
}

}  // namespace cube_search

#endif  // CAYLEYR_CUBE_SEARCH_H
