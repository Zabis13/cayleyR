#ifndef CAYLEYR_KOCIEMBA3_H
#define CAYLEYR_KOCIEMBA3_H

#include "kociemba_core.h"
#include "cube_cubie.h"
#include "cube_search.h"

// ---- The 3x3x3 in two phases --------------------------------------------
//
// Kociemba's algorithm is the observation that a cube is much easier to solve
// if you first make it easy. The subgroup G1 = <U, D, L2, R2, F2, B2> is the
// set of states reachable without ever quarter-turning L, R, F or B, and it is
// small: about 20 million states against 43 quintillion. Getting into it is
// the first phase; finishing inside it is the second.
//
// What makes a state a member of G1 is three things, and each is exactly what
// the excluded quarter turns would break:
//
//   * every edge oriented        -- F and B quarter turns flip edges
//   * every corner oriented      -- L, R, F, B quarter turns twist corners
//   * the four E-slice edges in the E slice -- those turns move edges between
//     slices, though which of the four sits where does not matter yet
//
// Phase 1 searches for that and nothing else, on a coordinate that has thrown
// away everything else about the cube. Phase 2 then works inside G1, where
// half turns of L, R, F and B are single moves, and finishes.
//
// ---- Where the move table comes from ------------------------------------
//
// Not from a table written out by hand. The package already generates the
// sticker permutation of every move from the cube's geometry, and cube_cubie.h
// already reads a sticker vector as pieces. Applying a move to the solved cube
// and reading the result back gives that move as a piece permutation, measured
// rather than asserted -- if the two representations ever disagree the tests
// catch it, and nothing here has to be kept in step by hand.

namespace kociemba3 {

using namespace kociemba;

// Corners are orbit 0, edges orbit 1: 8 + 12 = 20 flat slots.
const int N_CORNERS = 8;
const int N_EDGES = 12;
const int C_OFF = 0;
const int E_OFF = 8;

inline PieceState from_cubie(const cube_cubie::CubieState& c) {
  PieceState s;
  s.perm.resize(20);
  s.ori.resize(20);
  for (int i = 0; i < N_CORNERS; i++) {
    s.perm[C_OFF + i] = (kociemba::Slot)c.cp[i];
    s.ori[C_OFF + i] = (uint8_t)c.co[i];
  }
  // Flat numbering throughout: edge k is slot E_OFF + k, both as an identity
  // and as an index, so a state and a move speak the same language and
  // identity() -- perm[i] == i -- means solved.
  for (int i = 0; i < N_EDGES; i++) {
    s.perm[E_OFF + i] = (kociemba::Slot)(E_OFF + c.ep[i]);
    s.ori[E_OFF + i] = (uint8_t)c.eo[i];
  }
  return s;
}

inline PieceState from_stickers(const std::vector<int>& state54) {
  return from_cubie(cube_cubie::read_state(state54));
}

// A quarter turn as a piece permutation, obtained by turning a solved cube and
// reading it. `qt` is the index in the 3x3x3 sticker alphabet.
//
// from_stickers already numbers the edges flat, so the entries of the move are
// indices into the whole state and apply_move can follow them directly.
inline OrbitMove quarter_turn_as_pieces(int qt) {
  const cube_search::CubeN& C = cube_search::cube3();
  std::vector<int> s(54);
  for (int i = 0; i < 54; i++) s[i] = i + 1;
  C.apply(s, qt);
  PieceState p = from_stickers(s);
  OrbitMove mv;
  mv.perm = p.perm;
  mv.ori = p.ori;
  return mv;
}

// The six faces, in the order the rest of this file assumes.
inline const char* const* face_names() {
  static const char* t[6] = {"U", "D", "L", "R", "F", "B"};
  return t;
}
inline int face_axis(int f) {
  // U/D share an axis, L/R share one, F/B share one.
  static const int t[6] = {0, 0, 1, 1, 2, 2};
  return t[f];
}
inline int face_layer(int f) {
  static const int t[6] = {0, 1, 0, 1, 0, 1};
  return t[f];
}

// The generator list of a phase, built from face turns. `powers` says which
// turn amounts that face contributes: 1 and 3 are the quarter turns, 2 the
// half turn. Phase 1 takes all three from every face; phase 2 takes all three
// from U and D and only the half turn from the rest -- which is what makes a
// half turn there cost one move rather than two.
struct FaceGen { int face; int power; };

inline void build_spec(const std::vector<FaceGen>& gens, PuzzleSpec& spec) {
  spec.orbits.clear();
  OrbitDef corners; corners.name = "CORNERS"; corners.n_pieces = 8; corners.n_orientations = 3;
  OrbitDef edges;   edges.name   = "EDGES";   edges.n_pieces  = 12; edges.n_orientations  = 2;
  spec.orbits.push_back(corners);
  spec.orbits.push_back(edges);
  spec.finish_layout();

  const std::vector<uint8_t> omod = spec.ori_mod();
  const cube_search::CubeN& C = cube_search::cube3();

  spec.moves.clear();
  spec.move_names.clear();
  spec.move_axis.clear();
  spec.move_layer.clear();

  for (size_t i = 0; i < gens.size(); i++) {
    const int f = gens[i].face;
    const int p = gens[i].power;
    const int qt = C.move_index(face_names()[f]);
    if (qt < 0) throw std::runtime_error("kociemba3: no such face turn");
    OrbitMove base = quarter_turn_as_pieces(qt);
    OrbitMove mv = base;
    for (int k = 1; k < p; k++) mv = compose(mv, base, omod);

    std::string nm = face_names()[f];
    if (p == 2) nm += "2";
    else if (p == 3) nm += "'";

    spec.moves.push_back(mv);
    spec.move_names.push_back(nm);
    spec.move_axis.push_back(face_axis(f));
    spec.move_layer.push_back(face_layer(f));
  }
}

inline std::vector<FaceGen> phase1_generators() {
  std::vector<FaceGen> g;
  for (int f = 0; f < 6; f++) {
    for (int p = 1; p <= 3; p++) { FaceGen x; x.face = f; x.power = p; g.push_back(x); }
  }
  return g;
}

// <U, D, L2, R2, F2, B2>. Ten generators, and the half turns are single moves.
inline std::vector<FaceGen> phase2_generators() {
  std::vector<FaceGen> g;
  for (int f = 0; f < 2; f++) {          // U, D: all three
    for (int p = 1; p <= 3; p++) { FaceGen x; x.face = f; x.power = p; g.push_back(x); }
  }
  for (int f = 2; f < 6; f++) {          // L, R, F, B: half turns only
    FaceGen x; x.face = f; x.power = 2; g.push_back(x);
  }
  return g;
}

// ---- Phase 1's coordinate ------------------------------------------------
//
// Corner orientation, edge orientation, and for each edge whether it belongs
// to the E slice -- not which E-slice edge it is. The last distinction is what
// keeps the coordinate small, and dropping it is safe because phase 2's
// generators can permute within the slice freely.
//
// E-slice edges are FR, FL, BL, BR, which cube_cubie.h numbers 8..11.

struct Phase1Deriver : public Deriver {
  void derive(const PieceState& in, PieceState& out) const {
    out.perm.assign(20, 0);
    out.ori.assign(20, 0);
    // corner orientation only; which corner is where is phase 2's problem
    for (int i = 0; i < N_CORNERS; i++) out.ori[C_OFF + i] = in.ori[C_OFF + i];
    for (int i = 0; i < N_EDGES; i++) {
      out.ori[E_OFF + i] = in.ori[E_OFF + i];
      // 1 if the edge in this slot is an E-slice edge, 0 otherwise
      out.perm[E_OFF + i] = (kociemba::Slot)(in.perm[E_OFF + i] >= E_OFF + 8 ? 1 : 0);
    }
  }
};

// Phase 2 searches the whole state: inside G1 there is nothing left to ignore.
struct IdentityDeriver : public Deriver {
  void derive(const PieceState& in, PieceState& out) const { out = in; }
};

// ---- The solver ---------------------------------------------------------

struct Solver3 {
  PuzzleSpec spec1, spec2;
  Phase1Deriver d1;
  IdentityDeriver d2;
  PruneTable p1, p2;
  std::vector<PieceState> goals1, goals2;
  bool ready;

  // What the last solve did, so a caller can tell "no solution" from "ran out
  // of budget" without reading the code.
  SearchOutcome last_outcome1, last_outcome2;
  long last_nodes1, last_nodes2;

  Solver3() : ready(false) {}

  // Depths of 0 seed the tables with the goals alone; the search grows them
  // from there. The sizes are the sizes twips gives these two phases.
  void init(size_t t1 = (size_t)1 << 22, int depth1 = 0,
            size_t t2 = (size_t)1 << 24, int depth2 = 0) {
    if (ready) return;
    build_spec(phase1_generators(), spec1);
    build_spec(phase2_generators(), spec2);

    // Phase 1's goal is membership of G1, and the solved cube is a member;
    // the deriver throws away everything that distinguishes other members, so
    // one goal state stands for all of them.
    goals1.clear();
    goals1.push_back(spec1.identity());
    goals2.clear();
    goals2.push_back(spec2.identity());

    // The tables are grown by the search, to half of whatever depth it is
    // about to look at. Building them to a fixed depth up front is what the
    // first version did, and it paid for levels that were never consulted
    // while leaving phase 2 -- the deep one -- with a table far too shallow to
    // prune with.
    build_prune_table(spec1, d1, goals1, t1, depth1, p1);
    build_prune_table(spec2, d2, goals2, t2, depth2, p2);
    ready = true;
  }

  // Solve, returning the move names of both phases run together.
  bool solve(const PieceState& start, std::vector<std::string>& out,
             const SearchLimits& lim1, const SearchLimits& lim2) {
    init();
    out.clear();

    last_outcome1 = SEARCH_NO_SOLUTION;
    last_outcome2 = SEARCH_NO_SOLUTION;
    last_nodes1 = last_nodes2 = 0;

    std::vector<int> w1;
    last_outcome1 = ida_search_outcome(spec1, d1, p1, goals1, start, lim1, w1,
                                       &last_nodes1, &p1);
    if (last_outcome1 != SEARCH_FOUND) return false;

    // Carry the cube through phase 1 to get phase 2's starting state.
    const std::vector<uint8_t> omod = spec1.ori_mod();
    PieceState cur = start, next;
    for (size_t i = 0; i < w1.size(); i++) {
      apply_move(cur, spec1.moves[w1[i]], omod, next);
      cur = next;
      out.push_back(spec1.move_names[w1[i]]);
    }

    std::vector<int> w2;
    last_outcome2 = ida_search_outcome(spec2, d2, p2, goals2, cur, lim2, w2,
                                       &last_nodes2, &p2);
    if (last_outcome2 != SEARCH_FOUND) return false;
    for (size_t i = 0; i < w2.size(); i++) out.push_back(spec2.move_names[w2[i]]);
    return true;
  }
};

// One solver per process: the prune tables are the expensive part and they do
// not depend on the cube being solved.
inline Solver3& solver3() {
  static Solver3 s;
  return s;
}

}  // namespace kociemba3

#endif  // CAYLEYR_KOCIEMBA3_H
