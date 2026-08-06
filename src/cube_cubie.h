#ifndef CAYLEYR_CUBE_CUBIE_H
#define CAYLEYR_CUBE_CUBIE_H

#include <vector>
#include <string>
#include <stdexcept>
#include "cube_nnn.h"

// ---- The 3x3x3 as cubies rather than stickers ---------------------------
//
// Everything else in the package works on stickers, and for search that is the
// right representation: it is flat, it is what a permutation acts on, and no
// algorithm has to know what a corner is. A human method does have to know.
// "The white-blue edge is in the bottom layer, flipped" is a sentence about
// cubies, and a solver that speaks in stickers cannot say it.
//
// So this file is a reading of a sticker vector, not a second representation
// to keep in step. Nothing here stores state; given the 54 stickers it says
// which cubie sits in each slot and which way round it is, and every predicate
// the solvers use is phrased in those terms.
//
// ---- Numbering ---------------------------------------------------------
//
// Corners 0..7 and edges 0..11 in the order Kociemba's literature uses, which
// is also the order that makes the U layer come first:
//
//   corners  URF UFL ULB UBR  DFR DLF DBL DRB
//   edges    UR UF UL UB  DR DF DL DB  FR FL BL BR
//
// A cubie's identity is where it belongs; its slot is where it is. Solved
// means every cubie is in its own slot with orientation 0.
//
// Orientation for a corner is how far its U-or-D coloured sticker has been
// twisted from the U/D axis: 0 if it faces U or D, 1 if one turn clockwise
// from there, 2 if two. For an edge it is 0 if the edge is "good" in the sense
// F/B turns preserve, 1 if flipped.

namespace cube_cubie {

using namespace cube_nnn;

// Sticker index on a 3x3x3: face f (0..5 as U R F D L B), row r, column c,
// all 0-based, seen from outside that face. This is the layout cube_nnn.h
// generates -- verified against it rather than assumed.
inline int sti(int f, int r, int c) { return f * 9 + r * 3 + c; }

// The three stickers of each corner, in the order (U/D face, then clockwise
// seen from outside the corner). The first entry is the one whose colour
// decides orientation.
struct CornerFacelets { int s[3]; };

inline const CornerFacelets* corner_facelets() {
  static const CornerFacelets t[8] = {
    {{sti(0,2,2), sti(1,0,0), sti(2,0,2)}},   // URF
    {{sti(0,2,0), sti(2,0,0), sti(4,0,2)}},   // UFL
    {{sti(0,0,0), sti(4,0,0), sti(5,0,2)}},   // ULB
    {{sti(0,0,2), sti(5,0,0), sti(1,0,2)}},   // UBR
    {{sti(3,0,2), sti(2,2,2), sti(1,2,0)}},   // DFR
    {{sti(3,0,0), sti(4,2,2), sti(2,2,0)}},   // DLF
    {{sti(3,2,0), sti(5,2,2), sti(4,2,0)}},   // DBL
    {{sti(3,2,2), sti(1,2,2), sti(5,2,0)}}    // DRB
  };
  return t;
}

// The two stickers of each edge. The first is the one on U or D where there is
// one, and on F or B for the four middle-layer edges; that choice is what
// makes orientation come out as the usual edge-flip parity.
struct EdgeFacelets { int s[2]; };

inline const EdgeFacelets* edge_facelets() {
  static const EdgeFacelets t[12] = {
    {{sti(0,1,2), sti(1,0,1)}},   // UR
    {{sti(0,2,1), sti(2,0,1)}},   // UF
    {{sti(0,1,0), sti(4,0,1)}},   // UL
    {{sti(0,0,1), sti(5,0,1)}},   // UB
    {{sti(3,1,2), sti(1,2,1)}},   // DR
    {{sti(3,0,1), sti(2,2,1)}},   // DF
    {{sti(3,1,0), sti(4,2,1)}},   // DL
    {{sti(3,2,1), sti(5,2,1)}},   // DB
    {{sti(2,1,2), sti(1,1,0)}},   // FR
    {{sti(2,1,0), sti(4,1,2)}},   // FL
    {{sti(5,1,2), sti(4,1,0)}},   // BL
    {{sti(5,1,0), sti(1,1,2)}}    // BR
  };
  return t;
}

// Names, for messages and for the human-readable side of a solution.
inline const char* const* corner_names() {
  static const char* t[8] = {"URF","UFL","ULB","UBR","DFR","DLF","DBL","DRB"};
  return t;
}
inline const char* const* edge_names() {
  static const char* t[12] = {"UR","UF","UL","UB","DR","DF","DL","DB",
                              "FR","FL","BL","BR"};
  return t;
}

// A sticker's colour is the face it started on: stickers are numbered 1..54 in
// face blocks of nine, so integer division recovers it.
inline int colour_of(int sticker_value) { return (sticker_value - 1) / 9; }

// ---- Reading a state ---------------------------------------------------
//
// cp[i] is which corner sits in slot i, co[i] how it is twisted; likewise
// ep/eo for edges. Reading is a lookup: take the colours in the slot, find the
// cubie that carries that set, and see how far round it has been turned.

struct CubieState {
  int cp[8], co[8];
  int ep[12], eo[12];
};

// Which corner carries this multiset of three colours, and how far it is
// rotated relative to the slot's own facelet order.
inline void identify_corner(const int col[3], int& piece, int& ori) {
  const CornerFacelets* cf = corner_facelets();
  for (int p = 0; p < 8; p++) {
    // the home colours of corner p, in its own facelet order
    int home[3];
    for (int k = 0; k < 3; k++) home[k] = cf[p].s[k] / 9;
    for (int r = 0; r < 3; r++) {
      if (col[0] == home[(0 + r) % 3] &&
          col[1] == home[(1 + r) % 3] &&
          col[2] == home[(2 + r) % 3]) {
        piece = p;
        // r is how far the piece's own facelet 0 sits from the slot's
        // facelet 0; orientation counts the same rotation the other way.
        ori = (3 - r) % 3;
        return;
      }
    }
  }
  throw std::runtime_error("cube: corner colours do not name a corner");
}

inline void identify_edge(const int col[2], int& piece, int& ori) {
  const EdgeFacelets* ef = edge_facelets();
  for (int p = 0; p < 12; p++) {
    int home[2];
    for (int k = 0; k < 2; k++) home[k] = ef[p].s[k] / 9;
    if (col[0] == home[0] && col[1] == home[1]) { piece = p; ori = 0; return; }
    if (col[0] == home[1] && col[1] == home[0]) { piece = p; ori = 1; return; }
  }
  throw std::runtime_error("cube: edge colours do not name an edge");
}

// state is 1-based, length 54, in the package convention.
inline CubieState read_state(const std::vector<int>& state) {
  if (state.size() != 54) {
    throw std::runtime_error("cube: a 3x3x3 state has 54 stickers, got " +
                             std::to_string(state.size()));
  }
  CubieState c;
  const CornerFacelets* cf = corner_facelets();
  const EdgeFacelets* ef = edge_facelets();

  for (int i = 0; i < 8; i++) {
    int col[3];
    for (int k = 0; k < 3; k++) col[k] = colour_of(state[cf[i].s[k]]);
    identify_corner(col, c.cp[i], c.co[i]);
  }
  for (int i = 0; i < 12; i++) {
    int col[2];
    for (int k = 0; k < 2; k++) col[k] = colour_of(state[ef[i].s[k]]);
    identify_edge(col, c.ep[i], c.eo[i]);
  }
  return c;
}

// ---- Slot vocabulary ---------------------------------------------------

enum CornerSlot { C_URF=0, C_UFL=1, C_ULB=2, C_UBR=3,
                  C_DFR=4, C_DLF=5, C_DBL=6, C_DRB=7 };
enum EdgeSlot { E_UR=0, E_UF=1, E_UL=2, E_UB=3,
                E_DR=4, E_DF=5, E_DL=6, E_DB=7,
                E_FR=8, E_FL=9, E_BL=10, E_BR=11 };

inline bool corner_home(const CubieState& c, int slot) {
  return c.cp[slot] == slot && c.co[slot] == 0;
}
inline bool edge_home(const CubieState& c, int slot) {
  return c.ep[slot] == slot && c.eo[slot] == 0;
}

// ---- Stage predicates --------------------------------------------------
//
// Each is the exact condition the corresponding stage of a human method ends
// on, and they are what the searches below aim at. Written on cubies because
// that is the only way to say "the cross is done but nothing else is".

// The four D edges home. This is the cross, solved on D.
inline bool cross_solved(const CubieState& c) {
  return edge_home(c, E_DR) && edge_home(c, E_DF) &&
         edge_home(c, E_DL) && edge_home(c, E_DB);
}

// One F2L pair: a bottom corner and the middle edge that goes beside it.
// Slots are numbered 0..3 as FR, FL, BL, BR going round.
struct Slot { int corner; int edge; };

inline Slot f2l_slot(int i) {
  static const Slot t[4] = { {C_DFR, E_FR}, {C_DLF, E_FL},
                             {C_DBL, E_BL}, {C_DRB, E_BR} };
  if (i < 0 || i > 3) throw std::runtime_error("cube: F2L slot must be 0..3");
  return t[i];
}

inline bool slot_solved(const CubieState& c, int i) {
  Slot s = f2l_slot(i);
  return corner_home(c, s.corner) && edge_home(c, s.edge);
}

// First two layers: the cross plus all four pairs.
inline bool f2l_solved(const CubieState& c) {
  if (!cross_solved(c)) return false;
  for (int i = 0; i < 4; i++) if (!slot_solved(c, i)) return false;
  return true;
}

// Every U piece showing U on top -- the end of OLL. Says nothing about where
// they are, which is exactly the point: that is PLL's business.
inline bool oll_solved(const CubieState& c) {
  for (int i = 0; i < 4; i++) if (c.co[i] != 0) return false;
  for (int i = 0; i < 4; i++) if (c.eo[i] != 0) return false;
  return true;
}

// The whole cube, ignoring nothing.
inline bool cube_solved(const CubieState& c) {
  for (int i = 0; i < 8; i++) if (!corner_home(c, i)) return false;
  for (int i = 0; i < 12; i++) if (!edge_home(c, i)) return false;
  return true;
}

// ---- Layer-by-layer stage predicates ----------------------------------
//
// LBL divides the cube differently from CFOP: it finishes the bottom layer
// before starting the middle, and it orients the last layer's edges and
// corners in separate steps. These are the milestones it stops at.

// The four bottom corners home as well as the cross: the first layer done.
inline bool first_layer_solved(const CubieState& c) {
  if (!cross_solved(c)) return false;
  return corner_home(c, C_DFR) && corner_home(c, C_DLF) &&
         corner_home(c, C_DBL) && corner_home(c, C_DRB);
}

// The yellow cross on top: U edges oriented, wherever they sit.
inline bool ll_cross_oriented(const CubieState& c) {
  for (int i = 0; i < 4; i++) if (c.eo[i] != 0) return false;
  return true;
}

// U edges in their own slots as well as oriented.
inline bool ll_edges_placed(const CubieState& c) {
  for (int i = 0; i < 4; i++) if (!edge_home(c, i)) return false;
  return true;
}

// U corners in the right slots, twist not yet fixed.
inline bool ll_corners_placed(const CubieState& c) {
  for (int i = 0; i < 4; i++) if (c.cp[i] != i) return false;
  return true;
}

}  // namespace cube_cubie

#endif  // CAYLEYR_CUBE_CUBIE_H
