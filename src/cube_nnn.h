#ifndef CAYLEYR_CUBE_NNN_H
#define CAYLEYR_CUBE_NNN_H

#include <vector>
#include <string>
#include <stdexcept>
#include "perm_group.h"

// ---- The N x N x N cube, generated rather than tabulated ----------------
//
// A move is a triple: an axis, which layer along it turns, and how far. That
// is the whole vocabulary, and it is why one generator covers every cube --
// the 3x3x3 middle slices are layer 1, the 4x4x4 inner slices are layers 1 and
// 2, and nothing about the code knows which case it is in.
//
// Stickers are numbered face by face in the order U R F D L B, and within a
// face left to right, top to bottom as seen from outside that face. For N = 3
// the twelve face turns are generated, not tabulated, which is the test
// the generator has to pass.
//
// ---- Coordinates -------------------------------------------------------
//
// Each sticker sits on a cubie at integer coordinates (x, y, z) in 0..N-1 and
// faces one of six directions. Working in coordinates rather than in sticker
// indices is what makes a layer turn a rotation: turning layer L about the x
// axis maps (x, y, z) -> (x, z, N-1-y) for every sticker with x == L, and
// carries the facing direction round with it. Going back to an index at the
// end is a lookup.
//
// The axes are the usual right-handed set seen from outside the cube:
//   x runs L -> R, y runs D -> U, z runs B -> F.
//
// Clockwise is clockwise as seen from the positive end of the axis, which is
// what makes layer 0 about the y axis come out as U rather than U'.

namespace cube_nnn {

enum Face { F_U = 0, F_R = 1, F_F = 2, F_D = 3, F_L = 4, F_B = 5 };
enum Axis { AX_X = 0, AX_Y = 1, AX_Z = 2 };

// A sticker: the cubie it belongs to, and which way it faces.
struct Sticker {
  int x, y, z;
  int face;
};

// Where a sticker sits in the flat state vector. Within a face the two
// in-plane coordinates become (row, col) as seen from outside, which is the
// only place the layout convention is written down.
inline int sticker_index(int n, const Sticker& s) {
  int row = 0, col = 0;
  switch (s.face) {
    case F_U: row = s.z;           col = s.x;             break;
    case F_D: row = (n - 1) - s.z; col = s.x;             break;
    case F_F: row = (n - 1) - s.y; col = s.x;             break;
    case F_B: row = (n - 1) - s.y; col = (n - 1) - s.x;   break;
    case F_R: row = (n - 1) - s.y; col = (n - 1) - s.z;   break;
    case F_L: row = (n - 1) - s.y; col = s.z;             break;
    default: throw std::runtime_error("cube_nnn: bad face");
  }
  return s.face * n * n + row * n + col;
}

// Every sticker of the cube, in index order.
inline std::vector<Sticker> all_stickers(int n) {
  std::vector<Sticker> out(6 * n * n);
  for (int x = 0; x < n; x++) {
    for (int y = 0; y < n; y++) {
      for (int z = 0; z < n; z++) {
        // only the outer shell carries stickers, and a cubie may carry
        // several -- one per face it touches
        if (y == n - 1) { Sticker s = {x, y, z, F_U}; out[sticker_index(n, s)] = s; }
        if (y == 0)     { Sticker s = {x, y, z, F_D}; out[sticker_index(n, s)] = s; }
        if (z == n - 1) { Sticker s = {x, y, z, F_F}; out[sticker_index(n, s)] = s; }
        if (z == 0)     { Sticker s = {x, y, z, F_B}; out[sticker_index(n, s)] = s; }
        if (x == n - 1) { Sticker s = {x, y, z, F_R}; out[sticker_index(n, s)] = s; }
        if (x == 0)     { Sticker s = {x, y, z, F_L}; out[sticker_index(n, s)] = s; }
      }
    }
  }
  return out;
}

// One clockwise quarter turn about an axis, applied to a sticker's cubie and
// to the direction it faces. Clockwise is seen from the positive end.
inline Sticker rotate_once(int n, Axis axis, const Sticker& s) {
  Sticker r = s;
  const int m = n - 1;
  switch (axis) {
    case AX_X:  // about L->R, clockwise seen from R
      r.x = s.x; r.y = m - s.z; r.z = s.y;
      // R takes U<-F, F<-D, D<-B, B<-U
      switch (s.face) {
        case F_U: r.face = F_F; break;
        case F_F: r.face = F_D; break;
        case F_D: r.face = F_B; break;
        case F_B: r.face = F_U; break;
        default:  r.face = s.face; break;   // R and L ride along
      }
      break;
    case AX_Y:  // about D->U, clockwise seen from U
      r.y = s.y; r.x = s.z; r.z = m - s.x;
      // U takes F<-R, R<-B, B<-L, L<-F
      switch (s.face) {
        case F_F: r.face = F_R; break;
        case F_R: r.face = F_B; break;
        case F_B: r.face = F_L; break;
        case F_L: r.face = F_F; break;
        default:  r.face = s.face; break;   // U and D ride along
      }
      break;
    case AX_Z:  // about B->F, clockwise seen from F
      r.z = s.z; r.x = m - s.y; r.y = s.x;
      // F takes U<-L, L<-D, D<-R, R<-U
      switch (s.face) {
        case F_U: r.face = F_L; break;
        case F_L: r.face = F_D; break;
        case F_D: r.face = F_R; break;
        case F_R: r.face = F_U; break;
        default:  r.face = s.face; break;   // F and B ride along
      }
      break;
    default: throw std::runtime_error("cube_nnn: bad axis");
  }
  return r;
}

// The coordinate along `axis` that says which layer a sticker belongs to.
inline int layer_of(Axis axis, const Sticker& s) {
  return axis == AX_X ? s.x : (axis == AX_Y ? s.y : s.z);
}

// One move as a permutation of 1..6n^2, in the package convention
// new[i] = state[perm[i]]. `turns` is 1, 2 or 3 quarter turns clockwise.
//
// The permutation is built backwards on purpose: for each destination we ask
// where its sticker came from, which is the rotation applied in reverse. That
// is what new[i] = state[perm[i]] means, and getting it the other way round
// would silently give every move its inverse.
inline std::vector<int> layer_move(int n, Axis axis, int layer, int turns) {
  if (n < 2) throw std::runtime_error("cube_nnn: n must be at least 2");
  if (layer < 0 || layer >= n) throw std::runtime_error("cube_nnn: layer out of range");
  if (turns < 1 || turns > 3) throw std::runtime_error("cube_nnn: turns must be 1..3");

  const std::vector<Sticker> st = all_stickers(n);
  const int total = 6 * n * n;
  std::vector<int> perm(total);

  const int back = 4 - turns;   // inverse rotation: 3 quarter turns undo 1
  for (int i = 0; i < total; i++) {
    Sticker s = st[i];
    if (layer_of(axis, s) == layer) {
      for (int t = 0; t < back; t++) s = rotate_once(n, axis, s);
    }
    perm[i] = sticker_index(n, s) + 1;   // 1-based
  }
  return perm;
}

// ---- Naming ------------------------------------------------------------
//
// Outer layers keep the letters the literature uses, so a 3x3x3 built here
// answers to R and U as it always has. Inner layers have no standard name
// beyond the 3x3x3 slices, so they are written axis-and-index: "2x" is layer 2
// along x. For n = 3 the single inner layer of each axis is M, E or S, in the
// orientation the literature gives them.
//
// The suffix is the usual one: none for a quarter turn, ' for its inverse,
// 2 for a half turn.

inline std::string base_name(int n, Axis axis, int layer) {
  const bool first = (layer == 0);
  const bool last  = (layer == n - 1);
  if (axis == AX_X) {
    if (last)  return "R";
    if (first) return "L";
    if (n == 3) return "M";          // M follows L, i.e. the same sense as x-
    return std::to_string(layer) + "x";
  }
  if (axis == AX_Y) {
    if (last)  return "U";
    if (first) return "D";
    if (n == 3) return "E";          // E follows D
    return std::to_string(layer) + "y";
  }
  if (last)  return "F";
  if (first) return "B";
  if (n == 3) return "S";            // S follows F
  return std::to_string(layer) + "z";
}

// Whether a layer's letter runs against the axis. L, D and B are named for the
// negative end, so a clockwise turn about the axis is their prime; M and E
// follow L and D, S follows F. Without this the names would be right but half
// of them would mean their own inverses.
inline bool name_is_reversed(int n, Axis axis, int layer) {
  // Which way a letter runs is settled by the reference table: R, U and F come
  // out of three quarter turns about their axis, L, D and B out of one. The
  // axes point L->R, D->U and B->F, so the far face turns against the axis
  // when seen from outside -- that asymmetry is in the puzzle's naming, not a
  // choice made here.
  if (layer == n - 1) return false;              // R, U, F
  if (layer == 0)     return true;               // L, D, B
  if (n == 3) return axis != AX_Z;               // S follows F; M, E follow L, D
  return false;                                  // numbered inner layers
}

// The name for a turn already expressed the way the letter counts it: 1 is the
// bare letter, 3 its prime, 2 the half turn. Callers that think in axis turns
// want move_name() below instead.
inline std::string plain_name(int n, Axis axis, int layer, int turns) {
  const std::string nm = base_name(n, axis, layer);
  if (turns == 2) return nm + "2";
  if (turns == 3) return nm + "'";
  return nm;
}

inline std::string move_name(int n, Axis axis, int layer, int turns) {
  std::string nm = base_name(n, axis, layer);
  int t = turns;
  if (name_is_reversed(n, axis, layer)) t = 4 - t;   // 1 <-> 3, 2 fixed
  if (t == 2) return nm + "2";
  if (t == 3) return nm + "'";
  return nm;
}

// ---- The whole alphabet ------------------------------------------------
//
// Three axes, n layers each, a quarter turn each way: 6n moves. This is the
// quarter-turn metric, in which a half turn is two moves rather than one, so
// U2 is not in the alphabet -- it is the word "U U". The distinction matters
// because it is what a shortest path is measured in: the same graph under HTM
// and QTM has different diameters (20 and 26 for the 3x3x3 faces).
//
// layer_move() below will still build a half turn on request, which is how a
// caller writes U2 when they want it; it is only the generating set that
// leaves them out.
//
// Ordered so that a 3x3x3 comes out U U' R R' F F' D D' L L' B B' and then the
// six slice turns, with the slices after the faces rather than interleaved.

struct CubeAlphabet {
  int n;
  std::vector<std::string> names;
  std::vector<std::vector<int> > perms;   // 1-based
};

inline CubeAlphabet build_alphabet(int n) {
  CubeAlphabet out;
  out.n = n;

  // faces first, in U R F D L B order, then the inner layers
  struct Slot { Axis axis; int layer; };
  std::vector<Slot> slots;
  slots.push_back(Slot{AX_Y, n - 1});   // U
  slots.push_back(Slot{AX_X, n - 1});   // R
  slots.push_back(Slot{AX_Z, n - 1});   // F
  slots.push_back(Slot{AX_Y, 0});       // D
  slots.push_back(Slot{AX_X, 0});       // L
  slots.push_back(Slot{AX_Z, 0});       // B
  for (int layer = 1; layer < n - 1; layer++) {
    slots.push_back(Slot{AX_X, layer});
    slots.push_back(Slot{AX_Y, layer});
    slots.push_back(Slot{AX_Z, layer});
  }

  // quarter turns only: 1 is the move as its letter names it, 3 its inverse
  const int qtm_turns[2] = {1, 3};
  for (size_t i = 0; i < slots.size(); i++) {
    for (int t = 0; t < 2; t++) {
      const int turns = qtm_turns[t];
      // `turns` is what the letter means: 1 for the bare name, 3 for its
      // prime. What the axis has to be turned by is the other way round for a
      // far face -- U is three quarter turns about y, D is one -- and that is
      // the whole of the discrepancy between the puzzle's names and the
      // right-handed axes.
      const int eff = name_is_reversed(n, slots[i].axis, slots[i].layer)
                        ? turns : 4 - turns;
      out.names.push_back(plain_name(n, slots[i].axis, slots[i].layer, turns));
      out.perms.push_back(layer_move(n, slots[i].axis, slots[i].layer, eff));
    }
  }
  return out;
}

// Build the cube as a group. TablePermGroup wants 0-based permutations.
inline PermGroup* make_cube_group(int n) {
  CubeAlphabet a = build_alphabet(n);
  std::vector<std::vector<int> > perms(a.perms.size());
  for (size_t m = 0; m < a.perms.size(); m++) {
    perms[m].resize(a.perms[m].size());
    for (size_t i = 0; i < a.perms[m].size(); i++) perms[m][i] = a.perms[m][i] - 1;
  }
  return new TablePermGroup(6 * n * n, a.names, perms);
}

}  // namespace cube_nnn

#endif  // CAYLEYR_CUBE_NNN_H
