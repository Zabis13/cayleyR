#ifndef CAYLEYR_CUBE_CENTRES5_H
#define CAYLEYR_CUBE_CENTRES5_H

#include <vector>
#include <string>
#include <stdexcept>
#include "cube_solve.h"

// ---- The centres of a 5x5x5 --------------------------------------------
//
// Measured from cube_moves(5) rather than reasoned about, the same way
// cube_centres.h was. Each face is five by five and its nine centre stickers
// are local indices 6,7,8,11,12,13,16,17,18 -- rows and columns 1..3 of the
// face, left to right then top to bottom seen from outside. Slot k of face f
// is f*25 + that.
//
// ---- Three orbits, not one ---------------------------------------------
//
// The nine split into three sets that no move mixes. Measured by walking the
// connected components of the centre stickers under all thirty generators:
//
//     corner centres   24 pieces   local 6, 8, 16, 18
//     plus centres     24 pieces   local 7, 11, 13, 17
//     fixed centres     6 pieces   local 12
//
// Two orbits of twenty-four, each behaving like the single orbit a 4x4x4 has,
// and six pieces that are one per face. That is the whole difference in
// structure from cube_centres.h, and it is why this file solves two orbits in
// turn rather than one.
//
// ---- The fixed centres are the face colours ----------------------------
//
// The reason this file needs no Orient. On a 4x4x4 no sticker tells you which
// face you are looking at, so cube_centres.h carries a six-entry map through
// every rotation and explains at length why reading the colour off the cube
// does not work there.
//
// Here it does work. Measured: of the thirty generators, only 2x, 2y and 2z
// permute the fixed centres at all -- the central layer of an odd cube IS the
// whole-cube rotation -- and the other twenty-four leave each face showing its
// own colour. Checked over forty random non-central moves: all six fixed
// centres still read as their own face.
//
// So the colour of the face at position p is (state[p*25 + 12] - 1) / 25,
// always, and a solve that avoids 2x/2y/2z needs nothing else. A solve that
// uses them re-reads the six stickers afterwards; it does not have to track
// anything.

namespace cube_solve5 {

const int N5 = 5;
const int FACE5 = 25;      // stickers per face
const int NSTICK5 = 150;

// The nine centre slots of a face, in local order.
inline const int* centre_locals() {
  static const int loc[9] = {6, 7, 8, 11, 12, 13, 16, 17, 18};
  return loc;
}

// The four corner-centre slots of a face: local 6, 8, 16, 18.
inline const int* corner_centre_locals() {
  static const int loc[4] = {6, 8, 16, 18};
  return loc;
}

// The four plus-centre slots of a face: local 7, 11, 13, 17.
inline const int* plus_centre_locals() {
  static const int loc[4] = {7, 11, 13, 17};
  return loc;
}

// The one fixed centre of a face.
inline int fixed_centre_local() { return 12; }

// Sticker index (0-based) of a face's fixed centre.
inline int fixed_centre_index(int face) {
  if (face < 0 || face > 5) throw std::runtime_error("cube_centres5: bad face");
  return face * FACE5 + fixed_centre_local();
}

// What colour a sticker shows. A sticker's value is where it began, and
// stickers are numbered face by face in blocks of 25, so the colour is that
// block. State is 1-based, as everywhere in the package.
inline int centre_colour(const std::vector<int>& state, int sticker0) {
  return (state[sticker0] - 1) / FACE5;
}

// The colour belonging at face position p, read off the cube. See the note
// above: this is exact for an odd cube, where only the central-layer turns
// move a fixed centre.
inline int face_colour(const std::vector<int>& state, int face) {
  return centre_colour(state, fixed_centre_index(face));
}

// How many of a face's four corner centres show that face's colour.
inline int corner_centre_count(const std::vector<int>& state, int face) {
  const int* loc = corner_centre_locals();
  const int home = face_colour(state, face);
  int c = 0;
  for (int k = 0; k < 4; k++)
    if (centre_colour(state, face * FACE5 + loc[k]) == home) c++;
  return c;
}

// How many of a face's four plus centres show that face's colour.
inline int plus_centre_count(const std::vector<int>& state, int face) {
  const int* loc = plus_centre_locals();
  const int home = face_colour(state, face);
  int c = 0;
  for (int k = 0; k < 4; k++)
    if (centre_colour(state, face * FACE5 + loc[k]) == home) c++;
  return c;
}

inline int corner_centres_total(const std::vector<int>& state) {
  int c = 0;
  for (int f = 0; f < 6; f++) c += corner_centre_count(state, f);
  return c;
}

inline int plus_centres_total(const std::vector<int>& state) {
  int c = 0;
  for (int f = 0; f < 6; f++) c += plus_centre_count(state, f);
  return c;
}

// Both orbits home: the centres are reduced and the cube can be treated as a
// 3x3x3 as far as its faces go.
inline bool centres_built(const std::vector<int>& state) {
  return corner_centres_total(state) == 24 && plus_centres_total(state) == 24;
}

// ---- The shots -----------------------------------------------------------
//
// Eight commutators that empty slots of U onto a side face and leave the D
// centres alone. Measured, not reasoned about: every three-move conjugation
// over the thirty generators was applied to a solved cube and kept only if the
// twenty-four D centre stickers came back unmoved and something crossed
// between faces.
//
// Two per side face, and each carries a column of three centres:
//
//     1x U 1x'     -> F        1x' U 1x     -> B
//     3x U 3x'     -> F        3x' U 3x     -> B
//     1z U 1z'     -> L        1z' U 1z     -> R
//     3z U 3z'     -> L        3z' U 3z     -> R
//
// The invariant that makes this a method rather than a search: D is untouched
// by all eight, so whatever has been built and turned to the bottom cannot be
// disturbed however many shots are fired. Checked over 400 random sequences of
// twelve shots with arbitrary U turns between them -- the D centres moved in
// none of them.
//
// Unlike the 4x4x4 shots these are not orbit-pure: each moves four corner
// centres and four plus centres together. That is not a defect here, because
// the two orbits are solved in one pass rather than two -- a shot fired for a
// corner centre carries a plus centre along, and the plus centre it carries is
// chosen by which U slot the setup turn brought round.
struct Shot5 {
  const char* word;
  int to_face;        // 1 R, 2 F, 4 L, 5 B
};

inline const Shot5* shots() {
  static const Shot5 s[8] = {
    {"1x U 1x'",  2}, {"3x U 3x'",  2},
    {"1x' U 1x",  5}, {"3x' U 3x",  5},
    {"1z U 1z'",  4}, {"3z U 3z'",  4},
    {"1z' U 1z",  1}, {"3z' U 3z",  1}
  };
  return s;
}

inline int n_shots() { return 8; }

// ---- The setup turn ------------------------------------------------------
//
// U cycles its centre slots in two four-cycles, measured by turning it once
// and asking where the sticker that started at each slot ended up:
//
//     corner   6 -> 8 -> 18 -> 16 -> 6
//     plus     7 -> 13 -> 17 -> 11 -> 7
//
// The direction is the part worth stating. Reading "U11 <- U7" off a
// permutation and writing 7 -> 11 gives the cycle backwards -- that is where
// the slot a piece CAME FROM, not where a piece GOES. Written backwards the
// table disagreed with the cube on 16 of 32 pairs, all of them plus centres.
//
// Both cycles have length four, so from any slot any other slot of the same
// orbit is a fixed number of U turns away, computed rather than searched for
// -- the same shape as u_setup_turns() in cube_centres.h.
inline int u_setup_turns(int from_local, int to_local) {
  static const int corner_next[19] = {0,0,0,0,0,0, 8,0, 18,0,0,0,0,0,0,0, 6,0, 16};
  static const int plus_next[19]   = {0,0,0,0,0,0,0, 13,0,0,0, 7,0, 17,0,0,0, 11,0};
  const bool is_corner = (from_local == 6 || from_local == 8 ||
                          from_local == 16 || from_local == 18);
  const int* nxt = is_corner ? corner_next : plus_next;
  int cur = from_local;
  for (int t = 0; t < 4; t++) {
    if (cur == to_local) return t;
    cur = nxt[cur];
  }
  return -1;   // different orbits: no number of U turns connects them
}

}  // namespace cube_solve5

#endif
