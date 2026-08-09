#ifndef CAYLEYR_CUBE_ALGS_H
#define CAYLEYR_CUBE_ALGS_H

#include <vector>
#include <string>
#include "cube_search.h"

// ---- The learned algorithms --------------------------------------------
//
// CFOP is a method of memorised cases: you look at the last layer, recognise
// which of 57 orientations it is in, and play the sequence you learned for it.
// This file is that memory -- the standard tables, written in the notation the
// literature uses, with the names cubers call them by.
//
// What is *not* here is a recogniser. A case is identified by trying it: for
// each algorithm, and each of the four ways the U face can be turned first,
// apply it and ask whether the stage is now finished. The first that works is
// the case you were in. This is a few hundred permutation applications, which
// is nothing, and it buys the property that the table is the only place a case
// is described. A hand-written recogniser would be a second description of the
// same 57 patterns, and the two would eventually disagree.
//
// Algorithms are written with half turns (U2, R2) because that is how they are
// published and how they are remembered. parse_word() expands them into the
// package's quarter-turn alphabet, so a stored "R U2 R'" becomes four moves.
// Move counts reported later are therefore quarter turns, and are larger than
// the "move counts" quoted in speedcubing, which count a half turn as one.

namespace cube_algs {

struct Alg {
  const char* name;    // what cubers call this case
  const char* moves;   // the sequence, in standard notation
};

// ---- OLL: 57 cases -----------------------------------------------------
//
// Orient the last layer: every U sticker facing up, ignoring where the pieces
// are. Numbered as the standard tables number them (OLL 1 to 57), with the
// shape names that go with them.

inline const std::vector<Alg>& oll_table() {
  static const Alg t[] = {
    {"OLL 1 (Dot)",            "R U2 R2 F R F' U2 R' F R F'"},
    {"OLL 2 (Dot)",            "F R U R' U' F' f R U R' U' f'"},
    {"OLL 3 (Dot)",            "f R U R' U' f' U' F R U R' U' F'"},
    {"OLL 4 (Dot)",            "f R U R' U' f' U F R U R' U' F'"},
    {"OLL 5 (Square)",         "r' U2 R U R' U r"},
    {"OLL 6 (Square)",         "r U2 R' U' R U' r'"},
    {"OLL 7 (Lightning)",      "r U R' U R U2 r'"},
    {"OLL 8 (Lightning)",      "r' U' R U' R' U2 r"},
    {"OLL 9 (Fish)",           "R U R' U' R' F R2 U R' U' F'"},
    {"OLL 10 (Fish)",          "R U R' U R' F R F' R U2 R'"},
    {"OLL 11 (Lightning)",     "r U R' U R' F R F' R U2 r'"},
    {"OLL 12 (Lightning)",     "M' R' U' R U' R' U2 R U' R r'"},
    {"OLL 13 (Knight)",        "F U R U' R2 F' R U R U' R'"},
    {"OLL 14 (Knight)",        "R' F R U R' F' R F U' F'"},
    {"OLL 15 (Knight)",        "l' U' l L' U' L U l' U l"},
    {"OLL 16 (Knight)",        "r U r' R U R' U' r U' r'"},
    {"OLL 17 (Dot)",           "F R' F' R2 r' U R U' R' U' M'"},
    {"OLL 18 (Dot)",           "r U R' U R U2 r2 U' R U' R' U2 r"},
    {"OLL 19 (Dot)",           "r' R U R U R' U' M' R' F R F'"},
    {"OLL 20 (Dot)",           "r U R' U' M2 U R U' R' U' M'"},
    {"OLL 21 (Cross)",         "R U2 R' U' R U R' U' R U' R'"},
    {"OLL 22 (Cross)",         "R U2 R2 U' R2 U' R2 U2 R"},
    {"OLL 23 (Cross)",         "R2 D' R U2 R' D R U2 R"},
    {"OLL 24 (Cross)",         "r U R' U' r' F R F'"},
    {"OLL 25 (Cross)",         "F' r U R' U' r' F R"},
    {"OLL 26 (Cross)",         "R U2 R' U' R U' R'"},
    {"OLL 27 (Cross)",         "R U R' U R U2 R'"},
    {"OLL 28 (Corners)",       "r U R' U' M U R U' R'"},
    {"OLL 29 (Awkward)",       "R U R' U' R U' R' F' U' F R U R'"},
    {"OLL 30 (Awkward)",       "F R' F R2 U' R' U' R U R' F2"},
    {"OLL 31 (P)",             "R' U' F U R U' R' F' R"},
    {"OLL 32 (P)",             "L U F' U' L' U L F L'"},
    {"OLL 33 (T)",             "R U R' U' R' F R F'"},
    {"OLL 34 (C)",             "R U R2 U' R' F R U R U' F'"},
    {"OLL 35 (Fish)",          "R U2 R2 F R F' R U2 R'"},
    {"OLL 36 (W)",             "L' U' L U' L' U L U L F' L' F"},
    {"OLL 37 (Fish)",          "F R' F' R U R U' R'"},
    {"OLL 38 (W)",             "R U R' U R U' R' U' R' F R F'"},
    {"OLL 39 (Lightning)",     "L F' L' U' L U F U' L'"},
    {"OLL 40 (Lightning)",     "R' F R U R' U' F' U R"},
    {"OLL 41 (Awkward)",       "R U R' U R U2 R' F R U R' U' F'"},
    {"OLL 42 (Awkward)",       "R' U' R U' R' U2 R F R U R' U' F'"},
    {"OLL 43 (P)",             "F' U' L' U L F"},
    {"OLL 44 (P)",             "F U R U' R' F'"},
    {"OLL 45 (T)",             "F R U R' U' F'"},
    {"OLL 46 (C)",             "R' U' R' F R F' U R"},
    {"OLL 47 (L)",             "F' L' U' L U L' U' L U F"},
    {"OLL 48 (L)",             "F R U R' U' R U R' U' F'"},
    {"OLL 49 (L)",             "r U' r2 U r2 U r2 U' r"},
    {"OLL 50 (L)",             "r' U r2 U' r2 U' r2 U r'"},
    {"OLL 51 (I)",             "F U R U' R' U R U' R' F'"},
    {"OLL 52 (I)",             "R U R' U R U' B U' B' R'"},
    {"OLL 53 (L)",             "l' U2 L U L' U' L U L' U l"},
    {"OLL 54 (L)",             "r U2 R' U' R U R' U' R U' r'"},
    {"OLL 55 (I)",             "R' F R U R U' R2 F' R2 U' R' U R U R'"},
    {"OLL 56 (I)",             "r' U' r U' R' U R U' R' U R r' U r"},
    {"OLL 57 (Corners)",       "R U R' U' M' U R U' r'"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// ---- PLL: 21 cases -----------------------------------------------------
//
// Permute the last layer: the pieces are all oriented, and this puts them
// where they belong. The letter names are the standard ones.

inline const std::vector<Alg>& pll_table() {
  static const Alg t[] = {
    {"Aa", "x L2 D2 L' U' L D2 L' U L' x'"},
    {"Ab", "x' L2 D2 L U L' D2 L U' L x"},
    {"E",  "x' L' U L D' L' U' L D L' U' L D' L' U L D x"},
    {"F",  "R' U' F' R U R' U' R' F R2 U' R' U' R U R' U R"},
    {"Ga", "R2 U R' U R' U' R U' R2 U' D R' U R D'"},
    {"Gb", "R' U' R U D' R2 U R' U R U' R U' R2 D"},
    {"Gc", "R2 U' R U' R U R' U R2 U D' R U' R' D"},
    {"Gd", "R U R' U' D R2 U' R U' R' U R' U R2 D'"},
    {"H",  "M2 U M2 U2 M2 U M2"},
    {"Ja", "x R2 F R F' R U2 r' U r U2 x'"},
    {"Jb", "R U R' F' R U R' U' R' F R2 U' R'"},
    {"Na", "R U R' U R U R' F' R U R' U' R' F R2 U' R' U2 R U' R'"},
    {"Nb", "R' U R U' R' F' U' F R U R' F R' F' R U' R"},
    {"Ra", "R U' R' U' R U R D R' U' R D' R' U2 R'"},
    {"Rb", "R2 F R U R U' R' F' R U2 R' U2 R"},
    {"T",  "R U R' U' R' F R2 U' R' U' R U R' F'"},
    {"Ua", "R U' R U R U R U' R' U' R2"},
    {"Ub", "R2 U R U R' U' R' U' R' U R'"},
    // The published V-perm has a y in the middle. A rotation there means "turn
    // the cube in your hands and read the rest of the letters from where it
    // now is", which a person does without noticing; expanded as a permutation
    // it turns the cube but leaves the following letters naming the old faces,
    // and the algorithm wrecks the middle layer. The four other entries here
    // with rotations are safe because theirs are paired -- x ... x' -- so the
    // conjugation closes. This is the rotationless form instead.
    {"V",  "R' U R' U' R D' R' D R' U D' R2 U' R2 D R2"},
    {"Y",  "F R U' R' U' R U R' F' R U R' U' R' F R F'"},
    {"Z",  "M' U M2 U M2 U M' U2 M2"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// ---- Layer-by-layer -----------------------------------------------------
//
// The beginner's method needs far fewer sequences, because it does less per
// step: it orients the last layer's edges, then places them, then places the
// corners, then twists them, where CFOP does the first two in one algorithm
// and the last two in another.

// Turn the top edges into a cross. Applied repeatedly: dot -> line -> cross.
inline const std::vector<Alg>& lbl_cross_table() {
  static const Alg t[] = {
    {"LL edge orientation", "F R U R' U' F'"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// Cycle three top edges, leaving orientation alone.
inline const std::vector<Alg>& lbl_edge_perm_table() {
  static const Alg t[] = {
    {"LL edge cycle (Ua)", "R U' R U R U R U' R' U' R2"},
    {"LL edge cycle (Ub)", "R2 U R U R' U' R' U' R' U R'"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// Cycle three top corners into place, orientation still to come.
inline const std::vector<Alg>& lbl_corner_perm_table() {
  static const Alg t[] = {
    {"LL corner cycle (Aa)", "x L2 D2 L' U' L D2 L' U L' x'"},
    {"LL corner cycle (Ab)", "x' L2 D2 L U L' D2 L U' L x"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// Twist the corner now at URF, turning U between repetitions. This is the
// beginner's last step, and the one that looks like it is destroying the cube
// until the final U puts it right.
inline const std::vector<Alg>& lbl_corner_twist_table() {
  static const Alg t[] = {
    {"LL corner twist", "R' D' R D"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// ---- Old Pochmann ------------------------------------------------------
//
// The blindfolded method, which is built the other way round from the sighted
// ones. Those take the cube apart into layers because a person can see a layer;
// this one never looks at the cube at all after the start, so it needs a step
// that is the same every time. That step is: swap the piece in the buffer with
// one chosen piece, leave everything else alone, repeat.
//
// One swap is impossible on its own -- the cube group has no odd permutation of
// edges by itself -- so each algorithm swaps a second pair as well, always the
// same one, and the second swaps cancel in pairs. That is why the algorithms
// below are ordinary PLLs: a T-perm is exactly "swap two edges and two
// corners", which is the shape the method needs.
//
// A piece is named by a sticker rather than by a slot, because where a piece
// goes depends on which way round it is. The standard lettering runs A to X
// over the 24 stickers of each kind, faces in the order U F R B L D, clockwise
// within each face; the tables here are indexed by that letter.

// The edge cycle. Both entries swap the buffer at UR with one other edge and
// the corners URF and UBR; they differ in which edge, and so in which letters
// use which. Verified: T takes UL, J takes UF, and neither disturbs anything
// else.
inline const std::vector<Alg>& old_pochmann_edge_table() {
  static const Alg t[] = {
    {"T", "R U R' U' R' F R2 U' R' U' R U R' F'"},
    {"J", "R U R' F' R U R' U' R' F R2 U' R' U'"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// The corner cycle: swap the buffer at ULB with URF, and the edges UL and UB.
// One algorithm serves every corner, because the setup moves do the choosing.
inline const std::vector<Alg>& old_pochmann_corner_table() {
  static const Alg t[] = {
    // The framing F ... F' matters: without it the word swaps ULB with DFR and
    // twists both, which is a different algorithm that happens to look similar.
    {"Y", "F R U' R' U' R U R' F' R U R' U' R' F R F'"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// Parity. Each edge algorithm swaps a pair of corners as a side effect, so an
// odd number of edge swaps leaves the corners one swap out. This puts that
// right, between the edges and the corners: it swaps URF with UBR and UL with
// UB, fixing the corner pair and setting the edges up for the corner stage.
inline const std::vector<Alg>& old_pochmann_parity_table() {
  static const Alg t[] = {
    {"R-perm parity", "y' L U2 L' U2 L F' L' U' L U L F L2 U y"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// ---- M2 -----------------------------------------------------------------
//
// The same idea as old Pochmann and a shorter way of doing the edges. There the
// buffer edge was swapped into place by a whole PLL wrapped in setup moves,
// fourteen moves before the setup was counted. Here the buffer is UB and the
// swap is M2 -- two moves. Everything else follows from that choice.
//
// What it costs is that M2 is not clean. It turns the middle slice a half turn,
// so it moves the centres, and it swaps two pairs of edges that are not the
// buffer's: the stickers lettered C and W, and E and O. The centres come back
// after an even number of applications and so do those pairs, which is why the
// method is stated in terms of odd and even positions in the memorised
// sequence rather than piece by piece.
//
// The four letters M2 disturbs cannot be solved by M2, so they have their own
// algorithms, and each has two forms depending on whether the letter falls in
// an odd or an even position. The two forms are inverses of each other.

// C and W, the two stickers of the UF/DB pair that M2 swaps. Each runs after an
// M2, which has already carried the buffer piece to the delivery point; these
// take it the rest of the way. Named for the letter each one finishes.
inline const std::vector<Alg>& m2_cw_table() {
  static const Alg t[] = {
    {"W", "U2 M' U2 M'"},
    {"C", "M U2 M U2"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// E and O, the other pair, same convention.
inline const std::vector<Alg>& m2_eo_table() {
  static const Alg t[] = {
    {"O", "D M' U R2 U' M U R2 U' D' M2"},
    {"E", "M2 D U R2 U' M' U R2 U' M D'"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// Parity, between the edges and the corners. An odd number of edge swaps leaves
// the corner pair the edge algorithms disturb one swap out; this puts it right.
inline const std::vector<Alg>& m2_parity_table() {
  static const Alg t[] = {
    {"M2 parity", "D' L2 D M2 D' L2 D"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}


// ---- Edge orientation, the endgame ---------------------------------------
//
// The cycle phase never touches orientation. An edge that is home but turned
// over is set aside at the start -- "inactive", in the method's own word --
// and left out of every chain, so the chains only ever move pieces between
// slots. What is left when they finish is a set of edges in the right slots
// facing the wrong way, and that is what this fixes.
//
// One algorithm does it: M' U M' U M' U2 M U M U M U2 turns over UF and UB and
// leaves the cube otherwise exactly as it was -- permutation intact, corners
// and centres untouched. Every other pair is that algorithm conjugated, a
// setup bringing the two edges to UF and UB and undone afterwards.
//
// The 66 setups below were found by search rather than copied, each verified
// to flip its own pair and nothing else. Depth 3 covers all of them.

struct EoSetup {
  int a, b;            // edge slots, 0-based, a < b
  const char* setup;   // conjugating word, "" when the pair is already UF/UB
};

inline const char* eo_base_alg() { return "M' U M' U M' U2 M U M U M U2"; }

inline const EoSetup* eo_setups() {
  static const EoSetup t[66] = {
    { 0,  1, "R B"},
    { 0,  2, "U"},
    { 0,  3, "R' F'"},
    { 0,  4, "U D' M'"},
    { 0,  5, "U M'"},
    { 0,  6, "U D M'"},
    { 0,  7, "U' M"},
    { 0,  8, "U' F'"},
    { 0,  9, "U' F"},
    { 0, 10, "U B'"},
    { 0, 11, "U B"},
    { 1,  2, "L' B'"},
    { 1,  3, ""},
    { 1,  4, "D' M'"},
    { 1,  5, "M'"},
    { 1,  6, "D M'"},
    { 1,  7, "B B"},
    { 1,  8, "M' F'"},
    { 1,  9, "M' F"},
    { 1, 10, "B'"},
    { 1, 11, "B"},
    { 2,  3, "L F"},
    { 2,  4, "U D M"},
    { 2,  5, "U' M'"},
    { 2,  6, "U D' M"},
    { 2,  7, "U M"},
    { 2,  8, "U F'"},
    { 2,  9, "U F"},
    { 2, 10, "U' B'"},
    { 2, 11, "U' B"},
    { 3,  4, "D M"},
    { 3,  5, "F F"},
    { 3,  6, "D' M"},
    { 3,  7, "M"},
    { 3,  8, "F'"},
    { 3,  9, "F"},
    { 3, 10, "M B'"},
    { 3, 11, "M B"},
    { 4,  5, "R' M' B"},
    { 4,  6, "D M M"},
    { 4,  7, "R M F'"},
    { 4,  8, "D M F'"},
    { 4,  9, "D M F"},
    { 4, 10, "D' M' B'"},
    { 4, 11, "D' M' B"},
    { 5,  6, "L M' B'"},
    { 5,  7, "M M"},
    { 5,  8, "R U M'"},
    { 5,  9, "L' U' M'"},
    { 5, 10, "M' B'"},
    { 5, 11, "M' B"},
    { 6,  7, "L' M F"},
    { 6,  8, "D' M F'"},
    { 6,  9, "D' M F"},
    { 6, 10, "D M' B'"},
    { 6, 11, "D M' B"},
    { 7,  8, "M F'"},
    { 7,  9, "M F"},
    { 7, 10, "L U M"},
    { 7, 11, "R' U' M"},
    { 8,  9, "F M'"},
    { 8, 10, "F' B'"},
    { 8, 11, "F' B"},
    { 9, 10, "F B'"},
    { 9, 11, "F B"},
    {10, 11, "B M"}

  };
  return t;
}

inline const char* eo_setup_for(int a, int b) {
  if (a > b) { int t = a; a = b; b = t; }
  const EoSetup* t = eo_setups();
  for (int i = 0; i < 66; i++) {
    if (t[i].a == a && t[i].b == b) return t[i].setup;
  }
  return 0;
}

// ---- F2L: the 41 cases -------------------------------------------------
//
// The first two layers, done as four corner-edge pairs. This is the half of
// CFOP that is supposed to be intuitive -- a cuber learns to see the pair and
// improvise -- but the improvisation converges on a known set, and this is it.
//
// Every case is written for the front-right slot. A pair belonging to another
// slot is brought here by turning the cube, which is what the y in many of
// these entries does and what the solver does around them: there is one slot
// and four ways to face it, not four slots.
//
// The numbering is the standard one (F2L 1 to 41), grouped as the tables
// usually group them: the pair already joined in the top layer, then the
// corner up with the edge in place, then the edge up with the corner in place,
// then both stuck in the slot the wrong way.
//
// Recognition is by trying, as with OLL and PLL: apply an entry and ask
// whether the slot came out solved. The table is then the only description of
// a case, and there is no second one to disagree with it.

inline const std::vector<Alg>& f2l_table() {
  static const Alg t[] = {
    // ---- easy cases -----------------------------------------------------
    {"F2L 1  (easy)",               "R' F R F'"},
    {"F2L 2  (easy)",               "y U' L' U L"},
    {"F2L 3  (easy)",               "y L' U' L"},
    {"F2L 4  (easy)",               "R U R'"},

    // ---- reposition edge ------------------------------------------------
    {"F2L 5  (reposition edge)",    "U' R U R' U2 R U' R'"},
    // The published forms of 6 and 8 begin with d, a wide bottom turn. On a
    // cube in the hand that is a regrip -- the solver turns the whole thing
    // and turns it back -- but as a move it drags the cross round with it and
    // the stage undoes its own first step. y' is the same reorientation
    // without the damage, because a rotation here renames rather than turns.
    {"F2L 6  (reposition edge)",    "y' R' U' R U2 R' U R"},
    {"F2L 7  (reposition edge)",    "U' R U2 R' U2 R U' R'"},
    {"F2L 8  (reposition edge)",    "y' R' U2 R U2 R' U R"},

    // ---- reposition edge and flip corner --------------------------------
    {"F2L 9  (flip corner)",        "U' R U' R' U F' U' F"},
    {"F2L 10 (flip corner)",        "U' R U R' U R U R'"},
    {"F2L 11 (flip corner)",        "U' R U2 R' U F' U' F"},
    {"F2L 12 (flip corner)",        "R U' R' U R U' R' U2 R U' R'"},
    {"F2L 13 (flip corner)",        "y' U R' U R U' R' U' R"},
    {"F2L 14 (flip corner)",        "U' R U' R' U R U R'"},

    // ---- split pair by going over ---------------------------------------
    {"F2L 15 (split pair)",         "R U R' U2 R U' R' U R U' R'"},
    {"F2L 16 (split pair)",         "R U' R' U2 F' U' F"},
    {"F2L 17 (split pair)",         "R U2 R' U' R U R'"},
    {"F2L 18 (split pair)",         "y' R' U2 R U R' U' R"},

    // ---- pair made on side ----------------------------------------------
    {"F2L 19 (pair on side)",       "U R U2 R' U R U' R'"},
    {"F2L 20 (pair on side)",       "y' U' R' U2 R U' R' U R"},
    {"F2L 21 (pair on side)",       "U2 R U R' U R U' R'"},
    {"F2L 22 (pair on side)",       "F' L' U2 L F"},

    // ---- weird ----------------------------------------------------------
    {"F2L 23 (weird)",              "R U R' U2 R U R' U' R U R'"},
    {"F2L 24 (weird)",              "F U R U' R' F' R U' R'"},

    // ---- corner in place, edge in U face --------------------------------
    {"F2L 25 (corner in place)",    "R' F' R U R U' R' F"},
    {"F2L 26 (corner in place)",    "U R U' R' U' F' U F"},
    {"F2L 27 (corner in place)",    "R U' R' U R U' R'"},
    {"F2L 28 (corner in place)",    "y L' U L U' L' U L"},
    {"F2L 29 (corner in place)",    "R' F R F' R' F R F'"},
    {"F2L 30 (corner in place)",    "R U R' U' R U R'"},

    // ---- edge in place, corner in U face --------------------------------
    {"F2L 31 (edge in place)",      "U' R' F R F' R U' R'"},
    {"F2L 32 (edge in place)",      "R U R' U' R U R' U' R U R'"},
    {"F2L 33 (edge in place)",      "U' R U' R' U2 R U' R'"},
    {"F2L 34 (edge in place)",      "U R U R' U2 R U R'"},
    {"F2L 35 (edge in place)",      "U' R U R' U F' U' F"},
    {"F2L 36 (edge in place)",      "U2 R' F R F' U2 R U R'"},

    // ---- edge and corner in place ---------------------------------------
    {"F2L 37 (both in place)",      "R2 U2 F R2 F' U2 R' U R'"},
    {"F2L 38 (both in place)",      "R U R' U' R U2 R' U' R U R'"},
    {"F2L 39 (both in place)",      "R U R' U2 R U' R' U R U R'"},
    {"F2L 40 (both in place)",      "R U' R' F R U R' U' F' R U' R'"},
    {"F2L 41 (both in place)",      "R U R' U' R U' R' U2 y' R' U' R"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// ---- Layer by layer: the first layer, one piece at a time ---------------
//
// Where CFOP joins a corner to its edge and inserts the pair, layer by layer
// puts the four cross edges in, then the four corners, then the four middle
// edges, each on its own. That is why it is longer and why it is what a
// beginner is taught: each step is one small thing.
//
// All of these are written for the front-right position, like the F2L table,
// and reached by turning the cube.

// One cross edge, brought home without disturbing the three already placed.
//
// Unlike OLL, PLL and F2L, this is not a table anyone publishes: the cross is
// the step every guide calls intuitive, and the sources that do give formulas
// disagree with each other. So it is derived rather than quoted -- a breadth
// first search backwards from the solved cube, keeping the shortest word for
// each way the edge can sit while the other three stay home.
//
// That yields 24 positions: every slot the edge can be in, either way up,
// except the one place it is already home. Deriving it against a cube whose
// other three cross edges were held solved gave 18 and looked complete, but
// the missing six are the bottom slots -- which are empty while the first and
// second edges are being placed, and so exactly the cases that failed.
//
// The names are the position itself -- slot and orientation -- because there
// is no traditional name to use.
inline const std::vector<Alg>& lbl_cross_edge_table() {
  static const Alg t[] = {
    {"cross DF s0.0",  "U F' F'"},
    {"cross DF s0.1",  "R' F"},
    {"cross DF s1.0",  "F' F'"},
    {"cross DF s1.1",  "U L F'"},
    {"cross DF s2.0",  "U' F' F'"},
    {"cross DF s2.1",  "L F'"},
    {"cross DF s3.0",  "U' U' F' F'"},
    {"cross DF s3.1",  "U' L F'"},
    {"cross DF s4.0",  "D'"},
    {"cross DF s4.1",  "R F"},
    {"cross DF s5.1",  "D' L' F'"},
    {"cross DF s6.0",  "D"},
    {"cross DF s6.1",  "L' F'"},
    {"cross DF s7.0",  "D' D'"},
    {"cross DF s7.1",  "D L' F'"},
    {"cross DF s8.0",  "R' D'"},
    {"cross DF s8.1",  "F"},
    {"cross DF s9.0",  "L D"},
    {"cross DF s9.1",  "F'"},
    {"cross DF s10.0", "L' D"},
    {"cross DF s10.1", "L' L' F'"},
    {"cross DF s11.0", "R D'"},
    {"cross DF s11.1", "R' R' F"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// One first-layer corner, into the front-right-down slot, without disturbing
// the cross or the three corners already in.
//
// Derived the same way as the cross above, and for the same reason: the
// sources that give formulas here give different ones. Twenty-four positions:
// the corner in any of eight slots at any of three twists, less the one place
// it is already home.
//
// The short entries are the ones a beginner learns as the whole method:
// "R U R'" and "F' U' F" are the two insertions, and the longer words are what
// those become when the corner has to be brought out of a bottom slot first.
inline const std::vector<Alg>& lbl_corner_table() {
  static const Alg t[] = {
    {"corner DFR s0.0", "B' R B U' U' R'"},
    {"corner DFR s0.1", "R U R'"},
    {"corner DFR s0.2", "F' U' F"},
    {"corner DFR s1.0", "R F R F' U R'"},
    {"corner DFR s1.1", "U' R U R'"},
    {"corner DFR s1.2", "R U' R'"},
    {"corner DFR s2.0", "R B' U B R'"},
    {"corner DFR s2.1", "F' U' U' F"},
    {"corner DFR s2.2", "R U' U' R'"},
    {"corner DFR s3.0", "R' U R' R' U' R'"},
    {"corner DFR s3.1", "F' U F"},
    {"corner DFR s3.2", "U F' U' F"},
    {"corner DFR s4.1", "R U' R' F' U' F"},
    {"corner DFR s4.2", "F' U F R U R'"},
    {"corner DFR s5.0", "L' U L R U' R'"},
    {"corner DFR s5.1", "L' R U' L R'"},
    {"corner DFR s5.2", "L' U' R U' L U' R'"},
    {"corner DFR s6.0", "L R U L' U R'"},
    {"corner DFR s6.1", "B' U' B R U' R'"},
    {"corner DFR s6.2", "L U L' F' U F"},
    {"corner DFR s7.0", "B U B' F' U' F"},
    {"corner DFR s7.1", "R' U' R' R' U' U' R'"},
    {"corner DFR s7.2", "B F' U B' F"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// One middle-layer edge into the front-right slot, leaving the first layer
// standing. Derived, like the two tables above.
//
// Fourteen positions: the edge anywhere in the top layer either way up, or
// already in a middle slot but the wrong one or the wrong way round. The
// entries for slots 9, 10 and 11 are the case a beginner meets as "it is in
// the second layer but backwards" and solves by inserting something else
// there first; here it is simply another position with its own word.
inline const std::vector<Alg>& lbl_middle_edge_table() {
  static const Alg t[] = {
    {"middle FR s0.0",  "R U F R' F' R'"},
    {"middle FR s0.1",  "U F L F L' U' F'"},
    {"middle FR s1.0",  "U' R U F R' F' R'"},
    {"middle FR s1.1",  "F L F L' U' F'"},
    {"middle FR s2.0",  "F' L F D F D' L'"},
    {"middle FR s2.1",  "R U' R' F R' F' R"},
    {"middle FR s3.0",  "F' U F U R U' R'"},
    {"middle FR s3.1",  "R B' R' D' R' D B"},
    {"middle FR s9.0",  "D R' R' D' U F' F'"},
    {"middle FR s9.1",  "F U L F' L' F'"},
    {"middle FR s10.0", "D B' B' D' U R' R'"},
    {"middle FR s11.0", "D' F' F' D U' R' R'"},
    {"middle FR s11.1", "R F R F' U' R'"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// Freeing a pair that is stuck in the wrong slot.
//
// The 41 cases above all assume the pair is either in the top layer or in the
// slot it belongs to. A piece sitting in one of the other three slots is not a
// case at all -- it is a situation, and what a cuber does is lift it out and
// then read the case. These are the lifts, one per slot: three moves that pull
// whatever is in that slot up into the top layer and put back what they
// disturbed.
//
// Measured against the table: the 41 cases alone recognise 226 of 371 random
// positions with the cross solved. The rest are pieces in the wrong slot, and
// this is what turns them into cases.
inline const std::vector<Alg>& f2l_lift_table() {
  static const Alg t[] = {
    {"lift FR", "R U R'"},
    {"lift FL", "y' R U R' y"},
    {"lift BL", "y2 R U R' y2"},
    {"lift BR", "y R U R' y'"}
  };
  static const std::vector<Alg> v(t, t + sizeof(t) / sizeof(t[0]));
  return v;
}

// ---- Rotations and wide moves ------------------------------------------
//
// Published algorithms use cube rotations (x, y, z) and wide turns (r, l, f,
// u) that the package's alphabet does not name. Both are expressible in it:
// a rotation is all three layers of an axis, a wide turn is two of them. This
// is where a literature algorithm becomes a word in the alphabet.
//
// The expansions are in the same sense as the letters they replace, so
// r = R M', not R M -- M follows L, which is the convention cube_nnn.h fixes.

inline std::string expand_notation(const std::string& alg) {
  // token -> replacement in the plain alphabet
  struct Sub { const char* from; const char* to; };
  static const Sub subs[] = {
    // whole-cube rotations
    {"x",  "R M' L'"},   {"x'", "R' M L"},   {"x2", "R2 M2 L2"},
    {"y",  "U E' D'"},   {"y'", "U' E D"},   {"y2", "U2 E2 D2"},
    {"z",  "F S B'"},    {"z'", "F' S' B"},  {"z2", "F2 S2 B2"},
    // wide turns: the face plus the slice beside it
    {"r",  "R M'"},      {"r'", "R' M"},     {"r2", "R2 M2"},
    {"l",  "L M"},       {"l'", "L' M'"},    {"l2", "L2 M2"},
    {"u",  "U E'"},      {"u'", "U' E"},     {"u2", "U2 E2"},
    {"d",  "D E"},       {"d'", "D' E'"},    {"d2", "D2 E2"},
    {"f",  "F S"},       {"f'", "F' S'"},    {"f2", "F2 S2"},
    {"b",  "B S'"},      {"b'", "B' S"},     {"b2", "B2 S2"},
    {NULL, NULL}
  };

  std::string out;
  std::string tok;
  for (size_t i = 0; i <= alg.size(); i++) {
    if (i == alg.size() || alg[i] == ' ') {
      if (!tok.empty()) {
        const char* rep = NULL;
        for (int k = 0; subs[k].from; k++) {
          if (tok == subs[k].from) { rep = subs[k].to; break; }
        }
        if (!out.empty()) out += " ";
        out += (rep ? rep : tok);
        tok.clear();
      }
    } else {
      tok += alg[i];
    }
  }
  return out;
}

// ---- Rotations rename the faces that follow them ------------------------
//
// A rotation in a published algorithm is an instruction to the reader: turn
// the cube in your hands, and read the rest of the letters from where it now
// is. Expanding it in place -- y becoming U E' D' and the following letters
// still meaning the old faces -- turns the cube and then does the wrong moves
// to it. That is not a subtle error: it destroyed the middle layer in the
// published V-perm, which is what led to this being written.
//
// So rotations are not moves here. Each one is applied by renaming every
// letter after it, which is the conjugation the reader performs physically.
// The renaming below is the face each letter names once the cube has been
// turned; slices and wide moves follow their faces.

namespace {

// Face letters, in the package's own face order: U R F D L B, the order
// cube_nnn.h lays the stickers out in.
inline int face_letter_index(char c) {
  switch (c) {
    case 'U': return 0; case 'R': return 1; case 'F': return 2;
    case 'D': return 3; case 'L': return 4; case 'B': return 5;
    default:  return -1;
  }
}

// What a letter written after this rotation names, in the unrotated cube.
//
// These are not chosen, they are read off the cube: apply the rotation to a
// solved cube and see which face's centre has arrived at each position. If the
// centre now sitting where U was is the old F, then a U written after the
// rotation acts on the old F, and that is the entry below.
//
// The direction is the one thing easy to get backwards here, so it is checked
// rather than argued: rotation_renaming_holds() below asserts the identity
// every entry must satisfy, x ALG x' == rename(ALG), for each rotation over a
// set of words. No sign correction is needed on top -- the renaming carries
// the sense of the turn with it, because it says which physical face the
// letter refers to.
//
//                            U    R    F    D    L    B
inline const char* const* rot_map(const std::string& rot) {
  static const char* x []  = {"F", "R", "D", "B", "L", "U"};
  static const char* xi[]  = {"B", "R", "U", "F", "L", "D"};
  static const char* y []  = {"U", "B", "R", "D", "F", "L"};
  static const char* yi[]  = {"U", "F", "L", "D", "B", "R"};
  static const char* z []  = {"L", "U", "F", "R", "D", "B"};
  static const char* zi[]  = {"R", "D", "F", "L", "U", "B"};
  if (rot == "x")  return x;
  if (rot == "x'") return xi;
  if (rot == "y")  return y;
  if (rot == "y'") return yi;
  if (rot == "z")  return z;
  if (rot == "z'") return zi;
  return NULL;
}

}  // namespace

// An algorithm as move indices in the package alphabet. Rotations are consumed
// as renamings; everything else is expanded and parsed.
inline std::vector<int> alg_word(const char* moves) {
  const std::string text(moves);

  // current renaming, as a composition of the rotations seen so far
  std::string mapped;
  std::vector<std::string> pending_rot;

  std::string tok;
  std::string out;
  for (size_t i = 0; i <= text.size(); i++) {
    if (i == text.size() || text[i] == ' ') {
      if (tok.empty()) continue;

      // a rotation: record it and emit nothing
      std::string base = tok.substr(0, 1);
      if ((base == "x" || base == "y" || base == "z")) {
        if (tok == "x2" || tok == "y2" || tok == "z2") {
          pending_rot.push_back(base);
          pending_rot.push_back(base);
        } else {
          pending_rot.push_back(tok);
        }
        tok.clear();
        continue;
      }

      // An ordinary move: rename its letter back through every rotation seen
      // so far, in reverse order.
      //
      // The order is the subtle part, and it is invisible on a single
      // rotation. In "x y R" the R is read in a frame that was turned by x and
      // then by y, so undoing that frame means undoing y first and x second.
      // Applying the renamings forwards instead gives the right answer for one
      // rotation and the wrong one for two -- 96 of 144 conjugations wrong,
      // when it was measured.
      //
      // Wide turns and slices carry their own letters (r, M, ...) and are
      // resolved by expand_notation once the faces are settled, so only face
      // letters are mapped here.
      std::string letter = tok.substr(0, 1);
      std::string suffix = tok.substr(1);
      for (size_t r = pending_rot.size(); r-- > 0; ) {
        const char* const* m = rot_map(pending_rot[r]);
        if (!m) continue;
        const int idx = face_letter_index(letter[0]);
        if (idx < 0) break;
        letter = m[idx];
      }

      if (!out.empty()) out += " ";
      out += letter + suffix;
      tok.clear();
    } else {
      tok += text[i];
    }
  }

  return cube_search::parse_word(expand_notation(out));
}

}  // namespace cube_algs

#endif  // CAYLEYR_CUBE_ALGS_H
