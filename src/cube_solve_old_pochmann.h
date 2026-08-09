#ifndef CAYLEYR_CUBE_SOLVE_OLD_POCHMANN_H
#define CAYLEYR_CUBE_SOLVE_OLD_POCHMANN_H

#include <vector>
#include <string>
#include <stdexcept>
#include "cube_solve.h"

// ---- Old Pochmann -------------------------------------------------------
//
// The blindfolded method, and it is built on a different principle from the
// sighted ones. CFOP and layer-by-layer both work by looking: a stage ends when
// the cuber can see that it has, and the next stage is chosen from what is now
// on the top face. Neither can be done without eyes. Old Pochmann never looks
// at the cube after the start, so it cannot branch on what it finds; instead it
// repeats one step whose shape is always the same.
//
// The step is: swap whatever is in the buffer with one chosen piece, disturbing
// nothing else, and repeat. Follow it and the pieces come home in a chain --
// the buffer piece goes where it belongs, which evicts the piece that was
// there, which becomes the new buffer piece, and so on. The chain closes when
// the buffer receives its own piece, and if the cube is not finished there is
// another cycle elsewhere to break into.
//
// Two things make it work as an algorithm rather than a wish:
//
//   - A single swap is not a cube move. Swapping two edges and nothing else is
//     an odd permutation, and the cube group has none. So every algorithm here
//     swaps a second, fixed pair as well -- the edge algorithms also swap the
//     corners URF and UBR, the corner algorithm also swaps the edges UL and UB
//     -- and those extra swaps cancel each other when their count is even. When
//     it is odd, one is left over, and that is the parity fix.
//
//   - A piece has to be named by a sticker, not by a slot. "Put the buffer edge
//     into DL" does not say which way round it lands, and landing it the wrong
//     way round is not solving it. The standard lettering names all 24 edge
//     stickers A to X and all 24 corner stickers likewise, and the tables below
//     are indexed by that letter. Two letters on the same piece are different
//     targets with different setup moves.
//
// The cost of never looking is length: around 350 quarter turns against 160 for
// layer-by-layer, because every piece is placed by its own conjugated algorithm
// and nothing is ever done to two pieces at once.

namespace cube_solve {

// ---- The lettering ------------------------------------------------------
//
// Sticker positions, 0-based, in the order the letters run: the four stickers
// of the U face, then F, R, B, L, D, clockwise within each face starting from
// the one nearest the top-left. Edges take the edge stickers, corners the
// corner stickers. Derived from the package's own facelet tables and checked
// against the published setup moves rather than assumed.

inline const int* old_pochmann_edge_letters() {
  // U: UB UR UF UL | F: FU FR FD FL | R: RU RB RD RF
  // B: BU BL BD BR | L: LU LF LD LB | D: DF DR DB DL
  static const int t[24] = {
     1,  5,  7,  3,   // A B C D   (U face)
    19, 23, 25, 21,   // E F G H   (F face)
    10, 14, 16, 12,   // I J K L   (R face)
    46, 50, 52, 48,   // M N O P   (B face)
    37, 41, 43, 39,   // Q R S T   (L face)
    28, 32, 34, 30    // U V W X   (D face)
  };
  return t;
}

inline const int* old_pochmann_corner_letters() {
  static const int t[24] = {
     0,  2,  8,  6,   // A B C D   (U face)
    18, 20, 26, 24,   // E F G H   (F face)
     9, 11, 17, 15,   // I J K L   (R face)
    45, 47, 53, 51,   // M N O P   (B face)
    36, 38, 44, 42,   // Q R S T   (L face)
    27, 29, 35, 33    // U V W X   (D face)
  };
  return t;
}

// ---- Setup moves --------------------------------------------------------
//
// A setup move carries the target sticker to the one place the algorithm can
// reach, and is undone afterwards so that nothing else is left disturbed. The
// whole step is therefore a conjugate: setup, algorithm, setup reversed.
//
// Which place depends on the algorithm. The T-perm swaps the edge buffer with
// UL and the J-perm with UF, so the edge letters split between them and the
// table records which. Every corner goes through the Y-perm, which swaps the
// corner buffer with URF.
//
// The constraint on a setup move is that it must not disturb the buffer or the
// pieces the algorithm swaps as its second pair. For edges that rules out R, F,
// B and S; for corners it rules out U and anything moving UL or UB.
//
// An empty string means the target is already where the algorithm wants it. A
// null entry means the letter is a sticker of the buffer piece itself, which is
// never a target: it is where the chain starts and where it closes.

struct OldPochmannSetup {
  const char* moves;   // setup word, "" if none, 0 if not a target
  int alg;             // index into old_pochmann_edge_table(); corners ignore it
};

inline const OldPochmannSetup* old_pochmann_edge_setups() {
  // alg 0 = T (target UL), alg 1 = J (target UF)
  static const OldPochmannSetup t[24] = {
    {"l2 D' L2", 0},   // A  UB
    {0,          0},   // B  buffer (UR)
    {"",         1},   // C  UF
    {"",         0},   // D  UL
    {"l D' L2",  0},   // E  UF flipped
    {"d2 L",     0},   // F  FR
    {"l'",       1},   // G  DF flipped
    {"L'",       0},   // H  FL
    {0,          0},   // I  buffer (UR flipped)
    {"d L",      0},   // J  BR flipped
    {"D' l'",    1},   // K  DR flipped
    {"d' L'",    0},   // L  FR flipped
    {"l",        1},   // M  UB flipped
    {"L",        0},   // N  BL
    {"D2 l'",    1},   // O  DB flipped
    {"d2 L'",    0},   // P  BR
    {"L2 d l'",  1},   // Q  UL flipped
    {"d' L",     0},   // R  FL flipped
    {"D l'",     1},   // S  DL flipped
    {"d L'",     0},   // T  BL flipped
    {"D' L2",    0},   // U  DF
    {"D2 L2",    0},   // V  DR
    {"D L2",     0},   // W  DB
    {"L2",       0}    // X  DL
  };
  return t;
}

// Found by search rather than copied: the published tables are written for a
// Y-perm in a particular framing, and the one used here swaps the buffer with
// sticker C. A setup taken from a table written against a different target
// silently aims at the wrong corner -- which is what the letter C did, sending
// the buffer to UFL for ever. So each entry is the shortest word, over an
// alphabet that cannot touch the buffer or the helper edges UL and UB, that
// brings the target sticker to C.
inline const OldPochmannSetup* old_pochmann_corner_setups() {
  static const OldPochmannSetup t[24] = {
    {0,        0},   // A  buffer (ULB)
    {"R D' F'", 0},  // B
    {"",       0},   // C  where the algorithm reaches
    {"F R' F'", 0},  // D
    {"F2 R",   0},   // E
    {"F R",    0},   // F
    {"R",      0},   // G
    {"D F'",   0},   // H
    {"R' F'",  0},   // I
    {"R2 F'",  0},   // J
    {"R F'",   0},   // K
    {"F'",     0},   // L
    {"R'",     0},   // M
    {0,        0},   // N  buffer (ULB twisted)
    {"D2 R",   0},   // O
    {"D' F'",  0},   // P
    {0,        0},   // Q  buffer (ULB twisted)
    {"F",      0},   // R
    {"D R",    0},   // S
    {"D2 F'",  0},   // T
    {"F2",     0},   // U
    {"R F R",  0},   // V
    {"R2",     0},   // W
    {"D F2",   0}    // X
  };
  return t;
}

// Where each algorithm takes its target from: the sticker the buffer piece is
// swapped with. Edge letter D (UL) for the T-perm, C (UF) for the J-perm, and
// corner letter L (DFR, reached from URF by the setup) for the Y-perm.
inline int old_pochmann_edge_target(int alg) {
  return alg == 0 ? old_pochmann_edge_letters()[3] : old_pochmann_edge_letters()[2];
}
inline int old_pochmann_corner_target() { return old_pochmann_corner_letters()[11]; }

// ---- Reading the cube in letters ---------------------------------------
//
// Which letter names the sticker now sitting at a given letter's home. The
// state vector holds, at each position, the sticker that is there; a letter is
// a position; so this is a lookup and then a search back through the table.

inline int old_pochmann_letter_at(const std::vector<int>& state, const int* letters,
                              int letter) {
  int sticker = state[letters[letter]];
  for (int i = 0; i < 24; i++) if (letters[i] == sticker - 1) return i;
  return -1;
}

// The letters that belong to the same physical piece as this one.
inline void old_pochmann_piece_letters(const int* letters, int letter, bool corner,
                                   int out[3], int& n) {
  n = 0;
  const int per = corner ? 3 : 2;
  // Stickers of one piece are those whose home slots coincide; rather than
  // re-derive the geometry, read it off the solved cube through the cubie
  // tables, which already know which facelets make a piece.
  if (corner) {
    const CornerFacelets* cf = corner_facelets();
    for (int p = 0; p < 8; p++) {
      bool mine = false;
      for (int k = 0; k < 3; k++) if (cf[p].s[k] == letters[letter]) mine = true;
      if (!mine) continue;
      for (int k = 0; k < 3; k++)
        for (int i = 0; i < 24; i++)
          if (letters[i] == cf[p].s[k]) out[n++] = i;
      return;
    }
  } else {
    const EdgeFacelets* ef = edge_facelets();
    for (int p = 0; p < 12; p++) {
      bool mine = false;
      for (int k = 0; k < 2; k++) if (ef[p].s[k] == letters[letter]) mine = true;
      if (!mine) continue;
      for (int k = 0; k < 2; k++)
        for (int i = 0; i < 24; i++)
          if (letters[i] == ef[p].s[k]) out[n++] = i;
      return;
    }
  }
  (void)per;
}

// ---- One swap -----------------------------------------------------------
//
// setup, algorithm, setup reversed. The reversal is what keeps the rest of the
// cube untouched, and it is why the setup alphabet is restricted: a setup that
// moved the buffer would be undone along with everything else and the swap
// would not have happened.

inline std::vector<int> old_pochmann_conjugate(const std::string& setup,
                                           const std::vector<int>& alg) {
  std::vector<int> pre = alg_word(setup.c_str());
  std::vector<int> w = pre;
  for (size_t i = 0; i < alg.size(); i++) w.push_back(alg[i]);
  // The alphabet pairs each move with its inverse, so m ^ 1 undoes m.
  for (size_t i = pre.size(); i-- > 0; ) w.push_back(pre[i] ^ 1);
  return w;
}

// ---- The solve ----------------------------------------------------------

inline void solve_old_pochmann_into(Solution& sol, const std::vector<int>& start) {
  sol.solved = false;
  std::vector<int> state = start;

  // Slice moves may have left the centres turned, and every letter below is a
  // position stated against them.
  {
    std::vector<int> w = orient_to_centres(state);
    if (!w.empty()) push_stage(sol, state, "orientation", "centres", w);
  }

  const int* EL = old_pochmann_edge_letters();
  const int* CL = old_pochmann_corner_letters();
  const OldPochmannSetup* ES = old_pochmann_edge_setups();
  const OldPochmannSetup* CS = old_pochmann_corner_setups();

  static const char* LETTER = "ABCDEFGHIJKLMNOPQRSTUVWX";

  // ---- Edges ------------------------------------------------------------
  //
  // The buffer is UR, letters B and I. Each swap sends the buffer sticker to
  // its home and brings back whatever was there; the chain is followed until
  // the buffer holds its own piece, then any remaining cycle is broken into by
  // aiming at one of its letters.
  int edge_swaps = 0;
  {
    const int buffer_letters[2] = {1, 8};   // B, I
    int guard = 0;
    while (true) {
      if (++guard > 200) throw std::runtime_error("cube_solve: edge chain did not close");

      // what is in the buffer now
      int target = old_pochmann_letter_at(state, EL, buffer_letters[0]);
      if (target < 0) throw std::runtime_error("cube_solve: edge letter not found");

      bool buffer_piece = (target == buffer_letters[0] || target == buffer_letters[1]);
      if (buffer_piece) {
        // The chain has closed. If anything is still out of place it is in a
        // cycle the buffer never reached, so break in at any of its letters.
        int broken = -1;
        for (int i = 0; i < 24 && broken < 0; i++) {
          if (ES[i].moves == 0) continue;
          if (old_pochmann_letter_at(state, EL, i) != i) broken = i;
        }
        if (broken < 0) break;   // edges done
        target = broken;
      }

      if (ES[target].moves == 0) {
        throw std::runtime_error("cube_solve: edge target is a buffer sticker");
      }

      const Alg& a = old_pochmann_edge_table()[ES[target].alg];
      std::vector<int> w = old_pochmann_conjugate(ES[target].moves, alg_word(a.moves));
      std::string detail = std::string(1, LETTER[target]) + " (" + a.name + ")";
      push_stage(sol, state, "edge", detail, w);
      edge_swaps++;
    }
  }

  // ---- Parity -----------------------------------------------------------
  //
  // Each edge swap also swapped the corners URF and UBR. An even number of them
  // cancels; an odd number leaves the corners one swap out, and no amount of
  // corner work can fix that, because the corner algorithm swaps a pair too.
  // This puts both right at once and must come between the two stages.
  if (edge_swaps % 2 == 1) {
    const Alg& p = old_pochmann_parity_table()[0];
    push_stage(sol, state, "parity", p.name, alg_word(p.moves));
  }

  // ---- Corners ----------------------------------------------------------
  //
  // The same again with the buffer at ULB, letters A, N and Q. A corner has
  // three stickers rather than two, so a piece that is home but twisted shows
  // up as a chain of length two rather than one, and needs no special case.
  {
    const int buffer_letters[3] = {0, 13, 16};   // A, N, Q
    int guard = 0;
    while (true) {
      if (++guard > 200) throw std::runtime_error("cube_solve: corner chain did not close");

      int target = old_pochmann_letter_at(state, CL, buffer_letters[0]);
      if (target < 0) throw std::runtime_error("cube_solve: corner letter not found");

      bool buffer_piece = (target == buffer_letters[0] ||
                           target == buffer_letters[1] ||
                           target == buffer_letters[2]);
      if (buffer_piece) {
        int broken = -1;
        for (int i = 0; i < 24 && broken < 0; i++) {
          if (CS[i].moves == 0) continue;
          if (old_pochmann_letter_at(state, CL, i) != i) broken = i;
        }
        if (broken < 0) break;   // corners done
        target = broken;
      }

      if (CS[target].moves == 0) {
        throw std::runtime_error("cube_solve: corner target is a buffer sticker");
      }

      const Alg& a = old_pochmann_corner_table()[0];
      std::vector<int> w = old_pochmann_conjugate(CS[target].moves, alg_word(a.moves));
      std::string detail = std::string(1, LETTER[target]) + " (" + a.name + ")";
      push_stage(sol, state, "corner", detail, w);
    }
  }

  sol.solved = cube_solved(read_state(state));
}

inline Solution solve_old_pochmann(const std::vector<int>& start) {
  Solution sol;
  sol.solved = false;
  solve_old_pochmann_into(sol, start);
  return sol;
}

}  // namespace cube_solve

#endif  // CAYLEYR_CUBE_SOLVE_OLD_POCHMANN_H
