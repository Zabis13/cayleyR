#ifndef CAYLEYR_CUBE_SOLVE_M2_H
#define CAYLEYR_CUBE_SOLVE_M2_H

#include <vector>
#include <string>
#include <stdexcept>
#include "cube_solve.h"
#include "cube_solve_old_pochmann.h"

// ---- M2 -----------------------------------------------------------------
//
// Old Pochmann's idea with a cheaper edge step, and the standard next method
// for someone solving blindfolded. There the buffer edge was placed by a PLL
// wrapped in setup moves -- fourteen moves before the setup was counted. Here
// the buffer is UB and the swap is M2, which is two. The corners are left
// exactly as old Pochmann does them, so this file borrows that half whole.
//
// What M2 costs is that it is not a clean swap. It turns the middle slice a
// half turn, which moves the centres, and it exchanges two pairs of edges
// besides the buffer's: the stickers lettered C and W, and E and O. None of
// that shows up while the count is even -- M2 is its own inverse, so a second
// application undoes the damage along with the swap -- and the whole method is
// arranged around keeping track of that parity rather than avoiding it.
//
// Three consequences, and they are the whole of what makes M2 different:
//
//   - The four letters M2 disturbs cannot be placed by M2. They have their own
//     algorithms, listed twice: one form for a letter in an odd position of
//     the sequence and its inverse for an even one.
//
//   - After an odd number of swaps the middle slice is left turned. The parity
//     algorithm between the edges and the corners puts it back.
//
//   - Unlike old Pochmann's T and J perms, M2 does not touch the corners at
//     all. So the edge stage runs up no debt on the corner stage, and the
//     parity step here is repairing edges, not corners -- the same name for a
//     different thing.

namespace cube_solve {

// The M2 buffer is DF, whose stickers are the letters U and G -- the two the
// published table marks "bank place". Setup moves bring the target where M2 can
// reach it, M2 swaps it with the buffer, and the setup is undone.
//
// The alphabet for a setup leaves the M slice alone: a setup that turned it
// would be undone by its own reversal and the swap would not have happened.
//
// A swap of two edges and nothing else is an odd permutation, which the cube
// group does not contain, so setup-M2-setup' always moves a second pair as
// well: the UF/DB edge, lettered C, W, E and O. Those cancel over an even
// number of swaps.
//
// Those four letters therefore cannot be targets -- aiming at the piece the
// algorithm is already disturbing does not place it. They are left to the
// orientation phase after the chain instead.
//
// A null entry is a sticker of the buffer or one of the four; an empty string
// means M2 alone.
inline const OldPochmannSetup* m2_edge_setups() {
  static const OldPochmannSetup t[24] = {
    {"",               0},   // A  M2 alone
    {"R' U R U'",      0},   // B
    {0,                0},   // C  UF/DB pair, own algorithm
    {"L U' L' U",      0},   // D
    {0,                0},   // E  UF/DB pair
    {"U R U'",         0},   // F
    {0,                0},   // G  buffer (DF flipped)
    {"U' L' U",        0},   // H
    {"B' R B",         0},   // I
    {"R' B' R B",      0},   // J
    {"B' R' B",        0},   // K
    {"B' R2 B",        0},   // L
    {"B' R B U R2 U'", 0},   // M
    {"U' L U",         0},   // N
    {0,                0},   // O  UF/DB pair
    {"U R' U'",        0},   // P
    {"B L' B'",        0},   // Q
    {"B L2 B'",        0},   // R
    {"B L B'",         0},   // S
    {"L B L' B'",      0},   // T
    {0,                0},   // U  buffer (DF)
    {"U R2 U'",        0},   // V
    {0,                0},   // W  UF/DB pair
    {"U' L2 U",        0}    // X
  };
  return t;
}

// Which of the four M-slice letters this is, or -1. C and W are the two
// stickers of one edge, E and O of the other; `second` marks W and O.
inline int m2_special_letter(int letter, bool& is_cw, bool& second) {
  switch (letter) {
    case 2:  is_cw = true;  second = false; return 0;   // C
    case 22: is_cw = true;  second = true;  return 0;   // W
    case 4:  is_cw = false; second = false; return 0;   // E
    case 14: is_cw = false; second = true;  return 0;   // O
    default: return -1;
  }
}

// Which edge slot a letter is a sticker of. Two letters share a slot -- they
// are the two faces of one piece -- and telling them apart from two letters on
// different pieces is the whole of what separates a flipped edge from a cycle.
inline int edge_of_letter(int letter) {
  const int* EL = old_pochmann_edge_letters();
  const EdgeFacelets* ef = edge_facelets();
  for (int s = 0; s < 12; s++) {
    for (int k = 0; k < 2; k++) {
      if (ef[s].s[k] == EL[letter]) return s;
    }
  }
  return -1;
}

inline void solve_m2_into(Solution& sol, const std::vector<int>& start) {
  sol.solved = false;
  std::vector<int> state = start;

  {
    std::vector<int> w = orient_to_centres(state);
    if (!w.empty()) push_stage(sol, state, "orientation", "centres", w);
  }

  const int* EL = old_pochmann_edge_letters();
  const int* CL = old_pochmann_corner_letters();
  const OldPochmannSetup* ES = m2_edge_setups();
  const OldPochmannSetup* CS = old_pochmann_corner_setups();

  static const char* LETTER = "ABCDEFGHIJKLMNOPQRSTUVWX";
  const std::vector<int> m2 = alg_word("M2");

  // ---- Edges ------------------------------------------------------------
  //
  // The buffer is UB, letters A and G. The chain runs as it does in old
  // Pochmann: place what is in the buffer, take back what was there, repeat.
  // The difference is only in how one swap is performed.
  //
  // Position in the sequence is what selects between the two forms of the four
  // special algorithms, so it is counted here rather than inferred later.
  int edge_swaps = 0;
  {
    // The two stickers of DF, and U comes first because it is the one the
    // chain is read from: a conjugate aimed at a letter leaves exactly that
    // letter's piece on the U sticker, while G picks up whatever the second
    // pair brought round. Reading the wrong one names a piece the next setup
    // was not built for.
    const int buffer_letters[2] = {20, 6};  // U, G
    int guard = 0;
    while (true) {
      if (++guard > 200) throw std::runtime_error("cube_solve: M2 edge chain did not close");

      // Read the cube as it actually is. The M2 in every conjugate really does
      // swap UF with DB -- that is a thing that happened to the cube, not a
      // wrinkle in how it is being looked at -- so turning the slice back before
      // reading names pieces that are not there. Doing that scored 0 of 280.
      const std::vector<int>& frame = state;

      int target = old_pochmann_letter_at(frame, EL, buffer_letters[0]);
      if (target < 0) throw std::runtime_error("cube_solve: M2 edge letter not found");

      bool buffer_piece = (target == buffer_letters[0] || target == buffer_letters[1]);
      if (buffer_piece) {
        // A cycle the chain never reached -- but only a cycle, never a piece
        // that is merely turned over. An edge home the wrong way round is
        // "inactive" in the method's own word: it is set aside before any chain
        // is built and left out of all of them, because the chains move pieces
        // between slots and orientation is not their business. It is the
        // endgame below that turns them.
        //
        // Breaking a chain into an inactive edge is what wrecked this stage
        // before. The chain would aim at C, land the piece on C's own slot the
        // other way up, read the buffer, get C back, and aim at C again. Four
        // letters wrong, everything else home, and the guard reached.
        int broken = -1;
        for (int i = 0; i < 24 && broken < 0; i++) {
          if (ES[i].moves == 0) continue;
          int at = old_pochmann_letter_at(frame, EL, i);
          if (at == i) continue;                       // solved
          if (edge_of_letter(i) == edge_of_letter(at)) continue;   // flipped in place
          broken = i;
        }
        if (broken < 0) break;
        target = broken;
      }

      std::vector<int> w;
      std::string detail;
      bool is_cw = false, second = false;

      if (m2_special_letter(target, is_cw, second) >= 0) {
        // The two edges M2 itself disturbs. Their algorithms replace the whole
        // conjugate, and which of the two forms is right depends on the parity
        // of the count: every conjugate turns the M slice a half turn, so on an
        // odd step the pair is reached the other way round.
        bool odd = (edge_swaps % 2 == 1);
        int form = (second == odd) ? 0 : 1;
        const std::vector<Alg>& tab = is_cw ? m2_cw_table() : m2_eo_table();
        w = alg_word(tab[form].moves);
        detail = std::string(1, LETTER[target]) + " (" + tab[form].name + ")";
      } else {
        if (ES[target].moves == 0) {
          throw std::runtime_error("cube_solve: M2 edge target has no setup");
        }
        w = old_pochmann_conjugate(ES[target].moves, m2);
        detail = std::string(1, LETTER[target]) + " (M2)";
      }

      push_stage(sol, state, "edge", detail, w);
      edge_swaps++;
    }
  }

  // ---- Edge orientation, the endgame --------------------------------------
  //
  // The chains moved pieces between slots and never turned one over, so what
  // is left is edges in the right places facing the wrong way. They come in
  // pairs -- a single flipped edge is not a cube position -- and each pair is
  // the one flip algorithm with a setup around it.
  //
  // This is a separate phase and not a case inside the chain, which is the
  // whole point: orientation and permutation are independent, and trying to
  // carry orientation through the cycle phase is what left four letters wrong
  // at the end of 136 solves out of 280.
  {
    int guard = 0;
    while (true) {
      if (++guard > 8) throw std::runtime_error("cube_solve: edge orientation did not close");

      CubieState c = read_state(state);
      int a = -1, b = -1;
      for (int e = 0; e < 12; e++) {
        if (c.eo[e] == 0) continue;
        if (a < 0) a = e; else if (b < 0) { b = e; break; }
      }
      if (a < 0) break;                 // all oriented
      if (b < 0) throw std::runtime_error("cube_solve: one edge flipped alone");

      const char* setup = eo_setup_for(a, b);
      if (setup == 0) throw std::runtime_error("cube_solve: no flip setup for that pair");

      std::vector<int> w = old_pochmann_conjugate(setup, alg_word(eo_base_alg()));
      push_stage(sol, state, "edge orientation",
                 std::string(edge_names()[a]) + "+" + edge_names()[b], w);
    }
  }

  // ---- Parity -----------------------------------------------------------
  //
  // An odd number of M2s leaves the middle slice turned. This puts it back,
  // and it goes here because the corner stage is stated against the centres.
  // Measured, not counted. The number of swaps the chain made is supposed to
  // tell you whether the parity fix is needed, and mostly it does -- but a
  // chain that breaks into a new cycle, or steps over an edge left flipped in
  // place, spends turns that the count does not see. Three solves in 280 ended
  // with the tally saying one thing and the cube saying the other: two chains
  // of ten swaps whose edges were an odd permutation, and one of nine whose
  // edges were even. Asking the permutation directly cannot drift.
  bool edges_odd;
  {
    CubieState c = read_state(state);
    bool seen[12] = {false};
    int transpositions = 0;
    for (int i = 0; i < 12; i++) {
      if (seen[i]) continue;
      int len = 0;
      for (int j = i; !seen[j]; j = c.ep[j]) { seen[j] = true; len++; }
      transpositions += len - 1;
    }
    edges_odd = (transpositions % 2 == 1);
  }

  if (edges_odd) {
    const Alg& p = m2_parity_table()[0];
    push_stage(sol, state, "parity", p.name, alg_word(p.moves));
  }

  // ---- Corners ----------------------------------------------------------
  //
  // Unchanged from old Pochmann: buffer at ULB, one Y-perm per piece, setup
  // moves around it.
  {
    const int buffer_letters[3] = {0, 13, 16};   // A, N, Q
    int guard = 0;
    while (true) {
      if (++guard > 200) throw std::runtime_error("cube_solve: M2 corner chain did not close");

      int target = old_pochmann_letter_at(state, CL, buffer_letters[0]);
      if (target < 0) throw std::runtime_error("cube_solve: M2 corner letter not found");

      bool buffer_piece = (target == buffer_letters[0] ||
                           target == buffer_letters[1] ||
                           target == buffer_letters[2]);
      if (buffer_piece) {
        int broken = -1;
        for (int i = 0; i < 24 && broken < 0; i++) {
          if (CS[i].moves == 0) continue;
          if (old_pochmann_letter_at(state, CL, i) != i) broken = i;
        }
        if (broken < 0) break;
        target = broken;
      }

      if (CS[target].moves == 0) {
        throw std::runtime_error("cube_solve: M2 corner target is a buffer sticker");
      }

      const Alg& a = old_pochmann_corner_table()[0];
      std::vector<int> w = old_pochmann_conjugate(CS[target].moves, alg_word(a.moves));
      std::string detail = std::string(1, LETTER[target]) + " (" + a.name + ")";
      push_stage(sol, state, "corner", detail, w);
    }
  }

  sol.solved = cube_solved(read_state(state));
}

inline Solution solve_m2(const std::vector<int>& start) {
  Solution sol;
  sol.solved = false;
  solve_m2_into(sol, start);
  return sol;
}

}  // namespace cube_solve

#endif  // CAYLEYR_CUBE_SOLVE_M2_H
