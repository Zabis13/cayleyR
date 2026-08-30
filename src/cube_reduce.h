#ifndef CAYLEYR_CUBE_REDUCE_H
#define CAYLEYR_CUBE_REDUCE_H

#include <vector>
#include <string>
#include "cube_edges.h"

// ---- From a reduced 4x4x4 to a solved one ---------------------------------
//
// Once cube_centres.h has built the centres and cube_edges.h has paired the
// edges, the cube behaves as a 3x3x3: the four pieces of a centre move as one,
// the two halves of an edge move as one. The 3x3x3 solver already in this
// package can finish it -- but three things stand between here and there, and
// each was measured rather than assumed.
//
// ---- One: the state has to be squeezed ------------------------------------
//
// cube_solve_cfop() takes 54 stickers. A 4x4x4 face is a 4x4 grid and a 3x3x3
// face is 3x3, so rows and columns fold 0,1,2,3 -> 0,1,1,2. Corners map one to
// one, the two halves of an edge both land on the same 3x3x3 edge, and the
// four centre pieces all land on the one centre. Checked: the solved 4x4x4
// squeezes to the solved 3x3x3, the colour counts come out nine apiece, and
// squeezing a turn equals turning the squeeze on all six faces.
//
// ---- Two: parity -----------------------------------------------------------
//
// An even cube reaches last-layer states a 3x3x3 cannot, so the tables of 57
// OLL and 21 PLL cases do not contain them and the solver stops with "no OLL
// case matched" or "no PLL case matched". Measured over forty reduced states:
// nine solved outright, nineteen stopped on OLL, twelve on PLL.
//
// Both are repaired here, on the 4x4x4, before the squeeze -- the algorithms
// turn inner layers, which the 3x3x3 has no way to express.
//
// The two are NOT independent, which the transition table shows: of the
// nineteen OLL cases, applying the OLL algorithm solved nine and turned ten
// into PLL cases. So the repair is a loop -- classify, fix, classify again --
// rather than one pass of each.
//
// ---- Three: the notation lies ----------------------------------------------
//
// The published parity algorithms write r for the inner slice alone, while
// this package's parser reads r as the wide turn R plus the slice. Measured:
// read as a wide turn the PLL algorithm leaves eight pairs of twelve and the
// centres broken; read as the slice alone it leaves all twelve and the centres
// built. So the PLL word below is spelled in package moves, not in the
// notation it came in.

namespace cube_solve {

// The squeeze: 96 stickers of a reduced 4x4x4 to the 54 colours of a 3x3x3.
inline std::vector<int> squeeze_to_3(const std::vector<int>& state) {
  static const int fold[4] = {0, 1, 1, 2};
  std::vector<int> out(54);
  for (int f = 0; f < 6; f++) {
    for (int i = 0; i < 16; i++) {
      const int r3 = fold[i / 4], c3 = fold[i % 4];
      out[f * 9 + r3 * 3 + c3] = (state[f * 16 + i] - 1) / 16;
    }
  }
  return out;
}

// ---- Parity algorithms ----------------------------------------------------
//
// Measured on the solved cube before use. The OLL word leaves the centres
// built and all twelve pairs intact -- it changes orientation only, which is
// what it is for. The PLL word does the same once r is read as the slice.
inline const char* oll_parity_word() {
  // Rw2 B2 U2 Lw U2 Rw' U2 Rw U2 F2 Rw F2 Lw' B2 Rw2, as cube_expand_word()
  // spells it for a 4x4x4. Transcribed from that output rather than by hand --
  // a hand copy of this line had two extra U turns in it, caught by comparing
  // against the expansion.
  return "R 2x R 2x B B U U L 1x' U U R' 2x' U U R 2x U U F F "
         "R 2x F F L' 1x B B R 2x R 2x";
}

inline const char* pll_parity_word() {
  // r2 U2 r2 Uw2 r2 u2 with r as the inner slice, not the wide turn
  return "2x 2x U U 2x 2x U 2y U 2y 2x 2x U 2y U 2y";
}

// ---- Lifting a 3x3x3 solution back to the 4x4x4 ---------------------------
//
// The face turns carry over one for one -- measured, each of the twelve has
// exactly one 4x4x4 move with the same effect on the squeeze. The slices do
// not, and this is where the obvious answer is wrong.
//
// Searching for a 4x4x4 move matching each 3x3x3 slice ON THE SQUEEZE finds
// E <- 1y', a single inner layer, because the squeeze cannot tell one half of
// an edge from the other and a single layer looks right to it. Physically it
// is not: measured on a reduced cube, 1y alone takes the pairing from twelve
// down to eight. A 3x3x3 slice is the whole middle of the cube, so on a 4x4x4
// it is BOTH inner layers -- which is what M and S already needed, and E turns
// out to need the same.
//
// With E lifted as a single layer the full solve finished 6 of 30; with both
// layers, 100 of 100.
inline const char* lift_move(const std::string& m) {
  if (m == "U")  return "U";
  if (m == "U'") return "U'";
  if (m == "R")  return "R";
  if (m == "R'") return "R'";
  if (m == "F")  return "F";
  if (m == "F'") return "F'";
  if (m == "D")  return "D";
  if (m == "D'") return "D'";
  if (m == "L")  return "L";
  if (m == "L'") return "L'";
  if (m == "B")  return "B";
  if (m == "B'") return "B'";
  if (m == "E")  return "1y' 2y'";
  if (m == "E'") return "1y 2y";
  if (m == "M")  return "1x' 2x'";
  if (m == "M'") return "1x 2x";
  if (m == "S")  return "1z 2z";
  if (m == "S'") return "1z' 2z'";
  return NULL;
}

// A whole 3x3x3 solution as 4x4x4 moves.
inline std::string lift_word(const std::vector<std::string>& path) {
  std::string out;
  for (size_t i = 0; i < path.size(); i++) {
    const char* w = lift_move(path[i]);
    if (!w) throw std::runtime_error("cube_reduce: cannot lift move '" +
                                     path[i] + "'");
    if (!out.empty()) out += " ";
    out += w;
  }
  return out;
}

}  // namespace cube_solve

#endif  // CAYLEYR_CUBE_REDUCE_H
