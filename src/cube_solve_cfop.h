#ifndef CAYLEYR_CUBE_SOLVE_CFOP_H
#define CAYLEYR_CUBE_SOLVE_CFOP_H

#include <vector>
#include <string>
#include <stdexcept>
#include "cube_solve.h"

// ---- CFOP ---------------------------------------------------------------
//
// Cross, then the four first-two-layer pairs, then orient the last layer, then
// permute it. The cross and the pairs are searched for; the last layer is
// looked up, which is the division of labour the method itself makes -- the
// first half is intuitive and the second is memorised.
//
// Where it differs from layer by layer is the middle. CFOP does a corner and
// its edge together in four pairs, which is fewer moves and much harder to
// see; LBL finishes the bottom layer first and then inserts the middle edges
// one at a time.

namespace cube_solve {

// How deep each stage is allowed to search. A cross is at most 8 quarter turns
// from any scramble; an F2L pair inserted into an empty slot is at most 11,
// and these are the standard bounds.
struct CfopLimits {
  int cross_depth;
  int slot_depth;
  CfopLimits() : cross_depth(8), slot_depth(11) {}
};

// As in the layer-by-layer method: the body writes into a Solution the caller
// owns, so a throw leaves the stages that landed behind for inspection.
inline void solve_cfop_into(Solution& sol, const std::vector<int>& start,
                            const CfopLimits& lim) {
  sol.solved = false;
  std::vector<int> state = start;

  // ---- Orientation ------------------------------------------------------
  // Slice moves may have turned the centres, and every stage below is stated
  // against them. Turn the whole cube back first -- see orient_to_centres().
  {
    std::vector<int> w = orient_to_centres(state);
    if (!w.empty()) push_stage(sol, state, "orientation", "centres", w);
  }

  // ---- Cross -----------------------------------------------------------
  //
  // The four D edges, searched for with face turns only.
  //
  // Excluding the slices is not a convenience, it is what keeps the rest of
  // the method meaningful. M, E and S turn the centres, and every stage after
  // this one is stated against the centres; worse, a slice changes the parity
  // of the edge permutation without touching the corners, so a cross found
  // with one hands the last layer a position no real cube can be in. PLL then
  // correctly matches nothing, and the failure surfaces four stages away from
  // its cause.
  //
  // Measured: with the full alphabet the parity was already broken after the
  // cross on every scramble tried, and half the solves died at PLL.
  {
    std::vector<int> w;
    if (!ida_solve_cubie(state, [](const CubieState& c) { return cross_solved(c); },
                         moves_faces(), lim.cross_depth, w)) {
      throw std::runtime_error("cube_solve: no cross within " +
                               std::to_string(lim.cross_depth) + " moves");
    }
    push_stage(sol, state, "cross", "", w);
  }

  // ---- F2L: four corner-edge pairs -------------------------------------
  //
  // Each pair is looked up, not searched for. The 41 cases below cover every
  // way a pair can sit once it is in the top layer or in its own slot, and a
  // pair stuck anywhere else is lifted out first -- which is what a cuber does
  // without thinking of it as a case at all.
  //
  // The table is written for the front-right slot alone. There is one slot and
  // four ways to face it, so the other three are reached by turning the cube,
  // and turning the cube is free: a rotation renames the faces and moves no
  // piece relative to another.
  //
  // Searching instead was the first attempt and does not work. An exact search
  // over all 18 moves to the depth an F2L pair needs is around 18^11 nodes; it
  // exhausted its budget at depth 7 and took over a minute to do it.
  for (int slot = 0; slot < 4; slot++) {
    const std::string label = "F2L slot " + std::to_string(slot + 1);

    // Bring this slot to the front right. Slot i sits i quarter turns round
    // from FR, so undoing that is y' applied i times.
    std::string face;
    for (int k = 0; k < slot; k++) face += "y' ";

    bool placed = false;
    for (int round = 0; round < 8 && !placed; round++) {
      {
        CubieState c = read_state(state);
        if (cross_solved(c) && slot_solved(c, slot)) { placed = true; break; }
      }

      // try to recognise the case
      std::vector<int> w;
      std::string name;
      const int target = slot;
      auto goal = [target](const CubieState& c) {
        return cross_solved(c) && slot_solved(c, target);
      };
      if (match_alg_prefixed(state, f2l_table(), face, goal, w, name)) {
        push_stage(sol, state, label, name, w);
        placed = true;
        break;
      }

      // not a case: the pair is stuck in another slot, so lift it out and
      // look again
      if (!lift_stuck_pair(state, slot, sol, label)) {
        throw std::runtime_error("cube_solve: F2L slot " +
                                 std::to_string(slot + 1) + " not recognised");
      }
    }

    if (!placed) {
      throw std::runtime_error("cube_solve: F2L slot " +
                               std::to_string(slot + 1) + " did not close");
    }
  }

  // ---- OLL --------------------------------------------------------------
  {
    std::vector<int> w;
    std::string name;
    auto goal = [](const CubieState& c) {
      return f2l_solved(c) && oll_solved(c);
    };
    if (!match_alg(state, oll_table(), goal, w, name)) {
      throw std::runtime_error("cube_solve: no OLL case matched");
    }
    push_stage(sol, state, "OLL", name, w);
  }

  // ---- PLL --------------------------------------------------------------
  //
  // Three of the last layer's 288 positions match no entry, and correctly so:
  // in them the corners and edges are permuted by the same amount, which means
  // the layer is already right and merely turned. That is not a case, it is
  // the final AUF below, so the failure to recognise it is not a failure.
  {
    std::vector<int> w;
    std::string name;
    auto goal = [](const CubieState& c) { return cube_solved(c); };
    if (match_alg(state, pll_table(), goal, w, name)) {
      push_stage(sol, state, "PLL", name, w);
    } else if (!auf_finishes(state)) {
      throw std::runtime_error("cube_solve: no PLL case matched");
    }
  }

  // ---- AUF --------------------------------------------------------------
  {
    std::vector<int> w = final_auf(state);
    if (!w.empty()) push_stage(sol, state, "AUF", "", w);
  }

  sol.solved = cube_solved(read_state(state));
}

inline Solution solve_cfop(const std::vector<int>& start,
                           const CfopLimits& lim = CfopLimits()) {
  Solution sol;
  sol.solved = false;
  solve_cfop_into(sol, start, lim);
  return sol;
}

}  // namespace cube_solve

#endif  // CAYLEYR_CUBE_SOLVE_CFOP_H
