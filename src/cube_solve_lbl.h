#ifndef CAYLEYR_CUBE_SOLVE_LBL_H
#define CAYLEYR_CUBE_SOLVE_LBL_H

#include <vector>
#include <string>
#include <stdexcept>
#include "cube_solve.h"

// ---- Layer by layer -----------------------------------------------------
//
// Seven steps, and the last four are the ones a beginner is taught as
// standalone tricks: make the yellow cross, put the yellow edges right, put
// the corners in their places, then twist them. Each is a small algorithm
// applied over and over with a U turn between, which is why the method is easy
// to learn and long to execute.
//
// It shares the cross on D and the last layer on U with CFOP. The middle is
// where it is slower and simpler: the bottom layer is finished outright, and
// only then do the middle edges go in, one at a time.

namespace cube_solve {

struct LblLimits {
  int cross_depth;
  int corner_depth;
  int edge_depth;
  LblLimits() : cross_depth(8), corner_depth(9), edge_depth(11) {}
};

// The body, writing into a Solution the caller owns. Split out so that a
// method which throws still leaves its partial work behind: the stages that
// landed and the position they ended on are what a diagnosis is made of.
inline void solve_lbl_into(Solution& sol, const std::vector<int>& start,
                           const LblLimits& lim) {
  sol.solved = false;
  std::vector<int> state = start;
  // Face turns only. The slices M, E and S turn the centres that every step
  // below is stated against, and they change the parity of the edge
  // permutation without touching the corners -- which hands the last layer a
  // position no real cube can be in, four steps before anything complains.
  const std::vector<int> all = moves_faces();

  // ---- 0. Orientation ---------------------------------------------------
  // Slice moves may have turned the centres, and every step below is stated
  // against them. Turn the whole cube back first -- see orient_to_centres().
  {
    std::vector<int> w = orient_to_centres(state);
    if (!w.empty()) push_stage(sol, state, "orientation", "centres", w);
  }

  // ---- 1. Cross on D, one edge at a time --------------------------------
  //
  // Worked out rather than looked up. This is the one step of the method that
  // is taught as a rule instead of a list, and writing it as a list is what
  // broke it: a table of shortest words per position places each edge and
  // knocks out the ones already in, because the search that produced it was
  // never asked to keep them. See solve_cross_edge(), which is the rule --
  // bring the edge to the top, turn it over its slot, drop it in -- and which
  // keeps the bottom layer by never entering it except on the last move.
  //
  // The goal for edge i demands the first i are still home, so a word that
  // would disturb them is rejected where it happens rather than four stages
  // later. The order is the one a y rotation visits: DF, DR, DB, DL.
  {
    for (int i = 0; i < 4; i++) {
      auto goal = [i](const CubieState& c) {
        static const int ord[4] = {E_DF, E_DR, E_DB, E_DL};
        for (int k = 0; k <= i; k++) if (!edge_home(c, ord[k])) return false;
        return true;
      };
      if (goal(read_state(state))) continue;

      if (!solve_cross_edge(state, i, goal, sol,
                            "cross edge " + std::to_string(i + 1))) {
        throw std::runtime_error("cube_solve: cross edge " +
                                 std::to_string(i + 1) + " not placed");
      }
    }
  }

  // ---- 2. The four bottom corners, one at a time ------------------------
  // LBL finishes the first layer before touching the middle, so each corner is
  // its own subgoal and the middle edges are left wherever they happen to be.
  // Same as the cross above: the order is the one y visits, DFR then DRB then
  // DBL then DLF, and not the order the slots are numbered in.
  {
    for (int i = 0; i < 4; i++) {
      auto goal = [i](const CubieState& c) {
        if (!cross_solved(c)) return false;
        static const int ord[4] = {C_DFR, C_DRB, C_DBL, C_DLF};
        for (int k = 0; k <= i; k++) if (!corner_home(c, ord[k])) return false;
        return true;
      };
      if (goal(read_state(state))) continue;

      std::string face;
      for (int k = 0; k < i; k++) face += "y ";

      std::vector<int> w;
      std::string name;
      if (!match_alg_prefixed(state, lbl_corner_table(), face, goal, w, name)) {
        throw std::runtime_error("cube_solve: first-layer corner " +
                                 std::to_string(i + 1) + " not recognised");
      }
      push_stage(sol, state, "first layer corner " + std::to_string(i + 1),
                 name, w);
    }
  }

  // ---- 3. The four middle edges -----------------------------------------
  // With the first layer whole, each middle edge goes in without breaking it.
  // And again the order y visits: FR, BR, BL, FL.
  {
    for (int i = 0; i < 4; i++) {
      auto goal = [i](const CubieState& c) {
        if (!first_layer_solved(c)) return false;
        static const int ord[4] = {E_FR, E_BR, E_BL, E_FL};
        for (int k = 0; k <= i; k++) if (!edge_home(c, ord[k])) return false;
        return true;
      };
      if (goal(read_state(state))) continue;

      static const int ord[4] = {E_FR, E_BR, E_BL, E_FL};
      if (!solve_middle_edge(state, i, ord[i], goal, sol,
                             "middle edge " + std::to_string(i + 1))) {
        throw std::runtime_error("cube_solve: middle edge " +
                                 std::to_string(i + 1) + " not placed");
      }
    }
  }

  // ---- 4. Orient the last layer's edges: the yellow cross ---------------
  // One algorithm, F R U R' U' F', applied up to three times with a U turn
  // between. The state goes dot -> L or line -> cross, and each application is
  // the same sequence; what changes is the pre-turn.
  //
  // The number of oriented edges is not a measure of progress, which is worth
  // knowing before trying to improve this loop. On the solved cube the
  // algorithm has order three in orientation -- 4 oriented edges to 2, then 2
  // to 2, then back to 4 -- so a version that demanded one more oriented edge
  // per application would reject exactly the application the dot case needs.
  //
  // Like every step here it is stated against the centres -- orientation is
  // measured towards the U centre -- which is why step 0 turns them home
  // before any of this runs.
  {
    // The case is read off the orientations directly rather than found by
    // trying the algorithm at each AUF and seeing what sticks. Three cases,
    // and each names the turn that puts it where F R U R' U' F' expects it:
    //
    //   dot   all four flipped   any AUF          -> L
    //   L     two adjacent       bring them to    -> line
    //                            UR and UF
    //   line  two opposite       lay them F-B     -> cross
    //
    // Slots 0..3 are UR, UF, UL, UB, which is one U turn apart each, so the
    // AUF is arithmetic on the bit mask of flipped edges: the L cases are the
    // four rotations of 0011 and the AUF is which rotation it is.
    //
    // Counting how many edges are oriented and taking the best, which is what
    // stood here, does not work. From a line all four AUFs leave exactly two
    // oriented, so the maximum is a four-way tie; the first candidate wins it,
    // the first candidate is AUF 0, and on a line that is the application that
    // puts the cube back where it started. The loop then repeated it until the
    // guard fired. How many edges are oriented is not the case -- which ones
    // are is.
    const CubeN& C = cube3();
    const std::vector<int> alg = alg_word(lbl_cross_table()[0].moves);

    int guard = 0;
    while (!ll_cross_oriented(read_state(state))) {
      if (++guard > 3) throw std::runtime_error("cube_solve: LL cross did not close");

      CubieState c = read_state(state);
      int mask = 0;
      for (int e = 0; e < 4; e++) if (c.eo[e] != 0) mask |= (1 << e);

      int auf = 0;
      const char* which = "dot";
      if (mask == 5 || mask == 10) {
        // line: flipped at UR+UL (0101 = 5) or UF+UB (1010 = 10). The
        // algorithm wants them front to back, so the first needs one turn.
        which = "line";
        auf = (mask == 5) ? 1 : 0;
      } else if (mask != 15) {
        // L: the four rotations of 0011, and the AUF is the rotation.
        which = "L";
        for (int k = 0; k < 4; k++) {
          int rot = ((3 << k) | (3 >> (4 - k))) & 15;
          if (rot == mask) { auf = k; break; }
        }
      }

      std::vector<int> w;
      for (int k = 0; k < auf; k++) w.push_back(C.move_index("U"));
      for (size_t k = 0; k < alg.size(); k++) w.push_back(alg[k]);
      push_stage(sol, state, "LL cross", which, w);
    }
  }

  // ---- 5. Place the last layer's edges ----------------------------------
  {
    auto goal = [](const CubieState& c) { return ll_edges_placed(c); };
    int guard = 0;
    while (!goal(read_state(state))) {
      if (++guard > 4) throw std::runtime_error("cube_solve: LL edges did not place");
      std::vector<int> w;
      std::string name;
      if (!match_alg(state, lbl_edge_perm_table(), goal, w, name)) {
        // a single three-cycle does not finish it: do one and try again
        std::vector<int> a = alg_word(lbl_edge_perm_table()[0].moves);
        push_stage(sol, state, "LL edge permutation",
                   lbl_edge_perm_table()[0].name, a);
      } else {
        push_stage(sol, state, "LL edge permutation", name, w);
      }
    }
  }

  // ---- 6. Place the last layer's corners --------------------------------
  {
    auto goal = [](const CubieState& c) {
      return ll_edges_placed(c) && ll_corners_placed(c);
    };
    int guard = 0;
    while (!goal(read_state(state))) {
      if (++guard > 4) throw std::runtime_error("cube_solve: LL corners did not place");
      std::vector<int> w;
      std::string name;
      if (!match_alg(state, lbl_corner_perm_table(), goal, w, name)) {
        std::vector<int> a = alg_word(lbl_corner_perm_table()[0].moves);
        push_stage(sol, state, "LL corner permutation",
                   lbl_corner_perm_table()[0].name, a);
      } else {
        push_stage(sol, state, "LL corner permutation", name, w);
      }
    }
  }

  // ---- 7. Twist the last layer's corners --------------------------------
  // The famous one. R' D' R D twists the corner at URF and wrecks everything
  // below; repeated two or four times it returns the cube to where it was with
  // that corner turned, and a U between corners brings the next one round. The
  // cube looks destroyed until the last U, and then it is solved.
  {
    const CubeN& C = cube3();
    const int U = C.move_index("U");
    const std::vector<int> twist = alg_word(lbl_corner_twist_table()[0].moves);

    // Four steps, one per corner, and the count is fixed rather than tested
    // for. That is the whole trick of this step and it is worth stating plainly
    // because the obvious loop -- turn until the cube comes out solved -- does
    // not work.
    //
    // R' D' R D turns URF by a third and takes the bottom two layers apart on
    // the way. They come back only on the sixth application, so the cube is in
    // pieces at every point in between and asking "is it solved yet" tells you
    // nothing about whether the step is going correctly. What makes it close is
    // arithmetic: each corner gets the applications that zero it (2 for a
    // corner at 1, 1 for a corner at 0), the twists over the last layer sum to
    // a multiple of three because the cube group says they do, and so the
    // applications sum to a multiple of six and the bottom comes back by
    // itself. Then four U turns have been spent walking the corners round, the
    // last layer is back where it started, and the cube is solved.
    //
    // Looping on cube_solved() instead spends a U on every corner that is
    // already upright, breaking the count: with two corners to fix it walks
    // past them, finds the cube unsolved, and keeps turning U forever with the
    // bottom two layers still scattered. Measured: a guard of 40 reached with
    // thirty-odd consecutive U turns on the end.
    for (int i = 0; i < 4; i++) {
      CubieState c = read_state(state);
      if (c.co[C_URF] != 0) {
        // The unit of work is R' D' R D twice, not once, and the reason is that
        // a single application does not leave the corner it is turning at URF.
        // D moves the bottom layer, so the piece sitting at URF alternates: the
        // corner being worked on, then the one that replaced it, then back. Read
        // co[URF] after an odd application and it belongs to a different cubie
        // and means nothing.
        //
        // Sampled on the corner itself -- the applications where cp[URF] is the
        // corner again -- the orientation runs 2, 1, 0: a pair of applications
        // takes off one third. So the count is co pairs, which is the four and
        // two that the beginner's method is written with.
        std::vector<int> w;
        int reps = 2 * c.co[C_URF];
        for (int r = 0; r < reps; r++) {
          for (size_t k = 0; k < twist.size(); k++) w.push_back(twist[k]);
        }
        push_stage(sol, state, "LL corner twist", corner_names()[C_URF], w);
      }
      // Bring the next corner round -- always, whether this one was turned or
      // not, because the four U turns are what put the last layer back.
      std::vector<int> u(1, U);
      push_stage(sol, state, "LL corner twist", "U", u);
    }

    // The four U turns above return the last layer to its own orientation, but
    // the layer as a whole may still be turned against the bottom: the corner
    // permutation step left it correct up to an AUF.
    if (!cube_solved(read_state(state))) {
      int auf = 0;
      for (; auf < 4; auf++) {
        if (cube_solved(read_state(state))) break;
        std::vector<int> u(1, U);
        push_stage(sol, state, "LL corner twist", "AUF", u);
      }
      if (auf == 4) throw std::runtime_error("cube_solve: LL twists did not close");
    }
  }

  sol.solved = cube_solved(read_state(state));
}

inline Solution solve_lbl(const std::vector<int>& start,
                          const LblLimits& lim = LblLimits()) {
  Solution sol;
  sol.solved = false;
  solve_lbl_into(sol, start, lim);
  return sol;
}

}  // namespace cube_solve

#endif  // CAYLEYR_CUBE_SOLVE_LBL_H
