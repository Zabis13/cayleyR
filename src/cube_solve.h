#ifndef CAYLEYR_CUBE_SOLVE_H
#define CAYLEYR_CUBE_SOLVE_H

#include <vector>
#include <string>
#include "cube_cubie.h"
#include "cube_search.h"
#include "cube_algs.h"

// ---- What a human method is, and what the methods share -----------------
//
// A human method is a chain of subgoals where nothing looks past the subgoal
// it is working on. That is what makes it human rather than optimal: the
// shortest solution to a scrambled cube is around 18 quarter turns and is
// found by searching the whole cube at once, which no person can do. Layer by
// layer takes about 110 and CFOP about 55, and both are reachable by someone
// deciding what to do next from what they can see.
//
// The cost of the chain is visible in those numbers. Each stage is solved
// optimally on its own -- the searches really do return shortest words for
// their subgoal -- and the total is still three to six times optimal, because
// a stage that is shortest in isolation usually leaves the next one worse off.
// That gap is the price of a method a person can hold in their head, and it is
// the same gap whether the stages are found by table or by search.
//
// This file holds what every such method needs: a way to record a solution in
// stages, and a way to recognise a last-layer case. One method per file beside
// it -- cube_solve_cfop.h, cube_solve_lbl.h -- each including this and knowing
// nothing about the others, so a new method is a new file and nothing else.

namespace cube_solve {

using namespace cube_cubie;
using namespace cube_search;
using namespace cube_algs;

// A solution, kept in stages so the shape of the method stays visible.
struct Stage {
  std::string name;         // "cross", "F2L slot 1", "OLL", ...
  std::string detail;       // the case name, where there is one
  std::vector<int> moves;   // move indices in the package alphabet
  std::vector<int> state;   // the cube after this stage
};

struct Solution {
  std::vector<Stage> stages;
  std::vector<int> moves;   // everything concatenated
  bool solved;
  // Why it stopped, empty if it did not. A method that gives up throws, and an
  // exception carries the message but loses the cube: the stages that did land
  // are what says where the trouble is, and the position they end on is the one
  // that has to be looked at. So the throw is caught at the R boundary and the
  // partial solution comes back with the reason attached.
  std::string failure;
};

// Progress reporting. A stage is an exact search and can take a long time, so
// a caller watching a solve needs to hear about each one as it lands rather
// than in a heap at the end. The hook is a function pointer because this
// header knows nothing of R: cube_solve_r.cpp points it at Rcpp::Rcout, a
// standalone program at printf, and by default it goes nowhere.
typedef void (*StageReporter)(const std::string& name,
                              const std::string& detail,
                              int n_moves, int total_moves);

inline StageReporter& stage_reporter() {
  static StageReporter r = 0;
  return r;
}

inline void push_stage(Solution& sol, std::vector<int>& state,
                       const std::string& name, const std::string& detail,
                       const std::vector<int>& word) {
  state = apply_word(state, word);
  Stage s;
  s.name = name;
  s.detail = detail;
  s.moves = word;
  s.state = state;
  sol.stages.push_back(s);
  for (size_t i = 0; i < word.size(); i++) sol.moves.push_back(word[i]);

  if (stage_reporter()) {
    stage_reporter()(name, detail, static_cast<int>(word.size()),
                     static_cast<int>(sol.moves.size()));
  }
}

// ---- Recognising a case by trying it ------------------------------------
//
// Given a table and a goal, find the entry that reaches the goal. A case is
// recognised by being solved: for each entry, and each way the U face can be
// turned around it, apply it and ask whether the stage is now finished.
//
// Both turns matter, and leaving out the second is the easy mistake. A cuber
// adjusts the U face before an algorithm to bring the case into the rotation
// the algorithm expects -- that is AUF -- and adjusts it again afterwards,
// because a permutation of the last layer is only correct up to how the layer
// as a whole is turned. Searching pre-turns alone recognises the cases whose
// algorithm happens to leave U aligned and rejects the rest: on the last
// layer's 288 positions that is 85 recognised instead of 285.
//
// A sixth possibility, that the stage is already done, is checked first.
//
// Returns the word (both AUFs included) and the case name.

template <typename Pred>
inline bool match_alg(const std::vector<int>& state,
                      const std::vector<Alg>& table, Pred goal,
                      std::vector<int>& word, std::string& case_name) {
  const Cube3& C = cube3();
  const int U = C.move_index("U");

  word.clear();
  if (goal(read_state(state))) { case_name = "skip"; return true; }

  for (int auf = 0; auf < 4; auf++) {
    std::vector<int> pre;
    for (int k = 0; k < auf; k++) pre.push_back(U);
    std::vector<int> base = apply_word(state, pre);

    for (size_t i = 0; i < table.size(); i++) {
      std::vector<int> w = alg_word(table[i].moves);
      std::vector<int> after = apply_word(base, w);

      for (int post = 0; post < 4; post++) {
        if (goal(read_state(after))) {
          word = pre;
          for (size_t k = 0; k < w.size(); k++) word.push_back(w[k]);
          for (int k = 0; k < post; k++) word.push_back(U);
          case_name = table[i].name;
          return true;
        }
        after = apply_word(after, std::vector<int>(1, U));
      }
    }
  }
  return false;
}

// ---- Recognising a case in a table written for one slot ------------------
//
// Same idea as match_alg, with a fixed prefix in front of every entry. F2L is
// written for the front-right slot only, so the other three are reached by
// turning the cube first: the prefix is a rotation, and because a rotation
// renames rather than turns, the entry that follows reads correctly in the
// rotated frame without any of it being spelled out again.

template <typename Pred>
inline bool match_alg_prefixed(const std::vector<int>& state,
                               const std::vector<Alg>& table,
                               const std::string& prefix, Pred goal,
                               std::vector<int>& word, std::string& case_name) {
  const Cube3& C = cube3();
  const int U = C.move_index("U");

  word.clear();
  if (goal(read_state(state))) { case_name = "skip"; return true; }

  for (int auf = 0; auf < 4; auf++) {
    std::vector<int> pre;
    for (int k = 0; k < auf; k++) pre.push_back(U);
    std::vector<int> base = apply_word(state, pre);

    for (size_t i = 0; i < table.size(); i++) {
      std::vector<int> w = cube_algs::alg_word((prefix + table[i].moves).c_str());
      std::vector<int> after = apply_word(base, w);

      for (int post = 0; post < 4; post++) {
        if (goal(read_state(after))) {
          word = pre;
          for (size_t k = 0; k < w.size(); k++) word.push_back(w[k]);
          for (int k = 0; k < post; k++) word.push_back(U);
          case_name = table[i].name;
          return true;
        }
        after = apply_word(after, std::vector<int>(1, U));
      }
    }
  }
  return false;
}

// ---- Freeing a pair from the wrong slot ---------------------------------
//
// The F2L table describes a pair that is in the top layer or in its own slot.
// A piece sitting in one of the other three slots is not a case: it has to
// come out first. This finds where the pair actually is and lifts it.
//
// Measured on random positions with the cross solved: the table alone
// recognises 1209 of 1414, and with this 1414 of 1414.
inline bool lift_stuck_pair(std::vector<int>& state, int slot, Solution& sol,
                            const std::string& label) {
  const CubieState c = read_state(state);
  const Slot want = f2l_slot(slot);

  int corner_at = -1, edge_at = -1;
  for (int i = 0; i < 8; i++) if (c.cp[i] == want.corner) corner_at = i;
  for (int i = 0; i < 12; i++) if (c.ep[i] == want.edge) edge_at = i;

  // which slot to empty: wherever the pair is sitting that is not the top
  // layer and not its own slot
  int stuck = -1;
  if (corner_at >= 4 && corner_at <= 7 && corner_at - 4 != slot) {
    stuck = corner_at - 4;
  } else if (edge_at >= 8 && edge_at <= 11 && edge_at - 8 != slot) {
    stuck = edge_at - 8;
  } else if (corner_at >= 4 && corner_at <= 7) {
    stuck = corner_at - 4;
  } else if (edge_at >= 8 && edge_at <= 11) {
    stuck = edge_at - 8;
  }
  if (stuck < 0) return false;

  const std::vector<Alg>& lifts = f2l_lift_table();
  std::vector<int> w = cube_algs::alg_word(lifts[stuck].moves);
  push_stage(sol, state, label, lifts[stuck].name, w);
  return true;
}

// ---- One cross edge, by principle rather than by table -------------------
//
// The cross is the step every guide calls intuitive, and it is the one step
// that is taught as a rule instead of a list. Trying to write it as a list is
// what broke it before: a table of shortest words per position solves each
// edge and wrecks the ones already placed, because "shortest for this edge"
// and "does not disturb its neighbours" are different problems and only the
// first was asked for. The rule below is the second problem stated directly.
//
// The rule, which is what a beginner is told:
//
//   * an edge in the bottom layer that is not home comes up into U first;
//   * an edge in the middle layer is pushed up into U;
//   * an edge in U is turned round to sit above its slot, then dropped in.
//
// Why it keeps what is already there: the bottom layer is only ever entered
// by the face directly above the target edge, and only on the last move of
// the insertion. Everything before that happens in U and in the side faces,
// and a side face turned and turned back leaves the bottom as it was. D is
// never used at all -- and D is exactly what the old table used to slide an
// edge round the bottom, turning the whole cross with it.
//
// The insertion is tried both ways up, and the drop that leaves the edge home
// is the one kept, so a flipped edge costs three moves instead of two rather
// than needing a case of its own.

// Which slot currently holds the cubie belonging to `home`.
inline int find_edge_slot(const CubieState& c, int home) {
  for (int i = 0; i < 12; i++) if (c.ep[i] == home) return i;
  throw std::runtime_error("cube: edge is not anywhere on the cube");
}

// The four cross slots, in the order a y rotation visits them, and for each
// the face that lifts and drops it and the side turns that push a middle edge
// up out of the two slots beside it.
struct CrossSlot {
  int slot;             // the D slot this edge belongs to
  const char* drop;     // face above it: two turns put a U edge in, one takes it out
};

inline const CrossSlot* cross_slots() {
  static const CrossSlot t[4] = {
    {E_DF, "F"},
    {E_DR, "R"},
    {E_DB, "B"},
    {E_DL, "L"}
  };
  return t;
}

// Getting a middle-layer edge up into U. One word per middle slot, each
// measured rather than reasoned about: the slot is emptied upwards and what
// the turn displaced is put back by its own inverse, so the bottom layer ends
// as it started. Slots are FR, FL, BL, BR in the package's numbering.
inline const char* middle_lift(int slot) {
  static const char* t[4] = {
    "R U R'",     // FR
    "F U F'",     // FL
    "L U L'",     // BL
    "B U B'"      // BR
  };
  return t[slot - E_FR];
}

// Place one cross edge, leaving every edge already home still home. `done` is
// how many of the four are already in; the goal checks those as well, so a
// word that would knock one out is simply not accepted.
template <typename Pred>
inline bool solve_cross_edge(std::vector<int>& state, int which, Pred goal,
                             Solution& sol, const std::string& label) {
  const Cube3& C = cube3();
  const CrossSlot& cs = cross_slots()[which];

  if (goal(read_state(state))) return true;

  // 1. Work out what brings the edge up into the top layer -- but do not do it
  //    yet. The lift on its own breaks the cross: R U R' empties a middle slot
  //    by turning a side face through the bottom, and an edge already home on
  //    that face comes out with it. The insertion that follows puts it back,
  //    so the pair is sound and the halves are not. Applying the lift as a
  //    stage of its own would therefore leave the cube, between two stages, in
  //    a position the goal rejects -- which is what an invariant is supposed to
  //    rule out. So the lift is kept as a prefix and judged together with the
  //    drop that completes it.
  std::string lift;
  {
    const CubieState c = read_state(state);
    const int at = find_edge_slot(c, cs.slot);

    if (at >= 4 && at <= 7) {
      // in the bottom layer, wrong slot or wrong way up: two turns of the
      // face above it bring it straight up to U.
      const CrossSlot* all = cross_slots();
      for (int k = 0; k < 4; k++) {
        if (all[k].slot == at) { lift = std::string(all[k].drop) + " " + all[k].drop; break; }
      }
    } else if (at >= 8 && at <= 11) {
      // in the middle layer: empty that slot upwards. Which word does it is a
      // property of the slot alone, not of where the edge is going.
      lift = middle_lift(at);
    }
  }

  // 2. Turn U until the edge is above its slot, then drop it in. Both ways up
  //    are tried; the one that leaves it home is the one that was right.
  // The two drops. An edge the right way up goes in with two turns of its own
  // face. A flipped one cannot -- two turns would put it in still flipped --
  // so it goes in sideways: the face to its right comes down, the slot's own
  // face turns once to catch it, and the right face goes back up, which is
  // three moves and undoes its own damage to the bottom.
  const int U = C.move_index("U");
  const char* right = cross_slots()[(which + 1) % 4].drop;
  const std::string drop2 = std::string(cs.drop) + " " + cs.drop;
  const std::string flip  = std::string(right) + "' " + cs.drop + " " + right;

  const std::vector<int> pre =
      lift.empty() ? std::vector<int>() : cube_algs::alg_word(lift.c_str());

  for (int auf = 0; auf < 4; auf++) {
    for (int variant = 0; variant < 2; variant++) {
      std::vector<int> w = pre;
      for (int k = 0; k < auf; k++) w.push_back(U);
      std::vector<int> ins =
          cube_algs::alg_word((variant == 0 ? drop2 : flip).c_str());
      for (size_t k = 0; k < ins.size(); k++) w.push_back(ins[k]);

      if (goal(read_state(apply_word(state, w)))) {
        push_stage(sol, state, label, variant == 0 ? "insert" : "insert flipped", w);
        return true;
      }
    }
  }
  return false;
}

// ---- One middle edge, by principle rather than by table ------------------
//
// The same illness the cross had, one layer up. The middle-edge table keeps
// the first layer -- all thirteen entries do -- but eleven of the thirteen
// disturb the middle edges already in, and the solver's goal demands those as
// well. So it works for the first edge, where there is nothing yet to disturb,
// and for the last, and fails on the two in between. Which is exactly what the
// failures said: steps 1 and 4 clean, steps 2 and 3 not.
//
// Measuring the first-layer predicate alone is what hid this for so long. The
// check has to be the whole of what is already built -- first layer plus every
// middle edge placed so far -- and not a part of it.
//
// The rule needs two words and no table:
//
//   * an edge waiting at UF, U-face sticker forward, goes in with the first;
//   * an edge waiting at UR, U-face sticker to the side, goes in with the
//     second.
//
// Both were read off the solved cube by running them backwards, which is what
// says where an insertion takes its edge from. Both keep the first layer and
// all three neighbouring middle edges -- measured, not assumed.
//
// An edge stuck in the middle layer needs no separate lift: either insertion
// applied to the slot it sits in throws it up into U, keeping everything else,
// and then it is an ordinary case. That is the same trick lift_stuck_pair()
// plays for F2L, except that here the lifting word and the inserting word are
// the same two.
inline const char* middle_edge_insert(int variant) {
  static const char* t[2] = {
    "U R U' R' U' F' U F",   // from UF
    "U' F' U F U R U' R'"    // from UR
  };
  return t[variant];
}

// Place one middle edge, keeping the first layer and every middle edge already
// in. `goal` carries both, so a word that would disturb either is not taken.
//
// As with the cross, a lift is never a stage of its own: it breaks the goal on
// its way and the insertion is what repairs it, so the two are judged as one
// word and the cube is never left between stages in a position the goal would
// reject.
template <typename Pred>
inline bool solve_middle_edge(std::vector<int>& state, int which, int home,
                              Pred goal, Solution& sol,
                              const std::string& label) {
  const Cube3& C = cube3();
  const int U = C.move_index("U");

  if (goal(read_state(state))) return true;

  std::string prefix;
  for (int k = 0; k < which; k++) prefix += "y ";

  // If the edge is in the middle layer, the word that frees it is an insertion
  // aimed at the slot holding it -- which is that slot's own rotation of the
  // pair above.
  std::vector<int> lift;
  {
    const CubieState c = read_state(state);
    const int at = find_edge_slot(c, home);
    if (at >= E_FR && at <= E_BR) {
      // slots run FR, FL, BL, BR; the rotations that name them are y^0, y^3,
      // y^2, y^1 -- the same order a y visits them, read backwards.
      static const int rot_of[4] = {0, 3, 2, 1};
      std::string p;
      for (int k = 0; k < rot_of[at - E_FR]; k++) p += "y ";
      lift = cube_algs::alg_word((p + middle_edge_insert(0)).c_str());
    }
  }

  for (int auf = 0; auf < 4; auf++) {
    for (int variant = 0; variant < 2; variant++) {
      std::vector<int> w = lift;
      for (int k = 0; k < auf; k++) w.push_back(U);
      std::vector<int> ins =
          cube_algs::alg_word((prefix + middle_edge_insert(variant)).c_str());
      for (size_t k = 0; k < ins.size(); k++) w.push_back(ins[k]);

      if (goal(read_state(apply_word(state, w)))) {
        push_stage(sol, state, label,
                   variant == 0 ? "insert from UF" : "insert from UR", w);
        return true;
      }
    }
  }
  return false;
}

// ---- Putting the centres back -------------------------------------------
//
// The package's alphabet has all 18 moves of the cube, slices included, and
// find_path_iterative() walks that whole graph. A human method cannot: M, E
// and S turn the centres relative to the faces, and every stage below is
// stated against the centres. "The yellow cross" means the four U edges
// oriented towards the U centre, so once that centre has moved there is no
// such thing to find, and the solver spins until a guard stops it.
//
// The fix is not to forbid slices but to undo their effect on the centres.
// Whatever a word of slices has done to them amounts to turning the whole cube
// in space, and turning it back costs nothing: it renames the faces without
// moving a single piece relative to another. So find the rotation that puts
// the centres home, apply it, and solve from there.
//
// The 24 rotations are generated as words in x and y -- four turns about the
// vertical axis for each of six ways to choose which face points up.

// Here a rotation has to be performed, not read: the cube in front of us
// really is turned the wrong way and really must be turned back. So these are
// spelled in slices, which are moves. Writing them as x and y would produce
// nothing at all -- alg_word() treats a rotation as a renaming of what follows
// it, and there is nothing following.
inline const std::vector<std::string>& rotation_words() {
  static const std::string X  = "R M' L'";
  static const std::string XI = "R' M L";
  static const std::string X2 = X + " " + X;
  static const std::string Y  = "U E' D'";
  static const std::string YI = "U' E D";
  static const std::string Y2 = Y + " " + Y;
  static const std::string Z  = "F S B'";
  static const std::string ZI = "F' S' B";

  static std::vector<std::string> v;
  if (v.empty()) {
    const std::string face[6] = { "", X, XI, X2, Z, ZI };
    const std::string turn[4] = { "", Y, Y2, YI };
    for (int f = 0; f < 6; f++) {
      for (int t = 0; t < 4; t++) {
        std::string w = face[f];
        if (!turn[t].empty()) w += (w.empty() ? "" : " ") + turn[t];
        v.push_back(w);
      }
    }
  }
  return v;
}

// Are the six centres in their home positions? The centre of face f is sticker
// f*9+4, and on a solved cube it holds the value f*9+5 (1-based).
inline bool centres_home(const std::vector<int>& state) {
  for (int f = 0; f < 6; f++) {
    if (state[f * 9 + 4] != f * 9 + 5) return false;
  }
  return true;
}

// The rotation that brings the centres home, as a word; empty if they already
// are. Throws if no rotation does, which would mean the state is not one the
// cube's own moves can reach.
inline std::vector<int> orient_to_centres(const std::vector<int>& state) {
  if (centres_home(state)) return std::vector<int>();

  const std::vector<std::string>& rots = rotation_words();
  for (size_t i = 1; i < rots.size(); i++) {
    std::vector<int> w = cube_algs::alg_word(rots[i].c_str());
    if (centres_home(apply_word(state, w))) return w;
  }
  throw std::runtime_error(
    "cube_solve: the centres of this state are in no orientation a cube "
    "can be in");
}

// The last layer may be finished and still need turning: PLL leaves the U face
// correct relative to itself but possibly rotated. This is the final AUF.
inline std::vector<int> final_auf(const std::vector<int>& state) {
  const Cube3& C = cube3();
  const int U = C.move_index("U");
  std::vector<int> w;
  for (int auf = 0; auf < 4; auf++) {
    if (cube_solved(read_state(apply_word(state, w)))) return w;
    w.push_back(U);
  }
  return std::vector<int>();
}

// Whether some number of U turns finishes the cube. An empty word from
// final_auf() means either "already solved" or "no amount of turning helps",
// and a caller deciding whether to raise needs to tell those apart.
inline bool auf_finishes(const std::vector<int>& state) {
  const Cube3& C = cube3();
  const int U = C.move_index("U");
  std::vector<int> s = state;
  for (int auf = 0; auf < 4; auf++) {
    if (cube_solved(read_state(s))) return true;
    s = apply_word(s, std::vector<int>(1, U));
  }
  return false;
}

}  // namespace cube_solve

#endif  // CAYLEYR_CUBE_SOLVE_H
