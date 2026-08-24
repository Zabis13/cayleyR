#include <Rcpp.h>
#include <vector>
#include <string>
#include "cube_centres.h"
#include "cube_edges.h"
#include "cube_reduce.h"

using namespace Rcpp;
using namespace cube_solve;

// ---- Centres of a 4x4x4, R-facing ---------------------------------------
//
// The 3x3x3 solvers next door take a 54-sticker state and share none of this
// marshalling, so this lives in its own file rather than widening theirs. What
// comes back is the Solution shape they all return.

namespace {

std::vector<int> as_state_4(IntegerVector state) {
  if (state.size() != 96) {
    stop("cube_centres: a 4x4x4 state has 96 entries, got %d",
         static_cast<int>(state.size()));
  }
  std::vector<bool> seen(97, false);
  for (int i = 0; i < 96; i++) {
    const int v = state[i];
    if (v < 1 || v > 96) {
      stop("cube_centres: state entries must be 1..96, got %d. Colours 0..5 "
           "are the other way of writing a cube down; cube_colour_state() "
           "converts one to the positions this wants.", v);
    }
    if (seen[v]) {
      stop("cube_centres: state must be a permutation of 1..96, but %d "
           "appears more than once.", v);
    }
    seen[v] = true;
  }
  return as< std::vector<int> >(state);
}

List solution_to_r_4(const Solution& sol) {
  const size_t n = sol.stages.size();
  CharacterVector name(n), detail(n);
  IntegerVector n_moves(n);
  List stage_moves(n), states(n);

  for (size_t i = 0; i < n; i++) {
    const Stage& s = sol.stages[i];
    name[i] = s.name;
    detail[i] = s.detail;
    n_moves[i] = static_cast<int>(s.moves.size());
    stage_moves[i] = wrap(cube_search::word_names(s.moves, 4));
    states[i] = wrap(s.state);
  }

  DataFrame stages = DataFrame::create(
    _["name"] = name, _["detail"] = detail, _["n_moves"] = n_moves,
    _["stringsAsFactors"] = false);
  stages.attr("moves") = stage_moves;

  return List::create(
    _["path"] = wrap(cube_search::word_names(sol.moves, 4)),
    _["found"] = sol.solved,
    _["stages"] = stages,
    _["states"] = states,
    _["failure"] = sol.failure);
}

}  // namespace

// [[Rcpp::export]]
List cube_centres_shoot_cpp(IntegerVector state) {
  const std::vector<int> s = as_state_4(state);
  Solution sol;
  sol.solved = false;
  std::vector<int> cur = s;
  try {
    // Step 3 on its own, for probing it in isolation. The caller is handing in
    // a cube already turned the way step 3 wants, so the map starts as the
    // identity here -- unlike in the full solve, where z' has just been made.
    Orient o;
    Orient dummy;
    sol.solved = empty_u_slice(cur, sol, o, &dummy, "u-slice");
  } catch (const std::exception& e) {
    sol.solved = false;
    sol.failure = e.what();
  }
  return solution_to_r_4(sol);
}

// [[Rcpp::export]]
IntegerVector cube_centre_counts_cpp(IntegerVector state) {
  const std::vector<int> s = as_state_4(state);
  IntegerVector out(6);
  Orient o;
  for (int f = 0; f < 6; f++) out[f] = centre_count(s, o, f);
  return out;
}

// [[Rcpp::export]]
int cube_choose_shot_cpp(IntegerVector state) {
  const std::vector<int> s = as_state_4(state);
  Orient o;
  return choose_shot(s, o, false);
}

// [[Rcpp::export]]
DataFrame cube_find_colour_cpp(IntegerVector state, int colour) {
  const std::vector<int> s = as_state_4(state);
  const std::vector<Spot> v = find_colour(s, colour);
  IntegerVector face(v.size()), slot(v.size());
  for (size_t i = 0; i < v.size(); i++) { face[i] = v[i].face; slot[i] = v[i].slot; }
  return DataFrame::create(_["face"] = face, _["slot"] = slot);
}

// [[Rcpp::export]]
LogicalVector cube_has_pair_cpp(IntegerVector state, int colour) {
  const std::vector<int> s = as_state_4(state);
  LogicalVector out(6);
  for (int f = 0; f < 6; f++) out[f] = has_pair(s, f, colour);
  return out;
}


// [[Rcpp::export]]
DataFrame cube_slice_map_cpp() {
  const SliceMap* m = slice_maps();
  std::vector<std::string> mv;
  std::vector<int> ff, fs, tf, ts;
  for (int i = 0; i < n_slices(); i++)
    for (int k = 0; k < 8; k++) {
      mv.push_back(m[i].move);
      ff.push_back(m[i].step[k].from_face);
      fs.push_back(m[i].step[k].from_slot);
      tf.push_back(m[i].step[k].to_face);
      ts.push_back(m[i].step[k].to_slot);
    }
  return DataFrame::create(_["move"] = mv, _["from_face"] = ff,
                           _["from_slot"] = fs, _["to_face"] = tf,
                           _["to_slot"] = ts, _["stringsAsFactors"] = false);
}

// ---- one pipeline, four entry points ---------------------------------------
//
// Four exports used to build the same centres by copy: each wrote out
// as_state_4, a Solution, a try block and the same calls, and three of them
// wrote the target face as the literal 4. The steps are shared here instead, so
// that the face is a parameter of the method rather than of one caller.
//
// Why the face was a literal: build_l_slice wants the first centre already on
// L, so building it anywhere else costs a rotation. That is a good reason to
// DEFAULT to L and no reason to be unable to ask for another -- and which face
// is cheapest depends on the cube, which is the thing being measured.

// The rotation that sends a face to the bottom, where step 3's shots cannot
// reach it. Read off the same table rotate_orient uses: the entry whose to[]
// sends this role to 3. U is the one face no single quarter turn brings down.
inline std::vector<std::string> rotation_to_bottom(int face) {
  std::vector<std::string> out;
  switch (face) {
    case 0: out.push_back("x");  out.push_back("x");  break;  // U -> B -> D
    case 1: out.push_back("z");  break;                       // R -> D
    case 2: out.push_back("x'"); break;                       // F -> D
    case 3: break;                                            // D: already there
    case 4: out.push_back("z'"); break;                       // L -> D
    case 5: out.push_back("x");  break;                       // B -> D
    default: throw std::runtime_error("cube_centres: no such face");
  }
  return out;
}

// Step 1 and step 2: a centre on `target_face`, then the layer beside it.
inline bool build_centre_and_slice(std::vector<int>& cur, Solution& sol,
                                   Orient& o, int target_face) {
  if (!build_first_centre(cur, sol, o, target_face, "first centre")) {
    sol.failure = "could not build the first centre";
    return false;
  }
  if (!build_l_slice(cur, sol, o, "l-slice", target_face)) {
    sol.failure = "could not finish the l-slice";
    return false;
  }
  return true;
}

// Steps 3 and 4, with the rotation that puts the built layer out of reach
// first. Returns the orientation step 5 must go on with through `after`.
inline bool finish_centres(std::vector<int>& cur, Solution& sol, Orient& o,
                           int target_face, Orient* after) {
  *after = o;
  // A cube handed in with its centres built needs no moves, and the rotation
  // below would put four in the path for nothing.
  if (centres_built(cur, o)) return true;

  // Pochmann turns the cube so the built face goes to the bottom, where the
  // shots of step 3 cannot touch it.
  const std::vector<std::string> rots = rotation_to_bottom(target_face);
  for (size_t i = 0; i < rots.size(); i++) {
    // rotation_moves spells the rotation in package moves -- the same words the
    // rest of the file uses, rather than a second copy of the table here.
    push_stage(sol, cur, "rotate", rots[i].c_str(),
               parse_word(rotation_moves(rots[i]), 4));
    o = rotate_orient(o, rots[i].c_str());
  }
  *after = o;

  // empty_u_slice reports whether it finished, but it also turns the cube as it
  // works, so the orientation it left off in is what the next step must use.
  bool done = empty_u_slice(cur, sol, o, after, "u-slice");
  // Step 3 shoots pieces down from U and is helpless once U holds nothing worth
  // shooting -- measured, that is how it fails: a finished U, four faces done,
  // and the last pieces swapped between two other faces. Swapping them directly
  // is a different tool, so it is a different stage.
  if (!done) done = settle_pairs(cur, sol, *after, "pairs");
  if (!done) sol.failure = "could not finish the centres";
  return done;
}

// The twelve cells of the layer beside a face, for checking the derived table
// against the hand-measured ones it replaces.
// [[Rcpp::export]]
DataFrame cube_slice_cells_cpp(int face) {
  const std::vector<SliceCell>& v = slice_cells_of(face);
  IntegerVector f(v.size()), s(v.size());
  for (size_t i = 0; i < v.size(); i++) { f[i] = v[i].face; s[i] = v[i].slot; }
  return DataFrame::create(_["face"] = f, _["slot"] = s,
                           _["stringsAsFactors"] = false);
}

// [[Rcpp::export]]
List cube_first_centre_cpp(IntegerVector state, int target_face) {
  const std::vector<int> s = as_state_4(state);
  Solution sol;
  sol.solved = false;
  std::vector<int> cur = s;
  try {
    Orient o;
    sol.solved = build_first_centre(cur, sol, o, target_face, "first centre");
  } catch (const std::exception& e) {
    sol.solved = false;
    sol.failure = e.what();
  }
  return solution_to_r_4(sol);
}

// [[Rcpp::export]]
int cube_l_slice_count_cpp(IntegerVector state) {
  const std::vector<int> s = as_state_4(state);
  Orient o;
  return l_slice_count(s, o);
}

// [[Rcpp::export]]
List cube_centres_12_cpp(IntegerVector state, int target_face = 4) {
  const std::vector<int> s = as_state_4(state);
  Solution sol;
  sol.solved = false;
  std::vector<int> cur = s;
  try {
    Orient o;
    sol.solved = build_centre_and_slice(cur, sol, o, target_face);
  } catch (const std::exception& e) {
    sol.solved = false;
    sol.failure = e.what();
  }
  return solution_to_r_4(sol);
}

// [[Rcpp::export]]
List cube_centres_cpp(IntegerVector state, int target_face = 4) {
  const std::vector<int> s = as_state_4(state);
  Solution sol;
  sol.solved = false;
  std::vector<int> cur = s;
  try {
    Orient o;
    if (!build_centre_and_slice(cur, sol, o, target_face))
      return solution_to_r_4(sol);
    Orient after = o;
    sol.solved = finish_centres(cur, sol, o, target_face, &after);
  } catch (const std::exception& e) {
    sol.solved = false;
    sol.failure = e.what();
  }
  return solution_to_r_4(sol);
}

// [[Rcpp::export]]
List cube_reduce_cpp(IntegerVector state, int target_face = 4) {
  const std::vector<int> s = as_state_4(state);
  Solution sol;
  sol.solved = false;
  std::vector<int> cur = s;
  try {
    Orient o;
    if (!build_centre_and_slice(cur, sol, o, target_face))
      return solution_to_r_4(sol);

    Orient after = o;
    if (!finish_centres(cur, sol, o, target_face, &after))
      return solution_to_r_4(sol);

    // Stage two: the edges. The centres are built and every algorithm below
    // was measured to leave them that way, so the guard is a check, not a
    // hope.
    sol.solved = pair_edges(cur, sol, after, "edges");
    if (!sol.solved) sol.failure = "could not pair the edges";
  } catch (const std::exception& e) {
    sol.solved = false;
    sol.failure = e.what();
  }
  return solution_to_r_4(sol);
}

// [[Rcpp::export]]
List cube_parity_fix_cpp(IntegerVector state, std::string which) {
  const std::vector<int> s = as_state_4(state);
  const char* w = (which == "OLL") ? oll_parity_word() : pll_parity_word();
  const std::vector<int> out = apply_word(s, parse_word(w, 4));
  return List::create(_["state"] = wrap(out),
                      _["path"] = wrap(cube_search::word_names(parse_word(w, 4), 4)));
}

// [[Rcpp::export]]
IntegerVector cube_squeeze_cpp(IntegerVector state) {
  const std::vector<int> s = as_state_4(state);
  return wrap(squeeze_to_3(s));
}

// [[Rcpp::export]]
List cube_lift_path_cpp(CharacterVector path) {
  std::vector<std::string> p;
  for (int i = 0; i < path.size(); i++) p.push_back(as<std::string>(path[i]));
  const std::string w = lift_word(p);
  return List::create(_["path"] = wrap(cube_search::word_names(parse_word(w, 4), 4)));
}
