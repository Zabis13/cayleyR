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
List cube_centres_12_cpp(IntegerVector state) {
  const std::vector<int> s = as_state_4(state);
  Solution sol;
  sol.solved = false;
  std::vector<int> cur = s;
  try {
    // Step 1 builds a centre; build it straight onto L, which is where step 2
    // wants it, rather than building elsewhere and turning the cube.
    Orient o;
    if (build_first_centre(cur, sol, o, 4, "first centre")) {
      sol.solved = build_l_slice(cur, sol, o, "l-slice");
    }
  } catch (const std::exception& e) {
    sol.solved = false;
    sol.failure = e.what();
  }
  return solution_to_r_4(sol);
}

// [[Rcpp::export]]
List cube_centres_cpp(IntegerVector state) {
  const std::vector<int> s = as_state_4(state);
  Solution sol;
  sol.solved = false;
  std::vector<int> cur = s;
  try {
    Orient o;
    if (!build_first_centre(cur, sol, o, 4, "first centre")) {
      sol.failure = "could not build the first centre";
      return solution_to_r_4(sol);
    }
    if (!build_l_slice(cur, sol, o, "l-slice")) {
      sol.failure = "could not finish the l-slice";
      return solution_to_r_4(sol);
    }
    // Already done -- a cube handed in with its centres built needs no moves,
    // and the rotation below would put four in the path for nothing.
    if (centres_built(cur, o)) {
      sol.solved = true;
      return solution_to_r_4(sol);
    }

    // Pochmann turns the cube so the built face goes to the bottom, where the
    // shots of step 3 cannot touch it. Measured: z' is the rotation that sends
    // L to D.
    push_stage(sol, cur, "rotate", "z'",
               parse_word("B 1z' 2z' F'", 4));
    o = rotate_orient(o, "z'");
    // empty_u_slice reports whether it finished, but it also turns the cube as
    // it works, so the orientation it left off in is what step 4 must use. It
    // is recomputed rather than returned: the stage only ever turns with y and
    // the tilts, and those are recorded in the stage list.
    Orient after = o;
    sol.solved = empty_u_slice(cur, sol, o, &after, "u-slice");

    // Step 4. Step 3 shoots pieces down from U and is helpless once U holds
    // nothing worth shooting -- measured, that is how it fails: a finished U,
    // four faces done, and the last pieces swapped between two other faces.
    // Swapping them directly is a different tool, so it is a different stage.
    if (!sol.solved) sol.solved = settle_pairs(cur, sol, after, "pairs");
    if (!sol.solved) sol.failure = "could not finish the centres";
  } catch (const std::exception& e) {
    sol.solved = false;
    sol.failure = e.what();
  }
  return solution_to_r_4(sol);
}

// [[Rcpp::export]]
List cube_reduce_cpp(IntegerVector state) {
  const std::vector<int> s = as_state_4(state);
  Solution sol;
  sol.solved = false;
  std::vector<int> cur = s;
  try {
    Orient o;
    if (!build_first_centre(cur, sol, o, 4, "first centre")) {
      sol.failure = "could not build the first centre";
      return solution_to_r_4(sol);
    }
    if (!build_l_slice(cur, sol, o, "l-slice")) {
      sol.failure = "could not finish the l-slice";
      return solution_to_r_4(sol);
    }
    Orient after = o;
    if (!centres_built(cur, o)) {
      push_stage(sol, cur, "rotate", "z'", parse_word("B 1z' 2z' F'", 4));
      o = rotate_orient(o, "z'");
      after = o;
      bool done = empty_u_slice(cur, sol, o, &after, "u-slice");
      if (!done) done = settle_pairs(cur, sol, after, "pairs");
      if (!done) {
        sol.failure = "could not finish the centres";
        return solution_to_r_4(sol);
      }
    }

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
