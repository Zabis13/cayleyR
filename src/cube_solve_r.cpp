#include <Rcpp.h>
#include <vector>
#include <string>
#include "cube_solve.h"
#include "cube_solve_cfop.h"
#include "cube_solve_lbl.h"

using namespace Rcpp;
using namespace cube_solve;

// ---- The two human methods, R-facing ------------------------------------
//
// The methods live in cube_solve_cfop.h and cube_solve_lbl.h and know nothing
// of R; everything here is argument marshalling. Both return the same Solution,
// so both convert through one function.
//
// Moves come back as names ("R", "U'") rather than indices, because that is
// what a person reads and what parse_word() takes back. States come back
// 1-based, as every state in the package is.

namespace {

// A Solution as R sees it: the whole word, whether it finished, the stages as
// a data.frame, and the cube after each stage.
List solution_to_r(const Solution& sol) {
  const size_t n = sol.stages.size();

  CharacterVector name(n), detail(n);
  IntegerVector n_moves(n);
  List stage_moves(n), states(n);

  for (size_t i = 0; i < n; i++) {
    const Stage& s = sol.stages[i];
    name[i] = s.name;
    detail[i] = s.detail;
    n_moves[i] = static_cast<int>(s.moves.size());

    std::vector<std::string> mv = word_names(s.moves);
    stage_moves[i] = wrap(mv);
    states[i] = wrap(s.state);
  }

  DataFrame stages = DataFrame::create(
    _["name"] = name,
    _["detail"] = detail,
    _["n_moves"] = n_moves,
    _["stringsAsFactors"] = false);
  stages.attr("moves") = stage_moves;

  // ---- The move log ------------------------------------------------------
  //
  // The same solve read the other way round. `stages` is the method's shape,
  // one row per step; this is one row per move, each carrying the step it came
  // from and its number in the whole solution.
  //
  // Which is what a solve looks like when it has to be checked by hand: the
  // path alone says what to turn but not why, and the stages say why but hide
  // the turns inside an attribute. A cross edge that goes in on move 14 of a
  // stage that claims three is visible here and nowhere else.
  const size_t total = sol.moves.size();
  CharacterVector log_move(total), log_stage(total), log_detail(total);
  IntegerVector log_index(total), log_in_stage(total);

  {
    std::vector<std::string> all = word_names(sol.moves);
    size_t at = 0;
    for (size_t i = 0; i < n; i++) {
      const Stage& s = sol.stages[i];
      for (size_t k = 0; k < s.moves.size() && at < total; k++, at++) {
        log_move[at] = all[at];
        log_stage[at] = s.name;
        log_detail[at] = s.detail;
        log_index[at] = static_cast<int>(at) + 1;
        log_in_stage[at] = static_cast<int>(k) + 1;
      }
    }
  }

  DataFrame log = DataFrame::create(
    _["move"] = log_move,
    _["stage"] = log_stage,
    _["detail"] = log_detail,
    _["index"] = log_index,
    _["in_stage"] = log_in_stage,
    _["stringsAsFactors"] = false);

  return List::create(
    _["path"] = wrap(word_names(sol.moves)),
    _["found"] = sol.solved,
    _["stages"] = stages,
    _["states"] = states,
    _["log"] = log,
    _["failure"] = sol.failure);
}

// Run a method, and if it gives up return what it had rather than nothing. The
// stages already in `sol` are the ones that landed; the cube after the last of
// them is the position the method could not read, which is exactly what has to
// be looked at to find out why.
template <typename Body>
List run_method(Body body) {
  Solution sol;
  sol.solved = false;
  try {
    body(sol);
  } catch (const std::exception& e) {
    sol.solved = false;
    sol.failure = e.what();
  }
  return solution_to_r(sol);
}

std::vector<int> as_state(IntegerVector state) {
  if (state.size() != 54) {
    stop("cube_solve: state must have 54 entries, got %d",
         static_cast<int>(state.size()));
  }
  return as< std::vector<int> >(state);
}

}  // namespace

// [[Rcpp::export]]
List cube_solve_cfop_cpp(IntegerVector state, int cross_depth, int slot_depth) {
  CfopLimits lim;
  lim.cross_depth = cross_depth;
  lim.slot_depth = slot_depth;
  const std::vector<int> s = as_state(state);
  return run_method([&](Solution& sol) { solve_cfop_into(sol, s, lim); });
}

// [[Rcpp::export]]
List cube_solve_lbl_cpp(IntegerVector state, int cross_depth, int corner_depth,
                        int edge_depth) {
  LblLimits lim;
  lim.cross_depth = cross_depth;
  lim.corner_depth = corner_depth;
  lim.edge_depth = edge_depth;
  const std::vector<int> s = as_state(state);
  return run_method([&](Solution& sol) { solve_lbl_into(sol, s, lim); });
}
