#include <Rcpp.h>
#include <vector>
#include <string>
#include "cube_cubie.h"
#include "cube_search.h"
#include "cube_algs.h"

using namespace Rcpp;
using namespace cube_cubie;
using namespace cube_search;
using namespace cube_algs;

// ---- The cube's own foundations, R-facing -------------------------------
//
// Everything a test needs to check that the layer below it means what it says:
// the alphabet, the reading of a state into pieces, the stage predicates, and
// the algorithm tables. These are not part of solving a cube -- they are how
// one finds out whether the parts that do are built on anything.
//
// The C++ side of the package has no test harness of its own, so the checks
// live in testthat and reach down through here.

namespace {

std::vector<int> as_state(IntegerVector state) {
  if (state.size() != 54) {
    stop("cube: state must have 54 entries, got %d",
         static_cast<int>(state.size()));
  }
  return as< std::vector<int> >(state);
}

}  // namespace

// A word of moves applied to a state, both in package convention.
// [[Rcpp::export]]
IntegerVector cube_apply_word_cpp(IntegerVector state, std::string word) {
  std::vector<int> w = parse_word(word);
  return wrap(apply_word(as_state(state), w));
}

// How many times a word must be repeated to return to where it started. A
// quarter turn is 4, a half turn 2; anything else means the move is not what
// it is named.
// [[Rcpp::export]]
int cube_word_order_cpp(std::string word, int max_order = 1260) {
  std::vector<int> w = parse_word(word);
  std::vector<int> id(54);
  for (int i = 0; i < 54; i++) id[i] = i + 1;

  std::vector<int> s = apply_word(id, w);
  for (int n = 1; n <= max_order; n++) {
    if (s == id) return n;
    s = apply_word(s, w);
  }
  return -1;
}

// A state read into pieces: which cubie sits in each slot and how it is
// turned. This is the reading every predicate is built on, so a test that
// doubts a predicate starts here.
// [[Rcpp::export]]
List cube_read_state_cpp(IntegerVector state) {
  CubieState c = read_state(as_state(state));

  IntegerVector cp(8), co(8), ep(12), eo(12);
  for (int i = 0; i < 8; i++) { cp[i] = c.cp[i]; co[i] = c.co[i]; }
  for (int i = 0; i < 12; i++) { ep[i] = c.ep[i]; eo[i] = c.eo[i]; }

  return List::create(_["cp"] = cp, _["co"] = co,
                      _["ep"] = ep, _["eo"] = eo);
}

// The stage predicates, all of them, as one named logical vector. A solver
// stage is exactly "search until the predicate holds", so these are the
// definitions of the stages themselves.
// [[Rcpp::export]]
LogicalVector cube_predicates_cpp(IntegerVector state) {
  CubieState c = read_state(as_state(state));

  LogicalVector out = LogicalVector::create(
    _["cross_solved"] = cross_solved(c),
    _["slot_1"] = slot_solved(c, 0),
    _["slot_2"] = slot_solved(c, 1),
    _["slot_3"] = slot_solved(c, 2),
    _["slot_4"] = slot_solved(c, 3),
    _["f2l_solved"] = f2l_solved(c),
    _["oll_solved"] = oll_solved(c),
    _["first_layer_solved"] = first_layer_solved(c),
    _["ll_cross_oriented"] = ll_cross_oriented(c),
    _["ll_edges_placed"] = ll_edges_placed(c),
    _["ll_corners_placed"] = ll_corners_placed(c),
    _["cube_solved"] = cube_solved(c));
  return out;
}

// One of the algorithm tables, as a data.frame of name and notation, with the
// expanded quarter-turn word on a "moves" attribute. Which table is named the
// way the solvers name them.
// [[Rcpp::export]]
DataFrame cube_alg_table_cpp(std::string which) {
  const std::vector<Alg>* t = 0;
  if (which == "oll") t = &oll_table();
  else if (which == "pll") t = &pll_table();
  else if (which == "lbl_cross") t = &lbl_cross_table();
  else if (which == "lbl_edge_perm") t = &lbl_edge_perm_table();
  else if (which == "lbl_corner_perm") t = &lbl_corner_perm_table();
  else if (which == "lbl_corner_twist") t = &lbl_corner_twist_table();
  else stop("cube_alg_table: no table named '%s'", which.c_str());

  const size_t n = t->size();
  CharacterVector name(n), notation(n);
  IntegerVector n_moves(n);
  List moves(n);

  for (size_t i = 0; i < n; i++) {
    name[i] = (*t)[i].name;
    notation[i] = (*t)[i].moves;
    std::vector<int> w = alg_word((*t)[i].moves);
    n_moves[i] = static_cast<int>(w.size());
    moves[i] = wrap(word_names(w));
  }

  DataFrame df = DataFrame::create(
    _["name"] = name, _["notation"] = notation, _["n_moves"] = n_moves,
    _["stringsAsFactors"] = false);
  df.attr("moves") = moves;
  return df;
}

// An algorithm's notation expanded into the package's quarter-turn alphabet,
// as move names. This is where rotations and wide turns are resolved, so it is
// what a test of the notation has to look at.
// [[Rcpp::export]]
CharacterVector cube_expand_alg_cpp(std::string notation) {
  return wrap(word_names(alg_word(notation.c_str())));
}

// The six centres of a 3x3x3, as sticker positions. They never move, which is
// what makes a face's name mean anything.
// [[Rcpp::export]]
IntegerVector cube_centre_positions_cpp() {
  IntegerVector v(6);
  for (int f = 0; f < 6; f++) v[f] = f * 9 + 4 + 1;   // 1-based
  return v;
}
