#include <Rcpp.h>
#include <vector>
#include <string>
#include "perm_group.h"

using namespace Rcpp;

// R-facing construction of the group objects the core runs on. A group crosses
// into C++ once, as an external pointer, and every later call passes that
// pointer rather than re-describing the puzzle. The permutation tables stay
// authored in R -- cube_moves() is the cube's only definition, here as well as
// there -- so there is no second copy to drift.

// [[Rcpp::export]]
SEXP perm_group_create_table_cpp(int state_length,
                                 CharacterVector move_names,
                                 List move_perms) {
  if (move_names.size() != move_perms.size()) {
    stop("perm_group: move_names and perms must have the same length");
  }
  if (move_names.size() == 0) stop("perm_group: need at least one move");

  std::vector<std::string> names;
  names.reserve(move_names.size());
  for (int i = 0; i < move_names.size(); i++) {
    names.push_back(as<std::string>(move_names[i]));
  }

  std::vector<std::vector<int> > perms;
  perms.reserve(move_perms.size());
  for (int m = 0; m < move_perms.size(); m++) {
    IntegerVector p = move_perms[m];
    std::vector<int> v(p.size());
    // R writes permutations 1-based; the core indexes 0-based
    for (int i = 0; i < p.size(); i++) {
      if (IntegerVector::is_na(p[i])) {
        stop("perm_group: move '%s' contains NA", names[m]);
      }
      v[i] = p[i] - 1;
    }
    perms.push_back(v);
  }

  try {
    PermGroup* g = new TablePermGroup(state_length, names, perms);
    XPtr<PermGroup> xp(g, true);
    return xp;
  } catch (const std::exception& e) {
    stop("perm_group: %s", e.what());
  }
}

// [[Rcpp::export]]
SEXP perm_group_create_topspin_cpp(int state_length, int k,
                                   CharacterVector move_names) {
  if (move_names.size() == 0) stop("perm_group: need at least one move");

  std::vector<std::string> names;
  names.reserve(move_names.size());
  for (int i = 0; i < move_names.size(); i++) {
    names.push_back(as<std::string>(move_names[i]));
  }

  try {
    PermGroup* g = new TopSpinGroup(state_length, k, names);
    XPtr<PermGroup> xp(g, true);
    return xp;
  } catch (const std::exception& e) {
    stop("perm_group: %s", e.what());
  }
}

// ---- Inspection and the contract, exposed for testing and for R-side use ----

// [[Rcpp::export]]
List perm_group_info_cpp(SEXP group) {
  XPtr<PermGroup> g(group);
  CharacterVector nm(g->n_moves());
  IntegerVector inv(g->n_moves());
  for (int m = 0; m < g->n_moves(); m++) {
    nm[m] = g->move_name(m);
    int i = g->inverse_move(m);
    inv[m] = (i < 0) ? NA_INTEGER : (i + 1);
  }
  return List::create(
    _["n"] = g->state_length(),
    _["n_moves"] = g->n_moves(),
    _["move_names"] = nm,
    _["inverse_of"] = inv
  );
}

// [[Rcpp::export]]
IntegerVector perm_group_apply_cpp(SEXP group, IntegerVector state,
                                   IntegerVector word) {
  XPtr<PermGroup> g(group);
  if (state.size() != g->state_length()) {
    stop("state has length %d, group expects %d", state.size(),
         g->state_length());
  }
  std::vector<int> s(state.begin(), state.end());
  for (int i = 0; i < word.size(); i++) {
    int m = word[i] - 1;   // R passes 1-based move indices
    if (m < 0 || m >= g->n_moves()) stop("move index %d out of range", word[i]);
    g->apply(s, m);
  }
  return wrap(s);
}

// [[Rcpp::export]]
IntegerVector perm_group_compose_cpp(SEXP group, IntegerVector word) {
  XPtr<PermGroup> g(group);
  std::vector<int> w(word.size());
  for (int i = 0; i < word.size(); i++) {
    int m = word[i] - 1;
    if (m < 0 || m >= g->n_moves()) stop("move index %d out of range", word[i]);
    w[i] = m;
  }
  std::vector<int> perm = g->compose(w);
  IntegerVector out(perm.size());
  for (size_t i = 0; i < perm.size(); i++) out[i] = perm[i] + 1;
  return out;
}

// [[Rcpp::export]]
IntegerVector perm_group_inverse_seq_cpp(SEXP group, IntegerVector word) {
  XPtr<PermGroup> g(group);
  std::vector<int> w(word.size());
  for (int i = 0; i < word.size(); i++) {
    int m = word[i] - 1;
    if (m < 0 || m >= g->n_moves()) stop("move index %d out of range", word[i]);
    w[i] = m;
  }
  try {
    std::vector<int> inv = g->inverse_seq(w);
    IntegerVector out(inv.size());
    for (size_t i = 0; i < inv.size(); i++) out[i] = inv[i] + 1;
    return out;
  } catch (const std::exception& e) {
    stop("%s", e.what());
  }
}

// [[Rcpp::export]]
IntegerVector perm_group_identity_cpp(SEXP group) {
  XPtr<PermGroup> g(group);
  return wrap(g->identity());
}
