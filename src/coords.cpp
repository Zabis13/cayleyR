#include <Rcpp.h>
#include <cmath>
#include <string>
#include <sstream>
#include <vector>
#include <unordered_set>
#include "cayley_utils.h"
#include "perm_group.h"
#include "celestial_coords.h"

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

CelestialCoords extract_coords_from_list(List cl) {
  CelestialCoords coords;
  coords.nL = as<int>(cl["nL"]);
  coords.nR = as<int>(cl["nR"]);
  coords.nX = as<int>(cl["nX"]);
  coords.theta = as<double>(cl["theta"]);
  coords.phi = as<double>(cl["phi"]);
  coords.omega_conformal = as<double>(cl["omega_conformal"]);
  return coords;
}

List pack_coords(const CelestialCoords& coords) {
  return List::create(
    Named("nL") = coords.nL,
    Named("nR") = coords.nR,
    Named("nX") = coords.nX,
    Named("theta") = coords.theta,
    Named("phi") = coords.phi,
    Named("omega_conformal") = coords.omega_conformal
  );
}

std::string state_hash(const IntegerVector& state) {
  std::stringstream ss; 
  for(int i = 0; i < state.size(); i++) ss << state[i]; 
  return ss.str();
}

// [[Rcpp::export]]
IntegerVector shift_left_simple(IntegerVector state){
  int n = state.size(); 
  if(n == 0) return state; 
  IntegerVector res(n);
  for(int i = 0; i < n - 1; i++) res[i] = state[i + 1]; 
  res[n - 1] = state[0]; 
  return res;
}

// [[Rcpp::export]]  
IntegerVector shift_right_simple(IntegerVector state){
  int n = state.size(); 
  if(n == 0) return state; 
  IntegerVector res(n);
  res[0] = state[n - 1]; 
  for(int i = 1; i < n; i++) res[i] = state[i - 1]; 
  return res;
}

// [[Rcpp::export]]
IntegerVector reverse_prefix_simple(IntegerVector state, int k){
  int n = state.size(); 
  if(k <= 0 || n == 0) return state; 
  IntegerVector res = clone(state);
  int end = std::min(k, n); 
  for(int i = 0; i < end/2; i++){
    int tmp = res[i];
    res[i] = res[end - 1 - i];
    res[end - 1 - i] = tmp;
  }
  return res;
}

// [[Rcpp::export]]
List shift_left(IntegerVector state, Nullable<List> coords = R_NilValue){
  int n = state.size(); 
  if(n == 0) {
    return List::create(Named("state") = state, Named("coords") = pack_coords(create_empty_coords()));
  }
  
  IntegerVector res(n);
  for(int i = 0; i < n - 1; i++) res[i] = state[i + 1]; 
  res[n - 1] = state[0];
  
  CelestialCoords old_coords;
  if (coords.isNull()) {
    old_coords = create_empty_coords();
  } else {
    List coords_list = coords.get();
    old_coords = extract_coords_from_list(coords_list);
  }
  
  CelestialCoords new_coords = update_coords(old_coords, 1, 0, 0);
  
  return List::create(
    Named("state") = res,
    Named("coords") = pack_coords(new_coords)
  );
}

// [[Rcpp::export]]  
List shift_right(IntegerVector state, Nullable<List> coords = R_NilValue){
  int n = state.size(); 
  if(n == 0) {
    return List::create(Named("state") = state, Named("coords") = pack_coords(create_empty_coords()));
  }
  
  IntegerVector res(n);
  res[0] = state[n - 1]; 
  for(int i = 1; i < n; i++) res[i] = state[i - 1];
  
  CelestialCoords old_coords;
  if (coords.isNull()) {
    old_coords = create_empty_coords();
  } else {
    List coords_list = coords.get();
    old_coords = extract_coords_from_list(coords_list);
  }
  
  CelestialCoords new_coords = update_coords(old_coords, 0, 1, 0);
  
  return List::create(
    Named("state") = res,
    Named("coords") = pack_coords(new_coords)
  );
}

// [[Rcpp::export]]
List reverse_prefix(IntegerVector state, int k, Nullable<List> coords = R_NilValue){
  int n = state.size(); 
  if(k <= 0 || n == 0) {
    return List::create(Named("state") = state, Named("coords") = pack_coords(create_empty_coords()));
  }
  
  IntegerVector res = clone(state);
  int end = std::min(k, n); 
  for(int i = 0; i < end/2; i++){
    int tmp = res[i];
    res[i] = res[end - 1 - i];
    res[end - 1 - i] = tmp;
  }
  
  CelestialCoords old_coords;
  if (coords.isNull()) {
    old_coords = create_empty_coords();
  } else {
    List coords_list = coords.get();
    old_coords = extract_coords_from_list(coords_list);
  }
  
  CelestialCoords new_coords = update_coords(old_coords, 0, 0, 1);
  
  return List::create(
    Named("state") = res,
    Named("coords") = pack_coords(new_coords)
  );
}

// apply_op_inplace() and state_to_key() are now in cayley_utils.h

// Cycle detection: returns (total_moves, unique_states_count)
static std::pair<int, int> cycle_detect(
    const std::vector<int>& start,
    const std::vector<int>& word,
    const PermGroup& g,
    int max_moves)
{
  std::vector<int> current = start;
  std::unordered_set<std::string> visited;
  visited.insert(state_to_key(start));
  int total_moves = 0;

  if (word.empty()) return std::make_pair(0, 1);

  // Every word closes eventually -- it is an element of a finite group -- but
  // "eventually" can be a very large number, so the walk is capped rather than
  // left to run unbounded.
  while (total_moves < max_moves) {
    for (size_t i = 0; i < word.size(); i++) {
      g.apply(current, word[i]);
      total_moves++;

      std::string key = state_to_key(current);
      visited.insert(key);

      if (current == start) {
        return std::make_pair(total_moves, (int)visited.size());
      }
    }
  }
  return std::make_pair(total_moves, (int)visited.size());
}

// [[Rcpp::export]]
List get_reachable_states_light_cpp(IntegerVector start_state,
                                     IntegerVector allowed_positions,
                                     SEXP group,
                                     int max_moves = 10000000) {
  XPtr<PermGroup> g(group);
  std::vector<int> start(start_state.begin(), start_state.end());
  std::vector<int> word;
  word.reserve(allowed_positions.size());
  for (int i = 0; i < allowed_positions.size(); i++) {
    int m = allowed_positions[i] - 1;
    if (m < 0 || m >= g->n_moves()) stop("move index %d out of range", allowed_positions[i]);
    word.push_back(m);
  }

  auto result = cycle_detect(start, word, *g, max_moves);

  return List::create(
    Named("total_moves") = result.first,
    Named("unique_states_count") = result.second
  );
}

// [[Rcpp::export]]
List find_best_random_combinations_cpp(
    IntegerVector start_state,
    SEXP group,
    IntegerVector moves,
    int combo_length,
    int n_samples,
    int max_moves = 10000000)
{
  XPtr<PermGroup> g(group);
  std::vector<int> start(start_state.begin(), start_state.end());

  // The alphabet to draw from, as indices into the group.
  std::vector<int> move_vec;
  move_vec.reserve(moves.size());
  for (int i = 0; i < moves.size(); i++) {
    int m = moves[i] - 1;
    if (m < 0 || m >= g->n_moves()) stop("move index %d out of range", moves[i]);
    move_vec.push_back(m);
  }
  int n_moves = (int)move_vec.size();
  if (n_moves == 0) stop("moves must contain at least one operation");

  // Pre-generate unique combos on main thread using R's RNG
  std::unordered_set<std::string> seen_keys;
  std::vector<std::vector<int> > combos;
  combos.reserve(n_samples);

  int max_iter = n_samples * 10;
  while ((int)combos.size() < n_samples && max_iter > 0) {
    std::vector<int> combo(combo_length);
    std::string key;
    key.reserve(combo_length * 4);
    for (int j = 0; j < combo_length; j++) {
      int idx = (int)(R::runif(0.0, 1.0) * n_moves);
      if (idx >= n_moves) idx = n_moves - 1;
      combo[j] = move_vec[idx];
      key += std::to_string(combo[j]);
      key += ',';
    }
    if (seen_keys.find(key) == seen_keys.end()) {
      seen_keys.insert(key);
      combos.push_back(combo);
    }
    max_iter--;
  }

  int n_combos = (int)combos.size();
  std::vector<int> res_total(n_combos, 0);
  std::vector<int> res_unique(n_combos, 0);

  // Words are reported in the group's own spelling, space separated. Joining
  // them unseparated would only be readable back for TopSpin, whose moves are
  // one character each; a cube's "R'" and "R" would run together.
  CharacterVector combo_keys(n_combos);
  for (int i = 0; i < n_combos; i++) {
    std::string key;
    for (int j = 0; j < (int)combos[i].size(); j++) {
      if (j > 0) key += ' ';
      key += g->move_name(combos[i][j]);
    }
    combo_keys[i] = key;
  }

  #pragma omp parallel for schedule(dynamic)
  for (int i = 0; i < n_combos; i++) {
    auto result = cycle_detect(start, combos[i], *g, max_moves);
    res_total[i] = result.first;
    res_unique[i] = result.second;
  }

  return List::create(
    Named("combination") = combo_keys,
    Named("total_moves") = IntegerVector(res_total.begin(), res_total.end()),
    Named("unique_states_count") = IntegerVector(res_unique.begin(), res_unique.end())
  );
}

// [[Rcpp::export]]
int openmp_threads() {
#ifdef _OPENMP
  return omp_get_max_threads();
#else
  return 1;
#endif
}

// [[Rcpp::export]]
List apply_operations(IntegerVector state, CharacterVector operations, int k,
                      Nullable<List> coords = R_NilValue,
                      bool compute_coords = true) {
  int n = state.size();
  // Work with raw int vector to avoid R allocations per operation
  std::vector<int> cur(state.begin(), state.end());

  CelestialCoords current_coords;
  if (compute_coords) {
    if (coords.isNull()) {
      current_coords = create_empty_coords();
    } else {
      List coords_list = coords.get();
      current_coords = extract_coords_from_list(coords_list);
    }
  }

  int n_ops = operations.size();

  for(int i = 0; i < n_ops; i++) {
    const char* op = CHAR(STRING_ELT(operations, i));
    char c = op[0];

    if(c == 'L' || c == '1') {
      if (compute_coords) current_coords = update_coords(current_coords, 1, 0, 0);
      int tmp = cur[0];
      for(int j = 0; j < n - 1; j++) cur[j] = cur[j + 1];
      cur[n - 1] = tmp;
    } else if(c == 'R' || c == '2') {
      if (compute_coords) current_coords = update_coords(current_coords, 0, 1, 0);
      int tmp = cur[n - 1];
      for(int j = n - 1; j > 0; j--) cur[j] = cur[j - 1];
      cur[0] = tmp;
    } else if(c == 'X' || c == '3') {
      if (compute_coords) current_coords = update_coords(current_coords, 0, 0, 1);
      int end = std::min(k, n);
      for(int j = 0; j < end/2; j++){
        int tmp = cur[j];
        cur[j] = cur[end - 1 - j];
        cur[end - 1 - j] = tmp;
      }
    } else {
      stop("Unknown operation: %s", op);
    }
  }

  // Convert back to IntegerVector for R
  IntegerVector result_state(cur.begin(), cur.end());

  if (compute_coords) {
    return List::create(
      Named("state") = result_state,
      Named("coords") = pack_coords(current_coords)
    );
  } else {
    return List::create(
      Named("state") = result_state
    );
  }
}

