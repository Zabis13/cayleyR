#include <Rcpp.h>
#include <cmath>
#include <string>
#include <sstream>
#include <vector>
#include <unordered_set>
#include "cayley_utils.h"

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

struct CelestialCoords {
  int nL;
  int nR;
  int nX;
  double theta;
  double phi;
  double omega_conformal;
};

CelestialCoords update_coords(const CelestialCoords& coords, int delta_L, int delta_R, int delta_X) {
  CelestialCoords new_coords;
  new_coords.nL = coords.nL + delta_L;
  new_coords.nR = coords.nR + delta_R;
  new_coords.nX = coords.nX + delta_X;
  
  double r = std::sqrt(new_coords.nL * new_coords.nL + 
                       new_coords.nR * new_coords.nR + 
                       new_coords.nX * new_coords.nX);
  
  new_coords.omega_conformal = r;
  
  if (r > 1e-10) {
    new_coords.theta = std::acos(new_coords.nX / r);
    new_coords.phi = std::atan2((double)new_coords.nR, (double)new_coords.nL);
  } else {
    new_coords.theta = 0.0;
    new_coords.phi = 0.0;
  }
  
  return new_coords;
}

CelestialCoords create_empty_coords() {
  CelestialCoords coords;
  coords.nL = 0;
  coords.nR = 0;
  coords.nX = 0;
  coords.theta = 0.0;
  coords.phi = 0.0;
  coords.omega_conformal = 0.0;
  return coords;
}

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
    const std::vector<std::string>& ops,
    int k)
{
  std::vector<int> current = start;
  std::unordered_set<std::string> visited;
  visited.insert(state_to_key(start));
  int total_moves = 0;

  while (true) {
    for (size_t i = 0; i < ops.size(); i++) {
      apply_op_inplace(current, ops[i], k);
      total_moves++;

      std::string key = state_to_key(current);
      visited.insert(key);

      if (current == start) {
        return std::make_pair(total_moves, (int)visited.size());
      }
    }
  }
  // Should not reach here for valid Cayley graph inputs
  return std::make_pair(total_moves, (int)visited.size());
}

// [[Rcpp::export]]
List get_reachable_states_light_cpp(IntegerVector start_state,
                                     CharacterVector allowed_positions,
                                     int k) {
  // Convert to std types
  std::vector<int> start(start_state.begin(), start_state.end());
  std::vector<std::string> ops(allowed_positions.size());
  for (int i = 0; i < allowed_positions.size(); i++) {
    ops[i] = as<std::string>(allowed_positions[i]);
  }

  auto result = cycle_detect(start, ops, k);

  return List::create(
    Named("total_moves") = result.first,
    Named("unique_states_count") = result.second
  );
}

// [[Rcpp::export]]
List find_best_random_combinations_cpp(
    IntegerVector start_state,
    int k,
    CharacterVector moves,
    int combo_length,
    int n_samples)
{
  // Convert inputs to std types
  std::vector<int> start(start_state.begin(), start_state.end());
  std::vector<std::string> move_vec(moves.size());
  for (int i = 0; i < moves.size(); i++) {
    move_vec[i] = as<std::string>(moves[i]);
  }
  int n_moves = (int)move_vec.size();

  // Pre-generate unique combos on main thread using R's RNG
  std::unordered_set<std::string> seen_keys;
  std::vector<std::vector<std::string>> combos;
  combos.reserve(n_samples);

  int max_iter = n_samples * 10;
  while ((int)combos.size() < n_samples && max_iter > 0) {
    std::vector<std::string> combo(combo_length);
    std::string key;
    key.reserve(combo_length * 2);
    for (int j = 0; j < combo_length; j++) {
      int idx = (int)(R::runif(0.0, 1.0) * n_moves);
      if (idx >= n_moves) idx = n_moves - 1;
      combo[j] = move_vec[idx];
      key += combo[j];
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

  // Build combo key strings (main thread, before parallel section)
  CharacterVector combo_keys(n_combos);
  for (int i = 0; i < n_combos; i++) {
    std::string key;
    for (int j = 0; j < (int)combos[i].size(); j++) {
      key += combos[i][j];
    }
    combo_keys[i] = key;
  }

  #pragma omp parallel for schedule(dynamic)
  for (int i = 0; i < n_combos; i++) {
    auto result = cycle_detect(start, combos[i], k);
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
                      Nullable<List> coords = R_NilValue) {
  IntegerVector current_state = clone(state);
  
  CelestialCoords current_coords;
  if (coords.isNull()) {
    current_coords = create_empty_coords();
  } else {
    List coords_list = coords.get();
    current_coords = extract_coords_from_list(coords_list);
  }
  
  int n_ops = operations.size();
  
  for(int i = 0; i < n_ops; i++) {
    std::string op = as<std::string>(operations[i]);
    
    if(op == "L" || op == "1") {
      current_coords = update_coords(current_coords, 1, 0, 0);
      int n = current_state.size();
      IntegerVector res(n);
      for(int j = 0; j < n - 1; j++) res[j] = current_state[j + 1]; 
      res[n - 1] = current_state[0];
      current_state = res;
    } else if(op == "R" || op == "2") {
      current_coords = update_coords(current_coords, 0, 1, 0);
      int n = current_state.size();
      IntegerVector res(n);
      res[0] = current_state[n - 1]; 
      for(int j = 1; j < n; j++) res[j] = current_state[j - 1];
      current_state = res;
    } else if(op == "X" || op == "3") {
      current_coords = update_coords(current_coords, 0, 0, 1);
      int n = current_state.size();
      int end = std::min(k, n); 
      for(int j = 0; j < end/2; j++){
        int tmp = current_state[j];
        current_state[j] = current_state[end - 1 - j];
        current_state[end - 1 - j] = tmp;
      }
    } else {
      stop("Unknown operation: " + op);
    }
  }
  
  return List::create(
    Named("state") = current_state,
    Named("coords") = pack_coords(current_coords)
  );
}

