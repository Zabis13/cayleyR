#include <Rcpp.h>
#include "state_store.h"
#include "celestial_coords.h"

using namespace Rcpp;

// ============================================================
// XPtr-based API for StateStore
// ============================================================

typedef XPtr<StateStore> StateStorePtr;

// [[Rcpp::export]]
SEXP state_store_create(int perm_length, int init_capacity = 10000) {
  StateStore* store = new StateStore(perm_length, init_capacity);
  StateStorePtr xp(store, true); // Release via destructor
  return xp;
}

// [[Rcpp::export]]
int state_store_size(SEXP xp) {
  StateStorePtr store(xp);
  return store->count;
}

// [[Rcpp::export]]
int state_store_unique_count(SEXP xp) {
  StateStorePtr store(xp);
  return store->unique_key_count();
}

// [[Rcpp::export]]
int state_store_perm_length(SEXP xp) {
  StateStorePtr store(xp);
  return store->L;
}

// Add a batch of states from an IntegerMatrix + metadata vectors.
// states_mat: n_rows x L (R column-major IntegerMatrix)
// op_int: integer vector of operation codes (1=L, 2=R, 3=X, 0/NA=NA)
// [[Rcpp::export]]
int state_store_add_batch(SEXP xp,
                          IntegerMatrix states_mat,
                          IntegerVector step_vec,
                          IntegerVector combo_vec,
                          int cycle_val,
                          IntegerVector op_vec,
                          IntegerVector nL_vec,
                          IntegerVector nR_vec,
                          IntegerVector nX_vec,
                          NumericVector theta_vec,
                          NumericVector phi_vec,
                          NumericVector omega_vec) {
  StateStorePtr store(xp);

  int n_rows = states_mat.nrow();
  int ncol = states_mat.ncol();
  if (ncol != store->L) {
    stop("states_mat has %d columns, expected %d", ncol, store->L);
  }

  // Validate vector lengths
  if (step_vec.size() != n_rows || combo_vec.size() != n_rows ||
      op_vec.size() != n_rows) {
    stop("Metadata vector lengths must match number of rows (%d)", n_rows);
  }

  const int* mat_ptr = states_mat.begin(); // column-major
  const int* step_ptr = step_vec.begin();
  const int* combo_ptr = combo_vec.begin();
  const int* op_ptr = op_vec.begin();

  const int* nL_ptr = (nL_vec.size() == n_rows) ? nL_vec.begin() : nullptr;
  const int* nR_ptr = (nR_vec.size() == n_rows) ? nR_vec.begin() : nullptr;
  const int* nX_ptr = (nX_vec.size() == n_rows) ? nX_vec.begin() : nullptr;
  const double* theta_ptr = (theta_vec.size() == n_rows) ? theta_vec.begin() : nullptr;
  const double* phi_ptr = (phi_vec.size() == n_rows) ? phi_vec.begin() : nullptr;
  const double* omega_ptr = (omega_vec.size() == n_rows) ? omega_vec.begin() : nullptr;

  return store->add_batch(mat_ptr, n_rows, /*col_major=*/true,
                          step_ptr, combo_ptr, cycle_val, op_ptr,
                          nL_ptr, nR_ptr, nX_ptr,
                          theta_ptr, phi_ptr, omega_ptr);
}

// Get state at index (0-based) as IntegerVector
// [[Rcpp::export]]
IntegerVector state_store_get_state(SEXP xp, int idx) {
  StateStorePtr store(xp);
  if (idx < 0 || idx >= store->count) {
    stop("Index %d out of range [0, %d)", idx, store->count);
  }
  const int* ptr = store->get_state_ptr(idx);
  return IntegerVector(ptr, ptr + store->L);
}

// Get metadata for a single row (0-based index)
// [[Rcpp::export]]
List state_store_get_meta(SEXP xp, int idx) {
  StateStorePtr store(xp);
  if (idx < 0 || idx >= store->count) {
    stop("Index %d out of range [0, %d)", idx, store->count);
  }
  return List::create(
    Named("step") = store->step[idx],
    Named("combo_number") = store->combo_number[idx],
    Named("cycle") = store->cycle[idx],
    Named("operation") = op_to_string(store->operation[idx]),
    Named("nL") = store->nL_vec[idx],
    Named("nR") = store->nR_vec[idx],
    Named("nX") = store->nX_vec[idx],
    Named("theta") = store->theta_vec[idx],
    Named("phi") = store->phi_vec[idx],
    Named("omega_conformal") = store->omega_vec[idx]
  );
}

// Lookup indices by state key string
// [[Rcpp::export]]
IntegerVector state_store_lookup(SEXP xp, std::string key) {
  StateStorePtr store(xp);
  const auto* indices = store->lookup(key);
  if (!indices) return IntegerVector(0);
  return IntegerVector(indices->begin(), indices->end());
}

// Lookup indices by state vector
// [[Rcpp::export]]
IntegerVector state_store_lookup_state(SEXP xp, IntegerVector state) {
  StateStorePtr store(xp);
  std::string key = StateStore::state_to_key_raw(state.begin(), state.size());
  const auto* indices = store->lookup(key);
  if (!indices) return IntegerVector(0);
  return IntegerVector(indices->begin(), indices->end());
}

// Find intersection keys between two stores
// Returns a CharacterVector of common state keys
// [[Rcpp::export]]
CharacterVector state_store_find_intersections(SEXP xp_a, SEXP xp_b) {
  StateStorePtr store_a(xp_a);
  StateStorePtr store_b(xp_b);
  auto keys = store_a->find_intersection_keys(*store_b);
  CharacterVector result(keys.size());
  for (size_t i = 0; i < keys.size(); i++) {
    result[i] = keys[i];
  }
  return result;
}

// Find best match (manhattan) among all states or a subset (indices)
// Returns 0-based index
// [[Rcpp::export]]
int state_store_find_best_match(SEXP xp, IntegerVector target,
                                IntegerVector candidate_indices) {
  StateStorePtr store(xp);
  if (target.size() != store->L) {
    stop("target length %d != store perm_length %d", target.size(), store->L);
  }
  std::vector<int> candidates(candidate_indices.begin(), candidate_indices.end());
  return store->find_best_match_manhattan(target.begin(), candidates);
}

// Get indices for a given cycle
// [[Rcpp::export]]
IntegerVector state_store_indices_for_cycle(SEXP xp, int target_cycle) {
  StateStorePtr store(xp);
  auto indices = store->indices_for_cycle(target_cycle);
  return IntegerVector(indices.begin(), indices.end());
}

// Filter middle states for a cycle
// [[Rcpp::export]]
IntegerVector state_store_filter_middle(SEXP xp, int target_cycle,
                                         int skip_first, int skip_last) {
  StateStorePtr store(xp);
  auto indices = store->filter_middle_indices(target_cycle, skip_first, skip_last);
  return IntegerVector(indices.begin(), indices.end());
}

// Convert entire store to a data.frame (for debugging / backward compat)
// [[Rcpp::export]]
DataFrame state_store_to_dataframe(SEXP xp) {
  StateStorePtr store(xp);
  int n = store->count;
  int L = store->L;

  // Build V1..VL columns
  List cols(L);
  CharacterVector col_names(L);
  for (int j = 0; j < L; j++) {
    IntegerVector col(n);
    for (int i = 0; i < n; i++) {
      col[i] = store->states[i * L + j];
    }
    cols[j] = col;
    col_names[j] = "V" + std::to_string(j + 1);
  }

  // Operation as character
  CharacterVector op_chr(n);
  for (int i = 0; i < n; i++) {
    std::string s = op_to_string(store->operation[i]);
    if (s.empty()) {
      op_chr[i] = NA_STRING;
    } else {
      op_chr[i] = s;
    }
  }

  // Step: convert NA_INTEGER properly
  IntegerVector step_out(store->step.begin(), store->step.end());

  // Build the data.frame
  int total_cols = L + 10; // V1..VL + operation + step + combo_number + cycle + nL + nR + nX + theta + phi + omega
  List df(total_cols);
  CharacterVector df_names(total_cols);

  for (int j = 0; j < L; j++) {
    df[j] = cols[j];
    df_names[j] = col_names[j];
  }

  int idx = L;
  df[idx] = op_chr;           df_names[idx] = "operation";      idx++;
  df[idx] = step_out;         df_names[idx] = "step";           idx++;
  df[idx] = IntegerVector(store->combo_number.begin(), store->combo_number.end());
                               df_names[idx] = "combo_number";   idx++;
  df[idx] = IntegerVector(store->cycle.begin(), store->cycle.end());
                               df_names[idx] = "cycle";          idx++;
  df[idx] = IntegerVector(store->nL_vec.begin(), store->nL_vec.end());
                               df_names[idx] = "nL";             idx++;
  df[idx] = IntegerVector(store->nR_vec.begin(), store->nR_vec.end());
                               df_names[idx] = "nR";             idx++;
  df[idx] = IntegerVector(store->nX_vec.begin(), store->nX_vec.end());
                               df_names[idx] = "nX";             idx++;
  df[idx] = NumericVector(store->theta_vec.begin(), store->theta_vec.end());
                               df_names[idx] = "theta";          idx++;
  df[idx] = NumericVector(store->phi_vec.begin(), store->phi_vec.end());
                               df_names[idx] = "phi";            idx++;
  df[idx] = NumericVector(store->omega_vec.begin(), store->omega_vec.end());
                               df_names[idx] = "omega_conformal"; idx++;

  df.attr("names") = df_names;
  df.attr("class") = "data.frame";
  df.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);

  return df;
}

// Reconstruct path from store: trace back through cycles to build operation sequence
// Returns CharacterVector of operations
// [[Rcpp::export]]
Nullable<CharacterVector> state_store_reconstruct_path(
    SEXP xp,
    IntegerVector start_state_vec,
    IntegerVector target_state_vec,
    int target_cycle,
    int target_combo) {

  StateStorePtr store(xp);
  int L = store->L;

  if (target_cycle == 0) {
    return CharacterVector(0);
  }

  std::vector<int> target_state(target_state_vec.begin(), target_state_vec.end());
  std::vector<std::string> full_path;

  for (int cyc = 1; cyc <= target_cycle; cyc++) {
    // Get all indices for this cycle
    auto cycle_indices = store->indices_for_cycle(cyc);
    if (cycle_indices.empty()) {
      return R_NilValue;
    }

    // Find initial state of this cycle (step == NA or step == 1)
    int initial_idx = -1;
    for (int idx : cycle_indices) {
      if (store->step[idx] == NA_INTEGER) {
        initial_idx = idx;
        break;
      }
    }
    if (initial_idx == -1) {
      // Try step == 1
      for (int idx : cycle_indices) {
        if (store->step[idx] == 1) {
          initial_idx = idx;
          break;
        }
      }
    }
    if (initial_idx == -1) {
      return R_NilValue;
    }

    const int* cycle_initial = store->get_state_ptr(initial_idx);

    // For cycles > 1: find this initial state in previous cycle, get path to it
    if (cyc > 1) {
      std::string init_key = StateStore::state_to_key_raw(cycle_initial, L);
      auto prev_indices = store->indices_for_cycle(cyc - 1);

      int match_idx = -1;
      for (int idx : prev_indices) {
        const int* s = store->get_state_ptr(idx);
        if (StateStore::state_to_key_raw(s, L) == init_key) {
          match_idx = idx;
          break;
        }
      }
      if (match_idx == -1) {
        return R_NilValue;
      }

      int match_combo = store->combo_number[match_idx];
      int match_step = store->step[match_idx];

      // Collect operations from this combo up to (but not including) the match step
      if (match_step != NA_INTEGER) {
        // Get all rows for this combo in prev cycle, sorted by step
        std::vector<std::pair<int, int>> combo_rows; // (step, idx)
        for (int idx : prev_indices) {
          if (store->combo_number[idx] == match_combo && store->step[idx] != NA_INTEGER) {
            combo_rows.push_back({store->step[idx], idx});
          }
        }
        std::sort(combo_rows.begin(), combo_rows.end());

        for (auto& p : combo_rows) {
          if (p.first >= match_step) break;
          std::string op = op_to_string(store->operation[p.second]);
          if (!op.empty()) {
            full_path.push_back(op);
          }
        }
      }
    }

    // For the target cycle: get operations up to the target state
    if (cyc == target_cycle) {
      // Find target in this cycle
      int target_idx = -1;
      for (int idx : cycle_indices) {
        if (store->combo_number[idx] != target_combo) continue;
        const int* s = store->get_state_ptr(idx);
        bool match = true;
        for (int j = 0; j < L; j++) {
          if (s[j] != target_state[j]) { match = false; break; }
        }
        if (match) { target_idx = idx; break; }
      }
      if (target_idx == -1) {
        return R_NilValue;
      }

      int target_step_val = store->step[target_idx];
      if (target_step_val == NA_INTEGER) {
        // Target is the initial state of this cycle — no additional ops
      } else {
        // Get combo operations up to target step
        std::vector<std::pair<int, int>> combo_rows;
        for (int idx : cycle_indices) {
          if (store->combo_number[idx] == target_combo && store->step[idx] != NA_INTEGER) {
            combo_rows.push_back({store->step[idx], idx});
          }
        }
        std::sort(combo_rows.begin(), combo_rows.end());

        for (auto& p : combo_rows) {
          if (p.first >= target_step_val) break;
          std::string op = op_to_string(store->operation[p.second]);
          if (!op.empty()) {
            full_path.push_back(op);
          }
        }
      }
    }
  }

  CharacterVector result(full_path.size());
  for (size_t i = 0; i < full_path.size(); i++) {
    result[i] = full_path[i];
  }
  return result;
}

// ============================================================
// Analyze combos directly into StateStore (replaces analyze_top_combinations)
// ============================================================

// Analyze a single combo: run full cycle from start_state, writing all
// intermediate states + coords directly into the store.
// combo_str: e.g. "132" — each char is an operation
// combo_number: 1-based combo index
// cycle_val: cycle number to assign
static void analyze_single_combo_to_store(
    StateStore* store,
    const std::string& combo_str,
    const std::vector<int>& start_state,
    int k,
    int combo_number,
    int cycle_val)
{
  int L = (int)start_state.size();
  std::vector<int> current = start_state;
  CelestialCoords coords = create_empty_coords();

  // Parse combo string into ops
  std::vector<std::string> ops;
  ops.reserve(combo_str.size());
  for (char c : combo_str) {
    ops.push_back(std::string(1, c));
  }

  // Collect all states, then write to store.
  // Format matches old R code: first row = start_state with step=1/op=op1,
  // intermediate rows = states with their steps/ops,
  // last row = start_state (returned) with step=NA/op=NA.
  //
  // Old R code layout:
  //   states_list = [start, after_op1, after_op2, ..., start_again]  (N+1 rows)
  //   ops_final   = [op1,   op2,       op3,       ..., NA]           (N+1)
  //   steps_final = [1,     2,         3,         ..., NA]           (N+1)

  // First: run the cycle, collecting states and ops
  std::vector<std::vector<int>> all_states;
  std::vector<CelestialCoords> all_coords;
  std::vector<OpCode> all_ops;

  all_states.push_back(start_state);
  all_coords.push_back(create_empty_coords());

  int step = 0;
  bool done = false;

  while (!done) {
    for (size_t oi = 0; oi < ops.size(); oi++) {
      const std::string& op = ops[oi];
      OpCode op_code = op_from_string(op);

      apply_op_inplace(current, op, k);

      int dL = (op_code == OP_L) ? 1 : 0;
      int dR = (op_code == OP_R) ? 1 : 0;
      int dX = (op_code == OP_X) ? 1 : 0;
      coords = update_coords(coords, dL, dR, dX);

      step++;
      all_states.push_back(current);
      all_coords.push_back(coords);
      all_ops.push_back(op_code);

      if (current == start_state && step > 0) {
        done = true;
        break;
      }
    }
  }

  // Now write to store in the same layout as old R code:
  // Row i (0-based): state=all_states[i], op=all_ops[i] (or NA for last), step=i+1 (or NA for last)
  int n_states = (int)all_states.size(); // N+1

  // all_ops has N entries (one per applied operation)
  // ops_final = [all_ops[0], all_ops[1], ..., all_ops[N-1], NA]  (N+1)
  // steps_final = [1, 2, ..., N, NA]                              (N+1)

  store->ensure_capacity(n_states);

  for (int i = 0; i < n_states; i++) {
    int step_val = (i < n_states - 1) ? (i + 1) : NA_INTEGER;
    OpCode op_val = (i < n_states - 1) ? all_ops[i] : OP_NA;
    const CelestialCoords& c = all_coords[i];

    store->add_state(all_states[i].data(),
                     step_val, combo_number, cycle_val, op_val,
                     c.nL, c.nR, c.nX,
                     c.theta, c.phi, c.omega_conformal);
  }
}

// [[Rcpp::export]]
int analyze_combos_to_store_cpp(SEXP xp,
                                 CharacterVector combinations,
                                 IntegerVector start_state,
                                 int k,
                                 int cycle_val) {
  StateStorePtr store(xp);

  std::vector<int> start(start_state.begin(), start_state.end());
  int total_added = 0;
  int initial_count = store->count;

  for (int i = 0; i < combinations.size(); i++) {
    std::string combo_str = as<std::string>(combinations[i]);
    int before = store->count;
    analyze_single_combo_to_store(store.get(), combo_str, start, k,
                                   i + 1, cycle_val); // 1-based combo_number
    total_added += (store->count - before);
  }

  return total_added;
}
