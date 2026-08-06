#include <Rcpp.h>
#include "state_store.h"
#include "celestial_coords.h"

using namespace Rcpp;

// ============================================================
// XPtr-based API for StateStore
// ============================================================

typedef XPtr<StateStore> StateStorePtr;

// The group a store belongs to, borrowed from the R-side XPtr. The deleter is
// a no-op: the group is owned by that XPtr, and R keeps it alive because the
// store wrapper holds a reference to the group object. Copying the group here
// instead would mean two answers to "what does move 2 do" once a caller starts
// asking the group directly.
static std::shared_ptr<PermGroup> borrow_group(SEXP group) {
  if (Rf_isNull(group)) return std::shared_ptr<PermGroup>();
  XPtr<PermGroup> g(group);
  return std::shared_ptr<PermGroup>(g.get(), [](PermGroup*) {});
}

// [[Rcpp::export]]
SEXP state_store_create(int perm_length, int init_capacity = 10000,
                        SEXP group = R_NilValue) {
  StateStore* store = new StateStore(perm_length, init_capacity);
  store->group = borrow_group(group);
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
    Named("operation") = store->op_name(store->operation[idx]),
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

// Extract many states at once as a matrix (one row per index).
// Pulling candidates one at a time from R is too slow when a custom distance
// method has to score every candidate.
// [[Rcpp::export]]
IntegerMatrix state_store_get_states(SEXP xp, IntegerVector indices) {
  StateStorePtr store(xp);
  const int L = store->L;
  const int m = indices.size();
  IntegerMatrix out(m, L);

  for (int i = 0; i < m; i++) {
    int idx = indices[i];
    if (idx < 0 || idx >= store->count) {
      stop("state_store_get_states: index %d out of range", idx);
    }
    const int* s = store->get_state_ptr(idx);
    for (int j = 0; j < L; j++) out(i, j) = s[j];
  }
  return out;
}

// Score a state the way a person solves: how much of the sorted run is still
// missing. Ties within a run length break on the gap phase 1 is working to
// close -- among states with an equal run, the one closest to placing its next
// value scores lower. The tie-break is scaled below 1 so it can never outweigh
// a longer run.
//
static double human_score_core(const std::vector<int>& st, int k) {
  const int n = (int)st.size();
  const int r = run_length_of(st);
  if (r >= n) return 0.0;

  // Position of the pair phase 1 would work on next: prev = r, m = r + 1.
  int p_prev = -1, p_m = -1;
  for (int i = 0; i < n; i++) {
    if (st[i] == r) p_prev = i;
    else if (st[i] == r + 1) p_m = i;
  }

  // Best gap reachable by one phase 1 move, not the raw gap as it stands.
  // Phase 1 never judges a state by where m happens to sit: it judges it by
  // how close one legal flip can bring m to distance k behind prev. Mirrors
  // the window scan in place_value() -- windows overlapping the finished run
  // are skipped, since phase 1 may not disturb it.
  int best_gap = n;  // sentinel: no legal window
  if (p_prev >= 0 && p_m >= 0) {
    const int g0 = fwd_gap_of(p_prev, p_m, n);
    best_gap = (g0 == 1) ? 0 : std::abs(g0 - k);

    for (int off = 0; off < k; off++) {
      const int start = ((p_m - off) % n + n) % n;

      bool hits = false;
      for (int i = 0; i < k && !hits; i++) {
        if (fwd_gap_of((start + i) % n, p_prev, n) < r) hits = true;
      }
      if (hits) continue;

      const int newp = (start + (k - 1 - off)) % n;
      const int ng = fwd_gap_of(p_prev, newp, n);
      if (ng == 0) continue;

      const int sc = (ng == 1) ? 0 : std::abs(ng - k);
      if (sc < best_gap) best_gap = sc;
    }
  }

  // Tie-break only: scaled strictly below 1 so a longer run always wins.
  if (best_gap > n - 1) best_gap = n - 1;
  return (double)(n - r) + (double)best_gap / (double)n;
}

// Scoring is against the identity 1:n, deliberately: run_length only means
// anything when the goal is the sorted ring. Relabelling into an arbitrary
// target's frame was tried and makes the metric worse -- the side of a
// two-ended search that grows from an unstructured state gets judged against a
// goal phase 1 cannot read, and the score degenerates into noise. So this
// method is for searches heading for 1:n, which is what the tail search after
// phase 1 does; `target` is accepted for interface uniformity and ignored.
static double human_score_towards(const std::vector<int>& st,
                                  const std::vector<int>& /*target*/, int k) {
  return human_score_core(st, k);
}

// [[Rcpp::export]]
NumericVector human_distance_cpp(IntegerMatrix states, IntegerVector target,
                                 int k) {
  const int m = states.nrow(), L = states.ncol();
  if (target.size() != L) {
    stop("human_distance_cpp: target length %d != state length %d",
         target.size(), L);
  }
  const std::vector<int> tgt(target.begin(), target.end());

  NumericVector out(m);
  std::vector<int> buf(L);

  for (int i = 0; i < m; i++) {
    for (int j = 0; j < L; j++) buf[j] = states(i, j);
    out[i] = human_score_towards(buf, tgt, k);
  }
  return out;
}

// Score every candidate in the store directly, without shipping states to R.
// [[Rcpp::export]]
NumericVector state_store_human_scores(SEXP xp, IntegerVector candidate_indices,
                                       IntegerVector target, int k) {
  StateStorePtr store(xp);
  const int L = store->L;
  if (target.size() != L) {
    stop("state_store_human_scores: target length %d != perm_length %d",
         target.size(), L);
  }
  const std::vector<int> tgt(target.begin(), target.end());

  const int m = candidate_indices.size();
  NumericVector out(m);
  std::vector<int> buf(L);

  for (int i = 0; i < m; i++) {
    const int idx = candidate_indices[i];
    if (idx < 0 || idx >= store->count) {
      stop("state_store_human_scores: index %d out of range", idx);
    }
    const int* s = store->get_state_ptr(idx);
    for (int j = 0; j < L; j++) buf[j] = s[j];
    out[i] = human_score_towards(buf, tgt, k);
  }
  return out;
}

// Pick the candidate with the lowest caller-supplied score.
// Lets R plug in any distance method without reimplementing it in C++.
// [[Rcpp::export]]
int state_store_find_best_match_scored(SEXP xp, IntegerVector candidate_indices,
                                       NumericVector scores) {
  StateStorePtr store(xp);
  if (candidate_indices.size() != scores.size()) {
    stop("state_store_find_best_match_scored: %d candidates but %d scores",
         candidate_indices.size(), scores.size());
  }
  std::vector<int> candidates(candidate_indices.begin(), candidate_indices.end());
  return store->find_best_match_scored(candidates, scores.begin());
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

// Set OPD combo filter for a cycle
// combos: integer vector of allowed combo_numbers (empty = clear filter)
// [[Rcpp::export]]
void state_store_set_opd(SEXP xp, int target_cycle, IntegerVector combos) {
  StateStorePtr store(xp);
  std::vector<int> combo_vec(combos.begin(), combos.end());
  store->set_opd_combos(target_cycle, combo_vec);
}

// Clear all OPD filters
// [[Rcpp::export]]
void state_store_clear_opd(SEXP xp) {
  StateStorePtr store(xp);
  store->clear_opd();
}

// Drop all states and indices, keeping allocated capacity
// [[Rcpp::export]]
void state_store_clear(SEXP xp) {
  StateStorePtr store(xp);
  store->clear();
}

// Find combo_numbers that contain a given state in a given cycle
// [[Rcpp::export]]
IntegerVector state_store_combos_for_state(SEXP xp, IntegerVector state_vec, int target_cycle) {
  StateStorePtr store(xp);
  std::string key = StateStore::state_to_key_raw(state_vec.begin(), state_vec.size());
  const auto* indices = store->lookup(key);
  if (!indices) return IntegerVector(0);

  std::unordered_set<int> combos;
  for (int idx : *indices) {
    if (store->cycle[idx] == target_cycle) {
      combos.insert(store->combo_number[idx]);
    }
  }
  return IntegerVector(combos.begin(), combos.end());
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
    std::string s = store->op_name(store->operation[i]);
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

// Helper: collect operations from a combo in a cycle, from step 1 up to (not including) end_step.
// Uses raw cycle_index (bypasses OPD) to ensure all combo rows are visible.
static void collect_combo_ops(const StateStore* store, int cyc, int combo, int end_step,
                               std::vector<std::string>& out) {
  // Ensure cycle index is built, then get ALL indices (bypass OPD filter)
  store->build_cycle_index();
  auto cycle_it = store->cycle_index.find(cyc);
  if (cycle_it == store->cycle_index.end()) return;
  const auto& cyc_indices = cycle_it->second;

  std::vector<std::pair<int, int>> combo_rows; // (step, idx)
  for (int idx : cyc_indices) {
    if (store->combo_number[idx] == combo && store->step[idx] != NA_INTEGER) {
      combo_rows.push_back({store->step[idx], idx});
    }
  }
  std::sort(combo_rows.begin(), combo_rows.end());

  for (auto& p : combo_rows) {
    if (p.first >= end_step) break;
    std::string op = store->op_name(store->operation[p.second]);
    if (!op.empty()) {
      out.push_back(op);
    }
  }
}

// Collect the operations leading to a state within one cycle+combo.
// Lets the caller keep the path segment to a bridge before the cycle's states
// are dropped, so reconstruction no longer needs earlier cycles in the store.
// Returns an empty vector when the state is the combo's start (step == NA).
// [[Rcpp::export]]
CharacterVector state_store_collect_ops(SEXP xp, int target_cycle,
                                        int target_combo, int end_step) {
  StateStorePtr store(xp);
  std::vector<std::string> ops;
  if (end_step != NA_INTEGER) {
    collect_combo_ops(store.get(), target_cycle, target_combo, end_step, ops);
  }
  CharacterVector result(ops.size());
  for (size_t i = 0; i < ops.size(); i++) result[i] = ops[i];
  return result;
}

// Reconstruct path from store using bridge state chain.
// bridge_states_mat: matrix (n_bridges x L), row 0 = root (cycle 0),
//   row i = bridge chosen at cycle i.
// target_state_vec: the intersection state to reach
// target_cycle: cycle where target was found
// target_combo: combo_number of target in target_cycle
//
// Path logic for each cycle 1..target_cycle:
//   - The "start state" of cycle C is bridge_states[C-1] (bridge from previous cycle, or root)
//   - Find bridge_states[C-1] in cycle C (it's the state with step==1 for some combo,
//     since analyze_combos starts from that state)
//   - For cycles < target_cycle: find bridge_states[C] in cycle C, collect ops to reach it
//   - For target_cycle: find target_state, collect ops to reach it
// [[Rcpp::export]]
Nullable<CharacterVector> state_store_reconstruct_path(
    SEXP xp,
    IntegerMatrix bridge_states_mat,
    IntegerVector target_state_vec,
    int target_cycle,
    int target_combo) {

  StateStorePtr store(xp);
  int L = store->L;

  if (target_cycle == 0) {
    return CharacterVector(0);
  }

  int n_bridges = bridge_states_mat.nrow(); // row 0=root(cycle0), row 1=bridge(cycle1), ...

  std::vector<int> target_state(target_state_vec.begin(), target_state_vec.end());
  std::vector<std::string> full_path;

  for (int cyc = 1; cyc <= target_cycle; cyc++) {
    // The state that was used as start_state for analyze_combos in this cycle
    // = bridge from cycle (cyc-1), stored in bridge_states_mat row (cyc-1)
    // For cyc=1, this is the root state (row 0)
    int bridge_row = cyc - 1;
    if (bridge_row >= n_bridges) {
      return R_NilValue; // not enough bridge states
    }

    // What state do we need to reach in this cycle?
    if (cyc < target_cycle) {
      // Need to reach bridge_states[cyc] (the bridge chosen at end of this cycle)
      int next_bridge_row = cyc;
      if (next_bridge_row >= n_bridges) {
        return R_NilValue;
      }

      // Build key for the target bridge in this cycle
      std::vector<int> bridge_target(L);
      for (int j = 0; j < L; j++) {
        bridge_target[j] = bridge_states_mat(next_bridge_row, j);
      }
      std::string bridge_key = StateStore::state_to_key_raw(bridge_target.data(), L);

      // Find this bridge state in this cycle via hash lookup
      const auto* key_indices = store->lookup(bridge_key);
      if (!key_indices) return R_NilValue;

      int match_idx = -1;
      for (int idx : *key_indices) {
        if (store->cycle[idx] == cyc) {
          match_idx = idx;
          break;
        }
      }
      if (match_idx == -1) return R_NilValue;

      int match_combo = store->combo_number[match_idx];
      int match_step = store->step[match_idx];

      // Collect ops from this combo up to match_step
      if (match_step != NA_INTEGER) {
        collect_combo_ops(store.get(), cyc, match_combo, match_step, full_path);
      }
      // If match_step == NA_INTEGER, bridge is the initial/final state of combo — 0 ops needed

    } else {
      // cyc == target_cycle: reach the target_state in target_combo
      std::string target_key = StateStore::state_to_key_raw(target_state.data(), L);

      // Find target in this cycle+combo via hash
      const auto* key_indices = store->lookup(target_key);
      if (!key_indices) return R_NilValue;

      int target_idx = -1;
      for (int idx : *key_indices) {
        if (store->cycle[idx] == cyc && store->combo_number[idx] == target_combo) {
          target_idx = idx;
          break;
        }
      }
      if (target_idx == -1) return R_NilValue;

      int target_step_val = store->step[target_idx];
      if (target_step_val != NA_INTEGER) {
        collect_combo_ops(store.get(), cyc, target_combo, target_step_val, full_path);
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

// Result buffer for one combo (computed in parallel, merged sequentially)
struct ComboResult {
  std::vector<std::vector<int>> all_states;
  std::vector<CelestialCoords> all_coords;
  std::vector<OpCode> all_ops;
};

// Analyze a single combo into a local buffer (thread-safe, no shared state).
//
// A combo is a word given as move indices into the group's alphabet, spun
// round until the state returns to where it started. Words used to arrive as
// strings parsed one character per op, which only ever worked because TopSpin
// spells its three moves with single characters; indices carry a cube's "R'"
// and "U2" just as well.
//
// The nL/nR/nX counters and the celestial coordinates built from them are
// TopSpin's. For any other alphabet `lrx_codes` is empty and they stay flat,
// which is what leaves those columns meaningless rather than wrong for a cube.
static ComboResult analyze_single_combo(
    const std::vector<int>& word,
    const std::vector<int>& start_state,
    const PermGroup& g,
    const std::vector<int>& lrx_codes,
    int max_reps)
{
  ComboResult result;
  std::vector<int> current = start_state;
  CelestialCoords coords = create_empty_coords();

  result.all_states.push_back(start_state);
  result.all_coords.push_back(create_empty_coords());

  if (word.empty()) return result;

  int step = 0;
  bool done = false;

  for (int rep = 0; rep < max_reps && !done; rep++) {
    for (size_t oi = 0; oi < word.size(); oi++) {
      int m = word[oi];

      g.apply(current, m);

      int code = lrx_codes.empty() ? 0 : lrx_codes[m];
      coords = update_coords(coords, code == 1 ? 1 : 0,
                             code == 2 ? 1 : 0, code == 3 ? 1 : 0);

      step++;
      result.all_states.push_back(current);
      result.all_coords.push_back(coords);
      result.all_ops.push_back(static_cast<OpCode>(m + 1));

      if (current == start_state && step > 0) {
        done = true;
        break;
      }
    }
  }

  return result;
}

// Merge a ComboResult into the store (sequential, not thread-safe)
static void merge_combo_to_store(
    StateStore* store,
    const ComboResult& result,
    int combo_number,
    int cycle_val)
{
  int n_states = (int)result.all_states.size();
  store->ensure_capacity(n_states);

  for (int i = 0; i < n_states; i++) {
    int step_val = (i < n_states - 1) ? (i + 1) : NA_INTEGER;
    OpCode op_val = (i < n_states - 1) ? result.all_ops[i] : OP_NA;
    const CelestialCoords& c = result.all_coords[i];

    store->add_state(result.all_states[i].data(),
                     step_val, combo_number, cycle_val, op_val,
                     c.nL, c.nR, c.nX,
                     c.theta, c.phi, c.omega_conformal);
  }
}

// Combos arrive as a list of integer words, one per combo, each entry a
// 1-based index into the group's alphabet. `max_reps` caps how many times a
// word may be spun before the cycle is abandoned as non-closing.
//
// [[Rcpp::export]]
int analyze_combos_to_store_cpp(SEXP xp,
                                 List combinations,
                                 IntegerVector start_state,
                                 SEXP group,
                                 int cycle_val,
                                 int max_reps = 100000) {
  StateStorePtr store(xp);
  XPtr<PermGroup> g(group);

  std::vector<int> start(start_state.begin(), start_state.end());
  int n_combos = combinations.size();
  int initial_count = store->count;

  // Pre-extract words as plain vectors: R objects are not safe to touch from
  // an OpenMP region.
  std::vector<std::vector<int> > words(n_combos);
  for (int i = 0; i < n_combos; i++) {
    IntegerVector w = combinations[i];
    words[i].reserve(w.size());
    for (int j = 0; j < w.size(); j++) {
      int m = w[j] - 1;
      if (m < 0 || m >= g->n_moves()) stop("move index %d out of range", w[j]);
      words[i].push_back(m);
    }
  }

  // Which alphabet slot is which of L/R/X, empty unless this is TopSpin.
  std::vector<int> lrx_codes;
  bool lrx = g->n_moves() <= 3;
  for (int m = 0; m < g->n_moves() && lrx; m++) {
    const std::string& s = g->move_name(m);
    if (s == "L" || s == "1") lrx_codes.push_back(1);
    else if (s == "R" || s == "2") lrx_codes.push_back(2);
    else if (s == "X" || s == "3") lrx_codes.push_back(3);
    else lrx = false;
  }
  if (!lrx) lrx_codes.clear();

  // Phase 1: parallel computation into local buffers
  std::vector<ComboResult> results(n_combos);

  #pragma omp parallel for schedule(dynamic)
  for (int i = 0; i < n_combos; i++) {
    results[i] = analyze_single_combo(words[i], start, *g, lrx_codes, max_reps);
  }

  // Phase 2: sequential merge into store
  for (int i = 0; i < n_combos; i++) {
    merge_combo_to_store(store.get(), results[i], i + 1, cycle_val);
  }

  return store->count - initial_count;
}
