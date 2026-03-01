#ifndef STATE_STORE_H
#define STATE_STORE_H

#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <cmath>
#include <algorithm>
#include "cayley_utils.h"

// Operation codes (compact int representation)
enum OpCode : int {
  OP_NA = 0,
  OP_L  = 1,  // shift left  ("1" / "L")
  OP_R  = 2,  // shift right ("2" / "R")
  OP_X  = 3   // reverse     ("3" / "X")
};

inline OpCode op_from_string(const std::string& s) {
  if (s == "L" || s == "1") return OP_L;
  if (s == "R" || s == "2") return OP_R;
  if (s == "X" || s == "3") return OP_X;
  return OP_NA;
}

inline std::string op_to_string(OpCode op) {
  switch (op) {
    case OP_L: return "1";
    case OP_R: return "2";
    case OP_X: return "3";
    default:   return "";
  }
}

class StateStore {
public:
  int L;         // permutation length (number of elements per state)
  int count;     // current number of states
  int capacity;  // allocated slots

  // Flat state storage: states[i*L .. i*L+L-1] = state i
  std::vector<int> states;

  // Metadata vectors (length = count)
  std::vector<int> step;
  std::vector<int> combo_number;
  std::vector<int> cycle;
  std::vector<OpCode> operation;

  // Celestial coordinates
  std::vector<int> nL_vec, nR_vec, nX_vec;
  std::vector<double> theta_vec, phi_vec, omega_vec;

  // Hash index: state_key -> vector of row indices
  std::unordered_map<std::string, std::vector<int>> hash_index;

  // Cycle index: cycle_val -> vector of row indices
  // Built on demand via build_cycle_index(), NOT maintained in add_state/add_batch
  mutable std::unordered_map<int, std::vector<int>> cycle_index;
  mutable int cycle_index_built_up_to = 0; // count at last build (0 = never built)

  // OPD filter: cycle_val -> allowed combo_numbers (empty = no filter)
  std::unordered_map<int, std::unordered_set<int>> opd_combos;

  // ---- Construction ----

  explicit StateStore(int perm_length, int init_capacity = 10000)
    : L(perm_length), count(0), capacity(init_capacity)
  {
    reserve(capacity);
  }

  // ---- Capacity management ----

  void reserve(int new_cap) {
    if (new_cap <= capacity && (int)states.size() >= new_cap * L) return;
    capacity = new_cap;
    states.reserve(capacity * L);
    step.reserve(capacity);
    combo_number.reserve(capacity);
    cycle.reserve(capacity);
    operation.reserve(capacity);
    nL_vec.reserve(capacity);
    nR_vec.reserve(capacity);
    nX_vec.reserve(capacity);
    theta_vec.reserve(capacity);
    phi_vec.reserve(capacity);
    omega_vec.reserve(capacity);
  }

  void ensure_capacity(int n_new) {
    int needed = count + n_new;
    if (needed > capacity) {
      int new_cap = capacity;
      while (new_cap < needed) new_cap *= 2;
      reserve(new_cap);
    }
  }

  // ---- Single state add (used internally) ----

  int add_state(const int* state_data,
                int step_val, int combo_val, int cycle_val, OpCode op_val,
                int nL_val, int nR_val, int nX_val,
                double theta_val, double phi_val, double omega_val) {
    ensure_capacity(1);
    int idx = count;

    states.insert(states.end(), state_data, state_data + L);
    step.push_back(step_val);
    combo_number.push_back(combo_val);
    cycle.push_back(cycle_val);
    operation.push_back(op_val);
    nL_vec.push_back(nL_val);
    nR_vec.push_back(nR_val);
    nX_vec.push_back(nX_val);
    theta_vec.push_back(theta_val);
    phi_vec.push_back(phi_val);
    omega_vec.push_back(omega_val);

    // Update hash index
    std::string key = state_to_key_raw(state_data, L);
    hash_index[key].push_back(idx);

    count++;
    return idx;
  }

  // ---- Batch add from R IntegerMatrix + meta ----
  // states_mat: n_rows x L (column-major from R!)
  // Returns number of states added

  int add_batch(const int* mat_data, int n_rows, bool col_major,
                const int* step_data,
                const int* combo_data,
                int cycle_val,
                const int* op_data,
                const int* nL_data, const int* nR_data, const int* nX_data,
                const double* theta_data, const double* phi_data, const double* omega_data) {
    ensure_capacity(n_rows);
    int start_idx = count;

    for (int i = 0; i < n_rows; i++) {
      // Extract state row
      int offset = count * L;
      states.resize(offset + L);
      if (col_major) {
        // R IntegerMatrix is column-major: mat[i, j] = mat_data[i + j*n_rows]
        for (int j = 0; j < L; j++) {
          states[offset + j] = mat_data[i + j * n_rows];
        }
      } else {
        for (int j = 0; j < L; j++) {
          states[offset + j] = mat_data[i * L + j];
        }
      }

      step.push_back(step_data ? step_data[i] : NA_INTEGER);
      combo_number.push_back(combo_data ? combo_data[i] : 0);
      cycle.push_back(cycle_val);
      operation.push_back(op_data ? static_cast<OpCode>(op_data[i]) : OP_NA);
      nL_vec.push_back(nL_data ? nL_data[i] : 0);
      nR_vec.push_back(nR_data ? nR_data[i] : 0);
      nX_vec.push_back(nX_data ? nX_data[i] : 0);
      theta_vec.push_back(theta_data ? theta_data[i] : 0.0);
      phi_vec.push_back(phi_data ? phi_data[i] : 0.0);
      omega_vec.push_back(omega_data ? omega_data[i] : 0.0);

      // Hash
      std::string key = state_to_key_raw(&states[offset], L);
      hash_index[key].push_back(count);

      count++;
    }

    return n_rows;
  }

  // ---- Lookup ----

  const std::vector<int>* lookup(const std::string& key) const {
    auto it = hash_index.find(key);
    if (it == hash_index.end()) return nullptr;
    return &(it->second);
  }

  // Get state at index as pointer
  const int* get_state_ptr(int idx) const {
    return &states[idx * L];
  }

  // ---- Intersections ----
  // Returns keys present in both stores
  std::vector<std::string> find_intersection_keys(const StateStore& other) const {
    std::vector<std::string> result;
    // Iterate over the smaller hash
    const auto& smaller = (hash_index.size() <= other.hash_index.size())
                          ? hash_index : other.hash_index;
    const auto& larger  = (hash_index.size() <= other.hash_index.size())
                          ? other.hash_index : hash_index;

    for (const auto& kv : smaller) {
      if (larger.count(kv.first)) {
        result.push_back(kv.first);
      }
    }
    return result;
  }

  // ---- Unique keys count ----
  int unique_key_count() const {
    return (int)hash_index.size();
  }

  // ---- Distance computation ----
  // Manhattan distance from target to state at idx
  int manhattan_distance(const int* target, int idx) const {
    const int* s = get_state_ptr(idx);
    int dist = 0;
    for (int j = 0; j < L; j++) {
      dist += std::abs(target[j] - s[j]);
    }
    return dist;
  }

  // Find index of state closest to target (manhattan), among indices in 'candidates'
  // If candidates is empty, searches all states
  int find_best_match_manhattan(const int* target,
                                const std::vector<int>& candidates) const {
    int best_idx = -1;
    int best_dist = INT_MAX;
    int best_step = INT_MAX;

    auto check = [&](int idx) {
      int d = manhattan_distance(target, idx);
      if (d < best_dist || (d == best_dist && step[idx] < best_step)) {
        best_dist = d;
        best_step = step[idx];
        best_idx = idx;
      }
    };

    if (candidates.empty()) {
      for (int i = 0; i < count; i++) check(i);
    } else {
      for (int idx : candidates) check(idx);
    }
    return best_idx;
  }

  // ---- OPD: set allowed combos for a cycle ----
  void set_opd_combos(int target_cycle, const std::vector<int>& combos) {
    if (combos.empty()) {
      opd_combos.erase(target_cycle);
    } else {
      opd_combos[target_cycle] = std::unordered_set<int>(combos.begin(), combos.end());
    }
  }

  // ---- OPD: clear all filters ----
  void clear_opd() {
    opd_combos.clear();
  }

  // ---- Check if index passes OPD filter ----
  bool passes_opd(int idx, int target_cycle) const {
    auto opd_it = opd_combos.find(target_cycle);
    if (opd_it == opd_combos.end()) return true; // no filter
    return opd_it->second.count(combo_number[idx]) > 0;
  }

  // ---- Build cycle index on demand (NOT called from add_state/add_batch) ----
  void build_cycle_index() const {
    if (cycle_index_built_up_to == count) return; // already up to date

    // Incremental: only process new states since last build
    for (int i = cycle_index_built_up_to; i < count; i++) {
      cycle_index[cycle[i]].push_back(i);
    }
    cycle_index_built_up_to = count;
  }

  // ---- Filter: get indices for a cycle, skipping first/last steps per combo ----
  std::vector<int> filter_middle_indices(int target_cycle,
                                          int skip_first, int skip_last) const {
    build_cycle_index();
    auto cycle_it = cycle_index.find(target_cycle);
    if (cycle_it == cycle_index.end()) return std::vector<int>();
    const auto& cyc_indices = cycle_it->second;

    // First pass: find max step per combo in this cycle (respecting OPD)
    std::unordered_map<int, int> combo_max_step;
    for (int i : cyc_indices) {
      if (step[i] == NA_INTEGER) continue;
      if (!passes_opd(i, target_cycle)) continue;
      auto it = combo_max_step.find(combo_number[i]);
      if (it == combo_max_step.end() || step[i] > it->second) {
        combo_max_step[combo_number[i]] = step[i];
      }
    }

    // Second pass: filter
    std::vector<int> result;
    for (int i : cyc_indices) {
      if (step[i] == NA_INTEGER) continue;
      if (!passes_opd(i, target_cycle)) continue;
      auto it = combo_max_step.find(combo_number[i]);
      if (it == combo_max_step.end()) continue;
      if (step[i] > skip_first && step[i] <= (it->second - skip_last)) {
        result.push_back(i);
      }
    }
    return result;
  }

  // ---- Indices for a given cycle (O(1) via cycle_index, with OPD filter) ----
  std::vector<int> indices_for_cycle(int target_cycle) const {
    build_cycle_index();
    auto it = cycle_index.find(target_cycle);
    if (it == cycle_index.end()) return std::vector<int>();

    // Check if OPD filter active for this cycle
    auto opd_it = opd_combos.find(target_cycle);
    if (opd_it == opd_combos.end()) return it->second; // no filter, fast path

    // Filter by allowed combos
    const auto& allowed = opd_it->second;
    std::vector<int> result;
    result.reserve(it->second.size());
    for (int idx : it->second) {
      if (allowed.count(combo_number[idx]) > 0) {
        result.push_back(idx);
      }
    }
    return result;
  }

  // ---- Helper: state_to_key from raw pointer ----
  static std::string state_to_key_raw(const int* data, int len) {
    std::string key;
    key.reserve(len * 4);
    for (int i = 0; i < len; i++) {
      if (i > 0) key += '_';
      key += std::to_string(data[i]);
    }
    return key;
  }
};

#endif // STATE_STORE_H
