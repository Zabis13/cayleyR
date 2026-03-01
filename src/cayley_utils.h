#ifndef CAYLEY_UTILS_H
#define CAYLEY_UTILS_H

#include <vector>
#include <string>
#include <algorithm>

// Apply a single operation to a state vector in-place
inline void apply_op_inplace(std::vector<int>& state, const std::string& op, int k) {
  int n = (int)state.size();
  if (n == 0) return;

  if (op == "L" || op == "1") {
    int first = state[0];
    for (int i = 0; i < n - 1; i++) state[i] = state[i + 1];
    state[n - 1] = first;
  } else if (op == "R" || op == "2") {
    int last = state[n - 1];
    for (int i = n - 1; i > 0; i--) state[i] = state[i - 1];
    state[0] = last;
  } else if (op == "X" || op == "3") {
    int end = std::min(k, n);
    for (int i = 0; i < end / 2; i++) {
      std::swap(state[i], state[end - 1 - i]);
    }
  }
}

// Hash a state vector into a string for set lookup
inline std::string state_to_key(const std::vector<int>& state) {
  std::string key;
  key.reserve(state.size() * 4);
  for (size_t i = 0; i < state.size(); i++) {
    if (i > 0) key += '_';
    key += std::to_string(state[i]);
  }
  return key;
}

#endif // CAYLEY_UTILS_H
