#ifndef CAYLEY_UTILS_H
#define CAYLEY_UTILS_H

#include <vector>
#include <string>
#include <algorithm>

// Length of the run 1,2,...,r currently consecutive on the ring, starting
// wherever value 1 sits. The quantity phase 1 of the human algorithm
// maximises: each insertion move appends one value to the run.
inline int run_length_of(const std::vector<int>& st) {
  int n = (int)st.size();
  int p1 = -1;
  for (int i = 0; i < n; i++) if (st[i] == 1) { p1 = i; break; }
  if (p1 < 0) return 0;
  int r = 1;
  while (r < n && st[(p1 + r) % n] == r + 1) r++;
  return r;
}

// Forward gap from a to b around a ring of size n.
inline int fwd_gap_of(int a, int b, int n) { return ((b - a) % n + n) % n; }

// Apply a single operation to a state vector in-place
// Op applied by integer code: 1=L, 2=R, 3=X. Skips the string comparisons of
// the string overload; hot loops that apply the same words millions of times
// pre-encode their ops once and call this.
inline void apply_op_code_inplace(std::vector<int>& state, int op, int k) {
  int n = (int)state.size();
  if (n == 0) return;

  if (op == 1) {
    int first = state[0];
    for (int i = 0; i < n - 1; i++) state[i] = state[i + 1];
    state[n - 1] = first;
  } else if (op == 2) {
    int last = state[n - 1];
    for (int i = n - 1; i > 0; i--) state[i] = state[i - 1];
    state[0] = last;
  } else if (op == 3) {
    int end = std::min(k, n);
    for (int i = 0; i < end / 2; i++) {
      std::swap(state[i], state[end - 1 - i]);
    }
  }
}

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
