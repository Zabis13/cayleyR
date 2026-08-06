#ifndef PERM_GROUP_H
#define PERM_GROUP_H

#include <vector>
#include <string>
#include <algorithm>
#include <stdexcept>
#include "cayley_utils.h"

// A permutation group as the core sees it: a fixed state length, a named
// alphabet of moves, and a way to apply one. Nothing above this file knows
// whether a move is a ring shift, a face turn, or something a future puzzle
// invents -- the contract is apply/compose/inverse/identity and no more.
//
// Concrete groups differ only in how apply() is realised. TopSpin keeps its
// direct shift/reverse loops, since L and R touch every element and a table
// would only add an indirection; Cube3 and anything declared from R carry a
// table of permutations. Both satisfy the same contract, so BFS, StateStore
// and the iterative solver call through the base pointer and never branch on
// which puzzle they are solving.
//
// Moves are addressed by index into the alphabet, 0-based. The name is for
// R's benefit; the core moves indices around.
class PermGroup {
public:
  virtual ~PermGroup() {}

  // ---- Contract ----

  // Length of a state vector. Every state this group touches has this size.
  virtual int state_length() const = 0;

  // Number of moves in the alphabet.
  virtual int n_moves() const = 0;

  // Name of move m, as R spells it ("L"/"R"/"X", "U"/"U'"/"R2", ...).
  virtual const std::string& move_name(int m) const = 0;

  // Apply move m to state in place. The one operation the core actually needs
  // in its hot loops.
  virtual void apply(std::vector<int>& state, int m) const = 0;

  // Index of the move undoing m, or -1 when the alphabet has no such move
  // (a group may be given generators whose inverses it does not name).
  virtual int inverse_move(int m) const = 0;

  // ---- Derived, uniform across groups ----

  // The state every group counts as solved: 1..n.
  std::vector<int> identity() const {
    std::vector<int> s(state_length());
    for (int i = 0; i < (int)s.size(); i++) s[i] = i + 1;
    return s;
  }

  bool is_identity(const std::vector<int>& state) const {
    int n = state_length();
    if ((int)state.size() != n) return false;
    for (int i = 0; i < n; i++) if (state[i] != i + 1) return false;
    return true;
  }

  // Apply a whole word, left to right.
  void apply_seq(std::vector<int>& state, const std::vector<int>& word) const {
    for (size_t i = 0; i < word.size(); i++) apply(state, word[i]);
  }

  // Multiply a word out into a single permutation, so repeating it costs one
  // subscript per repetition instead of one apply per move. Returned in the
  // same convention as apply(): new[i] = state[perm[i]].
  std::vector<int> compose(const std::vector<int>& word) const {
    std::vector<int> perm = identity();
    apply_seq(perm, word);
    for (size_t i = 0; i < perm.size(); i++) perm[i] -= 1;  // to 0-based
    return perm;
  }

  // The word undoing `word`: reversed, each move replaced by its inverse.
  // Throws if any move in the word has no named inverse.
  std::vector<int> inverse_seq(const std::vector<int>& word) const {
    std::vector<int> out;
    out.reserve(word.size());
    for (size_t i = word.size(); i-- > 0; ) {
      int inv = inverse_move(word[i]);
      if (inv < 0) {
        throw std::runtime_error("inverse_seq: move '" + move_name(word[i]) +
                                 "' has no inverse in this alphabet");
      }
      out.push_back(inv);
    }
    return out;
  }

  // Index of a move by name, or -1. Linear, but alphabets are tiny and this
  // is only ever called while translating R arguments.
  int move_index(const std::string& name) const {
    for (int m = 0; m < n_moves(); m++) if (move_name(m) == name) return m;
    return -1;
  }
};

// ---- Table-backed group ------------------------------------------------
//
// The general case: moves given as explicit permutations, perms[m][i] read as
// new[i] = state[perms[m][i]] with perms 0-based inside. This is what a group
// declared from R becomes, the 3x3x3 cube included -- its 18 face turns are
// already written as permutations of 54 stickers on the R side, so they cross
// into C++ unchanged and there is no second copy to keep in step.

class TablePermGroup : public PermGroup {
public:
  int n;
  std::vector<std::string> names;
  std::vector<std::vector<int> > perms;   // 0-based
  std::vector<int> inv_of;                // -1 where unknown

  TablePermGroup(int state_len,
                 const std::vector<std::string>& move_names,
                 const std::vector<std::vector<int> >& move_perms)
    : n(state_len), names(move_names), perms(move_perms)
  {
    for (size_t m = 0; m < perms.size(); m++) {
      if ((int)perms[m].size() != n) {
        throw std::runtime_error("permutation for move '" + names[m] +
                                 "' has wrong length");
      }
      std::vector<bool> seen(n, false);
      for (int i = 0; i < n; i++) {
        int v = perms[m][i];
        if (v < 0 || v >= n || seen[v]) {
          throw std::runtime_error("move '" + names[m] +
                                   "' is not a permutation of 1.." +
                                   std::to_string(n));
        }
        seen[v] = true;
      }
    }
    build_inverses();
  }

  int state_length() const { return n; }
  int n_moves() const { return (int)perms.size(); }
  const std::string& move_name(int m) const { return names[m]; }

  // The scratch buffer is a local rather than a member: apply() is called from
  // OpenMP parallel regions (the sparse BFS look-ahead), and a shared mutable
  // buffer would be a data race there. Groups must be safe to apply from
  // several threads at once, so they carry no per-call state.
  void apply(std::vector<int>& state, int m) const {
    const std::vector<int>& p = perms[m];
    std::vector<int> buf(n);
    for (int i = 0; i < n; i++) buf[i] = state[p[i]];
    state.swap(buf);
  }

  int inverse_move(int m) const { return inv_of[m]; }

private:

  // A move's inverse is the move whose permutation undoes it. Found by
  // composing rather than by naming convention, so a group declared from R
  // gets its inverses for free without having to say which is which.
  void build_inverses() {
    int nm = (int)perms.size();
    inv_of.assign(nm, -1);
    for (int a = 0; a < nm; a++) {
      for (int b = 0; b < nm; b++) {
        bool id = true;
        for (int i = 0; i < n && id; i++) {
          if (perms[b][perms[a][i]] != i) id = false;
        }
        if (id) { inv_of[a] = b; break; }
      }
    }
  }
};

// ---- TopSpin group -----------------------------------------------------
//
// L and R rotate the whole ring by one, X reverses the first k elements. Kept
// as direct loops rather than a table: this is the group the package was built
// on, and its behaviour here is exactly what apply_op_code_inplace() has always
// done, so existing results do not move.
//
// The alphabet is a subset of L, R, X in that order; k is baked in at
// construction and never travels through the core again.

class TopSpinGroup : public PermGroup {
public:
  int n;
  int k;
  std::vector<std::string> names;   // as R spelled them
  std::vector<int> codes;           // 1 = L, 2 = R, 3 = X

  TopSpinGroup(int state_len, int k_val,
               const std::vector<std::string>& move_names)
    : n(state_len), k(k_val), names(move_names)
  {
    codes.reserve(names.size());
    for (size_t i = 0; i < names.size(); i++) {
      const std::string& s = names[i];
      if (s == "L" || s == "1") codes.push_back(1);
      else if (s == "R" || s == "2") codes.push_back(2);
      else if (s == "X" || s == "3") codes.push_back(3);
      else throw std::runtime_error("Unknown operation '" + s +
                                    "' (expected L/R/X or 1/2/3)");
    }
  }

  int state_length() const { return n; }
  int n_moves() const { return (int)codes.size(); }
  const std::string& move_name(int m) const { return names[m]; }

  void apply(std::vector<int>& state, int m) const {
    apply_op_code_inplace(state, codes[m], k);
  }

  // L and R undo each other; X is its own inverse. Reported only when the
  // partner is actually in the alphabet.
  int inverse_move(int m) const {
    int want = codes[m] == 1 ? 2 : (codes[m] == 2 ? 1 : 3);
    for (int i = 0; i < (int)codes.size(); i++) if (codes[i] == want) return i;
    return -1;
  }
};

#endif // PERM_GROUP_H
