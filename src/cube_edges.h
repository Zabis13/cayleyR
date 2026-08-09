#ifndef CAYLEYR_CUBE_EDGES_H
#define CAYLEYR_CUBE_EDGES_H

#include <vector>
#include <string>
#include <stdexcept>
#include <algorithm>
#include "cube_centres.h"

// ---- Pairing the edges of a 4x4x4 -----------------------------------------
//
// The second stage of reduction, after cube_centres.h has built the centres.
// A 3x3x3 has twelve edges of one piece each; a 4x4x4 has twenty-four halves
// that must be joined into twelve pairs before the cube can be finished as a
// 3x3x3.
//
// ---- What the measurement says --------------------------------------------
//
// The twelve edges of the cube each hold two slots, called A and B here, and
// those twenty-four slots are what the stage moves around. Measured over the
// full alphabet:
//
//   the twelve OUTER turns carry A and B together -- an edge travels whole, so
//   they never pair anything and never break a pair. Over twenty positions the
//   count of paired edges was unchanged by every one of them.
//
//   the twelve INNER slices carry A or B alone -- 1x moves only A slots, 2x
//   only B. That mismatch is the entire mechanism of pairing, and it is also
//   why a slice on its own wrecks the centres: measured, 1x alone drops four
//   pairs and leaves the centres unsolved, while 1x and 2x together are a wide
//   turn that costs nothing.
//
// So a slice has to be wrapped: open with it, work with outer turns, close
// with its inverse. That is the same shape the l-slice stage of the centres
// used, and it is what the published algorithms below already are.
//
// ---- The rings ------------------------------------------------------------
//
// The twelve edges fall into three rings of four, one per axis, and each edge
// belongs to exactly one:
//
//   x   0-2  0-5  2-3  3-5
//   y   1-2  1-5  2-4  4-5
//   z   0-1  0-4  1-3  3-4
//
// A slice turns its own ring and nothing else. This matters because a word
// found on one ring does NOT transfer to another: a commutator found by search
// on the x ring broke the centres on all eight test positions once its slice
// was swapped for the y or z one. The published algorithms are stated against
// fixed positions for that reason, and the cube is turned to meet them.

namespace cube_solve {

// ---- The slots ------------------------------------------------------------
//
// Slot k of the table is one half of one cube edge: the two sticker positions
// it occupies, zero-based. Order is the twelve edges by face pair, A then B.
// Measured from cube_pieces(4), not derived from the numbering.
inline const int (*edge_slots())[2] {
  static const int t[24][2] = {
    { 7, 18},   // 0-1A
    {11, 17},   // 0-1B
    {13, 33},   // 0-2A
    {14, 34},   // 0-2B
    { 4, 65},   // 0-4A
    { 8, 66},   // 0-4B
    { 1, 82},   // 0-5A
    { 2, 81},   // 0-5B
    {24, 43},   // 1-2A
    {20, 39},   // 1-2B
    {30, 59},   // 1-3A
    {29, 55},   // 1-3B
    {27, 88},   // 1-5A
    {23, 84},   // 1-5B
    {45, 49},   // 2-3A
    {46, 50},   // 2-3B
    {40, 75},   // 2-4A
    {36, 71},   // 2-4B
    {56, 77},   // 3-4A
    {52, 78},   // 3-4B
    {61, 94},   // 3-5A
    {62, 93},   // 3-5B
    {72, 91},   // 4-5A
    {68, 87}    // 4-5B
  };
  return t;
}

inline int n_edge_slots() { return 24; }

// The colours a slot shows, as an ordered pair so two slots can be compared.
inline void slot_colours(const std::vector<int>& state, int slot,
                         int* lo, int* hi) {
  const int (*t)[2] = edge_slots();
  const int a = (state[t[slot][0]] - 1) / 16;
  const int b = (state[t[slot][1]] - 1) / 16;
  *lo = a < b ? a : b;
  *hi = a < b ? b : a;
}

// An edge is paired when its two halves show the same two colours.
inline bool edge_paired(const std::vector<int>& state, int edge) {
  int a1, a2, b1, b2;
  slot_colours(state, edge * 2, &a1, &a2);
  slot_colours(state, edge * 2 + 1, &b1, &b2);
  return a1 == b1 && a2 == b2;
}

inline int edges_paired(const std::vector<int>& state) {
  int c = 0;
  for (int e = 0; e < 12; e++) if (edge_paired(state, e)) c++;
  return c;
}

inline bool edges_built(const std::vector<int>& state) {
  return edges_paired(state) == 12;
}

// ---- Where an outer turn sends each slot ----------------------------------
//
// Twelve outer turns, twenty-four slots, computed once by applying each turn
// to the solved cube and seeing which slot each one's contents lands in. The
// solver reads this table; it does not search.
inline const char* const* outer_turns() {
  static const char* const t[12] = {
    "U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'"
  };
  return t;
}

inline int n_outer_turns() { return 12; }

inline const std::vector<std::vector<int> >& slot_maps() {
  static std::vector<std::vector<int> > maps;
  if (maps.empty()) {
    std::vector<int> id(96);
    for (int i = 0; i < 96; i++) id[i] = i + 1;
    const int (*t)[2] = edge_slots();

    for (int m = 0; m < n_outer_turns(); m++) {
      const std::vector<int> after =
        apply_word(id, parse_word(outer_turns()[m], 4));
      std::vector<int> map(24, -1);
      for (int i = 0; i < 24; i++) {
        // the contents of slot i before the turn, as a sorted sticker pair
        int a = id[t[i][0]], b = id[t[i][1]];
        if (a > b) std::swap(a, b);
        for (int j = 0; j < 24; j++) {
          int c = after[t[j][0]], d = after[t[j][1]];
          if (c > d) std::swap(c, d);
          if (c == a && d == b) { map[i] = j; break; }
        }
      }
      maps.push_back(map);
    }
  }
  return maps;
}

// ---- Setup words ----------------------------------------------------------
//
// Which turns bring a given slot to a given one. There is no formula to
// compute this the way the centres' setup was computed: there the four slots
// of a face cycled 1 -> 2 -> 4 -> 3 under a turn of that face, so the answer
// was a number of turns; here every turn carries a slot onto a different edge
// of the cube entirely, and the sources are an irregular mix of A and B slots.
// Measured, for the working slot 1-2B: one turn reaches it from 1-3B, 0-1A,
// 0-2A and 2-3B, and nothing regular connects those.
//
// So the words are built once by composing the measured maps -- depth one,
// then two, then three, then four -- and stored. Measured coverage from each
// working slot: 5 slots at depth 1, 15 at depth 2, 23 at depth 3, and all 24
// at depth 4. Stopping at three would leave exactly one slot unreachable, the
// one on the opposite edge.
inline const std::vector<std::vector<std::string> >& setup_words() {
  static std::vector<std::vector<std::string> > table;   // [target][from]
  if (table.empty()) {
    const std::vector<std::vector<int> >& maps = slot_maps();
    table.assign(24, std::vector<std::string>(24, "\xff"));   // unset marker

    for (int target = 0; target < 24; target++) {
      std::vector<std::string>& row = table[target];
      row[target] = "";

      // depth by depth, composing one more turn onto what is already reached
      std::vector<int> frontier;
      frontier.push_back(target);
      for (int depth = 0; depth < 4; depth++) {
        std::vector<int> next;
        for (int m = 0; m < n_outer_turns(); m++) {
          for (int from = 0; from < 24; from++) {
            const int to = maps[m][from];
            if (to < 0 || row[to] == "\xff") continue;
            if (row[from] != "\xff") continue;
            row[from] = row[to].empty()
              ? std::string(outer_turns()[m])
              : std::string(outer_turns()[m]) + " " + row[to];
            next.push_back(from);
          }
        }
        if (next.empty()) break;
      }
    }
  }
  return table;
}

// ---- The published algorithms ---------------------------------------------
//
// Slice-flip-slice, the standard method: a slice brings two halves together, a
// face turn flips one of them, the slice goes back. Every one of these was
// measured on the solved cube before use, and all six leave the centres built
// -- which is what makes them safe to run after cube_solve_centres.
//
//   word                            length  pairs broken  slots touched
//   r' F R F' r                        7         3             8
//   Dw R U R' Dw'                      7         3             8
//   Dw R F' U R' F Dw'                 9         2            10
//   Dw R U R' F R' F' R Dw'           11         2            10
//   Uw2 R U R' F R' F' R Uw2          15         2            10
//   (Rw U2) x5                        20         4             6
//
// The last one is the odd one: fewest slots, most pairs, and it is the parity
// case -- the one that swaps two halves rather than permuting whole edges.
// Written in the package alphabet, not in the notation the sources use. The
// C++ parse_word() takes faces and numbered slices only; expand_notation() in
// cube_algs.h does handle r and Dw, but it expands them into M, E and S, which
// are 3x3x3 slices and wrong here. Rather than write a second parser, each
// word below is what cube_expand_word() produced for a 4x4x4 -- one source of
// truth, translated once.
//
//   r' F R F' r                 ->  R' 2x' F R F' R 2x
//   Dw R U R' Dw'               ->  D 1y' R U R' D' 1y
//   Dw R F' U R' F Dw'          ->  D 1y' R F' U R' F D' 1y
//   Dw R U R' F R' F' R Dw'     ->  D 1y' R U R' F R' F' R D' 1y
//   Uw2 R U R' F R' F' R Uw2    ->  U 2y U 2y R U R' F R' F' R U 2y U 2y
//   (Rw U2) x5                  ->  R 2x U U ... five times
inline const char* const* edge_algs() {
  static const char* const t[6] = {
    "R' 2x' F R F' R 2x",
    "D 1y' R U R' D' 1y",
    "D 1y' R F' U R' F D' 1y",
    "D 1y' R U R' F R' F' R D' 1y",
    "U 2y U 2y R U R' F R' F' R U 2y U 2y",
    "R 2x U U R 2x U U R 2x U U R 2x U U R 2x U U"
  };
  return t;
}

inline int n_edge_algs() { return 6; }

// ---- The stage ------------------------------------------------------------
//
// Try every algorithm under every setup word, take whatever pairs the most,
// repeat. The setup words come from the table above rather than a search, and
// the algorithms are the six measured ones; what is chosen each round is the
// pair (setup, algorithm) that leaves the most edges paired without breaking a
// centre.
//
// A round that cannot improve ends the stage. Measured on fifteen positions
// with a setup pool of one and two outer turns: thirteen reached all twelve
// pairs, and the two that did not had left a cycle running through four or
// five half-edges -- the same shape a 4-cycle of centres had, and the same
// reason, that a greedy step cannot see a move which pays only on the next
// one.
inline bool pair_edges(std::vector<int>& state, Solution& sol, const Orient& o,
                       const std::string& label) {
  const std::vector<std::vector<std::string> >& setups = setup_words();

  // The setup pool: the empty word, then every word the table holds. The table
  // is built to depth four, and the length matters -- measured on the five
  // positions this stage used to stall on, every one of them needed a setup of
  // THREE outer turns before an algorithm would take it from ten pairs to
  // twelve. A pool of shorter words leaves them exactly where they were.
  std::vector<std::string> pool;
  pool.push_back("");
  for (int target = 0; target < 24; target++) {
    for (int from = 0; from < 24; from++) {
      const std::string& w = setups[target][from];
      if (w.empty() || w == "\xff") continue;
      bool seen = false;
      for (size_t q = 0; q < pool.size() && !seen; q++) if (pool[q] == w) seen = true;
      if (!seen) pool.push_back(w);
    }
  }

  // Those words all end at one of the twenty-four slots, which is not the same
  // as every word of three turns: a slot is reached by many words and the
  // table keeps one. The stalls needed words the table had discarded, so the
  // pool is completed with every composition of up to three outer turns.
  {
    const int n = n_outer_turns();
    std::vector<std::string> extra;
    for (int a = 0; a < n; a++) {
      extra.push_back(outer_turns()[a]);
      for (int b = 0; b < n; b++) {
        const std::string ab =
          std::string(outer_turns()[a]) + " " + outer_turns()[b];
        extra.push_back(ab);
        for (int c = 0; c < n; c++)
          extra.push_back(ab + " " + outer_turns()[c]);
      }
    }
    for (size_t q = 0; q < extra.size(); q++) {
      bool seen = false;
      for (size_t r = 0; r < pool.size() && !seen; r++) if (pool[r] == extra[q]) seen = true;
      if (!seen) pool.push_back(extra[q]);
    }
  }

  for (int guard = 0; guard < 40; guard++) {
    const int before = edges_paired(state);
    if (before == 12) return true;

    int best_score = before;
    std::string best_word;

    for (size_t q = 0; q < pool.size(); q++) {
      std::vector<int> staged = state;
      if (!pool[q].empty())
        staged = apply_word(staged, parse_word(pool[q], 4));

      for (int a = 0; a < n_edge_algs(); a++) {
        const std::vector<int> cand =
          apply_word(staged, parse_word(edge_algs()[a], 4));
        if (!centres_built(cand, o)) continue;      // never spend the centres
        const int score = edges_paired(cand);
        if (score <= best_score) continue;
        best_score = score;
        best_word = pool[q].empty()
          ? std::string(edge_algs()[a])
          : pool[q] + " " + edge_algs()[a];
      }
    }

    if (best_word.empty()) return edges_built(state);
    push_stage(sol, state, label, best_word, parse_word(best_word, 4));
  }
  return edges_built(state);
}

}  // namespace cube_solve

#endif  // CAYLEYR_CUBE_EDGES_H
