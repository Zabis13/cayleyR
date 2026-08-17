#ifndef CAYLEYR_CENTRE_TABLE_H
#define CAYLEYR_CENTRE_TABLE_H

// An exact prune table over phase 3's centre arrangements.
//
// The hash table this sits beside cannot be made to work: phase 3's full
// coordinate is about 1e11 states against 2^28 slots, waste 0.975, and a bound
// of 0 comes back on cubes that are nowhere near solved because another state
// claimed the slot. Depth and width were both measured and both are the same
// overflow seen from different ends (TODO.md, 14.08.2026).
//
// The centres admit an exact table instead, and it is small. Of the
// 24!/(4!)^6 = 3,246,670,537,110 arrangements that exist, phase 3 can reach
// only 58,800 -- measured by diag_centre_coord.R, closing the solved cube
// under all seventeen generators. The reason is in the generators: fourteen of
// them are outer-face turns that do not move a centre piece at all, and only
// Uw2, Rw2 and Fw2 disturb the centres, eight positions each.
//
// So this table is complete. Every reachable arrangement has its own entry,
// nothing collides, and what comes back is that arrangement's true distance to
// the nearest goal rather than a lower bound weakened by whoever else hashed
// the same way.
//
// What it does NOT do is bound the wings. It says how far the centres are from
// home, which stops the search wandering into branches that take the centres
// apart -- measured on seed 8, where the closest branch swung the centres
// 10 -> 14 -> 8 -> 14 while pairing nothing. Pairing is bounded by the wing
// coordinate (12! = 479,001,600, built and verified by bench_coord12_bfs.R),
// which is not built here because it costs 324 s and 228 MB and wants
// serialising first.
//
// The two are combined by taking the MAXIMUM, never the sum. Each is a lower
// bound on the moves remaining; the larger of two valid lower bounds is still
// a valid lower bound, while their sum is not -- one move can reduce both.

#include <map>
#include <vector>
#include <stdint.h>

#include "kociemba_core.h"

namespace kociemba4 {

// The centre arrangement as phase 3 sees it: the colour in each of the 24
// centre positions. Phase3Deriver4 already copies exactly this out of the
// piece state, so the key is read from the derived state and no new reading of
// the cube is introduced.
struct CentreKey {
  uint8_t c[N_CENTRES];

  bool operator<(const CentreKey& o) const {
    for (int i = 0; i < N_CENTRES; i++) {
      if (c[i] != o.c[i]) return c[i] < o.c[i];
    }
    return false;
  }
};

inline CentreKey centre_key_of(const kociemba::PieceState& derived) {
  CentreKey k;
  for (int i = 0; i < N_CENTRES; i++) {
    k.c[i] = (uint8_t)derived.perm[Z_OFF + i];
  }
  return k;
}

struct CentreTable {
  // Arrangement -> distance to the nearest goal. A std::map rather than a hash
  // is deliberate: 58,800 entries is small enough that the lookup cost does not
  // matter next to expanding a node, and an ordered map has no collisions to
  // reason about, which is the entire point of replacing the hash table.
  std::map<CentreKey, uint8_t> dist;
  uint8_t max_depth;
  bool ready;

  CentreTable() : max_depth(0), ready(false) {}

  // Breadth-first from the goals, over the phase's own generators.
  //
  // The frontier is carried as full piece states, not as centre keys: which
  // arrangements a move can produce depends on the whole cube, and a walk over
  // keys alone would be walking a graph that does not exist. Two states with
  // the same centre arrangement are merged only in `dist`, where the question
  // is the distance and the rest of the cube does not enter it.
  void build(const kociemba::PuzzleSpec& spec, const kociemba::Deriver& dv,
             const std::vector<kociemba::PieceState>& goals) {
    dist.clear();
    max_depth = 0;

    const std::vector<uint8_t> omod = spec.ori_mod();
    std::vector<kociemba::PieceState> frontier;
    kociemba::PieceState d;

    for (size_t i = 0; i < goals.size(); i++) {
      dv.derive(goals[i], d);
      const CentreKey k = centre_key_of(d);
      if (dist.find(k) == dist.end()) {
        dist[k] = 0;
        frontier.push_back(goals[i]);
      }
    }

    for (int depth = 1; !frontier.empty(); depth++) {
      std::vector<kociemba::PieceState> next;
      kociemba::PieceState moved;

      for (size_t i = 0; i < frontier.size(); i++) {
        for (int m = 0; m < spec.n_moves(); m++) {
          apply_move(frontier[i], spec.moves[m], omod, moved);
          dv.derive(moved, d);
          const CentreKey k = centre_key_of(d);
          if (dist.find(k) == dist.end()) {
            dist[k] = (uint8_t)depth;
            next.push_back(moved);
          }
        }
      }

      if (!next.empty()) max_depth = (uint8_t)depth;
      frontier.swap(next);
    }

    ready = true;
  }

  // The distance for a derived state, or 0 when the table has nothing to say.
  //
  // Zero rather than a large number on a miss, and the difference matters: a
  // prune table that guesses high is not admissible, and an inadmissible bound
  // does not slow the search down, it makes it wrong -- cutting branches that
  // held the solution. Every arrangement phase 3 can reach is in the table, so
  // a miss means the state came from somewhere the phase cannot go, and 0
  // prunes nothing while claiming nothing.
  uint8_t get(const kociemba::PieceState& derived) const {
    if (!ready) return 0;
    std::map<CentreKey, uint8_t>::const_iterator it =
      dist.find(centre_key_of(derived));
    return it == dist.end() ? (uint8_t)0 : it->second;
  }
};

// The table as the search sees it. Kept separate from CentreTable so that the
// table can be built and inspected without a search in hand.
struct CentreBound : public kociemba::ExtraBound {
  const CentreTable* tab;
  CentreBound() : tab(0) {}
  explicit CentreBound(const CentreTable* t) : tab(t) {}
  uint8_t bound(const kociemba::PieceState& derived) const {
    return tab ? tab->get(derived) : (uint8_t)0;
  }
};

}  // namespace kociemba4

#endif
