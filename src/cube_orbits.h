#ifndef CAYLEYR_CUBE_ORBITS_H
#define CAYLEYR_CUBE_ORBITS_H

#include <vector>
#include <string>
#include <map>
#include <algorithm>
#include <stdexcept>
#include "cube_nnn.h"

// ---- Pieces of a cube of any size, and the orbits they fall into --------
//
// "Corners and edges" is a sentence about a 3x3x3. A 4x4x4 has no edges in
// that sense -- what looks like one is two wings that never swap -- and its
// centres move. So the vocabulary has to come from the geometry instead, and
// this file derives it rather than tabulating it.
//
// ---- What a piece is ---------------------------------------------------
//
// A piece is a cubie: a position (x, y, z) carrying one sticker per face it
// touches. How many faces it touches is how many of its coordinates sit at an
// end of their range, and that is already the whole classification:
//
//   3 stickers   a corner. Always 8 of them, at every size.
//   2 stickers   an edge piece. On a 3x3x3 that is the 12 edges; on larger
//                cubes they split into wings, and only odd sizes keep a true
//                middle edge.
//   1 sticker    a centre. Six fixed ones on an odd cube, and beyond that
//                they move like anything else.
//
// ---- What an orbit is --------------------------------------------------
//
// Two pieces are in the same orbit when some sequence of moves takes one to
// the other, and the point of orbits is what they forbid: no word, however
// long, moves a piece out of its own orbit. A method may therefore treat each
// orbit separately, which is exactly what solving a big cube consists of.
//
// The invariant that names an orbit is small. Take the piece's three
// coordinates, replace each by its distance to the nearer end of the range
// (so k and n-1-k become the same number), and sort the three. That, with the
// sticker count, is most of the orbit:
//
//   f3:0,0,0     the 8 corners, at every n
//   f2:0,0,k     an edge orbit: k = 0 is the corner-adjacent wings, and on
//                odd n the middle k = (n-1)/2 is the true edges
//   f1:0,j,k     a centre class: j = k = 0 cannot occur, j = k is a diagonal
//                class, and on odd n j = k = (n-1)/2 is the six fixed centres
//
// ---- Where the depths are not enough ------------------------------------
//
// Sorting the depths sometimes glues two orbits together. A centre at (j, k)
// with j != k has a mirror image at (k, j); sorting sends both to the same
// triple, and on an even cube no move takes one to the other, so the class
// holds 48 pieces and two orbits.
//
// On an odd cube the same class holds 24 and is a single orbit: the face has a
// true centre, the piece can be carried round through it, and the mirror image
// is reached after all. So the split is not a property of the depths, and not
// of the parity of n either -- on a 7x7x7 the class (0,1,2) splits while
// (0,1,3) does not.
//
// Guessing which is which was tried and was wrong twice. What settles it is
// the definition: an orbit is what the group can reach, so the orbits are
// computed by walking the cube move by move from one piece and seeing where
// it can go. The depths and the mirror sign then only name what the walk
// found -- they label orbits, they do not define them.
//
// The sign itself: within a face take the sticker's offset from the centre,
// (u, v) with u down and v right; whether the row or the column is deeper,
// times the sign of u*v, is opposite on mirror images and constant along an
// orbit. Pieces with |u| == |v| lie on a diagonal, are their own mirror, and
// take sign 0.
//
// Verified for every n from 2 to 8: every move maps each orbit into itself,
// and every orbit is a single connected set under the group. Sizes are 24
// apart from the six fixed centres and the twelve middle edges of an odd
// cube, both of which sit on a symmetry axis, and the 8 corners.

namespace cube_orbits {

using namespace cube_nnn;

enum PieceKind {
  PK_CENTRE = 1,   // one sticker
  PK_EDGE   = 2,   // two stickers
  PK_CORNER = 3    // three stickers
};

// One piece: where it is, and which stickers of the flat state vector it
// carries. The stickers are in a fixed order per piece -- ascending face
// number -- so that "the same piece elsewhere" can be compared facelet by
// facelet.
struct Piece {
  int x, y, z;
  int kind;                    // PieceKind: how many stickers
  std::vector<int> stickers;   // 0-based indices into the state vector
  int orbit;                   // index into Orbits::orbit
};

// One orbit: the pieces in it, and what names it.
struct Orbit {
  std::string label;           // "f3:0,0,0", "f1:0,1,2+", ...
  int kind;                    // PieceKind shared by every piece in it
  int depth_a, depth_b;        // the two non-trivial sorted depths
  int chirality;               // -1, 0 or +1; 0 when the class is its own mirror
  std::vector<int> pieces;     // indices into Orbits::piece
};

struct Orbits {
  int n;
  std::vector<Piece> piece;
  std::vector<Orbit> orbit;

  int n_pieces() const { return (int)piece.size(); }
  int n_orbits() const { return (int)orbit.size(); }

  // Which piece carries this sticker. -1 never happens on a well-formed cube
  // but is returned rather than thrown so a caller can check.
  int piece_of_sticker(int s) const {
    if (s < 0 || s >= (int)sticker_piece.size()) return -1;
    return sticker_piece[s];
  }

  std::vector<int> sticker_piece;   // sticker index -> piece index
};

// The depth triple that names a piece's orbit: distance to the nearer end on
// each axis, sorted. This is the whole invariant.
inline void orbit_depths(int n, int x, int y, int z, int d[3]) {
  const int c[3] = {x, y, z};
  for (int k = 0; k < 3; k++) {
    const int v = c[k];
    d[k] = (v < n - 1 - v) ? v : (n - 1 - v);
  }
  std::sort(d, d + 3);
}

// Which of a mirror pair a one-sticker piece belongs to. Offsets are taken
// from the face centre and doubled, so that an even cube -- where no sticker
// sits at the centre -- still lands on integers. Returns 0 for a piece on a
// diagonal, which is its own mirror and so not chiral at all.
//
// Only centres need this. A corner or an edge piece is fixed by its depths:
// its stickers lie on two or three faces, and that already pins the handedness
// down.
inline int sticker_chirality(int n, int sticker) {
  const int within = sticker % (n * n);
  const int r = within / n, c = within % n;
  const int u = 2 * r - (n - 1);      // + is down the face
  const int v = 2 * c - (n - 1);      // + is right across it
  const int au = u < 0 ? -u : u;
  const int av = v < 0 ? -v : v;
  if (au == av) return 0;             // on a diagonal: its own mirror
  const int deeper_row = (au > av) ? 1 : -1;
  const int sgn = ((u > 0) ? 1 : -1) * ((v > 0) ? 1 : -1);
  return deeper_row * sgn;
}

inline std::string orbit_label(int kind, const int d[3], int chirality) {
  char buf[64];
  const char* mark = (chirality > 0) ? "+" : (chirality < 0 ? "-" : "");
  snprintf(buf, sizeof(buf), "f%d:%d,%d,%d%s", kind, d[0], d[1], d[2], mark);
  return std::string(buf);
}

// Build every piece and every orbit of an n x n x n cube.
inline Orbits build_orbits(int n) {
  if (n < 2) {
    throw std::runtime_error("cube_orbits: a cube has side 2 or more, got " +
                             std::to_string(n));
  }

  Orbits out;
  out.n = n;
  const std::vector<Sticker> st = all_stickers(n);
  out.sticker_piece.assign(st.size(), -1);

  // Gather stickers by the cubie they sit on. A map keyed on the coordinates
  // keeps the pieces in a stable order -- the same cube always comes out the
  // same way, which matters for anything that indexes into these tables.
  std::map<std::vector<int>, std::vector<int> > by_cubie;
  for (size_t i = 0; i < st.size(); i++) {
    std::vector<int> key(3);
    key[0] = st[i].x; key[1] = st[i].y; key[2] = st[i].z;
    by_cubie[key].push_back((int)i);
  }

  for (std::map<std::vector<int>, std::vector<int> >::const_iterator it =
           by_cubie.begin(); it != by_cubie.end(); ++it) {
    const std::vector<int>& c = it->first;
    const std::vector<int>& ss = it->second;

    Piece p;
    p.x = c[0]; p.y = c[1]; p.z = c[2];
    p.kind = (int)ss.size();
    p.stickers = ss;   // all_stickers walks faces in order, so these are too

    p.orbit = -1;   // filled in below, once the walk has found the orbits

    const int pi = (int)out.piece.size();
    for (size_t k = 0; k < ss.size(); k++) out.sticker_piece[ss[k]] = pi;
    out.piece.push_back(p);
  }

  // ---- The orbits themselves, by walking ---------------------------------
  //
  // Take a piece that has no orbit yet, and see everywhere the group can send
  // it: for each move, follow each of its stickers to the position that move
  // brings it from, and collect the pieces there. Repeat until nothing new
  // appears. That set is one orbit by definition, and doing it this way means
  // no rule about depths or mirrors has to be right in advance.

  const CubeAlphabet a = build_alphabet(n);
  // where each sticker comes FROM is what perms record; to follow a sticker
  // forwards, invert the map once per move rather than scanning it per piece.
  std::vector<std::vector<int> > fwd(a.perms.size());
  for (size_t m = 0; m < a.perms.size(); m++) {
    fwd[m].assign(st.size(), -1);
    for (size_t i = 0; i < a.perms[m].size(); i++) {
      fwd[m][a.perms[m][i] - 1] = (int)i;   // new[i] = old[perm[i]]
    }
  }

  for (size_t seed = 0; seed < out.piece.size(); seed++) {
    if (out.piece[seed].orbit >= 0) continue;

    const int oi = (int)out.orbit.size();
    Orbit o;
    o.kind = out.piece[seed].kind;
    int d[3];
    orbit_depths(n, out.piece[seed].x, out.piece[seed].y, out.piece[seed].z, d);
    o.depth_a = d[1];
    o.depth_b = d[2];
    o.chirality = (o.kind == PK_CENTRE)
                      ? sticker_chirality(n, out.piece[seed].stickers[0])
                      : 0;

    std::vector<int> frontier;
    frontier.push_back((int)seed);
    out.piece[seed].orbit = oi;
    o.pieces.push_back((int)seed);

    while (!frontier.empty()) {
      const int pi = frontier.back();
      frontier.pop_back();
      const std::vector<int>& mine = out.piece[pi].stickers;
      for (size_t m = 0; m < fwd.size(); m++) {
        for (size_t k = 0; k < mine.size(); k++) {
          const int to = fwd[m][mine[k]];
          if (to < 0) continue;
          const int np = out.sticker_piece[to];
          if (np < 0 || out.piece[np].orbit >= 0) continue;
          out.piece[np].orbit = oi;
          o.pieces.push_back(np);
          frontier.push_back(np);
        }
      }
    }

    out.orbit.push_back(o);
  }

  // Names last: a label distinguishes orbits, it does not decide them. Two
  // orbits sharing depths and kind are a mirror pair and the sign tells them
  // apart; when the walk found only one, the sign is not needed and the bare
  // triple reads better.
  {
    std::map<std::string, int> seen_bare;
    for (size_t i = 0; i < out.orbit.size(); i++) {
      const int dd[3] = {0, out.orbit[i].depth_a, out.orbit[i].depth_b};
      seen_bare[orbit_label(out.orbit[i].kind, dd, 0)]++;
    }
    for (size_t i = 0; i < out.orbit.size(); i++) {
      const int dd[3] = {0, out.orbit[i].depth_a, out.orbit[i].depth_b};
      const std::string bare = orbit_label(out.orbit[i].kind, dd, 0);
      if (seen_bare[bare] > 1) {
        out.orbit[i].label = orbit_label(out.orbit[i].kind, dd,
                                         out.orbit[i].chirality);
      } else {
        out.orbit[i].label = bare;
        out.orbit[i].chirality = 0;   // not a mirror pair: the sign means nothing
      }
    }
  }

  return out;
}

// One set of orbits per size, built on first use. Building them walks every
// sticker of the cube, which is worth doing once and not once per call.
inline const Orbits& orbits_of(int n) {
  // Held by value rather than by pointer: the cache lives for the process, and
  // a new'd entry is never deleted, which valgrind reports as a definite leak.
  // std::map does not move its mapped values, so the reference stays good.
  static std::map<int, Orbits> cache;
  std::map<int, Orbits>::iterator it = cache.find(n);
  if (it != cache.end()) return it->second;
  return cache.insert(std::make_pair(n, build_orbits(n))).first->second;
}

// ---- Reading a state in terms of pieces ---------------------------------
//
// A sticker's colour is the face it started on: stickers are numbered face by
// face in blocks of n*n, so integer division recovers it. This is the n-aware
// form of what cube_cubie.h does with a hardcoded 9.
inline int colour_of(int n, int sticker_value) {
  return (sticker_value - 1) / (n * n);
}

// ---- What each face is currently showing --------------------------------
//
// Which colour a face wears is read off the cube rather than assumed from its
// number, because turning the cube bodily in space moves every colour to a
// different face and leaves the puzzle solved. A face's colour is taken from
// the sticker at its centre on an odd cube -- that one cannot move relative to
// the rest of the face -- and otherwise by majority, which is what a person
// does when they glance at a face and say what colour it is.
//
// This mirrors cube_is_colour_solved(): the package already treats "solved"
// as a statement about colours, and a piece being home has to mean the same
// thing or a cube solved relative to its centres would count as scrambled.
inline std::vector<int> face_colours(int n, const std::vector<int>& state) {
  const int fs = n * n;
  std::vector<int> out(6, -1);
  for (int f = 0; f < 6; f++) {
    if (n % 2 == 1) {
      out[f] = colour_of(n, state[f * fs + (fs - 1) / 2]);
      continue;
    }
    std::vector<int> tally(6, 0);
    for (int i = 0; i < fs; i++) {
      const int c = colour_of(n, state[f * fs + i]);
      if (c >= 0 && c < 6) tally[c]++;
    }
    int best = 0;
    for (int c = 1; c < 6; c++) if (tally[c] > tally[best]) best = c;
    out[f] = best;
  }
  return out;
}

// Is this piece home -- in its own slot, the right way round?
//
// A piece is home when every sticker it carries shows the colour the face it
// lies on is currently wearing. Stated that way it is a fact about colours
// rather than about sticker numbers, so a cube turned bodily in space is still
// home, and so is one solved relative to its centres -- which is what a real
// cube looks like and what the blindfold methods mean by solved.
//
// `faces` is the current colour of each face, from face_colours(); it is
// passed in because it costs a pass over the cube and every piece needs the
// same one.
inline bool piece_home(const Orbits& O, const std::vector<int>& state,
                       const std::vector<int>& faces, int pi) {
  const Piece& p = O.piece[pi];
  const int fs = O.n * O.n;
  for (size_t k = 0; k < p.stickers.size(); k++) {
    const int s = p.stickers[k];
    if (colour_of(O.n, state[s]) != faces[s / fs]) return false;
  }
  return true;
}

inline bool piece_home(const Orbits& O, const std::vector<int>& state, int pi) {
  return piece_home(O, state, face_colours(O.n, state), pi);
}

// How many pieces of an orbit are home. This is the measure a solver's
// progress is stated in, and the one thing a distance on a big cube can be
// built from: "how much of the cube is done" is per-orbit or it is nothing.
inline int orbit_solved_count(const Orbits& O, const std::vector<int>& state,
                              const std::vector<int>& faces, int oi) {
  const Orbit& o = O.orbit[oi];
  int c = 0;
  for (size_t k = 0; k < o.pieces.size(); k++) {
    if (piece_home(O, state, faces, o.pieces[k])) c++;
  }
  return c;
}

inline int orbit_solved_count(const Orbits& O, const std::vector<int>& state,
                              int oi) {
  return orbit_solved_count(O, state, face_colours(O.n, state), oi);
}

// Every orbit's solved count, in orbit order.
inline std::vector<int> orbit_progress(const Orbits& O,
                                       const std::vector<int>& state) {
  const std::vector<int> faces = face_colours(O.n, state);
  std::vector<int> out(O.orbit.size());
  for (size_t i = 0; i < O.orbit.size(); i++) {
    out[i] = orbit_solved_count(O, state, faces, (int)i);
  }
  return out;
}

// Pieces home over pieces in total. The blunt summary, for when a single
// number is wanted -- a per-orbit vector says more.
inline int pieces_home(const Orbits& O, const std::vector<int>& state) {
  const std::vector<int> faces = face_colours(O.n, state);
  int c = 0;
  for (size_t i = 0; i < O.piece.size(); i++) {
    if (piece_home(O, state, faces, (int)i)) c++;
  }
  return c;
}

}  // namespace cube_orbits

#endif  // CAYLEYR_CUBE_ORBITS_H
