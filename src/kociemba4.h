#ifndef CAYLEYR_KOCIEMBA4_H
#define CAYLEYR_KOCIEMBA4_H

#include "kociemba_core.h"
#include "kociemba3.h"
#include "cube_search.h"
#include "cube_orbits.h"

// ---- The 4x4x4 in four phases -------------------------------------------
//
// The division into phases, and parity carried as a coordinate rather than
// repaired afterwards, follow twips -- https://github.com/cubing/twips, by
// Lucas Garron and the cubing.js authors, MPL-2.0 -- specifically its
// `scramble/puzzles/cube4x4x4/`. The piece reading below is this package's own:
// it is measured against the sticker geometry cube_orbits.h already generates,
// rather than tabulated as twips does.
//
// A 4x4x4 is not a bigger 3x3x3 and cannot be searched as one: 24 wings and 24
// centres on top of the 8 corners put the state count past anything a single
// search reaches. The way through is reduction -- make the centres act as one
// face and the wing pairs act as one edge, and what is left is a 3x3x3 with
// fat pieces, which the two-phase solver already in this package finishes.
//
// Four phases, each throwing away less than the one before:
//
//   1. F/B centres onto the F/B axis
//   2. L/R and U/D centres onto their axes, F/B solvable by half turns
//   3. pair the wings, avoiding parity
//   4. hand the reduced cube to the 3x3x3 solver
//
// ---- Why the centres come first ------------------------------------------
//
// Every later phase is stated against the centres: "this wing belongs beside
// that centre" has no meaning until the centres are where they belong. The
// same reason the 3x3x3 solver turns the cube back before it starts, one level
// up.
//
// ---- Parity, and why it is a coordinate rather than a repair --------------
//
// Reduction can leave the cube in a state no 3x3x3 reaches: the wings can be
// paired and the corners solved while the permutation is odd, which on a
// 3x3x3 is impossible. The package's existing 4x4x4 solver meets this at the
// end and repairs it with a long algorithm that turns inner layers -- see
// cube_reduce.h, which measures nineteen of forty states stopping at OLL and
// twelve at PLL.
//
// Here it never arises. The parity of the wing permutation is carried as part
// of the phase's coordinate, so a solution that would leave the cube parity-odd
// is not a solution to the phase at all and the search does not return it. The
// cost is a coordinate one bit larger; the saving is the whole repair step.
//
// ---- Where the move table comes from --------------------------------------
//
// Measured, as on the 3x3x3: apply the move to a solved cube with the package's
// own sticker permutation, read the result back as pieces. Nothing here asserts
// what a move does.

namespace kociemba4 {

using namespace kociemba;

const int N_CORNERS4 = 8;
const int N_WINGS = 24;
const int N_CENTRES = 24;
const int C_OFF4 = 0;                        // corners  0..7
const int W_OFF = 8;                         // wings    8..31
const int Z_OFF = 32;                        // centres 32..55
const int TOTAL4 = 56;

}  // namespace kociemba4

// Included here rather than at the top of the file because it is written in
// terms of the offsets just defined. It reopens namespace kociemba4 itself.
#include "centre_table.h"

namespace kociemba4 {

// Restated because the `using` above ended with the block that held it.
using namespace kociemba;

// ---- Reading a 4x4x4 state as pieces --------------------------------------
//
// cube_orbits.h already works out, from the geometry, which stickers belong to
// which piece and in which order. That is the whole of what is needed: a piece
// is identified by the multiset of colours it carries, and its orientation by
// how far round that multiset has been turned from the piece's own order.
//
// Corners have three stickers and three orientations, wings two stickers and
// -- on a 4x4x4 -- no orientation at all. That last one is the difference from
// the 3x3x3 worth spelling out: a 3x3x3 edge can be flipped in place because
// its two slots are the same slot, while a 4x4x4 wing cannot, because the two
// halves of a dedge sit in different positions. A flipped wing is not a flipped
// piece, it is the other piece of the pair. So wings carry one orientation, and
// what a 3x3x3 would call a flip shows up here as a permutation.

struct Cube4Layout {
  std::vector<std::vector<int> > corner_stickers;   // 8 x 3, 0-based
  std::vector<std::vector<int> > wing_stickers;     // 24 x 2
  std::vector<std::vector<int> > centre_stickers;   // 24 x 1
  bool built;
  Cube4Layout() : built(false) {}
};

inline int colour_of4(int sticker_value) { return (sticker_value - 1) / 16; }

inline Cube4Layout& cube4_layout() {
  static Cube4Layout L;
  if (L.built) return L;

  // cube_orbits.h works out from the geometry which stickers sit on which
  // piece, at any size. Its sticker indices are already 0-based, and it walks
  // the faces in order, so the pieces come out in a fixed order that only has
  // to be split by how many stickers each carries.
  const cube_orbits::Orbits& O = cube_orbits::orbits_of(4);
  for (size_t i = 0; i < O.piece.size(); i++) {
    const cube_orbits::Piece& p = O.piece[i];
    if (p.stickers.size() == 3) L.corner_stickers.push_back(p.stickers);
    else if (p.stickers.size() == 2) L.wing_stickers.push_back(p.stickers);
    else L.centre_stickers.push_back(p.stickers);
  }
  L.built = true;
  return L;
}

// ---- Putting the corner stickers in rotational order ----------------------
//
// Orientation counts how far a corner has been twisted, and twisting is a
// rotation, so it can only be counted against a listing that goes round the
// corner the same way on every one of the eight. cube_orbits.h does not give
// that: it sorts a piece's stickers by face number, and sorting is not
// rotation. Measured on this cube -- turn R and ask whether each corner's three
// stickers move cyclically within the listed order -- six corners say yes and
// two say no, because on those two the ascending order runs the other way
// round.
//
// So the order is rebuilt here from the geometry. A corner at (x, y, z) has
// three stickers, one on each of the three faces it touches; going round it
// anticlockwise seen from outside is the same as taking the faces in the order
// given by the sign of the corner's position on each axis. Re-listed this way
// every corner turns cyclically, which is what makes an orientation additive.

struct CornerSticker { int piece; int index; };

// The three stickers of each corner, in rotational order: the U or D sticker
// first, then the other two clockwise seen from outside the corner. This is the
// listing cube_cubie.h uses for the 3x3x3, where it is checked by four solvers,
// written out here for a 4x4x4 -- same eight corners, same geometry, larger
// faces.
//
// Faces are numbered U R F D L B, and a face of a cube of side n is n*n
// stickers in reading order, so the corner cell of face f at row r, column c is
// f*n*n + r*n + c.
inline int st4(int f, int r, int c) { return f * 16 + r * 4 + c; }

inline const std::vector<std::vector<int> >& corner_stickers_cyclic() {
  static std::vector<std::vector<int> > out;
  if (!out.empty()) return out;

  // URF UFL ULB UBR DFR DLF DBL DRB -- Kociemba's order, the one kociemba3.h
  // already speaks, so a reduced 4x4x4 can be handed to the 3x3x3 solver
  // without a second translation.
  static const int t[8][3][3] = {
    {{0,3,3},{1,0,0},{2,0,3}},   // URF
    {{0,3,0},{2,0,0},{4,0,3}},   // UFL
    {{0,0,0},{4,0,0},{5,0,3}},   // ULB
    {{0,0,3},{5,0,0},{1,0,3}},   // UBR
    {{3,0,3},{2,3,3},{1,3,0}},   // DFR
    {{3,0,0},{4,3,3},{2,3,0}},   // DLF
    {{3,3,0},{5,3,3},{4,3,0}},   // DBL
    {{3,3,3},{1,3,3},{5,3,0}}    // DRB
  };
  for (int p = 0; p < 8; p++) {
    std::vector<int> st(3);
    for (int k = 0; k < 3; k++) st[k] = st4(t[p][k][0], t[p][k][1], t[p][k][2]);
    out.push_back(st);
  }
  return out;
}

inline const std::vector<CornerSticker>& sticker_to_corner() {
  static std::vector<CornerSticker> owner;
  if (!owner.empty()) return owner;
  CornerSticker none; none.piece = -1; none.index = -1;
  owner.assign(96, none);
  const std::vector<std::vector<int> >& cs4 = corner_stickers_cyclic();
  for (int p = 0; p < N_CORNERS4; p++) {
    for (int k = 0; k < 3; k++) {
      CornerSticker cs; cs.piece = p; cs.index = k;
      owner[cs4[p][k]] = cs;
    }
  }
  return owner;
}

// The corner in a slot, and how far it is turned: the piece is named by the
// sticker in the slot's first position, and the orientation is how far that
// sticker has moved from being the piece's own first.
inline void identify_corner4_by_stickers(const std::vector<int>& state,
                                         int slot, int& piece, int& ori) {
  // Both the slot and the piece are named in the rotational listing, never in
  // cube_orbits.h's sorted one -- mixing the two numbers eight corners twice
  // over and nothing lines up.
  const std::vector<std::vector<int> >& cs4 = corner_stickers_cyclic();
  const std::vector<CornerSticker>& owner = sticker_to_corner();
  const CornerSticker& cs = owner[state[cs4[slot][0]] - 1];
  if (cs.piece < 0) throw std::runtime_error("cube4: sticker is not a corner's");
  piece = cs.piece;
  ori = cs.index;
}

// ---- Telling the two wings of a dedge apart -------------------------------
//
// Colour will not do it. Measured on this package's own geometry: the 24 wing
// slots carry only 12 distinct ordered colour pairs, two slots each, because
// cube_orbits.h lists a piece's stickers in ascending face order and both
// wings of a dedge therefore come out with the same pair. A wing is not a
// 3x3x3 edge that can be flipped in place; it is one of two pieces, and which
// one is a fact about position rather than colour.
//
// What separates them is where they sit, and the stickers carry that with them:
// a sticker's number, as opposed to merely its colour, belongs to exactly one
// wing. So a wing is read from the sticker found in the slot rather than from
// the colours, and no table of "which side of the dedge is this" is needed.
//
// Which wing owns each sticker on a solved cube. A sticker travels with its
// piece, so the number found in a slot names the wing that is there -- which is
// the identification colours cannot make, since the two wings of a dedge carry
// the same pair of colours.
inline const std::vector<int>& sticker_to_wing() {
  static std::vector<int> owner;
  if (!owner.empty()) return owner;
  owner.assign(96, -1);
  const Cube4Layout& L = cube4_layout();
  for (int p = 0; p < N_WINGS; p++) {
    owner[L.wing_stickers[p][0]] = p;
    owner[L.wing_stickers[p][1]] = p;
  }
  return owner;
}

// The wing sitting in a slot, named by the sticker it carries.
inline int identify_wing_by_sticker(int sticker_value) {
  const int w = sticker_to_wing()[sticker_value - 1];
  if (w < 0) throw std::runtime_error("cube4: sticker does not belong to a wing");
  return w;
}

// Centres are indistinguishable within a face, so a centre "piece" is really
// just its colour. That is what makes the centre phases tractable.
inline int identify_centre(int col) { return col; }

inline PieceState from_stickers4(const std::vector<int>& state96) {
  if (state96.size() != 96) {
    throw std::runtime_error("cube4: a 4x4x4 state has 96 stickers, got " +
                             std::to_string(state96.size()));
  }
  const Cube4Layout& L = cube4_layout();
  PieceState s;
  s.perm.assign(TOTAL4, 0);
  s.ori.assign(TOTAL4, 0);

  for (int i = 0; i < N_CORNERS4; i++) {
    int piece, ori;
    identify_corner4_by_stickers(state96, i, piece, ori);
    s.perm[C_OFF4 + i] = (kociemba::Slot)(C_OFF4 + piece);
    s.ori[C_OFF4 + i] = (uint8_t)ori;
  }
  for (int i = 0; i < N_WINGS; i++) {
    s.perm[W_OFF + i] =
        (kociemba::Slot)(W_OFF + identify_wing_by_sticker(state96[L.wing_stickers[i][0]]));
  }
  for (int i = 0; i < N_CENTRES; i++) {
    // A centre is known only by its colour: which of the four faces' centres it
    // is does not exist as a fact about the cube, and the phases below are
    // built on that. Numbering by colour is what makes two states they cannot
    // tell apart come out equal.
    s.perm[Z_OFF + i] = (kociemba::Slot)(Z_OFF + colour_of4(state96[L.centre_stickers[i][0]]));
  }
  return s;
}

// ---- Moves ----------------------------------------------------------------

inline OrbitMove move_as_pieces4(int qt) {
  const cube_search::CubeN& C = cube_search::cube_n(4);
  std::vector<int> s(96);
  for (int i = 0; i < 96; i++) s[i] = i + 1;
  C.apply(s, qt);
  // The centres of a solved cube are read by colour, so the identity reading
  // of a move would lose which centre went where. Read positions instead: the
  // piece now in slot i is whichever slot its stickers came from.
  PieceState p;
  p.perm.assign(TOTAL4, 0);
  p.ori.assign(TOTAL4, 0);

  const Cube4Layout& L = cube4_layout();
  // For each slot, find where its first sticker came from, and hence which
  // slot's piece now sits here.
  // A move's orientation entry is a *twist* -- how much to add to whatever the
  // arriving piece already carried -- not the arriving piece's own orientation.
  // apply_move adds it modulo 3, and only a relative amount may be added: an
  // absolute "which sticker is in position 0" summed twice is meaningless, and
  // shows up as R followed by R' failing to be the identity while R alone is
  // right.
  //
  // On the solved cube every piece starts at orientation 0, so the reading of
  // the turned cube *is* the twist that turn applies.
  for (int i = 0; i < N_CORNERS4; i++) {
    int piece, ori;
    identify_corner4_by_stickers(s, i, piece, ori);
    p.perm[C_OFF4 + i] = (kociemba::Slot)(C_OFF4 + piece);
    p.ori[C_OFF4 + i] = (uint8_t)ori;
  }
  for (int i = 0; i < N_WINGS; i++) {
    p.perm[W_OFF + i] =
        (kociemba::Slot)(W_OFF + identify_wing_by_sticker(s[L.wing_stickers[i][0]]));
  }
  // Centres: the sticker value itself says which slot it came from, which is
  // what a move has to record even though a *state* only knows colours.
  for (int i = 0; i < N_CENTRES; i++) {
    const int from_sticker = s[L.centre_stickers[i][0]] - 1;
    int from_slot = -1;
    for (int j = 0; j < N_CENTRES; j++) {
      if (L.centre_stickers[j][0] == from_sticker) { from_slot = j; break; }
    }
    if (from_slot < 0) throw std::runtime_error("cube4: lost a centre");
    p.perm[Z_OFF + i] = (kociemba::Slot)(Z_OFF + from_slot);
  }

  OrbitMove mv;
  mv.perm = p.perm;
  mv.ori = p.ori;
  return mv;
}

// ---- The puzzle, as the phases see it -------------------------------------

inline void build_spec4(const std::vector<std::string>& gen_names,
                        PuzzleSpec& spec) {
  spec.orbits.clear();
  OrbitDef c; c.name = "CORNERS"; c.n_pieces = 8;  c.n_orientations = 3;
  OrbitDef w; w.name = "WINGS";   w.n_pieces = 24; w.n_orientations = 1;
  OrbitDef z; z.name = "CENTRES"; z.n_pieces = 24; z.n_orientations = 1;
  spec.orbits.push_back(c);
  spec.orbits.push_back(w);
  spec.orbits.push_back(z);
  spec.finish_layout();

  const std::vector<uint8_t> omod = spec.ori_mod();
  const cube_search::CubeN& C = cube_search::cube_n(4);

  spec.moves.clear();
  spec.move_names.clear();
  spec.move_axis.clear();
  spec.move_layer.clear();
  spec.move_classes_.clear();

  // A generator is a *word* of the package's alphabet, written with spaces --
  // "U", "U'", "U2", or "U 2y" for a wide turn -- and it costs the phase one
  // step whatever its length. That is what lets this match twips, whose phases
  // are stated in block notation: its search puzzle defines Uw2 as a move of
  // its own, so a wide turn is one node there and must be one node here too.
  // Spelling it as two moves instead would make the phase count a different
  // metric and search a differently shaped tree.
  //
  // The axis and layer come from the word's first move. They feed the canonical
  // automaton, which uses them to refuse a word that merely repeats the one
  // before; for a wide turn the leading face is the right thing to key on,
  // since two wide turns of the same face are what the automaton must catch.
  for (size_t i = 0; i < gen_names.size(); i++) {
    const std::string& g = gen_names[i];

    // Split on spaces.
    std::vector<std::string> tokens;
    for (size_t a = 0; a < g.size();) {
      while (a < g.size() && g[a] == ' ') a++;
      size_t b = a;
      while (b < g.size() && g[b] != ' ') b++;
      if (b > a) tokens.push_back(g.substr(a, b - a));
      a = b;
    }
    if (tokens.empty()) throw std::runtime_error("cube4: empty generator");

    OrbitMove mv;
    bool first = true;
    int lead_qt = -1;
    for (size_t t = 0; t < tokens.size(); t++) {
      std::string base = tokens[t];
      int power = 1;
      if (base.size() > 1 && base[base.size() - 1] == '\'') {
        base = base.substr(0, base.size() - 1);
        power = 3;
      } else if (base.size() > 1 && base[base.size() - 1] == '2') {
        base = base.substr(0, base.size() - 1);
        power = 2;
      }
      const int qt = C.move_index(base);
      if (qt < 0) throw std::runtime_error("cube4: no such move '" + base + "'");
      if (lead_qt < 0) lead_qt = qt;

      OrbitMove b = move_as_pieces4(qt);
      OrbitMove step = b;
      for (int k = 1; k < power; k++) step = compose(step, b, omod);

      if (first) { mv = step; first = false; }
      else mv = compose(mv, step, omod);
    }

    spec.moves.push_back(mv);
    spec.move_names.push_back(g);
    spec.move_axis.push_back(C.axis_of(lead_qt));
    spec.move_layer.push_back(C.layer_of(lead_qt));
  }
}

// ---- Phase 1: the F/B centres onto the F/B axis ---------------------------
//
// Nothing else. Which of the eight F/B centre pieces sits where does not
// matter, and neither does anything about corners, wings or the other centres:
// the coordinate says, for each centre slot, only whether it holds a centre
// that belongs on the F/B axis.
//
// Measured on this package's geometry rather than tabulated: F is slots
// 7, 9, 15, 17 and B is 6, 8, 14, 16 -- see the note on centre numbering above.
// A centre's identity in a state is its colour, so "belongs on the F/B axis"
// is a test on the colour.

inline bool face_is_fb(int face) { return face == 2 || face == 5; }

struct Phase1Deriver4 : public Deriver {
  void derive(const PieceState& in, PieceState& out) const {
    out.perm.assign(TOTAL4, 0);
    out.ori.assign(TOTAL4, 0);
    for (int i = 0; i < N_CENTRES; i++) {
      const int colour = in.perm[Z_OFF + i] - Z_OFF;
      out.perm[Z_OFF + i] = (kociemba::Slot)(face_is_fb(colour) ? 1 : 0);
    }
  }
};

// ---- Phase 2: the other centres onto their axes, and wing parity ----------
//
// The L/R and U/D centres go to their own axes, and the F/B centres -- already
// on the right axis -- have to end up somewhere half turns can finish from.
//
// Wing parity joins the coordinate here, which is the part worth stating. The
// parity of the wing permutation cannot be changed by the generators of the
// later phases, so a state that reaches phase 3 with the wrong parity can never
// be finished and the search would grind until its budget ran out. Carrying the
// parity now means such a state is not a solution to *this* phase, and phase 2
// simply returns a different word.

inline int basic_parity(const std::vector<kociemba::Slot>& perm, int off, int n) {
  // Parity of a permutation: count transpositions by walking its cycles.
  std::vector<char> seen(n, 0);
  int swaps = 0;
  for (int i = 0; i < n; i++) {
    if (seen[i]) continue;
    int j = i, len = 0;
    while (!seen[j]) { seen[j] = 1; j = perm[off + j] - off; len++; }
    swaps += len - 1;
  }
  return swaps & 1;
}

struct Phase2Deriver4 : public Deriver {
  void derive(const PieceState& in, PieceState& out) const {
    out.perm.assign(TOTAL4, 0);
    out.ori.assign(TOTAL4, 0);
    // Which centres this phase can tell apart, taken from twips's phase-2
    // puzzle. Its default pattern numbers the 24 centre slots
    //
    //   [0,0,0,0, 4,4,4,4, 8,8,8,8, 4,4,4,4, 16,16,16,16, 0,0,0,0]
    //
    // and its faces run in Speffz order, U L F R B D. Reading it that way:
    // U and D share a value, L and R share another, F and B keep their own.
    // Opposite faces are merged, which is what "get each centre onto its own
    // axis" means -- the phase settles the axis and leaves which of the two
    // faces to phase 3.
    //
    // The trap here, and the reason this is spelled out: their face order is
    // not ours. This package numbers faces U R F D L B, so their array cannot
    // be copied across position by position. Doing exactly that was a bug --
    // it merged U with B and R with D, pairs that are not opposite, and phase
    // 2 duly finished with two centres on the wrong axis while its own
    // coordinate reported success.
    static const int centre_class[6] = {
      /* U */ 0, /* R */ 4, /* F */ 8, /* D */ 0, /* L */ 4, /* B */ 16
    };
    for (int i = 0; i < N_CENTRES; i++) {
      const int colour = in.perm[Z_OFF + i] - Z_OFF;
      out.perm[Z_OFF + i] = (kociemba::Slot)centre_class[colour];
    }
    // Wing parity, as one bit, in the first corner slot's orientation -- an
    // unused entry of the derived state rather than an orbit of its own.
    out.ori[C_OFF4] = (uint8_t)basic_parity(in.perm, W_OFF, N_WINGS);
  }
};

// ---- Phase 3: pair the wings ----------------------------------------------
//
// The centres stay where phase 2 left them and the wings are brought together
// into dedges. What the coordinate keeps is which dedge each wing belongs to,
// not which of the pair it is: two states differing only by swapping the halves
// of a pair are the same state for this purpose, and telling them apart would
// multiply the search space for nothing.
//
// A wing and its partner are the two pieces carrying the same pair of colours.
// That is measurable -- 24 wings, 12 colour pairs, two each -- so the partner
// table is built from the geometry rather than written out.

inline const std::vector<int>& wing_to_dedge() {
  static std::vector<int> dedge;
  if (!dedge.empty()) return dedge;
  const Cube4Layout& L = cube4_layout();
  std::map<std::pair<int,int>, int> id;
  dedge.assign(N_WINGS, -1);
  for (int p = 0; p < N_WINGS; p++) {
    int a = L.wing_stickers[p][0] / 16;
    int b = L.wing_stickers[p][1] / 16;
    if (a > b) std::swap(a, b);
    const std::pair<int,int> key(a, b);
    std::map<std::pair<int,int>,int>::iterator it = id.find(key);
    if (it == id.end()) {
      const int next = (int)id.size();
      id[key] = next;
      dedge[p] = next;
    } else {
      dedge[p] = it->second;
    }
  }
  return dedge;
}

// The other wing of the same dedge. Measured, not tabulated: the two wings of a
// pair are the two slots carrying the same pair of colours, and there are
// exactly two of each.
inline const std::vector<int>& wing_to_partner() {
  static std::vector<int> partner;
  if (!partner.empty()) return partner;
  partner.assign(N_WINGS, -1);
  const std::vector<int>& dd = wing_to_dedge();
  for (int a = 0; a < N_WINGS; a++) {
    for (int b = 0; b < N_WINGS; b++) {
      if (b != a && dd[b] == dd[a]) { partner[a] = b; break; }
    }
  }
  return partner;
}

// ---- Speffz, for reading twips's tables ------------------------------------
//
// Speffz is the lettering scheme blindfolded cubers use, and it is the
// numbering twips's 4x4x4 tables are written in. Our wing slots come out of
// cube_orbits.h in traversal order instead, so nothing of theirs can be copied
// across without a translation -- index 4 of their table is not our slot 4.
//
// The scheme numbers wing STICKERS, not slots: four per face, faces in the
// order U L F R B D, and within a face clockwise from the top edge. On a 4x4
// face the eight wing stickers sit at row-major cells 1,2 (top), 7,11 (right),
// 13,14 (bottom), 4,8 (left), and Speffz takes the clockwise-leading one of
// each pair -- cells 1, 7, 14, 8. The wing slot is whoever owns that sticker.
//
// This is derived from the geometry rather than tabulated, which also makes it
// checkable: translating twips's own tables through the map has to reproduce
// what we measure independently. It does, for all three of them, 24 entries
// each -- see tests/testthat/test-cube-speffz.R. POSITION_IS_PRIMARY is the
// one that makes the check worth something: it is an asymmetric 12/12 split,
// so a wrong map would almost certainly break it.
inline const std::vector<int>& speffz_to_wing_slot() {
  static std::vector<int> map;
  if (!map.empty()) return map;

  const Cube4Layout& L = cube4_layout();
  // Which slot owns each sticker. cube_orbits.h indexes stickers from 0.
  std::vector<int> owner(96, -1);
  for (int p = 0; p < N_WINGS; p++) {
    owner[L.wing_stickers[p][0]] = p;
    owner[L.wing_stickers[p][1]] = p;
  }

  // Speffz face order, in this package's face numbering (U R F D L B).
  static const int face_order[6] = {0, 4, 2, 1, 5, 3};   // U L F R B D
  static const int lead_cell[4] = {1, 7, 14, 8};         // top, right, bottom, left

  map.assign(N_WINGS, -1);
  int k = 0;
  for (int f = 0; f < 6; f++) {
    for (int e = 0; e < 4; e++) {
      const int sticker = face_order[f] * 16 + lead_cell[e];
      const int slot = owner[sticker];
      if (slot < 0) throw std::runtime_error("cube4: speffz sticker is not a wing's");
      map[k++] = slot;
    }
  }
  return map;
}

// ---- Canonicalising the wings ---------------------------------------------
//
// Which half of a dedge is which does not matter to this phase: a state and the
// state got by swapping the two wings of every pair are the same problem, and
// the pairing that finishes one finishes the other. Treating them as different
// states is what makes the phase unsearchable -- measured, a plain
// breadth-first from a scrambled cube visits a million states in five levels
// without reaching the goal.
//
// So the wings are renumbered before the state is hashed: walk the slots in
// order, and the first time a piece is seen give it the next number available,
// giving its partner the partner of that number at the same time. Two states
// that differ only by swapping halves get the same numbering and become one
// state. This is twips's `canonicalize_wings`, which it applies inside every
// move; doing it in the deriver is the same thing, since the deriver is what
// the table and the goal test see.
inline void canonicalize_wings(std::vector<kociemba::Slot>& perm) {
  const std::vector<int>& partner = wing_to_partner();
  int mapping[N_WINGS];
  bool blocked[N_WINGS];
  for (int i = 0; i < N_WINGS; i++) { mapping[i] = -1; blocked[i] = false; }

  int next_assignment = 0;
  for (int i = 0; i < N_WINGS; i++) {
    const int piece = perm[W_OFF + i] - W_OFF;
    if (mapping[piece] >= 0) continue;

    const int assigned = next_assignment;
    mapping[piece] = assigned;
    mapping[partner[piece]] = partner[assigned];
    blocked[partner[assigned]] = true;

    next_assignment++;
    while (next_assignment < N_WINGS && blocked[next_assignment]) next_assignment++;
  }
  for (int i = 0; i < N_WINGS; i++) {
    perm[W_OFF + i] = (kociemba::Slot)(W_OFF + mapping[perm[W_OFF + i] - W_OFF]);
  }
}

// ---- One half of each dedge -----------------------------------------------
//
// PLL parity is not a fact about all 24 wings; it is a fact about the twelve
// dedges, and to count it each dedge has to be represented once. twips calls
// the chosen half of a pair "primary" and defines it as the positions reachable
// from one corner wing using <U, L, R, D> -- the turns that cannot separate a
// pair. Here that set is measured rather than tabulated: run those generators
// from slot 0 and see which slots come up.
inline const std::vector<char>& wing_position_is_primary() {
  static std::vector<char> primary;
  if (!primary.empty()) return primary;

  PuzzleSpec spec;
  static const char* g[] = {"U","U'","L","L'","R","R'","D","D'"};
  build_spec4(std::vector<std::string>(g, g + 8), spec);
  const std::vector<uint8_t> omod = spec.ori_mod();

  primary.assign(N_WINGS, 0);
  std::vector<char> seen(N_WINGS, 0);
  std::vector<int> frontier;
  seen[0] = 1; primary[0] = 1; frontier.push_back(0);

  // Where slot 0's piece can travel: follow each generator as a map on slots.
  while (!frontier.empty()) {
    std::vector<int> next;
    for (size_t f = 0; f < frontier.size(); f++) {
      for (int m = 0; m < spec.n_moves(); m++) {
        // The move sends the piece in slot mv.perm[i] to slot i, so slot
        // frontier[f] feeds whichever i names it.
        for (int i = 0; i < N_WINGS; i++) {
          if (spec.moves[m].perm[W_OFF + i] - W_OFF != frontier[f]) continue;
          if (seen[i]) continue;
          seen[i] = 1; primary[i] = 1; next.push_back(i);
        }
      }
    }
    frontier.swap(next);
  }
  return primary;
}

// Parity of the corner permutation, over the eight corner slots.
inline int corner_parity(const PieceState& s) {
  return basic_parity(s.perm, C_OFF4, N_CORNERS4);
}

// ---- What phase 2 must not hand on ----------------------------------------
//
// Building the centres is not all phase 2 has to do. A cube can have its
// centres finished and still be a position phase 3 cannot pair the wings from,
// and phase 3 discovers that only by spending its whole budget -- measured, a
// plain breadth-first from such a state visits a million positions in five
// levels without reaching the goal.
//
// The condition is twips's
// `is_each_wing_pair_separated_across_primary_and_secondary`: every dedge must
// have one of its two wings in a primary position and the other in a secondary
// one, and the number of primary pieces sitting in primary positions must be
// even. A solution of phase 2 leaving the cube otherwise is refused, and the
// search goes on to the next one.
//
// It is a filter on solutions rather than part of the coordinate because it is
// not what the phase aims at -- it is what the phase must not break on the way.
struct Phase2SolutionFilter : public SolutionFilter {
  bool accept(const PieceState& s) const {
    const std::vector<char>& primary = wing_position_is_primary();
    const std::vector<int>& dd = wing_to_dedge();

    std::vector<char> seen_primary(N_WINGS, 0), seen_secondary(N_WINGS, 0);
    int primary_in_primary = 0;

    for (int pos = 0; pos < N_WINGS; pos++) {
      const int piece = s.perm[W_OFF + pos] - W_OFF;
      const int dedge = dd[piece];
      if (primary[pos]) {
        if (primary[piece]) primary_in_primary++;
        if (seen_primary[dedge]) return false;
        seen_primary[dedge] = 1;
      } else {
        if (seen_secondary[dedge]) return false;
        seen_secondary[dedge] = 1;
      }
    }
    return (primary_in_primary & 1) == 0;
  }
};

// ---- Collecting more than one phase-2 solution -----------------------------
//
// Phase 2 returns its first solution and stops, and that one word decides what
// phase 3 is given. Measured, it decides a great deal: across four rotations of
// one cube phase 2 returned words of 16, 15, 15 and 13 moves, and the phase-3
// searches that followed differed by more than a factor of three in nodes --
// one finished at depth 13 where another had not finished at 14.
//
// The rotations were the cascade's way of getting phase 3 a second chance, and
// the measurement that prompted this says they are a dear one: they re-run
// phases 1 and 2 and land in four unrelated positions, so nothing carries over
// between them and three of the four attempts are usually wasted whole.
// Different phase-2 solutions in ONE rotation are the cheap version of the same
// idea -- phase 2 costs a tenth of a second against phase 3's forty.
//
// This filter is the mechanism. `accept` is called when the search reaches the
// goal, and answering false sends it looking for another way there, which is
// already how Phase2SolutionFilter refuses a solution phase 3 could not pair
// from. So a filter that records the state and then refuses it turns "the first
// solution" into "every solution", up to the number asked for.
//
// It wraps the real filter rather than replacing it: a solution that leaves the
// wings unpairable is no more use collected than it was returned, and must
// still be refused before it is recorded.
struct Phase2SolutionCollector : public SolutionFilter {
  const SolutionFilter* inner;      // the real acceptance test, may be null
  size_t want;                      // stop refusing once this many are held
  mutable std::vector<std::vector<int> > words;   // move indices, per solution
  mutable std::vector<PieceState> states;         // where each one arrived

  Phase2SolutionCollector(const SolutionFilter* inner_, size_t want_)
    : inner(inner_), want(want_) {}

  // Only the word-carrying form is implemented with any content: a collector
  // that is handed a state without the moves that reached it has nothing it can
  // hand back, so being called through plain accept() would silently collect
  // nothing. It refuses instead, which cannot be mistaken for having worked.
  bool accept(const PieceState&) const { return false; }

  bool accept_word(const PieceState& s, const std::vector<int>& word) const {
    if (inner && !inner->accept(s)) return false;

    // The same goal state can be reached by different words at the same depth.
    // Recording it twice would spend a phase-3 search proving what the first
    // copy already proved, so only distinct states count towards `want`.
    for (size_t i = 0; i < states.size(); i++) {
      if (states[i].perm == s.perm && states[i].ori == s.ori) return false;
    }
    states.push_back(s);
    words.push_back(word);

    // Accepting the last one ends the search in the ordinary way, which leaves
    // `out` holding its moves and the solver's bookkeeping consistent with a
    // search that simply found a solution. Refusing every one instead would
    // collect them all and then report failure.
    return states.size() >= want;
  }
};

struct Phase3Deriver4 : public Deriver {
  void derive(const PieceState& in, PieceState& out) const {
    out.perm.assign(TOTAL4, 0);
    out.ori.assign(TOTAL4, 0);
    for (int i = 0; i < N_WINGS; i++) out.perm[W_OFF + i] = in.perm[W_OFF + i];
    canonicalize_wings(out.perm);
    for (int i = 0; i < N_CENTRES; i++) {
      out.perm[Z_OFF + i] = (kociemba::Slot)(in.perm[Z_OFF + i] - Z_OFF);
    }

    // PLL parity, in one bit. Not the parity of the 24 wings -- that is not the
    // quantity a 3x3x3 cannot express. What it cannot express is the corners
    // and the dedges disagreeing, so the bit is the parity of the corners plus
    // the parity of the twelve dedges, each dedge counted once by taking the
    // wing in its primary position.
    //
    // Carrying it in the coordinate is what stops the phase ever returning a
    // solution that would need a parity algorithm afterwards: a state with the
    // wrong bit is simply not the goal.
    const std::vector<char>& primary = wing_position_is_primary();
    const std::vector<int>& dd = wing_to_dedge();
    std::vector<kociemba::Slot> dedges;
    for (int i = 0; i < N_WINGS; i++) {
      if (!primary[i]) continue;
      dedges.push_back((kociemba::Slot)dd[in.perm[W_OFF + i] - W_OFF]);
    }
    const int dedge_parity = basic_parity(dedges, 0, (int)dedges.size());
    out.ori[C_OFF4] = (uint8_t)((corner_parity(in) + dedge_parity) & 1);
  }
};

// ---- The generator lists ---------------------------------------------------
//
// Each phase turns fewer things than the one before, which is what keeps its
// search shallow and stops it undoing the phase before. The names are this
// package's own alphabet: outer faces U R F D L B, inner layers 1x, 2y and so
// on, with ' and 2 composed into single moves where the phase counts them as
// one.
//
// ---- Reading twips's generator lists --------------------------------------
//
// twips writes its phases in the WCA block notation (Regulations 12a2) and this
// package names layers by axis, so the lists below cannot be compared to its
// without translating first. R/cube_wide.R already does that -- use
// cube_expand_move() rather than working it out by hand, which is how the
// primes below came to be missed once:
//
//   Uw -> U 2y      Lw -> L 1x'
//   Rw -> R 2x      Dw -> D 1y'
//   Fw -> F 2z      Bw -> B 1z'
//
// The prime on L, D and B is not a slip. An axis has one positive sense but its
// two faces are seen from opposite sides -- WCA 12a4a puts it as "x, same
// direction as R or L'" -- so the layer that follows R unprimed follows L
// primed. twips's own definition file agrees: it derives Rw as "2R R" and gets
// the rest by conjugation, `Lw = [y2: Rw]`.
//
// Two further things make the counts differ, and neither is a defect:
//
//   * twips lists move *classes* and expands them by the hand metric, so U
//     becomes three moves and F2 stays one. Nine classes is twenty-three moves,
//     not nine.
//   * a block turn is a word of two moves here, never one, so a generator list
//     in this alphabet cannot contain Uw2 -- only U and 2y separately. Measured,
//     U is no power of Uw, so <U, 2y> is strictly larger than <Uw>.
//
// The lists below are twips's, move for move, with the wide turns written as
// words and counted as one step each -- which is what its phase puzzles do,
// defining Rw2 as a move rather than as a pair.
//
// Making that work took a change one level down. The canonical automaton used
// to key its state on (axis, layer), which is well defined only while a move
// turns a single layer; a wide turn is one axis and two layers, so registering
// it under its leading face left the other layer unmarked and the automaton
// refused legal continuations -- measured, a five-move scramble that had
// reduced in a tenth of a second spent its whole budget. kociemba_core.h now
// follows twips's canonical_fsm.rs instead, measuring which move classes
// commute rather than inferring it from geometry, and the same measurement
// decides which moves share a class.

// twips's <Uw, U, Lw, L, Fw, F, Rw, R, Bw, B, Dw, D>: every face by quarters
// and every face's wide turn by quarters, thirty-six moves.
inline std::vector<std::string> phase1_gens4() {
  static const char* g[] = {
    "U","U'","U2","D","D'","D2","L","L'","L2","R","R'","R2",
    "F","F'","F2","B","B'","B2",
    "1x","1x'","1x2","2x","2x'","2x2",
    "1y","1y'","1y2","2y","2y'","2y2",
    "1z","1z'","1z2","2z","2z'","2z2"
  };
  return std::vector<std::string>(g, g + 36);
}

// Phase 2 needs quarter turns of the inner layers on two of the three axes,
// and this is the part that cannot be reasoned out from "phase 1 arranged the
// centres, so do not disturb them". Measured: with the inner layers restricted
// to half turns, phase 2's coordinate is fixed -- breadth-first from the goal
// produces no new state at any depth, because nothing in the generator set
// moves a centre from one axis to another, which is exactly what the phase is
// for. A phase whose coordinate its own moves cannot change either starts at
// the goal or can never reach it, and the search reports "no solution" at once.
//
// twips gives this phase <Uw2, U, L, Fw, F, Rw2, R, B, D>: the F/B inner layer
// turns freely, the U/D and L/R inner layers only by halves. That is what
// preserves the F/B axis phase 1 built while still letting the other four
// centres move between axes, and the same split is written here in this
// package's own alphabet -- z is the F/B axis.
//
// Twenty-eight moves here against twips's twenty-three, for the reasons given
// above the generator lists. Measured, none of the five is spare: breadth-first
// over this phase's derived coordinate to depth five reaches 326448 states with
// all twenty-eight, 232886 with any one inner half turn removed, and 126350
// with either inner z class removed. Dropping any of them would put positions
// out of reach and the phase would answer "no solution" where one exists.
inline std::vector<std::string> phase2_gens4() {
  static const char* g[] = {
    "U","U'","U2","D","D'","D2","L","L'","L2","R","R'","R2",
    "F","F'","F2","B","B'","B2",
    "1x2","2x2","1y2","2y2",
    "1z","1z'","1z2","2z","2z'","2z2"
  };
  return std::vector<std::string>(g, g + 28);
}

// Phase 3 is the deep one -- its coordinate grows by a factor of fifteen per
// level -- so what it may turn matters more here than anywhere else.
//
// twips gives it <Uw2, U, L, Fw2, F2, Rw2, R, B2, D>: nine move classes, which
// its engine expands by the hand metric into seventeen moves -- a quarter-turn
// class becomes three (U, U2, U'), a half-turn class stays one. The twenty
// below is the expanded set, so the comparison to make is twenty against
// seventeen, not twenty against nine. The outer part agrees exactly: U D L R by
// quarters and F2 B2, fourteen moves on both sides. The whole difference is the
// inner layers -- six here, three there.
//
// Getting to seventeen took widening the goal, and the two go together: with a
// single goal the narrow set is wrong, and with twenty-four goals the wide set
// is merely wasteful.
//
// Measured, Lw2 = x2 . Rw2 and 1x2 = x2 . Rw2 . L2 -- the inner-left half turn
// is reachable from this set only with a whole-cube rotation in hand. twips has
// one: its phase-3 puzzle is built on Lv and Dv with every other move a
// conjugate of them. Ours are single layers, so against a single goal the
// seventeen are strictly weaker -- breadth-first over the phase's coordinate
// visits 442932 states in five levels without ever producing 1x2.
//
// Rotating the goal supplies what the generators lack. A solution may now
// finish with the cube turned, which costs nothing: phase 4 squeezes to a
// 3x3x3, and that solver does not care how the cube is oriented.
//
// U and D, L and R keep their quarter turns; F and B do not, because a quarter
// turn there moves a wing between the two halves of a dedge and undoes the
// pairing the phase exists to build.
inline std::vector<std::string> phase3_gens4() {
  // twips's nine classes, expanded by the metric to seventeen moves:
  // <Uw2, U, L, Fw2, F2, Rw2, R, B2, D>. The wide half turns are written as
  // words -- Rw2 is the face and its inner layer -- and cost one step each,
  // which is what makes this the same seventeen and not a longer set.
  // The wide half turns as cube_expand_move() spells them. Which inner layer
  // goes with which face does not follow from the letter -- Uw takes layer 2,
  // Dw layer 1 -- and writing these by hand put layer 1 under Uw2 and Fw2,
  // which made phase 3 search with a generator set that was not the one it
  // documents. test-cube-kociemba4-gens3.R holds all seventeen against the
  // block notation.
  static const char* g[] = {
    "U","U'","U2","D","D'","D2","L","L'","L2","R","R'","R2",
    "F2","B2",
    "U 2y U 2y",          // Uw2
    "R 2x R 2x",          // Rw2
    "F 2z F 2z"           // Fw2
  };
  return std::vector<std::string>(g, g + 17);
}

// ---- Phase 4: hand the reduced cube to the 3x3x3 solver -------------------
//
// Once the centres are built and the wings paired, the cube behaves as a 3x3x3
// and the two-phase solver already in this package finishes it. The bridge is
// cube_reduce.h, which squeezes 96 stickers to 54 and lifts the answer back --
// both measured, both already used by cube_solve4().
//
// Parity is not repaired here, because phase 3 could not have produced it: the
// parity of the wing permutation is part of that phase's coordinate, so a state
// whose parity a 3x3x3 cannot express is not a state phase 3 stops at.

// ---- The solver ------------------------------------------------------------

// Which phase is currently searching, 1..3, for the progress line to name.
// Zero when nothing is running.
inline int& reporting_phase() {
  static int phase = 0;
  return phase;
}

// The ceiling on phase 3's prune table, in slots. Held in a function-local so
// it can be set before the solver is built and read by init() afterwards.
//
// 1<<28 is twips's figure. Measured here it binds: filled breadth first to
// depth 7 the table holds 47.6M entries in 268M slots, and the fill's
// visits-per-write had already gone from 1.87 at depth 6 to 2.95 at depth 7 --
// states colliding, and their successors never expanded. Raising this is one
// of the few dials left, so it is a dial rather than a constant.
inline size_t& phase3_max_size_ref() {
  static size_t v = (size_t)1 << 28;
  return v;
}
inline size_t phase3_max_size() { return phase3_max_size_ref(); }

struct Solver4 {
  PuzzleSpec spec1, spec2, spec3;
  Phase1Deriver4 d1;
  Phase2Deriver4 d2;
  Phase3Deriver4 d3;
  Phase2SolutionFilter f2;
  PruneTable p1, p2, p3;
  std::vector<PieceState> goals1, goals2, goals3;
  bool ready;

  SearchOutcome outcome[3];
  long nodes[3];
  // Seconds each phase's search took, filled by run_phase. CPU time rather
  // than wall clock: the search is single threaded, so they agree, and CPU
  // time does not move when the machine is busy with something else.
  double secs[3];
  // Kept per phase alongside the node counts, for the same reason: a phase
  // reports how much it spent, and this reports whether the table it spent it
  // against was pruning anything.
  kociemba::PruneStats prune_stats[3];

  // What each level of iterative deepening cost, per phase. `secs` above is
  // their sum, and the sum is the number that cannot answer the question the
  // phase actually raises: at limits 12 to 14 against a table filled to 7 the
  // levels differ by the branching factor, so almost all of a phase's seconds
  // belong to its last level, and knowing which level that was is the whole of
  // knowing where the time went.
  std::vector<kociemba::DepthStat> depth_stats[3];

  // The smallest bound the prune table gave any node the phase visited, or -1
  // if it visited none. On a phase that failed this is how close the table
  // thought it ever got, which is the one number that separates "the search
  // was circling the goal and ran out of budget" from "the search never came
  // near it".
  int best_bound[3];

  // The exact centre table, and the adapter the search reads it through.
  //
  // Off by default: it changes the bound phase 3 searches against, and a
  // change of that kind is measured before it becomes the default. Turned on
  // with use_exact_centres, which cube_kociemba4_phase3_cpp exposes.
  CentreTable centres3;
  CentreBound centres3_bound;
  bool use_exact_centres;

  Solver4() : ready(false), use_exact_centres(false) {
    for (int i = 0; i < 3; i++) {
      outcome[i] = SEARCH_NO_SOLUTION; nodes[i] = 0; best_bound[i] = -1;
      secs[i] = 0.0;
    }
  }

  // Phase 3 starts small on purpose, and the reason is the opposite of what it
  // looks like.
  //
  // grow_to() only ever grows: `if (sz <= table.size()) return false`. It sizes
  // the table from the estimated cost of the level about to be searched, so a
  // table that starts *above* that estimate never grows at all. Phase 3 used to
  // start at 1<<24, and 1<<24 is larger than the estimate for every level a
  // search of this budget reaches -- measured, n_grows was 0 on every run, and
  // the table sat at 16.7M slots with its own ceiling of 1<<28 untouched.
  //
  // The cost of that was measured too. At 1<<24 the fill to depth 6 lost 146 of
  // 150 million writes to collisions and left 78% of occupied slots holding the
  // single value 6, so states 23 and 26 moves from the goal were told they were
  // 6 moves away. A too-generous starting size was buying a permanently
  // undersized table.
  //
  // twips starts its hash prune tables at 1<<20 (DEFAULT_MIN_PRUNE_TABLE_SIZE)
  // with the same 1<<28 ceiling on this phase, and lets the estimate carry them
  // up. Same table, same coordinate, same first-writer-wins -- the growth is
  // the part we were not getting.
  void init(size_t t1 = (size_t)1 << 22,
            size_t t2 = (size_t)1 << 24,
            size_t t3 = (size_t)1 << 20) {
    if (ready) return;
    build_spec4(phase1_gens4(), spec1);
    build_spec4(phase2_gens4(), spec2);
    build_spec4(phase3_gens4(), spec3);

    // The goal is the solved cube as *read from its stickers*, not
    // spec.identity(). The two differ where the centres are: a state numbers a
    // centre by its colour, while identity() numbers every slot by itself, and
    // a deriver that reads perm as a colour gets a slot number instead. That
    // makes the goal a state no cube is ever in, and every phase reports
    // "no solution" against it.
    std::vector<int> solved(96);
    for (int i = 0; i < 96; i++) solved[i] = i + 1;
    const PieceState id = from_stickers4(solved);
    goals1.assign(1, id);

    // Phase 3 has twenty-four goals: the solved cube in each of its
    // orientations. Not for the same reason phase 2 has twelve -- that is
    // about centres being indistinguishable -- but because of what the
    // generator set can reach.
    //
    // Measured: the inner-left half turn factors as 1x2 = x2 . Rw2 . L2, so a
    // set holding Rw2 and L2 reaches it only if it can also turn the whole
    // cube. twips's phase 3 can: its puzzle is defined on Lv and Dv, whole-cube
    // rotations, with every other move built from them by conjugation, and that
    // is why nine classes suffice there. Ours are single layers, so without the
    // rotations the set is strictly smaller -- breadth-first over the phase's
    // own coordinate confirms it, 442932 states in five levels and 1x2 not
    // among them.
    //
    // Rotating the goal instead of the generators buys the same thing: a
    // solution may finish with the cube turned, and that is harmless, since
    // phase 4 squeezes to a 3x3x3 whose solver does not care which way round
    // the cube sits.
    //
    // The twenty-four are generated by closing the solved state under x and y
    // rather than written out, so nothing depends on a list being right.
    goals3.clear();
    {
      const cube_search::CubeN& C = cube_search::cube_n(4);
      const std::vector<int> rot_x = cube_search::parse_word("L' 1x 2x R", 4);
      const std::vector<int> rot_y = cube_search::parse_word("D' 1y 2y U", 4);

      std::vector<std::vector<int> > seen;
      std::vector<std::vector<int> > frontier;
      seen.push_back(solved);
      frontier.push_back(solved);

      while (!frontier.empty()) {
        std::vector<std::vector<int> > next;
        for (size_t f = 0; f < frontier.size(); f++) {
          for (int r = 0; r < 2; r++) {
            const std::vector<int>& rot = r == 0 ? rot_x : rot_y;
            std::vector<int> st = frontier[f];
            for (size_t k = 0; k < rot.size(); k++) C.apply(st, rot[k]);
            bool known = false;
            for (size_t s = 0; s < seen.size(); s++) {
              if (seen[s] == st) { known = true; break; }
            }
            if (!known) { seen.push_back(st); next.push_back(st); }
          }
        }
        frontier.swap(next);
      }
      if (seen.size() != 24) {
        throw std::runtime_error("cube4: cube rotations did not close at 24");
      }
      for (size_t s = 0; s < seen.size(); s++) goals3.push_back(from_stickers4(seen[s]));
    }

    // Phase 2 has twelve goals, not one. The centres of a face are
    // indistinguishable, so a great many finished positions are the same cube
    // as far as the phase can tell -- and each of them is a legitimate place to
    // stop. Offering only the solved arrangement makes the phase hunt for one
    // particular member of a class it cannot even tell apart, which is a much
    // narrower search than the phase actually needs; with the solution filter
    // in front of it as well, narrow enough to spend its budget.
    //
    // The twelve words are twips's, in this package's alphabet: a wide turn
    // Lw2 is the outer face and its inner layer together, and y2 is a rotation
    // of the whole cube, which here is the three layers of the y axis.
    goals2.clear();
    {
      static const char* words[12] = {
        "",
        // The wide half turns, spelled as cube_expand_move() spells them:
        //   Lw2 = L 1x' L 1x'   Rw2 = R 2x R 2x    Uw2 = U 2y U 2y
        //   Dw2 = D 1y' D 1y'   Fw2 = F 2z F 2z
        // Which inner layer belongs to which face is not guessable from the
        // letter -- Uw takes layer 2 and Dw layer 1, and the prime on L, D and
        // B comes from WCA 12a4a. Writing these by hand got Uw2, Dw2 and Fw2
        // wrong on the layer number, and phase 2 was offered goals that were
        // not the cubes twips means. tests/testthat/test-cube-kociemba4-goals2.R
        // holds all twelve against their block notation.
        "U U D' D' 1y 1y 2y 2y",                    // y2
        "L 1x' L 1x'",                              // Lw2
        "R 2x R 2x",                                // Rw2
        "U 2y U 2y",                                // Uw2
        "D 1y' D 1y'",                              // Dw2
        "L 1x' L 1x' F 2z F 2z",                    // Lw2 Fw2
        "R 2x R 2x F 2z F 2z",                      // Rw2 Fw2
        "U 2y U 2y F 2z F 2z",                      // Uw2 Fw2
        "D 1y' D 1y' F 2z F 2z",                    // Dw2 Fw2
        "D 1y' D 1y' F 2z F 2z L 1x' L 1x'",        // Dw2 Fw2 Lw2
        "L 1x' L 1x' F 2z F 2z U 2y U 2y"           // Lw2 Fw2 Uw2
      };
      const cube_search::CubeN& C = cube_search::cube_n(4);
      for (int g = 0; g < 12; g++) {
        std::vector<int> st(96);
        for (int i = 0; i < 96; i++) st[i] = i + 1;
        if (words[g][0]) {
          const std::vector<int> w = cube_search::parse_word(words[g], 4);
          for (size_t k = 0; k < w.size(); k++) C.apply(st, w[k]);
        }
        goals2.push_back(from_stickers4(st));
      }
    }

    build_prune_table(spec1, d1, goals1, t1, 0, p1);
    build_prune_table(spec2, d2, goals2, t2, 0, p2);
    build_prune_table(spec3, d3, goals3, t3, 0, p3);

    // Every table needs a ceiling, and the default is not one. PruneTable
    // starts with max_size = 0, and grow_to() reads zero as "no limit" rather
    // than "not set":
    //
    //     if (max_size && sz > max_size) sz = max_size;
    //
    // so a table left at the default grows to whatever the branching estimate
    // asks for. Measured on phase 2: it reached 1<<28 slots -- 256MB -- holding
    // 122568 entries, a fill ratio of 0.0005.
    //
    // The waste is not in the search. A sparse table still answers correctly:
    // a miss returns built_depth + 1, which is a valid bound however few
    // entries there are. It is in the upkeep. grow_to() rebuilds the table with
    // table.assign(sz, 0), and that costs the full size every time it happens,
    // whatever ends up being stored -- and it also throws away every level
    // already filled, so the levels are walked again from scratch.
    //
    // Phases 1 and 2 have small coordinates: centres by axis and a parity bit
    // for phase 2, centres alone for phase 1. Their levels are counted in tens
    // of thousands, and a table of 1<<24 holds them with room to spare.
    p1.max_size = (size_t)1 << 24;
    p2.max_size = (size_t)1 << 24;

    // Phase 3 is allowed to grow its table far past the others: its coordinate
    // is the wings by dedge plus the centres plus the parity bit, which grows
    // by a factor of fifteen a level, and a table capped where phases 1 and 2
    // are capped collides down to a bound that prunes nothing. The ceiling is
    // the one twips gives this phase.
    //
    // This ceiling was here and unreachable until 2026-08-13: the table started
    // at 1<<24, grow_to() only grows, and no level's estimate exceeded 1<<24
    // within the budgets being run, so n_grows stayed 0 and the table never
    // left its starting size. See init() for the starting size that lets the
    // estimate carry it up here.
    // Settable, because 1<<28 is twips's figure rather than one measured on
    // this implementation, and the measurements here suggest it binds. Filled
    // breadth first to depth 7 the table holds 47.6M entries in 268M slots --
    // 17.7% -- and at that occupancy the fill's visits-per-write had already
    // climbed from 1.87 to 2.95, which is states being lost to collisions and
    // their successors never expanded. A larger table would keep them.
    // cube_kociemba4_set_table_size_cpp() is how a measurement moves it.
    p3.max_size = phase3_max_size();

    ready = true;
  }

  // Build the exact centre table, once. Measured at 58,800 arrangements
  // (diag_centre_coord.R), so this is quick and small -- unlike the wing
  // coordinate, which is exact too but wants 228 MB and 324 s and is not built
  // here.
  void ensure_centre_table() {
    init();
    if (centres3.ready) return;
    centres3.build(spec3, d3, goals3);
    centres3_bound.tab = &centres3;
  }

  // Run one phase and carry the cube through it.
  // `trace`, when given, collects the state the phase was in after each of its
  // moves, in the search's own representation. The starting state is not
  // included: the caller already holds it.
  //
  // On success those are the moves of the solution. On failure they are the
  // branch that came closest -- see Searcher::best_word -- and `best_names`
  // gets the move names to go with them, since `out` is left untouched when a
  // phase fails. Read them as where the search went, not as progress towards
  // a solution: the branch is chosen by a prune-table estimate, and the table
  // collides.
  bool run_phase(int which, PuzzleSpec& spec, Deriver& dv, PruneTable& pt,
                 std::vector<PieceState>& goals, PieceState& cur,
                 const SearchLimits& lim, std::vector<std::string>& out,
                 const SolutionFilter* filter = 0,
                 kociemba::ProgressFn progress = 0,
                 std::vector<PieceState>* trace = 0,
                 std::vector<std::string>* best_names = 0,
                 kociemba::DepthDoneFn depth_done = 0) {
    // Which phase is running is what the progress line most needs to say, and
    // a plain function pointer cannot carry it. A file-scope variable does,
    // and this solver is a singleton run from one thread.
    reporting_phase() = which + 1;
    std::vector<int> w;
    std::vector<int> best_w;
    // The exact centre table applies to phase 3 only: it is built from that
    // phase's generators and goals, and means nothing about the two before it.
    const kociemba::ExtraBound* extra =
      (which == 2 && use_exact_centres && centres3.ready) ? &centres3_bound : 0;

    // Timed here rather than around the call in R, because the two are not the
    // same measurement. Running the phases separately from outside means
    // running phase 1 twice to price phase 2 by subtraction, and the second run
    // is a different search -- phase 1 under a phase-2 depth limit can return a
    // different prefix of the same length. The seconds then belong to two
    // searches and the moves to neither. Inside one reduce() there is only the
    // search that actually ran.
    const double t_start = (double)clock() / (double)CLOCKS_PER_SEC;

    // Cleared rather than appended to: the solver is a singleton, so a phase
    // run twice would otherwise report the levels of both runs as one.
    depth_stats[which].clear();

    outcome[which] = ida_search_outcome(spec, dv, pt, goals, cur, lim, w,
                                        &nodes[which], &pt, filter, progress,
                                        &prune_stats[which], &best_w,
                                        &best_bound[which], extra,
                                        &depth_stats[which], depth_done);

    secs[which] = (double)clock() / (double)CLOCKS_PER_SEC - t_start;

    const std::vector<uint8_t> omod_ = spec.ori_mod();
    if (outcome[which] != SEARCH_FOUND) {
      // No solution, but the search still went somewhere, and where it went is
      // the only thing left to look at. Walk the closest branch and hand back
      // the states along it -- on a copy, because a phase that failed must not
      // move the cube on for the phases after it.
      if (trace) {
        PieceState probe = cur, nxt;
        for (size_t i = 0; i < best_w.size(); i++) {
          apply_move(probe, spec.moves[best_w[i]], omod_, nxt);
          probe = nxt;
          trace->push_back(probe);
        }
      }
      if (best_names) {
        for (size_t i = 0; i < best_w.size(); i++) {
          best_names->push_back(spec.move_names[best_w[i]]);
        }
      }
      return false;
    }

    PieceState next;
    for (size_t i = 0; i < w.size(); i++) {
      apply_move(cur, spec.moves[w[i]], omod_, next);
      cur = next;
      out.push_back(spec.move_names[w[i]]);
      if (trace) trace->push_back(cur);
    }
    return true;
  }

  // The three reduction phases. Phase 4 is the 3x3x3 solver and lives on the R
  // side of the boundary, where cube_reduce.h already is.
  // How many of the moves in `out` each phase had contributed by the time it
  // finished. Phases that fail leave their entry at whatever the phase before
  // reached, so a caller can always apply the prefix that did succeed -- which
  // is the state worth inspecting when a later phase gets stuck, and the only
  // way to test one phase's promise without the next one's outcome mixed in.
  size_t moves_after_phase1;
  size_t moves_after_phase2;

  bool reduce(const PieceState& start, std::vector<std::string>& out,
              const SearchLimits& lim1, const SearchLimits& lim2,
              const SearchLimits& lim3,
              kociemba::ProgressFn progress = 0) {
    init();
    out.clear();
    moves_after_phase1 = 0;
    moves_after_phase2 = 0;
    for (int i = 0; i < 3; i++) {
      outcome[i] = SEARCH_NO_SOLUTION; nodes[i] = 0; secs[i] = 0.0;
      // Cleared here as well as in run_phase: a phase that this call never
      // reaches never clears its own, and would report the levels of whichever
      // earlier call last ran it as though they belonged to this one.
      depth_stats[i].clear();
    }

    PieceState cur = start;
    if (!run_phase(0, spec1, d1, p1, goals1, cur, lim1, out, 0, progress)) return false;
    moves_after_phase1 = out.size();
    // Phase 2 carries the filter: reaching its goal is not enough if it leaves
    // the wings in a position phase 3 cannot pair from.
    if (!run_phase(1, spec2, d2, p2, goals2, cur, lim2, out, &f2, progress)) return false;
    moves_after_phase2 = out.size();
    if (!run_phase(2, spec3, d3, p3, goals3, cur, lim3, out, 0, progress)) return false;
    return true;
  }

  // Phases 1 and 2, stopping with several phase-2 solutions rather than one.
  //
  // `states` comes back holding the cube as phase 2 left it, once per distinct
  // solution, in the order the search met them -- which is shortest first,
  // since the search deepens. `out` holds the moves of the last one, so a
  // caller that wants only the first solution's word gets the same thing
  // reduce() would have given it.
  //
  // Phase 1 is run once and shared. Its solution is not what varies here, and
  // re-running it per phase-2 solution would pay for the same search twice.
  // `phase1` comes back holding phase 1's moves, and `phase2_words` one entry
  // per distinct phase-2 solution, each a word to be applied after phase 1.
  // The full handover for solution i is phase1 followed by phase2_words[i].
  bool collect_phase2(const PieceState& start, std::vector<std::string>& phase1,
                      const SearchLimits& lim1, const SearchLimits& lim2,
                      size_t want,
                      std::vector<std::vector<std::string> >& phase2_words,
                      kociemba::ProgressFn progress = 0) {
    init();
    phase1.clear();
    phase2_words.clear();
    moves_after_phase1 = 0;
    moves_after_phase2 = 0;
    for (int i = 0; i < 3; i++) {
      outcome[i] = SEARCH_NO_SOLUTION; nodes[i] = 0; secs[i] = 0.0;
      depth_stats[i].clear();
    }
    if (want < 1) want = 1;

    PieceState cur = start;
    if (!run_phase(0, spec1, d1, p1, goals1, cur, lim1, phase1, 0, progress)) {
      return false;
    }
    moves_after_phase1 = phase1.size();

    // run_phase appends phase 2's moves to the word it is given, and here that
    // word is phase 1's. The collector's own solutions are what this call is
    // for, so phase 2 is given a scratch vector and phase 1's word is left as
    // the caller needs it -- the prefix every collected solution follows.
    std::vector<std::string> scratch;
    Phase2SolutionCollector collector(&f2, want);
    const bool ok = run_phase(1, spec2, d2, p2, goals2, cur, lim2, scratch,
                              &collector, progress);
    moves_after_phase2 = moves_after_phase1 + scratch.size();

    for (size_t i = 0; i < collector.words.size(); i++) {
      std::vector<std::string> names;
      for (size_t j = 0; j < collector.words[i].size(); j++) {
        names.push_back(spec2.move_names[collector.words[i][j]]);
      }
      phase2_words.push_back(names);
    }

    // A search that ran out of depth or budget still leaves behind whatever it
    // collected on the way, and those solutions are as usable as the ones a
    // completed search returns -- so having any is success, whatever the search
    // itself concluded.
    return ok || !phase2_words.empty();
  }
};

inline Solver4& solver4() {
  static Solver4 s;
  return s;
}

}  // namespace kociemba4

#endif  // CAYLEYR_KOCIEMBA4_H
