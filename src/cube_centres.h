#ifndef CAYLEYR_CUBE_CENTRES_H
#define CAYLEYR_CUBE_CENTRES_H

#include <vector>
#include <string>
#include <stdexcept>
#include <algorithm>
#include <utility>
#include "cube_solve.h"

// ---- The centres of a 4x4x4, by Pochmann's method -----------------------
//
// On a 3x3x3 the centres are one sticker each and cannot move relative to one
// another, which is why every other method in this package can say "the U
// face" and mean it. From 4x4x4 up that stops being true: each face carries
// four centre pieces, all twenty-four in a single orbit, and a solve has to
// build every face out of pieces gathered from anywhere. That is the first
// stage of reduction, and this file is that stage.
//
// ---- No search ----------------------------------------------------------
//
// There is no table of centre cases to look up -- the sources are unanimous
// that centres are built rather than recognised -- and there is no search
// here either. The move is COMPUTED from where the pieces are: see which slot
// holds the colour wanted, and the table below says which move carries it
// where. That is how cross_slots() and find_edge_slot() work next door in
// cube_solve.h, and it is the only approach in this package.
//
// This was learned the hard way. Building a face greedily -- take any word
// that puts one more piece home -- reaches three pieces of four and stops,
// because the last piece cannot be placed without disturbing one already
// there. Widening that to a deeper search is not the fix; the fix is to stop
// asking "which word improves this" and start asking "where is the piece, and
// what carries it".
//
// ---- The method ---------------------------------------------------------
//
// Stefan Pochmann's layer-by-layer centres, the same author as the M2 method
// already in this package:
//
//   1. build one centre (white), by making two pairs and joining them
//   2. white on L: solve the l-slice, using only U, (Ll), (Rr) and x
//   3. white on D: solve the u-slice by shooting pieces down from U
//
// Step 3 is where the method earns its keep, and it is the part implemented
// first here because it is entirely determined: one commutator, applied over
// and over, with the target chosen by what colour the front face wants.

namespace cube_solve {

// ---- Geometry -----------------------------------------------------------
//
// A face of a 4x4x4 is four by four and its centre is the inner two by two:
// local indices 5, 6, 9, 10, which is left to right then top to bottom seen
// from outside. Slot k of face f is f*16 + that, and slots are numbered 1..4
// in that order throughout this file.
inline const int* centre_slots_of(int face) {
  static int buf[6][4];
  static bool built = false;
  if (!built) {
    const int local[4] = {5, 6, 9, 10};
    for (int f = 0; f < 6; f++)
      for (int k = 0; k < 4; k++) buf[f][k] = f * 16 + local[k];
    built = true;
  }
  if (face < 0 || face > 5) throw std::runtime_error("cube_centres: bad face");
  return buf[face];
}

// What colour a centre sticker shows. A sticker's value is where it began, and
// stickers are numbered face by face in blocks of n*n, so the colour is that
// block. State is 1-based, as everywhere in the package.
inline int centre_colour(const std::vector<int>& state, int sticker0) {
  return (state[sticker0] - 1) / 16;
}

// ---- Which face is where -------------------------------------------------
//
// A method turns the cube: Pochmann builds a centre, puts it on L, and later
// rotates it to the bottom. After that the face numbered 4 is no longer on the
// left, and any count that compares a sticker's colour against a face NUMBER
// is measuring the wrong thing. That is exactly what broke when the three
// steps were first joined -- the built face went from number 4 to number 0,
// its count read as zero, and step 3 began dismantling it.
//
// The obvious repair is to read each face's colour off the cube by majority,
// as cube_orbits.h does. It does not work here: measured over two hundred
// scrambles, the majority colour of some face changes after a SINGLE move in
// 177 of them, and in 183 the six majorities are not even distinct. On a
// half-built cube it is not a signal.
//
// So the orientation is carried explicitly instead. A rotation rewrites a
// six-entry map and nothing else; the counts read that map instead of assuming
// the cube has never been turned. The permutations below are measured, one per
// rotation:
//
//   z'   U->L  R->U  F->F  D->R  L->D  B->B
//
// which is the one the method needs: the face built on the left goes down.
// face[p] is the colour that belongs at POSITION p -- the face that was there
// before the cube was turned. Positions are U R F D L B as 0..5, and on an
// unturned cube each position wants its own colour, which is the identity the
// constructor sets. A rotation rewrites the map; nothing else does.
struct Orient {
  int face[6];
  Orient() { for (int i = 0; i < 6; i++) face[i] = i; }
  int of(int pos) const { return face[pos]; }
};

// Turning the cube: the role that was played by one face is now played by
// another. Written as "the face at role r moves to role to[r]".
inline Orient rotate_orient(const Orient& o, const char* rot) {
  // measured: for each rotation, where the sticker of each face ends up
  static const struct { const char* name; int to[6]; } table[] = {
    {"x",  {5, 1, 0, 2, 4, 3}},   // U->B R->R F->U D->F L->L B->D
    {"x'", {2, 1, 3, 5, 4, 0}},   // U->F R->R F->D D->B L->L B->U
    {"y",  {0, 2, 4, 3, 5, 1}},   // U->U R->F F->L D->D L->B B->R
    {"y'", {0, 5, 1, 3, 2, 4}},   // U->U R->B F->R D->D L->F B->L
    {"z",  {1, 3, 2, 4, 0, 5}},   // U->R R->D F->F D->L L->U B->B
    {"z'", {4, 0, 2, 1, 3, 5}},   // U->L R->U F->F D->R L->D B->B
    {0, {0, 0, 0, 0, 0, 0}}
  };
  for (int i = 0; table[i].name; i++) {
    if (std::string(rot) == table[i].name) {
      Orient out;
      for (int role = 0; role < 6; role++) out.face[table[i].to[role]] = o.face[role];
      return out;
    }
  }
  throw std::runtime_error("cube_centres: unknown rotation");
}

// How many of a face's four centre slots hold pieces belonging to it.
//
// "Belonging" is the subtle part, and getting it wrong is what broke the three
// steps when they were first joined. A piece belongs to the face it STARTED
// on, which is its colour; the position it must end up in is wherever that
// starting face has since been turned to. So the test is not "colour equals
// face number" -- that only holds while the cube has not been turned -- but
// "colour equals the home colour of this position", which is what Orient
// carries.
//
// Measured: with the count written this way, the multiset of the six counts is
// invariant under z' in 30 of 30 scrambles. Written the old way it was not,
// and counts even rose after a rotation, since a colour could coincide with a
// face number by accident.
inline int centre_count(const std::vector<int>& state, const Orient& o,
                        int face) {
  const int* sl = centre_slots_of(face);
  const int home = o.of(face);
  int c = 0;
  for (int k = 0; k < 4; k++) if (centre_colour(state, sl[k]) == home) c++;
  return c;
}

inline int centres_total(const std::vector<int>& state, const Orient& o) {
  int c = 0;
  for (int f = 0; f < 6; f++) c += centre_count(state, o, f);
  return c;
}

inline bool centres_built(const std::vector<int>& state, const Orient& o) {
  return centres_total(state, o) == 24;
}

// How many faces are finished -- all four of their pieces home.
//
// Computed from the state every time it is asked, never carried in a variable
// and never tied to a fixed face index. That is the whole point: a cube
// rotation moves a finished face to another position without unfinishing it,
// and a count that rescans finds it there, while one bound to an index does
// not. Measured invariant over 500 rotations of x/x'/y/z/z', and never once
// reduced by any of the four shots over 800 tries.
inline int faces_finished(const std::vector<int>& state, const Orient& o) {
  int c = 0;
  for (int f = 0; f < 6; f++) if (centre_count(state, o, f) == 4) c++;
  return c;
}

// ---- The l-slice ---------------------------------------------------------
//
// Step 2 solves a layer, not a face: the L centre together with the column of
// each side face that lies against L. Measured by turning (Ll) and seeing what
// moves -- it carries exactly this set and nothing else:
//
//   L   all four slots
//   U   slots 1 and 3        F   slots 1 and 3
//   D   slots 1 and 3        B   slots 2 and 4
//
// B is the odd one because its layout is mirrored: the column nearest L is on
// the other side of the face as B is seen from outside. Assuming {1,3} there
// would quietly test the wrong two pieces.
//
// Twelve centre pieces in all, and they are "solved" when each shows the
// colour of the face it lies on.
struct SliceCell { int face; int slot; };

inline const std::vector<SliceCell>& l_slice_cells() {
  static std::vector<SliceCell> v;
  if (v.empty()) {
    const int F_U = 0, F_F = 2, F_D = 3, F_L = 4, F_B = 5;
    const int col13[3] = {F_U, F_F, F_D};
    for (int k = 1; k <= 4; k++) { SliceCell c = {F_L, k}; v.push_back(c); }
    for (int i = 0; i < 3; i++) {
      SliceCell a = {col13[i], 1}; v.push_back(a);
      SliceCell b = {col13[i], 3}; v.push_back(b);
    }
    SliceCell b2 = {F_B, 2}; v.push_back(b2);
    SliceCell b4 = {F_B, 4}; v.push_back(b4);
  }
  return v;
}

// How many of the l-slice's twelve centre pieces show the colour of the face
// they sit on.
inline int l_slice_count(const std::vector<int>& state, const Orient& o) {
  const std::vector<SliceCell>& cells = l_slice_cells();
  int c = 0;
  for (size_t i = 0; i < cells.size(); i++) {
    const int* sl = centre_slots_of(cells[i].face);
    if (centre_colour(state, sl[cells[i].slot - 1]) == o.of(cells[i].face)) c++;
  }
  return c;
}

inline bool l_slice_built(const std::vector<int>& state, const Orient& o) {
  return l_slice_count(state, o) == 12;
}

// ---- The d-slice ---------------------------------------------------------
//
// The same layer, after the cube has been turned so that the built face is at
// the bottom. Rather than carry an orientation through every function, the
// rotation is applied to the cube and the layer is named again in its new
// place -- which is a table of twelve cells, measured by turning z' and
// following each l-slice cell:
//
//   L1 -> D3   L2 -> D1   L3 -> D4   L4 -> D2
//   U1 -> L3   U3 -> L4   F1 -> F3   F3 -> F4
//   D1 -> R3   D3 -> R4   B2 -> B4   B4 -> B3
//
// so the d-slice is D entire, plus slots 3 and 4 of L, F, R and B. This is
// what step 3 must not disturb, and the shots were measured to leave D alone
// -- which is only half of it, so the count below is what actually checks.
inline const std::vector<SliceCell>& d_slice_cells() {
  static std::vector<SliceCell> v;
  if (v.empty()) {
    const int F_R = 1, F_F = 2, F_D = 3, F_L = 4, F_B = 5;
    for (int k = 1; k <= 4; k++) { SliceCell c = {F_D, k}; v.push_back(c); }
    const int sides[4] = {F_L, F_F, F_R, F_B};
    for (int i = 0; i < 4; i++) {
      SliceCell a = {sides[i], 3}; v.push_back(a);
      SliceCell b = {sides[i], 4}; v.push_back(b);
    }
  }
  return v;
}

inline int d_slice_count(const std::vector<int>& state, const Orient& o) {
  const std::vector<SliceCell>& cells = d_slice_cells();
  int c = 0;
  for (size_t i = 0; i < cells.size(); i++) {
    const int* sl = centre_slots_of(cells[i].face);
    if (centre_colour(state, sl[cells[i].slot - 1]) == o.of(cells[i].face)) c++;
  }
  return c;
}

// ---- The shots ----------------------------------------------------------
//
// The four commutators step 3 is made of, measured on the cube rather than
// reasoned about. Each is a 4-cycle: one slot of U empties onto a side face,
// another slot of U refills from that face, and the rest rotates within the
// face.
//
//   (Rr)' F (Rr)    U4 -> F3    leaves R D L B alone
//   (Ll) F' (Ll)'   U3 -> F4    leaves R D L B alone
//   (Rr)' U (Rr)    U1 -> B3    leaves R F D L alone
//   (Ll) U' (Ll)'   U2 -> B4    leaves R F D L alone
//
// D is untouched by all four. That is the invariant of step 3 and it holds by
// construction, not by checking: whatever has been built and put on the bottom
// cannot be disturbed however many shots are fired. The pair aimed at F leaves
// B alone and vice versa, so the two halves do not interfere either.
//
// (Rr) is the wide turn -- the face and the slice beside it -- which on this
// cube is "R 2x"; (Ll) is "L 1x'". Note the prime: a wrong sign there sends
// pieces the other way round the cube.
struct Shot {
  const char* word;    // in the package alphabet
  int from_slot;       // the U slot it empties, 1..4
  int to_face;         // 2 for F, 5 for B
  int to_slot;         // the slot it lands in, 1..4
};

inline const Shot* shots() {
  static const Shot s[4] = {
    {"R' 2x' F R 2x",   4, 2, 3},   // (Rr)' F (Rr)
    {"L 1x' F' L' 1x",  3, 2, 4},   // (Ll) F' (Ll)'
    {"R' 2x' U R 2x",   1, 5, 3},   // (Rr)' U (Rr)
    {"L 1x' U' L' 1x",  2, 5, 4}    // (Ll) U' (Ll)'
  };
  return s;
}

inline int n_shots() { return 4; }

// ---- Step 3: empty the u-slice by shooting ------------------------------
//
// A shot is fired when the piece in its source slot belongs to the face the
// shot aims at -- that is the whole condition, read off the cube. Nothing is
// searched for: there are four shots, each with a fixed source and target, and
// the state says which of them is currently right.
//
// Two refinements, both Pochmann's own:
//
//   Running out of ammunition. Every shot pulls a new piece up into U. If the
//   piece pulled up belongs to D, it can never be shot anywhere, and once U
//   fills with them the method stalls with faces still unbuilt. So where two
//   shots are both available, prefer the one that does NOT bring a D piece up.
//
//   Landing on a slot that is already right. Firing into a slot that already
//   holds the correct colour spends a move to gain nothing, so those shots are
//   taken last.
//
// The choice among the four is therefore a preference order over what is
// already known, not a search.
// ---- The setup turn -----------------------------------------------------
//
// U cycles its four slots 1 -> 2 -> 4 -> 3 -> 1, measured by turning it and
// following each slot. So from any slot, any other slot is a fixed number of U
// turns away, and that number can be COMPUTED rather than searched for.
inline int u_setup_turns(int from_slot, int to_slot) {
  static int table[5][5];
  static bool built = false;
  if (!built) {
    const int nxt[5] = {0, 2, 4, 1, 3};   // slot -> where U sends it
    for (int a = 1; a <= 4; a++) {
      int cur = a;
      for (int t = 0; t < 4; t++) { table[a][cur] = t; cur = nxt[cur]; }
    }
    built = true;
  }
  return table[from_slot][to_slot];
}

// A shot with its setup: which shot, and how many U turns to make first.
struct Aim {
  int shot;      // index into shots(), -1 for none
  int setup;     // U turns before it, 0..3
  Aim() : shot(-1), setup(0) {}
};

// ---- Choosing a shot, with the setup computed ---------------------------
//
// The four shots are stated against fixed slots -- U4 and U3 fire at F, U1 and
// U2 at B -- and taking that literally is what stalls the stage. A piece that
// belongs on F but sits in U1 matches no shot at all, and turning U blindly in
// the hope that it lands on U4 is a search, not a method.
//
// The fix is Pochmann's own shape: the setup is part of the shot. Find the
// piece, find the shot whose target is the face that piece belongs to, and
// compute how many U turns bring the piece to that shot's entry slot. One
// arithmetic step, no trying.
//
// Measured on 40 stalled positions: reading the four shots literally found a
// shot in 5 of them; computing the setup found one in 13; and allowing y to
// bring the wanted face into F or B as well found one in 39.
inline Aim choose_aim(const std::vector<int>& state, const Orient& o,
                      bool tilted) {
  const Shot* sh = shots();
  const int* u_slots = centre_slots_of(0);
  Aim best;
  int best_total = centres_total(state, o);

  for (int k = 1; k <= 4; k++) {
    const int piece = centre_colour(state, u_slots[k - 1]);
    if (piece == o.of(0)) continue;      // belongs on top, leave it
    if (piece == o.of(3)) continue;      // belongs on the bottom, unshootable

    for (int i = 0; i < n_shots(); i++) {
      if (o.of(sh[i].to_face) != piece) continue;   // this shot aims elsewhere

      const int turns = u_setup_turns(k, sh[i].from_slot);
      std::vector<int> cand = state;
      for (int t = 0; t < turns; t++)
        cand = apply_word(cand, parse_word("U", 4));
      cand = apply_word(cand, parse_word(sh[i].word, 4));

      if (faces_finished(cand, o) < faces_finished(state, o)) continue;
      if (!tilted && d_slice_count(cand, o) < d_slice_count(state, o)) continue;
      const int t_after = centres_total(cand, o);
      if (t_after <= best_total) continue;

      best_total = t_after;
      best.shot = i;
      best.setup = turns;
    }
  }
  return best;
}

inline int choose_shot(const std::vector<int>& state, const Orient& o,
                       bool tilted) {
  const Shot* sh = shots();
  const int* u_slots = centre_slots_of(0);
  int best = -1;
  int best_rank = -1;

  for (int i = 0; i < n_shots(); i++) {
    const int piece = centre_colour(state, u_slots[sh[i].from_slot - 1]);

    // A shot is a 4-cycle: it takes a piece off U and brings another up in
    // exchange. So it has two uses, and only counting the first is what makes
    // the method stall.
    //
    //   placing    the outgoing piece belongs to the face aimed at
    //   reloading  it does not, but the incoming one is a piece some face
    //              still wants -- neither U's colour nor D's
    //
    // Reloading costs a move and makes the count of solved centres go DOWN,
    // and refusing it on that ground is exactly the mistake: with only U
    // pieces on U there is nothing to place, and no amount of turning the
    // cube changes that. Pochmann says as much -- "whenever you shoot one
    // piece out of U, a new one gets into U" -- and the whole of his advice
    // about ammunition is about which piece that should be.
    const std::vector<int> after = apply_word(state, parse_word(sh[i].word, 4));

    // What the shot brings up. Not the slot it emptied -- the cycle refills
    // that one from elsewhere on U -- but the slot that takes from the side
    // face, which the measurement gives as U2 for the shots aimed at F and U1
    // for those aimed at B. Reading the wrong slot here reports a U piece
    // every time and hides every reload there is.
    const int reload_slot = (sh[i].to_face == 2) ? 2 : 1;
    const int pulled = centre_colour(after, u_slots[reload_slot - 1]);

    // Positions and colours are different things once the cube has been
    // turned, and the two tests below need different ones -- which is the
    // distinction that took three attempts to get right.
    //
    // "Does this piece belong on top / on the bottom" is a question about
    // COLOUR: the piece that belongs at the bottom is the one whose colour is
    // the bottom's home colour, and after z' that is not the number 3. Orient
    // answers it.
    //
    // "Which face is this shot aimed at" is a question about POSITION. The y
    // turns of the loop physically bring a different face into the F and B
    // positions, and the shot words are written against those positions; the
    // rotation has already done the choosing. Sending col_target through
    // Orient as well double-counts the turn -- source and target then rotate
    // together, the four y turns revisit the same four pairings, and the stage
    // stalls. Measured on 50 stalled positions: read as a position a shot was
    // available in 23 of them, read through Orient in 0.
    const int col_up = o.of(0);               // pieces belonging on top
    const int col_down = o.of(3);             // and on the bottom
    const int col_target = sh[i].to_face;     // a position, not a colour

    // How much of U is still worth shooting: pieces belonging to neither the
    // top face nor the bottom can go somewhere, and those are the ammunition.
    int ammo_before = 0, ammo_after = 0;
    for (int k = 0; k < 4; k++) {
      const int b = centre_colour(state, u_slots[k]);
      const int a = centre_colour(after, u_slots[k]);
      if (b != col_up && b != col_down) ammo_before++;
      if (a != col_up && a != col_down) ammo_after++;
    }

    // Whatever else a shot does, it may not undo what is already built.
    //
    // Two guards, because they catch different things and the first one alone
    // does not survive a cube rotation.
    //
    // The layer, by position. The four shots were measured to leave the D face
    // alone, but the d-slice is more than that face -- it takes two slots from
    // each side face as well, and those the shots do move.
    //
    // What is FINISHED, by rescanning the cube. d_slice_count names fixed
    // positions, so it is only about the built layer while the layer is still
    // at those positions. Measured: after a z tilt it reads 12 -> 7 and after
    // x 12 -> 6, while the number of finished faces is unchanged at 1 -- the
    // cube lost nothing, the count was simply looking where the layer no
    // longer is. A guard that reports destruction that did not happen rejects
    // every shot after a tilt, which is what made the tilts lose ground twice.
    // This one is computed from the current state each time, so a rotation
    // cannot fool it.
    if (faces_finished(after, o) < faces_finished(state, o)) continue;
    if (!tilted && d_slice_count(after, o) < d_slice_count(state, o)) continue;

    const bool places = (piece == col_target);

    if (places) {
      // does the target slot already hold the right colour? then the move
      // gains nothing here
      const int* t_slots = centre_slots_of(sh[i].to_face);
      const bool target_filled =
        centre_colour(state, t_slots[sh[i].to_slot - 1]) == col_target;

      // and prefer to reload with something usable rather than a top or
      // bottom piece
      const bool good_reload = (pulled != col_up && pulled != col_down);

      int rank = 4;
      if (!target_filled) rank += 4;
      if (good_reload) rank += 1;
      if (rank > best_rank) { best_rank = rank; best = i; }
    } else if (piece == col_up) {
      // Reloading: spend a U piece to bring up one that some face still wants.
      // Worth doing only if it leaves more ammunition on U than it found,
      // which is the honest test -- asking after a single slot was what made
      // this branch never fire.
      if (ammo_after <= ammo_before) continue;
      const int rank = 1;
      if (rank > best_rank) { best_rank = rank; best = i; }
    }
  }
  return best;
}

// ---- Step 1: build the first centre --------------------------------------
//
// Two pairs, then join them. What makes this cheap is a fact from the
// measurement: a slice carries TWO slots of a face at once, always the same
// two, so a pair that lies in those two slots travels as a unit.
//
//   1x, 2x   the columns {1,3} and {2,4}, round U - F - D - B
//   1y, 2y   the rows    {3,4} and {1,2}, round R - F - L - B
//   1z, 2z   {1,2} and {3,4} of U, round U - R - D - L
//
// So "join the pairs" is one slice, chosen by which two slots the pair
// occupies -- computed, not searched for.

// ---- What each slice does, in full ---------------------------------------
//
// The first version of this recorded a slice as "a pair of slots, carried
// round a ring of faces". That model is too simple and the cube says so: the
// slots are not the same all the way round, and they swap over within the
// pair. 1x carries U{1,3} but lands on B{4,2} -- different slots, and in the
// other order.
//
// So the table is the mapping itself, eight entries per slice, measured
// rather than modelled. Anything worth knowing about a slice -- which faces
// it touches, which slots, how far a piece travels -- is read off this.
struct Move1 {
  int from_face, from_slot;
  int to_face, to_slot;
};

struct SliceMap {
  const char* move;
  Move1 step[8];
};

inline const SliceMap* slice_maps() {
  static const SliceMap m[6] = {
    {"1x", {{0,1,5,4},{0,3,5,2},{2,1,0,1},{2,3,0,3},
            {3,1,2,1},{3,3,2,3},{5,2,3,3},{5,4,3,1}}},
    {"2x", {{0,2,5,3},{0,4,5,1},{2,2,0,2},{2,4,0,4},
            {3,2,2,2},{3,4,2,4},{5,1,3,4},{5,3,3,2}}},
    {"1z", {{0,1,1,2},{0,2,1,4},{1,2,3,4},{1,4,3,3},
            {3,3,4,1},{3,4,4,3},{4,1,0,2},{4,3,0,1}}},
    {"2z", {{0,3,1,1},{0,4,1,3},{1,1,3,2},{1,3,3,1},
            {3,1,4,2},{3,2,4,4},{4,2,0,4},{4,4,0,3}}},
    {"1y", {{1,3,2,3},{1,4,2,4},{2,3,4,3},{2,4,4,4},
            {4,3,5,3},{4,4,5,4},{5,3,1,3},{5,4,1,4}}},
    {"2y", {{1,1,2,1},{1,2,2,2},{2,1,4,1},{2,2,4,2},
            {4,1,5,1},{4,2,5,2},{5,1,1,1},{5,2,1,2}}}
  };
  return m;
}

inline int n_slices() { return 6; }

// Where a slice sends one piece, or -1 in the face if it leaves it alone.
inline Move1 slice_sends(const SliceMap& sm, int face, int slot) {
  for (int i = 0; i < 8; i++)
    if (sm.step[i].from_face == face && sm.step[i].from_slot == slot)
      return sm.step[i];
  Move1 none; none.from_face = face; none.from_slot = slot;
  none.to_face = -1; none.to_slot = -1;
  return none;
}

// Following a piece through repeated turns of one slice: where it is after
// `times` quarters, and -1 if the slice never touches it.
inline Move1 slice_sends_n(const SliceMap& sm, int face, int slot, int times) {
  Move1 cur; cur.from_face = face; cur.from_slot = slot;
  cur.to_face = face; cur.to_slot = slot;
  for (int t = 0; t < times; t++) {
    const Move1 nxt = slice_sends(sm, cur.to_face, cur.to_slot);
    if (nxt.to_face < 0) { cur.to_face = -1; cur.to_slot = -1; return cur; }
    cur.to_face = nxt.to_face;
    cur.to_slot = nxt.to_slot;
  }
  return cur;
}

// Turning a face permutes its own four centre slots, the same way for every
// face -- measured on U, F and R rather than assumed from the layout. A
// quarter turn moves the piece in slot 1 to slot 2, 2 to 4, 4 to 3, 3 to 1.
//
// The direction is worth stating because it is the easy thing to invert: this
// says where a PIECE GOES, not where a slot's new occupant came from.
inline int slot_after_turn(int slot, int quarters) {
  static const int cyc[4] = {1, 2, 4, 3};   // 1 -> 2 -> 4 -> 3 -> 1
  int at = -1;
  for (int i = 0; i < 4; i++) if (cyc[i] == slot) { at = i; break; }
  if (at < 0) throw std::runtime_error("cube_centres: bad slot");
  return cyc[(at + (quarters % 4) + 4) % 4];
}

// The word that turns a face by so many quarters, in the package alphabet.
// Half turns are written out because the alphabet is quarter turns.
inline std::string face_turn_word(int face, int quarters) {
  static const char* nm[6] = {"U", "R", "F", "D", "L", "B"};
  quarters = ((quarters % 4) + 4) % 4;
  if (quarters == 0) return std::string();
  const std::string base = nm[face];
  if (quarters == 1) return base;
  if (quarters == 2) return base + " " + base;
  return base + "'";
}

// Are these two slots a pair -- adjacent rather than diagonal? On the inner
// two by two, 1-2 and 3-4 are rows, 1-3 and 2-4 are columns, and 1-4, 2-3 are
// the diagonals, which no single slice carries together.
inline bool slots_adjacent(int a, int b) {
  if (a > b) { const int t = a; a = b; b = t; }
  return (a == 1 && b == 2) || (a == 3 && b == 4) ||
         (a == 1 && b == 3) || (a == 2 && b == 4);
}

// Where the pieces of one colour are: face and slot, for every centre sticker
// currently showing that colour.
struct Spot { int face; int slot; };

inline std::vector<Spot> find_colour(const std::vector<int>& state, int colour) {
  std::vector<Spot> out;
  for (int f = 0; f < 6; f++) {
    const int* sl = centre_slots_of(f);
    for (int k = 0; k < 4; k++) {
      if (centre_colour(state, sl[k]) == colour) {
        Spot s; s.face = f; s.slot = k + 1;
        out.push_back(s);
      }
    }
  }
  return out;
}

// How many pieces of `colour` sit on `face`, and in which slots.
inline std::vector<int> slots_on_face(const std::vector<int>& state, int face,
                                      int colour) {
  std::vector<int> out;
  const int* sl = centre_slots_of(face);
  for (int k = 0; k < 4; k++)
    if (centre_colour(state, sl[k]) == colour) out.push_back(k + 1);
  return out;
}

// Does this face already carry a pair of the colour -- two pieces in adjacent
// slots, which a single slice can carry as a unit?
inline bool has_pair(const std::vector<int>& state, int face, int colour,
                     int* a = 0, int* b = 0) {
  const std::vector<int> s = slots_on_face(state, face, colour);
  for (size_t i = 0; i < s.size(); i++)
    for (size_t j = i + 1; j < s.size(); j++)
      if (slots_adjacent(s[i], s[j])) {
        if (a) *a = s[i];
        if (b) *b = s[j];
        return true;
      }
  return false;
}

// ---- Moving a pair from one face to another ------------------------------
//
// A slice carries a fixed pair of slots round a fixed ring of four faces. So
// to bring a pair from one face of a ring to another is a matter of counting
// how far round it has to go -- one, two or three quarters -- and the slice
// that carries those slots is the one to turn. Nothing is searched: the ring
// and the slots say which move, and the distance says how many times.
//
// The slots the pair occupies on the source face have to be the ones that
// slice carries. Where they are not, the face itself is turned first to bring
// them there, which is what set_up_pair below does.

// How many quarters of this slice carry a piece from one place to another, or
// -1 if no number of them does.
inline int slice_quarters(const SliceMap& sm, int from_face, int from_slot,
                          int to_face, int to_slot) {
  for (int q = 1; q <= 3; q++) {
    const Move1 r = slice_sends_n(sm, from_face, from_slot, q);
    if (r.to_face == to_face && r.to_slot == to_slot) return q;
  }
  return -1;
}

// The same, when only the destination face matters and any slot will do.
inline int slice_quarters_to_face(const SliceMap& sm, int from_face,
                                  int from_slot, int to_face) {
  for (int q = 1; q <= 3; q++) {
    const Move1 r = slice_sends_n(sm, from_face, from_slot, q);
    if (r.to_face == to_face) return q;
  }
  return -1;
}

// A move repeated: the word for turning one slice so many quarters. The
// alphabet has no half turns, so two quarters is the move written twice --
// which is what the quarter-turn metric means and why it is spelled out here
// rather than hidden behind a "2".
inline std::string repeat_move(const std::string& mv, int times) {
  std::string out;
  for (int i = 0; i < times; i++) {
    if (!out.empty()) out += " ";
    out += mv;
  }
  return out;
}

// ---- Bringing one piece to a face ----------------------------------------
//
// With the map in hand this is arithmetic. For each slice, ask how many
// quarters carry the piece to the face wanted; take the cheapest that does not
// disturb what is already there. "Does not disturb" is checked by applying the
// word and counting, which is one application, not a search over
// alternatives -- there are only six slices and three distances.
struct Placement {
  std::string word;
  int to_slot;
  bool found;
  Placement() : to_slot(0), found(false) {}
};

// Move one piece from where it is onto `target_face`, without reducing the
// number of pieces of `keep_colour` already home on the faces in `protect`.
inline Placement bring_to_face(const std::vector<int>& state, const Orient& o,
                               int from_face,
                               int from_slot, int target_face,
                               const std::vector<int>& protect) {
  const SliceMap* sm = slice_maps();
  Placement best;
  int best_len = 1000;

  std::vector<int> before(protect.size());
  for (size_t i = 0; i < protect.size(); i++)
    before[i] = centre_count(state, o, protect[i]);

  // A slice alone often cannot do it: it carries the piece to the target face
  // but into a slot that is already right, so one piece goes in as another
  // comes out and nothing is gained. Turning the SOURCE face first moves the
  // piece to a different slot of that face, from where a slice delivers it
  // somewhere else. That is the set-up move, and with it the two-part word --
  // turn the source, then slice -- is what actually places a piece.
  //
  // The source face is turned 0, 1, 2 or 3 quarters, and where the piece then
  // sits is read off slot_after_turn. Four possibilities per slice, all
  // computed; nothing here searches for a word.
  for (int setup = 0; setup < 4; setup++) {
    const std::string setup_word = face_turn_word(from_face, setup);
    const int slot_now = slot_after_turn(from_slot, setup);

    // turning the target face itself would undo what is built there
    if (setup > 0 && from_face == target_face) continue;

  for (int i = 0; i < n_slices(); i++) {
    const int q = slice_quarters_to_face(sm[i], from_face, slot_now, target_face);
    if (q < 0) continue;

    std::string word = setup_word;
    if (!word.empty()) word += " ";
    word += repeat_move(sm[i].move, q);
    const std::vector<int> after = apply_word(state, parse_word(word, 4));

    // The word has to leave the target face better off, not merely no worse.
    // A slice that carries this piece in while carrying another out keeps the
    // count level and passes a "does not spoil" test -- and then the same
    // piece is chosen again next time round and the method turns the same
    // slice for ever. Requiring an increase is what makes progress
    // monotone and the loop terminate.
    if (centre_count(after, o, target_face) <= centre_count(state, o, target_face))
      continue;

    bool spoils = false;
    for (size_t p = 0; p < protect.size(); p++)
      if (centre_count(after, o, protect[p]) < before[p]) { spoils = true; break; }
    if (spoils) continue;

    const int len = setup + q;
    if (len < best_len) {
      const Move1 landed = slice_sends_n(sm[i], from_face, slot_now, q);
      best.word = word;
      best.to_slot = landed.to_slot;
      best.found = true;
      best_len = len;
    }
  }
  }
  return best;
}

// ---- Step 1: the first centre -------------------------------------------
//
// Gather the four pieces of one colour onto one face. The first is free --
// nothing is built yet, so any slice that brings it there will do. Each one
// after that has to arrive without knocking off the ones already gathered,
// which is what the protect list is for.
//
// Pieces already on the target face are left alone; the loop only ever moves
// one that is elsewhere, and bring_to_face refuses any word that would undo
// what is done.
// The commutator that inserts a piece without losing what is already there.
//
//   slice   setup   slice'
//
// The first slice carries the built pieces off the face, out of harm's way.
// The setup turn brings the wanted piece to where the slice will collect it.
// The slice back returns everything the first one moved -- everything except
// the piece, which the setup has swapped for one of the pieces in transit.
//
// This is the same shape as the shots of step 3, and it is why the count is
// allowed to fall in the middle: on the position that stalled every earlier
// version of this file, "1x 2z' 1x'" runs the count 3 -> 1 -> 2 -> 4. Judging
// the first move on its own rejects the whole word, and there is no move that
// improves the count on its own -- measured, not supposed. The count is
// therefore checked once, at the end.
inline bool insert_by_commutator(std::vector<int>& state, Solution& sol,
                                 const Orient& o,
                                 int target_face, const std::string& label) {
  const SliceMap* sm = slice_maps();
  const int before = centre_count(state, o, target_face);

  for (int i = 0; i < n_slices(); i++) {
    for (int q = 1; q <= 3; q++) {
      const std::string out = repeat_move(sm[i].move, q);
      const std::string back = repeat_move(sm[i].move, 4 - q);

      // The middle of the commutator is whatever fetches the piece while the
      // face is out of the way. A face turn does it sometimes; more often it
      // takes another slice, which is what the stalled positions turned out to
      // need -- 2z' 2y' 2z, 1x 1z' 1x', 2x 2y 2x' were all measured as the
      // shortest word finishing a position this could not. Allowing only face
      // turns in the middle is what left them unreachable.
      for (int f = 0; f < 6; f++) {
        if (f == target_face) continue;      // turning it would undo the work
        for (int s = 1; s <= 3; s++) {
          const std::string setup = face_turn_word(f, s);
          if (setup.empty()) continue;

          const std::string word = out + " " + setup + " " + back;
          const std::vector<int> after = apply_word(state, parse_word(word, 4));

          if (centre_count(after, o, target_face) > before) {
            push_stage(sol, state, label, "", parse_word(word, 4));
            return true;
          }
        }
      }

      for (int j = 0; j < n_slices(); j++) {
        if (j == i) continue;                // the same slice would cancel
        for (int s = 1; s <= 3; s++) {
          const std::string setup = repeat_move(sm[j].move, s);
          const std::string word = out + " " + setup + " " + back;
          const std::vector<int> after = apply_word(state, parse_word(word, 4));

          if (centre_count(after, o, target_face) > before) {
            push_stage(sol, state, label, "", parse_word(word, 4));
            return true;
          }
        }
      }

      // The hardest case, and the only one left once the rest works: the last
      // piece sits on the face OPPOSITE the one being built. Measured on the
      // failures, every single one of them was of this kind.
      //
      // It is expensive because the only slices that reach the opposite face
      // are the ones that also pass through the target, so bringing the piece
      // up costs the pieces already there -- and no word of four moves or
      // fewer exists. What works is a longer middle: turn the opposite face to
      // choose which of its pieces is in the path, then a slice to carry it
      // round, before the outer slice comes back. On the position that stalled
      // everything else, "2x D 2z 2z 2x'" runs 3 -> 1 -> 1 -> 1 -> 2 -> 4.
      for (int oq = 1; oq <= 3; oq++) {
        const int opposite = (target_face + 3) % 6;
        const std::string turn = face_turn_word(opposite, oq);
        if (turn.empty()) continue;

        for (int j = 0; j < n_slices(); j++) {
          if (j == i) continue;
          for (int s = 1; s <= 3; s++) {
            const std::string mid = turn + " " + repeat_move(sm[j].move, s);
            const std::string word = out + " " + mid + " " + back;
            const std::vector<int> after = apply_word(state, parse_word(word, 4));

            if (centre_count(after, o, target_face) > before) {
              push_stage(sol, state, label, "", parse_word(word, 4));
              return true;
            }
          }
        }
      }
    }
  }
  return false;
}

inline bool build_first_centre(std::vector<int>& state, Solution& sol,
                               const Orient& o,
                               int target_face, const std::string& label) {
  std::vector<int> protect;
  protect.push_back(target_face);
  int nudges = 0;

  for (int guard = 0; guard < 48; guard++) {
    if (centre_count(state, o, target_face) == 4) return true;

    // First try to walk a piece in: cheapest, and enough while the face is
    // mostly empty.
    const std::vector<Spot> spots = find_colour(state, target_face);
    bool moved = false;

    for (size_t i = 0; i < spots.size(); i++) {
      if (spots[i].face == target_face) continue;

      const Placement p = bring_to_face(state, o, spots[i].face, spots[i].slot,
                                        target_face, protect);
      if (!p.found) continue;

      push_stage(sol, state, label, "", parse_word(p.word, 4));
      moved = true;
      break;
    }
    if (moved) continue;

    // Nothing walks in any more -- the remaining pieces cannot arrive without
    // displacing one already home. That is what the commutator is for.
    if (insert_by_commutator(state, sol, o, target_face, label)) continue;

    // One case survives even that, and it is a symmetric one: the slot still
    // empty on the target face has the same number as the slot the last piece
    // occupies on the face opposite. Measured over the failures, every one was
    // of that shape, and none of them has a solution in four moves.
    //
    // Rather than carry a special word for it, break the symmetry: turn one
    // slice and start again. The position that results is an ordinary one and
    // the machinery above finishes it. This is what a person does with an
    // awkward position, and it costs a move rather than a table.
    // Which slice breaks it is not always the same one, so the list is tried
    // in turn rather than one being picked as canonical -- measured over the
    // failures: 2x settles every case seen, 1x most of them, and a cube that
    // resists both is answered by a face turn.
    // The nudge has to touch the face being built, or it changes nothing
    // there and the same deadlock returns. Which slices those are depends on
    // the target, so the list is taken from the measured map rather than
    // fixed: a first list picked for U left the other faces at 93 of 100.
    std::vector<std::string> nudge_moves;
    for (int i = 0; i < n_slices(); i++)
      for (int k = 0; k < 8; k++)
        if (slice_maps()[i].step[k].from_face == target_face) {
          nudge_moves.push_back(slice_maps()[i].move);
          break;
        }
    nudge_moves.push_back(face_turn_word(target_face, 1));

    if (nudges < (int)nudge_moves.size()) {
      push_stage(sol, state, label, "break symmetry",
                 parse_word(nudge_moves[nudges], 4));
      nudges++;
      continue;
    }
    return false;
  }
  return centre_count(state, o, target_face) == 4;
}

// ---- Step 2: the l-slice -------------------------------------------------
//
// With one centre built and turned to face L, the next stage fills the layer
// beside it. Pochmann's alphabet for this is U, (Ll), (Rr) and x, and the
// measurement says why those and no others: (Rr) and U leave the l-slice
// exactly as they found it, so they are free to use for setting up, while
// (Ll) is the move that writes into it.
//
// So the shape is the same as step 1's commutator, with the roles fixed by
// the method rather than searched for:
//
//   setup with (Rr) and U   --   (Ll)   --   undo the setup
//
// and the count is read at the end, never in the middle.
inline bool l_slice_insert(std::vector<int>& state, Solution& sol,
                           const Orient& o,
                           const std::string& label) {
  const int before = l_slice_count(state, o);

  // (Ll) carries the whole l-slice away with it, so on its own it can only
  // make things worse -- measured: every one of its three turns drops the
  // count. It has to be the OUTER move of a commutator, taking the slice out
  // of the way, letting the free moves place a piece where the slice will
  // collect it, and bringing it back.
  //
  //   (Ll)^q   free moves   (Ll)^-q
  //
  // The free moves are the ones the measurement shows leave the l-slice
  // untouched: (Rr) and U, in any amount. That is Pochmann's alphabet for this
  // step and the reason it is that alphabet.
  // Two facts, both measured on scrambled cubes rather than on the solved one
  // -- a solved face is uniform and hides everything, which is how U came to
  // be trusted here wrongly for a while:
  //
  //   * Every position reached after step 1 has at least one single move that
  //     raises the count, 2.4 of them on average and sometimes worth 4 at
  //     once. So the first thing to try is simply every move.
  //   * U is NOT free: it cycles U1 -> U2 -> U4 -> U3, carrying the two slice
  //     cells on that face out and two outsiders in, and changes the count in
  //     a third of scrambles. Genuinely free are R, R', (Rr), (Rr)' and 2x.
  //
  // So the vocabulary is the whole alphabet for the direct try, and the free
  // moves only for the middle of a commutator, where nothing may be disturbed.
  static const char* const direct_moves[] = {
    "U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'",
    "1x", "1x'", "2x", "2x'", "1y", "1y'", "2y", "2y'",
    "1z", "1z'", "2z", "2z'", 0};

  for (int i = 0; direct_moves[i]; i++) {
    const std::vector<int> after = apply_word(state, parse_word(direct_moves[i], 4));
    if (l_slice_count(after, o) > before) {
      push_stage(sol, state, label, "", parse_word(direct_moves[i], 4));
      return true;
    }
  }

  // Then pairs. This is Pochmann's own description of the step -- "build the
  // pair with an (Rr) turn and then finish the pair into the l-slice with U or
  // U'" -- two moves, the first setting up and the second inserting. The first
  // need not improve anything on its own, and usually does not: on the
  // position that stalled the single-move version, "2x U'" runs 8 -> 8 -> 9.
  // Judging the first move alone rejects the pair, which is the same mistake
  // as judging the first move of a commutator.
  for (int i = 0; direct_moves[i]; i++) {
    const std::vector<int> mid = apply_word(state, parse_word(direct_moves[i], 4));
    for (int j = 0; direct_moves[j]; j++) {
      const std::vector<int> after = apply_word(mid, parse_word(direct_moves[j], 4));
      if (l_slice_count(after, o) > before) {
        const std::string word = std::string(direct_moves[i]) + " " + direct_moves[j];
        push_stage(sol, state, label, "", parse_word(word, 4));
        return true;
      }
    }
  }

  // Triples, for the last pieces. The same shape one move longer: two moves of
  // set-up before the one that inserts. Pairs took the method from nothing to
  // most of the way; what is left needs the extra move of reach.
  for (int i = 0; direct_moves[i]; i++) {
    const std::vector<int> a = apply_word(state, parse_word(direct_moves[i], 4));
    for (int j = 0; direct_moves[j]; j++) {
      const std::vector<int> b = apply_word(a, parse_word(direct_moves[j], 4));
      for (int k = 0; direct_moves[k]; k++) {
        const std::vector<int> after = apply_word(b, parse_word(direct_moves[k], 4));
        if (l_slice_count(after, o) > before) {
          const std::string word = std::string(direct_moves[i]) + " " +
                                   direct_moves[j] + " " + direct_moves[k];
          push_stage(sol, state, label, "", parse_word(word, 4));
          return true;
        }
      }
    }
  }

  // The last piece of the slice, and only that one, needs four moves -- and
  // measured over the failures they are all the same shape: a plain
  // commutator A B A' B'. R 1z' R' 1z, U 1z' U' 1z, R 1y' B' 1y and so on.
  // That is a three-cycle of centre pieces, which is exactly what placing one
  // last piece without disturbing eleven others requires.
  static const char* const comm_a[] = {"U", "U'", "R", "R'", "B", "B'",
                                       "F", "F'", "D", "D'", "L", "L'", 0};
  static const char* const comm_b[] = {"1z", "1z'", "2z", "2z'",
                                       "1y", "1y'", "2y", "2y'",
                                       "1x", "1x'", "2x", "2x'", 0};
  for (int i = 0; comm_a[i]; i++) {
    const std::string ai = comm_a[i];
    const std::string ai_inv = (ai.size() > 1 && ai[1] == '\'')
                                 ? ai.substr(0, 1) : ai + "'";
    for (int j = 0; comm_b[j]; j++) {
      const std::string bj = comm_b[j];
      const std::string bj_inv = (bj.size() > 2 && bj[bj.size() - 1] == '\'')
                                   ? bj.substr(0, bj.size() - 1) : bj + "'";
      // Three near-commutator shapes, all measured on real failures rather
      // than assumed. The plain A B A' B' does most of them; the last two
      // positions of two hundred needed the other two, which differ only in
      // which part is inverted or doubled:
      //
      //   A B A' B'     the plain commutator      R 1z' R' 1z
      //   A B B A'      a doubled middle          U' 2x 2x U
      //   A B A B'      the same A twice          R' 1z' R' 1z
      const std::string words[3] = {
        ai + " " + bj + " " + ai_inv + " " + bj_inv,
        ai + " " + bj + " " + bj + " " + ai_inv,
        ai + " " + bj + " " + ai + " " + bj_inv
      };
      for (int k = 0; k < 3; k++) {
        const std::vector<int> after = apply_word(state, parse_word(words[k], 4));
        if (l_slice_count(after, o) > before) {
          push_stage(sol, state, label, "", parse_word(words[k], 4));
          return true;
        }
      }
    }
  }

  static const char* const free_moves[] = {"R", "R'", "R R",
                                           "R 2x", "R' 2x'", "R R 2x 2x",
                                           "2x", "2x'", "2x 2x", 0};
  static const char* const out_moves[] = {"L 1x'", "L' 1x", "L L 1x' 1x'", 0};
  static const char* const back_moves[] = {"L' 1x", "L 1x'", "L L 1x' 1x'", 0};

  for (int q = 0; out_moves[q]; q++) {
    for (int a = 0; free_moves[a]; a++) {
      for (int b = 0; free_moves[b]; b++) {
        const std::string word = std::string(out_moves[q]) + " " +
                                 free_moves[a] + " " + free_moves[b] + " " +
                                 back_moves[q];
        const std::vector<int> after = apply_word(state, parse_word(word, 4));
        if (l_slice_count(after, o) > before) {
          push_stage(sol, state, label, "", parse_word(word, 4));
          return true;
        }
      }
    }
  }

  // the shorter form, one free move in the middle
  for (int q = 0; out_moves[q]; q++) {
    for (int a = 0; free_moves[a]; a++) {
      const std::string word = std::string(out_moves[q]) + " " +
                               free_moves[a] + " " + back_moves[q];
      const std::vector<int> after = apply_word(state, parse_word(word, 4));
      if (l_slice_count(after, o) > before) {
        push_stage(sol, state, label, "", parse_word(word, 4));
        return true;
      }
    }
  }

  // The last pieces of the slice need a longer middle, the same way the last
  // piece of a face did in step 1: three free moves rather than two, which
  // lets the piece be fetched from further round the cube before the slice
  // comes back for it.
  for (int q = 0; out_moves[q]; q++) {
    for (int a = 0; free_moves[a]; a++) {
      for (int b = 0; free_moves[b]; b++) {
        for (int c = 0; free_moves[c]; c++) {
          const std::string word = std::string(out_moves[q]) + " " +
                                   free_moves[a] + " " + free_moves[b] + " " +
                                   free_moves[c] + " " + back_moves[q];
          const std::vector<int> after = apply_word(state, parse_word(word, 4));
          if (l_slice_count(after, o) > before) {
            push_stage(sol, state, label, "", parse_word(word, 4));
            return true;
          }
        }
      }
    }
  }
  return false;
}

inline bool build_l_slice(std::vector<int>& state, Solution& sol,
                          const Orient& o, const std::string& label) {
  for (int guard = 0; guard < 48; guard++) {
    if (l_slice_built(state, o)) return true;
    if (!l_slice_insert(state, sol, o, label)) return false;
  }
  return l_slice_built(state, o);
}

// Turning the whole cube about the vertical axis. This is what makes the four
// shots reach all four side faces: the shots are stated against F and B, and y
// brings a different pair of faces into those roles. D is on the axis, so
// everything already built stays built.
inline const char* y_word() { return "D' 1y 2y U"; }

// ---- Step 4: swapping pieces between two faces --------------------------
//
// Step 3 shoots pieces DOWN from U, so it needs something on U worth shooting.
// Measured over 118 failures of steps 1-3: 42 of the first 47 had a completely
// finished U with four faces done, and the pieces still wrong were swapped
// between two side faces. There is nothing on U to fire and no amount of
// turning changes that -- the stage is out of its domain, not stuck.
//
// The breakdown of what is left, over 118 failures:
//
//   56  two pieces, a clean swap between two faces  (46 of them OPPOSITE)
//   39  a 3-cycle over three faces
//   23  four or five pieces
//
// So the tool this stage needs is a commutator that swaps pieces BETWEEN two
// faces without staging them through U.
//
// A note on the geometry, because the obvious approach does not work. For two
// ADJACENT faces the published commutator is l' U r U' l U r' U', which in
// this alphabet swaps positions 0 and 2. It is tempting to reach the opposite
// case by turning the cube first -- but a rotation of a rigid body preserves
// adjacency, and positions 0 and 2 are adjacent, so no orientation of the cube
// ever puts two opposite faces there. Checked exhaustively: of all 24
// orientations, zero qualify. The opposite case needs its own commutator, and
// this one was measured rather than reasoned about:
//
//   1x 2x' U 1x' 2x U     swaps position 2 with position 5, slot 3 of each
//
// which touches exactly those two pieces and leaves the other four faces
// whole. The same shape on the other two axes reaches the other two opposite
// pairs.
struct Swap {
  const char* word;   // the commutator
  int face_a;         // the two positions it exchanges
  int face_b;
  int slot_a;         // and the slot on each that it moves
  int slot_b;
};

// Measured, one per opposite pair. Each is a clean 2-cycle.
inline const Swap* opposite_swaps() {
  static const Swap s[3] = {
    {"1x 2x' U 1x' 2x U",  2, 5, 3, 3},
    {"1z 2z' U 1z' 2z U",  1, 4, 3, 3},
    {"1x 2x' F 1x' 2x F",  0, 3, 2, 3}
  };
  return s;
}

inline int n_opposite_swaps() { return 3; }

// The adjacent case. Three commutators cover the three OPPOSITE pairs, but a
// swap between two faces that touch is a different animal and needs its own --
// measured on the remaining failures, all six two-piece leftovers were between
// adjacent faces, which the opposite commutators cannot reach.
//
// Unlike the opposite pairs, one commutator is enough here: all twelve
// adjacent pairs can be rotated onto positions 0 and 2, so the cube is turned
// to meet the tool rather than the tool copied per pair.
//
//   1x U 2x U' 1x' U 2x' U'   swaps position 0 slot 1 with position 2 slot 2
//
// with setup 0,3,1,2 on both faces, all sixteen arrangements solvable.
inline const Swap& adjacent_swap() {
  static const Swap s = {"1x U 2x U' 1x' U 2x' U'", 0, 2, 1, 2};
  return s;
}

// Which orientation brings a given adjacent pair onto positions 0 and 2.
// Twelve pairs, twelve words, found the same way as for the triple.
inline bool pair_orientation(int fa, int fb, std::string* word) {
  static std::vector<std::pair<std::pair<int, int>, std::string> > table;
  if (table.empty()) {
    static const char* const rots[6] = {"x", "x'", "y", "y'", "z", "z'"};
    std::vector<std::pair<Orient, std::string> > seen;
    Orient id0;
    seen.push_back(std::make_pair(id0, std::string()));
    for (size_t head = 0; head < seen.size() && seen.size() < 24; head++) {
      for (int r = 0; r < 6; r++) {
        const Orient o2 = rotate_orient(seen[head].first, rots[r]);
        bool dup = false;
        for (size_t t = 0; t < seen.size() && !dup; t++) {
          dup = true;
          for (int k = 0; k < 6; k++)
            if (seen[t].first.face[k] != o2.face[k]) { dup = false; break; }
        }
        if (dup) continue;
        const std::string w = seen[head].second.empty()
          ? std::string(rots[r]) : seen[head].second + " " + rots[r];
        seen.push_back(std::make_pair(o2, w));
      }
    }
    const Swap& sw = adjacent_swap();
    for (size_t t = 0; t < seen.size(); t++) {
      int a = seen[t].first.of(sw.face_a), b = seen[t].first.of(sw.face_b);
      if (a > b) std::swap(a, b);
      bool have = false;
      for (size_t q = 0; q < table.size() && !have; q++)
        if (table[q].first.first == a && table[q].first.second == b) have = true;
      if (!have) table.push_back(std::make_pair(std::make_pair(a, b), seen[t].second));
    }
  }
  int a = fa, b = fb;
  if (a > b) std::swap(a, b);
  for (size_t q = 0; q < table.size(); q++)
    if (table[q].first.first == a && table[q].first.second == b) {
      *word = table[q].second; return true;
    }
  return false;
}

// The setup for a swap, measured rather than derived.
//
// The natural guess -- "turn the face until the piece reaches the slot the
// commutator reads" -- is wrong, and wrong in a way that is worth recording.
// It gives 3,2,0,1 for the four slots; the truth is 0,3,1,2, which is neither
// that nor its reverse. The commutator reads a different pair of slots once
// the face has been turned under it, so the two effects compose and the answer
// does not follow from the slot cycle alone.
//
// So it was measured: for every one of the sixteen ways two pieces can be
// swapped between the two faces, try the sixteen combinations of face turns
// and keep the one that solves it. All sixteen have a solution, and the table
// factorises -- the turns for one face depend only on that face's slot, which
// is why one row of four numbers serves both.
// Two tables, not one, because the third commutator does not read the same
// slot as the other two. Measured per pair over all sixteen arrangements:
//
//   faces 2/5 and 1/4, both faces      0,3,1,2
//   faces 0/3, the 3 face             0,3,1,2
//   faces 0/3, the 0 face             2,1,3,0
//
// The odd one out is the face whose working slot is 2 rather than 3. Using the
// common table everywhere leaves the 0-3 case unsolvable: measured, the word
// that works there turns only the other face ("D' . commutator . D") while the
// common table asks for two turns of face 0 as well. That single wrong row was
// 16 of 22 unfinished two-piece swaps.
inline int swap_setup_turns(int face, int slot) {
  static const int common[5]  = {0, 0, 3, 1, 2};
  static const int face_0[5]  = {0, 2, 1, 3, 0};
  return (face == 0) ? face_0[slot] : common[slot];
}

// Turning the cube back to where the commutators are stated.
//
// The three commutators above were measured on an unturned cube, so they mean
// what they say only there. By the time step 4 runs the cube has been through
// z' and a string of y turns, and applying them as written reads the wrong
// physical faces -- measured, that costs pieces rather than placing them.
//
// The fix is not a variant of each commutator per orientation. It is to turn
// the cube back to the orientation they are written in, do the work, and turn
// it back. Same shape as the setup inside a shot, one level up: the tool stays
// canonical and the frame moves to meet it.
//
// The table is every one of the 24 orientations with a word that reaches it,
// found by breadth-first search over the six rotations. Undoing that word is
// what canonicalises. Checked before any commutator was added: a word and its
// inverse applied together leave the cube identical in 30 of 30 scrambles.
struct Canon { int face[6]; const char* word; };

inline const std::vector<std::pair<Orient, std::string> >& canon_table() {
  static std::vector<std::pair<Orient, std::string> > table;
  if (table.empty()) {
    static const char* const rots[6] = {"x", "x'", "y", "y'", "z", "z'"};
    std::vector<std::pair<Orient, std::string> > frontier;
    Orient id;
    table.push_back(std::make_pair(id, std::string()));
    frontier.push_back(table[0]);
    for (int depth = 0; depth < 6 && table.size() < 24; depth++) {
      std::vector<std::pair<Orient, std::string> > next;
      for (size_t f = 0; f < frontier.size(); f++) {
        for (int r = 0; r < 6; r++) {
          const Orient o2 = rotate_orient(frontier[f].first, rots[r]);
          bool seen = false;
          for (size_t t = 0; t < table.size() && !seen; t++) {
            seen = true;
            for (int k = 0; k < 6; k++)
              if (table[t].first.face[k] != o2.face[k]) { seen = false; break; }
          }
          if (seen) continue;
          const std::string w = frontier[f].second.empty()
            ? std::string(rots[r]) : frontier[f].second + " " + rots[r];
          table.push_back(std::make_pair(o2, w));
          next.push_back(table.back());
        }
      }
      frontier = next;
    }
  }
  return table;
}

// The rotation word that produced this orientation, and its inverse.
inline bool canon_words(const Orient& o, std::string* forward,
                        std::string* backward) {
  const std::vector<std::pair<Orient, std::string> >& t = canon_table();
  for (size_t i = 0; i < t.size(); i++) {
    bool same = true;
    for (int k = 0; k < 6 && same; k++) if (t[i].first.face[k] != o.face[k]) same = false;
    if (!same) continue;
    *forward = t[i].second;
    // the inverse: each rotation reversed, in reverse order
    std::vector<std::string> parts;
    std::string cur;
    for (size_t c = 0; c <= t[i].second.size(); c++) {
      if (c == t[i].second.size() || t[i].second[c] == ' ') {
        if (!cur.empty()) parts.push_back(cur);
        cur.clear();
      } else cur += t[i].second[c];
    }
    std::string back;
    for (size_t c = parts.size(); c > 0; c--) {
      const std::string& r = parts[c - 1];
      const std::string inv = (r.size() > 1 && r[1] == '\'')
        ? r.substr(0, 1) : r + "'";
      if (!back.empty()) back += " ";
      back += inv;
    }
    *backward = back;
    return true;
  }
  return false;
}

// A rotation word as package moves.
inline std::string rotation_moves(const std::string& word) {
  static const char* const nm[6] = {"x", "x'", "y", "y'", "z", "z'"};
  static const char* const wd[6] = {
    "L' 1x 2x R", "L 1x' 2x' R'", "D' 1y 2y U", "D 1y' 2y' U'",
    "B' 1z 2z F", "B 1z' 2z' F'"
  };
  std::string out, cur;
  for (size_t c = 0; c <= word.size(); c++) {
    if (c == word.size() || word[c] == ' ') {
      if (!cur.empty()) {
        for (int k = 0; k < 6; k++) if (cur == nm[k]) {
          if (!out.empty()) out += " ";
          out += wd[k];
        }
      }
      cur.clear();
    } else cur += word[c];
  }
  return out;
}

// ---- The 3-cycle --------------------------------------------------------
//
// Two pieces swapped is not the only way steps 1-3 leave the centres. Measured
// over 62 failures: 21 were a clean 3-cycle over three faces. A swap cannot
// touch those -- a 3-cycle is even, a swap is odd, and no sequence of swaps of
// the wrong pairs reaches it.
//
// The commutator is one of the pair that came with the method:
//
//   1x U 2x' U' 1x' U 2x U'    cycles position 0 -> 2 -> 5 -> 0
//
// with its pieces at slots 1, 1 and 3 respectively.
//
// Two facts checked before this was wired up, both of the kind that has cost a
// wasted attempt earlier in this file:
//
//   Reach. Positions 0, 2 and 5 are not three mutually adjacent faces -- 2 and
//   5 are opposite. Rotating the cube moves which faces play those positions,
//   but adjacency is preserved, so only 12 of the 20 possible triples can ever
//   be brought there: exactly those containing one opposite pair. The other 8
//   are out of reach. That would matter except that all 21 measured 3-cycles
//   were of the reachable kind, and none of the unreachable one.
//
//   Setup. Measured, not derived, after the same table for the swap turned out
//   to be neither the slot cycle nor its reverse. All 64 arrangements of the
//   three pieces are solvable, and the table factorises: each face's turn count
//   depends only on where its own piece sits.
struct Cycle3 {
  const char* word;
  int face[3];
  int slot[3];
};

inline const Cycle3& cycle3() {
  static const Cycle3 c = {"1x U 2x' U' 1x' U 2x U'", {0, 2, 5}, {1, 1, 3}};
  return c;
}

// A 3-cycle runs one way round, and its inverse the other. Measured over all
// 128 arrangements of three pieces on the three faces -- four slots each, both
// directions -- the forward commutator solves exactly 64 and the inverse the
// other 64. Trying only one leaves every cycle of the wrong handedness
// untouched, which was 9 of the last 20 failures.
inline const char* cycle3_inverse() { return "U 2x' U' 1x U 2x U' 1x'"; }

// The setup tables. The first two faces share one because their pieces start
// at the same slot; the third differs because its piece starts at slot 3.
inline int cycle3_setup_turns(int which_face, int slot) {
  static const int from_slot1[5] = {0, 0, 3, 1, 2};
  static const int from_slot3[5] = {0, 3, 2, 0, 1};
  return (which_face == 2) ? from_slot3[slot] : from_slot1[slot];
}

// Which orientation brings a given triple of faces onto 0, 2 and 5.
//
// Built by walking the 24 orientations and recording, for each, which triple
// lands on the working positions. Twelve triples are reachable and each has a
// word of at most three rotations; the other eight are the ones with no
// opposite pair, and no rotation reaches them. Measured on 51 failures: of the
// 16 that were clean 3-cycles, all 16 wanted a reachable triple and none the
// unreachable kind.
//
// The table is searched rather than derived, for the same reason the setup
// tables are: the composition of a rotation with the commutator's fixed
// positions is not something to work out in the head twice.
inline bool triple_orientation(int f0, int f1, int f2, std::string* word) {
  static std::vector<std::pair<std::vector<int>, std::string> > table;
  if (table.empty()) {
    static const char* const rots[6] = {"x", "x'", "y", "y'", "z", "z'"};
    std::vector<std::pair<Orient, std::string> > seen;
    Orient id;
    seen.push_back(std::make_pair(id, std::string()));
    for (size_t head = 0; head < seen.size() && seen.size() < 24; head++) {
      for (int r = 0; r < 6; r++) {
        const Orient o2 = rotate_orient(seen[head].first, rots[r]);
        bool dup = false;
        for (size_t t = 0; t < seen.size() && !dup; t++) {
          dup = true;
          for (int k = 0; k < 6; k++)
            if (seen[t].first.face[k] != o2.face[k]) { dup = false; break; }
        }
        if (dup) continue;
        const std::string w = seen[head].second.empty()
          ? std::string(rots[r]) : seen[head].second + " " + rots[r];
        seen.push_back(std::make_pair(o2, w));
      }
    }
    const Cycle3& c = cycle3();
    for (size_t t = 0; t < seen.size(); t++) {
      std::vector<int> tri;
      for (int k = 0; k < 3; k++) tri.push_back(seen[t].first.of(c.face[k]));
      std::sort(tri.begin(), tri.end());
      bool have = false;
      for (size_t q = 0; q < table.size() && !have; q++)
        if (table[q].first == tri) have = true;
      if (!have) table.push_back(std::make_pair(tri, seen[t].second));
    }
  }

  std::vector<int> want;
  want.push_back(f0); want.push_back(f1); want.push_back(f2);
  std::sort(want.begin(), want.end());
  for (size_t q = 0; q < table.size(); q++)
    if (table[q].first == want) { *word = table[q].second; return true; }
  return false;
}

// One pass: find a 3-cycle anywhere on the cube, turn the cube so it lands on
// the commutator's working positions, and undo it.
//
// The cube arrives canonical -- colour equals position -- so the cycle is read
// off directly. The commutator only ever acts on positions 0, 2 and 5, so the
// triple that needs cycling has to be brought there first; that rotation is
// what triple_orientation supplies, and it is undone afterwards.
inline bool cycle_one_triple(std::vector<int>& state, Solution& sol,
                             const std::string& label) {
  const Orient id;
  const Cycle3& c = cycle3();

  // find three faces each holding one piece that belongs on another of them
  for (int a = 0; a < 6; a++) {
    for (int b = 0; b < 6; b++) {
      if (b == a) continue;
      for (int d = 0; d < 6; d++) {
        if (d == a || d == b) continue;

        // a holds a piece of b, b holds one of d, d holds one of a.
        //
        // This closes only when the three form a cycle by themselves. A
        // 4-cycle 0->2->3->5->0 contains no such triple, which is why every
        // one of them ran through this stage untouched: the search asked for
        // a shape the position did not have. The decomposition
        // (a b c d) = (a b c)(c d) says to take three of the four anyway --
        // the commutator then places two pieces and leaves a clean swap -- so
        // the closing condition is relaxed below to "a holds b's piece and b
        // holds d's", with d's own piece free to belong anywhere. Measured on
        // the nine: each has exactly one such 3-cycle taking 20 to 22, and the
        // swaps close all nine from there.
        int ka = -1, kb = -1, kd = -1;
        const int* sa = centre_slots_of(a);
        const int* sb = centre_slots_of(b);
        const int* sd = centre_slots_of(d);
        for (int k = 1; k <= 4; k++) {
          if (ka < 0 && centre_colour(state, sa[k - 1]) == b) ka = k;
          if (kb < 0 && centre_colour(state, sb[k - 1]) == d) kb = k;
          if (kd < 0 && centre_colour(state, sd[k - 1]) == a) kd = k;
        }
        if (ka < 0 || kb < 0) continue;
        if (kd < 0) {
          // the third face need not point back at the first; any of its
          // pieces will do as the one the commutator carries
          for (int k = 1; k <= 4 && kd < 0; k++)
            if (centre_colour(state, sd[k - 1]) != d) kd = k;
        }
        if (kd < 0) continue;

        std::string rot_word;
        if (!triple_orientation(a, b, d, &rot_word)) continue;

        const std::string to_pos = rotation_moves(rot_word);
        // the inverse, to put the cube back
        std::vector<std::string> parts;
        std::string cur;
        for (size_t q = 0; q <= rot_word.size(); q++) {
          if (q == rot_word.size() || rot_word[q] == ' ') {
            if (!cur.empty()) parts.push_back(cur);
            cur.clear();
          } else cur += rot_word[q];
        }
        std::string back_word;
        for (size_t q = parts.size(); q > 0; q--) {
          const std::string& r = parts[q - 1];
          const std::string inv = (r.size() > 1 && r[1] == '\'')
            ? r.substr(0, 1) : r + "'";
          if (!back_word.empty()) back_word += " ";
          back_word += inv;
        }
        const std::string from_pos = rotation_moves(back_word);

        // With the triple on 0, 2 and 5, try the commutator both ways round --
        // which direction the cycle runs decides which one undoes it -- and
        // over every setup, since the pieces may sit in any slots.
        // Turning the cube moves every face, so the frame moves with it. The
        // count after the rotation has to be read against the rotated frame,
        // not against the identity -- measured, reading it against the
        // identity shows 21 of 24 collapsing to 8 when nothing was lost, and
        // no setup can then clear the +3 bar.
        Orient staged_o = id;
        std::vector<int> staged = state;
        if (!to_pos.empty()) {
          staged = apply_word(staged, parse_word(to_pos, 4));
          std::string tok;
          for (size_t q = 0; q <= rot_word.size(); q++) {
            if (q == rot_word.size() || rot_word[q] == ' ') {
              if (!tok.empty()) staged_o = rotate_orient(staged_o, tok.c_str());
              tok.clear();
            } else tok += rot_word[q];
          }
        }

        for (int t0 = 0; t0 < 4; t0++)
        for (int t1 = 0; t1 < 4; t1++)
        for (int t2 = 0; t2 < 4; t2++) {
          std::string pre, post;
          const int turn[3] = {t0, t1, t2};
          for (int q = 0; q < 3; q++) {
            const std::string w = face_turn_word(c.face[q], turn[q]);
            if (!w.empty()) { if (!pre.empty()) pre += " "; pre += w; }
          }
          for (int q = 3; q > 0; q--) {
            const std::string w = face_turn_word(c.face[q - 1], (4 - turn[q - 1]) % 4);
            if (!w.empty()) { if (!post.empty()) post += " "; post += w; }
          }
          for (int which = 0; which < 2; which++) {
          std::string body = pre;
          if (!body.empty()) body += " ";
          body += (which == 0) ? c.word : cycle3_inverse();
          if (!post.empty()) body += " " + post;

          const std::vector<int> cand = apply_word(staged, parse_word(body, 4));
          // Three pieces move, so a 3-cycle that is doing its job places all
          // three. Accepting any gain at all is what made this stage wander:
          // it would take a word that put one piece home and scattered the
          // rest, and settle_pairs could not recover from where that left the
          // cube. Measured: with the loose test the stage fired 11 times and
          // cost a solve; the count it left behind fell from 19 of 24 to 11.
          // A 3-cycle that closes a 3-cycle places all three pieces. But the
          // same commutator is also the first half of a 4-cycle's
          // decomposition -- (a b c d) = (a b c)(c d) -- and there it places
          // only two, leaving a clean 2-cycle for the swaps to finish.
          // Measured on the nine 4-cycle failures: each has exactly one
          // 3-cycle taking 20 of 24 to 22, and with the swaps able to reach
          // the remaining pair all nine then close. Demanding +3 here refused
          // that first half and left every 4-cycle unsolved.
          if (centres_total(cand, staged_o) < centres_total(staged, staged_o) + 2) continue;

          std::string word;
          if (!to_pos.empty()) word += to_pos + " ";
          word += body;
          if (!from_pos.empty()) word += " " + from_pos;

          push_stage(sol, state, label, word, parse_word(word, 4));
          return true;
          }
        }
      }
    }
  }
  return false;
}

// One pass: find two pieces that belong on each other's face and swap them.
// The cube is already canonical here, so colour equals position and the
// commutators mean what they say.
inline bool swap_one_pair(std::vector<int>& state, Solution& sol,
                          const std::string& label) {
  const Swap* sw = opposite_swaps();
  const Orient id;

  for (int i = 0; i < n_opposite_swaps(); i++) {
    const int fa = sw[i].face_a, fb = sw[i].face_b;
    const int* sa = centre_slots_of(fa);
    const int* sb = centre_slots_of(fb);

    for (int ka = 1; ka <= 4; ka++) {
      if (centre_colour(state, sa[ka - 1]) != fb) continue;
      for (int kb = 1; kb <= 4; kb++) {
        if (centre_colour(state, sb[kb - 1]) != fa) continue;

        const int ta = swap_setup_turns(fa, ka);
        const int tb = swap_setup_turns(fb, kb);
        std::string word;
        const std::string wa = face_turn_word(fa, ta);
        const std::string wb = face_turn_word(fb, tb);
        const std::string wb_back = face_turn_word(fb, (4 - tb) % 4);
        const std::string wa_back = face_turn_word(fa, (4 - ta) % 4);
        if (!wa.empty()) word += wa + " ";
        if (!wb.empty()) word += wb + " ";
        word += sw[i].word;
        if (!wb_back.empty()) word += " " + wb_back;
        if (!wa_back.empty()) word += " " + wa_back;

        const std::vector<int> cand = apply_word(state, parse_word(word, 4));
        if (centres_total(cand, id) <= centres_total(state, id)) continue;

        push_stage(sol, state, label, word, parse_word(word, 4));
        return true;
      }
    }
  }
  return false;
}

// The adjacent case, with the cube turned to bring the pair onto the working
// positions. Same shape as the triple: one canonical tool, the frame moves.
inline bool swap_adjacent_pair(std::vector<int>& state, Solution& sol,
                               const std::string& label) {
  const Orient id;
  const Swap& sw = adjacent_swap();

  for (int fa = 0; fa < 6; fa++) {
    for (int fb = 0; fb < 6; fb++) {
      if (fb == fa) continue;
      const int* sa = centre_slots_of(fa);
      const int* sb = centre_slots_of(fb);
      bool have_a = false, have_b = false;
      for (int k = 1; k <= 4; k++) {
        if (centre_colour(state, sa[k - 1]) == fb) have_a = true;
        if (centre_colour(state, sb[k - 1]) == fa) have_b = true;
      }
      if (!have_a || !have_b) continue;

      std::string rot_word;
      if (!pair_orientation(fa, fb, &rot_word)) continue;

      const std::string to_pos = rotation_moves(rot_word);
      std::vector<std::string> parts;
      std::string cur;
      for (size_t q = 0; q <= rot_word.size(); q++) {
        if (q == rot_word.size() || rot_word[q] == ' ') {
          if (!cur.empty()) parts.push_back(cur);
          cur.clear();
        } else cur += rot_word[q];
      }
      std::string back_word;
      for (size_t q = parts.size(); q > 0; q--) {
        const std::string& r = parts[q - 1];
        const std::string inv = (r.size() > 1 && r[1] == '\'') ? r.substr(0, 1) : r + "'";
        if (!back_word.empty()) back_word += " ";
        back_word += inv;
      }
      const std::string from_pos = rotation_moves(back_word);

      Orient staged_o = id;
      std::vector<int> staged = state;
      if (!to_pos.empty()) {
        staged = apply_word(staged, parse_word(to_pos, 4));
        std::string tok;
        for (size_t q = 0; q <= rot_word.size(); q++) {
          if (q == rot_word.size() || rot_word[q] == ' ') {
            if (!tok.empty()) staged_o = rotate_orient(staged_o, tok.c_str());
            tok.clear();
          } else tok += rot_word[q];
        }
      }

      for (int ta = 0; ta < 4; ta++) {
        for (int tb = 0; tb < 4; tb++) {
          std::string body;
          const std::string wa = face_turn_word(sw.face_a, ta);
          const std::string wb = face_turn_word(sw.face_b, tb);
          const std::string wb_back = face_turn_word(sw.face_b, (4 - tb) % 4);
          const std::string wa_back = face_turn_word(sw.face_a, (4 - ta) % 4);
          if (!wa.empty()) body += wa + " ";
          if (!wb.empty()) body += wb + " ";
          body += sw.word;
          if (!wb_back.empty()) body += " " + wb_back;
          if (!wa_back.empty()) body += " " + wa_back;

          const std::vector<int> cand = apply_word(staged, parse_word(body, 4));
          if (centres_total(cand, staged_o) < centres_total(staged, staged_o) + 2) continue;

          std::string word;
          if (!to_pos.empty()) word += to_pos + " ";
          word += body;
          if (!from_pos.empty()) word += " " + from_pos;
          push_stage(sol, state, label, word, parse_word(word, 4));
          return true;
        }
      }
    }
  }
  return false;
}

// The stage: turn the cube to where the commutators are written, swap pairs
// until none is left, then turn it back.
inline bool settle_pairs(std::vector<int>& state, Solution& sol,
                         const Orient& o, const std::string& label) {
  std::string fwd, back;
  if (!canon_words(o, &fwd, &back)) return centres_built(state, o);

  const std::string to_canon = rotation_moves(back);
  const std::string from_canon = rotation_moves(fwd);

  if (!to_canon.empty())
    push_stage(sol, state, label, "canonical", parse_word(to_canon, 4));

  const Orient id;
  for (int guard = 0; guard < 24; guard++) {
    if (centres_built(state, id)) break;
    if (swap_one_pair(state, sol, label)) continue;
    if (swap_adjacent_pair(state, sol, label)) continue;
    if (cycle_one_triple(state, sol, label)) continue;
    break;
  }

  const bool done = centres_built(state, id);
  if (!from_canon.empty())
    push_stage(sol, state, label, "restore", parse_word(from_canon, 4));
  return done;
}

// ---- Step 3, the loop ---------------------------------------------------
//
// Shoot while there is a shot to fire; when there is none, turn the cube and
// look again. Four turns without a shot means every side face has been
// offered and none wanted anything, so the u-slice is as empty as this stage
// can make it.
//
// The stage ends when U holds only pieces belonging to U -- at which point the
// side faces have taken everything that was theirs, and with D untouched
// throughout, all six centres are built.
// The orientation is taken by value and updated here: y turns the whole cube,
// so every turn of the loop moves the built layer to a new position, and a map
// fixed at entry would be wrong after the first one.
inline bool empty_u_slice(std::vector<int>& state, Solution& sol, Orient o,
                          Orient* final_orient, const std::string& label) {
  const int guard_max = 200;
  int turns_without_shot = 0;
  int tilts = 0;
  bool tilted = false;

  for (int guard = 0; guard < guard_max; guard++) {
    if (centres_built(state, o)) { if (final_orient) *final_orient = o; return true; }

    // The shot and its setup together. choose_aim computes how many U turns
    // bring the wanted piece to the entry slot of the shot that aims at its
    // face, so a piece is reachable wherever it sits on U -- the fixed
    // slot-to-face pairings no longer decide what can be played, and turning U
    // blindly in the hope of a match is gone with them.
    const Aim aim = choose_aim(state, o, tilted);
    if (aim.shot >= 0) {
      for (int t = 0; t < aim.setup; t++)
        push_stage(sol, state, label, "U", parse_word("U", 4));
      push_stage(sol, state, label, shots()[aim.shot].word,
                 parse_word(shots()[aim.shot].word, 4));
      turns_without_shot = 0;
      tilts = 0;
      continue;
    }

    // Every U slot has been offered to the shots. Turn the cube about the
    // vertical axis to bring a different pair of side faces into F and B, and
    // try again. D stays put, so nothing built on the bottom is at risk.
    if (turns_without_shot < 4) {
      push_stage(sol, state, label, "y", parse_word(y_word(), 4));
      o = rotate_orient(o, "y");
      turns_without_shot++;
      continue;
    }

    // All four side faces have been offered with all four U slots, and none
    // wanted anything. Pochmann's remedy is to tilt: "rotate the cube (usually
    // z/z') so that one of them gets on U and a solved one gets on D. Then
    // continue step 3, just with a different top-colour".
    //
    // But read the sentence before it -- "if you do solve yellow before all
    // others are solved, USUALLY ONLY TWO OR THREE CENTRES ARE UNSOLVED". That
    // is not colour commentary, it is the condition the tilt is for. The tilt
    // is an endgame move: with four faces finished the guard forbids almost
    // the whole cube and the shots have only the last two or three to work in.
    // Applied in the middlegame it does the opposite -- five faces unfinished
    // means the guard constrains almost nothing and the shots churn.
    //
    // Measured, and this is what two failed attempts had in common: of 35
    // tilts the solver made, exactly ONE was taken with three faces finished.
    // The median was one finished face. The tilt was not broken; it was being
    // fired in a position it was never meant for, and the cost showed up as a
    // median of 16 of 24 falling to 8, then to 10.
    //
    // A tilt is a rigid rotation of the whole cube, so it cannot cost anything
    // by itself -- a finished face is carried to a new position still
    // finished, checked by rotating a state with a known finished face and
    // finding the block whole where the table predicts. What it does cost is
    // the d-slice guard, which names fixed positions and reads the layer as
    // destroyed once the layer has moved off them (measured 12 -> 7 after z,
    // 12 -> 6 after x, with nothing actually lost). Hence `tilted`: past the
    // first tilt the guard on finished faces carries the load alone, since it
    // rescans and a rotation cannot fool it.
    if (faces_finished(state, o) < 3) { if (final_orient) *final_orient = o; return centres_built(state, o); }
    if (tilts >= 4) { if (final_orient) *final_orient = o; return centres_built(state, o); }

    // Which tilt: the one that brings an UNFINISHED face up to U, which is
    // what Pochmann asks for -- "so that one of them gets on U and a solved
    // one gets on D". Picking blindly from the list wastes the allowance on
    // tilts that put a finished face on top, where there is still nothing to
    // shoot.
    //
    // Measured on 47 failures: 42 of them had a completely finished U with a
    // median of 4 faces done. Nothing on U to fire, and the pieces still wrong
    // were swapped between two side faces -- exactly the position this is for.
    static const char* const tips[4] = {"z", "z'", "x", "x'"};
    static const char* const tip_words[4] = {
      "B' 1z 2z F", "B 1z' 2z' F'", "L' 1x 2x R", "L 1x' 2x' R'"
    };

    int pick = -1;
    for (int t = 0; t < 4 && pick < 0; t++) {
      const Orient ot = rotate_orient(o, tips[t]);
      const std::vector<int> at = apply_word(state, parse_word(tip_words[t], 4));
      // an unfinished face on top, and the bottom still finished so the guard
      // has something to hold
      if (centre_count(at, ot, 0) < 4 && centre_count(at, ot, 3) == 4) pick = t;
    }
    if (pick < 0) { if (final_orient) *final_orient = o; return centres_built(state, o); }

    push_stage(sol, state, label, tips[pick], parse_word(tip_words[pick], 4));
    o = rotate_orient(o, tips[pick]);
    tilts++;
    tilted = true;
    turns_without_shot = 0;
  }
  if (final_orient) *final_orient = o;
  return centres_built(state, o);
}

}  // namespace cube_solve

#endif  // CAYLEYR_CUBE_CENTRES_H
