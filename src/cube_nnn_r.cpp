#include <Rcpp.h>
#include <vector>
#include <string>
#include "cube_nnn.h"
#include "perm_group.h"

using namespace Rcpp;
using namespace cube_nnn;

// ---- The N x N x N cube, R-facing --------------------------------------
//
// The geometry lives in cube_nnn.h and knows nothing of R; everything here is
// argument marshalling, the usual split between table and interface.
//
// A cube of side n has 6n^2 stickers and 6n moves: three axes, n layers each,
// a quarter turn either way. Half turns are words rather than letters, which
// is the quarter-turn metric -- see cube_nnn.h.

namespace {

int check_n(int n, const char* fn) {
  if (n < 2) stop("%s: n must be at least 2, got %d", fn, n);
  if (n > 20) {
    // 6n^2 stickers, and the move table is 6n of them: n = 20 is already
    // 2400 stickers and 120 moves. Beyond that the caller has almost
    // certainly passed something that is not a cube size.
    stop("%s: n = %d is implausibly large (would be %d stickers)", fn, n,
         6 * n * n);
  }
  return n;
}

}  // namespace

// The move table for a cube of side n, as a named list of 1-based integer
// vectors -- the shape perm_group expects, so it can be handed to
// perm_group() directly.
// [[Rcpp::export]]
List cube_moves_cpp(int n) {
  check_n(n, "cube_moves");
  CubeAlphabet a = build_alphabet(n);
  const int total = 6 * n * n;

  List out(a.perms.size());
  CharacterVector nm(a.names.size());
  for (size_t m = 0; m < a.perms.size(); m++) {
    IntegerVector v(total);
    for (int i = 0; i < total; i++) v[i] = a.perms[m][i];
    out[m] = v;
    nm[m] = a.names[m];
  }
  out.attr("names") = nm;
  return out;
}

// Just the names, in table order.
// [[Rcpp::export]]
CharacterVector cube_move_names_cpp(int n) {
  check_n(n, "cube_move_names");
  CubeAlphabet a = build_alphabet(n);
  CharacterVector out(a.names.size());
  for (size_t m = 0; m < a.names.size(); m++) out[m] = a.names[m];
  return out;
}

// One move on its own, named by axis, layer and quarter turns. This is the
// generator's own vocabulary, for callers who would rather say "layer 1 about
// x" than look up what that layer is called on this size of cube.
//
// `axis` is 1, 2 or 3 for x, y, z; `layer` is 1-based to match R.
// [[Rcpp::export]]
IntegerVector cube_layer_move_cpp(int n, int axis, int layer, int turns) {
  check_n(n, "cube_layer_move");
  if (axis < 1 || axis > 3) stop("cube_layer_move: axis must be 1, 2 or 3");
  if (layer < 1 || layer > n) {
    stop("cube_layer_move: layer must be in 1..%d, got %d", n, layer);
  }
  if (turns < 1 || turns > 3) {
    stop("cube_layer_move: turns must be 1, 2 or 3, got %d", turns);
  }
  std::vector<int> p = layer_move(n, (Axis)(axis - 1), layer - 1, turns);
  return wrap(p);
}

// The solved state, 1..6n^2.
// [[Rcpp::export]]
IntegerVector cube_identity_cpp(int n) {
  check_n(n, "cube_identity");
  const int total = 6 * n * n;
  IntegerVector out(total);
  for (int i = 0; i < total; i++) out[i] = i + 1;
  return out;
}

// Whether every face carries a single colour. A sticker's colour is the face
// it started on, so this asks whether each block of n^2 entries all come from
// one such block -- which is what "solved" means once slice turns are in the
// alphabet, since those move the centres and a cube turned bodily in space is
// still solved.
//
// Note this is weaker than state == 1..6n^2: it does not care how the stickers
// of a face are arranged among themselves, only that they agree in colour.
// [[Rcpp::export]]
bool cube_is_colour_solved_cpp(IntegerVector state, int n) {
  check_n(n, "cube_is_colour_solved");
  const int face_size = n * n;
  const int total = 6 * face_size;
  if (state.size() != total) return false;

  for (int f = 0; f < 6; f++) {
    if (state[f * face_size] == NA_INTEGER) return false;
    const int want = (state[f * face_size] - 1) / face_size;
    for (int i = 1; i < face_size; i++) {
      const int v = state[f * face_size + i];
      if (v == NA_INTEGER || (v - 1) / face_size != want) return false;
    }
  }
  return true;
}
