#include <Rcpp.h>
#include <vector>
#include <string>
#include "cube_orbits.h"

using namespace Rcpp;
using namespace cube_orbits;

// ---- Piece orbits, R-facing ---------------------------------------------
//
// The geometry lives in cube_orbits.h and knows nothing of R; everything here
// is argument marshalling.

namespace {

int check_n(int n, const char* fn) {
  if (n < 2) stop("%s: n must be at least 2, got %d", fn, n);
  if (n > 20) {
    stop("%s: n = %d is implausibly large (would be %d stickers)", fn, n,
         6 * n * n);
  }
  return n;
}

// The side of the cube a state vector describes, or an error naming what it
// found. A caller passes stickers and nothing else, so this is where a wrong
// length is caught.
int side_from_state(R_xlen_t len, const char* fn) {
  for (int n = 2; n <= 20; n++) {
    if ((R_xlen_t)(6 * n * n) == len) return n;
  }
  stop("%s: %d stickers is not a cube of any size (6n^2 for n from 2 to 20)",
       fn, (int)len);
  return -1;
}

}  // namespace

// One row per orbit: its label, what kind of piece it holds, how many pieces,
// and the two depths and mirror sign that name it.
// [[Rcpp::export]]
DataFrame cube_orbits_cpp(int n) {
  check_n(n, "cube_orbits");
  const Orbits& O = orbits_of(n);

  const int k = O.n_orbits();
  CharacterVector label(k);
  IntegerVector kind(k), size(k), depth_a(k), depth_b(k), chirality(k);
  CharacterVector kind_name(k);

  for (int i = 0; i < k; i++) {
    const Orbit& o = O.orbit[i];
    label[i] = o.label;
    kind[i] = o.kind;
    kind_name[i] = (o.kind == PK_CORNER) ? "corner"
                 : (o.kind == PK_EDGE)   ? "edge" : "centre";
    size[i] = (int)o.pieces.size();
    depth_a[i] = o.depth_a;
    depth_b[i] = o.depth_b;
    chirality[i] = o.chirality;
  }

  return DataFrame::create(
      _["orbit"] = seq_len(k),
      _["label"] = label,
      _["kind"] = kind_name,
      _["stickers_per_piece"] = kind,
      _["n_pieces"] = size,
      _["depth_a"] = depth_a,
      _["depth_b"] = depth_b,
      _["chirality"] = chirality,
      _["stringsAsFactors"] = false);
}

// One row per piece: where it sits, which orbit it belongs to, and the
// stickers it carries as a comma-separated string -- a list column would be
// truer but a data.frame of scalars is what the rest of the package returns.
// [[Rcpp::export]]
DataFrame cube_pieces_cpp(int n) {
  check_n(n, "cube_pieces");
  const Orbits& O = orbits_of(n);

  const int k = O.n_pieces();
  IntegerVector piece(k), x(k), y(k), z(k), orbit(k), nst(k);
  CharacterVector label(k), stickers(k);

  for (int i = 0; i < k; i++) {
    const Piece& p = O.piece[i];
    piece[i] = i + 1;
    x[i] = p.x; y[i] = p.y; z[i] = p.z;
    orbit[i] = p.orbit + 1;
    label[i] = O.orbit[p.orbit].label;
    nst[i] = (int)p.stickers.size();
    std::string s;
    for (size_t j = 0; j < p.stickers.size(); j++) {
      if (j) s += ",";
      s += std::to_string(p.stickers[j] + 1);   // 1-based for R
    }
    stickers[i] = s;
  }

  return DataFrame::create(
      _["piece"] = piece,
      _["x"] = x, _["y"] = y, _["z"] = z,
      _["orbit"] = orbit,
      _["label"] = label,
      _["n_stickers"] = nst,
      _["stickers"] = stickers,
      _["stringsAsFactors"] = false);
}

// How much of each orbit is home, for one state. This is the measure a
// distance on a large cube is built from: one number for the whole cube says
// too little when the orbits are solved at different times.
// [[Rcpp::export]]
DataFrame cube_progress_cpp(IntegerVector state) {
  const int n = side_from_state(state.size(), "cube_progress");
  const Orbits& O = orbits_of(n);
  const std::vector<int> s(state.begin(), state.end());

  const int k = O.n_orbits();
  CharacterVector label(k);
  CharacterVector kind_name(k);
  IntegerVector solved(k), total(k);
  NumericVector frac(k);

  // one pass over the cube for the face colours, shared by every orbit
  const std::vector<int> faces = face_colours(n, s);

  for (int i = 0; i < k; i++) {
    const Orbit& o = O.orbit[i];
    label[i] = o.label;
    kind_name[i] = (o.kind == PK_CORNER) ? "corner"
                 : (o.kind == PK_EDGE)   ? "edge" : "centre";
    const int c = orbit_solved_count(O, s, faces, i);
    solved[i] = c;
    total[i] = (int)o.pieces.size();
    frac[i] = (double)c / (double)o.pieces.size();
  }

  return DataFrame::create(
      _["orbit"] = seq_len(k),
      _["label"] = label,
      _["kind"] = kind_name,
      _["solved"] = solved,
      _["total"] = total,
      _["fraction"] = frac,
      _["stringsAsFactors"] = false);
}

// Pieces home over pieces in total: the blunt summary of the above.
// [[Rcpp::export]]
IntegerVector cube_pieces_home_cpp(IntegerVector state) {
  const int n = side_from_state(state.size(), "cube_pieces_home");
  const Orbits& O = orbits_of(n);
  const std::vector<int> s(state.begin(), state.end());
  return IntegerVector::create(_["home"] = pieces_home(O, s),
                               _["total"] = O.n_pieces());
}
