#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <string>
#include <queue>
#include <algorithm>
#include "cayley_utils.h"
#include "celestial_coords.h"
#include "perm_group.h"

using namespace Rcpp;

// Move indices arrive 1-based from R and index the group's alphabet; a search
// may use any subset of it. Nothing here knows what a move does -- the group
// answers that -- so the same sweep measures a TopSpin ring and a cube.
static std::vector<int> check_moves(const IntegerVector& moves,
                                    const PermGroup& g) {
  std::vector<int> idx;
  idx.reserve(moves.size());
  for (int i = 0; i < moves.size(); i++) {
    int m = moves[i] - 1;
    if (m < 0 || m >= g.n_moves()) stop("move index %d out of range", moves[i]);
    idx.push_back(m);
  }
  if (idx.empty()) stop("moves must contain at least one operation");
  return idx;
}

// The nL/nR/nX counters below are TopSpin's three operations. For any other
// alphabet they have no meaning, so they are reported as zero and the
// celestial coordinates derived from them are left flat.
static bool is_lrx(const PermGroup& g) {
  if (g.n_moves() > 3) return false;
  for (int m = 0; m < g.n_moves(); m++) {
    const std::string& s = g.move_name(m);
    if (s != "L" && s != "1" && s != "R" && s != "2" && s != "X" && s != "3") {
      return false;
    }
  }
  return true;
}

// Which of L/R/X a move is (1/2/3), or 0 when the group is not TopSpin.
static int lrx_code(const PermGroup& g, int m) {
  const std::string& s = g.move_name(m);
  if (s == "L" || s == "1") return 1;
  if (s == "R" || s == "2") return 2;
  if (s == "X" || s == "3") return 3;
  return 0;
}

// One full BFS over the component reachable from `start`.
//
// Vertices are interned into `index` (key -> id) as they are discovered, so
// ids are assigned in BFS order and id 0 is always the start state. `states`
// holds the state vector of each id, parallel to `dist`.
//
// The nL/nR/nX counters are carried along the BFS tree: a vertex inherits the
// counters of the parent that first reached it, plus the op used. Celestial
// coordinates are a property of a path, not of a vertex, so this pins them to
// the specific shortest path BFS found first.
struct BfsResult {
  std::vector<std::string> keys;
  std::vector<std::vector<int> > states;
  std::vector<int> dist;
  std::vector<int> nL, nR, nX;
};

static void bfs_from(const std::vector<int>& start,
                     const PermGroup& g,
                     const std::vector<int>& move_idx,
                     BfsResult& out) {
  std::unordered_map<std::string, int> index;
  bool lrx = is_lrx(g);

  std::string start_key = state_to_key(start);
  index[start_key] = 0;
  out.keys.push_back(start_key);
  out.states.push_back(start);
  out.dist.push_back(0);
  out.nL.push_back(0);
  out.nR.push_back(0);
  out.nX.push_back(0);

  std::queue<int> q;
  q.push(0);

  std::vector<int> child;
  while (!q.empty()) {
    int cur = q.front();
    q.pop();

    for (size_t mi = 0; mi < move_idx.size(); mi++) {
      int m = move_idx[mi];
      child = out.states[cur];
      g.apply(child, m);

      std::string key = state_to_key(child);
      if (index.find(key) != index.end()) continue;

      int id = (int)out.keys.size();
      index[key] = id;
      out.keys.push_back(key);
      out.states.push_back(child);
      out.dist.push_back(out.dist[cur] + 1);
      int op = lrx ? lrx_code(g, m) : 0;
      out.nL.push_back(out.nL[cur] + (op == 1 ? 1 : 0));
      out.nR.push_back(out.nR[cur] + (op == 2 ? 1 : 0));
      out.nX.push_back(out.nX[cur] + (op == 3 ? 1 : 0));
      q.push(id);
    }
  }
}

// Distances only, over a vertex set already interned by a first BFS. Used by
// the all_pairs sweep, where every source explores the same component and the
// key -> id mapping can therefore be shared instead of rebuilt n! times.
static void bfs_dist_only(int source,
                          const PermGroup& g,
                          const std::vector<int>& move_idx,
                          const std::vector<std::vector<int> >& states,
                          const std::unordered_map<std::string, int>& index,
                          std::vector<int>& dist) {
  std::fill(dist.begin(), dist.end(), -1);
  dist[source] = 0;

  std::queue<int> q;
  q.push(source);

  std::vector<int> child;
  while (!q.empty()) {
    int cur = q.front();
    q.pop();

    for (size_t mi = 0; mi < move_idx.size(); mi++) {
      child = states[cur];
      g.apply(child, move_idx[mi]);

      std::unordered_map<std::string, int>::const_iterator it =
        index.find(state_to_key(child));
      if (it == index.end()) continue;   // cannot happen for a closed set
      int id = it->second;
      if (dist[id] >= 0) continue;

      dist[id] = dist[cur] + 1;
      q.push(id);
    }
  }
}

static DataFrame bfs_to_dataframe(const BfsResult& r) {
  int n = (int)r.keys.size();
  NumericVector theta(n), phi(n), omega(n);

  for (int i = 0; i < n; i++) {
    CelestialCoords c = create_empty_coords();
    c = update_coords(c, r.nL[i], r.nR[i], r.nX[i]);
    theta[i] = c.theta;
    phi[i] = c.phi;
    omega[i] = c.omega_conformal;
  }

  return DataFrame::create(
    _["state_str"] = wrap(r.keys),
    _["dist"] = wrap(r.dist),
    _["nL"] = wrap(r.nL),
    _["nR"] = wrap(r.nR),
    _["nX"] = wrap(r.nX),
    _["theta"] = theta,
    _["phi"] = phi,
    _["omega"] = omega,
    _["stringsAsFactors"] = false
  );
}

// [[Rcpp::export]]
DataFrame cayley_bfs_full_cpp(IntegerVector start_state,
                              SEXP group,
                              IntegerVector moves) {
  std::vector<int> start(start_state.begin(), start_state.end());
  if (start.empty()) stop("start_state must not be empty");

  XPtr<PermGroup> g(group);
  std::vector<int> move_idx = check_moves(moves, *g);

  BfsResult r;
  bfs_from(start, *g, move_idx, r);
  return bfs_to_dataframe(r);
}

// Diameter over the component reachable from `start_state`.
//
// method 0 = all_pairs: BFS from every vertex; the true diameter, and every
//   diametral pair. Cost is |V| BFS runs.
// method 1 = from_start: a single BFS; reports the eccentricity of the start
//   vertex and the pairs (start, v) realising it. Equals the diameter only
//   when the graph is vertex-transitive.
//
// max_pairs caps how many pairs are materialised; n_pairs in the result is
// always the honest total, capped or not.
//
// [[Rcpp::export]]
List cayley_graph_diameter_cpp(IntegerVector start_state,
                               SEXP group,
                               IntegerVector moves,
                               int method,
                               double max_pairs,
                               bool verbose) {
  std::vector<int> start(start_state.begin(), start_state.end());
  if (start.empty()) stop("start_state must not be empty");

  XPtr<PermGroup> g(group);
  std::vector<int> move_idx = check_moves(moves, *g);

  BfsResult r;
  bfs_from(start, *g, move_idx, r);
  int nv = (int)r.keys.size();

  if (verbose) Rcout << "Vertices reachable: " << nv << "\n";

  std::vector<int> ecc(nv, 0);
  std::vector<int> pair_from, pair_to, pair_dist;
  double n_pairs = 0;
  int diameter = 0;

  bool truncated = false;

  if (method == 1) {
    // Single BFS: eccentricity of the start vertex only.
    for (int i = 0; i < nv; i++) if (r.dist[i] > diameter) diameter = r.dist[i];
    ecc[0] = diameter;
    for (int i = 1; i < nv; i++) ecc[i] = NA_INTEGER;

    for (int i = 0; i < nv; i++) {
      if (r.dist[i] != diameter) continue;
      n_pairs += 1;
      if ((double)pair_from.size() >= max_pairs) { truncated = true; continue; }
      pair_from.push_back(0);
      pair_to.push_back(i);
      pair_dist.push_back(diameter);
    }
  } else {
    // All-pairs sweep. Rebuild the key -> id map once and reuse it.
    std::unordered_map<std::string, int> index;
    index.reserve(nv * 2);
    for (int i = 0; i < nv; i++) index[r.keys[i]] = i;

    // Pass 1: eccentricities, so the diameter is known before any pair is
    // recorded. Pass 2 then keeps only pairs at exactly that distance.
    std::vector<int> dist(nv);
    for (int s = 0; s < nv; s++) {
      if (verbose && nv > 200 && s % (nv / 20 + 1) == 0) {
        Rcout << "  pass 1: " << s << "/" << nv << "\n";
      }
      Rcpp::checkUserInterrupt();
      bfs_dist_only(s, *g, move_idx, r.states, index, dist);
      int e = 0;
      for (int i = 0; i < nv; i++) if (dist[i] > e) e = dist[i];
      ecc[s] = e;
      if (e > diameter) diameter = e;
    }

    for (int s = 0; s < nv; s++) {
      if (ecc[s] != diameter) continue;   // only eccentric vertices can be ends
      if (verbose && nv > 200) Rcout << "  pass 2: source " << s << "\n";
      Rcpp::checkUserInterrupt();
      bfs_dist_only(s, *g, move_idx, r.states, index, dist);
      for (int i = 0; i < nv; i++) {
        if (dist[i] != diameter) continue;
        if (i < s) continue;              // unordered pairs, recorded once
        n_pairs += 1;
        if ((double)pair_from.size() >= max_pairs) { truncated = true; continue; }
        pair_from.push_back(s);
        pair_to.push_back(i);
        pair_dist.push_back(diameter);
      }
    }
  }

  // Distance histogram from the start vertex.
  std::vector<int> hist(diameter + 1, 0);
  for (int i = 0; i < nv; i++) {
    if (r.dist[i] >= 0 && r.dist[i] <= diameter) hist[r.dist[i]]++;
  }
  IntegerVector hist_d(diameter + 1), hist_n(diameter + 1);
  for (int d = 0; d <= diameter; d++) { hist_d[d] = d; hist_n[d] = hist[d]; }

  return List::create(
    _["diameter"] = diameter,
    _["n_vertices"] = nv,
    _["n_pairs"] = n_pairs,
    _["truncated"] = truncated,
    _["pair_from"] = wrap(pair_from),
    _["pair_to"] = wrap(pair_to),
    _["pair_dist"] = wrap(pair_dist),
    _["ecc"] = wrap(ecc),
    _["bfs"] = bfs_to_dataframe(r),
    _["hist_dist"] = hist_d,
    _["hist_count"] = hist_n
  );
}
