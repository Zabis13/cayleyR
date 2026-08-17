#include <Rcpp.h>
#include <iomanip>
#include "kociemba3.h"
#include "kociemba4.h"
#include "cube_solve.h"

using namespace Rcpp;

// ---- R's view of the phase solvers --------------------------------------
//
// The solvers speak in pieces and the package speaks in stickers, so the
// boundary is here: a state vector goes in, a character vector of moves comes
// out, and the piece representation stays inside.

// [[Rcpp::export]]
CharacterVector cube_kociemba_cpp(IntegerVector state,
                                  int max_depth1 = 12,
                                  int max_depth2 = 18,
                                  double node_budget = 2e8) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 54) {
    stop("a 3x3x3 state has 54 stickers, got %d", (int)s.size());
  }

  // Slices turn the centres, and a piece is identified by its colours read
  // against the centres -- so on a cube whose centres have moved, every cubie
  // reads as the wrong one and the solve is nonsense. The four human methods
  // in this package all open by turning the cube back; this does the same, and
  // the rotation goes on the front of the solution so the answer applies to
  // the cube as it was handed over.
  std::vector<std::string> prefix;
  {
    std::vector<int> rot = cube_solve::orient_to_centres(s);
    if (!rot.empty()) {
      s = cube_search::apply_word(s, rot);
      std::vector<std::string> nm = cube_search::word_names(rot, 3);
      for (size_t i = 0; i < nm.size(); i++) prefix.push_back(nm[i]);
    }
  }

  kociemba::PieceState start = kociemba3::from_stickers(s);

  kociemba::SearchLimits lim1, lim2;
  lim1.max_depth = max_depth1;
  lim2.max_depth = max_depth2;
  lim1.node_budget = (long)node_budget;
  lim2.node_budget = (long)node_budget;

  std::vector<std::string> word;
  const bool ok = kociemba3::solver3().solve(start, word, lim1, lim2);
  if (!ok) return CharacterVector(0);

  // Inside the solver a half turn is one move, because phase 2's group is
  // defined that way. The package's alphabet is quarter turns throughout, and
  // a path it cannot apply is not a path -- so "U2" leaves here as "U" "U".
  // The metric the search counted in and the metric the answer is written in
  // are different things, and only the first is the phase's business.
  std::vector<std::string> full(prefix);
  for (size_t i = 0; i < word.size(); i++) {
    const std::string& mv = word[i];
    if (mv.size() > 1 && mv[mv.size() - 1] == '2') {
      const std::string base = mv.substr(0, mv.size() - 1);
      full.push_back(base);
      full.push_back(base);
    } else {
      full.push_back(mv);
    }
  }

  CharacterVector out(full.size());
  for (size_t i = 0; i < full.size(); i++) out[i] = full[i];
  return out;
}

// Build the prune tables now rather than on the first solve, so a caller can
// pay that cost where they expect it.
// [[Rcpp::export]]
void cube_kociemba_init_cpp(double table1 = 4194304, int depth1 = 0,
                            double table2 = 16777216, int depth2 = 0) {
  kociemba3::solver3().init((size_t)table1, depth1, (size_t)table2, depth2);
}

// The piece reading of a state, for tests and for anyone who wants to see what
// the solver sees.
// [[Rcpp::export]]
List cube_cubie_pieces_cpp(IntegerVector state) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 54) stop("a 3x3x3 state has 54 stickers, got %d", (int)s.size());
  kociemba::PieceState p = kociemba3::from_stickers(s);

  IntegerVector cp(8), co(8), ep(12), eo(12);
  for (int i = 0; i < 8; i++) {
    cp[i] = p.perm[kociemba3::C_OFF + i] + 1;
    co[i] = p.ori[kociemba3::C_OFF + i];
  }
  for (int i = 0; i < 12; i++) {
    ep[i] = p.perm[kociemba3::E_OFF + i] - kociemba3::E_OFF + 1;
    eo[i] = p.ori[kociemba3::E_OFF + i];
  }
  return List::create(_["corner_perm"] = cp, _["corner_ori"] = co,
                      _["edge_perm"] = ep, _["edge_ori"] = eo);
}

// Whether a state is in G1: every piece oriented and the E slice occupied by
// E-slice edges. This is what phase 1 aims at, exposed so a test can check the
// phase did what it claims.
// [[Rcpp::export]]
bool cube_in_g1_cpp(IntegerVector state) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 54) stop("a 3x3x3 state has 54 stickers, got %d", (int)s.size());
  kociemba::PieceState p = kociemba3::from_stickers(s);
  for (int i = 0; i < 8; i++) if (p.ori[kociemba3::C_OFF + i] != 0) return false;
  for (int i = 0; i < 12; i++) if (p.ori[kociemba3::E_OFF + i] != 0) return false;
  for (int i = 8; i < 12; i++) {
    if (p.perm[kociemba3::E_OFF + i] < kociemba3::E_OFF + 8) return false;
  }
  return true;
}

// What the last solve actually did, phase by phase. "Found", "no solution" and
// "ran out of budget" are three different facts, and a caller that cannot tell
// them apart goes looking for a bug in the algorithm when the answer is to
// raise a limit.
// [[Rcpp::export]]
List cube_kociemba_last_cpp() {
  kociemba3::Solver3& S = kociemba3::solver3();
  const char* nm[3] = {"found", "no_solution", "exhausted"};
  return List::create(
    _["phase1"] = std::string(nm[(int)S.last_outcome1]),
    _["phase1_nodes"] = (double)S.last_nodes1,
    _["phase2"] = std::string(nm[(int)S.last_outcome2]),
    _["phase2_nodes"] = (double)S.last_nodes2);
}

// ---- The 4x4x4 ------------------------------------------------------------

// The three reduction phases, as moves of the package's 4x4x4 alphabet. The
// cube that comes back behaves as a 3x3x3; finishing it is the caller's next
// step, and cube_reduce.h is the bridge.
// A phase counts a generator as one step whatever it is -- a half turn, or a
// wide turn written as a word of two moves. The package's alphabet is quarter
// turns of single layers and nothing else, so each generator is spelled back
// out here: split on spaces, and expand a trailing 2 into the move twice.
static std::vector<std::string> expand_generator_words(
    const std::vector<std::string>& word, size_t upto) {
  std::vector<std::string> full;
  for (size_t i = 0; i < upto && i < word.size(); i++) {
    const std::string& g = word[i];
    for (size_t a = 0; a < g.size();) {
      while (a < g.size() && g[a] == ' ') a++;
      size_t b = a;
      while (b < g.size() && g[b] != ' ') b++;
      if (b > a) {
        const std::string tok = g.substr(a, b - a);
        if (tok.size() > 1 && tok[tok.size() - 1] == '2') {
          const std::string base = tok.substr(0, tok.size() - 1);
          full.push_back(base);
          full.push_back(base);
        } else {
          full.push_back(tok);
        }
      }
      a = b;
    }
  }
  return full;
}

// Prints one progress line. Rcpp::Rcout rather than std::cout, so the text
// goes through R's own console and appears when it is written rather than
// whenever the C runtime feels like flushing.
static void report_progress(int depth_limit, long nodes) {
  Rcpp::Rcout << "    phase " << kociemba4::reporting_phase()
              << "  depth " << depth_limit
              << "  " << nodes << " nodes\n";
  R_FlushConsole();
}

// Prints one line as a level of iterative deepening finishes: what it cost and
// how fast it went. The rate is the part worth having -- seconds and nodes both
// grow with the level, so only their ratio says whether a level was slow
// because it was large or slow because the search itself got harder.
//
// Fill time appears only when there was any: a phase running against a table
// loaded from disk never fills, and a column of zeroes on every line would say
// nothing on every line.
static void report_depth_done(const kociemba::DepthStat& st) {
  Rcpp::Rcout << "    phase " << kociemba4::reporting_phase()
              << "  depth " << st.limit
              << "  done: " << st.nodes << " nodes in "
              << std::fixed << std::setprecision(2) << st.search_secs << "s";
  if (st.search_secs > 0.0) {
    Rcpp::Rcout << " (" << (long)(st.nodes / st.search_secs) << " nodes/s)";
  }
  if (st.fill_secs > 0.005) {
    Rcpp::Rcout << ", fill to " << st.fill_to << " took "
                << st.fill_secs << "s";
  }
  Rcpp::Rcout << "\n";
  R_FlushConsole();
}

// The per-level records of one phase, as a data.frame.
//
// `nodes_per_sec` is computed here rather than stored, and is NA for a level
// too fast to time: dividing by a zero that means "below the clock's
// resolution" gives an infinity that reads like a measurement.
static DataFrame depth_stats_df(const std::vector<kociemba::DepthStat>& v) {
  const int n = (int)v.size();
  IntegerVector limit(n), fill_to(n);
  NumericVector nodes(n), fill_secs(n), search_secs(n), rate(n);
  for (int i = 0; i < n; i++) {
    limit[i] = v[i].limit;
    fill_to[i] = v[i].fill_to < 0 ? NA_INTEGER : v[i].fill_to;
    nodes[i] = (double)v[i].nodes;
    fill_secs[i] = v[i].fill_secs;
    search_secs[i] = v[i].search_secs;
    rate[i] = v[i].search_secs > 0.0
      ? (double)v[i].nodes / v[i].search_secs : NA_REAL;
  }
  return DataFrame::create(
    _["depth"] = limit,
    _["fill_to"] = fill_to,
    _["nodes"] = nodes,
    _["fill_secs"] = fill_secs,
    _["search_secs"] = search_secs,
    _["nodes_per_sec"] = rate,
    _["stringsAsFactors"] = false);
}

// [[Rcpp::export]]
CharacterVector cube_kociemba4_reduce_cpp(IntegerVector state,
                                          int max_depth1 = 10,
                                          int max_depth2 = 12,
                                          int max_depth3 = 14,
                                          double node_budget = 5e7,
                                          double progress_every = 0,
                                          int prune_depth_bonus = 0) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 96) {
    stop("a 4x4x4 state has 96 stickers, got %d", (int)s.size());
  }
  kociemba::PieceState start = kociemba4::from_stickers4(s);

  kociemba::SearchLimits l1, l2, l3;
  l1.max_depth = max_depth1; l2.max_depth = max_depth2; l3.max_depth = max_depth3;
  l1.node_budget = l2.node_budget = l3.node_budget = (long)node_budget;
  l1.progress_every = l2.progress_every = l3.progress_every =
    (long)progress_every;
  // Only phase 3 -- it is the one being measured, and deepening the other two
  // would change what they hand over, so the comparison would not be like for
  // like.
  l3.prune_depth_bonus = prune_depth_bonus;

  std::vector<std::string> word;
  const bool ok = kociemba4::solver4().reduce(
    start, word, l1, l2, l3,
    progress_every > 0 ? report_progress : 0);
  if (!ok) return CharacterVector(0);

  const std::vector<std::string> full =
    expand_generator_words(word, word.size());
  CharacterVector out(full.size());
  for (size_t i = 0; i < full.size(); i++) out[i] = full[i];
  return out;
}

// What each reduction phase did.
//
// The seconds and the move counts are here so a profile does not have to be
// assembled from outside by running the phases separately. That approach was
// tried and is wrong twice over: pricing phase 2 as (phases 1+2) minus (phase 1)
// runs phase 1 twice under different depth limits, so the difference is between
// two searches rather than one phase; and a path stitched from those pieces
// does not always solve the cube. These come from the one reduce() that ran.
//
// moves1 and moves2 are cumulative -- moves after phase 1, moves after phase 2 --
// in the solver's own generator words, before expand_generator_words() turns
// them into named turns. Phase 2's own contribution is the difference. They are
// zero for a phase that did not finish, so read them with the outcome.
// [[Rcpp::export]]
List cube_kociemba4_last_cpp() {
  kociemba4::Solver4& S = kociemba4::solver4();
  const char* nm[3] = {"found", "no_solution", "exhausted"};
  return List::create(
    _["phase1"] = std::string(nm[(int)S.outcome[0]]),
    _["phase1_nodes"] = (double)S.nodes[0],
    _["phase1_secs"] = S.secs[0],
    _["phase2"] = std::string(nm[(int)S.outcome[1]]),
    _["phase2_nodes"] = (double)S.nodes[1],
    _["phase2_secs"] = S.secs[1],
    _["phase3"] = std::string(nm[(int)S.outcome[2]]),
    _["phase3_nodes"] = (double)S.nodes[2],
    _["phase3_secs"] = S.secs[2],
    _["moves_after_phase1"] = (double)S.moves_after_phase1,
    _["moves_after_phase2"] = (double)S.moves_after_phase2);
}

// What the reduction's prune tables actually hold, read off the tables
// themselves after a run.
//
// The outcome report says a phase spent its budget; it does not say whether
// the heuristic that was supposed to stop it from having to was doing
// anything. These are the figures that answer that:
//
//   size         slots allocated. grow_to() sizes the table from how the last
//                two levels grew, within [min_size, max_size]
//   built_depth  levels filled. get() scores anything beyond this as
//                built_depth + 1, so a table built to 5 cannot tell a state 6
//                moves out from one 20 moves out
//   filled       slots holding a value. Close to size means the table is
//                saturated: fresh states collide onto occupied slots, take
//                whatever shallow value is there, and the bound goes soft
//   depth_counts how many slots hold each distance. A table doing its job has
//                most of its mass at the deepest level built; one whose mass
//                sits at the shallow end is one whose entries mostly say
//                "close to the goal", which prunes nothing
//   n_visits     states the fill derived and offered to the table
//   n_writes     of those, the ones that found an empty slot and were kept
//   n_collisions the rest, which the table could not record
//   waste_ratio  n_collisions / n_visits -- the share of the walk that cost
//                time and taught the table nothing
//
// A saturated table is the failure worth naming: it does not get better with
// depth, because every extra level lands on slots that are already taken.
// filled says how full it is now; waste_ratio says whether the last level
// filled was already fighting for space. The two answer different halves of
// "is another level worth walking", and neither on its own separates a table
// that is full from a walk that never reached the states being scored -- for
// that, compare filled against the stub bounds those states come back with.
// [[Rcpp::export]]
List cube_kociemba4_tables_cpp() {
  kociemba4::Solver4& S = kociemba4::solver4();
  const kociemba::PruneTable* pt[3] = {&S.p1, &S.p2, &S.p3};

  List out(3);
  CharacterVector nms(3);
  for (int i = 0; i < 3; i++) {
    const kociemba::PruneTable& p = *pt[i];
    const size_t sz = p.table.size();
    size_t filled = 0;
    // Distances run 0..254 in the table's own encoding (stored value minus
    // one); 256 buckets covers every value a uint8_t can hold.
    std::vector<double> counts(256, 0.0);
    for (size_t k = 0; k < sz; k++) {
      const uint8_t v = p.table[k];
      if (v != 0) { filled++; counts[v - 1] += 1.0; }
    }
    // Trimmed to the deepest level actually present, so the vector handed back
    // is as long as the table is deep rather than always 256.
    int top = 0;
    for (int d = 255; d >= 0; d--) { if (counts[d] > 0) { top = d; break; } }
    counts.resize((size_t)top + 1);

    nms[i] = std::string("phase") + (char)('1' + i);
    out[i] = List::create(
      _["size"] = (double)sz,
      _["min_size"] = (double)p.min_size,
      _["max_size"] = (double)p.max_size,
      _["built_depth"] = (int)p.built_depth,
      _["n_grows"] = (double)p.n_grows,
      _["filled"] = (double)filled,
      _["fill_ratio"] = sz ? (double)filled / (double)sz : 0.0,
      _["n_visits"] = (double)p.n_visits,
      _["n_writes"] = (double)p.n_writes,
      _["n_collisions"] = (double)p.n_collisions,
      _["waste_ratio"] = p.n_visits ?
        (double)p.n_collisions / (double)p.n_visits : 0.0,
      _["depth_counts"] = NumericVector(counts.begin(), counts.end()));
  }
  out.attr("names") = nms;
  return out;
}

// Whether a state sits at a given phase's goal, judged the way the search
// judges it: derive the state through that phase's coordinate and compare
// against the phase's own goal list.
//
// This is the test the phases apply internally, and having it in R is what
// lets a diagnostic ask the question the outcome codes cannot answer. "Phase 3
// exhausted" says the search gave up; it does not say whether the cube it was
// handed could have been finished at all. Phase 2 can reach its own goal and
// still leave the cube somewhere phase 3 has no path from -- and then phase 3
// spends its whole budget proving a negative, which reads exactly like a slow
// search but is a phase-2 fault.
//
// `phase` is 1, 2 or 3.
// [[Rcpp::export]]
bool cube_at_phase_goal_cpp(IntegerVector state, int phase) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 96) stop("a 4x4x4 state has 96 stickers, got %d", (int)s.size());
  if (phase < 1 || phase > 3) stop("phase must be 1, 2 or 3, got %d", phase);

  kociemba4::Solver4& S = kociemba4::solver4();
  S.init();

  const kociemba::PieceState cur = kociemba4::from_stickers4(s);
  const kociemba::Deriver* dv[3] = {&S.d1, &S.d2, &S.d3};
  const std::vector<kociemba::PieceState>* gl[3] =
    {&S.goals1, &S.goals2, &S.goals3};

  kociemba::PieceState a, b;
  dv[phase - 1]->derive(cur, a);
  const std::vector<kociemba::PieceState>& goals = *gl[phase - 1];
  for (size_t i = 0; i < goals.size(); i++) {
    dv[phase - 1]->derive(goals[i], b);
    if (a == b) return true;
  }
  return false;
}

// Run phase 3 alone, from a state phases 1 and 2 have already been through.
//
// "Can phase 3 finish from here?" is a search, not a comparison. Asking
// whether the state already sits at phase 3's goal answers a different
// question and answers it "no" for every cube that still needs solving --
// which is how a diagnostic can end up blaming the phase before.
//
// So: give phase 3 the cube, let it search, and report what it says. A
// `found` means the handover was sound and phase 3 could do its job; an
// `exhausted` or `no_solution` from a cube whose solution is known to be
// short is phase 3's own fault.
//
// Fill phase 3's prune table to a given depth and stop, without searching.
//
// Every other way into this table goes through a search, which builds it as a
// side effect and sizes it from its own branching estimate. That is fine for
// solving and useless for measuring: the table's cost and coverage arrive
// tangled with the cost of the search that triggered it, and the depth reached
// is whatever the search happened to want.
//
// Filling is idempotent and additive -- extend_prune_table skips levels already
// built -- so a caller wanting level d alone must start from a fresh process,
// which is how the accompanying script drives it.
//
// `table_size` matters more than it looks. A real search sizes the table from
// the estimated cost of the level it is about to search, and filling directly
// bypasses that, leaving whatever size init() started with. Measuring a fill at
// the starting size answers a question nobody asked: the earlier saturation
// figures -- 22% full, 146M collisions of 150M writes -- were taken at 1<<24
// while the search itself is entitled to 1<<28. Pass the size the measurement
// is about, and 0 to keep whatever the table already has.
// Raise or lower the ceiling on phase 3's prune table, in slots.
//
// Must be called before anything builds the solver, since init() reads it once
// and `if (ready) return` means it is not read again. Returns the value in
// force after the call, so a caller can check it took.
//
// The default of 1<<28 comes from twips rather than from a measurement here,
// and the measurements here suggest it binds: a breadth-first fill to depth 7
// puts 47.6M entries in 268M slots and loses states to collisions on the way.
// [[Rcpp::export]]
double cube_kociemba4_set_table_size_cpp(double slots) {
  if (slots > 0) {
    size_t sz = 1;
    while (sz < (size_t)slots) sz <<= 1;
    kociemba4::phase3_max_size_ref() = sz;
  }
  return (double)kociemba4::phase3_max_size();
}

// Write phase 3's table to a file, and read one back.
//
// 2^28 slots is a byte each, so the file is 256 MB and takes seconds to move
// either way against the 88 seconds the fill costs. For a batch of a thousand
// cubes that is the difference between paying the fill once per process and
// paying it once ever.
//
// The load checks a signature over the generators and goals before accepting
// anything. A table built from a different generator set is not detectably
// wrong at the call site -- it returns bounds, they are simply too large, and
// too-large bounds prune away branches that hold solutions. The search then
// says "no solution" about a cube that has one. This package has already had
// its phase 3 generators wrong once, so the check is not hypothetical.
// [[Rcpp::export]]
bool cube_kociemba4_save_phase3_cpp(std::string path) {
  kociemba4::Solver4& S = kociemba4::solver4();
  S.init();
  const uint64_t sig = kociemba::prune_signature(S.spec3, S.goals3);
  return kociemba::save_prune_table(path, S.p3, sig);
}

// [[Rcpp::export]]
List cube_kociemba4_load_phase3_cpp(std::string path) {
  kociemba4::Solver4& S = kociemba4::solver4();
  S.init();
  const uint64_t sig = kociemba::prune_signature(S.spec3, S.goals3);
  const kociemba::PruneLoadResult r =
    kociemba::load_prune_table(path, S.p3, sig);

  const char* why[6] = {"ok", "no such file", "not a prune table file",
                        "written by a different version",
                        "built from different generators or goals",
                        "file is truncated"};

  return List::create(
    _["ok"] = (r == kociemba::PRUNE_LOAD_OK),
    _["reason"] = std::string(why[(int)r]),
    _["built_depth"] = (int)S.p3.built_depth,
    _["size"] = (double)S.p3.table.size(),
    _["n_writes"] = (double)S.p3.n_writes);
}

// The same three operations for any of the three phases.
//
// Phase 3 got them first because its fill is the expensive one -- 88 seconds
// against a table phases 1 and 2 fill in a fraction of that. But the profiling
// that prompted these showed the cost is not where the size is: on a cube
// solved in 36 seconds, phases 1 and 2 took 30.6 of them and phase 3 took 5.5,
// because those two tables are filled lazily inside the search (see
// ida_search_outcome, which extends to limit/2 at every level) and grow_to()
// discards what it has each time it reallocates. A table prepared once and
// loaded does not pay either cost.
//
// Selecting the phase by number rather than writing three copies: the tables
// differ in what they are built from, not in what is done to them, and the
// spec/deriver/goals triple is the only thing that varies. A bad number is an
// error rather than a default, because silently measuring phase 1 when the
// caller asked for phase 4 is the kind of wrong that reads as a puzzling
// result instead of a mistake.
namespace {

struct PhaseParts {
  kociemba::PuzzleSpec* spec;
  kociemba::Deriver* deriver;
  std::vector<kociemba::PieceState>* goals;
  kociemba::PruneTable* table;
};

PhaseParts phase_parts(int phase) {
  kociemba4::Solver4& S = kociemba4::solver4();
  S.init();
  PhaseParts p;
  switch (phase) {
    case 1: p.spec = &S.spec1; p.deriver = &S.d1;
            p.goals = &S.goals1; p.table = &S.p1; break;
    case 2: p.spec = &S.spec2; p.deriver = &S.d2;
            p.goals = &S.goals2; p.table = &S.p2; break;
    case 3: p.spec = &S.spec3; p.deriver = &S.d3;
            p.goals = &S.goals3; p.table = &S.p3; break;
    default:
      stop("phase must be 1, 2 or 3, got %d", phase);
  }
  return p;
}

}  // namespace

// [[Rcpp::export]]
bool cube_kociemba4_save_phase_cpp(std::string path, int phase) {
  PhaseParts p = phase_parts(phase);
  const uint64_t sig = kociemba::prune_signature(*p.spec, *p.goals);
  return kociemba::save_prune_table(path, *p.table, sig);
}

// [[Rcpp::export]]
List cube_kociemba4_load_phase_cpp(std::string path, int phase) {
  PhaseParts p = phase_parts(phase);
  const uint64_t sig = kociemba::prune_signature(*p.spec, *p.goals);
  const kociemba::PruneLoadResult r =
    kociemba::load_prune_table(path, *p.table, sig);

  const char* why[6] = {"ok", "no such file", "not a prune table file",
                        "written by a different version",
                        "built from different generators or goals",
                        "file is truncated"};

  return List::create(
    _["ok"] = (r == kociemba::PRUNE_LOAD_OK),
    _["reason"] = std::string(why[(int)r]),
    _["phase"] = phase,
    _["built_depth"] = (int)p.table->built_depth,
    _["size"] = (double)p.table->table.size(),
    _["n_writes"] = (double)p.table->n_writes);
}

// `table_size` is passed through to grow_to(), which only ever grows and is
// capped by the max_size init() set for that phase -- 1<<24 for phases 1 and 2,
// phase3_max_size() for phase 3. Asking for more than the cap gets the cap.
// [[Rcpp::export]]
List cube_kociemba4_fill_phase_cpp(int depth, int phase, double table_size = 0,
                                   bool breadth_first = true,
                                   double max_frontier = 40e6) {
  PhaseParts p = phase_parts(phase);

  if (table_size > 0) p.table->grow_to((size_t)table_size);

  if (breadth_first) {
    kociemba::fill_prune_table_bfs(*p.spec, *p.deriver, *p.goals, depth,
                                   *p.table, (size_t)max_frontier);
  } else {
    kociemba::extend_prune_table(*p.spec, *p.deriver, *p.goals, depth,
                                 *p.table);
  }

  return List::create(
    _["depth"] = depth,
    _["phase"] = phase,
    _["built_depth"] = (int)p.table->built_depth,
    _["size"] = (double)p.table->table.size(),
    _["n_visits"] = (double)p.table->n_visits,
    _["n_writes"] = (double)p.table->n_writes,
    _["n_collisions"] = (double)p.table->n_collisions);
}

// `breadth_first` chooses which fill does the work, and the two are not
// equivalent in cost. The depth-first fill walks words and revisits states --
// measured to depth 6, 150 million visits for 8 million writes -- while the
// breadth-first one walks states and visits each once. Both leave an
// admissible table; the second is roughly a hundred times cheaper at this
// depth and needs memory for its frontier, capped by `max_frontier`.
// [[Rcpp::export]]
List cube_kociemba4_fill_phase3_cpp(int depth, double table_size = 0,
                                    bool breadth_first = true,
                                    double max_frontier = 40e6) {
  kociemba4::Solver4& S = kociemba4::solver4();
  S.init();

  if (table_size > 0) S.p3.grow_to((size_t)table_size);

  if (breadth_first) {
    kociemba::fill_prune_table_bfs(S.spec3, S.d3, S.goals3, depth, S.p3,
                                   (size_t)max_frontier);
  } else {
    kociemba::extend_prune_table(S.spec3, S.d3, S.goals3, depth, S.p3);
  }

  // Timing is left to the caller: proc.time() around this call measures the
  // same thing without this file having to reach for a clock.
  return List::create(
    _["depth"] = depth,
    _["built_depth"] = (int)S.p3.built_depth,
    _["size"] = (double)S.p3.table.size(),
    _["n_visits"] = (double)S.p3.n_visits,
    _["n_writes"] = (double)S.p3.n_writes,
    _["n_collisions"] = (double)S.p3.n_collisions);
}

// [[Rcpp::export]]
List cube_kociemba4_phase3_cpp(IntegerVector state,
                               int max_depth3 = 14,
                               double node_budget = 5e7,
                               int prune_depth_bonus = 0,
                               bool use_exact_centres = false,
                               double progress_every = 0) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 96) {
    stop("a 4x4x4 state has 96 stickers, got %d", (int)s.size());
  }
  kociemba4::Solver4& S = kociemba4::solver4();
  S.init();

  // The solver is a singleton, so the flag has to be set every call rather
  // than left wherever the last call put it.
  S.use_exact_centres = use_exact_centres;
  if (use_exact_centres) S.ensure_centre_table();

  kociemba::PieceState cur = kociemba4::from_stickers4(s);
  kociemba::SearchLimits l3;
  l3.max_depth = max_depth3;
  l3.node_budget = (long)node_budget;
  l3.prune_depth_bonus = prune_depth_bonus;
  // Without this the phase is silent, and phase 3 is the one that most needs
  // not to be: it is the only stage that can spend tens of seconds inside a
  // single call, and a caller watching a still screen cannot tell that from a
  // hang. Measured, one orientation of one cube exhausted 5e7 nodes in 43 s
  // with nothing printed from beginning to end.
  l3.progress_every = (long)progress_every;

  std::vector<std::string> word;
  std::vector<kociemba::PieceState> trace;
  std::vector<std::string> best_names;
  const bool ok = S.run_phase(2, S.spec3, S.d3, S.p3, S.goals3, cur, l3, word,
                              0, progress_every > 0 ? report_progress : 0,
                              &trace, &best_names,
                              progress_every > 0 ? report_depth_done : 0);

  // On failure `word` is empty and the branch that came closest is in
  // `best_names`. The fields below are filled from whichever of the two the
  // run produced, so a caller always has moves to walk and states to walk
  // them against; `found` says which of the two it is looking at.
  const std::vector<std::string>& steps = ok ? word : best_names;

  const char* nm[3] = {"found", "no_solution", "exhausted"};
  const std::vector<std::string> full =
    expand_generator_words(steps, steps.size());
  CharacterVector out(full.size());
  for (size_t i = 0; i < full.size(); i++) out[i] = full[i];

  // The states the search passed through, one row per generator applied.
  //
  // These are rows against `generators`, NOT against `path`: a generator such
  // as "R2" expands to two entries in `path`, so the two vectors have
  // different lengths and cannot be indexed together. Pairing a state with the
  // wrong move is the mistake this pair of fields exists to prevent.
  //
  // The representation is the search's own -- piece permutation and
  // orientation. It is not converted back to stickers because from_stickers4()
  // is deliberately not invertible: centres are numbered by colour, so the
  // four centres of a face are one value here and the distinction does not
  // survive. Measures that read stickers have to replay `path` in R instead.
  const size_t n_tr = trace.size();
  const size_t width = n_tr ? trace[0].perm.size() : 0;
  IntegerMatrix perm_out((int)n_tr, (int)width);
  IntegerMatrix ori_out((int)n_tr, (int)width);
  for (size_t r = 0; r < n_tr; r++) {
    for (size_t c = 0; c < width; c++) {
      perm_out((int)r, (int)c) = (int)trace[r].perm[c];
      ori_out((int)r, (int)c) = (int)trace[r].ori[c];
    }
  }
  CharacterVector gens(steps.size());
  for (size_t i = 0; i < steps.size(); i++) gens[i] = steps[i];

  // The prune counters go back with the result because the node count on its
  // own cannot distinguish a heuristic that got weaker from one that never
  // fires. `cut_ratio` is the number to read: zero means the table was
  // consulted at every node and pruned at none of them.
  const kociemba::PruneStats& ps = S.prune_stats[2];
  return List::create(
    _["found"] = ok,
    _["path"] = out,
    _["generators"] = gens,
    _["states_perm"] = perm_out,
    _["states_ori"] = ori_out,
    _["outcome"] = std::string(nm[(int)S.outcome[2]]),
    _["best_bound"] = S.best_bound[2],
    _["exact_centres"] = use_exact_centres,
    _["centre_states"] = (double)S.centres3.dist.size(),
    _["centre_depth"] = (int)S.centres3.max_depth,
    _["nodes"] = (double)S.nodes[2],
    _["prune_lookups"] = (double)ps.lookups,
    _["prune_cuts"] = (double)ps.cuts,
    _["prune_class_cuts"] = (double)ps.class_cuts,
    _["cut_ratio"] = ps.lookups ? (double)ps.cuts / (double)ps.lookups : 0.0,
    _["mean_bound"] = ps.mean_bound,
    _["secs"] = S.secs[2],
    // One row per level of iterative deepening. `secs` above is the sum of its
    // fill and search columns, and this is where that sum came from.
    _["depths"] = depth_stats_df(S.depth_stats[2]));
}

// A phase's generators, and what each one does to a solved cube.
//
// The names are only half the story. build_spec4() parses each generator word
// through cube_search::cube_n(4) and turns it into a piece permutation with
// move_as_pieces4(); a phase then searches with those permutations, never with
// the names. So a phase can hold exactly the right list of names and still be
// turning something else -- the names would match twips's while the geometry
// underneath did not.
//
// This returns, for each generator, its name and the piece permutation the
// phase will actually search with -- corners, wings and centres, as the phase
// stores them.
//
// The check it enables: take the same generator word, apply it to a solved
// cube with cube_moves(4) in R, read the resulting cube back through
// from_stickers4(), and compare the permutations. They must agree. If they do
// not, the phase's alphabet and the package's have drifted apart, and every
// search the phase has ever run was over the wrong moves.
// [[Rcpp::export]]
List cube_phase_generators_cpp(int phase) {
  if (phase < 1 || phase > 3) stop("phase must be 1, 2 or 3, got %d", phase);
  kociemba4::Solver4& S = kociemba4::solver4();
  S.init();

  const kociemba::PuzzleSpec* sp[3] = {&S.spec1, &S.spec2, &S.spec3};
  const kociemba::PuzzleSpec& spec = *sp[phase - 1];

  // The solved cube, as the phases read it.
  std::vector<int> solved(96);
  for (int i = 0; i < 96; i++) solved[i] = i + 1;
  const kociemba::PieceState id = kociemba4::from_stickers4(solved);
  const std::vector<uint8_t> omod = spec.ori_mod();

  const int n = spec.n_moves();
  CharacterVector names(n);
  List perms(n), oris(n);
  IntegerVector axis(n), layer(n);

  for (int m = 0; m < n; m++) {
    names[m] = spec.move_names[m];
    axis[m] = spec.move_axis[m];
    layer[m] = spec.move_layer[m];

    kociemba::PieceState next;
    kociemba::apply_move(id, spec.moves[m], omod, next);
    perms[m] = IntegerVector(next.perm.begin(), next.perm.end());
    oris[m] = IntegerVector(next.ori.begin(), next.ori.end());
  }

  return List::create(
    _["names"] = names,
    _["perm"] = perms,
    _["ori"] = oris,
    _["axis"] = axis,
    _["layer"] = layer);
}

// A 96-sticker cube read through the phases' own from_stickers4(), returned as
// the piece permutation it becomes. This is the other half of the generator
// check: it lets R turn a cube it built with cube_moves(4) into the
// representation the phases search in, so the two can be compared directly.
// [[Rcpp::export]]
List cube_to_pieces4_cpp(IntegerVector state) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 96) stop("a 4x4x4 state has 96 stickers, got %d", (int)s.size());
  const kociemba::PieceState st = kociemba4::from_stickers4(s);
  return List::create(
    _["perm"] = IntegerVector(st.perm.begin(), st.perm.end()),
    _["ori"] = IntegerVector(st.ori.begin(), st.ori.end()));
}

// Phase 3's coordinate, broken into the three things it is made of, plus what
// the prune table thinks of the state.
//
// The coordinate is centres, canonicalised wings and one parity bit. A state
// phase 3 cannot finish differs from one it can in at least one of them, and
// which one it is decides where to look next: a state that is genuinely far by
// the coordinate is a question for phases 1 and 2, while a state the table
// misjudges is a question for the table.
//
// `centre_mismatch` and `wing_mismatch` are counted against the NEAREST goal
// -- phase 3 has 24, the solved cube in each orientation, and being far from
// one of them means nothing. `prune_bound` is what the table says the distance
// is: if that is small while the search cannot finish, the table is
// underestimating and the search has no gradient to follow.
// [[Rcpp::export]]
List cube_phase3_coord_cpp(IntegerVector state) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 96) stop("a 4x4x4 state has 96 stickers, got %d", (int)s.size());

  kociemba4::Solver4& S = kociemba4::solver4();
  S.init();

  const kociemba::PieceState cur = kociemba4::from_stickers4(s);
  kociemba::PieceState d, g;
  S.d3.derive(cur, d);

  int best_centre = 1 << 30, best_wing = 1 << 30, best_total = 1 << 30;
  int best_at = -1;
  bool exact = false;
  for (size_t i = 0; i < S.goals3.size(); i++) {
    S.d3.derive(S.goals3[i], g);
    int cm = 0, wm = 0;
    for (int k = 0; k < kociemba4::N_CENTRES; k++) {
      if (d.perm[kociemba4::Z_OFF + k] != g.perm[kociemba4::Z_OFF + k]) cm++;
    }
    for (int k = 0; k < kociemba4::N_WINGS; k++) {
      if (d.perm[kociemba4::W_OFF + k] != g.perm[kociemba4::W_OFF + k]) wm++;
    }
    if (d == g) exact = true;
    if (cm + wm < best_total) {
      best_total = cm + wm; best_centre = cm; best_wing = wm; best_at = (int)i;
    }
  }

  // The parity bit as the coordinate carries it, and the bit of the goal this
  // state is actually nearest to.
  //
  // Against goals3[0] instead, which is what this reported until 2026-08-14,
  // the two disagree on half the cubes for no reason: the twenty-four goals do
  // not share a bit. Measured by diag_goal_parity.R, twelve carry 0 and twelve
  // carry 1 -- Uw2, Rw2 and Fw2 flip it, so the orbit splits in half -- and
  // every state therefore has twelve goals it can reach. A disagreement with
  // goals3[0] alone means only that the nearer half was the other half, and
  // reading it as "this state can reach no goal" sent a whole line of
  // investigation after a phantom.
  //
  // `n_goals_matching_bit` is printed beside it so the same mistake cannot be
  // made from this output again: zero there is a state with no reachable goal,
  // and anything else is a state with goals to aim at.
  const int bit = (int)d.ori[kociemba4::C_OFF4];
  S.d3.derive(S.goals3[best_at >= 0 ? best_at : 0], g);
  const int goal_bit = (int)g.ori[kociemba4::C_OFF4];

  int n_bit_match = 0;
  for (size_t i = 0; i < S.goals3.size(); i++) {
    kociemba::PieceState gi;
    S.d3.derive(S.goals3[i], gi);
    if ((int)gi.ori[kociemba4::C_OFF4] == bit) n_bit_match++;
  }

  // What the prune table makes of this state.
  std::string key;
  kociemba::state_key(d, key);
  const uint8_t bound = S.p3.get(kociemba::state_hash(d));

  // And what the exact centre table makes of it, which is a different kind of
  // claim. The hash table above is filled to a depth -- seven, against searches
  // that run to fourteen -- so every state further away than that reads as the
  // same number, and states the search will find easy and states it will find
  // impossible are indistinguishable at the start. This one is a complete
  // breadth-first walk over a smaller coordinate: 58,800 arrangements, no
  // ceiling, so its distance is exact for every state phase 3 can reach.
  //
  // Built on demand: it costs about a second, and a caller asking for the
  // coordinate of one state should not pay for it unless it wants this field.
  S.ensure_centre_table();
  const int centre_dist = (int)S.centres3.get(d);

  return List::create(
    _["at_goal"] = exact,
    _["centre_mismatch"] = best_centre,
    _["wing_mismatch"] = best_wing,
    _["nearest_goal"] = best_at,
    _["parity_bit"] = bit,
    _["goal_parity_bit"] = goal_bit,
    _["n_goals_matching_bit"] = n_bit_match,
    _["prune_bound"] = (int)bound,
    _["table_built_depth"] = (int)S.p3.built_depth,
    // Exact, not a lower bound clipped by how far the hash table was filled.
    _["centre_dist"] = centre_dist,
    _["centre_max_depth"] = (int)S.centres3.max_depth);
}

// The two parities the reduction cares about, side by side.
//
// They are different quantities, and the phases use them for different things:
//
//   phase 2's filter  the parity of primary wings sitting in primary
//                     positions. Phase2SolutionFilter refuses a solution whose
//                     value is odd.
//   phase 3's goal    the parity of the corners plus the parity of the twelve
//                     dedges -- PLL parity, the thing a 3x3x3 cannot express.
//                     Phase3Deriver4 carries it as a bit of the coordinate, so
//                     a state with the wrong value is not the goal and no
//                     amount of searching will make it one.
//
// Phase 2 enforcing the first does not imply the second. If a cube can pass
// phase 2's filter and still reach phase 3 with the PLL bit set, phase 3 is
// being asked to reach a goal that is not reachable -- and it will spend its
// whole budget failing to. Comparing the two on a cube that fails is what
// tells that story.
// [[Rcpp::export]]
List cube_wing_parities_cpp(IntegerVector state) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 96) stop("a 4x4x4 state has 96 stickers, got %d", (int)s.size());
  const kociemba::PieceState st = kociemba4::from_stickers4(s);

  const std::vector<char>& primary = kociemba4::wing_position_is_primary();
  const std::vector<int>& dd = kociemba4::wing_to_dedge();

  int primary_in_primary = 0;
  for (int pos = 0; pos < kociemba4::N_WINGS; pos++) {
    const int piece = st.perm[kociemba4::W_OFF + pos] - kociemba4::W_OFF;
    if (primary[pos] && primary[piece]) primary_in_primary++;
  }

  std::vector<kociemba::Slot> dedges;
  for (int i = 0; i < kociemba4::N_WINGS; i++) {
    if (!primary[i]) continue;
    dedges.push_back((kociemba::Slot)dd[st.perm[kociemba4::W_OFF + i]
                                        - kociemba4::W_OFF]);
  }
  const int dedge_par = kociemba4::basic_parity(dedges, 0, (int)dedges.size());
  const int corner_par = kociemba4::corner_parity(st);

  return List::create(
    _["primary_in_primary"] = primary_in_primary & 1,
    _["corner_parity"] = corner_par,
    _["dedge_parity"] = dedge_par,
    _["pll_bit"] = (corner_par + dedge_par) & 1,
    _["wing_parity"] = kociemba4::basic_parity(st.perm, kociemba4::W_OFF,
                                               kociemba4::N_WINGS));
}

// How many distinct keys a phase's goals collapse to under its own coordinate.
//
// A phase's goal list holds the solved cube in several orientations, and the
// coordinate is meant to merge the ones it cannot tell apart -- that is what
// makes the search cheap. Merging more than intended is a bug, and the two
// look identical from outside: both show up only as a prune table that starts
// with fewer entries than the goal list has members.
//
// Returns the goal count and the distinct-key count, so the two can be
// compared directly rather than inferred from the table's depth-0 occupancy,
// which counts slots and not keys.
// [[Rcpp::export]]
List cube_phase_goal_keys_cpp(int phase) {
  if (phase < 1 || phase > 3) stop("phase must be 1, 2 or 3, got %d", phase);
  kociemba4::Solver4& S = kociemba4::solver4();
  S.init();

  const kociemba::Deriver* dv[3] = {&S.d1, &S.d2, &S.d3};
  const std::vector<kociemba::PieceState>* gl[3] =
    {&S.goals1, &S.goals2, &S.goals3};
  const std::vector<kociemba::PieceState>& goals = *gl[phase - 1];

  std::vector<std::string> keys;
  for (size_t i = 0; i < goals.size(); i++) {
    kociemba::PieceState d;
    dv[phase - 1]->derive(goals[i], d);
    std::string k;
    kociemba::state_key(d, k);
    keys.push_back(k);
  }
  std::vector<std::string> uniq(keys);
  std::sort(uniq.begin(), uniq.end());
  uniq.erase(std::unique(uniq.begin(), uniq.end()), uniq.end());

  return List::create(
    _["goals"] = (int)goals.size(),
    _["distinct_keys"] = (int)uniq.size());
}

// Whether a state is reduced: centres built and wings paired, so that the cube
// acts as a 3x3x3. This is what phase 3 aims at, exposed so a test can check
// the phases did what they claim.
// [[Rcpp::export]]
bool cube_is_reduced_cpp(IntegerVector state) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 96) stop("a 4x4x4 state has 96 stickers, got %d", (int)s.size());
  kociemba::PieceState p = kociemba4::from_stickers4(s);

  // Centres: all four of a face the same colour.
  const kociemba4::Cube4Layout& L = kociemba4::cube4_layout();
  for (int f = 0; f < 6; f++) {
    int colour = -1;
    for (int i = 0; i < kociemba4::N_CENTRES; i++) {
      if (L.centre_stickers[i][0] / 16 != f) continue;
      const int c = p.perm[kociemba4::Z_OFF + i] - kociemba4::Z_OFF;
      if (colour < 0) colour = c;
      else if (c != colour) return false;
    }
  }
  // Wings: the two slots of every dedge hold two wings that belong together --
  // not that they hold the *right* dedge. Where each pair sits is the 3x3x3's
  // business, and demanding it here calls a solved cube turned by one U
  // unreduced, which it plainly is not.
  const std::vector<int>& dd = kociemba4::wing_to_dedge();
  const std::vector<int>& partner = kociemba4::wing_to_partner();
  for (int i = 0; i < kociemba4::N_WINGS; i++) {
    const int j = partner[i];                 // the slot beside it in the dedge
    const int a = p.perm[kociemba4::W_OFF + i] - kociemba4::W_OFF;
    const int b = p.perm[kociemba4::W_OFF + j] - kociemba4::W_OFF;
    if (dd[a] != dd[b]) return false;
  }
  return true;
}

// ---- How big the canonical automaton gets --------------------------------
//
// The automaton collapses classes that commute with the same set, which is
// what stops its state count growing exponentially in the number of move
// classes. A big cube is where that matters: three axes times n layers, and
// every layer of an axis commutes with every other layer of that axis. This
// builds the automaton on the full quarter-turn alphabet of an n x n x n cube
// and reports the count both ways, so the claim can be measured.
//
// The moves are taken as permutations of the whole state -- one orbit of 6n^2
// stickers. The automaton only ever looks at how moves compose, so it does not
// care that these are stickers rather than pieces.
// [[Rcpp::export]]
List cube_fsm_size_cpp(int n) {
  const cube_search::CubeN& C = cube_search::cube_n(n);
  const int n_stickers = 6 * n * n;

  // Slots are uint16_t, so this models cubes far past any size worth
  // searching; the guard is against the type, not against the cube.
  if (n_stickers > 65535) {
    stop("cube_fsm_size: n = %d needs %d slots, past what a Slot holds",
         n, n_stickers);
  }

  kociemba::PuzzleSpec spec;
  kociemba::OrbitDef o;
  o.name = "STICKERS";
  o.n_pieces = n_stickers;
  o.n_orientations = 1;
  spec.orbits.push_back(o);
  spec.finish_layout();

  // C.perm[m] is already what an OrbitMove wants: 0-based, and slot i takes
  // whatever was in slot perm[m][i].
  for (int m = 0; m < (int)C.a.names.size(); m++) {
    kociemba::OrbitMove mv;
    mv.perm.resize(n_stickers);
    mv.ori.assign(n_stickers, 0);
    for (int i = 0; i < n_stickers; i++) mv.perm[i] = (uint8_t)C.perm[m][i];
    spec.moves.push_back(mv);
    spec.move_names.push_back(C.a.names[m]);
    spec.move_axis.push_back(C.axis_of(m));
    spec.move_layer.push_back(C.layer_of(m));
  }

  kociemba::CanonicalFSM with, without;
  with.init(spec, true);
  without.init(spec, false);

  return List::create(
    _["n"]              = n,
    _["n_moves"]        = (int)spec.moves.size(),
    _["n_classes"]      = with.n_classes(),
    _["states_with"]    = with.n_states(),
    _["states_without"] = without.n_states());
}

// The moves phases 1 and 2 contributed, whatever phase 3 then did.
//
// When phase 3 fails the whole path is discarded, which leaves no way to look
// at the cube phase 3 was handed -- and that is the state worth inspecting,
// since a phase that searches to its ceiling without finding anything may be
// looking for something that is not there. Wing parity is preserved by every
// one of phase 3's generators, so a cube whose parity does not match the goal
// cannot be reduced by it at any depth, and the way to find that out is to
// apply these moves and measure.
// `upto_phase` is 1 or 2: the moves that phase and everything before it
// contributed. Phases after it are given no depth, so they cost nothing and
// cannot fail the run before the prefix is recorded.
// [[Rcpp::export]]
CharacterVector cube_kociemba4_phase12_cpp(IntegerVector state,
                                           int upto_phase = 2,
                                           int max_depth1 = 10,
                                           int max_depth2 = 12,
                                           double node_budget = 5e7) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 96) {
    stop("a 4x4x4 state has 96 stickers, got %d", (int)s.size());
  }
  if (upto_phase < 1 || upto_phase > 2) {
    stop("upto_phase must be 1 or 2, got %d", upto_phase);
  }
  kociemba::PieceState start = kociemba4::from_stickers4(s);

  kociemba::SearchLimits l1, l2, l3;
  l1.max_depth = max_depth1;
  l2.max_depth = (upto_phase >= 2) ? max_depth2 : 0;
  l3.max_depth = 0;
  l1.node_budget = l2.node_budget = l3.node_budget = (long)node_budget;

  std::vector<std::string> word;
  // reduce() returns false when a phase does not finish, and it stops at that
  // phase. moves_after_phase2 is then whatever it was left at -- zero if phase
  // 2 never ran -- so the prefix has to be read against how far it got rather
  // than assumed. Ignoring the return value here handed back a short word that
  // the caller replayed as though it were the phase's output, which is a
  // handover to phase 3 that no phase produced.
  kociemba4::solver4().reduce(start, word, l1, l2, l3, 0);

  kociemba4::Solver4& S4 = kociemba4::solver4();
  const bool reached = (upto_phase == 1)
    ? (S4.outcome[0] == kociemba::SEARCH_FOUND)
    : (S4.outcome[0] == kociemba::SEARCH_FOUND &&
       S4.outcome[1] == kociemba::SEARCH_FOUND);
  if (!reached) return CharacterVector(0);

  const size_t upto = (upto_phase == 1)
    ? kociemba4::solver4().moves_after_phase1
    : kociemba4::solver4().moves_after_phase2;
  const std::vector<std::string> full = expand_generator_words(word, upto);

  CharacterVector out(full.size());
  for (size_t i = 0; i < full.size(); i++) out[i] = full[i];

  // What the two phases cost, carried as attributes rather than by wrapping
  // the result in a list. Every caller of this function treats the return value
  // as the moves themselves -- they concatenate it, take its length, replay it
  // -- and a list would break all of them to tell them something none of them
  // asked for. Attributes ride along and only the caller that looks pays.
  //
  // These are the seconds that used to disappear: run once per orientation
  // before the budget ladder starts, never attributed to any phase, and so
  // counted in whatever the benchmark had left over.
  out.attr("phase1_secs") = S4.secs[0];
  out.attr("phase2_secs") = (upto_phase >= 2) ? S4.secs[1] : 0.0;
  out.attr("phase1_nodes") = (double)S4.nodes[0];
  out.attr("phase2_nodes") = (upto_phase >= 2) ? (double)S4.nodes[1] : 0.0;
  out.attr("phase1_depths") = depth_stats_df(S4.depth_stats[0]);
  if (upto_phase >= 2) {
    out.attr("phase2_depths") = depth_stats_df(S4.depth_stats[1]);
  }
  return out;
}

// Phases 1 and 2, returning several phase-2 solutions instead of the first.
//
// The cascade's way of giving phase 3 another chance is to rotate the whole
// cube and start again, and measurement put a price on it: across four
// rotations of one cube, phases 1 and 2 returned words of 16, 15, 15 and 13
// moves and landed phase 3 in four unrelated positions -- carried back into one
// frame they were four distinct states, and a phase-3 solution found in one
// rotation solved in none of the others (12 transfers, 0 successes). So the
// rotations are not four views of one problem that could be canonicalised away;
// they are four problems, and three of the four are usually paid for in full.
//
// Different phase-2 solutions are the same second chance bought differently.
// Phase 2 costs a tenth of a second where phase 3 costs forty, so several
// starting points from one rotation are cheap where four rotations are not.
//
// Returns a list: `phase1` (the shared prefix) and `solutions` (a list of
// words, each to be applied after it). Applying phase1 then solutions[[i]]
// gives the i-th state phase 3 could be started from.
// [[Rcpp::export]]
List cube_kociemba4_phase2_solutions_cpp(IntegerVector state,
                                         int n_solutions = 4,
                                         int max_depth1 = 10,
                                         int max_depth2 = 12,
                                         double node_budget = 5e7) {
  std::vector<int> s = as<std::vector<int> >(state);
  if (s.size() != 96) {
    stop("a 4x4x4 state has 96 stickers, got %d", (int)s.size());
  }
  if (n_solutions < 1) stop("n_solutions must be at least 1, got %d", n_solutions);
  kociemba::PieceState start = kociemba4::from_stickers4(s);

  kociemba::SearchLimits l1, l2;
  l1.max_depth = max_depth1;
  l2.max_depth = max_depth2;
  l1.node_budget = l2.node_budget = (long)node_budget;

  std::vector<std::string> phase1;
  std::vector<std::vector<std::string> > words;
  const bool ok = kociemba4::solver4().collect_phase2(
      start, phase1, l1, l2, (size_t)n_solutions, words, 0);

  kociemba4::Solver4& S4 = kociemba4::solver4();
  if (!ok) {
    return List::create(
      _["phase1"] = CharacterVector(0),
      _["solutions"] = List(0),
      _["phase1_secs"] = S4.secs[0],
      _["phase2_secs"] = S4.secs[1],
      _["phase1_nodes"] = (double)S4.nodes[0],
      _["phase2_nodes"] = (double)S4.nodes[1]);
  }

  // Expanded the same way phase12_cpp expands its word, so a caller can replay
  // these with apply_path() exactly as it replays that one. A word left in the
  // solver's own generator alphabet would contain "R2" where R/cube_wide.R
  // expects two moves.
  const std::vector<std::string> p1full =
      expand_generator_words(phase1, phase1.size());
  CharacterVector p1out(p1full.size());
  for (size_t i = 0; i < p1full.size(); i++) p1out[i] = p1full[i];

  List sols(words.size());
  for (size_t i = 0; i < words.size(); i++) {
    const std::vector<std::string> full =
        expand_generator_words(words[i], words[i].size());
    CharacterVector w(full.size());
    for (size_t j = 0; j < full.size(); j++) w[j] = full[j];
    sols[i] = w;
  }

  return List::create(
    _["phase1"] = p1out,
    _["solutions"] = sols,
    _["phase1_secs"] = S4.secs[0],
    _["phase2_secs"] = S4.secs[1],
    _["phase1_nodes"] = (double)S4.nodes[0],
    _["phase2_nodes"] = (double)S4.nodes[1]);
}

// The wing geometry the 4x4x4 phases run on, exposed so a test can hold it
// against twips's tables. All three are measured here rather than tabulated,
// which is exactly why they are worth checking against someone else's
// independently written constants.
// [[Rcpp::export]]
List cube_wing_geometry_cpp() {
  const std::vector<int>& speffz = kociemba4::speffz_to_wing_slot();
  const std::vector<int>& partner = kociemba4::wing_to_partner();
  const std::vector<char>& primary = kociemba4::wing_position_is_primary();
  const std::vector<int>& dd = kociemba4::wing_to_dedge();

  // The primary half of each wing's dedge -- twips's
  // WING_TO_PRIMARY_WING_IN_DEDGE, built here from the two measured pieces.
  IntegerVector pid(kociemba4::N_WINGS);
  for (int i = 0; i < kociemba4::N_WINGS; i++) {
    pid[i] = primary[i] ? i : partner[i];
  }

  return List::create(
    _["speffz_to_slot"] = wrap(speffz),
    _["partner"]        = wrap(partner),
    _["primary"]        = LogicalVector(primary.begin(), primary.end()),
    _["dedge"]          = wrap(dd),
    _["primary_in_dedge"] = pid);
}
