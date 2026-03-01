#include <Rcpp.h>
#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <string>
#include <algorithm>
#include "cayley_utils.h"

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

struct Candidate {
  std::vector<int> state;
  std::string parent_key;
  std::string child_key;
  std::string op;
  int degree; // look-ahead score
};

// [[Rcpp::export]]
DataFrame sparse_bfs_cpp(IntegerVector start_state,
                         int k,
                         int n_hubs = 7,
                         int n_random = 3,
                         int max_levels = 1000) {

  const std::vector<std::string> ops = {"L", "R", "X"};

  // Result vectors
  std::vector<std::string> res_parent;
  std::vector<std::string> res_child;
  std::vector<std::string> res_op;
  std::vector<int> res_level;

  // Init
  std::vector<int> start(start_state.begin(), start_state.end());
  std::string start_key = state_to_key(start);

  std::unordered_set<std::string> visited;
  visited.insert(start_key);

  // current_level: key -> state
  std::unordered_map<std::string, std::vector<int>> current_level;
  current_level[start_key] = start;

  for (int level = 1; level <= max_levels; level++) {
    if (current_level.empty()) break;

    // Check for user interrupt periodically
    if (level % 100 == 0) Rcpp::checkUserInterrupt();

    // 1. Collect all unique candidates from current level
    std::unordered_map<std::string, Candidate> candidates;

    for (auto& kv : current_level) {
      const std::string& parent_key = kv.first;
      const std::vector<int>& parent_state = kv.second;

      for (const auto& op : ops) {
        std::vector<int> child = parent_state;
        apply_op_inplace(child, op, k);
        std::string child_key = state_to_key(child);

        if (visited.count(child_key)) continue;
        if (candidates.count(child_key)) continue;

        Candidate cand;
        cand.state = std::move(child);
        cand.parent_key = parent_key;
        cand.child_key = child_key;
        cand.op = op;
        cand.degree = 0;
        candidates[child_key] = std::move(cand);
      }
    }

    if (candidates.empty()) break;

    int total_select = n_hubs + n_random;

    // If few candidates, take all
    if ((int)candidates.size() <= total_select) {
      for (auto& kv : candidates) {
        Candidate& c = kv.second;
        res_parent.push_back(c.parent_key);
        res_child.push_back(c.child_key);
        res_op.push_back(c.op);
        res_level.push_back(level);
        visited.insert(c.child_key);
      }
      // Build next level
      current_level.clear();
      for (auto& kv : candidates) {
        current_level[kv.first] = std::move(kv.second.state);
      }
      continue;
    }

    // 2. Look-ahead: compute degree for each candidate (OpenMP parallel)
    // Convert to vector for indexed access
    std::vector<Candidate*> cand_ptrs;
    cand_ptrs.reserve(candidates.size());
    for (auto& kv : candidates) {
      cand_ptrs.push_back(&kv.second);
    }

    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic)
    #endif
    for (int ci = 0; ci < (int)cand_ptrs.size(); ci++) {
      Candidate& cand = *cand_ptrs[ci];
      int deg = 0;
      for (const auto& op : ops) {
        std::vector<int> grandchild = cand.state;
        apply_op_inplace(grandchild, op, k);
        std::string gc_key = state_to_key(grandchild);
        if (!visited.count(gc_key) && !candidates.count(gc_key)) {
          deg++;
        }
      }
      cand.degree = deg;
    }

    // 3. Sort candidates by degree DESC
    std::vector<Candidate*> sorted_cands;
    sorted_cands.reserve(candidates.size());
    for (auto& kv : candidates) {
      sorted_cands.push_back(&kv.second);
    }
    std::sort(sorted_cands.begin(), sorted_cands.end(),
              [](const Candidate* a, const Candidate* b) {
                return a->degree > b->degree;
              });

    // 4. Hybrid selection: top n_hubs + random n_random from remainder
    std::vector<Candidate*> selected;
    selected.reserve(total_select);

    int actual_hubs = std::min(n_hubs, (int)sorted_cands.size());
    for (int i = 0; i < actual_hubs; i++) {
      selected.push_back(sorted_cands[i]);
    }

    // Random from remainder
    int remainder_size = (int)sorted_cands.size() - actual_hubs;
    int actual_random = std::min(n_random, remainder_size);
    if (actual_random > 0) {
      // Fisher-Yates partial shuffle on remainder
      for (int i = 0; i < actual_random; i++) {
        int j = actual_hubs + i + (int)(R::runif(0.0, 1.0) * (remainder_size - i));
        if (j >= (int)sorted_cands.size()) j = (int)sorted_cands.size() - 1;
        std::swap(sorted_cands[actual_hubs + i], sorted_cands[j]);
        selected.push_back(sorted_cands[actual_hubs + i]);
      }
    }

    // 5. Record edges, update visited, build next level
    current_level.clear();
    for (auto* c : selected) {
      res_parent.push_back(c->parent_key);
      res_child.push_back(c->child_key);
      res_op.push_back(c->op);
      res_level.push_back(level);
      visited.insert(c->child_key);
      current_level[c->child_key] = std::move(c->state);
    }
  }

  return DataFrame::create(
    Named("parent_key") = wrap(res_parent),
    Named("child_key") = wrap(res_child),
    Named("operation") = wrap(res_op),
    Named("level") = wrap(res_level),
    Named("stringsAsFactors") = false
  );
}
