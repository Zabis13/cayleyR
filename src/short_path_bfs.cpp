#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <string>
#include <algorithm>
#include "cayley_utils.h"
#include "perm_group.h"

using namespace Rcpp;

static std::vector<int> reconstruct_path(
    const std::unordered_map<std::string, std::pair<std::string, int>>& parent_map,
    const std::string& start_key,
    const std::string& target_key)
{
  std::vector<int> path;
  std::string cur = target_key;
  while (cur != start_key) {
    auto it = parent_map.find(cur);
    if (it == parent_map.end()) break;
    path.push_back(it->second.second);
    cur = it->second.first;
  }
  std::reverse(path.begin(), path.end());
  return path;
}

// [[Rcpp::export]]
List short_path_bfs_cpp(IntegerVector start_state,
                        IntegerVector path,
                        SEXP group,
                        IntegerVector moves,
                        int depth) {

  XPtr<PermGroup> g(group);

  // Both the path being shortened and the alphabet it may be rewritten over
  // arrive as move indices; names appear only in the result, so the shortener
  // never has to know what the moves mean.
  std::vector<int> ops;
  for (int i = 0; i < moves.size(); i++) {
    int m = moves[i] - 1;
    if (m < 0 || m >= g->n_moves()) stop("move index %d out of range", moves[i]);
    ops.push_back(m);
  }
  if (ops.empty()) stop("moves must contain at least one operation");

  int n_ops = path.size();
  if (n_ops == 0) {
    return List::create(
      Named("path") = IntegerVector(0),
      Named("original_length") = 0,
      Named("new_length") = 0,
      Named("savings") = 0
    );
  }

  if (n_ops <= depth) {
    return List::create(
      Named("path") = path,
      Named("original_length") = n_ops,
      Named("new_length") = n_ops,
      Named("savings") = 0
    );
  }

  // 1. Replay path to get all intermediate states and build multi-index lookup
  //    state -> vector of all path indices where this state occurs
  std::unordered_map<std::string, std::vector<int>> path_index_map;
  std::vector<std::vector<int>> path_states;
  path_states.reserve(n_ops + 1);

  std::vector<int> current(start_state.begin(), start_state.end());
  path_states.push_back(current);
  path_index_map[state_to_key(current)].push_back(0);

  for (int i = 0; i < n_ops; i++) {
    int m = path[i] - 1;
    if (m < 0 || m >= g->n_moves()) stop("path move index %d out of range", path[i]);
    g->apply(current, m);
    path_states.push_back(current);
    path_index_map[state_to_key(current)].push_back(i + 1);
  }

  // 2. Greedy BFS hopping with depth-limited exploration
  std::vector<int> result_path;
  int cursor = 0;

  while (cursor < n_ops) {
    Rcpp::checkUserInterrupt();

    std::string start_key = state_to_key(path_states[cursor]);

    // BFS structures
    std::unordered_map<std::string, std::pair<std::string, int>> parent_map;
    std::unordered_map<std::string, std::vector<int>> state_map;
    state_map[start_key] = path_states[cursor];

    std::vector<std::string> frontier_keys = {start_key};

    int best_path_idx = cursor;
    std::string best_key = start_key;


    // BFS limited to `depth` levels
    for (int d = 0; d < depth && !frontier_keys.empty(); d++) {
      std::vector<std::string> new_frontier;

      for (const auto& pkey : frontier_keys) {
        const auto& pstate = state_map[pkey];

        for (size_t oi = 0; oi < ops.size(); oi++) {
          std::vector<int> child = pstate;
          g->apply(child, ops[oi]);
          std::string ckey = state_to_key(child);

          if (ckey == start_key || parent_map.count(ckey)) continue;

          parent_map[ckey] = {pkey, ops[oi]};
          state_map[ckey] = child;
          new_frontier.push_back(ckey);

          // Check if this state appears on the path ahead of cursor + bfs_steps
          auto pit = path_index_map.find(ckey);
          if (pit != path_index_map.end()) {
            // Find the maximum index that is strictly greater than cursor + (d+1)
            // (we reached this state in d+1 BFS steps, so it must save at least 1 step)
            const auto& indices = pit->second;
            // indices are sorted ascending (built in order)
            for (int idx = (int)indices.size() - 1; idx >= 0; idx--) {
              if (indices[idx] > cursor + (d + 1)) {
                if (indices[idx] > best_path_idx) {
                  best_path_idx = indices[idx];
                  best_key = ckey;

                }
                break; // found max for this state
              }
            }
          }
        }
      }

      frontier_keys = std::move(new_frontier);
    }

    if (best_path_idx == cursor) {
      // No shortcut found — follow original path one step
      result_path.push_back(path[cursor] - 1);
      cursor++;
    } else {
      // Reconstruct the BFS path from cursor's state to best_key
      std::vector<int> segment = reconstruct_path(parent_map, start_key, best_key);
      result_path.insert(result_path.end(), segment.begin(), segment.end());
      cursor = best_path_idx;
    }
  }

  IntegerVector result_cv(result_path.size());
  for (size_t i = 0; i < result_path.size(); i++) {
    result_cv[i] = result_path[i] + 1;   // back to R's 1-based indices
  }

  int savings = n_ops - (int)result_path.size();

  return List::create(
    Named("path") = result_cv,
    Named("original_length") = n_ops,
    Named("new_length") = (int)result_path.size(),
    Named("savings") = savings
  );
}
