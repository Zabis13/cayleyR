#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <string>
#include <algorithm>
#include "cayley_utils.h"

using namespace Rcpp;

static std::vector<std::string> reconstruct_path(
    const std::unordered_map<std::string, std::pair<std::string, std::string>>& parent_map,
    const std::string& start_key,
    const std::string& target_key)
{
  std::vector<std::string> path;
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
                        CharacterVector path,
                        int k,
                        int n_hits) {

  int n_ops = path.size();
  if (n_ops == 0) {
    return List::create(
      Named("path") = CharacterVector(0),
      Named("original_length") = 0,
      Named("new_length") = 0,
      Named("savings") = 0
    );
  }

  // Path too short to compress — return as-is
  if (n_ops <= n_hits) {
    return List::create(
      Named("path") = path,
      Named("original_length") = n_ops,
      Named("new_length") = n_ops,
      Named("savings") = 0
    );
  }

  // 1. Replay path to get all intermediate states and build lookup
  std::unordered_map<std::string, int> path_index_map;
  std::vector<std::vector<int>> path_states;
  path_states.reserve(n_ops + 1);

  std::vector<int> current(start_state.begin(), start_state.end());
  path_states.push_back(current);
  path_index_map[state_to_key(current)] = 0;

  for (int i = 0; i < n_ops; i++) {
    std::string op = as<std::string>(path[i]);
    apply_op_inplace(current, op, k);
    path_states.push_back(current);
    std::string key = state_to_key(current);
    path_index_map[key] = i + 1;
  }

  // 2. Greedy BFS hopping
  const std::vector<std::string> ops = {"L", "R", "X"};
  std::vector<std::string> result_path;
  int cursor = 0;

  while (cursor < n_ops) {
    Rcpp::checkUserInterrupt();

    std::string start_key = state_to_key(path_states[cursor]);

    std::unordered_map<std::string, std::pair<std::string, std::string>> parent_map;
    std::unordered_map<std::string, std::vector<int>> state_map;
    std::vector<std::string> frontier_keys = {start_key};
    state_map[start_key] = path_states[cursor];

    int best_path_idx = cursor;
    std::string best_key = start_key;
    int hits = 0;

    while (!frontier_keys.empty() && hits < n_hits) {
      std::vector<std::string> new_frontier;

      for (const auto& pkey : frontier_keys) {
        if (hits >= n_hits) break;
        const auto& pstate = state_map[pkey];

        for (int oi = 0; oi < 3; oi++) {
          std::vector<int> child = pstate;
          apply_op_inplace(child, ops[oi], k);
          std::string ckey = state_to_key(child);

          if (ckey == start_key || parent_map.count(ckey)) continue;

          parent_map[ckey] = {pkey, ops[oi]};
          state_map[ckey] = child;
          new_frontier.push_back(ckey);

          auto pit = path_index_map.find(ckey);
          if (pit != path_index_map.end() && pit->second > cursor) {
            hits++;
            if (pit->second > best_path_idx) {
              best_path_idx = pit->second;
              best_key = ckey;
            }
          }
        }
      }

      frontier_keys = std::move(new_frontier);
    }

    if (best_path_idx == cursor) {
      result_path.push_back(as<std::string>(path[cursor]));
      cursor++;
    } else {
      std::vector<std::string> segment = reconstruct_path(parent_map, start_key, best_key);
      result_path.insert(result_path.end(), segment.begin(), segment.end());
      cursor = best_path_idx;
    }

    // Free BFS memory
    parent_map.clear();
    state_map.clear();
  }

  CharacterVector result_cv(result_path.size());
  for (size_t i = 0; i < result_path.size(); i++) {
    result_cv[i] = result_path[i];
  }

  int savings = n_ops - (int)result_path.size();

  return List::create(
    Named("path") = result_cv,
    Named("original_length") = n_ops,
    Named("new_length") = (int)result_path.size(),
    Named("savings") = savings
  );
}
