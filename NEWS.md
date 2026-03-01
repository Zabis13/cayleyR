# cayleyR 0.2.2

## Breaking changes

* `find_path_iterative()` now uses C++ `StateStore` backend instead of `data.frame` lists — memory usage is linear instead of quadratic
* `analyze_top_combinations()` still works but the new `store_analyze_combos()` writes directly to C++ store without intermediate data.frames

## New features

* **C++ StateStore** — compact hash-indexed state storage (`src/state_store.h`, `src/state_store.cpp`):
  - Flat `vector<int>` for states, separate metadata vectors, `unordered_map` hash index
  - Capacity starts at 10,000 and doubles on demand (amortized O(1) insert)
  - Operations encoded as `int` (1/2/3) instead of strings for cache efficiency
  - Incremental hash: keys computed only for new states, never recomputed
  - `state_store_find_intersections()` — O(min(N,M)) set intersection via hash
  - `state_store_find_best_match()` — Manhattan distance on flat array
  - `state_store_filter_middle()` — skip first/last steps per combo
  - `state_store_reconstruct_path()` — full C++ path reconstruction
  - `state_store_to_dataframe()` — convert to data.frame for debugging

* **`store_analyze_combos()`** — C++ cycle expansion writing directly to StateStore, bypassing all R-level list/data.frame creation

* **`sort_by` parameter** for `find_best_random_combinations()`:
  - Flexible vector of sorting criteria: `"longest"`, `"shortest"`, `"most_unique"`, `"least_unique"`, `"most_repeated"`, `"least_repeated"`
  - Criteria combine freely: e.g. `c("shortest", "most_unique")`
  - New `repetition_ratio` column in output
  - `sort_by` parameter propagated to `find_path_iterative()`

* **Bridge state output** — `find_path_iterative()` and `find_path_bfs()` now print bridge state chains with 1-based numbering and labels

* **Direct vs hub distance check** in `find_path_bfs()` — compares Manhattan distance start<->final vs hub_s<->hub_f, skips BFS hubs when direct is closer

## Improvements

* `CelestialCoords` struct extracted to shared header (`celestial_coords.h`) for reuse across C++ files
* `find_path_bfs()` now returns `bridge_states_start` and `bridge_states_final` in its result, including BFS hub states

# cayleyR 0.2.1

## Breaking changes

* Removed `arrow` dependency — all functions now return plain `data.frame` instead of Arrow Tables
* `data.table` moved from `Imports` to `Suggests` — package works without it, uses `data.table` for speed when available

## Improvements

* Significantly faster installation: removed heavy `arrow` dependency (~50+ MB)
* Lightweight by default: only `Rcpp` is required

# cayleyR 0.2.0

## New features

* **GPU acceleration** via ggmlR Vulkan backend (optional):
  - `cayley_gpu_available()`, `cayley_gpu_init()`, `cayley_gpu_status()`, `cayley_gpu_free()` — GPU infrastructure with lazy initialization
  - `calculate_differences(..., use_gpu = TRUE)` — Manhattan distance on GPU (sub -> abs -> sum_rows)
  - `apply_operations_batch_gpu()` — batch permutation operations via matrix multiplication on GPU
  - `manhattan_distance_matrix_gpu()` — pairwise N*M Manhattan distance matrix using 3D tensors
* **Sparse BFS**: `sparse_bfs()`, `reconstruct_bfs_path()` — sparse BFS with hybrid hub/random selection
* **BFS pathfinding**: `find_path_bfs()` — find path via BFS highways + iterative connector
* **Path shortening**: `short_path_bfs()` — greedy BFS hopping to shorten existing paths
* **Bidirectional BFS** pathfinding: `bidirectional_bfs()` for shortest path between permutation states
* **Iterative path solver**: `find_path_iterative()` for finding paths via cycle expansion
* **Celestial coordinates**: `convert_LRX_to_celestial()`, `calculate_angular_distance_z()`, `calculate_midpoint_z()`, `find_closest_to_coords()` — map operation counts to spherical coordinates
* **Combination analysis**: `analyze_top_combinations()` for full cycle analysis of top operation sequences
* **State utilities**: `generate_state()`, `generate_unique_states_df()`, `select_unique()`, `check_duplicates()`, `save_bridge_states()`, `find_combination_in_states()`, `convert_digits()`
* **Path utilities**: `invert_path()`, `validate_and_simplify_path()`, `reconstruct_full_path()`
* **Distance metrics**: `manhattan_distance()`, `breakpoint_distance()`, `short_position()`
* Simple operation variants: `shift_left_simple()`, `shift_right_simple()`, `reverse_prefix_simple()`

## Improvements

* C++ implementations of all core operations via Rcpp with OpenMP
* GPU functions fall back to CPU automatically when ggmlR/Vulkan is unavailable

# cayleyR 0.1.0

* Initial CRAN submission.
* Basic TopSpin operations: `shift_left()`, `shift_right()`, `reverse_prefix()`
* Cycle analysis functions: `get_reachable_states()`, `get_reachable_states_light()`
* Optimization tools: `find_best_random_combinations()`
