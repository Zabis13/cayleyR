# cayleyR 0.2.5

## New features

* **`human_algorithm_to()`** (`R/human_algorithm_to.R`) — reaches an arbitrary target without routing through the identity state. `human_algorithm()` solves both endpoints to `1:n` and concatenates the first path with the inverse of the second, so the word comes out roughly twice as long as it needs to be. Here the problem is relabelled instead: the three operations permute *positions* and treat the values as inert labels, so renaming every value to its position in the target turns "reach the target" into "reach `1:n`", and the solver runs once. Measured over 20 random pairs at `n = 20, k = 4`: mean 183 operations against 344 for the old route.

* **`cycle_shortcut()`** (`R/cycle_shortcut.R`, `src/cycle_shortcut.cpp`) — shortens a path by cutting across cycles. A combo word applied round and round from a point on the path traces a loop; where that loop meets a state occurring later in the path, the stretch between the two meeting points can be replaced by the stretch of the loop. Complements `short_path_bfs()`, which only sees rejoin points within its BFS depth.
  - Points are processed one at a time and a cut is applied the moment it is found, so every later point searches the already-shortened path. Points are carried as states rather than positions, because a cut renumbers everything downstream of it; points swallowed by an earlier cut are skipped.
  - Combos are sampled and ranked per point (`n_samples`, `n_top`, `sort_by`, all six criteria of `find_best_random_combinations()`); `sort_by = NULL` skips ranking altogether, which is markedly cheaper since ranking has to unroll every candidate to score it.
  - Scoring and searching run under OpenMP, `n_threads` defaulting to two below the core count. Points themselves stay sequential by construction. Measured 6x on 12 cores.
  - The result is verified by applying it to the start state; on a mismatch the original path is returned.

* **`cayley_bfs_full()`** and **`cayley_graph_diameter()`** (`R/graph_metrics.R`, `src/graph_metrics.cpp`) — full BFS over the reachable component with per-state graph distance, and graph diameter by all-pairs or from-start, with `max_pairs` sampling for sizes where all-pairs is out of reach.

* **`openmp_threads()`** is now exported. It was already present but missing from NAMESPACE, so it was unavailable to anything outside the package.

## Improvements

* **The ring-size cap of 63 is gone** — `human_algorithm()` and `human_algorithm_to()` now run at `n = 1000` and beyond. The limit was never algorithmic: the finish table packed tile numbers six bits to a slot, so values had to stay under 64. The tiles keyed there are always the tail values `bs+1 .. n`, so what is stored is now the offset `v - bs`, which runs `1 .. TAIL` and always fits. Verified to `n = 1000` at `k = 4` and `n = 150` at `k = 6`, each by applying the returned path and comparing against the target.

## Tests

* `tests/testthat/test-human-algorithm.R` now covers both solvers. The two files were merged: the C++ core caches its finish table per `(n, k)`, and building that table is what the runtime consists of, so split across two files the same pairs were built twice over. Ring sizes are deliberately few for the same reason — phase 1 is insensitive to `n`, so sweeping sizes mostly re-pays the table cost.
* `k = 6` was dropped from the suite. Its table takes about 45 seconds to build, against well under a second for every other width, and it exercises no path the narrower widths miss. Runtime went from 57s across the two files to 7s.
* Added `tests/testthat/test-graph-metrics.R`.

## Examples

* `inst/examples/benchmark_human_algorithm_to.R` and `inst/examples/benchmark_cycle_shortcut.R`.
* Example scripts are now plain ASCII on disk. `Rscript` takes the locale of whatever shell starts it, and a literal multi-byte character in the source breaks the parser in a non-UTF-8 one — the script would run to completion and then die on its final print. Box-drawing frames are written as `\u` escapes and built at run time instead.

# cayleyR 0.2.4

## New features

* **`human_algorithm()`** — solver that follows the way a person solves TopSpin by hand, implemented in C++ (`src/human_algorithm.cpp`):
  - Phase 1 grows a sorted run one value at a time: the ring is manoeuvred until the new value sits exactly `k` positions after its predecessor, and a single reverse-prefix drops it into place. The run is tracked as a contiguous range of ring positions, and auxiliary flips only use windows lying wholly inside the unsorted arc, so the run is never disturbed.
  - Phase 2 finishes the last eight tiles with local cycle primitives that leave the rest of the ring untouched. The primitives are *derived by search* for each `k` rather than hard-coded — a word's effect depends on the ring it runs on, so fixed words do not carry over between sizes.
  - Conjugates of those primitives generate the alternating group on the tail, so the finish is a table lookup rather than a search. The table is built once per `(n, k)` and cached.
  - Cycle primitives are even permutations, so odd tail arrangements are out of their reach; those are handled by firing one flip across the block boundary and rebuilding the run, which changes the parity of the split.
  - Accepts an arbitrary target state (`human_algorithm(start, final, k)`), not just sorting to `1:n`.
  - Coverage: solves all tested states for even ring sizes with `k` from 3 to 6, including the classic `n = 20, k = 4` TopSpin. Odd `n` combined with odd `k` succeeds only partially — see TODO.

## Bug fixes

* Fixed `short_position()` returning `NULL` instead of `character(0)` when a path cancels out completely (`unlist()` on an empty list). The `NULL` then reached the C++ layer through `validate_and_simplify_path()` and crashed the session with "Not compatible with STRSXP". Triggered by any fully reducible path, e.g. `RRRRRRRRLLLLLLLL`.
* Exported `state_store_size()`, `state_store_perm_length()`, `state_store_unique_count()` and `state_store_indices_for_cycle()` — documented and used by tests, but missing from NAMESPACE.
* Exported `build_permutation_matrix()` and `compose_permutation_matrix()`, previously marked internal but referenced by the GPU tests.

## Tests

* Added `tests/testthat/test-human-algorithm.R` — sorting, arbitrary targets, already-sorted input, several ring sizes and flipper widths, and argument validation.

# cayleyR 0.2.3

## Breaking changes

* Disabled GPU for `store_analyze_combos()` — benchmarks showed GPU (Vulkan via ggmlR) is ~24x slower than CPU (C++) for this workload. ggmlR remains as optional dependency for other use cases.

## New features

* **`short_path_bfs.cpp`** — `short_path_bfs()` rewritten entirely in C++ with depth-limited BFS and multi-index lookup, replacing the previous R implementation for significantly better performance.

## Bug fixes

* Fixed non-ASCII characters in R code (replaced Russian text with English in diagnostic messages)
* Fixed documentation mismatches: `sort_by` parameter now properly documented in `find_best_random_combinations()` and `find_path_iterative()`

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
