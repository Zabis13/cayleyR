# cayleyR 0.2.9

* **`cube_solve_old_pochmann()`** and **`cube_solve_m2()`** — the two
  blindfolded methods, so the 3x3x3 now has four solvers. Both place one piece
  at a time by conjugation and never look at the cube after the start.

* Over 10 states, all four solving all 10: CFOP 89.2 moves, LBL 169.2, M2
  277.6, Old Pochmann 432.6.


# cayleyR 0.2.8

* **The whole `cube3_*` module is removed** — `cube_group(3)` and
  `cube_moves(3)` replace it, and the generated alphabet is the only one the
  package now has. `cube3_group()`, `cube3_moves`, `cube3_move_codes`,
  `cube3_apply()`, `cube3_identity()`, `cube3_is_solved()`, `cube3_cycle()`,
  `cube3_scramble()`, `cube3_print()`, `cube3_parse_moves()`,
  `cube3_move_names()` and `cube3_inverse_move()` are gone, along with the
  numeric move codes 1--18. Half turns went with them: the metric is
  quarter-turn throughout, so `"U2"` is no longer a move name and is rejected
  rather than read as `"U U"`.

* **Cubes of any size** — `cube_group(n)`, with moves generated from the
  geometry as a turn of one layer about one axis. A cube of side `n` has `6n^2`
  stickers and `6n` moves. The 3x3x3 slice turns `M`, `E`, `S` follow from the
  same rule as the faces rather than being added by hand; inner layers of
  larger cubes are written `"1x"`, `"2y"` and so on. Quarter-turn metric: `U2`
  is the word `"U U"`. Also `cube_moves()`, `cube_move_names()`,
  `cube_identity()`, `cube_layer_move()`.

* **`cube_is_colour_solved()`** — a cube is solved when every face carries one
  colour. Slice turns move the centres, so a cube turned bodily in space is
  solved without its stickers being back where they started.

* **`generate_state()`** takes a `group`, and then returns a state reachable in
  it. Passing `n = 54` alone used to give a permutation of 54 stickers that no
  cube can reach.

* `cube3_apply_seq()`, `cube3_compose()`, `cube3_order()` and
  `cube3_inverse_seq()` are **removed** — `group_apply()`, `group_compose()`,
  `group_order()` and `group_inverse_seq()` already did the same. The remaining
  `cube3_*` functions now compute in C++.


# cayleyR 0.2.7

* **The 3x3x3 Rubik's cube** (`R/cube3.R`) — a second puzzle alongside TopSpin,
  in the same terms: a state is an integer vector, a move permutes it. Stickers
  are numbered 1 to 54 face by face, the solved cube is `1:54`, the six centres
  never move. Pure R, no compiled code.
  - `cube3_moves` — the eighteen generators as explicit permutations.
    `cube3_apply()`, `cube3_apply_seq()`, `cube3_compose()` apply them;
    `cube3_scramble()`, `cube3_inverse_seq()`, `cube3_is_solved()`,
    `cube3_print()` round out the basics.
  - `cube3_cycle()` unrolls a word until the state returns to where it started
    — the cube counterpart of the cycle expansion the ICI solver is built on.
    `cube3_order()` gives just the length.
  - Moves take either spelling: names (`"R U R' U'"`) or codes 1--18
    (`c(4, 1, 5, 2)`), mixed freely. Codes are returned by default;
    `as_names = TRUE` and `cube3_move_names()` give the readable form.

* `Description` now covers both puzzles rather than TopSpin alone.

# cayleyR 0.2.6

* **`landmark_states()`** — 25 rule-defined permutations of `1:n` as fixed probe points for graphs too large to enumerate; rotation-equivalent constructions are detected and separated automatically.
* **`convex_hull_3d()`, `enclosing_hull_3d()`** — the solid a point cloud bounds, with area and volume; the second passes through every point, not just the convex ones. New vignette `landmark-geometry` and two example scripts.

# cayleyR 0.2.5

* **`human_algorithm_to()`** — reaches an arbitrary target by relabelling values to their target positions and solving once, instead of routing through `1:n` twice. About half the word length of the old route.
* **`cycle_shortcut()`** — shortens a path by cutting across cycles: a combo looped from a point on the path can rejoin a later state, replacing the stretch between them. Complements `short_path_bfs()`. OpenMP-parallel; result verified against the start state.
* **`cayley_bfs_full()`** and **`cayley_graph_diameter()`** — full BFS over the reachable component with per-state distance, and graph diameter (all-pairs or from-start, with `max_pairs` sampling).
* **`openmp_threads()`** is now exported.

* **The ring-size cap of 63 is gone** — `human_algorithm()` and `human_algorithm_to()` now run at `n = 1000` and beyond, by storing tail offsets instead of raw tile numbers in the finish table.

# cayleyR 0.2.4

* **`human_algorithm()`** — solver that follows the way a person solves TopSpin by hand (C++, `src/human_algorithm.cpp`). Phase 1 grows a sorted run one value at a time; phase 2 finishes the tail with cycle primitives derived by search per `k` and cached as a lookup table. Accepts an arbitrary target, not just sorting to `1:n`. Solves even ring sizes with `k` from 3 to 6, including the classic `n = 20, k = 4` TopSpin.

# cayleyR 0.2.3

* Disabled GPU for `store_analyze_combos()` — benchmarks showed GPU (Vulkan via ggmlR) is ~24x slower than CPU (C++) for this workload. ggmlR remains as optional dependency for other use cases.

* **`short_path_bfs.cpp`** — `short_path_bfs()` rewritten entirely in C++ with depth-limited BFS and multi-index lookup, replacing the previous R implementation for significantly better performance.

# cayleyR 0.2.2

* `find_path_iterative()` now uses C++ `StateStore` backend instead of `data.frame` lists — memory usage is linear instead of quadratic
* `analyze_top_combinations()` still works but the new `store_analyze_combos()` writes directly to C++ store without intermediate data.frames

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

* `CelestialCoords` struct extracted to shared header (`celestial_coords.h`) for reuse across C++ files
* `find_path_bfs()` now returns `bridge_states_start` and `bridge_states_final` in its result, including BFS hub states

# cayleyR 0.2.1

* Removed `arrow` dependency — all functions now return plain `data.frame` instead of Arrow Tables
* `data.table` moved from `Imports` to `Suggests` — package works without it, uses `data.table` for speed when available

* Significantly faster installation: removed heavy `arrow` dependency (~50+ MB)
* Lightweight by default: only `Rcpp` is required

# cayleyR 0.2.0

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

* C++ implementations of all core operations via Rcpp with OpenMP
* GPU functions fall back to CPU automatically when ggmlR/Vulkan is unavailable

# cayleyR 0.1.0

* Initial CRAN submission.
* Basic TopSpin operations: `shift_left()`, `shift_right()`, `reverse_prefix()`
* Cycle analysis functions: `get_reachable_states()`, `get_reachable_states_light()`
* Optimization tools: `find_best_random_combinations()`
