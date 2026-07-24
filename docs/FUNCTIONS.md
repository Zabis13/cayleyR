# cayleyR: Function Reference

## Basic Permutation Operations (C++)

### `shift_left(state, coords = NULL)`
Cyclic left shift: moves first element to the end. Tracks celestial LRX coordinates.

### `shift_right(state, coords = NULL)`
Cyclic right shift: moves last element to the front. Tracks celestial LRX coordinates.

### `reverse_prefix(state, k, coords = NULL)`
Reverses the first k elements of the state vector (turnstile operation). Tracks celestial LRX coordinates.

### `shift_left_simple(state)`
Cyclic left shift without coordinate tracking.

### `shift_right_simple(state)`
Cyclic right shift without coordinate tracking.

### `reverse_prefix_simple(state, k)`
Reverses the first k elements without coordinate tracking.

### `apply_operations(state, operations, k, coords = NULL, compute_coords = TRUE)`
Applies a sequence of shift/reverse operations to a permutation state. Operations: "1"/"L" (left), "2"/"R" (right), "3"/"X" (reverse prefix). Returns new state and celestial coordinates.

### `openmp_threads()`
Returns the number of OpenMP threads available for parallel C++ computations.


## Cycle Analysis

### `get_reachable_states(start_state, allowed_positions, k, verbose = FALSE)`
Full cycle analysis: repeatedly applies an operation sequence until returning to the start state. Returns all visited states, celestial coordinates, operation history, and cycle statistics.

### `get_reachable_states_light(start_state, allowed_positions, k)`
Lightweight cycle detection (C++): returns only cycle length and unique state count without storing intermediate states. Fast for batch testing many operation sequences.

### `analyze_top_combinations(top_combos, start_state, k)`
Runs full cycle analysis for each combination in a data frame and collects all states with celestial coordinates into a single data frame.


## Pathfinding

### `bidirectional_bfs(n, state1, state2, max_level, moves, k)`
Finds the shortest path between two permutation states using bidirectional BFS. Expands from both start and goal simultaneously, meeting in the middle.

### `find_path_iterative(start_state, final_state, k, ...)`
Iterative path finder using cycle expansion. Generates random operation sequences, analyzes cycles, and searches for intersections between forward/backward state sets. Uses C++ StateStore for O(1) hash-indexed storage. Key parameters: `moves`, `combo_length`, `n_samples`, `n_top`, `max_iterations`, `potc`, `ptr`, `opd`, `reuse_combos`, `distance_method`, `sort_by`.

### `find_path_bfs(start_state, final_state, k, ...)`
Hybrid BFS + iterative pathfinding. Builds sparse BFS highway trees from both states, finds closest hub pair, then connects hubs via `find_path_iterative`. Assembles the full path: BFS(start->hub_s) + iterative(hub_s->hub_f) + inverted_BFS(final->hub_f). Key parameters: `bfs_levels`, `bfs_n_hubs`, `bfs_n_random`, `distance_method`.

### `sparse_bfs(start_state, k, n_hubs, n_random, max_levels)`
Sparse BFS with look-ahead and hybrid selection (C++). Keeps top-degree candidates (exploitation) and random candidates (exploration) per level. Returns a data.frame of edges (parent_key, child_key, operation, level).

### `reconstruct_bfs_path(bfs_result, target_key)`
Traces back from a target state to root using edges from `sparse_bfs()`. Returns operation sequence.

### `short_path_bfs(path, start_state, k, depth = 5)`
Shortens a path via depth-limited BFS hopping (C++). At each position, explores states within `depth` BFS steps and jumps to the farthest match found later in the original path.

### `human_algorithm(start_state, final_state = NULL, k = 4L, simplify = TRUE)`
Solves a state the way a person solves TopSpin by hand. Grows a sorted run one value at a time: manoeuvres the ring until `m` sits exactly `k` positions after `m-1`, then a single reverse-prefix drops `m` into place. Auxiliary flips stay wholly inside the unsorted arc, so the run is never disturbed. The last eight tiles — where the insertion move no longer fits — are finished with two local 3-cycles (`XLXLXRX` and `LXRXLXLX`, each with a compensating rotation) whose conjugates generate the alternating group on the tail. Odd tail arrangements are out of reach of 3-cycles; these are handled by firing one flip across the block boundary and rebuilding the run, which flips the parity of the split. `final_state` defaults to `1:n` (plain sorting). Returns a list with `found`, `path`, `length`. See `inst/doc/HUMAN_ALGORITHM.md` for the method written out for a human solver.

### `human_algorithm_cpp(start_state, k, max_ops, final_rotate)` *(internal)*
C++ backend for `human_algorithm()`.


## Combination Search

### `find_best_random_combinations(moves, combo_length, n_samples, n_top, start_state, k, sort_by)`
Generates random operation sequences and evaluates their cycle lengths (C++ with OpenMP). Returns top sequences sorted by criteria: "longest", "shortest", "most_unique", "least_unique", "most_repeated", "least_repeated".


## Path Utilities

### `invert_path(path)`
Reverses and inverts an operation sequence: "1" <-> "2", "3" stays unchanged.

### `short_position(allowed_positions, n)`
Simplifies an operation path: cancels inverse pairs, reduces shift chains modulo n, and simplifies blocks between reverses.

### `validate_and_simplify_path(path_candidate, start_state, final_state, k)`
Verifies a path transforms start_state into final_state, then simplifies it. Returns simplified path if still valid, otherwise original.


## State Processing & Distance

### `calculate_differences(start_state, reachable_states_start, method, use_gpu)`
Computes Manhattan distance from a reference state to every row in a state table. Supports GPU acceleration via ggmlR. Adds a `difference` column and sorts ascending.

### `select_unique(df)`
Removes duplicate rows based on state columns (V1..Vn).

### `check_duplicates(df1, df2)`
Finds states present in both tables by comparing V-columns. Used for intersection detection between forward/backward searches.

### `manhattan_distance(start_state, target_state)`
Sum of absolute differences between two permutation states.

### `breakpoint_distance(start_state, target_state)`
Counts positions where consecutive elements in the relative permutation differ by more than 1. Effective for TopSpin puzzles.

### `save_bridge_states(bridge_states, filename)`
Writes bridge states (state + cycle number) to a CSV file.

### `filter_middle_states(data, skip_first, skip_last)` *(internal)*
Removes first/last steps from each combo, keeping only middle states.

### `find_best_match_state(target_state, reachable_states, method, use_gpu)` *(internal)*
Finds the state with minimum distance to target. Tie-breaks by smallest step number.

### `select_new_state(target_all, opposite_state, method)` *(internal)*
Selects a new bridge state close to an opposite state. Randomly picks from the top 10 closest.


## State Indexing

### `add_state_keys(states_df, new_states, v_cols)` *(internal)*
Computes paste-based state keys for V-columns and adds a `state_key` column.

### `create_hash_index(states_df)` *(internal)*
Builds a hash environment mapping state keys to row indices for O(1) lookup.


## State Store (C++ Backend)

### `create_state_store(perm_length, init_capacity)`
Creates a C++ StateStore for compact, incremental, hash-indexed storage of permutation states.

### `state_store_size(xp)`
Number of states currently in the store.

### `state_store_unique_count(xp)`
Number of unique states in the store.

### `state_store_perm_length(xp)`
Permutation length the store was created with.

*These three share a single help page, `?state_store_query`.*

### `store_add_from_df(store, df, cycle_val)`
Converts a data.frame of states (from `analyze_top_combinations`) into the C++ store.

### `store_get_state(store, idx)`
Retrieves a single permutation state by 0-based index.

### `store_get_meta(store, idx)`
Retrieves metadata (step, combo, cycle, operation, coordinates) for a state by 0-based index.

### `store_find_intersections(store_a, store_b)`
Returns state keys present in both stores. O(min(N,M)) via hash lookup.

### `store_lookup(store, state)`
Returns 0-based indices of a state in the store.

### `store_find_best_match(store, target, candidate_indices)`
Finds the state with minimum Manhattan distance to target among candidates.

### `store_filter_middle(store, target_cycle, skip_first, skip_last)`
Returns indices of mid-cycle states, excluding first/last steps per combo.

### `store_set_opd(store, target_cycle, combos)`
Restricts cycle queries to only specified combo numbers (OPD filtering).

### `store_clear_opd(store)`
Clears all OPD combo filters.

### `store_combos_for_state(store, state, target_cycle)`
Returns combo numbers containing a given state in a given cycle.

### `store_to_dataframe(store)`
Converts entire store to a data.frame (for debugging/backward compatibility).

### `store_reconstruct_path(store, bridge_states, target_state, target_cycle, target_combo)`
Traces back through cycle chain using bridge states to reconstruct the operation sequence.

### `store_analyze_combos(store, top_combos, start_state, k, cycle_val)`
C++ cycle expansion: writes states and coordinates directly into the store, bypassing data.frame creation.

### `store_analyze_combos_gpu(store, top_combos, start_state, k, cycle_val)`
GPU-accelerated version of `store_analyze_combos`. Processes combinations step-by-step using GPU matrix multiplication. Falls back to CPU if GPU unavailable.


## Celestial Coordinates

### `convert_LRX_to_celestial(nL, nR, nX)`
Maps cumulative operation counts (Left, Right, Reverse) to spherical celestial coordinates via stereographic projection. Returns z, z_bar, theta, phi, omega_conformal.

### `calculate_angular_distance_z(result1, result2)`
Angular distance on the celestial sphere between two points given as coordinate lists with complex `z` component.

### `calculate_midpoint_z(coords1, coords2)`
Midpoint on the celestial sphere between two coordinate sets by averaging Cartesian unit-sphere positions and re-projecting.

### `find_closest_to_coords(reachable_states, target_coords, v_cols)`
Finds the state in a table whose celestial coordinates are closest to a target. Among top 10% closest, selects the one with smallest step number.


## GPU Acceleration (optional, requires ggmlR)

### `cayley_gpu_available()`
Checks whether ggmlR is installed and a Vulkan GPU device is present.

### `cayley_gpu_init(device = 0, force = FALSE)`
Lazily initializes the Vulkan backend. Safe to call multiple times.

### `cayley_gpu_status()`
Prints and returns GPU availability, device info, and backend status.

### `cayley_gpu_free()`
Frees GPU backend resources.

### `apply_operations_batch_gpu(states_matrix, operations, k)`
Applies a sequence of operations to multiple states simultaneously via GPU matrix multiplication.

### `manhattan_distance_matrix_gpu(states1, states2, batch_size = 256)`
Computes all pairwise Manhattan distances between two sets of states on GPU. Batches computation to avoid memory overflow.

### `calculate_differences_gpu(start_state, states_matrix)` *(internal)*
GPU Manhattan distance from one reference state to each row of a matrix.

### `build_permutation_matrix(op, n, k)` *(internal)*
Creates an n x n permutation matrix representing a single operation for GPU multiplication.

### `compose_permutation_matrix(operations, n, k)` *(internal)*
Multiplies individual permutation matrices into one combined matrix.


## Utilities

### `convert_digits(s)`
Parses a digit string ("123") or space-separated numbers ("10 11 12") into an integer vector.

### `generate_state(n, k, n_moves, moves, max_attempts)`
Generates a random permutation state reachable from 1:n by applying random operations. Guarantees the result is in the same connected component.

### `generate_unique_states_df(n, n_rows)`
Generates a data frame of unique random permutation states (columns V1..Vn).

### `has_data_table()` *(internal)*
Checks if the data.table package is available.


## Intersection Processing (internal)

### `reconstruct_full_path(reachable_states, start_state, target_state, target_cycle, target_combo, v_cols)`
Traces back through a chain of cycles to build the full operation path from root to target state.

### `process_start_intersection(intersection_state, reachable_states_final, bridge_states_final, final_index, v_cols)`
Handles intersection at the original start state. Reconstructs path through backward search tree.

### `process_final_intersection(intersection_state, reachable_states_start, bridge_states_start, start_index, v_cols)`
Handles intersection at the original final state. Reconstructs path through forward search tree.

### `process_intermediate_intersection(intersection_state, reachable_states_start, reachable_states_final, bridge_states_start, bridge_states_final, start_index, final_index, v_cols)`
Handles a general intersection found in both search trees. Combines forward and backward paths.


## State Search

### `find_combination_in_states(reachable_states_start, search_state)`
Searches for a specific permutation state in a reachable states table and returns the first matching row with metadata.
