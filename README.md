# cayleyR

[![arXiv](https://img.shields.io/badge/arXiv-2607.13219-b31b1b.svg)](https://arxiv.org/abs/2607.13219)

**cayleyR** is an R package for analyzing Cayley graphs of permutation
groups for the TopSpin puzzle. The package implements algorithms for
cycle detection, state space exploration, bidirectional BFS pathfinding,
and finding optimal operation sequences in permutation groups generated
by shift and reverse operations.

The core method — **Iterative Cycle Intersection (ICI)**, implemented in
`find_path_iterative()` — grows cycles from both endpoints and looks for states
where the two sides meet, using the meeting points as bridges to narrow the
search. It is described in the accompanying paper:
[CayleyR: Solving the TopSpin puzzle via cycle intersection](https://arxiv.org/abs/2607.13219).

## Features

- **Basic permutation operations** (C++): cyclic left/right shifts, prefix reversal
- **Cycle analysis**: find cycles in Cayley graphs with detailed state information
- **Sequence optimization**: search for operation sequences with flexible sorting criteria
- **C++ StateStore**: compact hash-indexed state storage with O(1) incremental insert and O(min(N,M)) intersection
- **Bidirectional BFS**: find shortest paths between permutation states
- **Iterative solver (ICI)**: find paths between arbitrary states via Iterative Cycle Intersection — expand cycles from both ends and bridge them at shared states
- **BFS highway solver**: combine sparse BFS trees with iterative solver, auto-selects direct vs hub path
- **Human-style solver**: solve the way a person does — grow a sorted run, then finish the tail with derived cycle primitives
- **Relabelling solver**: reach an arbitrary target in one pass instead of routing through the sorted state, roughly halving the word
- **Cycle shortcut**: shorten a path by cutting across cycles that leave it and rejoin it further along (OpenMP)
- **Graph metrics**: full BFS over the reachable component, and graph diameter by all-pairs or sampling
- **Landmark states**: 25 permutations defined by rules rather than by chance, as fixed probe points in graphs too large to enumerate
- **Hull geometry**: convex hull of a point cloud, or a non-convex one passing through every point, with surface area and volume
- **Celestial coordinates**: map LRX operation counts to spherical coordinates
- **GPU acceleration** (optional): Vulkan-based batch computations via ggmlR
- **Fast processing**: lightweight version for batch testing of combinations

## Installation

```r
# From GitHub:
devtools::install_github("Zabis13/cayleyR")
```

## Quick start

```r
library(cayleyR)

# Basic operations
state <- 1:10
shift_left(state)              # cyclic left shift
shift_right(state)             # cyclic right shift
reverse_prefix(state, k = 4)  # reverse first 4 elements

# Apply a sequence of operations
apply_operations(state, c("L", "R", "X"), k = 4)

# Find cycle length for an operation sequence
get_reachable_states_light(state, c("1", "3"), k = 4)

# Find best random operation sequences (flexible sorting)
find_best_random_combinations(
  moves = c("1", "2", "3"),
  combo_length = 10,
  n_samples = 50,
  n_top = 5,
  start_state = 1:10,
  k = 4,
  sort_by = c("shortest", "most_unique")  # or "longest", "most_repeated", etc.
)

# Bidirectional BFS shortest path
bidirectional_bfs(start = 1:10, target = c(2:10, 1), k = 4)

# Iterative path finder
find_path_iterative(
  start_state = 1:10,
  final_state = c(3, 1, 2, 4, 5, 6, 7, 8, 9, 10),
  k = 4,
  sort_by = c("longest", "most_unique")
)

# BFS highway path finder (auto-selects direct vs hub route)
find_path_bfs(
  start_state = 1:10,
  final_state = c(3, 1, 2, 4, 5, 6, 7, 8, 9, 10),
  k = 4,
  bfs_levels = 200,
  sort_by = c("longest", "most_unique")
)

# Human-style solver: sort a scrambled ring the way a person would
state <- generate_state(20, k = 4, n_moves = 60)
res <- human_algorithm(state, k = 4)
res$found
res$path

# Or aim at an arbitrary target instead of 1:n
human_algorithm(state, final_state = c(2:20, 1), k = 4)
```

## Landmark states and the solid they span

A Cayley graph at `n = 20` has 20! vertices, so nothing about it can be measured
by enumeration. `landmark_states()` gives a set of fixed probe points instead:
25 permutations defined by rules that generalise across `n` — a full reversal, a
riffle shuffle, the doubling map `2j mod (n+1)`, and so on — so the same
construction can be compared between graph sizes.

```r
lm <- landmark_states(20)
lm[, c("name", "state_str")]
```

### Rotations are free

Two constructions can be different permutations and still be the same point of
the graph. An early version of `derangement` looked like this next to
`reverse_first`:

```
reverse_first:  10 9 8 7 6 5 4 3 2 1 | 11 12 ... 20
derangement:    11 12 ... 20 | 10 9 8 7 6 5 4 3 2 1
```

One has eight fixed points, the other none — but they are the same sequence cut
at a different place, and cutting the ring elsewhere is exactly what `L` does.
The two sat 7 moves apart at `n = 14`, against a mean of 133. `block_swap`,
`block_rotate3` and `shift_third` collided for the same reason.

Fixing the formulas by hand did not work: each correction collided with
something else. `landmark_states()` now detects collisions instead — it reduces
every state to its lexicographic minimum over all `n` rotations and breaks a tie
by transposing one adjacent pair in the later construction. Verified distinct
for every `n` from 6 to 20. The cost is that a state may differ by one
transposition from its stated formula.

### Distances between landmarks

Use `human_algorithm_to()`, not `human_algorithm()`. The latter reaches an
arbitrary target by solving *both* endpoints down to `1:n` and splicing the
first word with the inverse of the second, so every route detours through the
identity — measurably so: the path from `envelope` to `riffle` at `n = 20`
passes a state with 11 of 20 tiles already home.

```r
a <- lm$state[[match("full_reverse", lm$name)]]
b <- lm$state[[match("riffle", lm$name)]]

human_algorithm_to(a, b, k = 4)$length          # direct
human_algorithm(a, final_state = b, k = 4)$length  # via the identity
```

| pair (`n = 20`) | via identity | direct |
|------|-------------:|-------:|
| `two_cycles` — `full_reverse` | 441 | 141 |
| `envelope` — `riffle` | 329 | 146 |

Switching solvers moved the median of all 300 pairwise distances from 238.5 to
158, which in turn changed which pairs count as far apart.

### The solid

Twelve points in space bound a figure. `convex_hull_3d()` gives the smallest
convex body containing them, but convexity means some points end up strictly
inside and are not corners. `enclosing_hull_3d()` dents that hull inwards until
every point is a vertex.

```r
cube <- as.matrix(expand.grid(c(0, 1), c(0, 1), c(0, 1)))
convex_hull_3d(cube)[c("area", "volume")]      # 6 and 1

withcentre <- rbind(cube, c(0.5, 0.5, 0.5))
h <- enclosing_hull_3d(withcentre)
length(h$vertices)                             # 9 -- the centre is a corner too
c(h$area, h$volume)                            # 6.5607 and 0.9167
```

The dent costs volume and adds area, as it should. Both functions are
implemented in the package rather than taken from `geometry`, keeping the
dependency list at Rcpp alone; they are checked against a cube, a box, a
tetrahedron and a regular icosahedron and agree to six decimal places.

Measured on twelve landmarks chosen to be far apart:

| | `n = 20` convex | `n = 20` enclosing | `n = 50` convex | `n = 50` enclosing |
|---|---:|---:|---:|---:|
| vertices | 11 of 12 | 12 of 12 | 11 of 12 | 12 of 12 |
| faces | 18 | 20 | 18 | 20 |
| surface area | 5 950.57 | 5 956.42 | 530 374.12 | 531 911.41 |
| volume | 14 173.83 | 14 165.50 | 6 141 465.83 | 5 928 340.50 |

A caveat worth stating plainly: a landmark's position comes from the celestial
coordinates of the route that reached it, and that route is not guaranteed
shortest. These numbers describe a particular set of walks under a particular
solver — comparable across `n` or across solver changes, but not an invariant of
the graph.

### Running it

```r
source(system.file("examples", "demo_landmark_network.R", package = "cayleyR"))
source(system.file("examples", "demo_landmark_paths.R", package = "cayleyR"))
```

The first measures the distances, picks disjoint pairs that are far apart, and
draws either the paths or the solid; the second draws the star of paths from the
identity out to each landmark. Parameters sit in a block at the top of each:
`N`, the layout (`celestial`, `spectral`, `diffusion`), and how landmarks are
selected. See `vignette("landmark-geometry")` for the full pipeline.

## C++ StateStore

The core state storage uses a compact C++ backend for high-performance
state accumulation during iterative search:

```r
# Create store for permutations of length 10
store <- create_state_store(10L)

# Analyze combos directly into store (no intermediate data.frames)
store_analyze_combos(store, top_combos, start_state, k = 4, cycle_val = 1L)

# Hash-based intersection between two stores: O(min(N,M))
common_keys <- store_find_intersections(store_start, store_final)

# Manhattan distance best match on flat integer array
best_idx <- store_find_best_match(store, target_state)

# Convert to data.frame for debugging
df <- store_to_dataframe(store)
```

## GPU acceleration

Optional GPU support via the [ggmlR](https://github.com/Zabis13/ggmlR) package (Vulkan backend).
Install ggmlR separately; cayleyR works without it (CPU fallback).

```r
# Check GPU availability
cayley_gpu_available()
cayley_gpu_status()

# Manhattan distance (GPU vs CPU)
calculate_differences(start_state, states_df, use_gpu = TRUE)

# Batch apply operations to many states at once
apply_operations_batch_gpu(states_matrix, c("1", "3", "2"), k = 4)

# Pairwise distance matrix
manhattan_distance_matrix_gpu(states1, states2)
```

## Package functions

**Basic Operations (C++):**
- `shift_left()` / `shift_right()` — cyclic shifts with coordinate tracking
- `shift_left_simple()` / `shift_right_simple()` — simple cyclic shifts
- `reverse_prefix()` / `reverse_prefix_simple()` — reverse first k elements
- `apply_operations()` — apply sequence of operations

**Analysis:**
- `get_reachable_states()` — full cycle analysis with state tracking
- `get_reachable_states_light()` — lightweight cycle detection
- `find_best_random_combinations()` — find best random sequences with flexible `sort_by`
- `analyze_top_combinations()` — analyze top operation sequences

**StateStore (C++ backend):**
- `create_state_store()` — create compact hash-indexed store
- `store_add_from_df()` — add states from data.frame
- `store_analyze_combos()` — analyze combos directly into store (no data.frame)
- `store_find_intersections()` — O(min(N,M)) hash intersection
- `store_find_best_match()` — Manhattan distance best match
- `store_filter_middle()` — filter middle steps per combo
- `store_to_dataframe()` — convert to data.frame for debugging
- `store_reconstruct_path()` — C++ path reconstruction through cycles
- `store_lookup()` — hash lookup by state vector
- `store_get_state()` / `store_get_meta()` — retrieve state and metadata by index

**Pathfinding:**
- `bidirectional_bfs()` — bidirectional BFS shortest path
- `find_path_iterative()` — Iterative Cycle Intersection (ICI) path solver (StateStore backend)
- `find_path_bfs()` — find path via BFS highways + iterative connector (auto direct vs hub)
- `short_path_bfs()` — shorten existing path via greedy BFS hopping
- `human_algorithm()` — human-style solver: grow a sorted run, finish the tail with derived cycle primitives
- `sparse_bfs()` — sparse BFS with hybrid hub/random selection
- `reconstruct_bfs_path()` — reconstruct path from sparse BFS result
- `validate_and_simplify_path()` — validate and simplify operation path
- `invert_path()` — reverse an operation path

**GPU (optional, requires ggmlR):**
- `cayley_gpu_available()` / `cayley_gpu_init()` / `cayley_gpu_status()` / `cayley_gpu_free()` — GPU management
- `calculate_differences(..., use_gpu = TRUE)` — Manhattan distance on GPU
- `apply_operations_batch_gpu()` — batch operations via matrix multiplication
- `manhattan_distance_matrix_gpu()` — pairwise distance matrix

**Distance Metrics:**
- `manhattan_distance()` — Manhattan distance between states
- `breakpoint_distance()` — breakpoint distance between states
- `calculate_differences()` — compute distances for all states in a table

**Celestial Coordinates:**
- `convert_LRX_to_celestial()` — map LRX operation counts to spherical coordinates
- `calculate_angular_distance_z()` — angular distance between two coordinate sets
- `calculate_midpoint_z()` — midpoint between two coordinate sets
- `find_closest_to_coords()` — find state closest to target coordinates

**State Utilities:**
- `generate_state()` / `generate_unique_states_df()` — random state generation
- `select_unique()` — deduplicate states by V-columns
- `check_duplicates()` — find states present in two tables
- `find_combination_in_states()` — find a specific state in results
- `save_bridge_states()` — save bridge states to CSV
- `short_position()` — simplify operation path
- `convert_digits()` — parse operation strings

## Measured performance

Measured on a 12-core machine, `k = 4`, paths produced by `human_algorithm_to()`.
Every figure below comes from a run of the scripts in `inst/examples/`; each
shortened path is verified by applying it to the start state and comparing
against the target.

### Solving (`inst/examples/benchmark_human_algorithm_to.R`)

The ring-size cap of 63 is gone, so these sizes are reachable at all. Time grows
close to linearly in `n`; the move count does not, because it depends on how the
scramble happened to fall.

| n | sec | raw | short | found |
|---|---|---|---|---|
| 100 | 2.576 | 1340.3 | 1325.7 | 3/3 |
| 200 | 4.727 | 1614.3 | 1606.3 | 3/3 |
| 300 | 6.949 | 4144.3 | 4137.0 | 3/3 |
| 400 | 9.208 | 5290.7 | 5278.0 | 3/3 |
| 500 | 11.607 | 6589.0 | 6138.3 | 3/3 |
| 600 | 14.045 | 6445.3 | 6431.3 | 3/3 |
| 700 | 16.493 | 6140.3 | 6125.7 | 3/3 |
| 800 | 19.083 | 12717.0 | 12713.7 | 3/3 |
| 900 | 21.651 | 15540.3 | 15534.3 | 3/3 |
| 1000 | 24.266 | 8124.7 | 8108.0 | 3/3 |

3 runs per size, shortening via `short_path_bfs(depth = 4)`.

### Shortening (`inst/examples/benchmark_cycle_shortcut.R`)

`cycle_shortcut()` and `short_path_bfs()` hunt for the same thing by different
means, and on these paths neither finds much. The honest summary is that
`short_path_bfs()` is the better default up to a point, and that ranking the
combos costs several times what it returns:

| n | path | cycle_shortcut (ranked) | cycle_shortcut (no ranking) | short_path_bfs |
|---|---|---|---|---|
| 100 | 978 | 0 saved, 7.2s | 0 saved, 1.2s | 0 saved, 0.5s |
| 500 | 4248 | 6 saved, 131.8s | 6 saved, 21.6s | 10 saved, 19.7s (depth 6) |
| 2000 | 31005 | 0 saved, 339.0s | 0 saved, 57.7s | 4 saved, 624.9s (depth 6) |

Two things worth reading off this table. Ranking combos (`sort_by`) has to unroll
every candidate to score it and bought nothing here — `sort_by = NULL` returned
the same cuts five to six times faster. And the crossover is at `n = 2000`, where
`short_path_bfs()` becomes the slower of the two by an order of magnitude while
still being the only one that finds anything.

Cutting across cycles pays off on small rings and thins out as the state space
grows: a random cycle leaving the path rarely meets it again when there is that
much room to wander. See TODO for what this needs.

## Examples

Scripts in `inst/examples/`, runnable with `Rscript inst/examples/<name>.R`.
Each has a parameters block at the top; nothing needs editing to get a first
run out of it.

### Solvers and paths

| Script | What it does |
|---|---|
| `test_path.R` | Integration run of `find_path_iterative()` (ICI) on a scrambled ring |
| `test_bh_in_path.R` | `find_path_bfs()` on a fixed 20-tile target, writing path and stats to CSV |
| `test_bh_path_coords.R` | Same path, plus celestial coordinates accumulated at every step |
| `test_human_algorithm.R` | The human-style solver: grow a sorted run, finish the tail with cycle primitives |
| `test_human_navigation.R` | Solving driven by phase 1 of the human algorithm instead of Manhattan distance, one direction only |
| `test_sparse_bh.R` | Sparse BFS with look-ahead and hybrid hub selection |

### Benchmarks

| Script | What it measures |
|---|---|
| `benchmark_human_algorithm_to.R` | `human_algorithm_to()` sweeping ring size: time, raw and shortened move counts |
| `benchmark_cycle_shortcut.R` | `cycle_shortcut()` against `short_path_bfs()`, with and without combo ranking |
| `benchmark_human_nav.R` | Three ways of solving the same states — navigation plus search, versus the human algorithm |
| `benchmark_n_size.R` | Difficulty as the ring grows, `n_moves` held fixed |
| `benchmark_n_moves.R` | Difficulty as the scramble lengthens, `n` held fixed |
| `benchmark_sort_by.R` | Which `sort_by` strategy serves `find_path_iterative()` best |
| `benchmark_gpu_vs_cpu.R` | `store_analyze_combos()` on CPU/C++ against the GPU path |

### Graph structure

| Script | What it shows |
|---|---|
| `graph_diameter.R` | Graph diameter and the state pairs that realise it |
| `demo_graph_celestial.R` | The whole Cayley graph plotted in celestial coordinates |
| `demo_graph_spectral.R` | The same graph in spectral coordinates, which separate states celestial ones collapse |
| `demo_graph_spectral_nobfs.R` | Spectral layout without a full BFS, for rings too large to enumerate |
| `coord_diagnostics.R` | Whether a coordinate actually predicts graph distance, over arbitrary pairs rather than pairs involving the identity |
| `probe_human_table.R` | What drives the cost of building the finish table — `k`, as it turns out, not the ring size |

## Dependencies

- **Rcpp** — C++ implementations of core operations (required)
- **data.table** (optional) — faster `rbindlist` when available
- **ggmlR** (optional) — GPU acceleration via Vulkan

## License

MIT
