---
title: 'cayleyR: Solving Permutation Puzzles via Cycle Intersection in Cayley Graphs'
tags:
  - R
  - combinatorics
  - graph algorithms
  - permutation puzzles
  - Cayley graphs
  - GPU computing
  - group theory
authors:
  - name: Yuri Baramykov
    orcid: 0009-0000-7627-4217
    affiliation: 1
affiliations:
  - name: Independent Researcher
    index: 1
date: 31 March 2026
bibliography: paper.bib
---

# Summary

`cayleyR` is an R package for solving permutation puzzles by finding paths
in Cayley graphs of the symmetric group $S_n$. The package targets the
**TopSpin(n, k)** puzzle — $n$ tokens arranged in a circular track with a
reversible turnstile window of width $k$ — but the underlying algorithmic
ideas apply to any puzzle whose state space forms a Cayley graph.

Each arrangement of tokens corresponds to a permutation in $S_n$; the two
shift directions and the reversal define three generators $\{L, R, X_k\}$,
and solving the puzzle is equivalent to finding a path between two vertices
in the Cayley graph $\Gamma(S_n, \{L, R, X_k\})$.

The central contribution is the **Iterative Cycle Intersection (ICI)**
algorithm: a randomised bidirectional search that generates algebraic cycles
from both the start and target states, and detects their intersection via
$O(1)$-amortised hash-indexed lookup. When no direct intersection is found,
a distance-guided bridge selection step narrows the gap and the process
repeats. An optional hub-based transport layer uses sparse BFS trees to
further reduce the effective search distance before invoking ICI.

Performance-critical components are implemented in C++ via Rcpp, with
optional Vulkan GPU acceleration for batch state transformation and pairwise
Manhattan distance computation. The package is available on CRAN [@cayleyR].

# Statement of Need

Permutation puzzles such as TopSpin, the 15-puzzle, and Rubik's Cube are
canonical benchmarks for combinatorial search. The state space of
TopSpin(n, k) is a Cayley graph of $S_n$ with $n!$ vertices; for $n \geq 12$
exhaustive BFS becomes intractable, and existing solvers based on pattern
databases [@bortoluzzi] or IDA* scale poorly beyond $n = 9$.

`cayleyR` addresses this gap without precomputation or memory-intensive
lookup tables. It is the first open-source R package for Cayley-graph
pathfinding and provides researchers in combinatorial optimisation, group
theory, and puzzle-solving algorithm design with a scriptable, extensible
environment for experimenting with large-scale permutation search.

The package delivers:

- **Three pathfinding strategies**: exact bidirectional BFS (optimal,
  $n \lesssim 10$), ICI (randomised, scalable), and hub-based transport
  network (BFS pre-reduction + ICI).
- **C++ StateStore**: open-addressing hash table with $O(1)$ amortised
  insert/lookup, supporting the accumulate-and-intersect pattern of ICI.
- **OpenMP-parallelised** combination ranking and cycle evaluation.
- **Vulkan GPU acceleration** (via `ggmlR`) for batch permutation
  matrix multiplication and pairwise distance matrices.
- **Celestial coordinate embedding**: cumulative operation counts mapped
  to $S^2$ via stereographic projection, inspired by the celestial
  holography programme of @pasterski2021, providing a geometric
  visualisation of search trajectories and a connection to the holographic
  Cayley graph framework of @chervov2026.
- **Path post-processing**: algebraic simplification and depth-limited
  BFS hopping that reduce solution length by up to 50%.

Computational experiments demonstrate that ICI solves TopSpin(14, 4)
instances across all scramble distances (20–150 moves) in a mean of
1.1 seconds, and TopSpin(10–16, 4) instances in under 0.5 seconds, with
no visible growth trend — consistent with the algorithm materialising only
a small fraction of the $n!$-vertex state space.

# Mathematics

The **ICI algorithm** (Algorithm 1 in the paper) maintains two state
stores $S$ and $F$, seeded at $\sigma_s$ and $\sigma_f$. Each round:

1. Sample $N$ random operation sequences of length $\ell$ over
   $\{L, R, X_k\}$; retain the top $N_{\text{top}}$ by a ranking
   criterion (longest cycle, most unique states, etc.).
2. Unroll full cycles into $S$ and $F$, recording metadata for path
   reconstruction.
3. Check $S \cap F \neq \emptyset$ via hash lookup. If found, reconstruct
   the path by chaining bridge segments and return
   $P_s \cdot \text{Invert}(P_f)$.
4. If no intersection, select bridge states $\beta_s \in S$, $\beta_f \in F$
   closest to the opposite endpoint (asymmetric greedy convergence) and
   restart from the bridges.

The hub-based strategy reduces the effective distance from $d(\sigma_s,
\sigma_f)$ to $d(\eta_s, \eta_f)$ — the closest hub pair found by sparse
BFS trees — before invoking ICI, enabling single-round convergence in
typical TopSpin(20, 4) instances.

# Acknowledgements

The author thanks the CRAN team for package review and the developers of
Rcpp and ggmlR for the infrastructure on which `cayleyR` depends.

# References
