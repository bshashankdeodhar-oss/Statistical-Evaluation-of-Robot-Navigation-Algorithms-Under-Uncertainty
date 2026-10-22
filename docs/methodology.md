# Methodology

## 1. Simulation Environment

A two-dimensional grid of W x H cells. Each cell is either free (0) or an obstacle (1).
The robot moves between free neighbouring cells. Grid connectivity is verified by BFS
before any trial is run.

**Four-directional movement:** Unit cost (1.0) per step.
**Eight-directional movement:** Unit cost (1.0) for cardinal, sqrt(2) for diagonal.
Corner-cutting is prevented by checking both adjacent cardinal cells.

## 2. Mathematical Formulation

### Dijkstra
Minimises: sum of edge costs along path P from start s to goal g.
Complexity: O((V + E) log V) with binary heap.

### A*
Minimises: g(n) + h(n), where g(n) = cost from s, h(n) = heuristic to g.
Heuristic (4-dir): Manhattan distance. Heuristic (8-dir): Octile distance.
Both are admissible. Complexity: O(b^d) where b = branching factor, d = depth.

### Risk-Aware Planner
Minimises: travel_cost + lambda_risk * risk_penalty(edge)
where risk_penalty = (hazard_src + hazard_dst)/2 + (disrupt_src + disrupt_dst)/2
All effective costs are clamped to [0, Inf).
The planner uses Dijkstra internally for correctness (A* with risk-augmented
costs would require a tighter, harder-to-prove admissible heuristic).

## 3. Uncertainty Model

| Mechanism | Distribution | Parameters |
|-----------|-------------|------------|
| Edge-cost noise | Gamma(shape=1/CV^2, scale=nominal/shape) | CV = uncertainty_level |
| Edge disruption | Bernoulli(p) per step | p from per-cell disruption_map |
| Sensor error | Bernoulli(error_prob) per free cell | false obstacle rate |
| Map inversion | Bernoulli(false_free_prob) per obstacle | false clearance rate |
| Hazard map | Gaussian radial basis function | n_zones, radius, max_hazard |

**Why Gamma for cost noise?** Support is (0,Inf), matching non-negative costs.
Shape controls concentration; CV provides an intuitive lever. Mean = nominal cost.

## 4. Experimental Design

Factorial design crossing:
- 3 algorithms x 4 obstacle densities x 2-3 uncertainty levels x 2 disruption levels
- x 3 lambda values x 2 replanning modes
- n_maps independent grids per condition, n_trials per map

Algorithms run on IDENTICAL maps (same seed), controlling for map difficulty.
Separate random streams for: map generation, hazard/disruption, sensor noise,
execution noise.

## 5. Performance Metrics

- **Success rate**: proportion of trials where the robot reached the goal.
- **Nominal cost**: planned path cost (expected under no uncertainty).
- **Realized cost**: actual cost incurred during execution.
- **Nodes expanded**: proxy for computational work.
- **Planning time**: wall-clock seconds for path computation.
- **Detour ratio**: nominal_cost / Dijkstra_nominal on same map (cost overhead).

## 6. Statistical Hypotheses

H1: No runtime difference between Dijkstra and A* (Wilcoxon signed-rank, paired).
H2: Route success rates equal across algorithms (logistic regression + McNemar).
H3: lambda_risk does not affect realized cost (Kruskal-Wallis + logistic).
H4: Uncertainty does not interact with algorithm on cost (LM with interaction term).

Significance threshold: alpha = 0.05.
Multiple comparison adjustment: Bonferroni where applicable.
Effect sizes reported: rank-biserial r (H1), OR (H2), eta^2 (H3), partial eta^2 (H4).

## 7. Reproducibility

Each trial stores its env_seed. Rerunning with the same base_seed and configuration
reproduces all results. Trial-level CSV is saved automatically after each run.

## 8. Limitations

- Grid topology is a simplified proxy for real environments.
- The Gamma noise model may not match real sensor error distributions.
- Multiple trials on the same map introduce dependence; map-level resampling
  was used where feasible but cannot fully eliminate correlation.
- The Risk-Aware planner does not reoptimise the heuristic; conclusions about
  it are specific to the implemented formulation.
- Simulated results do not prove safety or optimality in physical systems.