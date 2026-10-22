# Beyond the Shortest Path
## Statistical Evaluation of Robot Navigation Algorithms Under Uncertainty

A complete R-based simulation and statistical research framework comparing three robot
navigation strategies under configurable environmental uncertainty.

---

## Research Question

> "How do environmental uncertainty, obstacle density, sensor errors, and unexpected route
> disruptions influence the efficiency, safety, and reliability of different robot navigation
> algorithms?"

---

## Algorithms Compared

| Algorithm | Description |
|-----------|-------------|
| **Dijkstra** | Classic shortest-path; benchmark baseline |
| **A\*** | Heuristic-guided shortest-path (faster, same optimality guarantee) |
| **Risk-Aware** | Balances travel cost and hazard exposure via `lambda_risk` parameter |

---

## Project Structure

```
Robot_Nav/
├── app.R                    # Shiny dashboard (entry point)
├── DESCRIPTION              # Package metadata
├── README.md                # This file
├── R/
│   ├── grid_world.R         # Grid generation, neighbours, heuristics
│   ├── algorithms.R         # Dijkstra, A*, Risk-Aware planners
│   ├── uncertainty.R        # Stochastic environment models
│   ├── simulation.R         # Single-trial engine
│   ├── experiments.R        # Monte Carlo experiment runner
│   ├── metrics.R            # Performance metric computation
│   ├── statistics.R         # Hypothesis tests (H1-H4)
│   ├── visualizations.R     # ggplot2 publication plots
│   └── report.R             # HTML report generator
├── tests/testthat/
│   ├── test-grid-world.R
│   ├── test-algorithms.R
│   ├── test-uncertainty.R
│   ├── test-simulation.R
│   └── test-statistics.R
├── data/                    # Experiment inputs
├── results/                 # CSV trial results (auto-saved)
├── reports/                 # Generated HTML reports
└── docs/
    ├── methodology.md
    └── user_guide.md
```

---

## Requirements

- R >= 4.2.0
- Packages: shiny, shinydashboard, shinyWidgets, shinycssloaders, ggplot2,
  dplyr, tidyr, igraph, testthat, DT, scales, viridis, gridExtra, broom,
  knitr, rmarkdown, htmltools, jsonlite, ggpubr, effectsize, base64enc

---

## Installation

### 1. Install R

Download from https://cran.r-project.org/bin/windows/base/

### 2. Install packages

```r
pkgs <- c("shiny","shinydashboard","shinyWidgets","shinycssloaders",
          "ggplot2","dplyr","tidyr","igraph","testthat","DT","scales",
          "viridis","gridExtra","broom","knitr","rmarkdown","htmltools",
          "jsonlite","ggpubr","effectsize","base64enc")
install.packages(pkgs, repos="https://cran.r-project.org")
```

### 3. Launch the Shiny dashboard

```r
setwd("c:/Shanki/VIT_CHENNAI/Projs/Robot_Nav")
shiny::runApp(".", port=3838)
```

---

## Running Simulations Without the Dashboard

```r
source("R/grid_world.R")
source("R/algorithms.R")
source("R/uncertainty.R")
source("R/simulation.R")
source("R/experiments.R")
source("R/metrics.R")
source("R/statistics.R")
source("R/visualizations.R")

# Generate a map
g <- create_grid(width=20, height=20, obs_density=0.25, seed=42)

# Run all three algorithms
d <- run_dijkstra(g)
a <- run_astar(g)
r <- run_risk_aware(g, lambda_risk=1.5)

# Visualise
plot_grid(g, paths=list(Dijkstra=d$path, "A*"=a$path, "Risk-Aware"=r$path))

# Monte Carlo experiment
results <- run_quick_experiment(n_trials=20, n_maps=3, base_seed=42)
summary_df <- compute_summary_metrics(results)
h1 <- test_h1_planning_efficiency(results)
```

---

## Running Automated Tests

```r
setwd("c:/Shanki/VIT_CHENNAI/Projs/Robot_Nav")
testthat::test_dir("tests/testthat")
```

---

## Reproducing Experiments

Every experiment is seeded. Record your `base_seed` and configuration;
re-running with the same parameters and seed reproduces identical results.

The CSV saved in `results/` contains `env_seed` for every trial.

---

## Statistical Hypotheses (Pre-Registered)

| # | Hypothesis | Test |
|---|-----------|------|
| H1 | No runtime difference between Dijkstra and A* | Wilcoxon signed-rank (paired) |
| H2 | Route success rates equal across algorithms | Logistic regression + McNemar |
| H3 | lambda_risk does not affect realized cost | Kruskal-Wallis + logistic |
| H4 | Uncertainty does not interact with algorithm on realized cost | LM with interaction |

---

## Notes and Limitations

- All conclusions are limited to the simulated conditions.
- Real-world robot performance may differ substantially.
- The Risk-Aware planner uses a simplified risk model; other formulations would differ.
- Path cost comparisons involving failed trials use cost imputation; results are
  sensitive to the chosen multiplier (adjustable in the dashboard).