# User Guide: Beyond the Shortest Path

## Statistical Evaluation of Robot Navigation Algorithms Under Uncertainty

---

## 1. Quick Start

### Starting the Interactive Dashboard

Launch R and run the Shiny dashboard from the project directory:

```r
setwd("c:/Shanki/VIT_CHENNAI/Projs/Robot_Nav")
shiny::runApp(".", port = 3838, launch.browser = TRUE)
```

The application will launch on `http://127.0.0.1:3838/`.

---

## 2. Dashboard Interface Overview

The dashboard consists of five core tabs:

### 2.1 Tab 1: Setup & Environment
- **Grid Configuration**: Width, Height (default: 20x20), and Movement type (`four` or `eight` directions).
- **Obstacle Density**: Slider from 0.05 to 0.40.
- **Uncertainty Controls**:
  - Edge Cost Uncertainty (CV of Gamma noise: 0.05 to 0.50).
  - Edge Disruption Probability (chance of unexpected obstruction: 0.00 to 0.30).
  - Sensor False Positive/Negative Rates.
  - Risk-Aware Weight ($\lambda_{\text{risk}}$): Tradeoff parameter between path length and hazard evasion.
  - Dynamic Replanning checkbox: Recomputes route dynamically if a planned edge becomes blocked.
- **"Generate Map & Plan"**: Generates a reproducible random world, executes all three planners (Dijkstra, A*, Risk-Aware), and displays their paths.

### 2.2 Tab 2: Live Planning
- **Visual Grid View**: Shows the generated environment, start position (green "S"), goal position (red "G"), and overlaid paths from Dijkstra (blue), A* (green), and Risk-Aware (amber).
- **Metric Cards**: Real-time comparisons for:
  - Planned (nominal) cost vs. Realized (execution) cost.
  - Nodes expanded (search effort).
  - Planning time (milliseconds).
  - Success/Failure status and reasons (e.g., path blocked, goal reached).

### 2.3 Tab 3: Performance & Visualizations
- Interactive ggplot2 visual distributions:
  - **Planning Runtime Distributions**: Kernel density estimates on logarithmic scale.
  - **Realized Travel Cost**: Histograms across obstacle densities.
  - **Route Success Rate**: Wilson 95% confidence intervals across conditions.
  - **Nodes Expanded**: Comparison of computational efficiency across planners.
  - **Interaction Effects**: Non-parallel response curves indicating algorithm-by-uncertainty interactions.
  - **Detour Ratios**: Cost overhead of Risk-Aware evasion relative to Dijkstra.

### 2.4 Tab 4: Statistical Testing (H1 - H4)
- Automated hypothesis evaluation:
  - **H1 (Planning Efficiency)**: Wilcoxon signed-rank test comparing Dijkstra and A* node expansions and runtimes with matched-pairs rank-biserial effect size $r$.
  - **H2 (Route Reliability)**: Logistic regression controlling for obstacle density and uncertainty, paired with McNemar tests for discordance.
  - **H3 (Risk-Aversion Effect)**: Kruskal-Wallis test evaluating whether increasing $\lambda_{\text{risk}}$ reduces travel failure rates.
  - **H4 (Algorithm $\times$ Uncertainty Interaction)**: Two-way factorial ANOVA / linear model evaluating whether uncertainty degrades some algorithms more severely than others (partial $\eta^2$).
- **Sensitivity Imputation Slider**: Tests robustness by varying failure penalties (1x to 5x max cost).

### 2.5 Tab 5: Export & Reproducibility
- Download raw trial-level simulation data as CSV.
- Download summarized condition statistics as CSV.
- Export full, publication-ready HTML Research Report with embedded base64 plots.
- View exact seed configuration and R session details for auditability.

---

## 3. Headless Scripting & Batch Experiments

You can also run experiments directly from R scripts without launching the web browser:

```r
source("R/grid_world.R")
source("R/algorithms.R")
source("R/uncertainty.R")
source("R/simulation.R")
source("R/experiments.R")
source("R/metrics.R")
source("R/statistics.R")
source("R/visualizations.R")
source("R/report.R")

# Run full Monte Carlo experiment
results <- run_quick_experiment(
  n_trials = 20L,
  n_maps   = 3L,
  base_seed = 42L,
  output_dir = "results"
)

# Compute summaries
summary_metrics <- compute_summary_metrics(results)

# Run hypothesis tests
h1_res <- test_h1_planning_efficiency(results)
h2_res <- test_h2_reliability(results)
h3_res <- test_h3_risk_aversion(results)
h4_res <- test_h4_interaction(results)

# Generate HTML report
generate_html_report(
  results_df     = results,
  summary_df     = summary_metrics,
  stats_results  = list(h1 = h1_res, h2 = h2_res, h3 = h3_res, h4 = h4_res),
  config         = list(base_seed = 42L, total_trials = nrow(results)),
  output_path    = "reports/research_report.html"
)
```

---

## 4. Running the Automated Test Suite

All 104 unit tests can be executed anytime using `testthat`:

```r
testthat::test_dir("tests/testthat")
```

Or via command line / PowerShell:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e ".libPaths(c('C:/Users/bshas/R/win-library/4.6', .libPaths())); testthat::test_dir('tests/testthat')"
```
