# =============================================================================
# tests/testthat/test-simulation.R
# =============================================================================
library(testthat)
source("../../R/grid_world.R")
source("../../R/algorithms.R")
source("../../R/uncertainty.R")
source("../../R/simulation.R")

make_simple_grid <- function() {
  g <- create_grid(width=10, height=10, obs_density=0.10, seed=42)
  g
}

test_that("run_single_trial returns expected columns", {
  g <- make_simple_grid()
  if (!g$reachable) skip("Map unreachable")
  r <- run_single_trial(g, algorithm="Dijkstra", env_seed=1, trial_id=1, map_id=1)
  expected_cols <- c("trial_id","map_id","algorithm","overall_success",
                     "nominal_cost","realized_cost","planning_time","nodes_expanded")
  expect_true(all(expected_cols %in% names(r)))
})

test_that("run_single_trial: Dijkstra succeeds on simple map", {
  g <- make_simple_grid()
  if (!g$reachable) skip("Map unreachable")
  r <- run_single_trial(g, algorithm="Dijkstra",
                        unc_config=create_uncertainty_config(uncertainty_level=0),
                        env_seed=1, trial_id=1, map_id=1)
  expect_true(r$planning_success)
})

test_that("run_single_trial: unreachable map returns failure row", {
  g <- create_grid(10,10,obs_density=0,seed=1)
  g$reachable <- FALSE
  r <- run_single_trial(g, algorithm="Dijkstra", env_seed=1, trial_id=1, map_id=1)
  expect_false(r$overall_success)
  expect_equal(r$failure_reason, "map_unreachable")
})

test_that("reproducibility: same seed gives same result", {
  g <- make_simple_grid()
  if (!g$reachable) skip("Map unreachable")
  unc <- create_uncertainty_config(uncertainty_level=0.1, disruption_prob=0.05)
  r1 <- run_single_trial(g, algorithm="Dijkstra", unc_config=unc, env_seed=42, trial_id=1, map_id=1)
  r2 <- run_single_trial(g, algorithm="Dijkstra", unc_config=unc, env_seed=42, trial_id=1, map_id=1)
  expect_equal(r1$nodes_expanded, r2$nodes_expanded)
  expect_equal(r1$nominal_cost,  r2$nominal_cost,  tolerance=1e-10)
  expect_equal(r1$realized_cost, r2$realized_cost, tolerance=1e-10)
  expect_equal(r1$path_length,   r2$path_length)
})

test_that("planning_time is non-negative", {
  g <- make_simple_grid()
  if (!g$reachable) skip("Map unreachable")
  r <- run_single_trial(g, algorithm="A*", env_seed=1, trial_id=1, map_id=1)
  expect_gte(r$planning_time, 0)
})
