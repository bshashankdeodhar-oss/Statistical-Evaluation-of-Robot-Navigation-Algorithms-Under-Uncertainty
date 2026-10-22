# =============================================================================
# tests/testthat/test-statistics.R
# =============================================================================
library(testthat)
library(dplyr)
source("../../R/grid_world.R")
source("../../R/algorithms.R")
source("../../R/uncertainty.R")
source("../../R/simulation.R")
source("../../R/metrics.R")
source("../../R/statistics.R")

# Build small synthetic result set for testing
make_test_results <- function(n=60) {
  set.seed(123)
  algs <- c("Dijkstra","A*","Risk-Aware")
  data.frame(
    trial_id    = 1:n,
    map_id      = rep(1:10, length.out=n),
    algorithm   = rep(algs, length.out=n),
    lambda_risk = rep(c(0.5,1.0,2.0), length.out=n),
    replanning  = FALSE,
    obs_density = rep(c(0.1,0.2,0.3), length.out=n),
    uncertainty_level = rep(c(0.05,0.2), length.out=n),
    disruption_prob   = 0.05,
    sensor_error      = 0.05,
    grid_width  = 20L,
    grid_height = 20L,
    planning_success  = rbinom(n, 1, 0.9) == 1,
    execution_success = rbinom(n, 1, 0.85) == 1,
    overall_success   = rbinom(n, 1, 0.80) == 1,
    nominal_cost      = rnorm(n, 25, 5),
    realized_cost     = rnorm(n, 28, 7),
    path_length       = sample(15:35, n, replace=TRUE),
    nodes_expanded    = sample(50:300, n, replace=TRUE),
    planning_time     = rexp(n, 200),
    n_replannings     = 0L,
    failure_reason    = "none",
    env_seed          = 1:n,
    condition_id      = 1L,
    stringsAsFactors  = FALSE
  )
}

test_that("descriptive_stats returns a data.frame", {
  df <- make_test_results()
  s <- descriptive_stats(df)
  expect_s3_class(s, "data.frame")
  expect_true("mean_plan_t" %in% names(s))
})

test_that("compute_summary_metrics returns expected columns", {
  df <- make_test_results()
  s <- compute_summary_metrics(df)
  expect_true("success_rate" %in% names(s))
  expect_true("mean_planning_time" %in% names(s))
})

test_that("H1 test runs without error on valid data", {
  df <- make_test_results(100)
  res <- test_h1_planning_efficiency(df)
  expect_type(res, "list")
  expect_true("p_value" %in% names(res) || "error" %in% names(res))
})

test_that("H3 test runs without error", {
  df <- make_test_results(100)
  res <- test_h3_risk_aversion(df)
  expect_type(res, "list")
})

test_that("H4 test runs without error", {
  df <- make_test_results(100)
  res <- test_h4_interaction(df)
  expect_type(res, "list")
})

test_that("bootstrap_success_ci returns valid CI bounds", {
  df <- make_test_results(60)
  ci <- bootstrap_success_ci(df, n_boot=100)
  expect_true(all(ci$boot_lower <= ci$boot_upper))
  expect_true(all(ci$boot_lower >= 0))
  expect_true(all(ci$boot_upper <= 1))
})

test_that("generate_results_summary returns non-empty string", {
  df <- make_test_results(60)
  h1 <- list(p_value=0.03, effect_size=0.3)
  h3 <- list(kruskal_cost=list(p.value=0.01, statistic=8.5), eta_squared=0.05)
  h4 <- list(interaction_p=0.2, partial_eta_sq=0.01)
  s <- generate_results_summary(df, h1, list(), h3, h4)
  expect_type(s, "character")
  expect_gt(nchar(s), 50)
})
