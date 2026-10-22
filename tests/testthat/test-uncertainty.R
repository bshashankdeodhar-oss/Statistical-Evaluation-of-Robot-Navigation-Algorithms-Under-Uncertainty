# =============================================================================
# tests/testthat/test-uncertainty.R
# =============================================================================
library(testthat)
source("../../R/grid_world.R")
source("../../R/uncertainty.R")

test_that("generate_hazard_map returns matrix in [0,1]", {
  h <- generate_hazard_map(10, 10, n_hazard_zones=3, seed=42)
  expect_true(is.matrix(h))
  expect_equal(dim(h), c(10,10))
  expect_true(all(h >= 0)); expect_true(all(h <= 1))
})

test_that("generate_disruption_map reproducible with seed", {
  d1 <- generate_disruption_map(10, 10, base_prob=0.1, seed=7)
  d2 <- generate_disruption_map(10, 10, base_prob=0.1, seed=7)
  expect_identical(d1, d2)
})

test_that("apply_sensor_error preserves dimensions", {
  g <- create_grid(8,8,seed=1)$grid
  pg <- apply_sensor_error(g, error_prob=0.05, seed=10)
  expect_equal(dim(pg), dim(g))
})

test_that("apply_edge_cost_uncertainty: zero uncertainty returns nominal cost", {
  expect_equal(apply_edge_cost_uncertainty(2.0, uncertainty=0), 2.0)
})

test_that("apply_edge_cost_uncertainty: positive result", {
  set.seed(1)
  c <- apply_edge_cost_uncertainty(1.0, uncertainty=0.3)
  expect_gt(c, 0)
})

test_that("apply_path_disruptions: no disruption at zero probability", {
  path <- list(c(1,1), c(1,2), c(1,3))
  dmap <- matrix(0, 5, 5)
  res <- apply_path_disruptions(path, dmap, seed=1)
  expect_equal(length(res$blocked_edges), 0)
  expect_true(is.na(res$first_block))
})

test_that("realize_path_cost: no block returns full cost", {
  path <- list(c(1,1), c(1,2), c(1,3), c(1,4))
  r <- realize_path_cost(path, first_block=NA, uncertainty=0, seed=42)
  expect_false(r$blocked)
  expect_equal(r$steps_taken, 3L)
  expect_equal(r$realized_cost, 3.0, tolerance=1e-10)
})

test_that("create_uncertainty_config returns correct structure", {
  cfg <- create_uncertainty_config(uncertainty_level=0.2, disruption_prob=0.1)
  expect_equal(cfg$uncertainty_level, 0.2)
  expect_equal(cfg$disruption_prob, 0.1)
})
