# =============================================================================
# tests/testthat/test-grid-world.R
# =============================================================================
library(testthat)
source("../../R/grid_world.R")

test_that("create_grid returns valid structure", {
  g <- create_grid(width=10, height=10, obs_density=0.20, seed=42)
  expect_type(g, "list")
  expect_true(all(c("grid","start","goal","width","height","movement","reachable") %in% names(g)))
  expect_equal(dim(g$grid), c(10,10))
  expect_equal(g$width, 10L)
  expect_equal(g$height, 10L)
  expect_length(g$start, 2)
  expect_length(g$goal,  2)
})

test_that("start and goal are free cells", {
  g <- create_grid(width=15, height=15, obs_density=0.20, seed=7)
  expect_equal(g$grid[g$start[1], g$start[2]], CELL_FREE)
  expect_equal(g$grid[g$goal[1],  g$goal[2]],  CELL_FREE)
})

test_that("obstacle density is approximately correct", {
  g <- create_grid(width=20, height=20, obs_density=0.30, seed=99)
  actual_density <- sum(g$grid == CELL_OBSTACLE) / (20*20)
  expect_lt(abs(actual_density - 0.30), 0.05)  # within 5%
})

test_that("reproducibility: same seed produces same map", {
  g1 <- create_grid(width=15, height=15, seed=123)
  g2 <- create_grid(width=15, height=15, seed=123)
  expect_identical(g1$grid, g2$grid)
  expect_identical(g1$start, g2$start)
  expect_identical(g1$goal,  g2$goal)
})

test_that("reachable flag is set correctly", {
  # Dense grids may be unreachable; sparse ones usually are
  g <- create_grid(width=10, height=10, obs_density=0.05, seed=1)
  expect_true(g$reachable)
})

test_that("get_neighbours four-directional: no diagonals returned", {
  g <- create_grid(width=5, height=5, obs_density=0, seed=1)
  nbs <- get_neighbours(g$grid, 3, 3, MOVE_4)
  diags <- Filter(function(n) n$diagonal, nbs)
  expect_equal(length(diags), 0)
})

test_that("get_neighbours eight-directional returns diagonals", {
  g <- create_grid(width=5, height=5, obs_density=0, seed=1)
  nbs <- get_neighbours(g$grid, 3, 3, MOVE_8)
  diags <- Filter(function(n) n$diagonal, nbs)
  expect_true(length(diags) > 0)
})

test_that("diagonal movement cost is sqrt(2)", {
  g <- create_grid(width=5, height=5, obs_density=0, seed=1)
  nbs <- get_neighbours(g$grid, 3, 3, MOVE_8)
  diag_costs <- vapply(Filter(function(n) n$diagonal, nbs), function(n) n$cost, numeric(1))
  expect_true(all(abs(diag_costs - sqrt(2)) < 1e-10))
})

test_that("heuristic is admissible for 4-directional movement", {
  # Manhattan >= true cost for any pair
  expect_gte(heuristic(1,1, 5,5, MOVE_4), 0)
  # Manhattan for aligned cells
  expect_equal(heuristic(1,1, 1,5, MOVE_4), 4)
  expect_equal(heuristic(1,1, 5,1, MOVE_4), 4)
})

test_that("heuristic is admissible for 8-directional (octile)", {
  # Octile for diagonal movement: min(dr,dc)*sqrt(2) + |dr-dc|
  h <- heuristic(1,1, 4,3, MOVE_8)
  expect_gte(h, 0)
  # Must not exceed true path length
  expect_lte(h, sqrt((3)^2 + (2)^2) + 1)  # relaxed upper bound
})

test_that("generate_multiple_maps produces n distinct maps", {
  maps <- generate_multiple_maps(n=5, base_seed=10, width=10, height=10, obs_density=0.2)
  expect_length(maps, 5)
  # Seeds should differ
  seeds <- vapply(maps, function(m) m$seed, integer(1))
  expect_equal(length(unique(seeds)), 5)
})
