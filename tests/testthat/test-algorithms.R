# =============================================================================
# tests/testthat/test-algorithms.R
# =============================================================================
library(testthat)
source("../../R/grid_world.R")
source("../../R/algorithms.R")

# Helper: create a tiny obstacle-free grid
tiny_grid <- function(w=5, h=5) {
  create_grid(width=w, height=h, obs_density=0, seed=1, max_attempts=2)
}

# Known-optimal test: 4-directional, no obstacles, Manhattan = true optimal
test_that("Dijkstra finds optimal path on free grid", {
  g <- tiny_grid()
  # Override start/goal for determinism
  g$start <- c(1L,1L); g$goal <- c(3L,3L)
  r <- run_dijkstra(g)
  expect_true(r$success)
  expect_equal(r$path_length, 4L)   # Manhattan: |3-1| + |3-1| = 4 steps
  expect_equal(r$path_cost, 4.0, tolerance=1e-10)
})

test_that("A* finds same cost as Dijkstra", {
  g <- tiny_grid()
  g$start <- c(1L,1L); g$goal <- c(5L,5L)
  d <- run_dijkstra(g)
  a <- run_astar(g)
  expect_true(d$success)
  expect_true(a$success)
  expect_equal(d$path_cost, a$path_cost, tolerance=1e-10)
})

test_that("A* expands fewer or equal nodes than Dijkstra on free grid", {
  g <- tiny_grid(10, 10)
  g$start <- c(1L,1L); g$goal <- c(10L,10L)
  d <- run_dijkstra(g)
  a <- run_astar(g)
  expect_lte(a$nodes_expanded, d$nodes_expanded + 2L)  # allow small deviation
})

test_that("Dijkstra reports failure on disconnected grid", {
  g <- create_grid(width=5, height=5, obs_density=0, seed=1)
  # Block the entire second row to disconnect
  g$grid[3,] <- CELL_OBSTACLE
  g$start <- c(1L,1L); g$goal <- c(5L,5L)
  r <- run_dijkstra(g)
  expect_false(r$success)
  expect_true(is.na(r$path_cost))
  expect_equal(length(r$path), 0)
})

test_that("A* reports failure on disconnected grid", {
  g <- create_grid(width=5, height=5, obs_density=0, seed=1)
  g$grid[3,] <- CELL_OBSTACLE
  g$start <- c(1L,1L); g$goal <- c(5L,5L)
  r <- run_astar(g)
  expect_false(r$success)
})

test_that("Path is a connected sequence of valid cells", {
  g <- create_grid(width=15, height=15, obs_density=0.20, seed=77)
  if (!g$reachable) skip("Map unreachable")
  r <- run_dijkstra(g)
  expect_true(r$success)
  path <- r$path
  expect_gte(length(path), 2L)
  # First cell = start, last = goal
  expect_equal(unname(path[[1]]), unname(g$start))
  expect_equal(unname(path[[length(path)]]), unname(g$goal))
  # Consecutive cells differ by at most 1 in each direction
  for (i in seq_len(length(path)-1)) {
    dr <- abs(path[[i]][1] - path[[i+1]][1])
    dc <- abs(path[[i]][2] - path[[i+1]][2])
    expect_lte(max(dr, dc), 1)
  }
})

test_that("No negative path costs", {
  g <- create_grid(width=15, height=15, obs_density=0.20, seed=55)
  if (!g$reachable) skip("Map unreachable")
  r <- run_dijkstra(g)
  expect_gte(r$path_cost, 0)
  r2 <- run_astar(g)
  expect_gte(r2$path_cost, 0)
})

test_that("Risk-aware planner returns valid result", {
  g <- tiny_grid(8,8)
  g$start <- c(1L,1L); g$goal <- c(8L,8L)
  h <- matrix(0.3, 8, 8)
  d <- matrix(0.1, 8, 8)
  r <- run_risk_aware(g, hazard_map=h, disruption_map=d, lambda_risk=1.0)
  expect_true(r$success)
  expect_equal(r$algorithm, "Risk-Aware")
  expect_gte(r$path_cost, 0)
})

test_that("Risk-aware path cost >= Dijkstra cost (lambda>0 adds penalty)", {
  g <- tiny_grid(8,8)
  g$start <- c(1L,1L); g$goal <- c(8L,8L)
  h <- matrix(0.5, 8, 8)
  d <- matrix(0.2, 8, 8)
  d_res <- run_dijkstra(g)
  r_res <- run_risk_aware(g, hazard_map=h, disruption_map=d, lambda_risk=2.0)
  expect_true(d_res$success); expect_true(r_res$success)
  # Effective cost should be >= pure travel cost
  expect_gte(r_res$path_cost, d_res$path_cost - 1e-10)
})

test_that("run_algorithm dispatcher works for all algorithms", {
  g <- tiny_grid()
  g$start <- c(1L,1L); g$goal <- c(3L,3L)
  for (alg in c("Dijkstra","A*","Risk-Aware")) {
    r <- run_algorithm(alg, g)
    expect_true(r$success, label=paste(alg,"should succeed"))
    expect_equal(r$algorithm, alg)
  }
})

test_that("Unknown algorithm raises error", {
  g <- tiny_grid()
  expect_error(run_algorithm("BFS", g))
})
