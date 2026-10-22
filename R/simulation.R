# =============================================================================
# simulation.R -- Single-Trial Simulation Engine
# =============================================================================
# Orchestrates one complete navigation trial:
#   1. Use the provided grid (real environment)
#   2. Generate a perceived map (sensor errors)
#   3. Plan on the perceived map
#   4. Apply disruptions and realize execution cost
#   5. Optional replanning on blocked edges
#   6. Return a structured result row
# =============================================================================

# Load dependencies (when run standalone; app.R sources everything globally)
# (loaded externally)
# (loaded externally)
# (loaded externally)

# Null-coalescing
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ---------------------------------------------------------------------------
# run_single_trial
# ---------------------------------------------------------------------------
#' Run one complete navigation trial.
#'
#' @param grid_obj      List from create_grid (the real environment).
#' @param algorithm     Character. "Dijkstra", "A*", or "Risk-Aware".
#' @param unc_config    List from create_uncertainty_config.
#' @param lambda_risk   Numeric. Risk aversion (used for "Risk-Aware" only).
#' @param replanning    Logical. Re-plan from current position when blocked.
#' @param env_seed      Integer. Seed for environmental randomness.
#' @param trial_id      Integer. Identifier for this trial.
#' @param map_id        Integer. Identifier for the map.
#'
#' @return Named list / data.frame row with all trial metrics.
run_single_trial <- function(grid_obj,
                             algorithm   = "Dijkstra",
                             unc_config  = create_uncertainty_config(),
                             lambda_risk = 1.0,
                             replanning  = FALSE,
                             env_seed    = 1L,
                             trial_id    = 1L,
                             map_id      = 1L) {

  # ---- Guard: unreachable real map -----------------------------------------
  if (!grid_obj$reachable) {
    return(.make_failure_row(trial_id, map_id, algorithm, lambda_risk,
                             replanning, unc_config,
                             reason = "map_unreachable"))
  }

  height <- grid_obj$height; width <- grid_obj$width

  # ---- 1. Generate stochastic environment elements -------------------------
  set.seed(env_seed)

  hazard_map <- generate_hazard_map(
    height, width,
    n_hazard_zones = unc_config$n_hazard_zones,
    hazard_radius  = unc_config$hazard_radius,
    max_hazard     = unc_config$max_hazard,
    seed           = env_seed
  )
  disruption_map <- generate_disruption_map(
    height, width,
    base_prob = unc_config$disruption_prob,
    seed      = env_seed + 1L
  )

  # ---- 2. Create perceived map (sensor errors) ------------------------------
  perceived_grid <- apply_sensor_error(
    grid_obj$grid,
    error_prob      = unc_config$sensor_error_prob,
    false_free_prob = unc_config$false_free_prob,
    seed            = env_seed + 2L
  )
  perceived_obj <- grid_obj
  perceived_obj$grid <- perceived_grid

  # Make sure start/goal are not erroneously blocked on the perceived map
  perceived_obj$grid[grid_obj$start[1], grid_obj$start[2]] <- 0L
  perceived_obj$grid[grid_obj$goal[1],  grid_obj$goal[2]]  <- 0L

  # Check reachability on perceived map
  perceived_reachable <- .bfs_reachable(
    perceived_obj$grid, grid_obj$start, grid_obj$goal, grid_obj$movement)

  # ---- 3. Plan on perceived map --------------------------------------------
  plan_t0 <- proc.time()[["elapsed"]]

  plan_result <- if (!perceived_reachable) {
    list(success = FALSE, path = list(), path_cost = NA_real_,
         path_length = NA_integer_, nodes_expanded = 0L,
         algorithm = algorithm, runtime_secs = 0.0)
  } else {
    switch(algorithm,
      "Dijkstra"   = run_dijkstra(perceived_obj),
      "A*"         = run_astar(perceived_obj),
      "Risk-Aware" = run_risk_aware(perceived_obj, hazard_map, disruption_map, lambda_risk),
      stop(paste("Unknown algorithm:", algorithm))
    )
  }

  planning_time <- proc.time()[["elapsed"]] - plan_t0

  # ---- 4. Execution on REAL map -------------------------------------------
  set.seed(env_seed + 3L)
  n_replannings  <- 0L
  total_realized <- 0.0
  execution_success <- FALSE
  path_used <- plan_result$path

  if (!plan_result$success || length(path_used) == 0L) {
    return(.make_failure_row(trial_id, map_id, algorithm, lambda_risk,
                             replanning, unc_config,
                             reason          = "planning_failed",
                             planning_time   = planning_time,
                             nodes_expanded  = plan_result$nodes_expanded %||% 0L,
                             nominal_cost    = NA_real_))
  }

  # Validate path on real grid (sensor error may have created phantom free cells)
  path_valid_on_real <- .validate_path(grid_obj$grid, path_used)
  if (!path_valid_on_real) {
    if (!replanning) {
      return(.make_failure_row(trial_id, map_id, algorithm, lambda_risk,
                               replanning, unc_config,
                               reason          = "path_invalid_on_real",
                               planning_time   = planning_time,
                               nodes_expanded  = plan_result$nodes_expanded %||% 0L,
                               nominal_cost    = plan_result$path_cost))
    }
    # Force a replan on real grid from start
    path_used <- .replan(grid_obj, algorithm, hazard_map, disruption_map, lambda_risk)
    n_replannings <- n_replannings + 1L
    if (is.null(path_used)) {
      return(.make_failure_row(trial_id, map_id, algorithm, lambda_risk,
                               replanning, unc_config,
                               reason          = "replan_failed",
                               planning_time   = planning_time,
                               nodes_expanded  = plan_result$nodes_expanded %||% 0L,
                               nominal_cost    = plan_result$path_cost))
    }
  }

  # Simulate execution step by step
  current_pos  <- grid_obj$start
  remaining    <- path_used
  set.seed(env_seed + 4L)
  max_replan_attempts <- 5L

  repeat {
    disruptions <- apply_path_disruptions(remaining, disruption_map)
    exec_result <- realize_path_cost(remaining, disruptions$first_block,
                                     unc_config$uncertainty_level)
    total_realized <- total_realized + exec_result$realized_cost

    if (!exec_result$blocked) {
      execution_success <- TRUE
      break
    }

    # Blocked
    if (!replanning || n_replannings >= max_replan_attempts) break

    # Advance to position just before block
    steps_done <- exec_result$steps_taken
    if (steps_done >= length(remaining) - 1L) { execution_success <- TRUE; break }
    current_pos <- remaining[[steps_done + 1L]]

    # Replan from current_pos on real grid
    tmp_obj <- grid_obj
    tmp_obj$start <- current_pos
    new_path <- .replan(tmp_obj, algorithm, hazard_map, disruption_map, lambda_risk)
    n_replannings <- n_replannings + 1L

    if (is.null(new_path)) break   # replan failed
    remaining <- new_path
  }

  # ---- 5. Assemble result row ----------------------------------------------
  data.frame(
    trial_id          = trial_id,
    map_id            = map_id,
    algorithm         = algorithm,
    lambda_risk       = lambda_risk,
    replanning        = replanning,
    obs_density       = grid_obj$obs_density,
    uncertainty_level = unc_config$uncertainty_level,
    disruption_prob   = unc_config$disruption_prob,
    sensor_error      = unc_config$sensor_error_prob,
    grid_width        = grid_obj$width,
    grid_height       = grid_obj$height,
    planning_success  = plan_result$success,
    execution_success = execution_success,
    overall_success   = plan_result$success && execution_success,
    nominal_cost      = plan_result$path_cost,
    realized_cost     = ifelse(execution_success, total_realized, NA_real_),
    path_length       = plan_result$path_length,
    nodes_expanded    = plan_result$nodes_expanded %||% 0L,
    planning_time     = planning_time,
    n_replannings     = n_replannings,
    failure_reason    = "none",
    env_seed          = env_seed,
    stringsAsFactors  = FALSE
  )
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------
.bfs_reachable <- function(grid, start, goal, movement) {
  height <- nrow(grid); width <- ncol(grid)
  visited <- matrix(FALSE, nrow = height, ncol = width)
  queue <- list(start); visited[start[1], start[2]] <- TRUE
  while (length(queue) > 0L) {
    cur <- queue[[1L]]; queue <- queue[-1L]
    if (cur[1] == goal[1] && cur[2] == goal[2]) return(TRUE)
    for (nb in get_neighbours(grid, cur[1], cur[2], movement)) {
      if (!visited[nb$row, nb$col]) {
        visited[nb$row, nb$col] <- TRUE
        queue <- c(queue, list(c(nb$row, nb$col)))
      }
    }
  }
  FALSE
}

.validate_path <- function(grid, path) {
  if (length(path) == 0L) return(FALSE)
  for (step in path) {
    if (grid[step[1], step[2]] != 0L) return(FALSE)
  }
  TRUE
}

.replan <- function(grid_obj, algorithm, hazard_map, disruption_map, lambda_risk) {
  res <- switch(algorithm,
    "Dijkstra"   = run_dijkstra(grid_obj),
    "A*"         = run_astar(grid_obj),
    "Risk-Aware" = run_risk_aware(grid_obj, hazard_map, disruption_map, lambda_risk),
    run_dijkstra(grid_obj)
  )
  if (res$success) res$path else NULL
}

.make_failure_row <- function(trial_id, map_id, algorithm, lambda_risk,
                              replanning, unc_config,
                              reason         = "unknown",
                              planning_time  = 0.0,
                              nodes_expanded = 0L,
                              nominal_cost   = NA_real_) {
  data.frame(
    trial_id          = trial_id,
    map_id            = map_id,
    algorithm         = algorithm,
    lambda_risk       = lambda_risk,
    replanning        = replanning,
    obs_density       = NA_real_,
    uncertainty_level = unc_config$uncertainty_level,
    disruption_prob   = unc_config$disruption_prob,
    sensor_error      = unc_config$sensor_error_prob,
    grid_width        = NA_integer_,
    grid_height       = NA_integer_,
    planning_success  = FALSE,
    execution_success = FALSE,
    overall_success   = FALSE,
    nominal_cost      = nominal_cost,
    realized_cost     = NA_real_,
    path_length       = NA_integer_,
    nodes_expanded    = nodes_expanded,
    planning_time     = planning_time,
    n_replannings     = 0L,
    failure_reason    = reason,
    env_seed          = NA_integer_,
    stringsAsFactors  = FALSE
  )
}
