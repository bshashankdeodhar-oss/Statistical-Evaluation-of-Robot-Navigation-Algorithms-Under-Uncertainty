# =============================================================================
# algorithms.R -- Pathfinding Algorithms
# =============================================================================
# Implements: Dijkstra, A*, Risk-Aware Planner
# All algorithms return a uniform result list so callers can treat them
# interchangeably.
#
# Return structure:
#   list(
#     success        = logical,
#     path           = list of c(row,col) vectors (empty on failure),
#     path_cost      = numeric (NA on failure),
#     path_length    = integer (number of steps, NA on failure),
#     nodes_expanded = integer,
#     algorithm      = character,
#     runtime_secs   = numeric
#   )
# =============================================================================

# (dependencies sourced externally via app.R or test files)

# Null-coalescing helper (base R)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ---------------------------------------------------------------------------
# Priority queue (min-heap) via environment-based sorted list
# For grids up to ~100x100 the simple approach is adequate.
# For larger grids, use a proper heap (data.table or Rcpp).
# ---------------------------------------------------------------------------
make_pq <- function() {
  pq <- list()
  list(
    push = function(priority, value) {
      pq[[length(pq) + 1L]] <<- list(p = priority, v = value)
    },
    pop = function() {
      if (length(pq) == 0L) stop("Priority queue is empty.")
      idx <- which.min(vapply(pq, function(x) x$p, numeric(1L)))
      val <- pq[[idx]]
      pq[[idx]] <<- NULL
      val$v
    },
    peek_priority = function() {
      if (length(pq) == 0L) return(Inf)
      min(vapply(pq, function(x) x$p, numeric(1L)))
    },
    size = function() length(pq),
    empty = function() length(pq) == 0L
  )
}

# ---------------------------------------------------------------------------
# reconstruct_path
# ---------------------------------------------------------------------------
reconstruct_path <- function(came_from, start_id, goal_id, width) {
  path <- list()
  cur  <- goal_id
  while (!is.na(cur) && cur != start_id) {
    path <- c(list(id_to_cell(cur, width)), path)
    cur  <- came_from[[as.character(cur)]]
  }
  if (!is.null(start_id) && !is.na(start_id)) {
    path <- c(list(id_to_cell(start_id, width)), path)
  }
  path
}

# ---------------------------------------------------------------------------
# run_dijkstra
# ---------------------------------------------------------------------------
#' Dijkstra shortest-path algorithm on a grid.
#'
#' @param grid_obj  List returned by \code{create_grid}.
#' @param cost_fn   Optional function(r1,c1,r2,c2,cost) -> numeric to override
#'                  edge costs (useful for risk-aware planning).
#' @return Standard result list (see module header).
run_dijkstra <- function(grid_obj, cost_fn = NULL) {
  t0 <- proc.time()[["elapsed"]]

  grid     <- grid_obj$grid
  start    <- grid_obj$start
  goal     <- grid_obj$goal
  movement <- grid_obj$movement
  width    <- grid_obj$width

  start_id <- cell_id(start[1], start[2], width)
  goal_id  <- cell_id(goal[1],  goal[2],  width)

  dist      <- list(); dist[[as.character(start_id)]] <- 0.0
  came_from <- list(); came_from[[as.character(start_id)]] <- NA_integer_

  pq <- make_pq(); pq$push(0.0, start_id)
  nodes_expanded <- 0L

  while (!pq$empty()) {
    cur_id <- pq$pop()
    cur    <- id_to_cell(cur_id, width)

    if (cur_id == goal_id) break

    cur_dist <- dist[[as.character(cur_id)]] %||% Inf
    if (cur_dist == Inf) next

    nodes_expanded <- nodes_expanded + 1L

    for (nb in get_neighbours(grid, cur[["row"]], cur[["col"]], movement)) {
      nb_id <- cell_id(nb$row, nb$col, width)
      edge_cost <- if (!is.null(cost_fn)) {
        cost_fn(cur[["row"]], cur[["col"]], nb$row, nb$col, nb$cost)
      } else {
        nb$cost
      }
      if (edge_cost < 0) edge_cost <- 0   # guard: costs must be non-negative
      new_dist <- cur_dist + edge_cost
      old_dist <- dist[[as.character(nb_id)]] %||% Inf
      if (new_dist < old_dist) {
        dist[[as.character(nb_id)]]      <- new_dist
        came_from[[as.character(nb_id)]] <- cur_id
        pq$push(new_dist, nb_id)
      }
    }
  }

  runtime <- proc.time()[["elapsed"]] - t0

  if (is.null(dist[[as.character(goal_id)]])) {
    return(list(success=FALSE, path=list(), path_cost=NA_real_,
                path_length=NA_integer_, nodes_expanded=nodes_expanded,
                algorithm="Dijkstra", runtime_secs=runtime))
  }

  path      <- reconstruct_path(came_from, start_id, goal_id, width)
  path_cost <- dist[[as.character(goal_id)]]

  list(success=TRUE, path=path, path_cost=path_cost,
       path_length=length(path) - 1L,
       nodes_expanded=nodes_expanded, algorithm="Dijkstra",
       runtime_secs=runtime)
}

# ---------------------------------------------------------------------------
# run_astar
# ---------------------------------------------------------------------------
#' A* algorithm on a grid.
#'
#' @param grid_obj List returned by \code{create_grid}.
#' @param cost_fn  Optional cost override (same signature as in Dijkstra).
#' @return Standard result list.
run_astar <- function(grid_obj, cost_fn = NULL) {
  t0 <- proc.time()[["elapsed"]]

  grid     <- grid_obj$grid
  start    <- grid_obj$start
  goal     <- grid_obj$goal
  movement <- grid_obj$movement
  width    <- grid_obj$width

  start_id <- cell_id(start[1], start[2], width)
  goal_id  <- cell_id(goal[1],  goal[2],  width)

  g_score   <- list(); g_score[[as.character(start_id)]] <- 0.0
  came_from <- list(); came_from[[as.character(start_id)]] <- NA_integer_

  h0 <- heuristic(start[1], start[2], goal[1], goal[2], movement)
  pq <- make_pq(); pq$push(h0, start_id)
  closed <- list()
  nodes_expanded <- 0L

  while (!pq$empty()) {
    cur_id <- pq$pop()
    if (!is.null(closed[[as.character(cur_id)]])) next
    closed[[as.character(cur_id)]] <- TRUE

    if (cur_id == goal_id) break

    cur     <- id_to_cell(cur_id, width)
    cur_g   <- g_score[[as.character(cur_id)]] %||% Inf
    nodes_expanded <- nodes_expanded + 1L

    for (nb in get_neighbours(grid, cur[["row"]], cur[["col"]], movement)) {
      nb_id <- cell_id(nb$row, nb$col, width)
      if (!is.null(closed[[as.character(nb_id)]])) next

      edge_cost <- if (!is.null(cost_fn)) {
        cost_fn(cur[["row"]], cur[["col"]], nb$row, nb$col, nb$cost)
      } else {
        nb$cost
      }
      if (edge_cost < 0) edge_cost <- 0
      new_g <- cur_g + edge_cost
      old_g <- g_score[[as.character(nb_id)]] %||% Inf

      if (new_g < old_g) {
        g_score[[as.character(nb_id)]]   <- new_g
        came_from[[as.character(nb_id)]] <- cur_id
        h_nb <- heuristic(nb$row, nb$col, goal[1], goal[2], movement)
        pq$push(new_g + h_nb, nb_id)
      }
    }
  }

  runtime <- proc.time()[["elapsed"]] - t0

  if (is.null(g_score[[as.character(goal_id)]])) {
    return(list(success=FALSE, path=list(), path_cost=NA_real_,
                path_length=NA_integer_, nodes_expanded=nodes_expanded,
                algorithm="A*", runtime_secs=runtime))
  }

  path      <- reconstruct_path(came_from, start_id, goal_id, width)
  path_cost <- g_score[[as.character(goal_id)]]

  list(success=TRUE, path=path, path_cost=path_cost,
       path_length=length(path) - 1L,
       nodes_expanded=nodes_expanded, algorithm="A*",
       runtime_secs=runtime)
}

# ---------------------------------------------------------------------------
# run_risk_aware
# ---------------------------------------------------------------------------
#' Risk-aware path planner.
#'
#' Minimises: travel_cost + lambda_risk * risk_penalty
#'
#' risk_penalty for an edge is derived from:
#'   - The hazard map (per-cell risk scores in [0, 1])
#'   - The disruption probability of the edge
#'
#' Effective cost: travel_cost + lambda_risk * (hazard_avg + disruption_prob)
#'
#' Assumptions:
#'   - lambda_risk >= 0
#'   - All effective costs >= 0 by construction
#'   - The planner uses Dijkstra internally (A* with risk-adjusted heuristic
#'     would require a tighter bound; omitted for correctness)
#'   - The planner does NOT guarantee shortest travel cost
#'
#' @param grid_obj       List returned by \code{create_grid}.
#' @param hazard_map     Numeric matrix (height x width), values in [0,1].
#'                       NULL means uniform zero risk.
#' @param disruption_map Numeric matrix (height x width). Per-cell probability
#'                       that an outgoing edge is blocked [0, 1].
#' @param lambda_risk    Numeric >= 0. Risk aversion parameter.
#' @return Standard result list with extra field \code{risk_cost}.
run_risk_aware <- function(grid_obj,
                           hazard_map     = NULL,
                           disruption_map = NULL,
                           lambda_risk    = 1.0) {
  stopifnot(lambda_risk >= 0)

  height <- grid_obj$height
  width  <- grid_obj$width

  if (is.null(hazard_map))
    hazard_map <- matrix(0.0, nrow = height, ncol = width)
  if (is.null(disruption_map))
    disruption_map <- matrix(0.0, nrow = height, ncol = width)

  risk_cost_fn <- function(r1, c1, r2, c2, base_cost) {
    hazard_avg   <- (hazard_map[r1, c1] + hazard_map[r2, c2]) / 2.0
    disrupt_prob <- (disruption_map[r1, c1] + disruption_map[r2, c2]) / 2.0
    risk_penalty <- hazard_avg + disrupt_prob
    eff_cost <- base_cost + lambda_risk * risk_penalty
    max(0.0, eff_cost)
  }

  t0     <- proc.time()[["elapsed"]]
  result <- run_dijkstra(grid_obj, cost_fn = risk_cost_fn)
  result$runtime_secs <- proc.time()[["elapsed"]] - t0
  result$algorithm    <- "Risk-Aware"
  result$lambda_risk  <- lambda_risk

  # Also compute pure travel cost of the chosen path
  if (result$success) {
    pure_travel <- 0.0
    path <- result$path
    for (i in seq_len(length(path) - 1L)) {
      s <- path[[i]]; e <- path[[i + 1L]]
      dist_r <- abs(s[1] - e[1]); dist_c <- abs(s[2] - e[2])
      step_cost <- if (dist_r == 1L && dist_c == 1L) sqrt(2) else 1.0
      pure_travel <- pure_travel + step_cost
    }
    result$pure_travel_cost <- pure_travel
    result$risk_cost <- result$path_cost - pure_travel  # risk penalty absorbed
  } else {
    result$pure_travel_cost <- NA_real_
    result$risk_cost        <- NA_real_
  }
  result
}

# ---------------------------------------------------------------------------
# run_algorithm  (dispatcher)
# ---------------------------------------------------------------------------
#' Run a named algorithm on a grid.
#'
#' @param algorithm  Character. One of "Dijkstra", "A*", "Risk-Aware".
#' @param grid_obj   List from \code{create_grid}.
#' @param ...        Additional args forwarded to the specific planner.
#' @return Standard result list.
run_algorithm <- function(algorithm, grid_obj, ...) {
  switch(algorithm,
         "Dijkstra"   = run_dijkstra(grid_obj, ...),
         "A*"         = run_astar(grid_obj, ...),
         "Risk-Aware" = run_risk_aware(grid_obj, ...),
         stop(sprintf("Unknown algorithm: '%s'", algorithm)))
}
