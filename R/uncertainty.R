# =============================================================================
# uncertainty.R -- Stochastic Environment Uncertainty
# =============================================================================
# Implements:
#   - Edge-cost uncertainty (actual cost differs from estimated)
#   - Unexpected edge blocking (post-plan disruptions)
#   - Sensor/map error (planner sees wrong map)
#   - Hazard maps (high-risk cells)
#   - All randomness uses explicit seeds for reproducibility
# =============================================================================

# ---------------------------------------------------------------------------
# generate_hazard_map
# ---------------------------------------------------------------------------
#' Generate a per-cell hazard score.
#'
#' @param height,width Grid dimensions.
#' @param n_hazard_zones Integer. Number of hazard clusters.
#' @param hazard_radius  Numeric. Radius of each cluster.
#' @param max_hazard     Numeric in (0,1]. Peak hazard value.
#' @param seed           Integer or NULL.
#'
#' @return Numeric matrix (height x width) with values in [0, max_hazard].
generate_hazard_map <- function(height, width,
                                n_hazard_zones = 3L,
                                hazard_radius  = 3.0,
                                max_hazard     = 0.8,
                                seed           = NULL) {
  if (!is.null(seed)) set.seed(seed)
  hmap <- matrix(0.0, nrow = height, ncol = width)

  for (z in seq_len(n_hazard_zones)) {
    cr <- sample(seq_len(height), 1L)
    cc <- sample(seq_len(width),  1L)
    for (r in seq_len(height)) {
      for (c in seq_len(width)) {
        d <- sqrt((r - cr)^2 + (c - cc)^2)
        influence <- max_hazard * exp(-d^2 / (2 * hazard_radius^2))
        hmap[r, c] <- min(1.0, hmap[r, c] + influence)
      }
    }
  }
  hmap
}

# ---------------------------------------------------------------------------
# generate_disruption_map
# ---------------------------------------------------------------------------
#' Generate a per-cell edge-disruption probability.
#'
#' Each cell value represents the probability that an outgoing edge from
#' that cell becomes blocked after planning (uniform across its neighbours).
#'
#' @param height,width Grid dimensions.
#' @param base_prob    Numeric in [0,1). Baseline disruption probability.
#' @param seed         Integer or NULL.
#'
#' @return Numeric matrix (height x width) with values in [0, 1).
generate_disruption_map <- function(height, width,
                                    base_prob = 0.05,
                                    seed      = NULL) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(base_prob >= 0 && base_prob < 1)
  # Beta distribution skewed to concentrate near base_prob
  alpha <- max(0.5, base_prob * 5)
  beta  <- max(0.5, (1 - base_prob) * 5)
  matrix(rbeta(height * width, alpha, beta) * base_prob * 2,
         nrow = height, ncol = width) |> pmin(0.99)
}

# ---------------------------------------------------------------------------
# apply_sensor_error
# ---------------------------------------------------------------------------
#' Produce a corrupted grid (the planner's erroneous map).
#'
#' @param true_grid    Integer matrix. The real environment.
#' @param error_prob   Numeric in [0,1]. Probability a free cell is shown as
#'                     blocked (false obstacle), or a blocked cell as free
#'                     (false clearance).
#' @param false_free_prob Numeric in [0,1]. Probability a true obstacle is
#'                        shown as traversable.
#' @param seed         Integer or NULL.
#'
#' @return Integer matrix (same dimensions) — the corrupted perceived map.
apply_sensor_error <- function(true_grid,
                               error_prob      = 0.05,
                               false_free_prob = 0.02,
                               seed            = NULL) {
  if (!is.null(seed)) set.seed(seed)
  perceived <- true_grid
  height <- nrow(true_grid); width <- ncol(true_grid)

  for (r in seq_len(height)) {
    for (c in seq_len(width)) {
      if (true_grid[r, c] == 0L) {
        # Free cell wrongly marked as obstacle
        if (runif(1) < error_prob) perceived[r, c] <- 1L
      } else {
        # Obstacle wrongly cleared
        if (runif(1) < false_free_prob) perceived[r, c] <- 0L
      }
    }
  }
  perceived
}

# ---------------------------------------------------------------------------
# apply_edge_cost_uncertainty
# ---------------------------------------------------------------------------
#' Sample realized edge costs with multiplicative noise.
#'
#' Model: realized_cost = nominal_cost * Gamma(shape, rate=shape/CV^2)
#'   where CV is the coefficient of variation.
#' The Gamma distribution is chosen because:
#'   - It is defined on (0, ∞), matching non-negative costs.
#'   - Its shape parameter controls how concentrated costs are.
#'   - CV provides an intuitive uncertainty lever.
#'
#' @param nominal_cost  Numeric. The planned edge cost.
#' @param uncertainty   Numeric >= 0. Coefficient of variation (CV).
#'                      0 = deterministic, 0.5 = high variability.
#'
#' @return Numeric. A non-negative realized cost.
apply_edge_cost_uncertainty <- function(nominal_cost, uncertainty = 0.1) {
  if (uncertainty <= 0 || nominal_cost <= 0) return(nominal_cost)
  # Gamma parameterization: mean = nominal_cost, CV = uncertainty
  shape <- 1.0 / (uncertainty^2)
  scale <- nominal_cost / shape
  rgamma(1L, shape = shape, scale = scale)
}

# ---------------------------------------------------------------------------
# apply_path_disruptions
# ---------------------------------------------------------------------------
#' Determine which edges in a planned path become blocked during execution.
#'
#' @param path           List of c(row,col) vectors.
#' @param disruption_map Numeric matrix. Per-cell disruption probability.
#' @param seed           Integer or NULL.
#'
#' @return List with:
#'   \item{blocked_edges}{Integer vector of step indices that are blocked.}
#'   \item{first_block}{Integer or NA. Index of first blocked step.}
apply_path_disruptions <- function(path, disruption_map, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_steps <- length(path) - 1L
  if (n_steps <= 0L) return(list(blocked_edges = integer(0), first_block = NA_integer_))

  blocked <- integer(0)
  for (i in seq_len(n_steps)) {
    r <- path[[i]][1]; c <- path[[i]][2]
    p_block <- disruption_map[r, c]
    if (runif(1L) < p_block) blocked <- c(blocked, i)
  }
  list(
    blocked_edges = blocked,
    first_block   = if (length(blocked) > 0L) min(blocked) else NA_integer_
  )
}

# ---------------------------------------------------------------------------
# realize_path_cost
# ---------------------------------------------------------------------------
#' Compute the realized travel cost when traversing a path.
#'
#' Steps up to (and including) a block are costed with uncertainty noise.
#' After a block: either execution fails or replanning occurs (handled outside).
#'
#' @param path         List of c(row,col) vectors (the planned path).
#' @param first_block  Integer or NA. First blocked step index.
#' @param uncertainty  Numeric. CV for edge cost noise.
#' @param seed         Integer or NULL.
#'
#' @return List with:
#'   \item{realized_cost}{Numeric. Cost actually incurred (up to block).}
#'   \item{steps_taken}{Integer.}
#'   \item{blocked}{Logical.}
realize_path_cost <- function(path, first_block = NA_integer_,
                              uncertainty = 0.1, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_steps <- length(path) - 1L
  if (n_steps <= 0L) return(list(realized_cost = 0.0, steps_taken = 0L, blocked = FALSE))

  end_step <- if (!is.na(first_block)) first_block - 1L else n_steps
  end_step  <- max(0L, end_step)

  realized_cost <- 0.0
  for (i in seq_len(end_step)) {
    s <- path[[i]]; e <- path[[i + 1L]]
    dr <- abs(s[1] - e[1]); dc <- abs(s[2] - e[2])
    nom_cost <- if (dr == 1L && dc == 1L) sqrt(2) else 1.0
    realized_cost <- realized_cost + apply_edge_cost_uncertainty(nom_cost, uncertainty)
  }

  list(
    realized_cost = realized_cost,
    steps_taken   = end_step,
    blocked       = !is.na(first_block)
  )
}

# ---------------------------------------------------------------------------
# create_uncertainty_config
# ---------------------------------------------------------------------------
#' Bundle all uncertainty parameters into a single configuration list.
#'
#' @param uncertainty_level Numeric [0,1]. Edge-cost CV.
#' @param disruption_prob   Numeric [0,1). Base edge-blocking probability.
#' @param sensor_error_prob Numeric [0,1). Free->obstacle error rate.
#' @param false_free_prob   Numeric [0,1). Obstacle->free error rate.
#' @param n_hazard_zones    Integer. Number of hazard clusters.
#' @param hazard_radius     Numeric. Hazard cluster radius.
#' @param max_hazard        Numeric (0,1]. Peak hazard intensity.
#'
#' @return Named list of parameters.
create_uncertainty_config <- function(uncertainty_level = 0.1,
                                      disruption_prob   = 0.05,
                                      sensor_error_prob = 0.05,
                                      false_free_prob   = 0.02,
                                      n_hazard_zones    = 3L,
                                      hazard_radius     = 3.0,
                                      max_hazard        = 0.5) {
  list(
    uncertainty_level = uncertainty_level,
    disruption_prob   = disruption_prob,
    sensor_error_prob = sensor_error_prob,
    false_free_prob   = false_free_prob,
    n_hazard_zones    = as.integer(n_hazard_zones),
    hazard_radius     = hazard_radius,
    max_hazard        = max_hazard
  )
}
