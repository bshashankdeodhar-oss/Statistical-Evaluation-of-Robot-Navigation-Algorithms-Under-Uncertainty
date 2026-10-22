# =============================================================================
# experiments.R -- Monte Carlo Experiment Engine
# =============================================================================
# Runs factorial experiments over:
#   - Algorithms
#   - Obstacle densities
#   - Uncertainty levels
#   - Disruption probabilities
#   - Risk aversion parameters
#   - Replanning modes
#
# Saves trial-level results to CSV; supports reproducibility via seeds.
# =============================================================================

# (loaded externally)

# ---------------------------------------------------------------------------
# run_experiment
# ---------------------------------------------------------------------------
#' Run a full Monte Carlo experiment.
#'
#' @param n_trials       Integer. Trials per experimental cell.
#' @param n_maps         Integer. Independently generated maps per condition.
#' @param algorithms     Character vector. Algorithms to compare.
#' @param obs_densities  Numeric vector. Obstacle density levels.
#' @param uncertainty_levels Numeric vector. Edge-cost CV values.
#' @param disruption_probs   Numeric vector. Edge-blocking probabilities.
#' @param lambda_values  Numeric vector. Risk-aversion parameters.
#' @param replanning_modes Logical vector. c(FALSE, TRUE) to test both.
#' @param grid_width     Integer.
#' @param grid_height    Integer.
#' @param movement       Character.
#' @param base_seed      Integer. Reproducible seed offset.
#' @param output_dir     Character. Directory to save CSV results.
#' @param progress_fn    Function(current, total, msg) for progress feedback.
#'                       NULL suppresses progress output.
#'
#' @return data.frame of all trial results.
run_experiment <- function(
    n_trials          = 30L,
    n_maps            = 5L,
    algorithms        = c("Dijkstra", "A*", "Risk-Aware"),
    obs_densities     = c(0.10, 0.20, 0.30),
    uncertainty_levels= c(0.05, 0.15, 0.30),
    disruption_probs  = c(0.05, 0.15),
    lambda_values     = c(0.5, 1.0, 2.0),
    replanning_modes  = c(FALSE, TRUE),
    grid_width        = 20L,
    grid_height       = 20L,
    movement          = "four",
    base_seed         = 42L,
    output_dir        = "results",
    progress_fn       = function(cur, tot, msg) {
      cat(sprintf("[%d/%d] %s\n", cur, tot, msg))
      flush.console()
    }
) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Build the experiment grid
  conditions <- expand.grid(
    obs_density       = obs_densities,
    uncertainty_level = uncertainty_levels,
    disruption_prob   = disruption_probs,
    lambda_risk       = lambda_values,
    replanning        = replanning_modes,
    stringsAsFactors  = FALSE
  )

  total_cells  <- nrow(conditions)
  total_trials <- total_cells * n_maps * n_trials * length(algorithms)
  all_results  <- vector("list", total_trials)
  idx <- 0L

  for (ci in seq_len(nrow(conditions))) {
    cond <- conditions[ci, ]
    unc_cfg <- create_uncertainty_config(
      uncertainty_level = cond$uncertainty_level,
      disruption_prob   = cond$disruption_prob
    )

    for (mi in seq_len(n_maps)) {
      map_seed <- base_seed + ci * 1000L + mi
      set.seed(map_seed)
      grid_obj <- create_grid(
        width       = grid_width,
        height      = grid_height,
        obs_density = cond$obs_density,
        movement    = movement,
        seed        = map_seed
      )
      map_id <- ci * 100L + mi

      for (alg in algorithms) {
        for (ti in seq_len(n_trials)) {
          idx <- idx + 1L
          env_seed <- base_seed + ci * 100000L + mi * 1000L +
                      which(algorithms == alg) * 100L + ti

          if (!is.null(progress_fn) && (idx %% max(1L, total_trials %/% 20L) == 0L)) {
            progress_fn(idx, total_trials,
                        sprintf("Cond %d/%d | Map %d | %s | Trial %d",
                                ci, total_cells, mi, alg, ti))
          }

          tryCatch({
            row <- run_single_trial(
              grid_obj    = grid_obj,
              algorithm   = alg,
              unc_config  = unc_cfg,
              lambda_risk = cond$lambda_risk,
              replanning  = cond$replanning,
              env_seed    = env_seed,
              trial_id    = idx,
              map_id      = map_id
            )
            row$condition_id <- ci
            all_results[[idx]] <- row
          }, error = function(e) {
            all_results[[idx]] <<- data.frame(
              trial_id = idx, map_id = map_id, algorithm = alg,
              failure_reason = paste("ERROR:", conditionMessage(e)),
              overall_success = FALSE, stringsAsFactors = FALSE
            )
          })
        }
      }
    }
  }

  # Combine and save
  results_df <- do.call(rbind, Filter(Negate(is.null), all_results))
  timestamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
  csv_path   <- file.path(output_dir,
                           sprintf("experiment_results_%s.csv", timestamp))
  write.csv(results_df, csv_path, row.names = FALSE)

  if (!is.null(progress_fn)) {
    progress_fn(total_trials, total_trials,
                sprintf("Experiment complete. Results saved to: %s", csv_path))
  }

  attr(results_df, "csv_path")  <- csv_path
  attr(results_df, "base_seed") <- base_seed
  results_df
}

# ---------------------------------------------------------------------------
# run_quick_experiment (practical default for local computers)
# ---------------------------------------------------------------------------
#' Smaller experiment suitable for interactive use (seconds to minutes).
run_quick_experiment <- function(
    n_trials          = 15L,
    n_maps            = 3L,
    algorithms        = c("Dijkstra", "A*", "Risk-Aware"),
    obs_densities     = c(0.10, 0.20, 0.30, 0.40),
    uncertainty_levels= c(0.05, 0.20),
    disruption_probs  = c(0.05, 0.15),
    lambda_values     = c(0.5, 1.5),
    replanning_modes  = c(FALSE, TRUE),
    grid_width        = 20L,
    grid_height       = 20L,
    movement          = "four",
    base_seed         = 42L,
    output_dir        = "results",
    progress_fn       = function(cur, tot, msg) cat(sprintf("[%d/%d] %s\n", cur, tot, msg))
) {
  run_experiment(
    n_trials           = n_trials,
    n_maps             = n_maps,
    algorithms         = algorithms,
    obs_densities      = obs_densities,
    uncertainty_levels = uncertainty_levels,
    disruption_probs   = disruption_probs,
    lambda_values      = lambda_values,
    replanning_modes   = replanning_modes,
    grid_width         = grid_width,
    grid_height        = grid_height,
    movement           = movement,
    base_seed          = base_seed,
    output_dir         = output_dir,
    progress_fn        = progress_fn
  )
}

# ---------------------------------------------------------------------------
# load_experiment_results
# ---------------------------------------------------------------------------
#' Load the most recent experiment CSV from output_dir.
load_experiment_results <- function(output_dir = "results") {
  csvs <- list.files(output_dir, pattern = "experiment_results_.*\\.csv",
                     full.names = TRUE)
  if (length(csvs) == 0L) stop("No experiment results found in: ", output_dir)
  newest <- csvs[order(file.info(csvs)$mtime, decreasing = TRUE)][1L]
  cat("Loading:", newest, "\n")
  read.csv(newest, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# experiment_config_summary
# ---------------------------------------------------------------------------
#' Return a human-readable summary of experiment configuration.
experiment_config_summary <- function(
    n_trials, n_maps, algorithms, obs_densities,
    uncertainty_levels, disruption_probs, lambda_values,
    replanning_modes, grid_width, grid_height, movement, base_seed) {
  n_conditions <- length(obs_densities) * length(uncertainty_levels) *
                  length(disruption_probs) * length(lambda_values) *
                  length(replanning_modes)
  total <- n_conditions * n_maps * n_trials * length(algorithms)
  list(
    n_conditions       = n_conditions,
    total_trials       = total,
    algorithms         = algorithms,
    obs_densities      = obs_densities,
    uncertainty_levels = uncertainty_levels,
    disruption_probs   = disruption_probs,
    lambda_values      = lambda_values,
    replanning_modes   = replanning_modes,
    grid_dims          = paste0(grid_width, "x", grid_height),
    movement           = movement,
    base_seed          = base_seed
  )
}
