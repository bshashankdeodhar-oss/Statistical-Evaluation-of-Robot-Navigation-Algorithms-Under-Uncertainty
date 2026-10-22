# =============================================================================
# metrics.R -- Performance Metrics Computation
# =============================================================================

#' Compute per-algorithm per-condition summary statistics.
#'
#' @param results_df data.frame of trial results from run_experiment.
#' @param failure_cost Numeric. Imputed cost for failed trials in cost analyses.
#'   Default: 3x the max observed realized cost (must be justified in report).
#'
#' @return data.frame with one row per (algorithm, condition) combination.
compute_summary_metrics <- function(results_df, failure_cost = NULL) {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required.")
  library(dplyr)

  # Determine failure_cost imputation value
  if (is.null(failure_cost)) {
    max_obs <- max(results_df$realized_cost, na.rm = TRUE)
    failure_cost <- if (is.finite(max_obs)) max_obs * 3.0 else NA_real_
  }

  results_df %>%
    group_by(algorithm, obs_density, uncertainty_level,
             disruption_prob, lambda_risk, replanning) %>%
    summarise(
      n_trials          = n(),
      n_success         = sum(overall_success, na.rm = TRUE),
      n_plan_fail       = sum(!planning_success, na.rm = TRUE),
      n_exec_fail       = sum(planning_success & !execution_success, na.rm = TRUE),
      success_rate      = mean(overall_success, na.rm = TRUE),
      failure_rate      = 1 - mean(overall_success, na.rm = TRUE),
      # Nominal (planned) cost -- on successful plans only
      mean_nominal_cost      = mean(nominal_cost[planning_success], na.rm = TRUE),
      median_nominal_cost    = median(nominal_cost[planning_success], na.rm = TRUE),
      sd_nominal_cost        = sd(nominal_cost[planning_success], na.rm = TRUE),
      # Realized cost -- successes only (and flagged as selective)
      mean_realized_succ     = mean(realized_cost[overall_success], na.rm = TRUE),
      median_realized_succ   = median(realized_cost[overall_success], na.rm = TRUE),
      sd_realized_succ       = sd(realized_cost[overall_success], na.rm = TRUE),
      q25_realized_succ      = quantile(realized_cost[overall_success], 0.25, na.rm = TRUE),
      q75_realized_succ      = quantile(realized_cost[overall_success], 0.75, na.rm = TRUE),
      q95_realized_succ      = quantile(realized_cost[overall_success], 0.95, na.rm = TRUE),
      # Cost with imputed failures (sensitivity analysis)
      mean_realized_imputed  = mean(ifelse(is.na(realized_cost), failure_cost, realized_cost), na.rm = TRUE),
      sd_realized_imputed    = sd(ifelse(is.na(realized_cost), failure_cost, realized_cost), na.rm = TRUE),
      # Planning efficiency
      mean_nodes_expanded = mean(nodes_expanded, na.rm = TRUE),
      sd_nodes_expanded   = sd(nodes_expanded, na.rm = TRUE),
      mean_planning_time  = mean(planning_time, na.rm = TRUE),
      sd_planning_time    = sd(planning_time, na.rm = TRUE),
      median_planning_time= median(planning_time, na.rm = TRUE),
      # Path lengths
      mean_path_length    = mean(path_length[planning_success], na.rm = TRUE),
      # Replanning
      mean_replannings    = mean(n_replannings, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # 95% CI on success rate (Wilson interval)
      ci_lower_success = pmax(0, success_rate - 1.96 * sqrt(success_rate * (1-success_rate) / n_trials)),
      ci_upper_success = pmin(1, success_rate + 1.96 * sqrt(success_rate * (1-success_rate) / n_trials)),
      # 95% CI on mean realized cost (normal approximation; use bootstrap in statistics.R for rigour)
      ci_lower_realized = mean_realized_succ - 1.96 * sd_realized_succ / sqrt(pmax(1, n_success)),
      ci_upper_realized = mean_realized_succ + 1.96 * sd_realized_succ / sqrt(pmax(1, n_success)),
      failure_cost_used = failure_cost
    )
}

#' Compute pairwise detour cost relative to Dijkstra baseline.
#'
#' @param results_df data.frame of trial results.
#' @return data.frame with detour_ratio columns for A* and Risk-Aware.
compute_detour_metrics <- function(results_df) {
  library(dplyr)
  baseline <- results_df %>%
    filter(algorithm == "Dijkstra", overall_success) %>%
    select(map_id, env_seed, obs_density, uncertainty_level,
           disruption_prob, nominal_cost) %>%
    rename(dijkstra_nominal = nominal_cost)

  results_df %>%
    filter(algorithm != "Dijkstra", overall_success) %>%
    left_join(baseline, by = c("map_id", "env_seed",
                               "obs_density", "uncertainty_level",
                               "disruption_prob")) %>%
    mutate(
      detour_ratio = nominal_cost / dijkstra_nominal,
      detour_abs   = nominal_cost - dijkstra_nominal
    ) %>%
    filter(!is.na(detour_ratio))
}

#' Compute per-trial realized vs. nominal cost ratio.
compute_cost_ratio <- function(results_df) {
  results_df %>%
    filter(overall_success, !is.na(realized_cost), !is.na(nominal_cost),
           nominal_cost > 0) %>%
    mutate(cost_ratio = realized_cost / nominal_cost)
}

#' Return a condensed metrics table suitable for display.
metrics_display_table <- function(summary_df) {
  summary_df %>%
    select(algorithm, obs_density, uncertainty_level, disruption_prob,
           lambda_risk, replanning, n_trials, success_rate,
           mean_planning_time, mean_nodes_expanded,
           mean_realized_succ, sd_realized_succ) %>%
    mutate(across(where(is.numeric), ~round(.x, 4)))
}
