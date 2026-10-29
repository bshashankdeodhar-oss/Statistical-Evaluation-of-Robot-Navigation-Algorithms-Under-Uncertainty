# =============================================================================
# visualizations.R -- ggplot2 Publication-Quality Plots
# =============================================================================

library(ggplot2)
library(dplyr)
library(scales)

ALGO_COLOURS <- c(
  "Dijkstra"   = "#3A86FF",
  "A*"         = "#06D6A0",
  "Risk-Aware" = "#FFB703"
)

THEME_ROBOT <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
  theme(
    plot.title    = element_text(face = "bold", size = base_size + 2, hjust = 0.5),
    plot.subtitle = element_text(size = base_size - 1, hjust = 0.5, colour = "grey50"),
    plot.caption  = element_text(size = base_size - 2, colour = "grey60", hjust = 1),
    legend.position = "right",
    legend.title  = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border  = element_rect(colour = "grey85", fill = NA, linewidth = 0.5),
    strip.text    = element_text(face = "bold"),
    axis.title    = element_text(face = "bold")
  )
}

# 1. Runtime distributions by algorithm
plot_runtime_distributions <- function(results_df) {
  df <- results_df %>% filter(planning_success)
  ggplot(df, aes(x = planning_time * 1000, fill = algorithm, colour = algorithm)) +
    geom_density(alpha = 0.35, linewidth = 0.8) +
    scale_fill_manual(values = ALGO_COLOURS) +
    scale_colour_manual(values = ALGO_COLOURS) +
    scale_x_log10(labels = label_comma()) +
    labs(title    = "Planning Runtime Distributions",
         subtitle = "Log scale; successful trials only",
         x        = "Planning time (ms, log scale)",
         y        = "Density",
         fill     = "Algorithm", colour = "Algorithm",
         caption  = "Kernel density estimate") +
    THEME_ROBOT()
}

# 2. Path-cost distributions
plot_cost_distributions <- function(results_df) {
  df <- results_df %>% filter(overall_success, !is.na(realized_cost))
  ggplot(df, aes(x = realized_cost, fill = algorithm)) +
    geom_histogram(bins = 30, alpha = 0.7, position = "dodge", colour = "white") +
    scale_fill_manual(values = ALGO_COLOURS) +
    facet_wrap(~obs_density, labeller = label_both, scales = "free_y") +
    labs(title    = "Realized Travel Cost Distributions",
         subtitle = "Successful trials; faceted by obstacle density",
         x        = "Realized cost", y = "Count", fill = "Algorithm",
         caption  = "Failed trials excluded — see success rate plots") +
    THEME_ROBOT()
}

# 3. Success rates with CI
plot_success_rates <- function(results_df) {
  df <- results_df %>%
    group_by(algorithm, obs_density) %>%
    summarise(n = n(), sr = mean(overall_success, na.rm = TRUE),
              se = sqrt(sr * (1-sr) / n),
              lo = pmax(0, sr - 1.96*se), hi = pmin(1, sr + 1.96*se),
              .groups = "drop")
  ggplot(df, aes(x = obs_density, y = sr, colour = algorithm, group = algorithm)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = algorithm), alpha = 0.15, colour = NA) +
    scale_colour_manual(values = ALGO_COLOURS) +
    scale_fill_manual(values = ALGO_COLOURS) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    labs(title    = "Route Success Rate vs Obstacle Density",
         x        = "Obstacle density", y = "Success rate",
         colour   = "Algorithm", fill = "Algorithm",
         caption  = "Shading = 95% CI (Wilson interval)") +
    THEME_ROBOT()
}

# 4. Nodes expanded vs grid-size proxy (obstacle density as surrogate)
plot_nodes_expanded <- function(results_df) {
  df <- results_df %>% filter(planning_success)
  ggplot(df, aes(x = obs_density, y = nodes_expanded,
                 colour = algorithm, group = algorithm)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.01) +
    scale_colour_manual(values = ALGO_COLOURS) +
    scale_x_continuous(labels = percent) +
    labs(title    = "Mean Nodes Expanded vs Obstacle Density",
         x        = "Obstacle density", y = "Nodes expanded (mean ± SE)",
         colour   = "Algorithm") +
    THEME_ROBOT()
}

# 5. Performance vs uncertainty
plot_perf_vs_uncertainty <- function(results_df) {
  df <- results_df %>%
    group_by(algorithm, uncertainty_level) %>%
    summarise(sr = mean(overall_success, na.rm = TRUE),
              mean_cost = mean(realized_cost[overall_success], na.rm = TRUE),
              .groups = "drop")
  p1 <- ggplot(df, aes(x = uncertainty_level, y = sr, colour = algorithm)) +
    geom_line(linewidth = 1.2) + geom_point(size = 3) +
    scale_colour_manual(values = ALGO_COLOURS) +
    scale_y_continuous(labels = percent) +
    labs(title = "Success Rate vs Uncertainty", x = "Uncertainty (CV)", y = "Success rate") +
    THEME_ROBOT() + theme(legend.position = "none")
  p2 <- ggplot(df, aes(x = uncertainty_level, y = mean_cost, colour = algorithm)) +
    geom_line(linewidth = 1.2) + geom_point(size = 3) +
    scale_colour_manual(values = ALGO_COLOURS) +
    labs(title = "Realized Cost vs Uncertainty", x = "Uncertainty (CV)", y = "Mean realized cost",
         colour = "Algorithm") +
    THEME_ROBOT()
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

# 6. Risk aversion vs realized cost
plot_risk_vs_cost <- function(results_df) {
  df <- results_df %>%
    filter(algorithm == "Risk-Aware", overall_success, !is.na(realized_cost))
  ggplot(df, aes(x = factor(lambda_risk), y = realized_cost)) +
    geom_boxplot(fill = ALGO_COLOURS["Risk-Aware"], alpha = 0.7, outlier.alpha = 0.4) +
    labs(title    = "Risk Aversion Parameter vs Realized Travel Cost",
         subtitle = "Risk-Aware planner only | Successful trials",
         x        = "lambda_risk", y = "Realized travel cost") +
    THEME_ROBOT()
}

# 7. Risk aversion vs success rate
plot_risk_vs_success <- function(results_df) {
  df <- results_df %>%
    filter(algorithm == "Risk-Aware") %>%
    group_by(lambda_risk, obs_density) %>%
    summarise(sr = mean(overall_success, na.rm = TRUE), n = n(), .groups = "drop")
  ggplot(df, aes(x = lambda_risk, y = sr, colour = factor(obs_density),
                 group = obs_density)) +
    geom_line(linewidth = 1.1) + geom_point(size = 2.5) +
    scale_colour_viridis_d(name = "Obstacle\ndensity", labels = function(x) paste0(round(as.numeric(x) * 100), "%")) +
    scale_y_continuous(labels = percent, limits = c(0,1)) +
    labs(title = "Risk Aversion vs Route Success Rate",
         x = "lambda_risk", y = "Success rate") +
    THEME_ROBOT()
}

# 8. Algorithm x Uncertainty interaction
plot_interaction <- function(results_df) {
  df <- results_df %>%
    filter(overall_success, !is.na(realized_cost)) %>%
    group_by(algorithm, uncertainty_level) %>%
    summarise(mean_cost = mean(realized_cost, na.rm = TRUE),
              se_cost   = sd(realized_cost, na.rm=TRUE)/sqrt(n()),
              .groups = "drop")
  ggplot(df, aes(x = uncertainty_level, y = mean_cost,
                 colour = algorithm, group = algorithm)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 3) +
    geom_ribbon(aes(ymin = mean_cost - se_cost, ymax = mean_cost + se_cost,
                    fill = algorithm), alpha = 0.15, colour = NA) +
    scale_colour_manual(values = ALGO_COLOURS) +
    scale_fill_manual(values = ALGO_COLOURS) +
    labs(title    = "Algorithm x Uncertainty Interaction",
         subtitle = "Non-parallel lines indicate interaction",
         x        = "Uncertainty level (CV)",
         y        = "Mean realized cost",
         colour   = "Algorithm", fill = "Algorithm") +
    THEME_ROBOT()
}

# 9. Boxplot comparison: planning time
plot_runtime_boxplot <- function(results_df) {
  df <- results_df %>% filter(planning_success)
  ggplot(df, aes(x = algorithm, y = planning_time * 1000, fill = algorithm)) +
    geom_boxplot(alpha = 0.8, outlier.size = 1, outlier.alpha = 0.5) +
    scale_fill_manual(values = ALGO_COLOURS, guide = "none") +
    scale_y_log10(labels = label_comma()) +
    labs(title    = "Planning Runtime Comparison",
         x        = "Algorithm",
         y        = "Planning time (ms, log scale)",
         caption  = "Boxes: Q1-Q3; whiskers: 1.5x IQR; dots: outliers") +
    THEME_ROBOT()
}

# 10. Detour ratio vs Dijkstra
plot_detour_ratio <- function(results_df) {
  df <- compute_detour_metrics(results_df)
  if (nrow(df) == 0) return(ggplot() + labs(title = "No detour data available"))
  ggplot(df, aes(x = algorithm, y = detour_ratio, fill = algorithm)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
    geom_boxplot(alpha = 0.8) +
    scale_fill_manual(values = ALGO_COLOURS[c("A*","Risk-Aware")], guide = "none") +
    labs(title    = "Nominal Cost Ratio vs Dijkstra Baseline",
         subtitle = "1.0 = same cost as Dijkstra on the same map",
         x        = "Algorithm", y = "Cost ratio (vs Dijkstra)") +
    THEME_ROBOT()
}

# 11. Diagnostic: Residual plot for linear model
plot_lm_diagnostics <- function(lm_fit) {
  if (is.null(lm_fit)) return(ggplot() + labs(title = "No model to diagnose"))
  df <- data.frame(
    fitted   = fitted(lm_fit),
    residuals= residuals(lm_fit)
  )
  p1 <- ggplot(df, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.4, size = 0.8) +
    geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, colour = "blue", linewidth = 0.8) +
    labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
    THEME_ROBOT()
  p2 <- ggplot(df, aes(sample = residuals)) +
    stat_qq(alpha = 0.5) + stat_qq_line(colour = "red") +
    labs(title = "Normal Q-Q Plot", x = "Theoretical quantiles", y = "Sample quantiles") +
    THEME_ROBOT()
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

# Helper (imported from metrics.R -- redefined here for standalone use)
compute_detour_metrics <- function(results_df) {
  library(dplyr)
  baseline <- results_df %>%
    filter(algorithm == "Dijkstra", overall_success) %>%
    group_by(map_id, obs_density, uncertainty_level, disruption_prob) %>%
    summarise(dijkstra_nominal = mean(nominal_cost, na.rm=TRUE), .groups="drop")
  results_df %>%
    filter(algorithm != "Dijkstra", overall_success) %>%
    left_join(baseline, by = c("map_id", "obs_density",
                               "uncertainty_level", "disruption_prob")) %>%
    mutate(detour_ratio = nominal_cost / dijkstra_nominal,
           detour_abs   = nominal_cost - dijkstra_nominal) %>%
    filter(!is.na(detour_ratio))
}
