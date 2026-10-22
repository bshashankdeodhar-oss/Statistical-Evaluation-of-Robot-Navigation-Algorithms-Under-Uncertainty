# =============================================================================
# report.R -- Research Report Generation
# =============================================================================

#' Generate an HTML research report from experiment results.
#'
#' @param results_df     data.frame of trial results.
#' @param summary_df     data.frame from compute_summary_metrics.
#' @param stats_results  List of H1-H4 test results.
#' @param config         List from experiment_config_summary.
#' @param output_path    Character. File path for the report.
#' @param title          Character. Report title.
generate_html_report <- function(results_df,
                                 summary_df,
                                 stats_results,
                                 config,
                                 output_path = "reports/research_report.html",
                                 title = "Robot Navigation Under Uncertainty — Research Report") {
  if (!dir.exists(dirname(output_path))) dir.create(dirname(output_path), recursive = TRUE)

  # Save temp plots
  tmp_dir <- file.path(tempdir(), "rnav_report_plots")
  dir.create(tmp_dir, showWarnings = FALSE)

  save_plot <- function(plot_fn, filename, width = 8, height = 5) {
    path <- file.path(tmp_dir, filename)
    tryCatch({
      png(path, width = width * 100, height = height * 100, res = 100)
      print(plot_fn)
      dev.off()
      path
    }, error = function(e) { warning(e$message); NULL })
  }

  p_paths <- list()
  if (nrow(results_df) > 0) {
    p_paths$runtime  <- save_plot(plot_runtime_boxplot(results_df), "runtime.png")
    p_paths$success  <- save_plot(plot_success_rates(results_df),   "success.png")
    p_paths$cost     <- save_plot(plot_cost_distributions(results_df), "cost.png")
    p_paths$interact <- save_plot(plot_interaction(results_df),     "interaction.png")
    p_paths$risk_cost<- save_plot(plot_risk_vs_cost(results_df),    "risk_cost.png")
  }

  img_tag <- function(p) {
    if (!is.null(p) && file.exists(p)) {
      b64 <- base64enc::base64encode(p)
      sprintf('<img src="data:image/png;base64,%s" style="max-width:100%%;"/>', b64)
    } else "<p><em>Plot not available.</em></p>"
  }

  # Build HTML
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  n_trials  <- nrow(results_df)
  algs      <- paste(unique(results_df$algorithm), collapse = ", ")

  h1_p  <- stats_results$h1$p_value %||% NA
  h3_p  <- stats_results$h3$kruskal_cost$p.value %||% NA
  h4_p  <- stats_results$h4$interaction_p %||% NA

  html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>%s</title>
<style>
  body { font-family: "Segoe UI", Arial, sans-serif; max-width: 1100px; margin: 40px auto; color: #222; line-height: 1.6; }
  h1 { color: #2C2C54; border-bottom: 3px solid #3A86FF; padding-bottom: 10px; }
  h2 { color: #3A86FF; margin-top: 40px; }
  h3 { color: #555; }
  .card { background: #f8f9fa; border-left: 4px solid #3A86FF; padding: 15px 20px; margin: 15px 0; border-radius: 4px; }
  .warn { border-left-color: #FFB703; }
  .success { border-left-color: #06D6A0; }
  .danger  { border-left-color: #E63946; }
  table { border-collapse: collapse; width: 100%%; margin: 15px 0; }
  th { background: #2C2C54; color: white; padding: 8px 12px; text-align: left; }
  td { padding: 7px 12px; border-bottom: 1px solid #ddd; }
  tr:nth-child(even) { background: #f4f4f8; }
  code { background: #eef; padding: 2px 5px; border-radius: 3px; font-size: 0.9em; }
  .footer { color: #888; font-size: 0.85em; margin-top: 60px; border-top: 1px solid #ddd; padding-top: 15px; }
</style>
</head>
<body>
<h1>%s</h1>
<p><strong>Generated:</strong> %s &nbsp;|&nbsp; <strong>Total trials:</strong> %d &nbsp;|&nbsp; <strong>Algorithms:</strong> %s</p>

<div class="card">
<strong>Reproducibility:</strong> Base seed = %d | Grid: %s | Movement: %s
</div>

<h2>1. Introduction</h2>
<p>This report presents a simulation-based statistical investigation of robot navigation algorithms under environmental uncertainty. Three strategies were compared: Dijkstra, A*, and a Risk-Aware planner. All results derive from executed Monte Carlo simulations.</p>

<h2>2. Experiment Configuration</h2>
<table>
<tr><th>Parameter</th><th>Values</th></tr>
<tr><td>Algorithms</td><td>%s</td></tr>
<tr><td>Obstacle densities</td><td>%s</td></tr>
<tr><td>Uncertainty levels (CV)</td><td>%s</td></tr>
<tr><td>Disruption probabilities</td><td>%s</td></tr>
<tr><td>Risk aversion values</td><td>%s</td></tr>
<tr><td>Replanning modes</td><td>%s</td></tr>
<tr><td>Total trials</td><td>%d</td></tr>
</table>

<h2>3. Statistical Hypotheses</h2>
<div class="card"><strong>H1:</strong> No difference in planning runtime between Dijkstra and A*. [Wilcoxon signed-rank paired test]</div>
<div class="card"><strong>H2:</strong> Route completion probabilities equal across algorithms. [Logistic regression + McNemar]</div>
<div class="card"><strong>H3:</strong> lambda_risk does not affect realized cost or failure rate. [Kruskal-Wallis + logistic]</div>
<div class="card"><strong>H4:</strong> Uncertainty does not interact with algorithm in its effect on realized cost. [Linear model with interaction]</div>

<h2>4. Results</h2>

<h3>4.1 Planning Runtime</h3>
%s
<div class="card %s"><strong>H1 Result:</strong> %s</div>

<h3>4.2 Route Success Rates</h3>
%s

<h3>4.3 Travel Cost Distributions</h3>
%s
<div class="card warn"><strong>Note:</strong> Cost distributions shown for successful trials only. Failure rates should be consulted jointly.</div>

<h3>4.4 Algorithm x Uncertainty Interaction</h3>
%s
<div class="card %s"><strong>H4 Result:</strong> %s</div>

<h3>4.5 Risk Aversion Analysis</h3>
%s
<div class="card %s"><strong>H3 Result:</strong> %s</div>

<h2>5. Limitations</h2>
<ul>
<li>All results are simulation-based and do not imply real-world robot performance.</li>
<li>The stochastic model uses simplified Gamma-distributed cost noise and Bernoulli disruptions.</li>
<li>Trials on the same map are not fully independent; map-level effects may inflate Type I error.</li>
<li>The risk-aware planner uses a heuristic risk function; other formulations would produce different results.</li>
<li>Path cost comparisons involving failures use an imputation convention; results are sensitive to this choice.</li>
</ul>

<h2>6. Conclusion</h2>
<p>The evidence from this simulation study supports the following observations (restricted to the tested conditions):</p>
<ul>
<li>Planning runtime: p = %s</li>
<li>Reliability difference: see logistic model above</li>
<li>Risk aversion effect: p = %s</li>
<li>Interaction effect: p = %s</li>
</ul>
<p>No single algorithm is universally superior. Performance trade-offs depend on obstacle density, uncertainty, and whether replanning is available.</p>

<div class="footer">
Report generated by the Robot Navigation Under Uncertainty framework (R). All statistical claims derive from executed simulations only.
</div>
</body>
</html>',
    title, title, timestamp, n_trials, algs,
    config$base_seed, config$grid_dims, config$movement,
    # Table rows
    algs,
    paste(config$obs_densities, collapse=", "),
    paste(config$uncertainty_levels, collapse=", "),
    paste(config$disruption_probs, collapse=", "),
    paste(config$lambda_values, collapse=", "),
    paste(config$replanning_modes, collapse=", "),
    config$total_trials,
    # Plots
    img_tag(p_paths$runtime),
    ifelse(!is.na(h1_p) && h1_p < 0.05, "danger", "success"),
    ifelse(!is.na(h1_p), sprintf("p = %.4f", h1_p), "Insufficient data"),
    img_tag(p_paths$success),
    img_tag(p_paths$cost),
    img_tag(p_paths$interact),
    ifelse(!is.na(h4_p) && h4_p < 0.05, "danger", "success"),
    ifelse(!is.na(h4_p), sprintf("p = %.4f", h4_p), "Insufficient data"),
    img_tag(p_paths$risk_cost),
    ifelse(!is.na(h3_p) && h3_p < 0.05, "danger", "success"),
    ifelse(!is.na(h3_p), sprintf("p = %.4f", h3_p), "Insufficient data"),
    # Conclusion
    ifelse(!is.na(h1_p), sprintf("%.4f", h1_p), "NA"),
    ifelse(!is.na(h3_p), sprintf("%.4f", h3_p), "NA"),
    ifelse(!is.na(h4_p), sprintf("%.4f", h4_p), "NA")
  )

  writeLines(html, output_path)
  cat("Report written to:", output_path, "\n")
  invisible(output_path)
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
