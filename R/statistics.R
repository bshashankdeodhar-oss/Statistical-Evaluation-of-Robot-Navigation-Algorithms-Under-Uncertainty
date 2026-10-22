# =============================================================================
# statistics.R -- Hypothesis Testing and Statistical Analysis
# =============================================================================
# Implements all four pre-registered hypotheses plus ancillary analyses.
# All tests are documented with assumption rationale.
# =============================================================================

library(dplyr)

# ---------------------------------------------------------------------------
# H1: Planning Efficiency -- Dijkstra vs A* runtime
# ---------------------------------------------------------------------------
#' Test H1: No difference in planning runtime between Dijkstra and A*.
#'
#' Method: Wilcoxon signed-rank test on paired trials (same map, same env_seed).
#' Rationale: Runtimes are non-negative and often right-skewed; normality
#' is unlikely. Pairing controls for map difficulty.
#'
#' @param results_df data.frame of trial results.
#' @return List with test result, effect size, and interpretation.
test_h1_planning_efficiency <- function(results_df) {
  library(dplyr)
  paired <- results_df %>%
    filter(algorithm %in% c("Dijkstra", "A*"), planning_success) %>%
    select(map_id, env_seed, algorithm, planning_time) %>%
    tidyr::pivot_wider(names_from = algorithm, values_from = planning_time,
                       values_fn = mean) %>%
    filter(!is.na(Dijkstra), !is.na(`A*`))

  if (nrow(paired) < 5L) {
    return(list(
      test = "Wilcoxon signed-rank (paired)",
      hypothesis = "H1: No difference in planning runtime between Dijkstra and A*",
      n = nrow(paired), p_value = NA_real_, statistic = NA_real_,
      effect_size = NA_real_, interpretation = "Insufficient paired observations.",
      paired_data = paired
    ))
  }

  wt <- wilcox.test(paired$Dijkstra, paired$`A*`, paired = TRUE,
                    alternative = "two.sided", exact = FALSE)

  # Effect size: matched-pairs rank-biserial correlation
  n    <- nrow(paired)
  r_rb <- 1 - (2 * wt$statistic) / (n * (n + 1) / 2)

  interp <- .interpret_test(wt$p.value, abs(r_rb), "r",
    h0 = "No difference in runtime between Dijkstra and A*",
    h1 = "Runtimes differ")

  list(
    test         = "Wilcoxon signed-rank (paired)",
    hypothesis   = "H1: No difference in planning runtime (Dijkstra vs A*)",
    n            = n,
    p_value      = wt$p.value,
    statistic    = wt$statistic,
    effect_size  = r_rb,
    effect_label = "rank-biserial r",
    ci           = NULL,
    interpretation = interp,
    paired_data  = paired
  )
}

# ---------------------------------------------------------------------------
# H2: Navigation Reliability -- success rate comparison
# ---------------------------------------------------------------------------
#' Test H2: Route completion probabilities are equal across algorithms.
#'
#' Method: Logistic regression with algorithm as predictor, controlling for
#' obs_density and uncertainty_level. This handles the multi-group case.
#' Also reports pairwise McNemar tests on matched pairs.
#'
#' @param results_df data.frame.
#' @return List with regression summary and pairwise tests.
test_h2_reliability <- function(results_df) {
  df <- results_df %>%
    mutate(
      success_int = as.integer(overall_success),
      algorithm   = relevel(factor(algorithm), ref = "Dijkstra")
    )

  if (length(unique(df$algorithm)) < 2L || nrow(df) < 20L) {
    return(list(hypothesis = "H2", error = "Insufficient data"))
  }

  model <- tryCatch(
    glm(success_int ~ algorithm + obs_density + uncertainty_level,
        data = df, family = binomial(link = "logit")),
    error = function(e) NULL
  )

  model_summary <- if (!is.null(model)) {
    s  <- summary(model)
    cf <- as.data.frame(coef(s))
    cf$variable <- rownames(cf)
    cf$OR <- exp(cf$Estimate)
    cf
  } else NULL

  # Pairwise McNemar on matched pairs
  algs   <- unique(df$algorithm)
  mcnem  <- list()
  pairs  <- combn(as.character(algs), 2, simplify = FALSE)
  for (pr in pairs) {
    wide <- df %>%
      filter(algorithm %in% pr) %>%
      select(map_id, env_seed, algorithm, success_int) %>%
      tidyr::pivot_wider(names_from = algorithm, values_from = success_int,
                         values_fn = function(x) x[1]) %>%
      filter(!is.na(.data[[pr[1]]]), !is.na(.data[[pr[2]]]))
    if (nrow(wide) >= 10L) {
      tab <- table(wide[[pr[1]]], wide[[pr[2]]])
      if (all(dim(tab) == 2)) {
        mn <- mcnemar.test(tab, correct = TRUE)
        mcnem[[paste(pr, collapse = " vs ")]] <- list(
          pair = pr, p_value = mn$p.value, statistic = mn$statistic, n = nrow(wide)
        )
      }
    }
  }

  list(
    hypothesis  = "H2: Route completion probabilities equal across algorithms",
    model       = model_summary,
    mcnemar     = mcnem,
    n           = nrow(df),
    interpretation = .glm_interpret(model, df)
  )
}

# ---------------------------------------------------------------------------
# H3: Risk-Aware Planning -- lambda_risk effect
# ---------------------------------------------------------------------------
#' Test H3: Changing lambda_risk does not affect realized cost or failure rate.
#'
#' Method:
#'   - Kruskal-Wallis test for realized cost across lambda levels.
#'   - Logistic regression for failure probability.
#' Restricted to Risk-Aware algorithm.
test_h3_risk_aversion <- function(results_df) {
  df <- results_df %>% filter(algorithm == "Risk-Aware")
  if (nrow(df) < 10L) return(list(hypothesis = "H3", error = "No Risk-Aware data"))

  # Realized cost
  cost_df <- df %>% filter(overall_success, !is.na(realized_cost))
  kw_cost <- if (nrow(cost_df) >= 10L && length(unique(cost_df$lambda_risk)) >= 2L) {
    tryCatch(kruskal.test(realized_cost ~ factor(lambda_risk), data = cost_df),
             error = function(e) NULL)
  } else NULL

  # Success rate by lambda
  lr_model <- tryCatch(
    glm(as.integer(overall_success) ~ lambda_risk + obs_density + uncertainty_level,
        data = df, family = binomial()),
    error = function(e) NULL
  )

  # Effect size: eta-squared from KW
  eta_sq <- if (!is.null(kw_cost)) {
    (kw_cost$statistic - length(unique(cost_df$lambda_risk)) + 1) /
    (nrow(cost_df) - length(unique(cost_df$lambda_risk)))
  } else NA_real_

  list(
    hypothesis       = "H3: lambda_risk does not affect realized cost or failure rate",
    kruskal_cost     = kw_cost,
    logistic_success = if (!is.null(lr_model)) as.data.frame(coef(summary(lr_model))) else NULL,
    eta_squared      = eta_sq,
    n                = nrow(df),
    n_cost           = nrow(cost_df),
    interpretation   = .kw_interpret(kw_cost, eta_sq, "lambda_risk effect on realized cost")
  )
}

# ---------------------------------------------------------------------------
# H4: Algorithm x Uncertainty Interaction
# ---------------------------------------------------------------------------
#' Test H4: Environmental uncertainty does not interact with algorithm choice.
#'
#' Method: Two-way mixed ANOVA / linear model with interaction term.
#' For non-normal outcomes, use rank-transformed response (aligned rank test).
#' Report interaction F-statistic and partial eta-squared.
test_h4_interaction <- function(results_df) {
  df <- results_df %>%
    filter(overall_success, !is.na(realized_cost)) %>%
    mutate(algorithm = factor(algorithm),
           unc_factor = factor(uncertainty_level))

  if (nrow(df) < 20L || length(unique(df$algorithm)) < 2L) {
    return(list(hypothesis = "H4", error = "Insufficient data"))
  }

  # Linear model with interaction
  lm_fit <- tryCatch(
    lm(realized_cost ~ algorithm * unc_factor + obs_density, data = df),
    error = function(e) NULL
  )
  lm_anova <- if (!is.null(lm_fit)) tryCatch(anova(lm_fit), error = function(e) NULL) else NULL

  # Rank-transform model for robustness
  df$rank_cost <- rank(df$realized_cost)
  lm_rank <- tryCatch(
    lm(rank_cost ~ algorithm * unc_factor + obs_density, data = df),
    error = function(e) NULL
  )
  lm_rank_anova <- if (!is.null(lm_rank)) tryCatch(anova(lm_rank), error = function(e) NULL) else NULL

  # Partial eta-squared for the interaction term
  p_eta_sq <- if (!is.null(lm_anova) && "algorithm:unc_factor" %in% rownames(lm_anova)) {
    ss_inter <- lm_anova["algorithm:unc_factor", "Sum Sq"]
    ss_resid <- lm_anova["Residuals", "Sum Sq"]
    ss_inter / (ss_inter + ss_resid)
  } else NA_real_

  interaction_p <- if (!is.null(lm_anova) && "algorithm:unc_factor" %in% rownames(lm_anova)) {
    lm_anova["algorithm:unc_factor", "Pr(>F)"]
  } else NA_real_

  list(
    hypothesis      = "H4: Uncertainty does not interact with algorithm on realized cost",
    lm_anova        = lm_anova,
    rank_anova      = lm_rank_anova,
    interaction_p   = interaction_p,
    partial_eta_sq  = p_eta_sq,
    n               = nrow(df),
    interpretation  = .interact_interpret(interaction_p, p_eta_sq)
  )
}

# ---------------------------------------------------------------------------
# Bootstrap CI for success rates
# ---------------------------------------------------------------------------
#' Compute bootstrap CIs for success rates by algorithm.
#'
#' @param results_df data.frame.
#' @param n_boot     Integer. Bootstrap replicates.
#' @param conf       Numeric. Confidence level.
#' @return data.frame with boot_lower and boot_upper columns.
bootstrap_success_ci <- function(results_df, n_boot = 1000L, conf = 0.95) {
  alpha <- 1 - conf
  results_df %>%
    group_by(algorithm, obs_density, uncertainty_level) %>%
    summarise(
      n = n(),
      obs_rate = mean(overall_success, na.rm = TRUE),
      boot_lower = {
        boots <- replicate(n_boot, mean(sample(overall_success, size = n(), replace = TRUE), na.rm = TRUE))
        quantile(boots, alpha / 2)
      },
      boot_upper = {
        boots <- replicate(n_boot, mean(sample(overall_success, size = n(), replace = TRUE), na.rm = TRUE))
        quantile(boots, 1 - alpha / 2)
      },
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# Descriptive statistics table
# ---------------------------------------------------------------------------
#' Generate a comprehensive descriptive statistics table.
descriptive_stats <- function(results_df) {
  results_df %>%
    group_by(algorithm, obs_density, uncertainty_level, disruption_prob) %>%
    summarise(
      n             = n(),
      success_rate  = mean(overall_success, na.rm = TRUE),
      mean_plan_t   = mean(planning_time, na.rm = TRUE),
      sd_plan_t     = sd(planning_time, na.rm = TRUE),
      median_plan_t = median(planning_time, na.rm = TRUE),
      iqr_plan_t    = IQR(planning_time, na.rm = TRUE),
      mean_nodes    = mean(nodes_expanded, na.rm = TRUE),
      sd_nodes      = sd(nodes_expanded, na.rm = TRUE),
      mean_nom_cost = mean(nominal_cost[planning_success], na.rm = TRUE),
      sd_nom_cost   = sd(nominal_cost[planning_success], na.rm = TRUE),
      mean_real_cost= mean(realized_cost[overall_success], na.rm = TRUE),
      sd_real_cost  = sd(realized_cost[overall_success], na.rm = TRUE),
      q25_real_cost = quantile(realized_cost[overall_success], 0.25, na.rm = TRUE),
      q75_real_cost = quantile(realized_cost[overall_success], 0.75, na.rm = TRUE),
      q95_real_cost = quantile(realized_cost[overall_success], 0.95, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# Auto-generate results summary
# ---------------------------------------------------------------------------
#' Generate a plain-language results summary from actual data.
generate_results_summary <- function(results_df, h1, h2, h3, h4) {
  alg_success <- results_df %>%
    group_by(algorithm) %>%
    summarise(sr = mean(overall_success, na.rm = TRUE),
              mean_t = mean(planning_time, na.rm = TRUE), .groups = "drop")

  best_success <- alg_success$algorithm[which.max(alg_success$sr)]
  worst_success<- alg_success$algorithm[which.min(alg_success$sr)]
  fastest      <- alg_success$algorithm[which.min(alg_success$mean_t)]

  h1_concl <- if (!is.null(h1$p_value) && !is.na(h1$p_value)) {
    if (h1$p_value < 0.05) {
      sprintf("H1 rejected (p=%.4f): Dijkstra and A* differ significantly in runtime.", h1$p_value)
    } else {
      sprintf("H1 not rejected (p=%.4f): No significant runtime difference detected.", h1$p_value)
    }
  } else "H1: Insufficient data."

  h3_concl <- if (!is.null(h3$kruskal_cost) && !is.na(h3$kruskal_cost$p.value)) {
    if (h3$kruskal_cost$p.value < 0.05) {
      sprintf("H3 rejected (p=%.4f): lambda_risk significantly affects realized cost.", h3$kruskal_cost$p.value)
    } else {
      sprintf("H3 not rejected (p=%.4f): lambda_risk did not significantly change realized cost.", h3$kruskal_cost$p.value)
    }
  } else "H3: Insufficient data."

  h4_concl <- if (!is.na(h4$interaction_p %||% NA)) {
    if (h4$interaction_p < 0.05) {
      sprintf("H4 rejected (p=%.4f): Significant algorithm x uncertainty interaction (partial eta^2 = %.3f).",
              h4$interaction_p, h4$partial_eta_sq %||% NA)
    } else {
      sprintf("H4 not rejected (p=%.4f): No significant interaction detected.", h4$interaction_p)
    }
  } else "H4: Insufficient data."

  paste(
    "=== AUTO-GENERATED RESULTS SUMMARY ===",
    sprintf("Total trials: %d | Algorithms: %s", nrow(results_df), paste(unique(results_df$algorithm), collapse=", ")),
    sprintf("Best success rate: %s (%.1f%%)", best_success, 100*max(alg_success$sr)),
    sprintf("Fastest planner:   %s", fastest),
    "",
    h1_concl, h3_concl, h4_concl,
    "",
    "NOTE: These conclusions are limited to the simulated conditions.",
    "They do not imply real-world robot performance.",
    sep = "\n"
  )
}

# ---------------------------------------------------------------------------
# Internal interpretation helpers
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

.interpret_test <- function(p, effect, effect_type, h0, h1) {
  sig   <- if (!is.na(p) && p < 0.05) "statistically significant" else "not statistically significant"
  mag   <- if (!is.na(effect)) {
    if (effect_type == "r") {
      if (abs(effect) < 0.1) "negligible" else if (abs(effect) < 0.3) "small" else if (abs(effect) < 0.5) "medium" else "large"
    } else "unclassified"
  } else "unknown"
  sprintf("Result is %s (p=%.4f). Effect size (%s=%.3f) is %s. %s",
          sig, p %||% NA, effect_type, effect %||% NA, mag,
          if (!is.na(p) && p < 0.05) paste("Evidence supports:", h1) else paste("Evidence supports:", h0))
}

.glm_interpret <- function(model, df) {
  if (is.null(model)) return("Model fitting failed.")
  cf <- coef(summary(model))
  alg_rows <- rownames(cf)[grepl("algorithm", rownames(cf))]
  if (length(alg_rows) == 0) return("No algorithm coefficients found.")
  parts <- vapply(alg_rows, function(r) {
    p  <- cf[r, "Pr(>|z|)"]
    OR <- exp(cf[r, "Estimate"])
    sprintf("%s: OR=%.2f, p=%.4f", gsub("algorithm","",r), OR, p)
  }, character(1))
  paste("Logistic regression (vs Dijkstra):", paste(parts, collapse="; "))
}

.kw_interpret <- function(kw, eta_sq, context) {
  if (is.null(kw)) return(paste("Kruskal-Wallis not computed for:", context))
  sig <- if (kw$p.value < 0.05) "significant" else "not significant"
  mag <- if (!is.na(eta_sq)) {
    if (eta_sq < 0.01) "small" else if (eta_sq < 0.06) "medium" else "large"
  } else "unknown"
  sprintf("Kruskal-Wallis: H=%.3f, p=%.4f (%s). Eta-squared=%.3f (%s effect).",
          kw$statistic, kw$p.value, sig, eta_sq %||% NA, mag)
}

.interact_interpret <- function(p, eta_sq) {
  if (is.na(p %||% NA)) return("Interaction test not computed.")
  sig <- if (p < 0.05) "significant" else "not significant"
  mag <- if (!is.na(eta_sq)) {
    if (eta_sq < 0.01) "small" else if (eta_sq < 0.06) "medium" else "large"
  } else "unknown"
  sprintf("Interaction is %s (p=%.4f). Partial eta-squared=%.3f (%s effect).",
          sig, p, eta_sq %||% NA, mag)
}
