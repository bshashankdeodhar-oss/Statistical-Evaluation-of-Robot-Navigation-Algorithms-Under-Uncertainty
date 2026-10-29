# =============================================================================
# app.R -- Shiny Dashboard: Beyond the Shortest Path
# =============================================================================
# Run with: shiny::runApp(".", port = 3838)
# =============================================================================

userLib <- "C:/Users/bshas/R/win-library/4.6"
if (dir.exists(userLib)) .libPaths(c(userLib, .libPaths()))

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinycssloaders)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(scales)
  library(gridExtra)
})

# Source all modules
for (f in c("grid_world","algorithms","uncertainty","simulation",
            "experiments","metrics","statistics","visualizations","report")) {
  source(file.path("R", paste0(f, ".R")), local = FALSE)
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(title = span(icon("robot"), "Robot Nav — Statistical Research")),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Setup",          tabName = "setup",      icon = icon("sliders")),
      menuItem("Live Planning",  tabName = "live",       icon = icon("map")),
      menuItem("Performance",    tabName = "perf",       icon = icon("chart-bar")),
      menuItem("Statistics",     tabName = "stats",      icon = icon("flask")),
      menuItem("Risk Analysis",  tabName = "risk",       icon = icon("shield-alt")),
      menuItem("Report & Export",tabName = "report_tab", icon = icon("file-alt"))
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-midnight .main-header .logo { background:#2C2C54!important; }
      .skin-midnight .main-header .navbar { background:#2C2C54!important; }
      .skin-midnight .main-sidebar { background:#1a1a3e!important; }
      .skin-midnight .sidebar-menu > li.active > a { background:#3A86FF!important; }
      .content-wrapper { background:#f4f6fb!important; }
      .box { border-top: 3px solid #3A86FF!important; }
      .box.box-warning { border-top-color: #FFB703!important; }
      .box.box-success { border-top-color: #06D6A0!important; }
      .box.box-danger  { border-top-color: #E63946!important; }
      h4.box-title { font-weight:bold; }
      .value-box { border-radius:8px; }
      .info-box { border-radius:8px; min-height:80px; }
    "))),

    tabItems(

      # ------------------------------------------------------------------
      # Page 1: Setup
      # ------------------------------------------------------------------
      tabItem(tabName = "setup",
        fluidRow(
          box(title = "Grid Configuration", status = "primary", solidHeader = TRUE,
              width = 4,
              sliderInput("grid_width",  "Grid width",  min=10, max=60, value=20, step=5),
              sliderInput("grid_height", "Grid height", min=10, max=60, value=20, step=5),
              sliderInput("obs_density", "Obstacle density",
                          min=5, max=50, value=20, step=5,
                          post="%"),
              radioButtons("movement", "Movement mode",
                           choices=c("Four-directional"="four","Eight-directional"="eight"),
                           selected="four")
          ),
          box(title = "Uncertainty Parameters", status = "warning", solidHeader = TRUE,
              width = 4,
              sliderInput("uncertainty_level","Edge-cost uncertainty (CV)",
                          min=0, max=0.5, value=0.1, step=0.05),
              sliderInput("disruption_prob","Edge-blocking probability",
                          min=0, max=0.4, value=0.05, step=0.01),
              sliderInput("sensor_error","Sensor error rate",
                          min=0, max=0.2, value=0.05, step=0.01),
              sliderInput("lambda_risk","Risk aversion (lambda)",
                          min=0, max=5, value=1.0, step=0.25)
          ),
          box(title = "Experiment Settings", status = "success", solidHeader = TRUE,
              width = 4,
              numericInput("n_trials","Monte Carlo trials per cell", value=20, min=5, max=500),
              numericInput("n_maps",  "Maps per condition", value=3, min=1, max=20),
              numericInput("base_seed","Random seed", value=42, min=1, max=99999),
              checkboxInput("replanning","Enable replanning", value=FALSE),
              checkboxGroupInput("algorithms","Algorithms",
                choices=c("Dijkstra","A*","Risk-Aware"),
                selected=c("Dijkstra","A*","Risk-Aware"))
          )
        ),
        fluidRow(
          box(width=12, title="Actions", status="primary",
              fluidRow(
                column(3, actionButton("btn_gen_map","Generate Map",
                                       icon=icon("map"), class="btn-primary btn-lg btn-block")),
                column(3, actionButton("btn_run_single","Run Single Sim",
                                       icon=icon("play"), class="btn-info btn-lg btn-block")),
                column(3, actionButton("btn_run_mc","Run Monte Carlo",
                                       icon=icon("cogs"), class="btn-warning btn-lg btn-block")),
                column(3, actionButton("btn_reset","Reset",
                                       icon=icon("undo"), class="btn-danger btn-lg btn-block"))
              ),
              br(),
              verbatimTextOutput("setup_status")
          )
        )
      ),

      # ------------------------------------------------------------------
      # Page 2: Live Path Planning
      # ------------------------------------------------------------------
      tabItem(tabName = "live",
        fluidRow(
          valueBoxOutput("vb_path_cost",  width=3),
          valueBoxOutput("vb_path_len",   width=3),
          valueBoxOutput("vb_nodes_exp",  width=3),
          valueBoxOutput("vb_plan_time",  width=3)
        ),
        fluidRow(
          box(title="Grid Map & Paths", status="primary", solidHeader=TRUE,
              width=8,
              withSpinner(plotOutput("grid_plot", height="500px"), type=6, color="#3A86FF")),
          box(title="Algorithm Selection", status="primary", solidHeader=TRUE,
              width=4,
              radioButtons("viz_algorithm","Display algorithm",
                           choices=c("All","Dijkstra","A*","Risk-Aware"), selected="All"),
              hr(),
              h4("Path Details"),
              uiOutput("path_detail_ui"),
              hr(),
              verbatimTextOutput("live_status")
          )
        )
      ),

      # ------------------------------------------------------------------
      # Page 3: Performance Comparison
      # ------------------------------------------------------------------
      tabItem(tabName = "perf",
        fluidRow(
          box(title="Runtime Comparison", status="primary", solidHeader=TRUE,
              width=6,
              withSpinner(plotOutput("plot_runtime", height="350px"), type=6)),
          box(title="Success Rates", status="success", solidHeader=TRUE,
              width=6,
              withSpinner(plotOutput("plot_success", height="350px"), type=6))
        ),
        fluidRow(
          box(title="Travel Cost Distributions", status="warning", solidHeader=TRUE,
              width=6,
              withSpinner(plotOutput("plot_cost", height="350px"), type=6)),
          box(title="Nodes Expanded", status="primary", solidHeader=TRUE,
              width=6,
              withSpinner(plotOutput("plot_nodes", height="350px"), type=6))
        ),
        fluidRow(
          box(title="Performance vs Uncertainty", status="warning", solidHeader=TRUE,
              width=12,
              withSpinner(plotOutput("plot_unc", height="350px"), type=6))
        ),
        fluidRow(
          box(title="Filter by Condition", status="primary", solidHeader=TRUE, width=12,
              fluidRow(
                column(4, sliderInput("filter_density","Obstacle density",
                                      min=0, max=0.5, value=c(0,0.5), step=0.05)),
                column(4, sliderInput("filter_unc","Uncertainty level",
                                      min=0, max=0.5, value=c(0,0.5), step=0.05)),
                column(4, checkboxInput("filter_replanning","Show replanning only", FALSE))
              ))
        )
      ),

      # ------------------------------------------------------------------
      # Page 4: Statistical Analysis
      # ------------------------------------------------------------------
      tabItem(tabName = "stats",
        fluidRow(
          box(title="Descriptive Statistics", status="primary", solidHeader=TRUE,
              width=12,
              withSpinner(DTOutput("desc_stats_table"), type=6))
        ),
        fluidRow(
          box(title="H1: Planning Efficiency (Dijkstra vs A*)", status="primary",
              solidHeader=TRUE, width=6,
              uiOutput("h1_result")),
          box(title="H2: Route Reliability", status="success",
              solidHeader=TRUE, width=6,
              uiOutput("h2_result"))
        ),
        fluidRow(
          box(title="H3: Risk Aversion Effect", status="warning",
              solidHeader=TRUE, width=6,
              uiOutput("h3_result")),
          box(title="H4: Algorithm x Uncertainty Interaction", status="danger",
              solidHeader=TRUE, width=6,
              uiOutput("h4_result"))
        ),
        fluidRow(
          box(title="Interaction Plot", status="primary", solidHeader=TRUE, width=12,
              withSpinner(plotOutput("plot_interaction", height="350px"), type=6))
        ),
        fluidRow(
          box(title="Auto-Generated Summary", status="primary", solidHeader=TRUE, width=12,
              verbatimTextOutput("results_summary"))
        )
      ),

      # ------------------------------------------------------------------
      # Page 5: Risk & Sensitivity
      # ------------------------------------------------------------------
      tabItem(tabName = "risk",
        fluidRow(
          box(title="Risk Aversion vs Realized Cost", status="warning",
              solidHeader=TRUE, width=6,
              withSpinner(plotOutput("plot_risk_cost", height="350px"), type=6)),
          box(title="Risk Aversion vs Success Rate", status="success",
              solidHeader=TRUE, width=6,
              withSpinner(plotOutput("plot_risk_success", height="350px"), type=6))
        ),
        fluidRow(
          box(title="Detour Ratio vs Dijkstra Baseline", status="primary",
              solidHeader=TRUE, width=6,
              withSpinner(plotOutput("plot_detour", height="350px"), type=6)),
          box(title="Sensitivity: Cost Imputation", status="danger",
              solidHeader=TRUE, width=6,
              sliderInput("failure_cost_mult","Failure cost multiplier",
                          min=1, max=10, value=3, step=0.5),
              verbatimTextOutput("sensitivity_note"),
              withSpinner(plotOutput("plot_sensitivity", height="280px"), type=6))
        )
      ),

      # ------------------------------------------------------------------
      # Page 6: Report & Export
      # ------------------------------------------------------------------
      tabItem(tabName = "report_tab",
        fluidRow(
          box(title="Experiment Summary", status="primary", solidHeader=TRUE, width=12,
              verbatimTextOutput("exp_config_summary"))
        ),
        fluidRow(
          box(title="Download Results", status="success", solidHeader=TRUE, width=6,
              downloadButton("dl_results_csv", "Download Trial CSV", class="btn-success btn-block"),
              br(),
              downloadButton("dl_summary_csv", "Download Summary CSV", class="btn-info btn-block"),
              br(),
              downloadButton("dl_report_html", "Download HTML Report", class="btn-primary btn-block")),
          box(title="Reproducibility Record", status="primary", solidHeader=TRUE, width=6,
              verbatimTextOutput("repro_record"))
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # Pre-load latest results and default map for immediate interactivity
  csv_files <- sort(list.files("results", pattern = "^experiment_results_.*\\.csv$", full.names = TRUE), decreasing = TRUE)
  init_mc <- if (length(csv_files) > 0) {
    tryCatch({
      df <- read.csv(csv_files[1])
      if (nrow(df) > 0) df else NULL
    }, error = function(e) NULL)
  } else NULL

  init_summary <- if (!is.null(init_mc)) tryCatch(compute_summary_metrics(init_mc), error = function(e) NULL) else NULL
  init_h1 <- if (!is.null(init_mc)) tryCatch(test_h1_planning_efficiency(init_mc), error = function(e) NULL) else NULL
  init_h2 <- if (!is.null(init_mc)) tryCatch(test_h2_reliability(init_mc), error = function(e) NULL) else NULL
  init_h3 <- if (!is.null(init_mc)) tryCatch(test_h3_risk_aversion(init_mc), error = function(e) NULL) else NULL
  init_h4 <- if (!is.null(init_mc)) tryCatch(test_h4_interaction(init_mc), error = function(e) NULL) else NULL

  init_grid <- tryCatch(create_grid(width = 20, height = 20, obs_density = 0.20, movement = "four", seed = 42), error = function(e) NULL)
  init_plans <- list()
  if (!is.null(init_grid) && isTRUE(init_grid$reachable)) {
    unc_default <- create_uncertainty_config(uncertainty_level = 0.1, disruption_prob = 0.05, sensor_error_prob = 0.05)
    for (alg in c("Dijkstra", "A*", "Risk-Aware")) {
      init_plans[[alg]] <- tryCatch(
        run_single_trial(init_grid, algorithm = alg, unc_config = unc_default, lambda_risk = 1.0, replanning = FALSE, env_seed = 43L, trial_id = 1L, map_id = 1L),
        error = function(e) list(overall_success = FALSE, failure_reason = e$message)
      )
    }
  }

  # Reactive state
  rv <- reactiveValues(
    grid_obj    = init_grid,
    plan_results= init_plans,
    mc_results  = init_mc,
    summary_df  = init_summary,
    stats_h1    = init_h1,
    stats_h2    = init_h2,
    stats_h3    = init_h3,
    stats_h4    = init_h4,
    status_msg  = if (!is.null(init_mc)) sprintf("Initialized with benchmark dataset (%d trials). Ready.", nrow(init_mc)) else "Ready. Click 'Generate Map' to begin.",
    mc_running  = FALSE
  )

  # -- Generate map
  observeEvent(input$btn_gen_map, {
    withProgress(message="Generating map...", {
      rv$grid_obj <- create_grid(
        width       = input$grid_width,
        height      = input$grid_height,
        obs_density = input$obs_density / 100,
        movement    = input$movement,
        seed        = input$base_seed
      )
      rv$plan_results <- list()
      rv$status_msg <- if (rv$grid_obj$reachable) {
        sprintf("Map generated (%dx%d, %.0f%% obstacles). Start: [%d,%d] Goal: [%d,%d]",
                input$grid_width, input$grid_height, input$obs_density,
                rv$grid_obj$start[1], rv$grid_obj$start[2],
                rv$grid_obj$goal[1],  rv$grid_obj$goal[2])
      } else {
        "WARNING: No path exists between start and goal on this map."
      }
    })
  })

  # -- Run single simulation
  observeEvent(input$btn_run_single, {
    req(rv$grid_obj)
    withProgress(message="Planning paths...", {
      algs <- input$algorithms
      unc  <- create_uncertainty_config(
        uncertainty_level = input$uncertainty_level,
        disruption_prob   = input$disruption_prob,
        sensor_error_prob = input$sensor_error
      )
      results <- list()
      for (alg in algs) {
        setProgress(detail = paste("Running", alg))
        r <- tryCatch(
          run_single_trial(rv$grid_obj, algorithm=alg, unc_config=unc,
                           lambda_risk=input$lambda_risk,
                           replanning=input$replanning,
                           env_seed=input$base_seed+1L, trial_id=1L, map_id=1L),
          error=function(e) list(overall_success=FALSE, failure_reason=e$message)
        )
        results[[alg]] <- r
      }
      rv$plan_results <- results
      succeeded <- vapply(algs, function(a) isTRUE(results[[a]]$overall_success), logical(1))
      rv$status_msg <- paste(
        vapply(algs, function(a) sprintf("%s: %s", a,
                                         ifelse(succeeded[a], "SUCCESS", "FAILED")),
               character(1)), collapse=" | ")
    })
  })

  # -- Run Monte Carlo experiment
  observeEvent(input$btn_run_mc, {
    req(!rv$mc_running)
    rv$mc_running <- TRUE
    withProgress(message="Running Monte Carlo...", value=0, {
      prog_fn <- function(cur, tot, msg) {
        setProgress(value=cur/tot, detail=msg)
      }
      tryCatch({
        rv$mc_results <- run_quick_experiment(
          n_trials     = input$n_trials,
          n_maps       = input$n_maps,
          algorithms   = input$algorithms,
          obs_densities= c(0.10, 0.20, 0.30, 0.40),
          uncertainty_levels = c(0.05, 0.20),
          disruption_probs   = c(input$disruption_prob),
          lambda_values      = c(input$lambda_risk),
          replanning_modes   = c(input$replanning),
          grid_width   = input$grid_width,
          grid_height  = input$grid_height,
          movement     = input$movement,
          base_seed    = input$base_seed,
          output_dir   = "results",
          progress_fn  = prog_fn
        )
        rv$summary_df <- compute_summary_metrics(rv$mc_results)
        # Run stats
        rv$stats_h1 <- tryCatch(test_h1_planning_efficiency(rv$mc_results), error=function(e) list(error=e$message))
        rv$stats_h2 <- tryCatch(test_h2_reliability(rv$mc_results),        error=function(e) list(error=e$message))
        rv$stats_h3 <- tryCatch(test_h3_risk_aversion(rv$mc_results),      error=function(e) list(error=e$message))
        rv$stats_h4 <- tryCatch(test_h4_interaction(rv$mc_results),        error=function(e) list(error=e$message))
        rv$status_msg <- sprintf("Monte Carlo complete. %d trials.", nrow(rv$mc_results))
      }, error=function(e) {
        rv$status_msg <- paste("MC Error:", e$message)
      })
    })
    rv$mc_running <- FALSE
  })

  # -- Reset
  observeEvent(input$btn_reset, {
    rv$grid_obj     <- NULL
    rv$plan_results <- list()
    rv$mc_results   <- NULL
    rv$summary_df   <- NULL
    rv$stats_h1 <- rv$stats_h2 <- rv$stats_h3 <- rv$stats_h4 <- NULL
    rv$status_msg   <- "Reset. Click Generate Map to start."
  })

  # ---- Outputs ----------------------------------------------------------------

  output$setup_status <- renderPrint({ cat(rv$status_msg) })
  output$live_status  <- renderPrint({
    if (length(rv$plan_results) == 0) { cat("No simulation run yet."); return() }
    for (alg in names(rv$plan_results)) {
      r <- rv$plan_results[[alg]]
      cat(sprintf("%s: success=%s | nominal=%.2f | nodes=%d | time=%.4fs\n",
                  alg, r$overall_success %||% "?",
                  r$nominal_cost %||% NA, r$nodes_expanded %||% 0L,
                  r$planning_time %||% 0.0))
    }
  })

  # Grid plot
  output$grid_plot <- renderPlot({
    req(rv$grid_obj)
    alg_filter <- input$viz_algorithm
    paths <- list()
    for (alg in names(rv$plan_results)) {
      if (alg_filter != "All" && alg != alg_filter) next
      r <- rv$plan_results[[alg]]
      if (isTRUE(r$planning_success) && length(r$path) > 0) {
        paths[[alg]] <- r$path
      }
    }
    plot_grid(rv$grid_obj, paths = paths,
              title = sprintf("Grid World (%dx%d, %.0f%% obstacles)",
                              rv$grid_obj$width, rv$grid_obj$height,
                              rv$grid_obj$obs_density * 100))
  })

  # Value boxes
  output$vb_path_cost <- renderValueBox({
    r <- rv$plan_results[[input$viz_algorithm]]
    val <- if (!is.null(r) && isTRUE(r$overall_success))
             round(r$realized_cost %||% r$nominal_cost %||% 0, 2) else "—"
    valueBox(val, "Realized Cost", icon=icon("route"), color="blue")
  })
  output$vb_path_len <- renderValueBox({
    r <- rv$plan_results[[input$viz_algorithm]]
    val <- if (!is.null(r)) r$path_length %||% "—" else "—"
    valueBox(val, "Path Length", icon=icon("arrows-alt-h"), color="green")
  })
  output$vb_nodes_exp <- renderValueBox({
    r <- rv$plan_results[[input$viz_algorithm]]
    val <- if (!is.null(r)) r$nodes_expanded %||% "—" else "—"
    valueBox(val, "Nodes Expanded", icon=icon("project-diagram"), color="purple")
  })
  output$vb_plan_time <- renderValueBox({
    r <- rv$plan_results[[input$viz_algorithm]]
    val <- if (!is.null(r)) sprintf("%.4f s", r$planning_time %||% 0) else "—"
    valueBox(val, "Planning Time", icon=icon("clock"), color="yellow")
  })

  output$path_detail_ui <- renderUI({
    if (length(rv$plan_results) == 0) return(p("Run a simulation to see details."))
    tags$ul(lapply(names(rv$plan_results), function(alg) {
      r <- rv$plan_results[[alg]]
      status_col <- if (isTRUE(r$overall_success)) "green" else "red"
      tags$li(
        tags$strong(alg), ": ",
        tags$span(style=paste0("color:", status_col),
                  ifelse(isTRUE(r$overall_success),"✓ Success","✗ Failed")),
        if (isTRUE(r$overall_success))
          sprintf(" | cost=%.2f | len=%d", r$nominal_cost %||% 0, r$path_length %||% 0)
        else
          sprintf(" | %s", r$failure_reason %||% "unknown")
      )
    }))
  })

  # -- Performance tab filtered data
  filtered_mc <- reactive({
    req(rv$mc_results)
    df <- rv$mc_results
    if (!is.null(input$filter_density))
      df <- df %>% filter(obs_density >= input$filter_density[1],
                          obs_density <= input$filter_density[2])
    if (!is.null(input$filter_unc))
      df <- df %>% filter(uncertainty_level >= input$filter_unc[1],
                          uncertainty_level <= input$filter_unc[2])
    if (isTRUE(input$filter_replanning))
      df <- df %>% filter(replanning == TRUE)
    df
  })

  output$plot_runtime  <- renderPlot({ req(filtered_mc()); plot_runtime_boxplot(filtered_mc()) })
  output$plot_success  <- renderPlot({ req(filtered_mc()); plot_success_rates(filtered_mc()) })
  output$plot_cost     <- renderPlot({ req(filtered_mc()); plot_cost_distributions(filtered_mc()) })
  output$plot_nodes    <- renderPlot({ req(filtered_mc()); plot_nodes_expanded(filtered_mc()) })
  output$plot_unc      <- renderPlot({ req(filtered_mc()); plot_perf_vs_uncertainty(filtered_mc()) })
  output$plot_interaction <- renderPlot({ req(rv$mc_results); plot_interaction(rv$mc_results) })
  output$plot_risk_cost   <- renderPlot({ req(rv$mc_results); plot_risk_vs_cost(rv$mc_results) })
  output$plot_risk_success<- renderPlot({ req(rv$mc_results); plot_risk_vs_success(rv$mc_results) })
  output$plot_detour   <- renderPlot({ req(rv$mc_results); plot_detour_ratio(rv$mc_results) })

  # Descriptive stats table
  output$desc_stats_table <- renderDT({
    req(rv$mc_results)
    df <- descriptive_stats(rv$mc_results) %>% mutate(across(where(is.numeric), ~round(.x, 4)))
    datatable(df, options=list(scrollX=TRUE, pageLength=10), rownames=FALSE)
  })

  # H1-H4 result cards
  output$h1_result <- renderUI({
    req(rv$stats_h1)
    h1 <- rv$stats_h1
    if (!is.null(h1$error)) return(p(paste("Error:", h1$error)))
    tags$div(
      p(strong("Test: "), h1$test),
      p(strong("N pairs: "), h1$n),
      p(strong("p-value: "), sprintf("%.4f", h1$p_value %||% NA)),
      p(strong("Effect (rank-biserial r): "), sprintf("%.3f", h1$effect_size %||% NA)),
      hr(),
      p(em(h1$interpretation %||% ""))
    )
  })
  output$h2_result <- renderUI({
    req(rv$stats_h2)
    h2 <- rv$stats_h2
    if (!is.null(h2$error)) return(p(paste("Error:", h2$error)))
    tags$div(
      p(strong("N: "), h2$n),
      p(strong("Method: "), "Logistic regression + McNemar"),
      p(em(h2$interpretation %||% "See model output below")),
      if (!is.null(h2$model)) renderTable(h2$model %>% select(variable,Estimate,`Std. Error`,`z value`,`Pr(>|z|)`,OR) %>% mutate(across(where(is.numeric), ~round(.x,4))))
    )
  })
  output$h3_result <- renderUI({
    req(rv$stats_h3)
    h3 <- rv$stats_h3
    if (!is.null(h3$error)) return(p(paste("Error:", h3$error)))
    kw <- h3$kruskal_cost
    tags$div(
      p(strong("N (Risk-Aware): "), h3$n),
      if (!is.null(kw)) p(strong("Kruskal-Wallis H: "), sprintf("%.3f, p=%.4f", kw$statistic, kw$p.value)),
      p(strong("Eta-squared: "), sprintf("%.3f", h3$eta_squared %||% NA)),
      hr(),
      p(em(h3$interpretation %||% ""))
    )
  })
  output$h4_result <- renderUI({
    req(rv$stats_h4)
    h4 <- rv$stats_h4
    if (!is.null(h4$error)) return(p(paste("Error:", h4$error)))
    tags$div(
      p(strong("N: "), h4$n),
      p(strong("Interaction p: "), sprintf("%.4f", h4$interaction_p %||% NA)),
      p(strong("Partial eta^2: "), sprintf("%.3f", h4$partial_eta_sq %||% NA)),
      hr(),
      p(em(h4$interpretation %||% ""))
    )
  })

  output$results_summary <- renderPrint({
    req(rv$mc_results, rv$stats_h1, rv$stats_h2, rv$stats_h3, rv$stats_h4)
    cat(generate_results_summary(rv$mc_results,
                                 rv$stats_h1, rv$stats_h2, rv$stats_h3, rv$stats_h4))
  })

  # Sensitivity plot
  output$sensitivity_note <- renderPrint({
    cat(sprintf("Failed trials imputed as %.1f x max observed cost\n", input$failure_cost_mult))
    cat("(Sensitivity: varying this multiplier shows how dependent conclusions are on imputation choice)")
  })
  output$plot_sensitivity <- renderPlot({
    req(rv$mc_results)
    df <- rv$mc_results
    max_obs <- max(df$realized_cost, na.rm=TRUE)
    fc <- max_obs * input$failure_cost_mult
    summ <- df %>%
      mutate(cost_imp = ifelse(is.na(realized_cost) | !overall_success, fc, realized_cost)) %>%
      group_by(algorithm, obs_density) %>%
      summarise(mean_cost_imp = mean(cost_imp), .groups="drop")
    ggplot(summ, aes(x=obs_density, y=mean_cost_imp, colour=algorithm, group=algorithm)) +
      geom_line(linewidth=1.2) + geom_point(size=3) +
      scale_colour_manual(values=ALGO_COLOURS) +
      scale_x_continuous(labels=percent) +
      labs(title=sprintf("Imputed Mean Cost (multiplier=%.1f)", input$failure_cost_mult),
           x="Obstacle density", y="Mean cost (failures imputed)") +
      THEME_ROBOT()
  })

  output$exp_config_summary <- renderPrint({
    req(rv$mc_results)
    cfg <- list(
      total_trials=nrow(rv$mc_results), base_seed=input$base_seed,
      grid_dims=paste0(input$grid_width,"x",input$grid_height),
      movement=input$movement,
      obs_densities=c(0.10,0.20,0.30,0.40),
      uncertainty_levels=c(0.05,0.20),
      disruption_probs=input$disruption_prob,
      lambda_values=input$lambda_risk,
      replanning_modes=input$replanning
    )
    cat("=== EXPERIMENT CONFIGURATION ===\n")
    for (nm in names(cfg)) cat(sprintf("  %-22s %s\n", nm, paste(cfg[[nm]], collapse=", ")))
  })

  output$repro_record <- renderPrint({
    cat(sprintf("Seed: %d\nTimestamp: %s\nR version: %s\n",
                input$base_seed, format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                R.version.string))
  })

  # Downloads
  output$dl_results_csv <- downloadHandler(
    filename = function() paste0("nav_results_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      req(rv$mc_results); write.csv(rv$mc_results, file, row.names=FALSE)
    }
  )
  output$dl_summary_csv <- downloadHandler(
    filename = function() paste0("nav_summary_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      req(rv$summary_df); write.csv(rv$summary_df, file, row.names=FALSE)
    }
  )
  output$dl_report_html <- downloadHandler(
    filename = function() paste0("nav_report_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".html"),
    content  = function(file) {
      req(rv$mc_results)
      tmpReport <- tempfile(fileext=".html")
      generate_html_report(
        rv$mc_results, rv$summary_df %||% data.frame(),
        list(h1=rv$stats_h1, h2=rv$stats_h2, h3=rv$stats_h3, h4=rv$stats_h4),
        list(base_seed=input$base_seed,
             grid_dims=paste0(input$grid_width,"x",input$grid_height),
             movement=input$movement,
             obs_densities=c(0.10,0.20,0.30,0.40),
             uncertainty_levels=c(0.05,0.20),
             disruption_probs=input$disruption_prob,
             lambda_values=input$lambda_risk,
             replanning_modes=input$replanning,
             total_trials=nrow(rv$mc_results)),
        output_path=tmpReport
      )
      file.copy(tmpReport, file)
    }
  )
}

shinyApp(ui, server)
