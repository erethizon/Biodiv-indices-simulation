# app.R
# Shiny app: teaching diversity indices and sampling with bead models
# Option C: Manual bead bag AND model-generated communities
#
# Author: (generated for you)
# Date: 2025-12-11
#
# Required packages:
# install.packages(c("shiny","tidyverse","DT"))
#
library(shiny)
library(tidyverse)
library(DT)

# -------------------------
# Helper functions
# -------------------------

# 1) Generate community abundance vector ------------------------------------------------
# This creates a named vector of integer counts summing to N for S species,
# according to different distribution shapes: "even", "dominant", "logseries".
generate_community <- function(S = 9, N = 100, distribution = c("even", "dominant", "logseries"),
                               dominance_fraction = 0.5, # for "dominant" option: fraction in the top species
                               logseries_alpha = 5, # tuning param for logseries-like shape
                               seed = NULL) {
  distribution <- match.arg(distribution)
  if (!is.null(seed)) set.seed(seed)
  if (distribution == "even") {
    abund <- rep(floor(N / S), S)
    remainder <- N - sum(abund)
    if (remainder > 0) abund[1:remainder] <- abund[1:remainder] + 1
  } else if (distribution == "dominant") {
    # Put a large chunk into species 1, remainder distributed across others via exponential
    common <- round(dominance_fraction * N)
    remaining <- N - common
    if (S == 1) {
      abund <- common
    } else {
      rare <- rexp(S - 1, rate = 1) # relative weights
      rare <- round(rare / sum(rare) * remaining)
      # adjust rounding
      diff <- remaining - sum(rare)
      if (diff > 0) rare[1:diff] <- rare[1:diff] + 1
      if (diff < 0) {
        # remove extras
        idx <- which(rare > 0)
        for (i in seq_len(-diff)) {
          rare[idx[((i - 1) %% length(idx)) + 1]] <- rare[idx[((i - 1) %% length(idx)) + 1]] - 1
        }
      }
      abund <- c(common, rare)
    }
  } else if (distribution == "logseries") {
    # rough logseries-like : use decreasing geometric weights
    k <- seq_len(S)
    weights <- k^(-1.2) # tune exponent for desired tail heaviness
    abund <- round(weights / sum(weights) * N)
    diff <- N - sum(abund)
    if (diff > 0) abund[1:diff] <- abund[1:diff] + 1
    if (diff < 0) {
      # subtract extras
      idx <- which(abund > 0)
      for (i in seq_len(-diff)) {
        abund[idx[((i - 1) %% length(idx)) + 1]] <- abund[idx[((i - 1) %% length(idx)) + 1]] - 1
      }
    }
  }
  # ensure no zeros (since you have 9 colors typically, but allow zeros if user sets)
  names(abund) <- paste0("sp", seq_along(abund))
  return(as.integer(abund))
}

# 2) Sampling the community --------------------------------------------------------------
# Simulate drawing sample_size beads without replacement from the true community.
sample_community_once <- function(true_abund, sample_size = 25, replace = FALSE, seed = NULL) {
  # true_abund: named integer vector, counts in the full community
  if (!is.null(seed)) set.seed(seed)
  S <- length(true_abund)
  N <- sum(true_abund)
  # if sample_size > N and replace == FALSE, set sample_size = N
  if (!replace && sample_size > N) sample_size <- N
  # replicate species names by counts (the "bag")
  species_vector <- rep(names(true_abund), times = true_abund)
  draw <- sample(species_vector, size = sample_size, replace = replace)
  tab <- table(factor(draw, levels = names(true_abund)))
  as.integer(tab) # return integer vector aligned with names(true_abund)
}

# 3) Diversity index computations --------------------------------------------------------
compute_indices_from_counts <- function(counts) {
  # counts: integer vector (one sample or true community)
  counts <- as.numeric(counts)
  # species with zero count are not counted in richness, but kept for p computation
  present <- counts > 0
  richness <- sum(present)
  N <- sum(counts)
  if (N == 0) {
    shannon <- NA_real_
    simpson <- NA_real_
  } else {
    p <- counts[counts > 0] / N  # only positive counts used in entropy calculation
    shannon <- -sum(p * log(p))
    simpson <- 1 - sum(p^2) # Simpson's diversity (Gini-Simpson)
  }
  tibble(richness = richness, shannon = shannon, simpson = simpson)
}

# 4) Simulation wrapper -----------------------------------------------------------------
# Run many replicate samples for one or multiple sample sizes and compute summary stats
simulate_sampling <- function(true_abund,
                              sample_sizes = c(25),
                              nrep = 200,
                              replace = FALSE,
                              seed = NULL) {
  # returns a tidy data.frame with per-replicate index estimates
  if (!is.null(seed)) set.seed(seed)
  N <- sum(true_abund)
  results <- list()
  for (ss in sample_sizes) {
    # for each replicate, sample and compute indices
    reps <- lapply(seq_len(nrep), function(i) {
      samp_counts <- sample_community_once(true_abund, sample_size = ss, replace = replace)
      ind <- compute_indices_from_counts(samp_counts)
      ind$sample_size <- ss
      ind$replicate <- i
      ind
    })
    results[[as.character(ss)]] <- bind_rows(reps)
  }
  out <- bind_rows(results) %>% select(sample_size, replicate, richness, shannon, simpson)
  return(out)
}

# 5) Metrics: bias, variance, RMSE ------------------------------------------------------
# Given per-replicate estimates and the true community indices, compute bias/var/RMSE
compute_performance_metrics <- function(estimates_df, true_indices) {
  # estimates_df: columns sample_size, replicate, richness, shannon, simpson
  # true_indices: tibble with single-row columns richness, shannon, simpson
  metric_fun <- function(x, true_val) {
    # x: vector of replicate estimates
    bias <- mean(x, na.rm = TRUE) - true_val
    variance <- var(x, na.rm = TRUE)
    rmse <- sqrt(mean((x - true_val)^2, na.rm = TRUE))
    tibble(bias = bias, variance = variance, rmse = rmse)
  }
  res <- estimates_df %>%
    pivot_longer(cols = c(richness, shannon, simpson), names_to = "index", values_to = "estimate") %>%
    group_by(sample_size, index) %>%
    summarise(
      true_value = case_when(
        index == "richness" ~ true_indices$richness,
        index == "shannon"  ~ true_indices$shannon,
        index == "simpson"  ~ true_indices$simpson,
        TRUE ~ NA_real_
      ),
      bias = mean(estimate, na.rm = TRUE) - unique(true_value),
      variance = var(estimate, na.rm = TRUE),
      rmse = sqrt(mean((estimate - unique(true_value))^2, na.rm = TRUE)),
      est_mean = mean(estimate, na.rm = TRUE),
      est_sd = sd(estimate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(index, sample_size)
  return(res)
}

# Small utility to format nicely for display
fmt <- function(x, digits = 3) round(x, digits)

# -------------------------
# Shiny UI
# -------------------------
ui <- navbarPage(
  title = "Diversity indices & bead simulation (Teaching tool)",
  # Tab 1: Controls + Summary / Plot
  tabPanel("Simulator",
           sidebarLayout(
             sidebarPanel(
               h4("Community (bag) setup"),
               radioButtons("bag_mode", "Bead bag mode:",
                            choices = c("Manual bag (enter counts)" = "manual",
                                        "Model-generated (even/dominant/logseries)" = "model"),
                            selected = "model"),
               conditionalPanel(
                 "input.bag_mode == 'manual'",
                 helpText("Enter counts for 9 bead colors; must sum to the community size below."),
                 numericInput("manual_counts_sp1", "Color 1 (sp1)", value = 20, min = 0, step = 1),
                 numericInput("manual_counts_sp2", "Color 2 (sp2)", value = 12, min = 0, step = 1),
                 numericInput("manual_counts_sp3", "Color 3 (sp3)", value = 10, min = 0, step = 1),
                 numericInput("manual_counts_sp4", "Color 4 (sp4)", value = 8, min = 0, step = 1),
                 numericInput("manual_counts_sp5", "Color 5 (sp5)", value = 6, min = 0, step = 1),
                 numericInput("manual_counts_sp6", "Color 6 (sp6)", value = 8, min = 0, step = 1),
                 numericInput("manual_counts_sp7", "Color 7 (sp7)", value = 9, min = 0, step = 1),
                 numericInput("manual_counts_sp8", "Color 8 (sp8)", value = 15, min = 0, step = 1),
                 numericInput("manual_counts_sp9", "Color 9 (sp9)", value = 12, min = 0, step = 1)
               ),
               conditionalPanel(
                 "input.bag_mode == 'model'",
                 selectInput("model_dist", "Community distribution", choices = c("even", "dominant", "logseries")),
                 numericInput("model_S", "Number of species (S)", value = 9, min = 1, step = 1),
                 numericInput("model_N", "Total beads in bag (N)", value = 100, min = 1, step = 1),
                 sliderInput("dominance_fraction", "Dominant species fraction (for 'dominant')", min = 0.1, max = 0.9, value = 0.5, step = 0.05)
               ),
               hr(),
               h4("Sampling parameters"),
               numericInput("first_sample_size", "Initial sample size (students normally do this first)", value = 25, min = 1, step = 1),
               checkboxGroupInput("followup_sample_sizes", "Follow-up sample sizes (students do these after first)",
                                  choices = c(5, 10, 20, 50),
                                  selected = c(5,10,20,50)),
               numericInput("nrep", "Replicates per sample size (for simulation)", value = 200, min = 10, step = 10),
               checkboxInput("replace_sampling", "Sample with replacement? (usually FALSE)", value = FALSE),
               hr(),
               actionButton("run_sim", "Run simulation", class = "btn-primary"),
               width = 3
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Overview",
                          h4("True community (bag)"),
                          DTOutput("true_comm_table"),
                          hr(),
                          h4("True indices (full bag)"),
                          tableOutput("true_indices"),
                          hr(),
                          h4("Notes"),
                          p("This simulator lets students see how sampling affects estimates of three commonly used diversity indices:"),
                          tags$ul(
                            tags$li(strong("Species richness:"), "count of species observed (sensitive to rare species and sample size)."),
                            tags$li(strong("Shannon index:"), "accounts for species abundances; more sensitive to changes in rare species than Simpson but still influenced by commonness/evenness."),
                            tags$li(strong("Simpson index (Gini-Simpson):"), "more weighted toward common species; less sensitive to rare species.")
                          ),
                          p("Use the 'Bead bag mode' to either manually enter counts per color (9 colors by default) OR auto-generate ecological-style abundance distributions.")
                 ),
                 
                 tabPanel("Per-replicate results",
                          h4("Replicate estimates (sample-by-sample)"),
                          helpText("This table shows each replicate's index estimates for chosen sample sizes."),
                          DTOutput("replicate_table")
                 ),
                 
                 tabPanel("Performance summary",
                          h4("Bias, Variance, RMSE by sample size and index"),
                          helpText("Bias = mean(estimate) - true_value. Variance = var(estimate). RMSE = sqrt(mean((estimate - true)^2))."),
                          DTOutput("perf_table"),
                          hr(),
                          plotOutput("perf_plot", height = "500px")
                 ),
                 
                 tabPanel("Visualizations",
                          h4("Distributions and sampling"),
                          fluidRow(
                            column(6, plotOutput("bar_true", height = "300px")),
                            column(6, plotOutput("bar_sampled", height = "300px"))
                          ),
                          hr(),
                          h4("Index estimates across sample sizes"),
                          plotOutput("index_boxplots", height = "400px")
                 ),
                 
                 tabPanel("Bead teaching tab",
                          h4("Physical bead simulation (visual + sample)"),
                          p("This tab shows a visual-style representation of the bead bag (table of counts). Use 'Manual bag' to mimic your actual bead colors and counts."),
                          p("You can also press 'Draw one sample' below to see a single example draw."),
                          actionButton("one_draw", "Draw one sample (single replicate)"),
                          verbatimTextOutput("one_draw_text"),
                          tableOutput("one_draw_table")
                 ),
                 
                 tabPanel("Help & Run instructions",
                          h4("Definitions"),
                          p(strong("Bias:"), "the average difference between an estimator and the true value. If bias is 0, the estimator is unbiased."),
                          p(strong("Variance:"), "the variability of the estimator across repeated samples."),
                          p(strong("RMSE (root mean squared error):"), "combines bias and variance: RMSE = sqrt( bias^2 + variance ). It measures the typical size of error."),
                          hr(),
                          h4("How to run this app locally (RStudio)"),
                          tags$ol(
                            tags$li("Create a new R Project (File → New Project → New Directory or Existing Directory)."),
                            tags$li("Save this file as app.R in the project folder."),
                            tags$li("Install dependencies if needed:"),
                            tags$pre('install.packages(c("shiny","tidyverse","DT"))'),
                            tags$li("In RStudio, click the 'Run App' button in the top-right of the editor (or run: shiny::runApp('path/to/project'))"),
                            tags$li("The app GUI will appear in the Viewer pane or your browser.")
                          ),
                          hr(),
                          h4("Notes about teaching workflow"),
                          tags$ul(
                            tags$li("Have students start with the same bag (e.g., 100 beads, 9 colors) and sample 25 beads. Record indices."),
                            tags$li("Then have them sample smaller sizes (5,10,20,50) and compare results."),
                            tags$li("Use the Performance tab to show how bias/variance change with sample size and community structure.")
                          ),
                          hr(),
                          h4("If you'd like"),
                          p("I can convert this to a multi-file Shiny app with nicer theming (bslib), downloadable CSVs, and an instructor notes panel.")
                 )
               )
             )
           )
  ), # end Simulator tabPanel
  # End of navbarPage
  footer = NULL
)

# -------------------------
# Server logic
# -------------------------
server <- function(input, output, session) {
  # Reactive: build the true community based on user inputs
  true_community <- eventReactive(input$run_sim, {
    if (input$bag_mode == "manual") {
      # read the 9 numericInputs
      counts <- c(
        input$manual_counts_sp1,
        input$manual_counts_sp2,
        input$manual_counts_sp3,
        input$manual_counts_sp4,
        input$manual_counts_sp5,
        input$manual_counts_sp6,
        input$manual_counts_sp7,
        input$manual_counts_sp8,
        input$manual_counts_sp9
      )
      # sanitize: make integers and ensure at least one > 0
      counts <- round(as.integer(counts))
      S <- length(counts)
      # if the sum is 0 (empty bag), set an even bag as fallback
      if (sum(counts) == 0) {
        counts <- rep(1, S)
      }
      names(counts) <- paste0("sp", seq_along(counts))
      # If counts don't sum to input$model_N, keep as entered (user said manual)
      counts
    } else {
      # model-generated
      S <- input$model_S
      N <- input$model_N
      generate_community(S = S, N = N, distribution = input$model_dist,
                         dominance_fraction = input$dominance_fraction)
    }
  }, ignoreNULL = FALSE) # run on app start too
  
  # Provide a small reactive that returns the 'first + followup' sizes
  sample_sizes_selected <- reactive({
    first <- input$first_sample_size
    follow <- as.numeric(input$followup_sample_sizes)
    unique(c(first, follow))
  })
  
  # Compute true indices from the full community (for comparison)
  true_indices <- reactive({
    abund <- true_community()
    compute_indices_from_counts(abund)
  })
  
  # When the user clicks 'Run simulation', run many replicates for each size
  sim_results <- eventReactive(input$run_sim, {
    abund <- true_community()
    sizes <- sample_sizes_selected()
    reps <- simulate_sampling(true_abund = abund,
                              sample_sizes = sizes,
                              nrep = input$nrep,
                              replace = input$replace_sampling,
                              seed = 123) # fixed seed for reproducibility
    # attach species names (for tables) - the simulate function returns indexes only
    reps
  }, ignoreNULL = FALSE)
  
  # Performance metrics based on sim_results & true_indices
  perf_metrics <- reactive({
    ests <- sim_results()
    tr <- true_indices()
    compute_performance_metrics(ests, tr)
  })
  
  # Outputs -----------------------------------------------------------
  output$true_comm_table <- renderDT({
    abund <- true_community()
    df <- tibble(species = names(abund), count = as.integer(abund)) %>%
      arrange(desc(count))
    datatable(df, options = list(pageLength = 9, dom = "t"), rownames = FALSE)
  })
  
  output$true_indices <- renderTable({
    true_indices() %>% mutate(across(everything(), ~ round(., 3)))
  }, rownames = FALSE)
  
  output$replicate_table <- renderDT({
    df <- sim_results() %>%
      mutate(across(c(richness, shannon, simpson), ~ round(., 4))) %>%
      arrange(sample_size, replicate)
    datatable(df, options = list(pageLength = 10))
  })
  
  output$perf_table <- renderDT({
    perf_metrics() %>%
      mutate(across(c(bias, variance, rmse, est_mean, est_sd, true_value), ~ round(., 4))) %>%
      datatable(options = list(pageLength = 20))
  })
  
  output$perf_plot <- renderPlot({
    per <- perf_metrics()
    # Plot bias and RMSE for each index across sample sizes
    per_long <- per %>%
      pivot_longer(cols = c(bias, variance, rmse), names_to = "metric", values_to = "value")
    
    ggplot(per_long, aes(x = factor(sample_size), y = value)) +
      geom_col(aes(fill = metric), position = position_dodge(width = 0.8)) +
      facet_wrap(~index, scales = "free_y") +
      labs(x = "Sample size", y = "Metric value", title = "Performance metrics by index and sample size") +
      theme_minimal()
  })
  
  output$bar_true <- renderPlot({
    abund <- true_community()
    df <- tibble(species = names(abund), count = as.integer(abund)) %>% arrange(desc(count))
    ggplot(df, aes(x = reorder(species, -count), y = count)) +
      geom_col() +
      labs(title = "True community (bag) composition", x = "Species (color)", y = "Count") +
      theme_minimal()
  })
  
  # bar for one example sampled (use first sample size, one replicate)
  output$bar_sampled <- renderPlot({
    ests <- sim_results()
    if (nrow(ests) == 0) return(NULL)
    # take the first sample_size, replicate 1
    ss <- unique(ests$sample_size)[1]
    # To show the counts, we re-simulate a single draw (deterministic seed)
    sampled_counts <- sample_community_once(true_abund = true_community(), sample_size = ss, seed = 999)
    df <- tibble(species = names(true_community()), count = as.integer(sampled_counts)) %>% arrange(desc(count))
    ggplot(df, aes(x = reorder(species, -count), y = count)) +
      geom_col() +
      labs(title = paste0("One sampled draw (n = ", ss, ")"), x = "Species (color)", y = "Count") +
      theme_minimal()
  })
  
  output$index_boxplots <- renderPlot({
    ests <- sim_results()
    if (nrow(ests) == 0) return(NULL)
    ests_long <- ests %>% pivot_longer(cols = c(richness, shannon, simpson), names_to = "index", values_to = "estimate")
    ggplot(ests_long, aes(x = factor(sample_size), y = estimate)) +
      geom_boxplot() +
      facet_wrap(~index, scales = "free_y") +
      labs(x = "Sample size", y = "Index estimate", title = "Distribution of index estimates by sample size") +
      theme_minimal()
  })
  
  # Bead teaching tab: one-draw and display
  observeEvent(input$one_draw, {
    sampled_counts <- sample_community_once(true_abund = true_community(), sample_size = input$first_sample_size, seed = sample.int(1e6, 1))
    output$one_draw_text <- renderText({
      paste0("One draw (n = ", input$first_sample_size, ") completed. Below are the counts by color/species.")
    })
    output$one_draw_table <- renderTable({
      tibble(species = names(true_community()), count = as.integer(sampled_counts)) %>% arrange(desc(count))
    })
  })
  
  # If user hasn't clicked 'one_draw' show placeholder
  output$one_draw_text <- renderText({
    "Press 'Draw one sample' to generate a single example draw."
  })
  output$one_draw_table <- renderTable({
    tibble(species = names(true_community()), count = as.integer(rep(0, length(true_community())))) %>% arrange(desc(count))
  })
}

# -------------------------
# Run the app
# -------------------------
shinyApp(ui = ui, server = server)