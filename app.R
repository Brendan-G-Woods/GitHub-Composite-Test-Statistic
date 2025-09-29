library('shinydashboard')
library('shiny')
library('DT')
library('ggplot2')
library('shinycssloaders')
library('data.table')
library('dplyr')
library('formattable')
library('tidyr')
library('ggpubr')
library('caTools')
library('knitr')
library('MASS')
library('shinyjs')
library('xml2')
library('rvest')
library('rmarkdown')
library('corrplot')
library('gridExtra')
library('rsconnect')
library('rlang')
library('survival')
library('stats')






html_content <- paste(readLines("Home-Page-HTML.html"), collapse = "\n")
html_tutorial <- paste(readLines("Tutorial.html"), collapse = "\n")

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Test Statistic Composite"),
  dashboardSidebar(
    sidebarMenu(id = "tab",
                menuItem("Home",     tabName = "home",     icon = icon("home")),
                menuItem("Tutorial",     tabName = "tutorial",     icon = icon("chalkboard-teacher")),
                menuItem("Analysis - SC/QC Composite", tabName = "analysis", icon = icon("chart-bar")),
                menuItem("Analysis - QC Composite", tabName = "analysis_cat", icon = icon("table")),
                menuItem("Source code", icon = icon("github"),
                         href = "https://github.com/Brendan-G-Woods/GitHub-Composite-Test-Statistic",
                         newtab = TRUE)
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .skin-green .box.box-solid.box-success,
        .skin-green .box.box-success {
          border: 1px solid #2E8B57;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                column(width = 12,
                       tags$iframe(
                         srcdoc = html_content,
                         width  = "90%",
                         height = "800px",
                         style  = "border: none; zoom: 1.8;"
                       )
                )
              )
      ),
      
      tabItem(tabName = "tutorial",
              fluidRow(
                column(width = 12,
                       tags$iframe(
                         srcdoc = html_tutorial,
                         width  = "80%",
                         height = "800px",
                         style  = "border: none; zoom: 1.8;"
                       )
                )
              )
      ),
      
      tabItem(tabName = "analysis",

              fluidRow(
                box(
                  title       = "Select Data", width = 12,
                  solidHeader = TRUE, status = "success",
                  radioButtons(
                    inputId  = "type",
                    label    = "Data Type",
                    choices  = c(
                      "Raw Data"                        = "raw",
                      "Correlation Matrix & Test Stats" = "cm_ts"
                    ),
                    selected = character(0)
                  )
                )
              ),
              
              fluidRow(
                box(
                  title       = "Analysis Settings", width = 12,
                  solidHeader = TRUE, status = "success",
                  actionButton("settings", "Change Defaults", icon = icon("cogs")),
                  div(id = "settings_panel", style = "display: none;",
                      tags$hr(),
                      numericInput("set_seed", "Set Seed", value = 123, min = 1),
                      radioButtons("tail_option", "Tail for SC p-value:",
                                   choices = c("Lower-tailed" = "lower",
                                               "Upper-tailed" = "upper",
                                               "Two-tailed"   = "two"),
                                   selected = "two"),
                      numericInput("alpha", "Significance Level (α)", value = 0.05, min = 0.001, max = 0.5)
                      
                  )
                )
              )
              ,
              
              fluidRow(
                conditionalPanel(
                  condition = "input.type == 'raw'",
                  column(3,
                         box(
                           title       = "Upload Files", width = NULL,
                           solidHeader = TRUE, status = "success",
                           fileInput("file1", "Choose CSV File"),
                           checkboxInput("header", "Header", TRUE),
                           tags$hr(),
                           uiOutput("raw_weight_inputs")
                         )
                  ),
                  column(9,
                         box(
                           title       = "Raw Data Table", width = NULL,
                           solidHeader = TRUE, status = "success",
                           DTOutput("mytable")
                         )
                  )
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.type == 'raw'",
                  box(
                    title       = "Raw Data Plots", width = 12,
                    solidHeader = TRUE, status = "success",
                    uiOutput("plot")
                  )
                )
              ),
              
              fluidRow(
                conditionalPanel(
                  condition = "input.type == 'cm_ts'",
                  column(3,
                         box(
                           title       = "Upload Corr. Matrix", width = NULL,
                           solidHeader = TRUE, status = "success",
                           fileInput("cm_file", "Choose CSV File")
                         )
                  ),
                  column(9,
                         box(
                           title       = "Corr. Matrix Preview", width = NULL,
                           solidHeader = TRUE, status = "success",
                           DTOutput("cm_table")
                         )
                  )
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.type == 'cm_ts'",
                  box(
                    title       = "Test Statistics", width = 12,
                    solidHeader = TRUE, status = "success",
                    uiOutput("TSinputs_stats")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title       = "Analysis", width = 12,
                  solidHeader = TRUE, status = "success",
                  fluidRow(
                    conditionalPanel(
                      condition = "input.type == 'raw'",
                      column(6,
                             checkboxGroupInput(
                               inputId = "analysis_methods",
                               label = "Select analysis method(s):",
                               choices = c("Asymptotic" = "asymptotic", "Permutation" = "permutation"),
                               selected = "asymptotic"
                             )
                      )
                    )
                  ),
                  
                  fluidRow(
                    conditionalPanel(
                      condition = "input.type == 'raw' && input.analysis_methods.includes('permutation')",
                      column(6,
                             numericInput("n_permutations", "Number of Permutations", value = 10000, min = 100, step = 1000)
                      )
                    )
                  ),
                  
                  fluidRow(
                    conditionalPanel(
                      condition = "input.analysis_methods.includes('asymptotic')",
                      column(6,
                             numericInput("n_sim", "Number of Simulations", value = 1000000, min = 100, step = 1000)
                      )
                    )
                  ),
                  
                  fluidRow(
                    column(6,
                           actionButton("GoAnalysis", "Run Analysis")
                    )
                  ),
                  tags$hr(),
                  fluidRow(
                    column(12,
                           uiOutput("ResultsTable")
                    )
                  ),
                  fluidRow(
                    column(6,
                           actionButton("GetGraphs", "Get Graphs")
                    )
                  ),
                  tags$hr(),
                  fluidRow(
                    column(12,
                           uiOutput("GraphOutput")  
                    )
                  )
                  
                )
              )
      ),
##################################################################################################      
      tabItem(tabName = "analysis_cat",
              
              fluidRow(
                box(
                  title       = "Analysis Settings", width = 12,
                  solidHeader = TRUE, status = "success",
                  actionButton("settings_cat", "Change Defaults", icon = icon("cogs")),
                  hidden(div(id = "settings_panel_cat", style = "display: none;",
                      tags$hr(),
                      numericInput("set_seed_cat", "Set Seed", value = 123, min = 1),
                      numericInput("alpha_cat", "Significance Level (α)", value = 0.05, min = 0.001, max = 0.5)
                      
                  )
                ))
              ),
              fluidRow(
                  column(3,
                         box(
                           title = "Upload Files", width = NULL,
                           solidHeader = TRUE, status = "success",
                           fileInput("file_cat", "Choose CSV File"),
                           checkboxInput("header_cat", "Header", TRUE),
                           tags$hr(),
                           uiOutput("categorical_selector_cat"),
                           numericInput("number_survival_variables", "Number of Survival Variables", value = 0, min = 0),
                           uiOutput("survival_pairs_ui"),
                           uiOutput("raw_weight_inputs_cat")
                         )
                  ),
                  column(9,
                         box(
                           title       = "Raw Data Table", width = NULL,
                           solidHeader = TRUE, status = "success",
                           DTOutput("mytable_cat")
                         )
                  )
                
              ),
              fluidRow(
               box(
                    title       = "Raw Data Plots", width = 12,
                    solidHeader = TRUE, status = "success",
                    actionButton("show_var_plots_cat", "Generate Plots", icon = icon("image")),
                    tags$hr(),
                    uiOutput("plot_cat")
                  )
              ),

fluidRow(
  box(
    title       = "Analysis", width = 12,
    solidHeader = TRUE, status = "success",
    fluidRow(
        column(6,
               numericInput("n_permutations_cat", "Number of Permutations", value = 1000, min = 100, step = 1000)
        )
    ),
    
    fluidRow(
      column(6,
             actionButton("GoAnalysisCat", "Run Analysis")
      )
    ),
    tags$hr(),
    fluidRow(
      column(12,
             uiOutput("ResultsTableCat")
      )
    ),
    fluidRow(
      column(6,
             actionButton("GetGraphsCat", "Get Null Distribution Graph")
      )
    ),
    tags$hr(),
    fluidRow(
      column(12,
             uiOutput("GraphOutputCat")  
      )
    )
    
  )
))
    )
  )
)


server <- function(input, output, session) {
  raw_data <- reactiveVal(NULL)
  cm_data  <- reactiveVal(NULL)
  asymptotic_result <- reactiveVal(NULL)
  permutation_result <- reactiveVal(NULL)
  settings_shown <- reactiveVal(FALSE)
  raw_data_cat            <- reactiveVal(NULL)
  permutation_result_cat  <- reactiveVal(NULL)


  test_col_names <- reactive({
    req(raw_data())
    df <- raw_data()
    test_cols <- setdiff(seq_along(df), c(1, 2))
    names(df)[test_cols]
  })
  
  observeEvent(input$file1, {
    req(input$file1)
    df <- read.csv(input$file1$datapath, header = input$header)
    raw_data(df)
  })
  
  
  observeEvent(input$settings, {
    toggle(id = "settings_panel")
  })
  
  output$settings_ui <- renderUI({
    if (!settings_shown()) return(NULL)
    
    tagList(
      tags$hr(),
      numericInput("set_seed", "Set Seed", value = analysis_settings$seed, min = 1),
      radioButtons("tail_option", "Tail for SC p-value:",
                   choices = c("Lower-tailed" = "lower",
                               "Upper-tailed" = "upper",
                               "Two-tailed"   = "two"),
                   selected = analysis_settings$tail)
    )
  })
  
  
  analysis_settings <- reactiveValues(
    seed = 123,
    tail = "two",
    alpha = 0.05
  )
  
  observeEvent(input$set_seed, {
    analysis_settings$seed <- input$set_seed
  }, ignoreNULL = TRUE)
  
  observeEvent(input$tail_option, {
    analysis_settings$tail <- input$tail_option
  }, ignoreNULL = TRUE)
  
  observeEvent(input$alpha, {
    analysis_settings$alpha <- input$alpha
  }, ignoreInit = TRUE)
  
  
  
  output$mytable <- renderDT({
    req(raw_data())
    datatable(raw_data(), options = list(scrollX = TRUE))
  })
  
  oi  <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#000000")
  oi2 <- oi[c(5, 1)]
  
  output$plot <- renderUI({
    req(raw_data())
    
    df <- raw_data()
    df[[2]] <- as.factor(df[[2]])
    colnames(df)[1:2] <- c("PatientID", "Treatment")
    
    plots <- list()
    for (i in 3:ncol(df)) {
      var <- df[[i]]
      var_name <- names(df)[i]
      temp_df <- data.frame(Treatment = df$Treatment, Value = var)
      
      if (length(unique(na.omit(var))) <= 2) {
        temp_df$Value <- as.factor(temp_df$Value)
        p <- ggplot(temp_df, aes(x = Treatment, fill = Value)) +
          geom_bar(position = "fill") +
          scale_fill_manual(values = oi2) +
          scale_y_continuous(labels = scales::percent_format()) +
          labs(title = paste("Stacked Barplot of", var_name),
               y = "Proportion", fill = var_name) +
          theme_minimal(base_size = 16)
      } else {
        p <- ggplot(temp_df, aes(x = Treatment, y = Value, fill = Treatment)) +
          geom_boxplot(outlier.shape = NA) +
          geom_jitter(width = 0.15, alpha = 0.2) +
          scale_fill_manual(values = oi2) +
          labs(title = paste("Boxplot of", var_name), y = var_name) +
          theme_minimal(base_size = 16) +
          theme(legend.position = "none")
      }
      plots[[length(plots) + 1]] <- p
    }
    
    ncol     <- 3L
    panel_h  <- 600L
    rows     <- ceiling(length(plots) / ncol)
    device_h <- rows * panel_h + 60
    
    output$rawPlot <- renderPlot({
      grid.arrange(grobs = plots, ncol = ncol)
    })
    
    plotOutput("rawPlot", height = paste0(device_h, "px"))
  })
  
  
  
  observeEvent(input$cm_file, {
    req(input$cm_file)
    df <- read.csv(input$cm_file$datapath, check.names = FALSE)
    row_labels <- df[[1]]
    df <- df[, -1]
    cm <- as.matrix(df)
    rownames(cm) <- row_labels
    colnames(cm) <- row_labels
    if (!isSymmetric(cm)) {
      showNotification("Matrix is not symmetric!", type = "error")
      return()
    }
    cm_data(cm)
  })
  
  output$cm_table <- renderDT({
    req(cm_data())
    datatable(cm_data(), options = list(scrollX = TRUE))
  })
  
  output$raw_weight_inputs <- renderUI({
    req(raw_data())
    df <- raw_data()
    cols <- names(df)
    
    tagList(
      strong("Specify weights for each test variable:"),
      lapply(seq_along(cols)[-c(1,2)], function(i) {
        numericInput(
          inputId = paste0("wt_raw_", i),
          label = paste("Weight for", cols[i]),
          value = 1, min = 0
        )
      })
    )
  })
  
  
  output$TSinputs_stats <- renderUI({
    req(cm_data())
    cm <- cm_data()
    var_names <- if (!is.null(rownames(cm))) rownames(cm) else paste("Var", 1:nrow(cm))
    tagList(
      fluidRow(column(6, strong("Test Statistics")), column(6, strong("Weights"))),
      lapply(seq_along(var_names), function(i) {
        fluidRow(
          column(6, numericInput(paste0("ts_", i), label = paste("TS for", var_names[i]), value = 0)),
          column(6, numericInput(paste0("wt_", i), label = paste("Weight for", var_names[i]), value = 1, min = 0))
        )
      })
    )
  })
  
  observeEvent(input$GoAnalysis, {
    asymptotic_result(NULL)
    permutation_result(NULL)
    
    if (input$type == "raw") {
      df <- raw_data()
      test_idx <- seq_len(ncol(df))[-c(1, 2)]
      
      weights <- sapply(test_idx, function(i) as.numeric(input[[paste0("wt_raw_", i)]]))
      
      req(length(weights) == length(test_idx))
      req(!any(is.na(weights)))
      
      if ("asymptotic" %in% input$analysis_methods) {
        asym_env <- new.env()
        source("rd_asymptotic_app.R", local = asym_env)
        asymptotic_result(asym_env$run_rawdata_analysis(
          df, weights,
          n_sim = input$n_sim,
          seed  = analysis_settings$seed,
          tail  = analysis_settings$tail
        ))
      }
      
      if ("permutation" %in% input$analysis_methods) {
        perm_env <- new.env()
        source("permutation_app.R", local = perm_env)
        permutation_result(perm_env$run_raw_permutation_analysis(
          df, weights,
          n_perm = input$n_permutations,
          seed   = analysis_settings$seed,
          tail   = analysis_settings$tail
        ))
      }
    }
    
    
    if (input$type == "cm_ts") {
      req(cm_data())
      
      source("CM-TS_asymptotic_app.R", local = TRUE)
      corr_matrix <- cm_data()
      n_vars <- nrow(corr_matrix)
      
      test_stats <- sapply(1:n_vars, function(i) input[[paste0("ts_", i)]])
      weights    <- sapply(1:n_vars, function(i) input[[paste0("wt_", i)]])
      
      result <- run_composite_analysis(corr_matrix,
                                       test_stats,
                                       weights,
                                       n_sim = input$n_sim,
                                       seed = analysis_settings$seed,
                                       tail = analysis_settings$tail)
      asymptotic_result(result)
    }
  })
  
  
  output$ResultsTable <- renderUI({
    output_list <- list()
    asym <- asymptotic_result()
    perm <- permutation_result()
    nsim <- isolate(input$n_sim) 
    nperm <- isolate(input$n_permutations)
    
    if (!is.null(asym)) {
      sc_pval <- if (asym$sc_pvalue < (1 / nsim)) sprintf("< %.1e", 1 / nsim) else sprintf("%.4g", asym$sc_pvalue)
      qc_pval <- if (asym$qc_pvalue < (1 / nsim)) sprintf("< %.1e", 1 / nsim) else sprintf("%.4g", asym$qc_pvalue)
      
      output$asymptotic_table <- renderDT({
        datatable(data.frame(
          `Composite Type` = c("Summed Test Statistic (SC)", "Squared Test Statistic (QC)"),
          Value = round(c(asym$sc_composite, asym$qc_composite), 4),
          `p-value` = c(sc_pval, qc_pval),
          check.names = FALSE
        ), options = list(dom = 't', paging = FALSE), rownames = FALSE)
      })
      
      output_list <- append(output_list, list(
        tags$h4("Asymptotic Results"),
        DTOutput("asymptotic_table"),
        tags$hr()
      ))
    }
    
    if (!is.null(perm)) {
      perm_sc_pval <- if (perm$sc_pvalue < (1 / nperm)) sprintf("< %.1e", 1 / nperm) else sprintf("%.4g", perm$sc_pvalue)
      perm_qc_pval <- if (perm$qc_pvalue < (1 / nperm)) sprintf("< %.1e", 1 / nperm) else sprintf("%.4g", perm$qc_pvalue)
      
      output$permutation_table <- renderDT({
        datatable(data.frame(
          `Composite Type` = c("Summed Test Statistic (SC)", "Squared Test Statistic (QC)"),
          Value = round(c(perm$sc_composite, perm$qc_composite), 4),
          `p-value` = c(perm_sc_pval, perm_qc_pval),
          check.names = FALSE
        ), options = list(dom = 't', paging = FALSE), rownames = FALSE)
      })
      
      output_list <- append(output_list, list(
        tags$h4("Permutation Results"),
        DTOutput("permutation_table"),
        tags$hr()
      ))
    }
    
    
    tagList(output_list)
  })
  
  output$SC_Null_Hist <- renderPlot({
    req(input$GetGraphs)
    asym <- asymptotic_result()
    perm <- permutation_result()
    plots <- list()

    if (!is.null(asym)) {
      z_null <- asym$sc_null_distribution
      obs <- asym$sc_composite
      alpha <- analysis_settings$alpha
      tail_type <- analysis_settings$tail

      critical_vals <- switch(tail_type,
                              "lower" = quantile(z_null, alpha),
                              "upper" = quantile(z_null, 1 - alpha),
                              "two"   = quantile(z_null, c(alpha / 2, 1 - alpha / 2))
      )
      
      df <- data.frame(null_distribution = z_null)
      n_bins <- max(20, min(250, round(length(df$null_distribution) / 10)))
      
      p <- ggplot(df, aes(x = null_distribution)) +
        geom_histogram(bins = n_bins, fill = "darksalmon", color = "darksalmon") +
        labs(title = "SC - Asymptotic", x = NULL, y = "Frequency") +
        theme_minimal() +  
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 16)
        )
      
      hist_data <- ggplot_build(p)$data[[1]]
      max_count <- max(hist_data$count)

      p <- p +
        geom_vline(xintercept = obs, color = "darkblue", linewidth = 1) +
        geom_vline(xintercept = critical_vals, linetype = "dashed", color = "red") +
        annotate("text", x = obs, y = max_count * 0.5,
                 label = "Observed Composite", color = "darkblue", angle = 90, vjust = -0.5, size = 6) +
        annotate("text", x = critical_vals, y = max_count * 0.5,
                 label = paste0("alpha = ", alpha), color = "red", angle = 90, vjust = -0.5, size = 6)
      
      plots$asym <- p
    }

    if (!is.null(perm)) {
      z_null <- perm$sc_null
      obs <- perm$sc_composite
      alpha <- analysis_settings$alpha
      tail_type <- analysis_settings$tail
      
      critical_vals <- switch(tail_type,
                              "lower" = quantile(z_null, alpha),
                              "upper" = quantile(z_null, 1 - alpha),
                              "two"   = quantile(z_null, c(alpha / 2, 1 - alpha / 2))
      )
      df <- data.frame(null_distribution = z_null)
      n_bins <- max(20, min(250, round(length(df$null_distribution) / 10)))
      
      p <- ggplot(df, aes(x = null_distribution)) +
        geom_histogram(bins = n_bins, fill = "darksalmon", color = "darksalmon") +
        labs(title = "SC - Permutation", x = NULL, y = "Frequency") +
        theme_minimal() + 
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 16)
        )

      
      hist_data <- ggplot_build(p)$data[[1]]
      max_count <- max(hist_data$count)
      
      p <- p +
        geom_vline(xintercept = obs, color = "darkblue", linewidth = 1) +
        geom_vline(xintercept = critical_vals, linetype = "dashed", color = "red") +
        annotate("text", x = obs, y = max_count * 0.5,
                 label = "Observed Composite", color = "darkblue", angle = 90, vjust = -0.5, size = 6) +
        annotate("text", x = critical_vals, y = max_count * 0.5,
                 label = paste0("alpha = ", alpha), color = "red", angle = 90, vjust = -0.5, size = 6)
      
      plots$perm <- p
    }
    
    grid.arrange(grobs = plots, ncol = 2)
  })
  
  
  output$QC_Null_Hist <- renderPlot({
    req(input$GetGraphs)
    asym <- asymptotic_result()
    perm <- permutation_result()
    plots <- list()
    alpha <- analysis_settings$alpha
    
    if (!is.null(asym)) {
      df <- data.frame(null_distribution = asym$qc_null_distribution)
      p <- ggplot(df, aes(x = null_distribution)) +
        geom_histogram(bins = max(20, min(250, round(length(df$null_distribution)/10))), fill = "darksalmon") +
        labs(title = "QC - Asymptotic", x = NULL, y = "Frequency") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 16)
        )
      
      hist_data <- ggplot_build(p)$data[[1]]
      max_count <- max(hist_data$count)
      p <- p +
        geom_vline(xintercept = asym$qc_composite, color = "darkblue", linewidth = 1) + 
        annotate("text", x = asym$qc_composite, y = 0.5*max_count, label = "Observed Composite",
                 color = "darkblue", angle = 90, vjust = -0.5, size = 6) + 
        geom_vline(xintercept = quantile(df$null_distribution, probs = c(1 - alpha)), color = "red", linetype = "dashed", linewidth = 0.75) +
        annotate("text", x = quantile(df$null_distribution, (1-alpha)), y = 0.5*max_count,
                 color = "red", angle = 90, vjust = -0.5, size = 6, label = paste0("alpha = ", alpha))
      plots$asym <- p
    }
    
    if (!is.null(perm)) {
      df <- data.frame(null_distribution = perm$qc_null)
      p <- ggplot(df, aes(x = null_distribution)) +
        geom_histogram(bins = max(20, min(250, round(length(df$null_distribution)/10))), fill = "darksalmon") +
        labs(title = "QC - Permutation", x = NULL, y = "Frequency") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 16)
        )
      
      hist_data <- ggplot_build(p)$data[[1]]
      max_count <- max(hist_data$count)
      p <- p +
        geom_vline(xintercept = perm$qc_composite, color = "darkblue", linewidth = 1) + 
        annotate("text", x = perm$qc_composite, y = 0.5*max_count, label = "Observed Composite",
                 color = "darkblue", angle = 90, vjust = -0.5, size = 6) + 
        geom_vline(xintercept = quantile(df$null_distribution, probs = c(1 - alpha)), color = "red", linetype = "dashed", linewidth = 0.75) +
        annotate("text", x = quantile(df$null_distribution, 0.95), y = 0.5*max_count,
                 color = "red", angle = 90, vjust = -0.5, size = 6, label = paste0("alpha = ", alpha)) 
      plots$perm <- p
    }
    
    grid.arrange(grobs = plots, ncol = 2)
  })
  
  output$GraphOutput <- renderUI({
    req(input$GetGraphs)
    tagList(
      fluidRow(
        column(6, plotOutput("SC_Null_Hist", height = "500px")),
        column(6, plotOutput("QC_Null_Hist", height = "500px"))
      )
    )
  })
  ######################################################################
  
  observeEvent(input$settings_cat, {
    toggle("settings_panel_cat", anim = TRUE, animType = "slide", time = 0.25)
  }, ignoreInit = TRUE)
  
  output$settings_ui_cat <- renderUI({
    if (!settings_shown()) return(NULL)
    
    tagList(
      tags$hr(),
      numericInput("set_seed_cat", "Set Seed", value = analysis_settings_cat$seed, min = 1),
    )
  })
  

  analysis_settings_cat <- reactiveValues(seed = 123, alpha = 0.05)
  
  observeEvent(input$set_seed_cat, {
    analysis_settings_cat$seed <- input$set_seed_cat
  }, ignoreInit = TRUE)
  
  observeEvent(input$alpha_cat, {
    analysis_settings_cat$alpha <- input$alpha_cat
  }, ignoreInit = TRUE)
  
  
  
  
  observeEvent(input$file_cat, {
    req(input$file_cat)
    df_cat <- read.csv(input$file_cat$datapath, header = input$header_cat)
    raw_data_cat(df_cat)
  })
  
  output$mytable_cat <- renderDT({
    req(raw_data_cat())
    datatable(raw_data_cat(), options = list(scrollX = TRUE))
  })
  
  output$categorical_selector_cat <- renderUI({
    req(raw_data_cat())
    df   <- raw_data_cat()
    cols <- names(df)
    
    choices <- setdiff(cols, cols[1:2])
    
    selectInput(
      inputId  = "categorical_columns_cat",
      label    = "Select Categorical Variables:",
      choices  = choices,
      multiple = TRUE,
      selected = NULL
    )
  })
  
  output$survival_pairs_ui <- renderUI({
    req(raw_data_cat())
    n <- as.integer(if (is.null(input$number_survival_variables)) 0L else input$number_survival_variables)
    if (is.na(n) || n <= 0) return(NULL)
    
    df   <- raw_data_cat()
    cols <- names(df)
    
    base_choices <- setdiff(cols, cols[1:2])
    
    cat_selected <- if (is.null(input$categorical_columns_cat)) character(0) else input$categorical_columns_cat
    
    make_choices <- function(x, blank_label) c(setNames("", blank_label), setNames(x, x))
    
    read_time   <- function(i) { val <- input[[paste0("surv_time_",   i)]]; if (is.null(val) || !nzchar(val)) "" else as.character(val) }
    read_censor <- function(i) { val <- input[[paste0("surv_censor_", i)]]; if (is.null(val) || !nzchar(val)) "" else as.character(val) }
    read_weight <- function(i) { val <- input[[paste0("surv_weight_", i)]]; if (is.null(val) || is.na(val)) 1 else as.numeric(val) }
    
    pre_time   <- vapply(seq_len(n), read_time,   character(1))
    pre_censor <- vapply(seq_len(n), read_censor, character(1))
    pre_weight <- vapply(seq_len(n), read_weight, numeric(1))
    
    ui_list <- vector("list", n)
    used_before <- character(0)  
    
    for (i in seq_len(n)) {
      time_pool <- setdiff(base_choices, c(cat_selected, used_before))
      
      sel_time <- if (nzchar(pre_time[i]) && pre_time[i] %in% time_pool) pre_time[i] else ""
      
      censor_pool <- setdiff(base_choices, c(cat_selected, used_before, if (nzchar(sel_time)) sel_time else character(0)))
      
      sel_censor <- if (nzchar(pre_censor[i]) && pre_censor[i] %in% censor_pool) pre_censor[i] else ""
      
      used_before <- unique(c(used_before, sel_time, sel_censor))
      used_before <- used_before[nzchar(used_before)]
      
      ui_list[[i]] <- box(
        title = paste("Survival Pair", i), width = NULL, status = "success", solidHeader = TRUE,
        
        selectizeInput(
          inputId  = paste0("surv_time_", i),
          label    = paste("Survival time variable", i),
          choices  = make_choices(time_pool, "\u2014 Select time \u2014"),
          selected = sel_time,
          multiple = FALSE
        ),
        
        selectizeInput(
          inputId  = paste0("surv_censor_", i),
          label    = paste("Censor variable", i),
          choices  = make_choices(censor_pool, "\u2014 Select censor \u2014"),
          selected = sel_censor,
          multiple = FALSE
        ),
        
        numericInput(
          inputId = paste0("surv_weight_", i),
          label   = paste("Weight for survival pair", i),
          value   = pre_weight[i], min = 0, step = 0.1
        )
      )
    }
    
    do.call(tagList, ui_list)
  })
  
  
  
  
          
  
  
  
  
  output$raw_weight_inputs_cat <- renderUI({
    req(raw_data_cat())
    df   <- raw_data_cat()
    cols <- names(df)
    
    base_idx <- setdiff(seq_along(cols), c(1, 2))  
    
    n <- as.integer(input$number_survival_variables %||% 0L)
    
    surv_times  <- if (!is.na(n) && n > 0) unlist(lapply(seq_len(n), function(i) input[[paste0("surv_time_",   i)]]))  else character(0)
    surv_censor <- if (!is.na(n) && n > 0) unlist(lapply(seq_len(n), function(i) input[[paste0("surv_censor_", i)]])) else character(0)
    
    idx_exclude <- unique(na.omit(c(match(surv_times, cols), match(surv_censor, cols))))
    keep_idx    <- setdiff(base_idx, idx_exclude)
    
    if (!length(keep_idx)) {
      return(tags$em("No variables available for weighting (all selected as survival time/censor variables)."))
    }
    
    tagList(
      strong("Specify weights for each test variable:"),
      lapply(keep_idx, function(i) {
        existing <- input[[paste0("wt_cat_", i)]]
        numericInput(
          inputId = paste0("wt_cat_", i),
          label   = paste("Weight for", cols[i]),
          value   = if (is.null(existing) || is.na(existing)) 1 else existing,
          min     = 0
        )
      })
    )
  })
  
  
  make_var_plots <- eventReactive(input$show_var_plots_cat, {
    req(raw_data_cat())
    df <- raw_data_cat()
    df[[2]] <- as.factor(df[[2]])
    colnames(df)[1:2] <- c("PatientID", "Treatment")
    
    cat_sel <- if (is.null(input$categorical_columns_cat)) character(0) else input$categorical_columns_cat
    
    n_surv <- as.integer(if (is.null(input$number_survival_variables)) 0L else input$number_survival_variables)
    surv_times <- if (!is.na(n_surv) && n_surv > 0)
      unlist(lapply(seq_len(n_surv), function(k) input[[paste0("surv_time_", k)]]), use.names = FALSE) else character(0)
    surv_cens  <- if (!is.na(n_surv) && n_surv > 0)
      unlist(lapply(seq_len(n_surv), function(k) input[[paste0("surv_censor_", k)]]), use.names = FALSE) else character(0)
    
    skip_vars <- unique(c(surv_times, surv_cens))
    skip_vars <- skip_vars[!is.na(skip_vars) & nzchar(skip_vars)]  # drop NA/empty
    
    plots <- list()
    
    for (i in 3:ncol(df)) {
      var_name <- names(df)[i]
      if (var_name %in% skip_vars) next  
      
      var     <- df[[i]]
      temp_df <- data.frame(Treatment = df$Treatment, Value = var)
      
      treat_as_cat <- (var_name %in% cat_sel) || length(unique(na.omit(var))) <= 2
      
      if (treat_as_cat) {
        temp_df$Value <- as.factor(temp_df$Value)
        p <- ggplot(temp_df, aes(x = Treatment, fill = Value)) +
          geom_bar(position = "fill") +
          scale_fill_manual(values = oi) +
          scale_y_continuous(labels = scales::percent_format()) +
          labs(title = paste("Stacked Barplot of", var_name),
                        x = "Treatment", y = "Proportion", fill = var_name) +
          theme_minimal(base_size = 16)
      } else {
        p <- ggplot(temp_df, aes(x = Treatment, y = Value, fill = Treatment)) +
          geom_boxplot(outlier.shape = NA) +
          geom_jitter(width = 0.15, alpha = 0.2) +
          scale_fill_manual(values = oi2) +
          labs(title = paste("Boxplot of", var_name),
                        x = "Treatment", y = var_name) +
          theme_minimal(base_size = 16) +
          theme(legend.position = "none")
      }
      plots[[length(plots) + 1]] <- p
    }
    
    if (!is.na(n_surv) && n_surv > 0) {
      for (k in seq_len(n_surv)) {
        time_name   <- input[[paste0("surv_time_",   k)]]
        censor_name <- input[[paste0("surv_censor_", k)]]
        if (is.null(time_name) || is.null(censor_name) || time_name == "" || censor_name == "") next
        
        keep <- complete.cases(df[[time_name]], df[[censor_name]], df$Treatment)
        if (!any(keep)) next
        
        d <- data.frame(
          time      = as.numeric(df[[time_name]][keep]),
          event     = as.numeric(!(df[[censor_name]][keep] == 1)),
          Treatment = droplevels(df$Treatment[keep])
        )
        
        if (requireNamespace("survminer", quietly = TRUE)) {
          fit <- survfit(Surv(time, event) ~ Treatment, data = d)
          gp <- survminer::ggsurvplot(
            fit, data = d, risk.table = FALSE, pval = FALSE, conf.int = FALSE,
            palette = oi, ggtheme = theme_minimal(base_size = 16)
          )$plot +
            labs(title = paste0("KM: ", time_name, " / ", censor_name),
                          x = "Time", y = "Survival") +
            theme(legend.title = element_blank())
          plots[[length(plots) + 1]] <- gp
        } else {
          fit <- survfit(Surv(time, event) ~ Treatment, data = d)
          s   <- summary(fit)
          strata_lab <- sub("^Treatment=", "", s$strata)
          km_df <- data.frame(time = s$time, surv = s$surv, strata = factor(strata_lab))
          p <- ggplot(km_df, aes(time, surv, color = strata)) +
            geom_step() +
            coord_cartesian(ylim = c(0, 1)) +
            labs(title = paste0("KM: ", time_name, " / ", censor_name),
                          x = "Time", y = "Survival", color = "Treatment") +
            theme_minimal(base_size = 16)
          if (length(levels(km_df$strata)) <= length(oi)) {
            p <- p + scale_color_manual(values = oi[seq_len(length(levels(km_df$strata)))])
          }
          plots[[length(plots) + 1]] <- p
        }
      }
    }
    
    plots
  })
  
  
  output$plot_cat <- renderUI({
    plots <- make_var_plots()
    if (!length(plots)) return(tags$em("No plots yet. Click Generate Plots."))
    
    ncol     <- 3L
    panel_h  <- 600L
    rows     <- ceiling(length(plots) / ncol)
    device_h <- rows * panel_h + 60 
    
    output$rawPlotCat <- renderPlot({
      grid.arrange(grobs = plots, ncol = ncol)
    }, height = device_h, res = 96)
    
    div(style = "max-height: 80vh; overflow-y: auto;",
    plotOutput("rawPlotCat", height = paste0(device_h, "px")))
  })
  
  
  
  

  observeEvent(input$GoAnalysisCat, {
    permutation_result_cat(NULL)
    req(raw_data_cat())
    df   <- raw_data_cat()
    cols <- names(df)
    
    n_surv <- as.integer(input$number_survival_variables %||% 0L)
    surv_pairs <- list()
    if (!is.na(n_surv) && n_surv > 0) {
      surv_pairs <- lapply(seq_len(n_surv), function(i) {
        list(
          time   = input[[paste0("surv_time_",   i)]],
          censor = input[[paste0("surv_censor_", i)]]
        )
      })
      
      
      if (any(vapply(surv_pairs, function(p) is.null(p$time) || is.null(p$censor) || p$time == "" || p$censor == "", logical(1)))) {
        showNotification("Please select both a time and a censor variable for each survival pair.", type = "error")
        return()
      }
      if (any(vapply(surv_pairs, function(p) identical(p$time, p$censor), logical(1)))) {
        showNotification("Each survival pair must use distinct time and censor columns.", type = "error")
        return()
      }
    }
    
    surv_weights <- if (!is.na(n_surv) && n_surv > 0) {
      vapply(seq_len(n_surv), function(i) {
        val <- input[[paste0("surv_weight_", i)]]
        if (is.null(val) || is.na(val)) 1 else as.numeric(val)
      }, numeric(1))
    } else numeric(0)
    
    base_idx    <- setdiff(seq_along(cols), c(1, 2))
    surv_times  <- if (length(surv_pairs)) vapply(surv_pairs, `[[`, "", "time")   else character(0)
    surv_censor <- if (length(surv_pairs)) vapply(surv_pairs, `[[`, "", "censor") else character(0)
    idx_exclude <- unique(na.omit(c(match(surv_times, cols), match(surv_censor, cols))))
    test_idx    <- setdiff(base_idx, idx_exclude)
    
    weights <- vapply(
      test_idx,
      function(i) {
        val <- input[[paste0("wt_cat_", i)]]
        if (is.null(val) || is.na(val)) 1 else as.numeric(val)
      },
      numeric(1)
    )
    
    perm_env_cat <- new.env()
    source("permutation_cat_app.R", local = perm_env_cat)
    permutation_result_cat(perm_env_cat$run_raw_permutation_analysis(
      df               = df,
      weights          = weights,                      
      n_perm           = input$n_permutations_cat,
      seed             = analysis_settings_cat$seed,
      categorical_var  = input$categorical_columns_cat,
      survival_pairs   = surv_pairs,                   
      survival_weights = surv_weights                  
    ))
  })
  
  
  

  output$ResultsTableCat <- renderUI({
    perm <- permutation_result_cat()
    nperm <- isolate(input$n_permutations_cat)
    if (is.null(perm)) return(NULL)
    
    perm_qc_pval <- if (perm$qc_pvalue < (1 / nperm)) sprintf("< %.1e", 1 / nperm) else sprintf("%.4g", perm$qc_pvalue)
    
    output$permutation_table_cat <- renderDT({
      datatable(data.frame(
        `Composite Type` = c("Squared Test Statistic (QC)"),
        Value = round(c(perm$qc_composite), 4),
        `p-value` = c(perm_qc_pval),
        check.names = FALSE
      ), options = list(dom = 't', paging = FALSE), rownames = FALSE)
    })
    
    tagList(
      tags$h4("Permutation Results"),
      DTOutput("permutation_table_cat"),
      tags$hr()
    )
  })
  


  
  output$QC_Null_Hist_Cat <- renderPlot({
    req(input$GetGraphsCat)
    perm <- permutation_result_cat()
    req(!is.null(perm))
    
    df <- data.frame(null_distribution = perm$qc_null)
    n_bins <- max(20, min(250, round(length(df$null_distribution) / 10)))
    critical_vals_cat <- (quantile(df$null_distribution, 1 - analysis_settings_cat$alpha))
    p <- ggplot(df, aes(x = null_distribution)) +
      geom_histogram(bins = n_bins, fill = "darksalmon", color = "darksalmon") +
      labs(title = "QC - Permutation", x = NULL, y = "Frequency") +
      theme_minimal(base_size = 16)
    
    hist_data <- ggplot_build(p)$data[[1]]
    max_count <- max(hist_data$count)
    
    p +
      geom_vline(xintercept = perm$qc_composite, color = "darkblue", linewidth = 1) +
      geom_vline(xintercept = quantile(df$null_distribution, probs = c(1 - analysis_settings_cat$alpha)),
                 linetype = "dashed", color = "red") +
      annotate("text", x = perm$qc_composite, y = 0.5 * max_count, label = "Observed Composite",
               color = "darkblue", angle = 90, vjust = -0.5, size = 6) +
      annotate("text", x = critical_vals_cat, y = max_count * 0.5,
               label = paste0("alpha = ", analysis_settings_cat$alpha), color = "red", angle = 90, vjust = -0.5, size = 6)
  })
  
  output$GraphOutputCat <- renderUI({
    req(input$GetGraphsCat)
    tagList(
      fluidRow(
        column(6, plotOutput("QC_Null_Hist_Cat", height = "500px"))
      )
    )
  })
  
  
  
  


}

shinyApp(ui, server)

