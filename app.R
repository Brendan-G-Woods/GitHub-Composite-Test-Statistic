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
library('stats')
library('MASS')
library('shinyjs')
library('xml2')
library('rvest')
library('rmarkdown')
library('corrplot')
library('gridExtra')
library('rsconnect')





html_content <- paste(readLines("Home-Page-HTML.html"), collapse = "\n")


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Test Statistic Composite"),
  dashboardSidebar(
    sidebarMenu(id = "tab",
                menuItem("Home",     tabName = "home",     icon = icon("home")),
                menuItem("Analysis - Continuous/Binary", tabName = "analysis", icon = icon("chart-bar")),
                menuItem("Analysis - All Data Types", tabName = "analysis_cat", icon = icon("table")),
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
                         width  = "100%",
                         height = "800px",
                         style  = "border: none;"
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
                           uiOutput("GraphOutput")  # dynamically inserted graph area
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
                           uiOutput("categorical_selector_cat"),   # <— dynamic
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
             actionButton("GetGraphsCat", "Get Graphs")
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
  
  output$plot <- renderUI({
    req(raw_data())
    output$rawPlot <- renderPlot({
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
            scale_fill_manual(values = c("0" = "orange", "1" = "black")) +
            scale_y_continuous(labels = scales::percent_format()) +
            labs(title = paste("Stacked Barplot of", var_name), y = "Proportion", fill = var_name) +
            theme_minimal(base_size = 16)
        } else {
          p <- ggplot(temp_df, aes(x = Treatment, y = Value, fill = Treatment)) +
            geom_boxplot() +
            scale_fill_manual(values = c("0" = "steelblue", "1" = "forestgreen")) +
            labs(title = paste("Boxplot of", var_name), y = var_name) +
            theme_minimal(base_size = 16)
        }
        
        plots[[length(plots) + 1]] <- p
      }
      
      grid.arrange(grobs = plots, ncol = 2)
    })
    plotOutput("rawPlot", height = "700px")
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
      
      # Run analysis
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
    
    gridExtra::grid.arrange(grobs = plots, ncol = 2)
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
    
    gridExtra::grid.arrange(grobs = plots, ncol = 2)
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
    shinyjs::toggle("settings_panel_cat", anim = TRUE, animType = "slide", time = 0.25)
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
  
  
  
  output$raw_weight_inputs_cat <- renderUI({
    req(raw_data_cat())
    df <- raw_data_cat()
    cols <- names(df)
    
    tagList(
      strong("Specify weights for each test variable:"),
      lapply(setdiff(seq_along(cols), c(1, 2)), function(i) {
        numericInput(
          inputId = paste0("wt_cat_", i),
          label   = paste("Weight for", cols[i]),
          value   = 1, min = 0
        )
      })
    )
  })
  

  # Replace your existing output$plot_cat with this
  
  oi <- c(
    "#E69F00", # orange
    "#56B4E9", # sky blue
    "#009E73", # bluish green
    "#F0E442", # yellow
    "#0072B2", # blue
    "#D55E00", # vermillion
    "#CC79A7", # reddish purple
    "#000000"  # black
  )
  oi2 <- oi[c(5, 1)]  
  
  output$plot_cat <- renderUI({
    req(raw_data_cat())
    output$rawPlotCat <- renderPlot({
      df <- raw_data_cat()
      df[[2]] <- as.factor(df[[2]])
      colnames(df)[1:2] <- c("PatientID", "Treatment")
      
      cat_sel <- if (!is.null(input$categorical_columns_cat)) input$categorical_columns_cat else character(0)
      
      plots <- list()
      for (i in 3:ncol(df)) {
        var_name <- names(df)[i]
        var      <- df[[i]]
        temp_df  <- data.frame(Treatment = df$Treatment, Value = var)
        

        treat_as_cat <- (var_name %in% cat_sel) ||
          length(unique(stats::na.omit(var))) <= 2
        
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
      
      gridExtra::grid.arrange(grobs = plots, ncol = 2)
    })
    plotOutput("rawPlotCat", height = "700px")
  })
  
  

  observeEvent(input$GoAnalysisCat, {
    permutation_result_cat(NULL)
    req(raw_data_cat())
    df <- raw_data_cat()
    
    test_idx <- setdiff(seq_len(ncol(df)), c(1, 2))
    weights  <- sapply(test_idx, function(i) as.numeric(input[[paste0("wt_cat_", i)]]))
    
    req(length(weights) == length(test_idx))
    req(all(!is.na(weights)))
    
    perm_env_cat <- new.env()
    source("permutation_cat_app.R", local = perm_env_cat)
    permutation_result_cat(perm_env_cat$run_raw_permutation_analysis(
      df      = df,
      weights = weights,
      n_perm  = input$n_permutations_cat,
      seed    = analysis_settings_cat$seed
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

