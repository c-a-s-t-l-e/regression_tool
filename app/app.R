library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)

ui <- page_fluid(
  card(
    card_header(
      h2("Regression Diagnostics Tool", class = "text-center")
    ),
    layout_sidebar(
      sidebar = sidebar(
        fileInput("file", "Upload CSV File"),
        selectInput("x_var", "Select X Variable", choices = NULL),
        selectInput("y_var", "Select Y Variable", choices = NULL),
        numericInput("cooks_multiplier", "Cook's Distance Threshold Multiplier", 
                     value = 4, min = 0, step = 0.1),
        numericInput("leverage_multiplier", "Leverage Threshold Multiplier", 
                     value = 2, min = 0, step = 0.1),
        actionButton("reset", "Reset Data")
      ),
      card(
        card_header(
          h3("Regression Analysis", class = "text-center")
        ),
        navset_tab(
          nav_panel(
            "Diagnostic Plots",
            plotOutput("regPlot", height = "400px"),
            br(),
            plotOutput("diagnosticPlots", height = "400px"),
            br(), br(),
            h3("Influential Points", class = "text-center"),
            DTOutput("influentialPoints")
          ),
          nav_panel(
            "R Code",
            verbatimTextOutput("rcode")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    data = NULL,
    removed_points = NULL
  )
  
  observeEvent(input$file, {
    rv$data <- read.csv(input$file$datapath)
    rv$removed_points <- NULL
    updateSelectInput(session, "x_var", choices = names(rv$data))
    updateSelectInput(session, "y_var", choices = names(rv$data))
  })
  
  observeEvent(input$reset, {
    if (!is.null(rv$data)) {
      rv$removed_points <- NULL
    }
  })
  
  regression_data <- reactive({
    req(rv$data, input$x_var, input$y_var)
    
    current_data <- rv$data
    if (!is.null(rv$removed_points)) {
      current_data <- current_data[-rv$removed_points,]
    }
    
    model <- lm(as.formula(paste(input$y_var, "~", input$x_var)), data = current_data)
    
    n <- nrow(current_data)
    p <- 2
    
    cooks_d <- cooks.distance(model)
    leverage <- hatvalues(model)
    studentized_residuals <- rstudent(model)
    
    cooks_threshold <- input$cooks_multiplier/n
    leverage_threshold <- input$leverage_multiplier * p/n
    
    diagnostic_df <- data.frame(
      row = 1:n,
      x = current_data[[input$x_var]],
      y = current_data[[input$y_var]],
      cooks_d = cooks_d,
      leverage = leverage,
      studentized_residuals = studentized_residuals,
      influential = cooks_d > cooks_threshold | leverage > leverage_threshold
    )
    
    list(
      model = model,
      diagnostic_df = diagnostic_df,
      cooks_threshold = cooks_threshold,
      leverage_threshold = leverage_threshold
    )
  })
  
  output$regPlot <- renderPlot({
    req(regression_data())
    
    reg_data <- regression_data()
    
    ggplot(reg_data$diagnostic_df, aes(x = x, y = y)) +
      geom_point(aes(color = influential), size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE) +
      labs(x = input$x_var, y = input$y_var, 
           title = "Regression Plot with Influential Points Highlighted") +
      scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red"))
  })
  
  output$diagnosticPlots <- renderPlot({
    req(regression_data())
    
    reg_data <- regression_data()
    
    par(mfrow = c(1, 2))
    
    plot(reg_data$diagnostic_df$cooks_d, type = "h",
         main = "Cook's Distance",
         ylab = "Cook's Distance",
         xlab = "Observation Number")
    abline(h = reg_data$cooks_threshold, col = "red", lty = 2)
    
    plot(reg_data$diagnostic_df$leverage, type = "h",
         main = "Leverage",
         ylab = "Leverage",
         xlab = "Observation Number")
    abline(h = reg_data$leverage_threshold, col = "red", lty = 2)
  })
  
  output$influentialPoints <- renderDT({
    req(regression_data())
    
    reg_data <- regression_data()
    influential <- reg_data$diagnostic_df[reg_data$diagnostic_df$influential, ]
    
    if(nrow(influential) > 0) {
      influential |>
        select(row, x, y, cooks_d, leverage, studentized_residuals) |>
        round(4)
    }
  })
  
  output$rcode <- renderText({
    req(input$x_var, input$y_var)
    
    paste0(
      "# Load required packages\n",
      "library(dplyr)\n\n",
      "# Fit linear regression model\n",
      sprintf("model <- lm(%s ~ %s, data = your_data)\n\n", input$y_var, input$x_var),
      "# Calculate diagnostic measures\n",
      "n <- nrow(your_data)\n",
      "p <- 2  # number of parameters in simple linear regression\n\n",
      "# Calculate thresholds\n",
      sprintf("cooks_threshold <- %s/n\n", input$cooks_multiplier),
      sprintf("leverage_threshold <- %s * p/n\n\n", input$leverage_multiplier),
      "# Calculate diagnostic statistics\n",
      "diagnostic_df <- data.frame(\n",
      "  cooks_d = cooks.distance(model),\n",
      "  leverage = hatvalues(model),\n",
      "  studentized_residuals = rstudent(model)\n",
      ")\n\n",
      "# Identify influential points\n",
      "influential <- diagnostic_df$cooks_d > cooks_threshold |\n",
      "  diagnostic_df$leverage > leverage_threshold\n\n",
      "# Get indices of influential points\n",
      "influential_points <- which(influential)\n"
    )
  })
}

shinyApp(ui, server)
