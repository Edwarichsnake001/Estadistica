library(shiny)
library(markdown)
library(DT)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Análisis Descriptivo y Intervalos de Confianza"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Cargar archivo CSV",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      tags$hr()
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Datos", dataTableOutput("table")),
                  tabPanel("Resumen", dataTableOutput("summaryTable")),
                  tabPanel("Análisis de Variable", 
                           uiOutput("variable_ui"), 
                           plotOutput("variablePlot"),
                           tableOutput("variableSummary"))
      )
    )
  ),
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        color: #333;
        margin-bottom: 15px;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        padding: 0.5em 1em;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
        cursor: default;
        color: #666;
        border: none;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: #428bca;
        color: white;
        border-radius: 4px;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: #428bca;
        color: white;
        border-radius: 4px;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:focus {
        outline: none;
        box-shadow: none;
      }
    "))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  datasetInput <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = TRUE,  # Siempre activado
                   sep = ",",  # Siempre coma
                   quote = '"')  # Siempre comillas dobles
    return(df)
  })
  
  observeEvent(input$tabs, {
    if (input$tabs == "Análisis de Variable") {
      numeric_vars <- names(datasetInput())[sapply(datasetInput(), is.numeric)]
      numeric_vars <- setdiff(numeric_vars, c("id", "sample_id"))
      updateSelectInput(session, "variable", "Selecciona una Variable", choices = numeric_vars)
    }
  })
  
  output$variable_ui <- renderUI({
    if (input$tabs == "Análisis de Variable") {
      numeric_vars <- names(datasetInput())[sapply(datasetInput(), is.numeric)]
      numeric_vars <- setdiff(numeric_vars, c("id", "sample_id"))
      selectInput("variable", "Selecciona una Variable", choices = numeric_vars)
    }
  })
  
  output$table <- renderDataTable({
    df <- datasetInput()
    datatable(df, options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  })
  
  output$summaryTable <- renderDataTable({
    df <- datasetInput()
    summary_df <- data.frame(
      Variable = names(df),
      Tipo = sapply(df, class),
      Conteo = sapply(df, function(x) sum(!is.na(x))),
      `Valores Únicos` = sapply(df, function(x) if(is.factor(x) || is.character(x)) length(unique(x)) else NA),
      `Más Frecuente` = sapply(df, function(x) if(is.factor(x) || is.character(x)) names(sort(table(x), decreasing = TRUE)[1]) else NA),
      Frecuencia = sapply(df, function(x) if(is.factor(x) || is.character(x)) sort(table(x), decreasing = TRUE)[1] else NA),
      Media = sapply(df, function(x) if(is.numeric(x)) round(mean(x, na.rm = TRUE), 3) else NA),
      `Desviación Estándar` = sapply(df, function(x) if(is.numeric(x)) round(sd(x, na.rm = TRUE), 3) else NA),
      Mínimo = sapply(df, function(x) if(is.numeric(x)) min(x, na.rm = TRUE) else NA),
      `1er Cuartil` = sapply(df, function(x) if(is.numeric(x)) quantile(x, 0.25, na.rm = TRUE) else NA),
      Mediana = sapply(df, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA),
      `3er Cuartil` = sapply(df, function(x) if(is.numeric(x)) quantile(x, 0.75, na.rm = TRUE) else NA),
      Máximo = sapply(df, function(x) if(is.numeric(x)) max(x, na.rm = TRUE) else NA)
    )
    datatable(summary_df, options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  })
  
  output$variablePlot <- renderPlot({
    df <- datasetInput()
    req(input$variable)
    var_data <- df[[input$variable]]
    
    mean_val <- round(mean(var_data, na.rm = TRUE), 3)
    sd_val <- round(sd(var_data, na.rm = TRUE), 3)
    n <- sum(!is.na(var_data))
    
    ci_lower <- mean_val - round(qt(0.975, df = n - 1) * sd_val / sqrt(n), 3)
    ci_upper <- mean_val + round(qt(0.975, df = n - 1) * sd_val / sqrt(n), 3)
    
    plot(var_data, main = paste("Análisis de", input$variable), 
         xlab = input$variable, ylab = "Valores", col = "blue")
    abline(h = mean_val, col = "red", lwd = 2)
    abline(h = c(ci_lower, ci_upper), col = "green", lwd = 2, lty = 2)
  })
  
  output$variableSummary <- renderTable({
    df <- datasetInput()
    req(input$variable)
    var_data <- df[[input$variable]]
    
    summary_df <- data.frame(
      Media = round(mean(var_data, na.rm = TRUE), 3),
      `Desviación Estándar` = round(sd(var_data, na.rm = TRUE), 3),
      Mínimo = min(var_data, na.rm = TRUE),
      Máximo = max(var_data, na.rm = TRUE),
      Conteo = sum(!is.na(var_data))
    )
    return(summary_df)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
