library(shiny)

# Definir UI
ui <- fluidPage(
  titlePanel("Análisis Descriptivo y Intervalos de Confianza"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Cargar archivo CSV",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("file2", "Cargar archivo Markdown del Cuestionario",
                accept = c("text/markdown", ".md")),
      tags$hr(),
      checkboxInput("header", "Encabezado", TRUE),
      radioButtons("sep", "Separador",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Comillas",
                   choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                   selected = '"')
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Datos", tableOutput("table")),
                  tabPanel("Resumen", verbatimTextOutput("summary")),
                  tabPanel("Intervalos de Confianza", plotOutput("ciPlot")),
                  tabPanel("Cuestionario", uiOutput("cuestionario"))
      )
    )
  )
)