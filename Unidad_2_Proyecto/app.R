library(shiny)
library(ggplot2)
library(DT)
library(rlang)

# Define las variables a omitir y variables cualitativas ordinales
omit_vars <- c("q3_last_codesnippet", "q4_refer_codesnippet", "q4_refer_codesnippet_other", 
               "q5_automate_task", "q6_scenario_technicaldebt", "q7_challenging_problem", 
               "q12_novice_java", "q13_expert_java", "q15_reasons_javarating", 
               "q18_main_role_other", "q20_degree_cs_other", "id_stackoverflow", 
               "ids_ghtorrent", "gh_name", "gh_login", "so_DisplayName", "gh_company", 
               "so_WebsiteUrl", "gh_created_at", "so_CreationDate", "sample_id", 
               "id", "gh_location", "so_Location", "q21_remarks", "q22_information_paper", 
               "so_Age")

ordinal_vars <- c("q1_purpose_github", "q2_purpose_stackoverflow", "q10_expertise_general", 
                  "q11_expertise_java", "q14_compare_others", "q17_gender", "q20_degree_cs")

nominal_vars <- c("q1_purpose_github", "q2_purpose_stackoverflow", "q18_main_role", 
                  "q17_gender", "participant_continent", "participant_country")

discrete_vars <- c("q8_experience_general", "q9_experience_java", "q16_age", "q19_work_time")

continuous_vars <- c("")

# Mapeo de nombres de variables a descripciones
variable_labels <- list(
  q1_purpose_github = "¿Para qué propósito usas GitHub?",
  q2_purpose_stackoverflow = "¿Para qué propósito usas StackOverflow?",
  q8_experience_general = "¿Cuántos años usted lleva programando? (General)",
  q9_experience_java = "¿Cuántos años usted lleva programando en Java?",
  q10_expertise_general = "¿Cuál es tu nivel de experiencia en programación (General)?",
  q11_expertise_java = "¿Cuál considera su nivel de experiencia en Java?",
  q14_compare_others = "Basado en su experiencia en Java, ¿usted se ha comparado con otros programadores que conozca?",
  q18_main_role = "¿Cuál es su rango principal cuando desarrolla software?",
  q16_age = "¿Cuál es la edad que tiene?",
  q17_gender = "¿Cuál es su género?",
  q19_work_time = "¿Cuánto tiempo dedica al desarrollo de software en su trabajo?",
  q20_degree_cs = "¿Cuenta con algún título en ciencias de la computación?",
  participant_continent = "Continente donde se encuentra el encuestado",
  participant_country = "País donde se encuentra el encuestado"
)

# Mapeo de valores a etiquetas para q1 y q2
q1_labels <- c("1" = "No uso GitHub.",
               "2" = "Uso Github para proyectos privados.",
               "3" = "Uso Github para proyectos relacionados al trabajo.",
               "4" = "Uso GitHub para ambas formas")

q2_labels <- c("1" = "No uso Overflow.",
               "2" = "Uso Overflow para proyectos privados.",
               "3" = "Uso Overflow para proyectos relacionados al trabajo.",
               "4" = "Uso Overflow para ambas formas")

# Ejemplo de etiquetas para otras variables
q10_labels <- c("1" = "Novato",
                "2" = "Principiante",
                "3" = "Intermedio",
                "4" = "Avanzado",
                "5" = "Muy Avanzado",
                "6" = "Experto")

q11_labels <- c("1" = "Novato",
                "2" = "Principiante",
                "3" = "Intermedio",
                "4" = "Avanzado",
                "5" = "Muy Avanzado",
                "6" = "Experto")

q14_labels <- c("1" = "Si", 
                "2" = "No")

q17_labels <- c("1" = "Masculino", 
                "2" = "Femenino", 
                "3" = "Otro")

q20_labels <- c("1" = "No", 
                "2" = "Yes, Licenciado o equivalente", 
                "3" = "Yes, Masterado o un equivalente", 
                "4" = "Si, Ph.D. o un equivalente", 
                "5" = "Si, otro")

# Función para identificar el tipo de variable
identificar_tipo <- function(df) {
  sapply(names(df), function(x) {
    if (x %in% ordinal_vars) {
      return("Cualitativa Ordinal")
    } else if (x %in% nominal_vars) {
      return("Cualitativa Nominal")
    } else if (x %in% discrete_vars) {
      return("Cuantitativa Discreta")
    } else if (x %in% continuous_vars) {
      return("Cuantitativa Continua")
    } else {
      return("Desconocido")
    }
  })
}

# Función para obtener el nombre descriptivo de una variable
get_label <- function(var_name) {
  if (var_name %in% names(variable_labels)) {
    return(variable_labels[[var_name]])
  } else {
    return(var_name)
  }
}

# Define UI
ui <- fluidPage(
  titlePanel("Análisis Descriptivo y Comparativo de Datos"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Cargar archivo CSV",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      tags$hr(),
      conditionalPanel(
        condition = "input.tabs == 'Análisis de Variable'",
        uiOutput("variable_ui"),
        uiOutput("graph_type_selector")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Comparación de Variables'",
        uiOutput("variable_comparison_ui")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Inicio",
                           h2("Uso de GitHub y Overflow"),
                           p("En la era digital, las plataformas colaborativas 
                           han transformado la forma en que los programadores desarrollan 
                           y comparten conocimiento. Github y Stack Overflow se han consolidado 
                           como herramientas indispensables en el día a día de los desarrolladores, 
                           facilitando tanto la gestión de proyectos como la resolución de problemas 
                           técnicos. Este proyecto se enfoca en analizar los datos recopilados a través 
                           de un cuestionario dirigido a programadores, con el objetivo de explorar 
                           cómo utilizan estas plataformas, qué impacto tienen en su productividad y 
                           cómo influyen en su aprendizaje continuo. A través de este análisis, 
                           buscamos entender mejor las tendencias de uso, 
                           identificar posibles áreas de mejora y generar insights 
                           valiosos que puedan beneficiar tanto a la comunidad de 
                           desarrolladores como a las propias plataformas."),
                           p("Los gráficos presentados en esta aplicación te permitirán visualizar los datos de manera interactiva y comparativa."),
                           p("El cuestionario fue diseñado para mantener conocer la visión
                             de los programadores frente a las plataformas de Github y Stack Overflow
                             y se ha aplicado a una muestra de 123 encuestados de diferentes"),
          
                           br(),
                           p("Objetivos del proyecto:"),
                           tags$ul(
                             tags$li("Analizar cómo y con qué frecuencia los programadores utilizan Github y Stack Overflow, 
                                     determinando las principales funciones y características que aprovechan en cada plataforma."),
                             tags$li("Analizar si existen diferencias significativas en el uso de Github y Stack Overflow 
                                     entre programadores novatos, intermedios y avanzados, con el fin de adaptar las plataformas 
                                     a las necesidades de cada grupo."),
                             tags$li("Proveer una herramienta interactiva para explorar los resultados.")
                           )),
                  tabPanel("Análisis de Variable", 
                           plotOutput("selectedPlot"),
                           tableOutput("variableSummary")),
                  tabPanel("Comparación de Variables", 
                           plotOutput("comparisonPlot"),
                           textOutput("error_message"),
                           tableOutput("comparisonSummary"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Cargar y procesar los datos, eliminando las variables especificadas
  datasetInput <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = '"')
    df <- df[, !(names(df) %in% omit_vars)] # Eliminar las variables especificadas
    
    # Convertir variables a factores con etiquetas
    df$q1_purpose_github <- factor(df$q1_purpose_github, levels = names(q1_labels), labels = q1_labels)
    df$q2_purpose_stackoverflow <- factor(df$q2_purpose_stackoverflow, levels = names(q2_labels), labels = q2_labels)
    df$q10_expertise_general <- factor(df$q10_expertise_general, levels = names(q10_labels), labels = q10_labels)
    df$q11_expertise_java <- factor(df$q11_expertise_java, levels = names(q11_labels), labels = q11_labels)
    df$q14_compare_others <- factor(df$q14_compare_others, levels = names(q14_labels), labels = q14_labels)
    df$q17_gender <- factor(df$q17_gender, levels = names(q17_labels), labels = q17_labels)
    df$q20_degree_cs <- factor(df$q20_degree_cs, levels = names(q20_labels), labels = q20_labels)
    
    return(df)
  })
  
  tipo_variables <- reactive({
    df <- datasetInput()
    identificar_tipo(df)
  })
  
  # UI para seleccionar la variable en la pestaña de Análisis de Variable
  output$variable_ui <- renderUI({
    df <- datasetInput()
    var_names <- names(df)
    var_labels <- sapply(var_names, get_label)
    selectInput("selected_var", "Selecciona una Variable para Analizar",
                choices = setNames(var_names, var_labels))
  })
  
  # UI para seleccionar el tipo de gráfico en la pestaña de Análisis de Variable
  output$graph_type_selector <- renderUI({
    req(input$selected_var)
    var_name <- input$selected_var
    tipo_var <- tipo_variables()[var_name]
    
    if (tipo_var == "Cualitativa Ordinal" || tipo_var == "Cualitativa Nominal") {
      selectInput("graph_type", "Selecciona el Tipo de Gráfico", choices = c(
        "Gráfico de Barras",
        "Gráfico de Barras Apiladas",
        "Gráfico de Densidad Acumulada",
        "Gráfico de Líneas para Tendencias"
      ))
    } else if (tipo_var == "Cuantitativa Discreta" || tipo_var == "Cuantitativa Continua") {
      selectInput("graph_type", "Selecciona el Tipo de Gráfico", choices = c(
        "Histograma",
        "Diagrama de Caja",
        "Curva de Distribución",
        "Gráfico de Campana con Intervalo de Confianza"
      ))
    }
  })
  
  # Graficar la variable seleccionada
  output$selectedPlot <- renderPlot({
    req(input$selected_var, input$graph_type)
    
    # Obtener y depurar los datos
    df <- datasetInput()
    var_data <- df[[input$selected_var]]
    var_label <- get_label(input$selected_var)
    
    if (all(is.na(var_data))) {
      return(NULL)
    }
    
    var_data <- var_data[!is.na(var_data)]
    
    plot <- NULL
    
    # Selección del gráfico basado en el tipo de gráfico
    if (input$graph_type == "Gráfico de Barras") {
      plot <- ggplot(data.frame(var_data), aes(x = factor(var_data))) +
        geom_bar(fill = "blue", color = "black", stat = "count") +
        labs(title = paste("Gráfico de Barras de", var_label),
             x = var_label, y = "Frecuencia")
    } else if (input$graph_type == "Gráfico de Barras Apiladas") {
      plot <- ggplot(data.frame(var_data), aes(x = factor(var_data), fill = factor(var_data))) +
        geom_bar(stat = "count") +
        labs(title = paste("Gráfico de Barras Apiladas de", var_label),
             x = var_label, y = "Frecuencia") +
        theme(legend.position = "bottom")
    } else if (input$graph_type == "Gráfico de Densidad Acumulada") {
      plot <- ggplot(data.frame(var_data), aes(x = as.numeric(factor(var_data)))) +
        stat_ecdf(geom = "step", color = "blue") +
        labs(title = paste("Gráfico de Densidad Acumulada de", var_label),
             x = var_label, y = "Frecuencia Acumulada")
    } else if (input$graph_type == "Gráfico de Líneas para Tendencias") {
      plot <- ggplot(data.frame(var_data), aes(x = factor(var_data), y = ..count.., group = 1)) +
        geom_line(stat = "count", color = "blue") +
        geom_point(stat = "count", color = "red") +
        labs(title = paste("Gráfico de Líneas para Tendencias de", var_label),
             x = var_label, y = "Frecuencia")
    } else if (input$graph_type == "Histograma") {
      if (is.numeric(var_data)) {
        plot <- ggplot(data.frame(var_data), aes(x = var_data)) +
          geom_histogram(binwidth = 30, fill = "blue", color = "black") +
          labs(title = paste("Histograma de", var_label),
               x = var_label, y = "Frecuencia")
      } else {
        output$error_message <- renderText("No se puede crear un histograma de una variable categórica.")
        return(NULL)
      }
    } else if (input$graph_type == "Diagrama de Caja") {
      plot <- ggplot(data.frame(var_data), aes(x = factor(0), y = var_data)) +
        geom_boxplot(fill = "blue", color = "black") +
        labs(title = paste("Diagrama de Caja de", var_label),
             x = "", y = var_label) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    } else if (input$graph_type == "Curva de Distribución") {
      if (is.numeric(var_data)) {
        plot <- ggplot(data.frame(var_data), aes(x = var_data)) +
          geom_density(fill = "blue", alpha = 0.5) +
          labs(title = paste("Curva de Distribución de", var_label),
               x = var_label, y = "Densidad")
      } else {
        output$error_message <- renderText("No se puede crear una curva de distribución de una variable categórica.")
        return(NULL)
      }
    } else if (input$graph_type == "Gráfico de Campana con Intervalo de Confianza") {
      if (is.numeric(var_data)) {
        n <- length(var_data)
        mean_val <- mean(var_data)
        sd_val <- sd(var_data)
        ci_upper <- mean_val + qt(0.975, df = n - 1) * sd_val / sqrt(n)
        ci_lower <- mean_val - qt(0.975, df = n - 1) * sd_val / sqrt(n)
        
        plot <- ggplot(data.frame(var_data), aes(x = var_data)) +
          geom_density(fill = "blue", alpha = 0.5) +
          labs(title = paste("Curva de Distribución con IC de", var_label),
               x = var_label, y = "Densidad") +
          geom_vline(xintercept = mean_val, color = "red") +
          geom_vline(xintercept = ci_upper, linetype="dashed", color = "black") +
          geom_vline(xintercept = ci_lower, linetype="dashed", color = "black")
      } else {
        output$error_message <- renderText("No se puede crear un gráfico de campana con IC de una variable categórica.")
        return(NULL)
      }
    } else {
      output$error_message <- renderText("Tipo de gráfico no soportado.")
      return(NULL)
    }
    
    # Renderizar la gráfica
    if (!is.null(plot)) {
      print(plot)
    }
  })
  
  # Mostrar resumen estadístico de la variable seleccionada
  output$variableSummary <- renderTable({
    req(input$selected_var)
    df <- datasetInput()
    var_data <- df[[input$selected_var]]
    
    if (all(is.na(var_data))) return(NULL)  # Evitar errores si todos los datos son NA
    
    var_data <- var_data[!is.na(var_data)]  # Eliminar valores NA
    
    # Verificar si la variable es numérica antes de calcular estadísticas
    if (is.numeric(var_data)) {
      summary_df <- data.frame(
        Media = round(mean(var_data, na.rm = TRUE), 3),
        `Desviación Estándar` = round(sd(var_data, na.rm = TRUE), 3),
        Mínimo = min(var_data, na.rm = TRUE),
        Máximo = max(var_data, na.rm = TRUE),
        Conteo = length(var_data)
      )
    } else {
      summary_df <- data.frame(
        `Valores Únicos` = length(unique(var_data)),
        `Más Frecuente` = names(sort(table(var_data), decreasing = TRUE)[1]),
        Frecuencia = sort(table(var_data), decreasing = TRUE)[1]
      )
    }
    
    return(summary_df)
  })
  
  # UI para comparación de variables
  output$variable_comparison_ui <- renderUI({
    df <- datasetInput()
    var_names <- names(df)
    var_labels <- sapply(var_names, get_label)
    tagList(
      selectInput("variable1", "Selecciona la Primera Variable", choices = setNames(var_names, var_labels)),
      selectInput("variable2", "Selecciona la Segunda Variable", choices = setNames(var_names, var_labels))
    )
  })
  
  output$comparisonPlot <- renderPlot({
    req(input$variable1, input$variable2)
    df <- datasetInput()
    
    # Asegurarse de que las variables seleccionadas no sean la misma
    if (input$variable1 == input$variable2) {
      output$error_message <- renderText("Las variables seleccionadas no pueden ser las mismas.")
      return(NULL)
    } else {
      output$error_message <- renderText("")
    }
    
    # Eliminar las filas con valores NA en cualquiera de las dos variables seleccionadas
    df <- df[complete.cases(df[[input$variable1]], df[[input$variable2]]), ]
    
    var_data1 <- df[[input$variable1]]
    var_data2 <- df[[input$variable2]]
    
    tipo_var1 <- tipo_variables()[input$variable1]
    tipo_var2 <- tipo_variables()[input$variable2]
    
    label_var1 <- get_label(input$variable1)
    label_var2 <- get_label(input$variable2)
    
    # Seleccionar el tipo de gráfico en función del tipo de variable
    if ((tipo_var1 %in% c("Cuantitativa Discreta", "Cuantitativa Continua")) &&
        (tipo_var2 %in% c("Cuantitativa Discreta", "Cuantitativa Continua"))) {
      
      # Gráfico de dispersión para dos variables numéricas
      plot <- ggplot(data.frame(var_data1, var_data2), aes(x = var_data1, y = var_data2)) +
        geom_point(color = "blue") +
        labs(title = paste("Dispersión entre", label_var1, "y", label_var2),
             x = label_var1, y = label_var2) +
        theme_minimal()
      
    } else if ((tipo_var1 == "Cuantitativa Continua" && tipo_var2 == "Cualitativa Ordinal") ||
               (tipo_var2 == "Cuantitativa Continua" && tipo_var1 == "Cualitativa Ordinal")) {
      # Gráfico de caja para una variable continua y una categórica ordinal
      continuous_var <- ifelse(tipo_var1 == "Cuantitativa Continua", input$variable1, input$variable2)
      categorical_var <- ifelse(tipo_var1 == "Cualitativa Ordinal", input$variable1, input$variable2)
      
      plot <- ggplot(df, aes_string(x = categorical_var, y = continuous_var, fill = categorical_var)) +
        geom_boxplot(color = "black") +
        labs(title = paste("Diagrama de Caja de", get_label(continuous_var), "por", get_label(categorical_var)),
             x = get_label(categorical_var), y = get_label(continuous_var)) +
        theme_minimal() +
        scale_fill_brewer(palette = "Set3")
      
    } else if (tipo_var1 == "Cualitativa Ordinal" && tipo_var2 == "Cualitativa Ordinal") {
      # Gráfico de barras apiladas para dos variables ordinales
      plot <- ggplot(df, aes_string(x = input$variable1, fill = input$variable2)) +
        geom_bar(position = "dodge", stat = "count") +
        labs(title = paste("Comparación de", label_var1, "y", label_var2),
             x = label_var1, y = "Frecuencia") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set3")
      
    } else if (tipo_var1 == "Cualitativa Nominal" && tipo_var2 == "Cualitativa Nominal") {
      # Gráfico de barras apiladas para dos variables nominales
      plot <- ggplot(df, aes_string(x = input$variable1, fill = input$variable2)) +
        geom_bar(position = "dodge", stat = "count") +
        labs(title = paste("Comparación de", label_var1, "y", label_var2),
             x = label_var1, y = "Frecuencia") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set3")
      
    } else if ((tipo_var1 == "Cualitativa Nominal" && tipo_var2 == "Cuantitativa Continua") ||
               (tipo_var2 == "Cualitativa Nominal" && tipo_var1 == "Cuantitativa Continua")) {
      # Gráfico de caja para una variable nominal y una variable continua
      continuous_var <- ifelse(tipo_var1 == "Cuantitativa Continua", input$variable1, input$variable2)
      categorical_var <- ifelse(tipo_var1 == "Cualitativa Nominal", input$variable1, input$variable2)
      
      plot <- ggplot(df, aes_string(x = categorical_var, y = continuous_var, fill = categorical_var)) +
        geom_boxplot(color = "black") +
        labs(title = paste("Diagrama de Caja de", get_label(continuous_var), "por", get_label(categorical_var)),
             x = get_label(categorical_var), y = get_label(continuous_var)) +
        theme_minimal() +
        scale_fill_brewer(palette = "Set3")
      
    } else {
      # Caso no soportado
      plot <- ggplot() + 
        labs(title = "Combinación de variables no soportada",
             subtitle = "Intenta seleccionar variables de tipos compatibles.")
    }
    
    # Renderizar la gráfica
    if (!is.null(plot)) {
      print(plot)
    }
  })
  
  # Resumen estadístico de la comparación
  output$comparisonSummary <- renderTable({
    req(input$variable1, input$variable2)
    
    if (input$variable1 == input$variable2) {
      return(NULL) # No mostrar el resumen si las variables son iguales
    }
    
    df <- datasetInput()
    
    var_data1 <- df[[input$variable1]]
    var_data2 <- df[[input$variable2]]
    
    tipo_var1 <- tipo_variables()[input$variable1]
    tipo_var2 <- tipo_variables()[input$variable2]
    
    # Si ambas variables son categóricas
    if (tipo_var1 %in% c("Cualitativa Ordinal", "Cualitativa Nominal") && 
        tipo_var2 %in% c("Cualitativa Ordinal", "Cualitativa Nominal")) {
      
      summary_df <- data.frame(
        `Frecuencia Var1` = table(var_data1),
        `Frecuencia Var2` = table(var_data2)
      )
      
    } else if (tipo_var1 %in% c("Cuantitativa Discreta", "Cuantitativa Continua") && 
               tipo_var2 %in% c("Cuantitativa Discreta", "Cuantitativa Continua")) {
      
      # Si ambas variables son numéricas
      summary_df <- data.frame(
        `Media Var1` = round(mean(var_data1, na.rm = TRUE), 3),
        `Desviación Estándar Var1` = round(sd(var_data1, na.rm = TRUE), 3),
        `Mínimo Var1` = min(var_data1, na.rm = TRUE),
        `Máximo Var1` = max(var_data1, na.rm = TRUE),
        `Media Var2` = round(mean(var_data2, na.rm = TRUE), 3),
        `Desviación Estándar Var2` = round(sd(var_data2, na.rm = TRUE), 3),
        `Mínimo Var2` = min(var_data2, na.rm = TRUE),
        `Máximo Var2` = max(var_data2, na.rm = TRUE)
      )
      
    } else if (tipo_var1 %in% c("Cuantitativa Discreta", "Cuantitativa Continua")) {
      
      # Si la primera variable es numérica y la segunda categórica
      summary_df <- data.frame(
        `Media Var1` = round(mean(var_data1, na.rm = TRUE), 3),
        `Desviación Estándar Var1` = round(sd(var_data1, na.rm = TRUE), 3),
        `Mínimo Var1` = min(var_data1, na.rm = TRUE),
        `Máximo Var1` = max(var_data1, na.rm = TRUE),
        `Frecuencia Var2` = table(var_data2)
      )
      
    } else if (tipo_var2 %in% c("Cuantitativa Discreta", "Cuantitativa Continua")) {
      
      # Si la primera variable es categórica y la segunda numérica
      summary_df <- data.frame(
        `Frecuencia Var1` = table(var_data1),
        `Media Var2` = round(mean(var_data2, na.rm = TRUE), 3),
        `Desviación Estándar Var2` = round(sd(var_data2, na.rm = TRUE), 3),
        `Mínimo Var2` = min(var_data2, na.rm = TRUE),
        `Máximo Var2` = max(var_data2, na.rm = TRUE)
      )
      
    } else {
      summary_df <- data.frame(
        `Variable 1` = "No compatible",
        `Variable 2` = "No compatible"
      )
    }
    
    return(summary_df)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
