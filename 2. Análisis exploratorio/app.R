# ============================================================
# ANÁLISIS EXPLORATORIO DE DATOS
# ============================================================

required_packages <- c(
  "shiny", "readxl", "dplyr", "ggplot2", 
  "DT", "tidyr", "stringr", "corrplot"
)

missing_packages <- required_packages[
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]

if(length(missing_packages) > 0){
  stop(
    paste0(
      "Faltan los siguientes paquetes: ",
      paste(missing_packages, collapse = ", "),
      ".\nInstálalos ejecutando: install.packages(c(",
      paste0('"', missing_packages, '"', collapse = ", "),
      "))"
    )
  )
}

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(stringr)
library(corrplot)

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  titlePanel("Análisis Exploratorio de Datos"),
  
  tags$div(
    style = "font-size: small; font-style: italic; margin-bottom: 15px;",
    "App desarrollada por Liliana De la Torre Desentis"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput("file", "Sube archivo Excel", accept = c(".xlsx", ".xls")),
      
      sliderInput("bins", "Número de clases (Histogramas)", min = 5, max = 50, value = 20),
      
      actionButton("calcular", "Analizar datos", class = "btn-primary", icon = icon("calculator"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Estadísticas Numéricas", DTOutput("tabla_resumen")),
        tabPanel("Histogramas", plotOutput("histograma", height = "600px")),
        tabPanel("Variables Cualitativas", 
                 br(),
                 selectInput("var_cualitativa", "Selecciona variable cualitativa", choices = NULL),
                 DTOutput("tabla_cualitativa")),
        tabPanel("Gráfica de Barras", 
                 br(),
                 selectInput("var_barras", "Variable cualitativa", choices = NULL),
                 plotOutput("barras")),
        tabPanel("Gráfica de Pastel", 
                 br(),
                 selectInput("var_pastel", "Variable cualitativa", choices = NULL),
                 plotOutput("pastel")),
        tabPanel("Boxplots", plotOutput("boxplots", height = "500px")),
        tabPanel("Mapa de Correlaciones", plotOutput("correlaciones", height = "500px")),
        tabPanel("Outliers Detectados", DTOutput("tabla_outliers")),
        tabPanel("Variables Problemáticas", DTOutput("tabla_problemas")),
        tabPanel("Resumen General", DTOutput("resumen"))
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session){
  
  # Carga reactiva tras presionar botón
  datos <- eventReactive(input$calcular, {
    validate(
      need(!is.null(input$file), "Por favor, selecciona y sube un archivo de Excel (.xlsx) antes de hacer clic en 'Analizar datos'.")
    )
    read_excel(input$file$datapath)
  })
  
  # Clasificación de variables
  clasificacion <- reactive({
    df <- datos()
    
    num_df <- df %>% select(where(is.numeric))
    cat_df <- df %>% select(where(\(x) is.character(x) || is.factor(x)))
    
    if(ncol(cat_df) > 0){
      cat_df <- cat_df %>% mutate(across(everything(), as.factor))
    }
    
    list(numericas = num_df, cualitativas = cat_df)
  })
  
  # Limpieza de numéricas
  numericas_limpias <- reactive({
    df <- clasificacion()$numericas
    
    validate(
      need(ncol(df) > 0, "El archivo cargado no contiene variables numéricas.")
    )
    
    # Reemplazar Inf por NA
    df <- df %>% mutate(across(everything(), \(x) ifelse(is.finite(x), x, NA_real_)))
    
    # Filtrar columnas válidas
    df <- df %>% 
      select(where(\(x) sum(!is.na(x)) > 5)) %>% 
      select(where(\(x) {
        v <- var(x, na.rm = TRUE)
        is.finite(v) && v > 0
      }))
    
    validate(
      need(ncol(df) > 0, "No hay variables numéricas adecuadas con varianza válida o suficiente información.")
    )
    
    df
  })
  
  # Actualización de SelectInputs para cualitativas
  observeEvent(clasificacion(), {
    vars <- names(clasificacion()$cualitativas)
    
    if(length(vars) > 0){
      updateSelectInput(session, "var_cualitativa", choices = vars, selected = vars[1])
      updateSelectInput(session, "var_barras", choices = vars, selected = vars[1])
      updateSelectInput(session, "var_pastel", choices = vars, selected = vars[1])
    } else {
      updateSelectInput(session, "var_cualitativa", choices = character(0))
      updateSelectInput(session, "var_barras", choices = character(0))
      updateSelectInput(session, "var_pastel", choices = character(0))
    }
  })
  
  # 1. TABLA RESUMEN NUMÉRICA
  output$tabla_resumen <- renderDT({
    df <- numericas_limpias()
    
    resumen <- lapply(names(df), function(col) {
      x <- df[[col]]
      x_valid <- x[is.finite(x)]
      
      m <- mean(x_valid, na.rm = TRUE)
      s <- sd(x_valid, na.rm = TRUE)
      cv_val <- if(!is.finite(m) || abs(m) < 1e-8) NA_real_ else (100 * s / abs(m))
      
      data.frame(
        Variable = col,
        N = length(x_valid),
        Media = round(m, 4),
        DE = round(s, 4),
        Varianza = round(var(x_valid, na.rm = TRUE), 4),
        `CV (%)` = round(cv_val, 2),
        Mediana = round(median(x_valid, na.rm = TRUE), 4),
        Q1 = round(quantile(x_valid, 0.25, na.rm = TRUE, names = FALSE), 4),
        Q3 = round(quantile(x_valid, 0.75, na.rm = TRUE, names = FALSE), 4),
        Min = round(min(x_valid, na.rm = TRUE), 4),
        Max = round(max(x_valid, na.rm = TRUE), 4),
        check.names = FALSE
      )
    }) %>% bind_rows()
    
    datatable(resumen, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # 2. HISTOGRAMAS
  output$histograma <- renderPlot({
    df <- numericas_limpias()
    df_long <- df %>% pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>% filter(is.finite(Valor))
    
    ggplot(df_long, aes(x = Valor)) +
      geom_histogram(aes(y = after_stat(density)), bins = input$bins, fill = "#337ab7", color = "white", alpha = 0.8) +
      geom_density(color = "darkred", linewidth = 1, na.rm = TRUE) +
      facet_wrap(~ Variable, scales = "free") +
      theme_minimal() +
      labs(x = "Valor", y = "Densidad", title = "Histogramas con curva de densidad")
  })
  
  # 3. TABLA CUALITATIVA
  output$tabla_cualitativa <- renderDT({     cat_df <- clasificacion()$cualitativas
    validate(need(ncol(cat_df) > 0 && !is.null(input$var_cualitativa) && input$var_cualitativa %in% names(cat_df), 
                  "No hay variables cualitativas disponibles."))
    
    tabla <- cat_df %>%
      filter(!is.na(.data[[input$var_cualitativa]])) %>%
      count(.data[[input$var_cualitativa]], name = "Frecuencia") %>%
      rename(Categoria = 1) %>%
      mutate(
        Proporcion = round(Frecuencia / sum(Frecuencia), 4),
        Porcentaje = round(Proporcion * 100, 2),
        Frecuencia_Acumulada = cumsum(Frecuencia)
      )
    
    datatable(tabla, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # 4. GRÁFICA DE BARRAS
  output$barras <- renderPlot({     cat_df <- clasificacion()$cualitativas
    validate(need(ncol(cat_df) > 0 && !is.null(input$var_barras) && input$var_barras %in% names(cat_df), 
                  "Sin datos cualitativos."))
    
    ggplot(cat_df, aes(x = .data[[input$var_barras]])) +
      geom_bar(fill = "#e74c3c", color = "white") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = input$var_barras, y = "Frecuencia", title = paste("Distribución de", input$var_barras))
  })
  
  # 5. GRÁFICA DE PASTEL
  output$pastel <- renderPlot({     cat_df <- clasificacion()$cualitativas
    validate(need(ncol(cat_df) > 0 && !is.null(input$var_pastel) && input$var_pastel %in% names(cat_df), 
                  "Sin datos cualitativos."))
    
    tabla <- cat_df %>%
      filter(!is.na(.data[[input$var_pastel]])) %>%
      count(.data[[input$var_pastel]], name = "Frecuencia") %>%
      rename(Categoria = 1) %>%
      mutate(
        Prop = Frecuencia / sum(Frecuencia),
        label = paste0(Categoria, "\n(", round(Prop * 100, 1), "%)")
      )
    
    ggplot(tabla, aes(x = "", y = Frecuencia, fill = Categoria)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y") +
      theme_void() +
      labs(fill = input$var_pastel, title = paste("Distribución de", input$var_pastel))
  })
  
  # 6. BOXPLOTS
  output$boxplots <- renderPlot({
    df <- numericas_limpias()
    df_long <- df %>% pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>% filter(is.finite(Valor))
    
    ggplot(df_long, aes(x = Variable, y = Valor, fill = Variable)) +
      geom_boxplot(outlier.colour = "red", alpha = 0.7, show.legend = FALSE) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Variable", y = "Valor", title = "Diagramas de Caja (Boxplots)")
  })
  
  # 7. MAPA DE CORRELACIONES
  output$correlaciones <- renderPlot({
    df <- numericas_limpias()
    validate(need(ncol(df) > 1, "Se necesitan al menos dos variables numéricas válidas para calcular correlaciones."))
    
    cor_matrix <- cor(df, use = "pairwise.complete.obs")
    validate(need(sum(is.finite(cor_matrix)) > ncol(df), "Datos insuficientes para correlación."))
    
    corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black", diag = FALSE, tl.col = "black")
  })
  
  # 8. OUTLIERS DETECTADOS
  output$tabla_outliers <- renderDT({
    df <- numericas_limpias()
    outliers_list <- list()
    
    for(v in names(df)){
      x <- df[[v]]
      x_clean <- x[is.finite(x)]
      if(length(x_clean) < 5) next
      
      Q1 <- quantile(x_clean, 0.25, names = FALSE)
      Q3 <- quantile(x_clean, 0.75, names = FALSE)
      RIQ <- Q3 - Q1
      lim_inf <- Q1 - 1.5 * RIQ
      lim_sup <- Q3 + 1.5 * RIQ
      
      pos <- which(is.finite(x) & (x < lim_inf | x > lim_sup))
      
      if(length(pos) > 0){
        outliers_list[[v]] <- data.frame(
          Variable = v,
          Observacion = pos,
          Valor = round(x[pos], 4),
          Q1 = round(Q1, 4),
          Q3 = round(Q3, 4),
          RIQ = round(RIQ, 4),
          Limite_Inferior = round(lim_inf, 4),
          Limite_Superior = round(lim_sup, 4)
        )
      }
    }
    
    tabla_final <- if(length(outliers_list) > 0) bind_rows(outliers_list) else data.frame()
    datatable(tabla_final, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # 9. VARIABLES PROBLEMÁTICAS
  output$tabla_problemas <- renderDT({
    df <- datos()
    
    problemas <- lapply(names(df), function(v){
      x <- df[[v]]
      n_total <- length(x)
      n_na <- sum(is.na(x))
      na_pct <- if(n_total > 0) (100 * n_na / n_total) else 100
      
      n_no_finitos <- if(is.numeric(x)) sum(!is.na(x) & !is.finite(x)) else 0
      n_validos <- if(is.numeric(x)) sum(is.finite(x)) else sum(!is.na(x))
      
      varianza <- if(is.numeric(x) && n_validos > 1) var(x[is.finite(x)], na.rm = TRUE) else NA_real_
      
      prob <- case_when(
        n_total == 0 ~ "Variable vacía",
        na_pct > 30 ~ "Muchos NA (>30%)",
        n_no_finitos > 0 ~ "Contiene Inf/-Inf",
        is.numeric(x) && n_validos <= 5 ~ "Pocos datos válidos",
        is.numeric(x) && is.finite(varianza) && varianza == 0 ~ "Varianza cero",
        TRUE ~ "OK"
      )
      
      data.frame(
        Variable = v,
        Tipo = paste(class(x), collapse = ", "),
        N = n_total,
        N_Validos = n_validos,
        NA_Count = n_na,
        `NA (%)` = round(na_pct, 2),
        `Inf/-Inf` = n_no_finitos,
        Unicos = dplyr::n_distinct(x, na.rm = TRUE),
        Varianza = ifelse(is.finite(varianza), round(varianza, 4), NA),
        Estado = prob,
        check.names = FALSE
      )
    }) %>% bind_rows()
    
    datatable(problemas, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # 10. RESUMEN GENERAL DE LA BASE
  output$resumen <- renderDT({
    df <- datos()
    
    resumen <- data.frame(
      Variable = names(df),
      Tipo = sapply(df, function(x) paste(class(x), collapse = ", ")),
      N = sapply(df, length),
      NA_Count = sapply(df, function(x) sum(is.na(x))),
      `NA (%)` = sapply(df, function(x) round(100 * sum(is.na(x)) / max(length(x), 1), 2)),
      Unicos = sapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE)),
      check.names = FALSE
    )
    
    datatable(resumen, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  
}

# ============================================================
# RUN APP
# ============================================================

shinyApp(ui = ui, server = server)