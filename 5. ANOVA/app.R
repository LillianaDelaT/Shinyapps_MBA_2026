# ==============================================================================
# 1. VERIFICACIÓN E INSTALACIÓN DE PAQUETES NECESARIOS
# ==============================================================================
paquetes_requeridos <- c("shiny", "ggplot2", "dplyr", "readxl", "janitor", "tidyr", "bslib")

# Identificar cuáles no están instalados aún
paquetes_faltantes <- paquetes_requeridos[!(paquetes_requeridos %in% installed.packages()[,"Package"])]

# Instalar solo los que falten
if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes, dependencies = TRUE)
}

# Cargar todas las librerías silenciosamente
suppressPackageStartupMessages({
  lapply(paquetes_requeridos, library, character.only = TRUE)
})

# ==============================================================================
# 2. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Prueba ANOVA de un Factor"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sube tu archivo Excel (.xlsx)", accept = c(".xlsx", ".xls")),
      numericInput("alpha", "Nivel de significancia (α)", value = 0.05, min = 0.01, max = 0.10, step = 0.01),
      actionButton("run", "Realizar ANOVA", class = "btn-primary w-100")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resultados",
                 h4("Resumen Estadístico"),
                 verbatimTextOutput("anova_summary"),
                 h4("Tabla ANOVA"),
                 tableOutput("anova_table"),
                 h4("Prueba Post Hoc de Tukey"),
                 tableOutput("tukey_result")
        ),
        tabPanel("Gráficos de Datos y F",
                 h4("Distribución de los Grupos"),
                 plotOutput("boxplot"),
                 h4("Distribución F y Región de Rechazo"),
                 plotOutput("f_rejection_plot")
        ),
        tabPanel("Diagnóstico de Supuestos",
                 h4("Gráficos de Residuos"),
                 plotOutput("residual_plot")
        )
      )
    )
  )
)

# ==============================================================================
# 3. LÓGICA DEL SERVIDOR (SERVER)
# ==============================================================================
server <- function(input, output, session) {
  
  # Carga y limpieza de datos
  data_long <- reactive({
    req(input$file)
    
    # Lectura del archivo
    df <- tryCatch({
      read_excel(input$file$datapath)
    }, error = function(e) {
      return(NULL)
    })
    
    validate(
      need(!is.null(df), "No se pudo leer el archivo Excel. Asegúrate de que tenga un formato válido.")
    )
    
    df <- janitor::clean_names(df)
    
    # Conversión a formato largo manteniendo solo columnas numéricas
    df_long <- df %>%
      pivot_longer(cols = everything(), names_to = "grupo", values_to = "valor") %>%
      filter(!is.na(valor)) %>%
      mutate(
        valor = as.numeric(valor),
        grupo = as.factor(grupo)
      ) %>%
      filter(!is.na(valor))
    
    validate(
      need(nrow(df_long) > 0, "El archivo no contiene datos numéricos válidos."),
      need(n_distinct(df_long$grupo) >= 2, "Se requieren al menos 2 grupos para realizar ANOVA.")
    )
    
    df_long
  })
  
  # Cálculo del ANOVA al presionar el botón
  resultado <- eventReactive(input$run, {
    df <- data_long()
    
    modelo <- aov(valor ~ grupo, data = df)
    resumen <- summary(modelo)[[1]]
    
    F_value <- resumen[["F value"]][1]
    p_value <- resumen[["Pr(>F)"]][1]
    df1 <- resumen[["Df"]][1]
    df2 <- resumen[["Df"]][2]
    
    tukey_df <- NULL
    if (!is.na(p_value) && p_value < input$alpha) {
      tukey_obj <- TukeyHSD(modelo)
      tukey_df <- as.data.frame(tukey_obj$grupo)
      tukey_df <- round(tukey_df, 4)
    }
    
    list(
      modelo = modelo, 
      resumen = resumen, 
      F_value = F_value, 
      p_value = p_value, 
      df1 = df1, 
      df2 = df2, 
      tukey = tukey_df
    )
  })
  
  # Salidas (Outputs)
  output$anova_summary <- renderPrint({
    req(resultado())
    res <- resultado()
    
    cat("Estadístico F calculado:", round(res$F_value, 4), "\n")
    cat("p-valor:", format.pval(res$p_value, digits = 4), "\n\n")
    
    if (res$p_value < input$alpha) {
      cat("Decisión: Se rechaza H₀ (α =", input$alpha, ")\n")
      cat("Conclusión: Existen diferencias estadísticamente significativas entre las medias de al menos dos grupos.\n")
    } else {
      cat("Decisión: No se rechaza H₀ (α =", input$alpha, ")\n")
      cat("Conclusión: No hay evidencia suficiente para afirmar que existen diferencias entre las medias.\n")
    }
  })
  
  output$anova_table <- renderTable({
    req(resultado())
    as.data.frame(resultado()$resumen)
  }, rownames = TRUE, na = "")
  
  output$tukey_result <- renderTable({
    req(resultado())
    tukey_data <- resultado()$tukey
    
    validate(
      need(!is.null(tukey_data), "El p-valor es mayor o igual a α; no se realiza la prueba de Tukey.")
    )
    
    tukey_data
  }, rownames = TRUE)
  
  output$boxplot <- renderPlot({
    df <- data_long()
    ggplot(df, aes(x = grupo, y = valor, fill = grupo)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE) +
      geom_jitter(width = 0.1, alpha = 0.5) +
      theme_minimal() +
      labs(title = "Distribución de Datos por Grupo", x = "Grupo", y = "Valor")
  })
  
  output$f_rejection_plot <- renderPlot({
    req(resultado())
    res <- resultado()
    df1 <- res$df1
    df2 <- res$df2
    f_calc <- res$F_value
    f_critico <- qf(1 - input$alpha, df1, df2)
    
    x_max <- max(f_critico, f_calc, 5) * 1.2
    
    curve(df(x, df1, df2), from = 0, to = x_max, n = 500, col = "royalblue", lwd = 2,
          ylab = "Densidad", xlab = "F", main = "Distribución F de Fisher")
    
    x_rect <- seq(f_critico, x_max, length.out = 200)
    polygon(c(f_critico, x_rect, x_max), c(0, df(x_rect, df1, df2), 0), col = rgb(1, 0, 0, 0.25), border = NA)
    
    abline(v = f_critico, col = "red", lwd = 2, lty = 2)
    abline(v = f_calc, col = "darkgreen", lwd = 2)
    
    legend("topright", 
           legend = c(paste("F crítico (", round(f_critico, 3), ")"), paste("F observado (", round(f_calc, 3), ")")),
           col = c("red", "darkgreen"), lty = c(2, 1), lwd = 2)
  })
  
  output$residual_plot <- renderPlot({
    req(resultado())
    modelo <- resultado()$modelo
    
    par(mfrow = c(1, 2))
    plot(modelo$fitted.values, modelo$residuals,
         main = "Residuos vs Valores Ajustados", xlab = "Ajustados", ylab = "Residuos",
         pch = 19, col = "#2C3E50")
    abline(h = 0, col = "red", lty = 2)
    
    qqnorm(modelo$residuals, main = "Normal Q-Q Plot", pch = 19, col = "#2C3E50")
    qqline(modelo$residuals, col = "red", lwd = 2)
  })
}

# ==============================================================================
# 4. EJECUCIÓN DE LA APLICACIÓN
# ==============================================================================
shinyApp(ui, server)