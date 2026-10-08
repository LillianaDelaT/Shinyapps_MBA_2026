library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel(
    div(
      "Prueba de Wilcoxon (muestras dependientes / pareadas)",
      tags$small("App desarrollada por Liliana De la Torre Desentis",
                 style = "display:block; font-style:italic; font-size:40%;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Sube un archivo Excel (.xlsx)", accept = ".xlsx"),
      uiOutput("columnas_globales")
    ),
    
    mainPanel(
      tabsetPanel(
        
        # ================= IC =================
        tabPanel("Intervalo de confianza",
                 numericInput("conf_ic", "Nivel de confianza (%)", 95, min = 1, max = 99),
                 actionButton("calc_ic", "Calcular IC", class = "btn-primary"),
                 hr(),
                 verbatimTextOutput("res_ic"),
                 plotOutput("plot_ic")
        ),
        
        # ================= PRUEBA =================
        tabPanel("Prueba de hipótesis",
                 selectInput("hip", "Hipótesis alternativa:",
                             c("Diferente" = "two.sided", "Mayor" = "greater", "Menor" = "less")),
                 numericInput("mu0", "Valor bajo H₀ (mediana de diff):", 0),
                 numericInput("alpha", "Nivel de significancia α:", 0.05, min = 0.001, max = 0.5),
                 actionButton("calc_ph", "Calcular Prueba", class = "btn-primary"),
                 hr(),
                 verbatimTextOutput("res_ph"),
                 plotOutput("plot_ph")
        ),
        
        # ================= TAMAÑO =================
        tabPanel("Tamaño de muestra",
                 numericInput("alpha_tm", "α", 0.05, min = 0.001, max = 0.5),
                 numericInput("power", "Poder (1-β)", 0.8, min = 0.5, max = 0.99),
                 numericInput("sd", "Desv. estándar estimada (s_d)", 1, min = 0.0001),
                 numericInput("delta", "Efecto detectable (δ)", 0.5),
                 actionButton("calc_tm", "Calcular Muestra", class = "btn-primary"),
                 hr(),
                 verbatimTextOutput("res_tm")
        ),
        
        # ================= W =================
        tabPanel("¿Cómo se construye W?",
                 actionButton("calc_w", "Mostrar procedimiento", class = "btn-info"),
                 hr(),
                 verbatimTextOutput("res_w"),
                 tableOutput("tabla_w"),
                 plotOutput("plot_w")
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  # ================= DATOS =================
  datos <- reactive({
    req(input$archivo)
    read_excel(input$archivo$datapath)
  })
  
  output$columnas_globales <- renderUI({
    req(datos())
    cols <- names(datos())
    tagList(
      selectInput("col1", "Variable X (Muestra 1 / Antes)", cols),
      selectInput("col2", "Variable Y (Muestra 2 / Después)", cols, selected = cols[min(2, length(cols))])
    )
  })
  
  # Preparación y limpieza de diferencias pareadas
  difs_clean <- reactive({
    df <- datos()
    req(input$col1, input$col2)
    
    x <- df[[input$col1]]
    y <- df[[input$col2]]
    
    if(!is.numeric(x) || !is.numeric(y)) return(NULL)
    
    df_paired <- na.omit(data.frame(x = x, y = y))
    if(nrow(df_paired) < 2) return(NULL)
    
    df_paired$d <- df_paired$x - df_paired$y
    df_paired
  })
  
  # ================= INTERVALO DE CONFIANZA =================
  res_ic_data <- eventReactive(input$calc_ic, {
    df_p <- difs_clean()
    req(df_p)
    
    prueba <- wilcox.test(df_p$x, df_p$y, 
                          paired = TRUE,
                          conf.int = TRUE,
                          conf.level = input$conf_ic / 100,
                          exact = FALSE)
    
    list(df_p = df_p, prueba = prueba, conf = input$conf_ic)
  })
  
  output$res_ic <- renderPrint({
    res <- res_ic_data()
    cat("=====================================\n")
    cat(" Intervalo de Confianza (Wilcoxon)\n")
    cat("=====================================\n")
    cat("Nivel de confianza:", res$conf, "%\n\n")
    
    cat("Pseudo-mediana estimada:", round(res$prueba$estimate, 4), "\n")
    cat("Intervalo de Confianza: [", round(res$prueba$conf.int[1], 4), ",",
        round(res$prueba$conf.int[2], 4), "]\n\n")
    
    cat("Interpretación:\n")
    cat("Estimación del desplazamiento o pseudo-mediana de la diferencia (X - Y).\n")
  })
  
  output$plot_ic <- renderPlot({
    res <- res_ic_data()
    ggplot(res$df_p, aes(x = d)) +
      geom_histogram(fill = "skyblue", color = "black", bins = max(5, floor(nrow(res$df_p)/3))) +
      geom_vline(xintercept = res$prueba$estimate, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = c(res$prueba$conf.int[1], res$prueba$conf.int[2]), 
                 color = "darkgreen", linetype = "dotted", size = 1) +
      theme_minimal() +
      labs(title = "Distribución de Diferencias (X - Y)",
           subtitle = "Línea roja: Pseudo-mediana | Líneas verdes: Límites IC",
           x = "Diferencia (d)", y = "Frecuencia")
  })
  
  # ================= PRUEBA DE HIPÓTESIS =================
  res_ph_data <- eventReactive(input$calc_ph, {
    df_p <- difs_clean()
    req(df_p)
    
    prueba <- wilcox.test(
      df_p$x, df_p$y,
      paired = TRUE,
      alternative = input$hip,
      mu = input$mu0,
      exact = FALSE
    )
    
    list(df_p = df_p, prueba = prueba, hip = input$hip, mu0 = input$mu0, alpha = input$alpha)
  })
  
  output$res_ph <- renderPrint({
    res <- res_ph_data()
    
    cat("=====================================\n")
    cat(" Prueba de Rangos con Signo de Wilcoxon\n")
    cat("=====================================\n\n")
    
    cat("Hipótesis:\n")
    cat("H₀: Mediana(X - Y) =", res$mu0, "\n")
    
    switch(res$hip,
           "less"      = cat("Hₐ: Mediana(X - Y) <", res$mu0, "\n"),
           "greater"   = cat("Hₐ: Mediana(X - Y) >", res$mu0, "\n"),
           "two.sided" = cat("Hₐ: Mediana(X - Y) ≠", res$mu0, "\n"))
    
    cat("\nNivel de significancia (α) =", res$alpha, "\n\n")
    cat("Estadístico V (W+ en R):", round(res$prueba$statistic, 4), "\n")
    cat("Valor p:", formatC(res$prueba$p.value, format = "e", digits = 4), "\n\n")
    
    cat("Decisión:\n")
    if(res$prueba$p.value < res$alpha){
      cat("🟢 Se rechaza H₀ (Diferencia estadísticamente significativa)\n")
    } else {
      cat("🔴 No se rechaza H₀ (Sin evidencia suficiente para rechazar H₀)\n")
    }
    
    if(any(res$df_p$d == 0)){
      cat("\n⚠️ Nota: Existen diferencias iguales a cero (d = 0). Se han excluido automáticamente en el cálculo de rangos.\n")
    }
  })
  
  output$plot_ph <- renderPlot({
    res <- res_ph_data()
    ggplot(res$df_p, aes(x = d)) +
      geom_histogram(fill = "lightgreen", color = "black", bins = max(5, floor(nrow(res$df_p)/3))) +
      geom_vline(xintercept = res$mu0, color = "blue", linetype = "dashed", size = 1) +
      geom_vline(xintercept = median(res$df_p$d), color = "darkred", linetype = "solid", size = 1) +
      theme_minimal() +
      labs(title = "Distribución de las diferencias",
           subtitle = "Línea azul: Valor bajo H0 | Línea roja: Mediana muestral",
           x = "Diferencia (X - Y)", y = "Frecuencia")
  })
  
  # ================= TAMAÑO DE MUESTRA =================
  res_tm_data <- eventReactive(input$calc_tm, {
    req(input$alpha_tm, input$power, input$sd, input$delta)
    
    # Tamaño paramétrico equivalente (Prueba t pareada)
    res_t <- power.t.test(delta = input$delta, 
                          sd = input$sd, 
                          sig.level = input$alpha_tm, 
                          power = input$power, 
                          type = "paired", 
                          alternative = "two.sided")
    
    # Corrección no paramétrica (Asymptotic Relative Efficiency ARE = 0.864 en el peor caso normal)
    n_wilcoxon <- ceiling(res_t$n / 0.864)
    n_t <- ceiling(res_t$n)
    
    list(n_wilcoxon = n_wilcoxon, n_t = n_t)
  })
  
  output$res_tm <- renderText({
    res <- res_tm_data()
    paste0("Muestra requerida para Wilcoxon: ", res$n_wilcoxon, " pares de datos.\n",
           "(Equivalente prueba t: ", res$n_t, " pares; ajustado por ARE de Lehmann = 0.864).")
  })
  
  # ================= CONSTRUCCIÓN PASO A PASO DE W =================
  res_w_data <- eventReactive(input$calc_w, {
    df_p <- difs_clean()
    req(df_p)
    
    tabla <- df_p %>%
      filter(d != 0) %>%
      mutate(
        abs_d = abs(d),
        rango = rank(abs_d), # Maneja empates asignando promedios
        signo = ifelse(d > 0, "+", "-"),
        r_sign = rango * ifelse(d > 0, 1, -1)
      )
    
    Wp <- sum(tabla$rango[tabla$signo == "+"])
    Wm <- sum(tabla$rango[tabla$signo == "-"])
    W_min <- min(Wp, Wm)
    
    list(tabla = tabla, Wp = Wp, Wm = Wm, W_min = W_min)
  })
  
  output$tabla_w <- renderTable({
    res_w_data()$tabla %>%
      select(x, y, d, abs_d, rango, signo, r_sign) %>%
      rename("X" = x, "Y" = y, "Diferencia (d)" = d, "|d|" = abs_d, 
             "Rango" = rango, "Signo" = signo, "Rango c/ Signo" = r_sign)
  })
  
  output$res_w <- renderPrint({
    res <- res_w_data()
    cat("=====================================\n")
    cat(" Desglose del Estadístico de Wilcoxon\n")
    cat("=====================================\n")
    cat("Suma de rangos positivos (W+ / V en R):", res$Wp, "\n")
    cat("Suma de rangos negativos (W-):", res$Wm, "\n")
    cat("Mínimo(W+, W-):", res$W_min, "\n\n")
    cat("Nota: R reporta 'V' en wilcox.test, el cual equivale a W+ (", res$Wp, ").\n")
  })
  
  output$plot_w <- renderPlot({
    res <- res_w_data()
    ggplot(res$tabla, aes(x = factor(1:nrow(res$tabla)), y = r_sign, fill = signo)) +
      geom_col() +
      scale_fill_manual(values = c("+" = "lightblue", "-" = "coral")) +
      theme_minimal() +
      labs(title = "Rangos Asignados a las Diferencias",
           x = "Observación (Excluidos d = 0)", y = "Rango con Signo", fill = "Signo")
  })
}

shinyApp(ui, server)