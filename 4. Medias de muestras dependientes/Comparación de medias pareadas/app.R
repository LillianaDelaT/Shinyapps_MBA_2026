library(shiny)
library(readxl)
library(ggplot2)

ui <- fluidPage(
  titlePanel(
    div(
      "Diferencia de medias para muestras pareadas",
      tags$small("App desarrollada por Liliana De la Torre Desentis",
                 style = "display:block; font-style:italic; font-size:40%;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Sube un archivo Excel (.xlsx):", accept = ".xlsx"),
      uiOutput("columnas")
    ),
    
    mainPanel(
      tabsetPanel(
        
        # ================= INTERVALO =================
        tabPanel("Intervalo de confianza",
                 withMathJax(),
                 helpText("$$\\bar{d} \\pm t_{\\alpha/2}\\frac{s_d}{\\sqrt{n}}$$"),
                 numericInput("conf_ic", "Nivel de confianza (%)", 95, min = 1, max = 99),
                 actionButton("calcular_ic", "Calcular", class = "btn-primary"),
                 hr(),
                 verbatimTextOutput("resultado_ic"),
                 plotOutput("grafica_ic")
        ),
        
        # ================= HIPOTESIS =================
        tabPanel("Prueba de hipótesis",
                 selectInput("hipotesis", "Hipótesis alternativa:",
                             c("Diferente"="two.sided", "Mayor"="greater", "Menor"="less")),
                 numericInput("conf_ph", "Nivel de confianza (%)", 95, min = 1, max = 99),
                 actionButton("calcular_ph", "Calcular", class = "btn-primary"),
                 hr(),
                 verbatimTextOutput("resultado_ph"),
                 plotOutput("grafica_ph")
        ),
        
        # ================= TAMAÑO =================
        tabPanel("Tamaño de muestra",
                 numericInput("alpha", "Nivel de significancia (α)", 0.05, min = 0.001, max = 0.5),
                 numericInput("poder", "Poder estadístico (1-β)", 0.80, min = 0.5, max = 0.99),
                 numericInput("sd", "Desviación estándar de diferencias (s_d)", 1, min = 0.0001),
                 numericInput("delta", "Diferencia mínima a detectar (δ)", 0.5),
                 actionButton("calc_n", "Calcular", class = "btn-primary"),
                 hr(),
                 verbatimTextOutput("res_n")
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
  
  output$columnas <- renderUI({
    req(datos())
    cols <- names(datos())
    tagList(
      selectInput("col1", "Columna 1 (Antes / Muestra 1)", cols),
      selectInput("col2", "Columna 2 (Después / Muestra 2)", cols, selected = cols[min(2, length(cols))])
    )
  })
  
  difs <- reactive({
    df <- datos()
    req(input$col1, input$col2)
    
    g1 <- df[[input$col1]]
    g2 <- df[[input$col2]]
    
    if(!is.numeric(g1) || !is.numeric(g2)) return(NULL)
    
    df_clean <- na.omit(data.frame(g1, g2))
    if(nrow(df_clean) < 2) return(NULL)
    
    df_clean$g1 - df_clean$g2
  })
  
  # ================= INTERVALO DE CONFIANZA =================
  res_ic <- eventReactive(input$calcular_ic, {
    d <- difs()
    req(d)
    
    n <- length(d)
    m <- mean(d)
    s <- sd(d)
    alpha <- 1 - input$conf_ic / 100
    tcrit <- qt(1 - alpha/2, df = n - 1)
    se <- s / sqrt(n)
    me <- tcrit * se
    
    list(d = d, n = n, m = m, s = s, se = se, tcrit = tcrit, me = me, 
         li = m - me, ls = m + me, conf = input$conf_ic)
  })
  
  output$resultado_ic <- renderPrint({
    res <- res_ic()
    cat("=====================================\n")
    cat(" Intervalo de Confianza para μ_d\n")
    cat("=====================================\n")
    cat("Nivel de confianza:", res$conf, "%\n")
    cat("Tamaño de muestra (n):", res$n, "\n")
    cat("Media de diferencias:", round(res$m, 4), "\n")
    cat("Desviación estándar:", round(res$s, 4), "\n")
    cat("Error estándar:", round(res$se, 4), "\n")
    cat("t crítico:", round(res$tcrit, 4), "\n")
    cat("Margen de error:", round(res$me, 4), "\n\n")
    cat("Intervalo:\n")
    cat("[", round(res$li, 4), ",", round(res$ls, 4), "]\n")
  })
  
  output$grafica_ic <- renderPlot({
    res <- res_ic()
    ggplot(data.frame(d = res$d), aes(x = d)) +
      geom_histogram(bins = max(5, floor(res$n/3)), fill = "skyblue", color = "black") +
      geom_vline(xintercept = res$m, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = c(res$li, res$ls), color = "darkgreen", linetype = "dotted", size = 1) +
      theme_minimal() +
      labs(title = "Distribución de las diferencias y CI",
           subtitle = "Línea roja: Media | Líneas verdes: Límites IC",
           x = "Diferencias (Col 1 - Col 2)", y = "Frecuencia")
  })
  
  # ================= HIPÓTESIS =================
  res_ph <- eventReactive(input$calcular_ph, {
    d <- difs()
    req(d)
    
    n <- length(d)
    m <- mean(d)
    s <- sd(d)
    se <- s / sqrt(n)
    t_obs <- m / se
    df_val <- n - 1
    alpha <- 1 - input$conf_ph / 100
    
    prueba <- t.test(d, alternative = input$hipotesis)
    
    list(d = d, n = n, m = m, s = s, se = se, t_obs = t_obs, df = df_val,
         alpha = alpha, p_value = prueba$p.value, hipotesis = input$hipotesis)
  })
  
  output$resultado_ph <- renderPrint({
    res <- res_ph()
    
    cat("=====================================\n")
    cat(" Prueba t para muestras pareadas\n")
    cat("=====================================\n")
    cat("Hipótesis nula: μ_d = 0\n")
    cat("Hipótesis alternativa:",
        switch(res$hipotesis,
               "two.sided" = "μ_d ≠ 0",
               "greater"   = "μ_d > 0",
               "less"      = "μ_d < 0"), "\n\n")
    
    cat("Nivel de significancia (α):", res$alpha, "\n")
    cat("Tamaño de muestra (n):", res$n, "\n")
    cat("Media de diferencias:", round(res$m, 4), "\n")
    cat("Desviación estándar:", round(res$s, 4), "\n")
    cat("Error estándar:", round(res$se, 4), "\n")
    cat("Grados de libertad:", res$df, "\n")
    cat("t observado:", round(res$t_obs, 4), "\n")
    cat("Valor p:", formatC(res$p_value, format = "e", digits = 4), "\n\n")
    
    if(res$p_value < res$alpha){
      cat("🟢 Se rechaza H₀ (Hay evidencia estadísticamente significativa)\n")
    } else {
      cat("🔴 No se rechaza H₀ (No hay suficiente evidencia estadística)\n")
    }
  })
  
  output$grafica_ph <- renderPlot({
    res <- res_ph()
    
    # Eje x dinámico para asegurar que t_obs siempre aparezca
    lim_x <- max(4, abs(res$t_obs) + 0.5)
    x <- seq(-lim_x, lim_x, length = 1000)
    y <- dt(x, res$df)
    df_plot <- data.frame(x, y)
    
    g <- ggplot(df_plot, aes(x, y)) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(title = "Distribución t nula con región de rechazo",
           subtitle = "Línea azul: t crítico | Línea roja punteada: t observado",
           x = "Valor t", y = "Densidad")
    
    if(res$hipotesis == "two.sided"){
      tcrit <- qt(1 - res$alpha/2, res$df)
      g <- g +
        geom_area(data = subset(df_plot, x <= -tcrit), fill = "red", alpha = 0.4) +
        geom_area(data = subset(df_plot, x >= tcrit), fill = "red", alpha = 0.4) +
        geom_vline(xintercept = c(-tcrit, tcrit), color = "blue", linetype = "solid")
    } else if(res$hipotesis == "greater"){
      tcrit <- qt(1 - res$alpha, res$df)
      g <- g +
        geom_area(data = subset(df_plot, x >= tcrit), fill = "red", alpha = 0.4) +
        geom_vline(xintercept = tcrit, color = "blue", linetype = "solid")
    } else if(res$hipotesis == "less"){
      tcrit <- qt(res$alpha, res$df)
      g <- g +
        geom_area(data = subset(df_plot, x <= tcrit), fill = "red", alpha = 0.4) +
        geom_vline(xintercept = tcrit, color = "blue", linetype = "solid")
    }
    
    g + geom_vline(xintercept = res$t_obs, color = "red", linetype = "dashed", size = 1.2)
  })
  
  # ================= TAMAÑO DE MUESTRA =================
  res_tamano <- eventReactive(input$calc_n, {
    req(input$alpha, input$poder, input$sd, input$delta)
    
    res <- power.t.test(delta = input$delta, 
                        sd = input$sd, 
                        sig.level = input$alpha, 
                        power = input$poder, 
                        type = "paired", 
                        alternative = "two.sided")
    
    ceiling(res$n)
  })
  
  output$res_n <- renderText({
    n_est <- res_tamano()
    paste0("Muestra requerida (pares de datos): ", n_est, " observaciones.")
  })
}

shinyApp(ui, server)
