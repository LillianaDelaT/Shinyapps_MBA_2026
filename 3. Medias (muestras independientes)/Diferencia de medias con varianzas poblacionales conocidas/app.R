# =========================================================
# DIFERENCIA DE MEDIAS - MUESTRAS INDEPENDIENTES
# VARIANZAS POBLACIONALES CONOCIDAS
# =========================================================

# =========================================================
# INSTALACIÓN Y CARGA DE PAQUETES
# =========================================================

paquetes <- c("shiny", "ggplot2", "bslib")
instalar <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]

if(length(instalar) > 0){
  install.packages(instalar)
}

library(shiny)
library(ggplot2)
library(bslib)

# =========================================================
# UI
# =========================================================

# Estilo CSS para compactar márgenes, entradas y rellenos
css_compacto <- "
  .well { padding: 12px !important; }
  .form-group { margin-bottom: 8px !important; }
  label { margin-bottom: 2px !important; font-size: 13px; font-weight: 600; }
  .form-control, .selectize-input { height: 32px !important; padding: 2px 8px !important; font-size: 13px !important; }
  hr { margin-top: 8px !important; margin-bottom: 8px !important; }
  h4 { margin-top: 5px !important; margin-bottom: 8px !important; font-size: 15px !important; font-weight: bold; }
  .btn { padding: 4px 12px !important; font-size: 14px !important; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(css_compacto))),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#003366"
  ),
  
  titlePanel(
    div(
      style = "text-align:center;",
      h3("Diferencia de medias de muestras independientes", style = "font-weight:bold; margin-bottom: 2px;"),
      h5("Varianzas poblacionales conocidas", style = "color:#555555; margin-top:0px;"),
      tags$small(
        style = "font-size:75%; display:block; margin-top:2px; font-style:italic; color:#666666;",
        "App desarrollada por Liliana De la Torre Desentis"
      )
    )
  ),
  
  br(),
  
  tabsetPanel(
    
    # =========================================================
    # INTERVALO DE CONFIANZA
    # =========================================================
    tabPanel(
      "Intervalo de confianza",
      br(),
      withMathJax(
        h5("Fórmula utilizada", style = "margin-bottom:0px;"),
        HTML("$$IC = (\\bar{X}_1 - \\bar{X}_2) \\pm Z_{\\alpha/2} \\sqrt{\\frac{\\sigma_1^2}{n_1} + \\frac{\\sigma_2^2}{n_2}}$$")
      ),
      sidebarLayout(
        sidebarPanel(
          width = 5,
          h4("Datos de las Muestras"),
          
          # Grupos lado a lado
          fluidRow(
            column(6,
              h5("Grupo 1", style = "color:#003366; font-size:14px;"),
              numericInput("media1_ic", "X̄₁", value = 50),
              numericInput("sigma1_ic", "σ₁", value = 10, min = 0.0001),
              numericInput("n1_ic", "n₁", value = 30, min = 2, step = 1)
            ),
            column(6,
              h5("Grupo 2", style = "color:#003366; font-size:14px;"),
              numericInput("media2_ic", "X̄₂", value = 45),
              numericInput("sigma2_ic", "σ₂", value = 12, min = 0.0001),
              numericInput("n2_ic", "n₂", value = 35, min = 2, step = 1)
            )
          ),
          
          tags$hr(),
          
          fluidRow(
            column(8, numericInput("conf_ic", "Nivel de confianza (%)", value = 95, min = 80, max = 99.9, step = 0.1)),
            column(4, style = "margin-top: 22px;", actionButton("calcular_ic", "Calcular", class = "btn-primary w-100"))
          )
        ),
        
        mainPanel(
          width = 7,
          h4("Resultados"),
          verbatimTextOutput("resultado_ic"),
          plotOutput("grafica_ic", height = "380px")
        )
      )
    ),
    
    # =========================================================
    # PRUEBA DE HIPÓTESIS
    # =========================================================
    tabPanel(
      "Prueba de hipótesis",
      br(),
      withMathJax(
        h5("Estadístico de prueba", style = "margin-bottom:0px;"),
        HTML("$$Z = \\frac{(\\bar{X}_1 - \\bar{X}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\frac{\\sigma_1^2}{n_1} + \\frac{\\sigma_2^2}{n_2}}}$$")
      ),
      sidebarLayout(
        sidebarPanel(
          width = 5,
          h4("Datos de las Muestras"),
          
          # Grupos lado a lado
          fluidRow(
            column(6,
              h5("Grupo 1", style = "color:#003366; font-size:14px;"),
              numericInput("media1_h", "X̄₁", value = 50),
              numericInput("sigma1_h", "σ₁", value = 10, min = 0.0001),
              numericInput("n1_h", "n₁", value = 30, min = 2, step = 1)
            ),
            column(6,
              h5("Grupo 2", style = "color:#003366; font-size:14px;"),
              numericInput("media2_h", "X̄₂", value = 45),
              numericInput("sigma2_h", "σ₂", value = 12, min = 0.0001),
              numericInput("n2_h", "n₂", value = 35, min = 2, step = 1)
            )
          ),
          
          tags$hr(),
          
          fluidRow(
            column(4, numericInput("d0_h", "Diff (μ₁-μ₂)", value = 0)),
            column(4, selectInput("tipo_h", "Prueba (Ha)", choices = c("Menor", "Mayor", "Diferente"))),
            column(4, numericInput("nivelconf_h", "Significancia (%)", value = 5, min = 0.1, max = 20, step = 0.1))
          ),
          
          div(style = "text-align: right; margin-top: 5px;",
            actionButton("calcular_h", "Calcular", class = "btn-primary w-100")
          )
        ),
        
        mainPanel(
          width = 7,
          h4("Resultados"),
          verbatimTextOutput("resultado_h"),
          plotOutput("grafica_h", height = "380px")
        )
      )
    ),
    
    # =========================================================
    # TAMAÑO DE MUESTRA
    # =========================================================
    tabPanel(
      "Tamaño de muestra",
      br(),
      withMathJax(
        h5("Fórmula utilizada", style = "margin-bottom:0px;"),
        HTML("$$n = \\frac{(Z_{\\alpha/2}+Z_{\\beta})^2 (\\sigma_1^2+\\sigma_2^2)}{d^2}$$")
      ),
      helpText("Se asume n₁ = n₂.", style = "margin-bottom: 5px;"),
      sidebarLayout(
        sidebarPanel(
          width = 5,
          h4("Parámetros del Cálculo"),
          
          fluidRow(
            column(6, numericInput("sigma1_n", "σ₁", value = 10, min = 0.0001)),
            column(6, numericInput("sigma2_n", "σ₂", value = 12, min = 0.0001))
          ),
          
          numericInput("d_n", "Diferencia mínima detectable (d)", value = 5, min = 0.0001),
          
          fluidRow(
            column(6, numericInput("conf_n", "Confianza (%)", value = 95, min = 80, max = 99.9, step = 0.1)),
            column(6, numericInput("power_n", "Potencia (%)", value = 80, min = 50, max = 99.9, step = 0.1))
          ),
          
          br(),
          actionButton("calcular_n", "Calcular", class = "btn-primary w-100")
        ),
        
        mainPanel(
          width = 7,
          h4("Resultados"),
          verbatimTextOutput("resultado_n")
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session){
  
  # --- INTERVALO DE CONFIANZA ---
  datos_ic <- eventReactive(input$calcular_ic, {
    validate(
      need(input$n1_ic >= 2, "n₁ debe ser mayor o igual a 2"),
      need(input$n2_ic >= 2, "n₂ debe ser mayor o igual a 2"),
      need(input$sigma1_ic > 0, "σ₁ debe ser positiva"),
      need(input$sigma2_ic > 0, "σ₂ debe ser positiva"),
      need(input$conf_ic > 0 & input$conf_ic < 100, "El nivel de confianza debe estar entre 0% y 100%")
    )
    
    diferencia <- input$media1_ic - input$media2_ic
    error_estandar <- sqrt(input$sigma1_ic^2/input$n1_ic + input$sigma2_ic^2/input$n2_ic)
    alfa <- 1 - input$conf_ic/100
    z_critico <- qnorm(1 - alfa/2)
    
    li <- diferencia - z_critico * error_estandar
    ls <- diferencia + z_critico * error_estandar
    margen_error <- z_critico * error_estandar
    
    list(
      diferencia = diferencia,
      error_estandar = error_estandar,
      alfa = alfa,
      z_critico = z_critico,
      li = li,
      ls = ls,
      margen_error = margen_error
    )
  }, ignoreNULL = FALSE)
  
  output$resultado_ic <- renderPrint({
    datos <- datos_ic()
    cat("Diferencia muestral:", round(datos$diferencia, 4), "\n")
    cat("Valor crítico (Zα/2):", round(datos$z_critico, 4), "\n")
    cat("Error estándar:", round(datos$error_estandar, 4), "\n")
    cat("Margen de error:", round(datos$margen_error, 4), "\n")
    cat("α:", round(datos$alfa, 4), "\n\n")
    cat("Intervalo de confianza al", input$conf_ic, "%:\n")
    cat("[", round(datos$li, 4), ",", round(datos$ls, 4), "]\n")
  })
  
  output$grafica_ic <- renderPlot({
    datos <- datos_ic()
    x <- seq(datos$diferencia - 4*datos$error_estandar, datos$diferencia + 4*datos$error_estandar, length = 1000)
    y <- dnorm(x, datos$diferencia, datos$error_estandar)
    df <- data.frame(x, y)
    
    ggplot(df, aes(x, y)) +
      geom_line(linewidth = 1.2, color = "#003366") +
      geom_area(data = subset(df, x >= datos$li & x <= datos$ls), fill = "skyblue", alpha = 0.5) +
      geom_vline(xintercept = datos$diferencia, color = "red", linewidth = 1) +
      geom_vline(xintercept = c(datos$li, datos$ls), linetype = "dashed", color = "darkblue", linewidth = 1) +
      annotate("text", x = datos$li, y = max(y)*0.9, label = paste0("LI = ", round(datos$li, 2)), color = "darkblue") +
      annotate("text", x = datos$ls, y = max(y)*0.9, label = paste0("LS = ", round(datos$ls, 2)), color = "darkblue") +
      labs(
        title = "Intervalo de confianza",
        subtitle = paste0("Nivel de confianza: ", input$conf_ic, "%"),
        x = "Diferencia de medias",
        y = "Densidad"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # --- PRUEBA DE HIPÓTESIS ---
  datos_h <- eventReactive(input$calcular_h, {
    validate(
      need(input$n1_h >= 2, "n₁ debe ser mayor o igual a 2"),
      need(input$n2_h >= 2, "n₂ debe ser mayor o igual a 2"),
      need(input$sigma1_h > 0, "σ₁ debe ser positiva"),
      need(input$sigma2_h > 0, "σ₂ debe ser positiva"),
      need(input$nivelconf_h > 0 & input$nivelconf_h < 100, "El nivel de significancia debe estar entre 0% y 100%")
    )
    
    diferencia <- input$media1_h - input$media2_h
    error_estandar <- sqrt(input$sigma1_h^2/input$n1_h + input$sigma2_h^2/input$n2_h)
    z <- (diferencia - input$d0_h) / error_estandar
    alfa <- input$nivelconf_h/100
    
    if(input$tipo_h == "Menor"){
      valor_critico <- qnorm(alfa)
      region_rechazo <- paste0("Z < ", round(valor_critico, 4))
      p_value <- pnorm(z)
    } else if(input$tipo_h == "Mayor"){
      valor_critico <- qnorm(1 - alfa)
      region_rechazo <- paste0("Z > ", round(valor_critico, 4))
      p_value <- 1 - pnorm(z)
    } else {
      valor_critico <- qnorm(1 - alfa/2)
      region_rechazo <- paste0("Z < ", round(-valor_critico, 4), " ó Z > ", round(valor_critico, 4))
      p_value <- 2 * (1 - pnorm(abs(z)))
    }
    
    list(
      diferencia = diferencia,
      diferencia_poblacional = input$d0_h,
      error_estandar = error_estandar,
      z = z,
      alfa = alfa,
      valor_critico = valor_critico,
      region_rechazo = region_rechazo,
      p_value = p_value
    )
  }, ignoreNULL = FALSE)
  
  output$resultado_h <- renderPrint({
    datos <- datos_h()
    cat("Diferencia muestral (X̄₁-X̄₂):", round(datos$diferencia, 4), "\n")
    cat("Diferencia poblacional (μ₁−μ₂):", round(datos$diferencia_poblacional, 4), "\n\n")
    cat("Error estándar:", round(datos$error_estandar, 4), "\n")
    cat("Valor crítico:", round(datos$valor_critico, 4), "\n")
    cat("Región de rechazo:", datos$region_rechazo, "\n\n")
    cat("Estadístico Z:", round(datos$z, 4), "\n\n")
    cat("p-value:", round(datos$p_value, 6))
  })
  
  output$grafica_h <- renderPlot({
    datos <- datos_h()
    x <- seq(-4, 4, length = 2000)
    y <- dnorm(x)
    df <- data.frame(x, y)
    
    p <- ggplot(df, aes(x, y)) +
      geom_line(linewidth = 1.2, color = "black") +
      labs(title = "Distribución normal estándar", x = "Z", y = "Densidad") +
      theme_minimal(base_size = 14)
    
    if(input$tipo_h == "Mayor"){
      p <- p + geom_area(data = subset(df, x >= datos$z), fill = "red", alpha = 0.4)
    } else if(input$tipo_h == "Menor"){
      p <- p + geom_area(data = subset(df, x <= datos$z), fill = "red", alpha = 0.4)
    } else {
      p <- p + 
        geom_area(data = subset(df, x <= -abs(datos$z)), fill = "red", alpha = 0.4) +
        geom_area(data = subset(df, x >= abs(datos$z)), fill = "red", alpha = 0.4)
    }
    
    p <- p +
      geom_vline(xintercept = datos$z, color = "blue", linewidth = 1.3) +
      annotate("text", x = datos$z, y = max(y)*0.95, label = paste0("Z = ", round(datos$z, 4)), color = "blue") +
      annotate("text", x = 0, y = max(y)*0.80, label = paste0("p-value = ", round(datos$p_value, 6)), color = "red", size = 5)
    
    p
  })
  
  # --- TAMAÑO DE MUESTRA ---
  datos_n <- eventReactive(input$calcular_n, {
    validate(
      need(input$sigma1_n > 0, "σ₁ debe ser positiva"),
      need(input$sigma2_n > 0, "σ₂ debe ser positiva"),
      need(input$d_n > 0, "La diferencia mínima detectable debe ser positiva"),
      need(input$conf_n > 0 & input$conf_n < 100, "El nivel de confianza debe estar entre 0% y 100%"),
      need(input$power_n > 0 & input$power_n < 100, "La potencia debe estar entre 0% y 100%")
    )
    
    alfa <- 1 - input$conf_n/100
    beta <- 1 - input$power_n/100
    z_alfa <- qnorm(1 - alfa/2)
    z_beta <- qnorm(1 - beta)
    
    n <- ((z_alfa + z_beta)^2 * (input$sigma1_n^2 + input$sigma2_n^2)) / (input$d_n^2)
    
    list(
      alfa = alfa,
      beta = beta,
      z_alfa = z_alfa,
      z_beta = z_beta,
      n_final = ceiling(n)
    )
  }, ignoreNULL = FALSE)
  
  output$resultado_n <- renderPrint({
    datos <- datos_n()
    cat("α:", round(datos$alfa, 4), "\n")
    cat("β:", round(datos$beta, 4), "\n")
    cat("Zα/2:", round(datos$z_alfa, 4), "\n")
    cat("Zβ:", round(datos$z_beta, 4), "\n\n")
    cat("Tamaño de muestra requerido por grupo:\n")
    cat("n =", datos$n_final)
  })
}

# =========================================================
# EJECUTAR APP
# =========================================================

shinyApp(ui, server)