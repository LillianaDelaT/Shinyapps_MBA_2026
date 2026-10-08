library(shiny)
library(ggplot2)

# ============================================================
# FUNCIONES AUXILIARES
# ============================================================

# ------------------------------------------------------------
# Validación para una varianza
# ------------------------------------------------------------

validar_una_varianza <- function(n, s2, conf = NULL, alpha = NULL) {
  
  if (is.null(n) || is.na(n) || n <= 1) {
    return("El tamaño de muestra n debe ser mayor que 1.")
  }
  
  if (n != round(n)) {
    return("El tamaño de muestra n debe ser un número entero.")
  }
  
  if (is.null(s2) || is.na(s2) || s2 <= 0) {
    return("La varianza muestral S² debe ser mayor que 0.")
  }
  
  if (!is.null(conf)) {
    if (is.na(conf) || conf <= 0 || conf >= 100) {
      return("El nivel de confianza debe estar entre 0% y 100%.")
    }
  }
  
  if (!is.null(alpha)) {
    if (is.na(alpha) || alpha <= 0 || alpha >= 1) {
      return("El nivel de significancia α debe estar entre 0 y 1.")
    }
  }
  
  return(NULL)
}


# ------------------------------------------------------------
# Validación para dos varianzas
# ------------------------------------------------------------

validar_dos_varianzas <- function(n1, n2, s1, s2,
                                  conf = NULL, alpha = NULL) {
  
  if (is.null(n1) || is.na(n1) || n1 <= 1) {
    return("El tamaño de muestra n1 debe ser mayor que 1.")
  }
  
  if (is.null(n2) || is.na(n2) || n2 <= 1) {
    return("El tamaño de muestra n2 debe ser mayor que 1.")
  }
  
  if (n1 != round(n1) || n2 != round(n2)) {
    return("Los tamaños de muestra n1 y n2 deben ser números enteros.")
  }
  
  if (is.null(s1) || is.na(s1) || s1 <= 0) {
    return("La varianza muestral S1² debe ser mayor que 0.")
  }
  
  if (is.null(s2) || is.na(s2) || s2 <= 0) {
    return("La varianza muestral S2² debe ser mayor que 0.")
  }
  
  if (!is.null(conf)) {
    if (is.na(conf) || conf <= 0 || conf >= 100) {
      return("El nivel de confianza debe estar entre 0% y 100%.")
    }
  }
  
  if (!is.null(alpha)) {
    if (is.na(alpha) || alpha <= 0 || alpha >= 1) {
      return("El nivel de significancia α debe estar entre 0 y 1.")
    }
  }
  
  return(NULL)
}


# ------------------------------------------------------------
# Hipótesis para una varianza
# ------------------------------------------------------------

hipotesis_chi <- function(tipo, sigma0) {
  
  if (tipo == "Bilateral") {
    
    h0 <- paste0("H₀: σ² = ", sigma0)
    ha <- paste0("Hₐ: σ² ≠ ", sigma0)
    
  } else if (tipo == "Cola inferior") {
    
    h0 <- paste0("H₀: σ² ≥ ", sigma0)
    ha <- paste0("Hₐ: σ² < ", sigma0)
    
  } else {
    
    h0 <- paste0("H₀: σ² ≤ ", sigma0)
    ha <- paste0("Hₐ: σ² > ", sigma0)
  }
  
  list(h0 = h0, ha = ha)
}


# ------------------------------------------------------------
# Hipótesis para dos varianzas
# ------------------------------------------------------------

hipotesis_F <- function(tipo) {
  
  if (tipo == "Bilateral") {
    
    h0 <- "H₀: σ₁² = σ₂²"
    ha <- "Hₐ: σ₁² ≠ σ₂²"
    
  } else if (tipo == "Cola inferior") {
    
    h0 <- "H₀: σ₁² ≥ σ₂²"
    ha <- "Hₐ: σ₁² < σ₂²"
    
  } else {
    
    h0 <- "H₀: σ₁² ≤ σ₂²"
    ha <- "Hₐ: σ₁² > σ₂²"
  }
  
  list(h0 = h0, ha = ha)
}


# ============================================================
# INTERFAZ
# ============================================================

ui <- fluidPage(
  
  titlePanel("Inferencia sobre varianzas y cociente de varianzas"),
  
  tags$div(
    "App desarrollada por Liliana De la Torre Desentis",
    style = "font-size:80%; font-style:italic;"
  ),
  
  br(),
  
  radioButtons(
    "tipo",
    "Seleccione el caso:",
    choices = c(
      "Una varianza" = "una",
      "Comparación de varianzas" = "dos"
    ),
    inline = TRUE
  ),
  
  uiOutput("paneles")
)


# ============================================================
# SERVIDOR
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # PANELES
  # ==========================================================
  
  output$paneles <- renderUI({
    
    if (input$tipo == "una") {
      
      tabsetPanel(
        
        # ====================================================
        # INTERVALO DE CONFIANZA - UNA VARIANZA
        # ====================================================
        
        tabPanel(
          "Intervalo de confianza",
          
          sidebarLayout(
            
            sidebarPanel(
              
              h4("Datos"),
              
              numericInput(
                "n1",
                "n:",
                value = 20,
                min = 2,
                step = 1
              ),
              
              numericInput(
                "s2_1",
                "S²:",
                value = 4,
                min = 0.000001,
                step = 0.1
              ),
              
              numericInput(
                "conf1",
                "Confianza (%):",
                value = 95,
                min = 0.1,
                max = 99.9,
                step = 1
              ),
              
              actionButton(
                "calc_ic1",
                "Calcular",
                class = "btn-primary"
              ),
              
              br(),
              br(),
              
              tags$div(
                style = "font-size:90%;",
                tags$b("Supuestos:"),
                tags$ul(
                  tags$li(
                    "La población se distribuye normalmente."
                  ),
                  tags$li(
                    "La muestra es aleatoria e independiente."
                  )
                )
              )
            ),
            
            mainPanel(
              
              h4("Resultados"),
              
              verbatimTextOutput("res_ic1"),
              
              plotOutput(
                "plot_ic1",
                height = "500px"
              )
            )
          )
        ),
        
        
        # ====================================================
        # PRUEBA DE HIPÓTESIS - UNA VARIANZA
        # ====================================================
        
        tabPanel(
          "Prueba de hipótesis",
          
          sidebarLayout(
            
            sidebarPanel(
              
              h4("Datos"),
              
              numericInput(
                "n1_h",
                "n:",
                value = 20,
                min = 2,
                step = 1
              ),
              
              numericInput(
                "s2_1_h",
                "S²:",
                value = 4,
                min = 0.000001,
                step = 0.1
              ),
              
              numericInput(
                "sigma2_0",
                "σ₀²:",
                value = 3,
                min = 0.000001,
                step = 0.1
              ),
              
              selectInput(
                "tipo_chi",
                "Tipo de prueba:",
                choices = c(
                  "Bilateral",
                  "Cola inferior",
                  "Cola superior"
                )
              ),
              
              numericInput(
                "alpha1",
                "α:",
                value = 0.05,
                min = 0.001,
                max = 0.999,
                step = 0.01
              ),
              
              actionButton(
                "calc_test1",
                "Calcular",
                class = "btn-primary"
              ),
              
              br(),
              br(),
              
              tags$div(
                style = "font-size:90%;",
                tags$b("Supuestos:"),
                tags$ul(
                  tags$li(
                    "La población se distribuye normalmente."
                  ),
                  tags$li(
                    "La muestra es aleatoria e independiente."
                  )
                )
              )
            ),
            
            mainPanel(
              
              h4("Resultados"),
              
              verbatimTextOutput("res_test1"),
              
              plotOutput(
                "plot_test1",
                height = "520px"
              )
            )
          )
        )
      )
      
    } else {
      
      tabsetPanel(
        
        # ====================================================
        # INTERVALO DE CONFIANZA - DOS VARIANZAS
        # ====================================================
        
        tabPanel(
          "Intervalo de confianza",
          
          sidebarLayout(
            
            sidebarPanel(
              
              h4("Datos"),
              
              numericInput(
                "n2_1",
                "n1:",
                value = 15,
                min = 2,
                step = 1
              ),
              
              numericInput(
                "n2_2",
                "n2:",
                value = 12,
                min = 2,
                step = 1
              ),
              
              numericInput(
                "s2_g1",
                "S1²:",
                value = 6,
                min = 0.000001,
                step = 0.1
              ),
              
              numericInput(
                "s2_g2",
                "S2²:",
                value = 4,
                min = 0.000001,
                step = 0.1
              ),
              
              numericInput(
                "conf2",
                "Confianza (%):",
                value = 95,
                min = 0.1,
                max = 99.9,
                step = 1
              ),
              
              actionButton(
                "calc_ic2",
                "Calcular",
                class = "btn-primary"
              ),
              
              br(),
              br(),
              
              tags$div(
                style = "font-size:90%;",
                tags$b("Supuestos:"),
                tags$ul(
                  tags$li(
                    "Las dos poblaciones se distribuyen normalmente."
                  ),
                  tags$li(
                    "Las muestras son aleatorias."
                  ),
                  tags$li(
                    "Las dos muestras son independientes entre sí."
                  )
                )
              )
            ),
            
            mainPanel(
              
              h4("Resultados"),
              
              verbatimTextOutput("res_ic2"),
              
              plotOutput(
                "plot_ic2",
                height = "500px"
              )
            )
          )
        ),
        
        
        # ====================================================
        # PRUEBA DE HIPÓTESIS - DOS VARIANZAS
        # ====================================================
        
        tabPanel(
          "Prueba de hipótesis",
          
          sidebarLayout(
            
            sidebarPanel(
              
              h4("Datos"),
              
              numericInput(
                "n2_1h",
                "n1:",
                value = 15,
                min = 2,
                step = 1
              ),
              
              numericInput(
                "n2_2h",
                "n2:",
                value = 12,
                min = 2,
                step = 1
              ),
              
              numericInput(
                "s2_g1h",
                "S1²:",
                value = 6,
                min = 0.000001,
                step = 0.1
              ),
              
              numericInput(
                "s2_g2h",
                "S2²:",
                value = 4,
                min = 0.000001,
                step = 0.1
              ),
              
              selectInput(
                "tipo_F",
                "Tipo de prueba:",
                choices = c(
                  "Bilateral",
                  "Cola inferior",
                  "Cola superior"
                )
              ),
              
              numericInput(
                "alpha2",
                "α:",
                value = 0.05,
                min = 0.001,
                max = 0.999,
                step = 0.01
              ),
              
              actionButton(
                "calc_test2",
                "Calcular",
                class = "btn-primary"
              ),
              
              br(),
              br(),
              
              tags$div(
                style = "font-size:90%;",
                tags$b("Supuestos:"),
                tags$ul(
                  tags$li(
                    "Las dos poblaciones se distribuyen normalmente."
                  ),
                  tags$li(
                    "Las muestras son aleatorias."
                  ),
                  tags$li(
                    "Las dos muestras son independientes entre sí."
                  )
                )
              )
            ),
            
            mainPanel(
              
              h4("Resultados"),
              
              verbatimTextOutput("res_test2"),
              
              plotOutput(
                "plot_test2",
                height = "520px"
              )
            )
          )
        )
      )
    }
  })
  
  
  # ==========================================================
  # INTERVALO DE CONFIANZA - UNA VARIANZA
  # ==========================================================
  
  ic1 <- eventReactive(input$calc_ic1, {
    
    n <- input$n1
    s2 <- input$s2_1
    conf <- input$conf1 / 100
    
    error <- validar_una_varianza(
      n = n,
      s2 = s2,
      conf = input$conf1
    )
    
    validate(
      need(is.null(error), error)
    )
    
    df <- n - 1
    alpha <- 1 - conf
    
    chi_low <- qchisq(
      alpha / 2,
      df
    )
    
    chi_high <- qchisq(
      1 - alpha / 2,
      df
    )
    
    li <- (df * s2) / chi_high
    ls <- (df * s2) / chi_low
    
    list(
      n = n,
      s2 = s2,
      df = df,
      conf = conf,
      alpha = alpha,
      chi_low = chi_low,
      chi_high = chi_high,
      li = li,
      ls = ls,
      sd_li = sqrt(li),
      sd_ls = sqrt(ls)
    )
  })
  
  
  output$res_ic1 <- renderPrint({
    
    d <- ic1()
    
    cat("ESTIMACIÓN POR INTERVALO PARA UNA VARIANZA\n")
    cat("===========================================\n\n")
    
    cat("Tamaño de muestra: n =", d$n, "\n")
    cat("Grados de libertad: gl =", d$df, "\n")
    
    cat(
      "Varianza muestral: S² =",
      round(d$s2, 6),
      "\n"
    )
    
    cat(
      "Desviación estándar muestral: S =",
      round(sqrt(d$s2), 6),
      "\n\n"
    )
    
    cat(
      "Nivel de confianza:",
      round(d$conf * 100, 2),
      "%\n"
    )
    
    cat(
      "Nivel de significancia: α =",
      round(d$alpha, 6),
      "\n\n"
    )
    
    cat("Valores críticos de χ²:\n")
    
    cat(
      "χ² inferior =",
      round(d$chi_low, 6),
      "\n"
    )
    
    cat(
      "χ² superior =",
      round(d$chi_high, 6),
      "\n\n"
    )
    
    cat("Estimación puntual:\n")
    
    cat(
      "σ² ≈ S² =",
      round(d$s2, 6),
      "\n\n"
    )
    
    cat("Intervalo de confianza para σ²:\n")
    
    cat(
      "[",
      round(d$li, 6),
      ", ",
      round(d$ls, 6),
      "]\n\n",
      sep = ""
    )
    
    cat("Intervalo equivalente para σ:\n")
    
    cat(
      "[",
      round(d$sd_li, 6),
      ", ",
      round(d$sd_ls, 6),
      "]\n",
      sep = ""
    )
  })
  
  
  # ==========================================================
  # GRÁFICA IC UNA VARIANZA
  # ==========================================================
  
  output$plot_ic1 <- renderPlot({
    
    d <- ic1()
    
    # Rango suficientemente amplio para mostrar toda la curva
    x_max <- max(
      qchisq(
        0.999,
        d$df
      ),
      d$chi_high * 1.10
    )
    
    x <- seq(
      0,
      x_max,
      length.out = 1500
    )
    
    y <- dchisq(
      x,
      d$df
    )
    
    ymax <- max(y)
    
    par(
      mar = c(
        7,
        5,
        4,
        2
      )
    )
    
    plot(
      x,
      y,
      type = "l",
      lwd = 3,
      xlab = expression(chi^2),
      ylab = "Densidad",
      main = paste0(
        "IC ",
        round(d$conf * 100, 1),
        "% para la varianza poblacional"
      )
    )
    
    # --------------------------------------------------------
    # Región central de confianza
    # --------------------------------------------------------
    
    x_central <- x[
      x >= d$chi_low &
        x <= d$chi_high
    ]
    
    polygon(
      c(
        d$chi_low,
        x_central,
        d$chi_high
      ),
      c(
        0,
        dchisq(
          x_central,
          d$df
        ),
        0
      ),
      col = rgb(
        0,
        0.7,
        1,
        0.25
      ),
      border = NA
    )
    
    # --------------------------------------------------------
    # Cola izquierda
    # --------------------------------------------------------
    
    x_left <- x[
      x <= d$chi_low
    ]
    
    polygon(
      c(
        0,
        x_left,
        d$chi_low
      ),
      c(
        0,
        dchisq(
          x_left,
          d$df
        ),
        0
      ),
      col = rgb(
        0.8,
        0.2,
        0.2,
        0.25
      ),
      border = NA
    )
    
    # --------------------------------------------------------
    # Cola derecha
    # --------------------------------------------------------
    
    x_right <- x[
      x >= d$chi_high
    ]
    
    polygon(
      c(
        d$chi_high,
        x_right,
        x_max
      ),
      c(
        0,
        dchisq(
          x_right,
          d$df
        ),
        0
      ),
      col = rgb(
        0.8,
        0.2,
        0.2,
        0.25
      ),
      border = NA
    )
    
    # --------------------------------------------------------
    # Valores críticos
    # --------------------------------------------------------
    
    abline(
      v = d$chi_low,
      lty = 2,
      lwd = 2
    )
    
    abline(
      v = d$chi_high,
      lty = 2,
      lwd = 2
    )
    
    # --------------------------------------------------------
    # Etiquetas de los valores críticos
    # --------------------------------------------------------
    
    text(
      d$chi_low,
      ymax * 0.35,
      paste0(
        "χ² = ",
        round(d$chi_low, 3)
      ),
      pos = 2,
      cex = 0.85
    )
    
    text(
      d$chi_high,
      ymax * 0.35,
      paste0(
        "χ² = ",
        round(d$chi_high, 3)
      ),
      pos = 4,
      cex = 0.85
    )
    
    # --------------------------------------------------------
    # Etiquetas de las colas
    # --------------------------------------------------------
    
    text(
      d$chi_low / 2,
      ymax * 0.12,
      expression(alpha/2),
      cex = 1
    )
    
    text(
      (
        d$chi_high +
          x_max
      ) / 2,
      ymax * 0.12,
      expression(alpha/2),
      cex = 1
    )
    
    # --------------------------------------------------------
    # Etiqueta de la región central
    # --------------------------------------------------------
    
    text(
      (
        d$chi_low +
          d$chi_high
      ) / 2,
      ymax * 0.25,
      paste0(
        round(d$conf * 100, 1),
        "% de confianza"
      ),
      cex = 0.9
    )
    
    # --------------------------------------------------------
    # Información del IC debajo de la gráfica
    # --------------------------------------------------------
    
    mtext(
      paste0(
        "IC para σ²: [",
        round(d$li, 3),
        ", ",
        round(d$ls, 3),
        "]"
      ),
      side = 1,
      line = 4.0,
      cex = 0.95
    )
    
    # --------------------------------------------------------
    # Correspondencia
    # --------------------------------------------------------
    
    mtext(
      paste0(
        "χ² inferior → LS = ",
        round(d$ls, 3),
        "       χ² superior → LI = ",
        round(d$li, 3)
      ),
      side = 1,
      line = 5.3,
      cex = 0.85
    )
  })
  
  
  # ==========================================================
  # PRUEBA DE HIPÓTESIS - UNA VARIANZA
  # ==========================================================
  
  test1 <- eventReactive(input$calc_test1, {
    
    n <- input$n1_h
    s2 <- input$s2_1_h
    sig0 <- input$sigma2_0
    df <- n - 1
    tipo <- input$tipo_chi
    a <- input$alpha1
    
    error <- validar_una_varianza(
      n = n,
      s2 = s2,
      alpha = a
    )
    
    validate(
      need(
        is.null(error),
        error
      ),
      
      need(
        !is.na(sig0) &&
          sig0 > 0,
        "El valor σ₀² debe ser mayor que 0."
      )
    )
    
    stat <- (
      df * s2
    ) / sig0
    
    if (tipo == "Bilateral") {
      
      cl <- qchisq(
        a / 2,
        df
      )
      
      ch <- qchisq(
        1 - a / 2,
        df
      )
      
      p <- 2 * min(
        pchisq(
          stat,
          df
        ),
        1 - pchisq(
          stat,
          df
        )
      )
      
    } else if (tipo == "Cola inferior") {
      
      cl <- qchisq(
        a,
        df
      )
      
      ch <- NA
      
      p <- pchisq(
        stat,
        df
      )
      
    } else {
      
      cl <- NA
      
      ch <- qchisq(
        1 - a,
        df
      )
      
      p <- 1 - pchisq(
        stat,
        df
      )
    }
    
    h <- hipotesis_chi(
      tipo,
      sig0
    )
    
    decision <- ifelse(
      p < a,
      "Rechazar H₀",
      "No rechazar H₀"
    )
    
    if (p < a) {
      
      if (tipo == "Bilateral") {
        
        conclusion <- paste0(
          "Existe evidencia estadísticamente significativa ",
          "para concluir que la varianza poblacional es ",
          "diferente de σ₀²."
        )
        
      } else if (tipo == "Cola inferior") {
        
        conclusion <- paste0(
          "Existe evidencia estadísticamente significativa ",
          "para concluir que la varianza poblacional es ",
          "menor que σ₀²."
        )
        
      } else {
        
        conclusion <- paste0(
          "Existe evidencia estadísticamente significativa ",
          "para concluir que la varianza poblacional es ",
          "mayor que σ₀²."
        )
      }
      
    } else {
      
      if (tipo == "Bilateral") {
        
        conclusion <- paste0(
          "No existe evidencia estadísticamente suficiente ",
          "para concluir que la varianza poblacional sea ",
          "diferente de σ₀²."
        )
        
      } else if (tipo == "Cola inferior") {
        
        conclusion <- paste0(
          "No existe evidencia estadísticamente suficiente ",
          "para concluir que la varianza poblacional sea ",
          "menor que σ₀²."
        )
        
      } else {
        
        conclusion <- paste0(
          "No existe evidencia estadísticamente suficiente ",
          "para concluir que la varianza poblacional sea ",
          "mayor que σ₀²."
        )
      }
    }
    
    list(
      n = n,
      s2 = s2,
      sig0 = sig0,
      df = df,
      stat = stat,
      tipo = tipo,
      a = a,
      cl = cl,
      ch = ch,
      p = p,
      h0 = h$h0,
      ha = h$ha,
      decision = decision,
      conclusion = conclusion
    )
  })
  
  
  output$res_test1 <- renderPrint({
    
    d <- test1()
    
    cat(
      "PRUEBA DE HIPÓTESIS PARA UNA VARIANZA\n"
    )
    
    cat(
      "=====================================\n\n"
    )
    
    cat(
      "Hipótesis:\n"
    )
    
    cat(
      d$h0,
      "\n"
    )
    
    cat(
      d$ha,
      "\n\n"
    )
    
    cat(
      "Nivel de significancia: α =",
      round(d$a, 6),
      "\n"
    )
    
    cat(
      "Grados de libertad: gl =",
      d$df,
      "\n\n"
    )
    
    cat(
      "Estadístico de prueba:\n"
    )
    
    cat(
      "χ² = (n - 1)S² / σ₀² =",
      round(d$stat, 6),
      "\n\n"
    )
    
    cat(
      "Región de rechazo:\n"
    )
    
    if (d$tipo == "Bilateral") {
      
      cat(
        "χ² <",
        round(d$cl, 6),
        "o χ² >",
        round(d$ch, 6),
        "\n\n"
      )
      
    } else if (d$tipo == "Cola inferior") {
      
      cat(
        "χ² <",
        round(d$cl, 6),
        "\n\n"
      )
      
    } else {
      
      cat(
        "χ² >",
        round(d$ch, 6),
        "\n\n"
      )
    }
    
    cat(
      "p-value =",
      round(d$p, 6),
      "\n\n"
    )
    
    cat(
      "Decisión:",
      d$decision,
      "\n\n"
    )
    
    cat(
      "Conclusión:\n"
    )
    
    cat(
      d$conclusion,
      "\n"
    )
  })
  
  
  # ==========================================================
  # GRÁFICA PRUEBA UNA VARIANZA
  # ==========================================================
  
  output$plot_test1 <- renderPlot({
    
    d <- test1()
    
    # --------------------------------------------------------
    # Rango
    # --------------------------------------------------------
    
    q999 <- qchisq(
      0.999,
      d$df
    )
    
    xmax <- max(
      q999,
      d$stat * 1.15,
      ifelse(
        is.na(d$cl),
        0,
        d$cl
      ) * 1.15,
      ifelse(
        is.na(d$ch),
        0,
        d$ch
      ) * 1.15
    )
    
    x <- seq(
      0,
      xmax,
      length.out = 1500
    )
    
    y <- dchisq(
      x,
      d$df
    )
    
    ymax <- max(y)
    
    par(
      mar = c(
        6,
        5,
        4,
        2
      )
    )
    
    # --------------------------------------------------------
    # Curva
    # --------------------------------------------------------
    
    plot(
      x,
      y,
      type = "l",
      lwd = 3,
      xlab = expression(chi^2),
      ylab = "Densidad",
      main = paste0(
        "Prueba χ² para una varianza — ",
        d$tipo
      )
    )
    
    # --------------------------------------------------------
    # Regiones de rechazo
    # --------------------------------------------------------
    
    if (d$tipo == "Bilateral") {
      
      # Cola izquierda
      x_left <- x[
        x <= d$cl
      ]
      
      polygon(
        c(
          0,
          x_left,
          d$cl
        ),
        c(
          0,
          dchisq(
            x_left,
            d$df
          ),
          0
        ),
        col = rgb(
          0.8,
          0.2,
          0.2,
          0.25
        ),
        border = NA
      )
      
      # Cola derecha
      x_right <- x[
        x >= d$ch
      ]
      
      polygon(
        c(
          d$ch,
          x_right,
          xmax
        ),
        c(
          0,
          dchisq(
            x_right,
            d$df
          ),
          0
        ),
        col = rgb(
          0.8,
          0.2,
          0.2,
          0.25
        ),
        border = NA
      )
      
      abline(
        v = d$cl,
        lty = 2,
        lwd = 2
      )
      
      abline(
        v = d$ch,
        lty = 2,
        lwd = 2
      )
      
      # Etiquetas
      text(
        d$cl,
        ymax * 0.25,
        paste0(
          "χ² crítico = ",
          round(d$cl, 3)
        ),
        pos = 2,
        cex = 0.82
      )
      
      text(
        d$ch,
        ymax * 0.25,
        paste0(
          "χ² crítico = ",
          round(d$ch, 3)
        ),
        pos = 4,
        cex = 0.82
      )
      
    } else if (d$tipo == "Cola inferior") {
      
      x_left <- x[
        x <= d$cl
      ]
      
      polygon(
        c(
          0,
          x_left,
          d$cl
        ),
        c(
          0,
          dchisq(
            x_left,
            d$df
          ),
          0
        ),
        col = rgb(
          0.8,
          0.2,
          0.2,
          0.25
        ),
        border = NA
      )
      
      abline(
        v = d$cl,
        lty = 2,
        lwd = 2
      )
      
      text(
        d$cl,
        ymax * 0.25,
        paste0(
          "χ² crítico = ",
          round(d$cl, 3)
        ),
        pos = 4,
        cex = 0.82
      )
      
    } else {
      
      x_right <- x[
        x >= d$ch
      ]
      
      polygon(
        c(
          d$ch,
          x_right,
          xmax
        ),
        c(
          0,
          dchisq(
            x_right,
            d$df
          ),
          0
        ),
        col = rgb(
          0.8,
          0.2,
          0.2,
          0.25
        ),
        border = NA
      )
      
      abline(
        v = d$ch,
        lty = 2,
        lwd = 2
      )
      
      text(
        d$ch,
        ymax * 0.25,
        paste0(
          "χ² crítico = ",
          round(d$ch, 3)
        ),
        pos = 2,
        cex = 0.82
      )
    }
    
    # --------------------------------------------------------
    # Estadístico observado
    # --------------------------------------------------------
    
    abline(
      v = d$stat,
      lwd = 3
    )
    
    # Etiqueta colocada debajo del eje
    mtext(
      paste0(
        "χ² observado = ",
        round(d$stat, 3)
      ),
      side = 1,
      line = 2.5,
      cex = 0.90
    )
    
    # --------------------------------------------------------
    # Nota didáctica
    # --------------------------------------------------------
    
    mtext(
      paste0(
        "Las áreas sombreadas corresponden a la región de rechazo."
      ),
      side = 1,
      line = 4.0,
      cex = 0.82
    )
  })
  
  
  # ==========================================================
  # INTERVALO DE CONFIANZA - DOS VARIANZAS
  # ==========================================================
  
  ic2 <- eventReactive(input$calc_ic2, {
    
    n1 <- input$n2_1
    n2 <- input$n2_2
    s1 <- input$s2_g1
    s2 <- input$s2_g2
    conf <- input$conf2 / 100
    
    error <- validar_dos_varianzas(
      n1 = n1,
      n2 = n2,
      s1 = s1,
      s2 = s2,
      conf = input$conf2
    )
    
    validate(
      need(
        is.null(error),
        error
      )
    )
    
    df1 <- n1 - 1
    df2 <- n2 - 1
    
    alpha <- 1 - conf
    
    f_low <- qf(
      alpha / 2,
      df1,
      df2
    )
    
    f_high <- qf(
      1 - alpha / 2,
      df1,
      df2
    )
    
    f_stat <- s1 / s2
    
    li <- f_stat / f_high
    ls <- f_stat / f_low
    
    list(
      n1 = n1,
      n2 = n2,
      s1 = s1,
      s2 = s2,
      df1 = df1,
      df2 = df2,
      conf = conf,
      alpha = alpha,
      f_low = f_low,
      f_high = f_high,
      f_stat = f_stat,
      li = li,
      ls = ls,
      sd_li = sqrt(li),
      sd_ls = sqrt(ls)
    )
  })
  
  
  output$res_ic2 <- renderPrint({
    
    d <- ic2()
    
    cat(
      "ESTIMACIÓN POR INTERVALO PARA EL COCIENTE DE VARIANZAS\n"
    )
    
    cat(
      "========================================================\n\n"
    )
    
    cat(
      "Tamaño de muestra 1: n1 =",
      d$n1,
      "\n"
    )
    
    cat(
      "Tamaño de muestra 2: n2 =",
      d$n2,
      "\n\n"
    )
    
    cat(
      "Grados de libertad:\n"
    )
    
    cat(
      "gl1 =",
      d$df1,
      "\n"
    )
    
    cat(
      "gl2 =",
      d$df2,
      "\n\n"
    )
    
    cat(
      "Varianza muestral 1: S1² =",
      round(d$s1, 6),
      "\n"
    )
    
    cat(
      "Varianza muestral 2: S2² =",
      round(d$s2, 6),
      "\n\n"
    )
    
    cat(
      "Estimación puntual:\n"
    )
    
    cat(
      "S1² / S2² =",
      round(d$f_stat, 6),
      "\n\n"
    )
    
    cat(
      "Nivel de confianza:",
      round(d$conf * 100, 2),
      "%\n"
    )
    
    cat(
      "Nivel de significancia: α =",
      round(d$alpha, 6),
      "\n\n"
    )
    
    cat(
      "Valores críticos de F:\n"
    )
    
    cat(
      "F inferior =",
      round(d$f_low, 6),
      "\n"
    )
    
    cat(
      "F superior =",
      round(d$f_high, 6),
      "\n\n"
    )
    
    cat(
      "Intervalo de confianza para σ₁²/σ₂²:\n"
    )
    
    cat(
      "[",
      round(d$li, 6),
      ", ",
      round(d$ls, 6),
      "]\n\n",
      sep = ""
    )
    
    cat(
      "Intervalo equivalente para σ₁/σ₂:\n"
    )
    
    cat(
      "[",
      round(d$sd_li, 6),
      ", ",
      round(d$sd_ls, 6),
      "]\n\n",
      sep = ""
    )
    
    if (
      d$li <= 1 &&
      d$ls >= 1
    ) {
      
      cat(
        "Interpretación respecto a 1:\n"
      )
      
      cat(
        "El intervalo contiene el valor 1.\n"
      )
      
      cat(
        "La igualdad de varianzas es compatible con los datos.\n"
      )
      
    } else {
      
      cat(
        "Interpretación respecto a 1:\n"
      )
      
      cat(
        "El intervalo no contiene el valor 1.\n"
      )
      
      cat(
        "Los datos proporcionan evidencia de que el cociente de varianzas es diferente de 1.\n"
      )
    }
  })
  
  
  # ==========================================================
  # GRÁFICA IC DOS VARIANZAS
  # ==========================================================
  
  output$plot_ic2 <- renderPlot({
    
    d <- ic2()
    
    x_max <- max(
      qf(
        0.999,
        d$df1,
        d$df2
      ),
      d$f_high * 1.10,
      d$f_stat * 1.15,
      1.2
    )
    
    x <- seq(
      0,
      x_max,
      length.out = 1500
    )
    
    y <- df(
      x,
      d$df1,
      d$df2
    )
    
    ymax <- max(y)
    
    par(
      mar = c(
        7,
        5,
        4,
        2
      )
    )
    
    plot(
      x,
      y,
      type = "l",
      lwd = 3,
      xlab = "F",
      ylab = "Densidad",
      main = paste0(
        "IC ",
        round(d$conf * 100, 1),
        "% para el cociente de varianzas"
      )
    )
    
    # --------------------------------------------------------
    # Región central
    # --------------------------------------------------------
    
    x_central <- x[
      x >= d$f_low &
        x <= d$f_high
    ]
    
    polygon(
      c(
        d$f_low,
        x_central,
        d$f_high
      ),
      c(
        0,
        df(
          x_central,
          d$df1,
          d$df2
        ),
        0
      ),
      col = rgb(
        0,
        0.7,
        1,
        0.25
      ),
      border = NA
    )
    
    # --------------------------------------------------------
    # Cola izquierda
    # --------------------------------------------------------
    
    x_left <- x[
      x <= d$f_low
    ]
    
    polygon(
      c(
        0,
        x_left,
        d$f_low
      ),
      c(
        0,
        df(
          x_left,
          d$df1,
          d$df2
        ),
        0
      ),
      col = rgb(
        0.8,
        0.2,
        0.2,
        0.25
      ),
      border = NA
    )
    
    # --------------------------------------------------------
    # Cola derecha
    # --------------------------------------------------------
    
    x_right <- x[
      x >= d$f_high
    ]
    
    polygon(
      c(
        d$f_high,
        x_right,
        x_max
      ),
      c(
        0,
        df(
          x_right,
          d$df1,
          d$df2
        ),
        0
      ),
      col = rgb(
        0.8,
        0.2,
        0.2,
        0.25
      ),
      border = NA
    )
    
    # --------------------------------------------------------
    # Valores críticos
    # --------------------------------------------------------
    
    abline(
      v = d$f_low,
      lty = 2,
      lwd = 2
    )
    
    abline(
      v = d$f_high,
      lty = 2,
      lwd = 2
    )
    
    # --------------------------------------------------------
    # Valor 1
    # --------------------------------------------------------
    
    abline(
      v = 1,
      lty = 3,
      lwd = 2
    )
    
    # --------------------------------------------------------
    # Etiquetas
    # --------------------------------------------------------
    
    text(
      d$f_low,
      ymax * 0.30,
      paste0(
        "F = ",
        round(d$f_low, 3)
      ),
      pos = 2,
      cex = 0.82
    )
    
    text(
      d$f_high,
      ymax * 0.30,
      paste0(
        "F = ",
        round(d$f_high, 3)
      ),
      pos = 4,
      cex = 0.82
    )
    
    text(
      1,
      ymax * 0.18,
      "1",
      pos = 3,
      cex = 1
    )
    
    # --------------------------------------------------------
    # Región central
    # --------------------------------------------------------
    
    text(
      (
        d$f_low +
          d$f_high
      ) / 2,
      ymax * 0.25,
      paste0(
        round(d$conf * 100, 1),
        "% de confianza"
      ),
      cex = 0.9
    )
    
    # --------------------------------------------------------
    # Información del IC debajo
    # --------------------------------------------------------
    
    mtext(
      paste0(
        "IC para σ₁²/σ₂²: [",
        round(d$li, 3),
        ", ",
        round(d$ls, 3),
        "]"
      ),
      side = 1,
      line = 4.0,
      cex = 0.95
    )
    
    mtext(
      "El valor 1 representa igualdad de varianzas.",
      side = 1,
      line = 5.3,
      cex = 0.85
    )
  })
  
  
  # ==========================================================
  # PRUEBA F
  # ==========================================================
  
  test2 <- eventReactive(input$calc_test2, {
    
    n1 <- input$n2_1h
    n2 <- input$n2_2h
    s1 <- input$s2_g1h
    s2 <- input$s2_g2h
    
    df1 <- n1 - 1
    df2 <- n2 - 1
    
    f_stat <- s1 / s2
    tipo <- input$tipo_F
    a <- input$alpha2
    
    error <- validar_dos_varianzas(
      n1 = n1,
      n2 = n2,
      s1 = s1,
      s2 = s2,
      alpha = a
    )
    
    validate(
      need(
        is.null(error),
        error
      )
    )
    
    if (tipo == "Bilateral") {
      
      fl <- qf(
        a / 2,
        df1,
        df2
      )
      
      fh <- qf(
        1 - a / 2,
        df1,
        df2
      )
      
      p <- 2 * min(
        pf(
          f_stat,
          df1,
          df2
        ),
        1 - pf(
          f_stat,
          df1,
          df2
        )
      )
      
    } else if (tipo == "Cola inferior") {
      
      fl <- qf(
        a,
        df1,
        df2
      )
      
      fh <- NA
      
      p <- pf(
        f_stat,
        df1,
        df2
      )
      
    } else {
      
      fl <- NA
      
      fh <- qf(
        1 - a,
        df1,
        df2
      )
      
      p <- 1 - pf(
        f_stat,
        df1,
        df2
      )
    }
    
    h <- hipotesis_F(
      tipo
    )
    
    decision <- ifelse(
      p < a,
      "Rechazar H₀",
      "No rechazar H₀"
    )
    
    if (p < a) {
      
      if (tipo == "Bilateral") {
        
        conclusion <- paste0(
          "Existe evidencia estadísticamente significativa ",
          "para concluir que las varianzas poblacionales ",
          "son diferentes."
        )
        
      } else if (tipo == "Cola inferior") {
        
        conclusion <- paste0(
          "Existe evidencia estadísticamente significativa ",
          "para concluir que σ₁² es menor que σ₂²."
        )
        
      } else {
        
        conclusion <- paste0(
          "Existe evidencia estadísticamente significativa ",
          "para concluir que σ₁² es mayor que σ₂²."
        )
      }
      
    } else {
      
      if (tipo == "Bilateral") {
        
        conclusion <- paste0(
          "No existe evidencia estadísticamente suficiente ",
          "para concluir que las varianzas poblacionales ",
          "sean diferentes."
        )
        
      } else if (tipo == "Cola inferior") {
        
        conclusion <- paste0(
          "No existe evidencia estadísticamente suficiente ",
          "para concluir que σ₁² sea menor que σ₂²."
        )
        
      } else {
        
        conclusion <- paste0(
          "No existe evidencia estadísticamente suficiente ",
          "para concluir que σ₁² sea mayor que σ₂²."
        )
      }
    }
    
    list(
      n1 = n1,
      n2 = n2,
      s1 = s1,
      s2 = s2,
      df1 = df1,
      df2 = df2,
      f_stat = f_stat,
      tipo = tipo,
      a = a,
      fl = fl,
      fh = fh,
      p = p,
      h0 = h$h0,
      ha = h$ha,
      decision = decision,
      conclusion = conclusion
    )
  })
  
  
  output$res_test2 <- renderPrint({
    
    d <- test2()
    
    cat(
      "PRUEBA DE HIPÓTESIS PARA EL COCIENTE DE VARIANZAS\n"
    )
    
    cat(
      "===================================================\n\n"
    )
    
    cat(
      "Hipótesis:\n"
    )
    
    cat(
      d$h0,
      "\n"
    )
    
    cat(
      d$ha,
      "\n\n"
    )
    
    cat(
      "Nivel de significancia: α =",
      round(d$a, 6),
      "\n\n"
    )
    
    cat(
      "Grados de libertad:\n"
    )
    
    cat(
      "gl1 =",
      d$df1,
      "\n"
    )
    
    cat(
      "gl2 =",
      d$df2,
      "\n\n"
    )
    
    cat(
      "Estadístico de prueba:\n"
    )
    
    cat(
      "F = S1² / S2² =",
      round(d$f_stat, 6),
      "\n\n"
    )
    
    cat(
      "Región de rechazo:\n"
    )
    
    if (d$tipo == "Bilateral") {
      
      cat(
        "F <",
        round(d$fl, 6),
        "o F >",
        round(d$fh, 6),
        "\n\n"
      )
      
    } else if (d$tipo == "Cola inferior") {
      
      cat(
        "F <",
        round(d$fl, 6),
        "\n\n"
      )
      
    } else {
      
      cat(
        "F >",
        round(d$fh, 6),
        "\n\n"
      )
    }
    
    cat(
      "p-value =",
      round(d$p, 6),
      "\n\n"
    )
    
    cat(
      "Decisión:",
      d$decision,
      "\n\n"
    )
    
    cat(
      "Conclusión:\n"
    )
    
    cat(
      d$conclusion,
      "\n"
    )
  })
  
  
  # ==========================================================
  # GRÁFICA PRUEBA F
  # ==========================================================
  
  output$plot_test2 <- renderPlot({
    
    d <- test2()
    
    q999 <- qf(
      0.999,
      d$df1,
      d$df2
    )
    
    xmax <- max(
      q999,
      d$f_stat * 1.15,
      ifelse(
        is.na(d$fl),
        0,
        d$fl
      ) * 1.15,
      ifelse(
        is.na(d$fh),
        0,
        d$fh
      ) * 1.15
    )
    
    x <- seq(
      0,
      xmax,
      length.out = 1500
    )
    
    y <- df(
      x,
      d$df1,
      d$df2
    )
    
    ymax <- max(y)
    
    par(
      mar = c(
        6,
        5,
        4,
        2
      )
    )
    
    # --------------------------------------------------------
    # Curva
    # --------------------------------------------------------
    
    plot(
      x,
      y,
      type = "l",
      lwd = 3,
      xlab = "F",
      ylab = "Densidad",
      main = paste0(
        "Prueba F para comparación de varianzas — ",
        d$tipo
      )
    )
    
    # --------------------------------------------------------
    # Regiones de rechazo
    # --------------------------------------------------------
    
    if (d$tipo == "Bilateral") {
      
      # Cola izquierda
      x_left <- x[
        x <= d$fl
      ]
      
      polygon(
        c(
          0,
          x_left,
          d$fl
        ),
        c(
          0,
          df(
            x_left,
            d$df1,
            d$df2
          ),
          0
        ),
        col = rgb(
          0.8,
          0.2,
          0.2,
          0.25
        ),
        border = NA
      )
      
      # Cola derecha
      x_right <- x[
        x >= d$fh
      ]
      
      polygon(
        c(
          d$fh,
          x_right,
          xmax
        ),
        c(
          0,
          df(
            x_right,
            d$df1,
            d$df2
          ),
          0
        ),
        col = rgb(
          0.8,
          0.2,
          0.2,
          0.25
        ),
        border = NA
      )
      
      abline(
        v = d$fl,
        lty = 2,
        lwd = 2
      )
      
      abline(
        v = d$fh,
        lty = 2,
        lwd = 2
      )
      
      text(
        d$fl,
        ymax * 0.25,
        paste0(
          "F crítico = ",
          round(d$fl, 3)
        ),
        pos = 2,
        cex = 0.82
      )
      
      text(
        d$fh,
        ymax * 0.25,
        paste0(
          "F crítico = ",
          round(d$fh, 3)
        ),
        pos = 4,
        cex = 0.82
      )
      
    } else if (d$tipo == "Cola inferior") {
      
      x_left <- x[
        x <= d$fl
      ]
      
      polygon(
        c(
          0,
          x_left,
          d$fl
        ),
        c(
          0,
          df(
            x_left,
            d$df1,
            d$df2
          ),
          0
        ),
        col = rgb(
          0.8,
          0.2,
          0.2,
          0.25
        ),
        border = NA
      )
      
      abline(
        v = d$fl,
        lty = 2,
        lwd = 2
      )
      
      text(
        d$fl,
        ymax * 0.25,
        paste0(
          "F crítico = ",
          round(d$fl, 3)
        ),
        pos = 4,
        cex = 0.82
      )
      
    } else {
      
      x_right <- x[
        x >= d$fh
      ]
      
      polygon(
        c(
          d$fh,
          x_right,
          xmax
        ),
        c(
          0,
          df(
            x_right,
            d$df1,
            d$df2
          ),
          0
        ),
        col = rgb(
          0.8,
          0.2,
          0.2,
          0.25
        ),
        border = NA
      )
      
      abline(
        v = d$fh,
        lty = 2,
        lwd = 2
      )
      
      text(
        d$fh,
        ymax * 0.25,
        paste0(
          "F crítico = ",
          round(d$fh, 3)
        ),
        pos = 2,
        cex = 0.82
      )
    }
    
    # --------------------------------------------------------
    # Estadístico observado
    # --------------------------------------------------------
    
    abline(
      v = d$f_stat,
      lwd = 3
    )
    
    mtext(
      paste0(
        "F observado = ",
        round(d$f_stat, 3)
      ),
      side = 1,
      line = 2.5,
      cex = 0.90
    )
    
    # --------------------------------------------------------
    # Nota didáctica
    # --------------------------------------------------------
    
    mtext(
      "Las áreas sombreadas corresponden a la región de rechazo.",
      side = 1,
      line = 4.0,
      cex = 0.82
    )
  })
}


# ============================================================
# EJECUTAR APLICACIÓN
# ============================================================

shinyApp(
  ui = ui,
  server = server
)