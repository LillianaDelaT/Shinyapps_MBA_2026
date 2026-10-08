# =============================================================================
# VERIFICACIÓN E INSTALACIÓN AUTOMÁTICA DE DEPENDENCIAS
# =============================================================================
paquetes_requeridos <- c("shiny", "ggplot2")

# Identificar paquetes no instalados
paquetes_faltantes <- paquetes_requeridos[!(paquetes_requeridos %in% installed.packages()[, "Package"])]

# Instalar si falta alguno
if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes, dependencies = TRUE)
}

# Cargar librerías
library(shiny)
library(ggplot2)

# =============================================================================
# INTERFAZ DE USUARIO (UI)
# =============================================================================
ui <- fluidPage(
  titlePanel("Inferencia Estadística para Proporciones"),
  
  tabsetPanel(
    # =========================================================================
    # PESTAÑA 1: INTERVALO DE CONFIANZA
    # =========================================================================
    tabPanel(
      "1. Intervalo de Confianza",
      sidebarLayout(
        sidebarPanel(
          numericInput("ic_n", "Tamaño de la muestra (n):", value = 100, min = 1),
          numericInput("ic_x", "Éxitos observados (x o p̂):", value = 45, min = 0),
          sliderInput("ic_nivel", "Nivel de Confianza (1 - α) %:", min = 80, max = 99, value = 95, step = 1),
          
          checkboxInput("ic_poblacion_finita", "¿Ajustar por población finita (FCPF)?", value = FALSE),
          conditionalPanel(
            condition = "input.ic_poblacion_finita == true",
            numericInput("ic_N", "Tamaño de la población (N):", value = 1000, min = 2)
          ),
          
          actionButton("ic_calcular", "Calcular IC", class = "btn-primary")
        ),
        mainPanel(
          verbatimTextOutput("ic_resumen"),
          plotOutput("ic_grafico")
        )
      )
    ),
    
    # =========================================================================
    # PESTAÑA 2: PRUEBA DE HIPÓTESIS
    # =========================================================================
    tabPanel(
      "2. Prueba de Hipótesis",
      sidebarLayout(
        sidebarPanel(
          numericInput("ph_n", "Tamaño de la muestra (n):", value = 100, min = 1),
          numericInput("ph_x", "Éxitos observados (x o p̂):", value = 40, min = 0),
          numericInput("ph_p0", "Proporción bajo H₀ (p₀):", value = 0.5, min = 0.0001, max = 0.9999, step = 0.01),
          
          selectInput(
            inputId = "ph_tipo_ha",
            label   = "Hipótesis alternativa (Hₐ):",
            choices = c("Bilateral (p ≠ p₀)" = "two.sided", 
                        "Unilateral Derecha (p > p₀)" = "greater", 
                        "Unilateral Izquierda (p < p₀)" = "less")
          ),
          
          selectInput(
            inputId  = "ph_alpha",
            label    = HTML("Nivel de significancia (&alpha;):"),
            choices  = c("0.01" = 0.01, "0.02" = 0.02, "0.05" = 0.05, "0.10" = 0.10),
            selected = 0.05
          ),
          
          checkboxInput("ph_poblacion_finita", "¿Ajustar por población finita (FCPF)?", value = FALSE),
          conditionalPanel(
            condition = "input.ph_poblacion_finita == true",
            numericInput("ph_N", "Tamaño de la población (N):", value = 1000, min = 2)
          ),
          
          actionButton("ph_calcular", "Ejecutar Prueba", class = "btn-primary")
        ),
        mainPanel(
          verbatimTextOutput("ph_resultados"),
          plotOutput("ph_grafico")
        )
      )
    ),
    
    # =========================================================================
    # PESTAÑA 3: TAMAÑO DE LA MUESTRA
    # =========================================================================
    tabPanel(
      "3. Tamaño de la Muestra",
      sidebarLayout(
        sidebarPanel(
          sliderInput("tm_nivel", "Nivel de Confianza (1 - α) %:", min = 80, max = 99, value = 95, step = 1),
          numericInput("tm_e", "Margen de error máximo deseado (E):", value = 0.05, min = 0.001, max = 0.5, step = 0.01),
          
          numericInput("tm_p", "Proporción esperada / previa (p):", value = 0.5, min = 0.01, max = 0.99, step = 0.05),
          helpText("Nota: Usar p = 0.50 garantiza la máxima varianza (escenario más conservador)."),
          
          checkboxInput("tm_conocer_N", "¿Se conoce el tamaño de la población (N)?", value = FALSE),
          conditionalPanel(
            condition = "input.tm_conocer_N == true",
            numericInput("tm_N", "Tamaño de la población (N):", value = 1000, min = 2)
          ),
          
          actionButton("tm_calcular", "Calcular Tamaño Muestral", class = "btn-primary")
        ),
        mainPanel(
          verbatimTextOutput("tm_resultados")
        )
      )
    )
  )
)

# =============================================================================
# LÓGICA DEL SERVIDOR (SERVER)
# =============================================================================
server <- function(input, output, session) {
  
  # =========================================================================
  # PESTAÑA 1: INTERVALO DE CONFIANZA
  # =========================================================================
  datos_ic <- eventReactive(input$ic_calcular, ignoreNULL = FALSE, {
    n <- input$ic_n
    x <- input$ic_x
    
    req(n, x)
    
    if (n <= 0) {
      showNotification("El tamaño de muestra (n) debe ser mayor a 0.", type = "error")
      req(FALSE)
    }
    
    if (x <= 1 && n > 1) {
      prop <- x
    } else {
      if (x > n || x < 0) {
        showNotification("Verifica que los éxitos (x) no sean negativos ni superen a n.", type = "error")
        req(FALSE)
      }
      prop <- x / n
    }
    
    fcpf <- 1
    if (isTRUE(input$ic_poblacion_finita)) {
      N <- input$ic_N
      req(N)
      
      if (N <= 1) {
        showNotification("El tamaño de la población (N) debe ser mayor a 1.", type = "error")
        req(FALSE)
      }
      if (N < n) {
        showNotification("El tamaño de la población (N) no puede ser menor que el tamaño muestral (n).", type = "error")
        req(FALSE)
      }
      if (N == n) {
        showNotification("Si N = n, se censó a toda la población (variabilidad muestral = 0).", type = "warning")
        req(FALSE)
      }
      if ((n / N) <= 0.05) {
        showNotification(
          sprintf("Nota: La fracción muestral (n/N = %.2f%%) es ≤ 5%%. El ajuste FCPF es opcional.", (n / N) * 100),
          type = "info", duration = 6
        )
      }
      
      fcpf <- sqrt((N - n) / (N - 1))
    }
    
    nivel  <- input$ic_nivel / 100
    alpha  <- 1 - nivel
    z_crit <- qnorm(1 - alpha / 2)
    
    e_exitos   <- n * prop
    e_fracasos <- n * (1 - prop)
    cumple_supuestos <- (e_exitos >= 10) && (e_fracasos >= 10)
    
    if (!cumple_supuestos && !(prop == 0 || prop == 1)) {
      showNotification(
        sprintf("⚠️ Advertencia: No se cumple n*p̂ >= 10 o n*(1-p̂) >= 10 (éxitos: %.1f, fracasos: %.1f).", e_exitos, e_fracasos),
        type = "warning", duration = 7
      )
    }
    
    if (prop == 0 || prop == 1) {
      showNotification(
        "Debido a que p̂ es 0 o 1, se utilizó el método de Wilson Score para evitar un intervalo nulo.",
        type = "warning", duration = 7
      )
      
      denominador <- 1 + (z_crit^2 / n)
      centro      <- (prop + (z_crit^2 / (2 * n))) / denominador
      margen      <- (z_crit * sqrt((prop * (1 - prop) / n) + (z_crit^2 / (4 * n^2)))) / denominador
      
      li <- max(0, centro - margen)
      ls <- min(1, centro + margen)
      se <- sqrt(prop * (1 - prop) / n) * fcpf
      
    } else {
      se <- sqrt(prop * (1 - prop) / n) * fcpf
      me <- z_crit * se
      li <- max(0, prop - me)
      ls <- min(1, prop + me)
    }
    
    list(prop = prop, n = n, z_crit = z_crit, se = se, li = li, ls = ls, 
         nivel = nivel, alpha = alpha, fcpf = fcpf,
         cumple_supuestos = cumple_supuestos, e_exitos = e_exitos, e_fracasos = e_fracasos)
  })
  
  output$ic_resumen <- renderPrint({
    d <- datos_ic()
    req(d)
    
    cat("========================================\n")
    cat(" INTERVALO DE CONFIANZA PARA UNA PROPORCIÓN\n")
    cat("========================================\n\n")
    cat(sprintf("Proporción muestral (p̂)       = %.4f\n", d$prop))
    cat(sprintf("Tamaño de muestra (n)         = %d\n", d$n))
    cat(sprintf("Nivel de Confianza (1 - α)    = %.2f%%\n", d$nivel * 100))
    cat(sprintf("Nivel de significancia (α)    = %.4f\n", d$alpha))
    cat(sprintf("Valor crítico z_{α/2}          = %.4f\n", d$z_crit))
    cat(sprintf("Error estándar (SE)           = %.4f %s\n", d$se, ifelse(d$fcpf < 1, "(con FCPF)", "")))
    cat(sprintf("Intervalo de Confianza (IC)   = [%.4f, %.4f]\n\n", d$li, d$ls))
    
    cat("INTERPRETACIÓN CONTEXTUAL:\n")
    cat(sprintf("  Con un nivel de confianza del %.1f%%, se estima que la verdadera proporción\n", d$nivel * 100))
    cat(sprintf("  poblacional (p) se encuentra entre %.4f y %.4f.\n\n", d$li, d$ls))
    
    cat("VERIFICACIÓN DE SUPUESTOS (Aproximación Normal):\n")
    cat(sprintf("  • Éxitos esperados   (n · p̂)     = %.2f %s\n", d$e_exitos, ifelse(d$e_exitos >= 10, "✓", "✗ (< 10)")))
    cat(sprintf("  • Fracasos esperados (n · (1-p̂)) = %.2f %s\n", d$e_fracasos, ifelse(d$e_fracasos >= 10, "✓", "✗ (< 10)")))
    if (!d$cumple_supuestos) {
      cat("  ⚠ Nota: Al no cumplirse las condiciones, se aconseja interpretar el intervalo Wald con precaución.\n")
    }
  })
  
  output$ic_grafico <- renderPlot({
    d <- datos_ic()
    req(d)
    
    if (d$se == 0) {
      plot.new()
      text(0.5, 0.5, "Error estándar igual a cero. No es posible generar el gráfico.", cex = 1.2)
      return()
    }
    
    lim_inf <- max(0, d$prop - 4 * d$se)
    lim_sup <- min(1, d$prop + 4 * d$se)
    
    x_vals <- seq(lim_inf, lim_sup, length.out = 500)
    y_vals <- dnorm(x_vals, mean = d$prop, sd = d$se)
    df_curva <- data.frame(x = x_vals, y = y_vals)
    df_area  <- subset(df_curva, x >= d$li & x <= d$ls)
    
    ggplot(df_curva, aes(x = x, y = y)) +
      geom_line(color = "#2c3e50", linewidth = 1) +
      geom_area(data = df_area, fill = "#3498db", alpha = 0.35) +
      geom_vline(xintercept = d$prop, color = "#2980b9", linetype = "solid", linewidth = 1.1) +
      geom_vline(xintercept = c(d$li, d$ls), color = "#c0392b", linetype = "dashed", linewidth = 0.9) +
      annotate("label", x = d$prop, y = max(y_vals) * 0.9, 
               label = sprintf("p̂ = %.4f", d$prop), 
               fill = "#ebf5fb", color = "#1b4f72", size = 4.5, fontface = "bold") +
      annotate("label", x = d$li, y = max(y_vals) * 0.4, 
               label = sprintf("LI = %.4f", d$li), 
               fill = "#fadbd8", color = "#78281f", size = 4) +
      annotate("label", x = d$ls, y = max(y_vals) * 0.4, 
               label = sprintf("LS = %.4f", d$ls), 
               fill = "#fadbd8", color = "#78281f", size = 4) +
      theme_minimal(base_size = 14) +
      labs(
        title = expression(paste("Distribución Muestral Estimada del Estimador ", hat(p))),
        subtitle = sprintf("Intervalo de Confianza del %.1f%% para la proporción poblacional p", d$nivel * 100),
        x = expression(paste("Valores de la Proporción Muestral (", hat(p), ")")),
        y = "Densidad Muestral Estimada",
        caption = expression(paste("Nota didáctica: La curva representa la variabilidad de ", hat(p), 
                                   " en muestras repetidas de tamaño n. NO representa una distribución de probabilidad sobre el parámetro fijo p."))
      ) +
      theme(
        plot.caption = element_text(size = 11, face = "italic", color = "#555555", hjust = 0),
        plot.title = element_text(face = "bold")
      )
  })
  
  # =========================================================================
  # PESTAÑA 2: PRUEBA DE HIPÓTESIS
  # =========================================================================
  datos_ph <- eventReactive(input$ph_calcular, ignoreNULL = FALSE, {
    req(input$ph_x, input$ph_n, input$ph_p0, input$ph_alpha)
    
    n  <- input$ph_n
    x  <- input$ph_x
    p0 <- input$ph_p0
    alpha <- as.numeric(input$ph_alpha)
    
    if (n <= 0) {
      showNotification("El tamaño de muestra (n) debe ser mayor a 0.", type = "error")
      req(FALSE)
    }
    
    if (x <= 1 && n > 1) {
      p_hat <- x
      x_conteo <- round(x * n)
    } else {
      if (x > n || x < 0) {
        showNotification("Verifica que los éxitos (x) no sean negativos ni superen a n.", type = "error")
        req(FALSE)
      }
      p_hat <- x / n
      x_conteo <- round(x)
    }
    
    fcpf <- 1
    if (isTRUE(input$ph_poblacion_finita)) {
      N <- input$ph_N
      req(N)
      
      if (N <= 1) {
        showNotification("El tamaño de la población (N) debe ser mayor a 1.", type = "error")
        req(FALSE)
      }
      if (N < n) {
        showNotification("El tamaño de la población (N) no puede ser menor que el tamaño muestral (n).", type = "error")
        req(FALSE)
      }
      if (N == n) {
        showNotification("Si N = n, se conoce toda la población; no se requiere inferencia muestral.", type = "warning")
        req(FALSE)
      }
      fcpf <- sqrt((N - n) / (N - 1))
    }
    
    if (p0 <= 0 || p0 >= 1) {
      showNotification(
        "⚠️ p₀ no puede ser 0 o 1 en la prueba Z. Se ejecuta la Prueba Binomial Exacta como alternativa.",
        type = "error", duration = 8
      )
      
      bt <- binom.test(x_conteo, n, p = p0, alternative = input$ph_tipo_ha)
      
      return(list(
        p_hat = p_hat, p0 = p0, n = n, z = NA, p_value = bt$p.value,
        alpha = alpha, tipo = input$ph_tipo_ha,
        cumple_supuestos = FALSE, e_exitos_h0 = n * p0, e_fracasos_h0 = n * (1 - p0),
        es_exacta = TRUE, fcpf = fcpf
      ))
    }
    
    e_exitos_h0   <- n * p0
    e_fracasos_h0 <- n * (1 - p0)
    cumple_supuestos <- (e_exitos_h0 >= 10) && (e_fracasos_h0 >= 10)
    
    if (!cumple_supuestos) {
      showNotification(
        sprintf("⚠️ Advertencia: Bajo H₀ no se cumple n*p₀ >= 10 o n*(1-p₀) >= 10 (esperados: %.1f éxitos, %.1f fracasos).", e_exitos_h0, e_fracasos_h0),
        type = "warning", duration = 8
      )
    }
    
    se <- sqrt((p0 * (1 - p0)) / n) * fcpf
    z  <- (p_hat - p0) / se
    
    tipo <- input$ph_tipo_ha
    p_value <- switch(tipo,
                      "two.sided" = 2 * (1 - pnorm(abs(z))),
                      "greater"   = 1 - pnorm(z),
                      "less"      = pnorm(z))
    
    list(p_hat = p_hat, p0 = p0, n = n, z = z, p_value = p_value, alpha = alpha, tipo = tipo,
         cumple_supuestos = cumple_supuestos, e_exitos_h0 = e_exitos_h0, e_fracasos_h0 = e_fracasos_h0,
         es_exacta = FALSE, fcpf = fcpf)
  })
  
  output$ph_resultados <- renderPrint({
    d <- datos_ph()
    req(d)
    
    simbolo_ha <- switch(d$tipo,
                         "two.sided" = "≠",
                         "greater"   = ">",
                         "less"      = "<")
    
    cat("========================================\n")
    cat(" PRUEBA DE HIPÓTESIS PARA UNA PROPORCIÓN\n")
    cat("========================================\n\n")
    
    cat("PLANTEAMIENTO FORMAL:\n")
    cat(sprintf("  • H₀: p  = %.4f\n", d$p0))
    cat(sprintf("  • Hₐ: p %s %.4f\n", simbolo_ha, d$p0))
    cat(sprintf("  • Nivel de significancia (α): %.4f (%.1f%%)\n\n", d$alpha, d$alpha * 100))
    
    cat("DATOS MUESTRALES Y ESTADÍSTICO DE PRUEBA:\n")
    cat(sprintf("  • Tamaño de muestra (n)  : %d\n", d$n))
    cat(sprintf("  • Proporción muestral (p̂): %.4f\n", d$p_hat))
    
    if (isTRUE(d$es_exacta)) {
      cat("  • Estadístico Z          : No aplica (p₀ es 0 o 1)\n")
      cat("  • Método aplicado        : Prueba Binomial Exacta (binom.test)\n")
    } else {
      cat(sprintf("  • Estadístico Z          : %.4f %s\n", d$z, ifelse(d$fcpf < 1, "(con FCPF)", "")))
    }
    
    cat(sprintf("  • Valor p                : %.6f\n\n", d$p_value))
    
    cat("DECISIÓN ESTADÍSTICA:\n")
    if (d$p_value < d$alpha) {
      cat(sprintf("  🟢 SE RECHAZA H₀ (p-valor = %.6f < α = %.4f)\n\n", d$p_value, d$alpha))
      cat(sprintf("     Existe evidencia estadísticamente significativa, al nivel de significancia α = %.4f, a favor de Hₐ (p %s %.4f).\n\n", 
                  d$alpha, simbolo_ha, d$p0))
    } else {
      cat(sprintf("  🔴 NO SE RECHAZA H₀ (p-valor = %.6f ≥ α = %.4f)\n\n", d$p_value, d$alpha))
      cat(sprintf("     No existe evidencia estadísticamente significativa, al nivel de significancia α = %.4f, para rechazar la hipótesis nula H₀ (p = %.4f).\n\n", 
                  d$alpha, d$p0))
    }
    
    if (!isTRUE(d$es_exacta)) {
      cat("VERIFICACIÓN DE SUPUESTOS (Aproximación Normal bajo H₀):\n")
      cat(sprintf("  • Éxitos esperados   (n · p₀)     = %.2f %s\n", d$e_exitos_h0, ifelse(d$e_exitos_h0 >= 10, "✓", "✗ (< 10)")))
      cat(sprintf("  • Fracasos esperados (n · (1-p₀)) = %.2f %s\n", d$e_fracasos_h0, ifelse(d$e_fracasos_h0 >= 10, "✓", "✗ (< 10)")))
      if (!d$cumple_supuestos) {
        cat("  ⚠ Nota: Las condiciones para la aproximación normal no se cumplen con rigurosidad.\n")
      }
    }
  })
  
  output$ph_grafico <- renderPlot({
    d <- datos_ph()
    req(d)
    
    if (isTRUE(d$es_exacta)) {
      plot.new()
      text(0.5, 0.5, "Gráfico no disponible para prueba exacta (p₀ es 0 o 1)", cex = 1.2)
      return()
    }
    
    alpha  <- d$alpha
    tipo   <- d$tipo
    z_calc <- d$z
    
    if (tipo == "two.sided") {
      z_crit_inf <- qnorm(alpha / 2)
      z_crit_sup <- qnorm(1 - alpha / 2)
      v_criticos <- c(z_crit_inf, z_crit_sup)
      labels_crit <- c(sprintf("-z_{α/2} (%.2f)", z_crit_inf), sprintf("z_{α/2} (%.2f)", z_crit_sup))
    } else if (tipo == "greater") {
      z_crit_sup <- qnorm(1 - alpha)
      v_criticos <- z_crit_sup
      labels_crit <- sprintf("z_{α} (%.2f)", z_crit_sup)
    } else {
      z_crit_inf <- qnorm(alpha)
      v_criticos <- z_crit_inf
      labels_crit <- sprintf("-z_{α} (%.2f)", z_crit_inf)
    }
    
    lim_x <- max(3.5, abs(z_calc) + 0.8, abs(v_criticos) + 0.8)
    x_vals <- seq(-lim_x, lim_x, length.out = 500)
    df_curva <- data.frame(x = x_vals, y = dnorm(x_vals))
    
    p <- ggplot(df_curva, aes(x = x, y = y)) +
      geom_line(color = "#2b2b2b", linewidth = 1) +
      theme_minimal(base_size = 14) +
      labs(
        title = "Distribución Normal Estándar N(0,1) y Región de Rechazo",
        subtitle = sprintf("Comparación entre Valor(es) Crítico(s) y Estadístico Calculado (Z = %.2f)", z_calc),
        x = "Valores de Z",
        y = "Densidad"
      )
    
    if (tipo == "two.sided") {
      p <- p + 
        geom_area(data = subset(df_curva, x <= z_crit_inf), fill = "#e74c3c", alpha = 0.4) +
        geom_area(data = subset(df_curva, x >= z_crit_sup), fill = "#e74c3c", alpha = 0.4)
    } else if (tipo == "greater") {
      p <- p + geom_area(data = subset(df_curva, x >= z_crit_sup), fill = "#e74c3c", alpha = 0.4)
    } else {
      p <- p + geom_area(data = subset(df_curva, x <= z_crit_inf), fill = "#e74c3c", alpha = 0.4)
    }
    
    for (i in seq_along(v_criticos)) {
      zc <- v_criticos[i]
      p <- p + 
        geom_vline(xintercept = zc, color = "#c0392b", linetype = "dashed", linewidth = 0.9) +
        annotate(
          "label", x = zc, y = dnorm(0) * 0.7, 
          label = labels_crit[i], fill = "#fadbd8", color = "#78281f", size = 4
        )
    }
    
    p <- p + 
      geom_vline(xintercept = z_calc, color = "#27ae60", linetype = "solid", linewidth = 1.2) +
      annotate(
        "label", x = z_calc, y = dnorm(0) * 0.95, 
        label = sprintf("Z_calc = %.2f", z_calc), 
        fill = "#d4efdf", color = "#145a32", size = 4.5, fontface = "bold"
      )
    
    p
  })
  
  # =========================================================================
  # PESTAÑA 3: TAMAÑO DE LA MUESTRA
  # =========================================================================
  datos_tm <- eventReactive(input$tm_calcular, ignoreNULL = FALSE, {
    nivel <- input$tm_nivel / 100
    E     <- input$tm_e
    p     <- input$tm_p
    
    # Validaciones
    if (E <= 0 || E >= 0.5) {
      showNotification("El error máximo debe estar entre 0.001 y 0.50.", type = "error")
      req(FALSE)
    }
    
    alpha  <- 1 - nivel
    z_crit <- qnorm(1 - alpha / 2)
    
    # Fórmula población infinita (n0)
    n0 <- (z_crit^2 * p * (1 - p)) / (E^2)
    n0_redondeado <- ceiling(n0)
    
    n_final <- n0_redondeado
    N <- NULL
    
    # Ajuste por Población Finita si aplica
    if (isTRUE(input$tm_conocer_N)) {
      N <- input$tm_N
      req(N)
      
      if (N <= 1) {
        showNotification("El tamaño de la población (N) debe ser mayor a 1.", type = "error")
        req(FALSE)
      }
      
      n_finita <- (n0 * N) / (n0 + (N - 1))
      n_final  <- ceiling(n_finita)
    }
    
    list(
      nivel = nivel, alpha = alpha, z_crit = z_crit,
      E = E, p = p, n0 = n0_redondeado, n_final = n_final, N = N
    )
  })
  
  output$tm_resultados <- renderPrint({
    d <- datos_tm()
    req(d)
    
    cat("========================================\n")
    cat(" CÁLCULO DEL TAMAÑO DE LA MUESTRA (n)\n")
    cat("========================================\n\n")
    
    cat("PARÁMETROS DE ENTRADA:\n")
    cat(sprintf("  • Nivel de Confianza (1 - α) : %.1f%%\n", d$nivel * 100))
    cat(sprintf("  • Valor crítico (z_{α/2})    : %.4f\n", d$z_crit))
    cat(sprintf("  • Margen de error (E)        : ±%.4f (%.2f%%)\n", d$E, d$E * 100))
    cat(sprintf("  • Proporción estimada (p)   : %.2f\n\n", d$p))
    
    cat("RESULTADOS DEL CÁLCULO:\n")
    if (is.null(d$N)) {
      cat("  • Población                  : Infinita / Desconocida\n")
      cat(sprintf("  👉 Tamaño muestral necesario (n) : %d observaciones\n\n", d$n_final))
      cat("FÓRMULA APLICADA:\n")
      cat("  n = (z_{α/2}² · p · (1 - p)) / E²\n")
    } else {
      cat(sprintf("  • Población Finita (N)       : %d\n", d$N))
      cat(sprintf("  • Muestra preliminar (n₀)    : %d\n", d$n0))
      cat(sprintf("  👉 Tamaño muestral ajustado (n): %d observaciones\n\n", d$n_final))
      cat("FÓRMULA APLICADA (Con corrección por población finita):\n")
      cat("  n = (n₀ · N) / (n₀ + N - 1)\n")
    }
    
    cat("\nNOTAS TÉCNICAS:\n")
    cat("  1. El valor reportado siempre utiliza redondeo hacia arriba (ceiling) para garantizar el error deseado.\n")
    cat(sprintf("  2. Si se mantiene el mismo nivel de confianza y se reduce el error a la mitad (±%.4f),\n", d$E / 2))
    cat("     el tamaño de la muestra requerido se cuadruplicará aproximadamente.\n")
  })
}

# =============================================================================
# LANZAMIENTO DE LA APLICACIÓN
# =============================================================================
shinyApp(ui = ui, server = server)