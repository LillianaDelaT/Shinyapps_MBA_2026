library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Diferencia de proporciones independientes"),
  
  tags$div(
    style = "font-size: small; font-style: italic; margin-bottom: 15px;",
    "App desarrollada por Liliana De la Torre Desentis"
  ),
  
  tabsetPanel(
    
    tabPanel("Intervalo de confianza",
             withMathJax(),
             helpText("Fórmulas utilizadas:"),
             helpText("$$\\text{SE} = \\sqrt{\\frac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\frac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}$$"),
             helpText("$$(\\hat{p}_1 - \\hat{p}_2) \\pm z_{\\alpha/2} \\cdot \\text{SE}$$"),
             
             sidebarLayout(
               sidebarPanel(
                 numericInput("ic_x1", "Número de éxitos muestra 1 (x1):", value = 18, min = 0),
                 numericInput("ic_n1", "Tamaño muestra 1 (n1):", value = 150, min = 1),
                 numericInput("ic_x2", "Número de éxitos muestra 2 (x2):", value = 10, min = 0),
                 numericInput("ic_n2", "Tamaño muestra 2 (n2):", value = 130, min = 1),
                 numericInput("ic_nivel", "Nivel de confianza (%):", value = 95, min = 80, max = 99.999, step = 0.1),
                 actionButton("ic_calcular", "Calcular", class = "btn-primary")
               ),
               
               mainPanel(
                 verbatimTextOutput("ic_resultados"),
                 plotOutput("ic_grafica", height = "400px")
               )
             )
    ),
    
    tabPanel("Prueba de hipótesis",
             withMathJax(),
             helpText("Estadístico de prueba Z:"),
             helpText("$$Z = \\frac{(\\hat{p}_1 - \\hat{p}_2) - p_0}{\\text{SE}}$$"),
             
             sidebarLayout(
               sidebarPanel(
                 numericInput("ph_x1", "Éxitos muestra 1 (x1):", value = 50, min = 0),
                 numericInput("ph_n1", "Tamaño muestra 1 (n1):", value = 100, min = 1),
                 numericInput("ph_x2", "Éxitos muestra 2 (x2):", value = 45, min = 0),
                 numericInput("ph_n2", "Tamaño muestra 2 (n2):", value = 100, min = 1),
                 numericInput("ph_d0", "Diferencia bajo H₀ (p₀):", value = 0, step = 0.01),
                 
                 selectInput("ph_tipo", "Tipo de prueba (Ha):",
                             choices = c("Mayor (p1 - p2 > p0)" = "mayor",
                                         "Menor (p1 - p2 < p0)" = "menor",
                                         "Diferente (p1 - p2 ≠ p0)" = "dos")),
                 
                 numericInput("ph_nivelconf", "Nivel de confianza (%):", value = 95, min = 80, max = 99.999, step = 0.1),
                 
                 actionButton("ph_calcular", "Realizar prueba", class = "btn-primary")
               ),
               
               mainPanel(
                 verbatimTextOutput("ph_resultados"),
                 plotOutput("ph_grafico", height = "400px")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Evento para Intervalo de Confianza
  observeEvent(input$ic_calcular, ignoreNULL = FALSE, {
    x1 <- input$ic_x1; n1 <- input$ic_n1
    x2 <- input$ic_x2; n2 <- input$ic_n2
    
    if (x1 > n1 | x2 > n2) {
      showNotification("Los éxitos no pueden ser mayores que el tamaño de la muestra", type = "error")
      return()
    }
    
    nc <- input$ic_nivel / 100
    alfa <- 1 - nc
    
    p1 <- x1 / n1
    p2 <- x2 / n2
    diff <- p1 - p2
    
    if (n1*p1 < 5 | n1*(1-p1) < 5 | n2*p2 < 5 | n2*(1-p2) < 5) {
      showNotification("Advertencia: Se recomienda np ≥ 5 y n(1-p) ≥ 5 para la aproximación normal", type = "warning")
    }
    
    # Error estándar no combinado para IC
    error_estandar <- sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
    
    z_critico <- qnorm(1 - alfa / 2)
    margen_error <- z_critico * error_estandar
    
    lim_inf <- diff - margen_error
    lim_sup <- diff + margen_error
    
    output$ic_resultados <- renderPrint({
      cat("p̂1 =", x1, "/", n1, "=", round(p1, 4), "\n")
      cat("p̂2 =", x2, "/", n2, "=", round(p2, 4), "\n\n")
      cat("Diferencia observada (p̂1 - p̂2) =", round(diff, 4), "\n")
      cat("Valor crítico (z) =", round(z_critico, 4), "\n")
      cat("Error estándar =", round(error_estandar, 4), "\n")
      cat("Margen de error =", round(margen_error, 4), "\n")
      cat("Intervalo de confianza al", input$ic_nivel, "% = [", round(lim_inf, 4), ",", round(lim_sup, 4), "]\n")
    })
    
    output$ic_grafica <- renderPlot({
      x_vals <- seq(diff - 4 * error_estandar, diff + 4 * error_estandar, length.out = 1000)
      y_vals <- dnorm(x_vals, mean = diff, sd = error_estandar)
      df <- data.frame(x = x_vals, y = y_vals)
      
      ggplot(df, aes(x, y)) +
        geom_line(color = "steelblue", linewidth = 1) +
        geom_vline(xintercept = diff, linetype = "dashed", color = "darkgreen", linewidth = 1) +
        geom_vline(xintercept = c(lim_inf, lim_sup), color = "red", linetype = "dotted", linewidth = 1) +
        geom_area(data = subset(df, x >= lim_inf & x <= lim_sup), aes(x = x, y = y), fill = "skyblue", alpha = 0.5) +
        labs(
          title = paste("Distribución de la diferencia de proporciones\nIntervalo de confianza al", input$ic_nivel, "%"),
          x = "Diferencia de proporciones (p̂1 - p̂2)",
          y = "Densidad"
        ) +
        theme_minimal()
    })
  })
  
  # Evento para Prueba de Hipótesis
  observeEvent(input$ph_calcular, ignoreNULL = FALSE, {
    x1 <- input$ph_x1; n1 <- input$ph_n1
    x2 <- input$ph_x2; n2 <- input$ph_n2
    
    if (x1 > n1 | x2 > n2) {
      showNotification("Los éxitos no pueden ser mayores que el tamaño de la muestra", type = "error")
      return()
    }
    
    d0 <- input$ph_d0
    tipo <- input$ph_tipo
    alpha <- 1 - (input$ph_nivelconf / 100)
    
    p1 <- x1 / n1
    p2 <- x2 / n2
    diff <- p1 - p2
    
    if (n1*p1 < 5 | n1*(1-p1) < 5 | n2*p2 < 5 | n2*(1-p2) < 5) {
      showNotification("Advertencia: Se recomienda np ≥ 5 y n(1-p) ≥ 5 para la aproximación normal", type = "warning")
    }
    
    # Cálculo del Error Estándar (Combinado solo si d0 == 0)
    if (d0 == 0) {
      p_comb <- (x1 + x2) / (n1 + n2)
      error_estandar <- sqrt(p_comb * (1 - p_comb) * (1/n1 + 1/n2))
    } else {
      p_comb <- NA
      error_estandar <- sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
    }
    
    z <- (diff - d0) / error_estandar
    
    p_value <- switch(tipo,
                      "menor" = pnorm(z),
                      "mayor" = 1 - pnorm(z),
                      "dos"   = 2 * min(pnorm(z), 1 - pnorm(z)))
    
    output$ph_resultados <- renderPrint({
      cat("p̂1 =", x1, "/", n1, "=", round(p1, 4), "\n")
      cat("p̂2 =", x2, "/", n2, "=", round(p2, 4), "\n\n")
      if (!is.na(p_comb)) {
        cat("Proporción combinada (p̂):", round(p_comb, 4), "\n")
      }
      cat("Diferencia observada (p̂1 - p̂2):", round(diff, 4), "\n")
      cat("Estadístico Z:", round(z, 4), "\n")
      cat("Valor p:", sprintf("%.6f", p_value), "\n\n")
      
      if (p_value < alpha) {
        cat("Decisión: Se rechaza H₀ (α =", alpha, ")\n")
      } else {
        cat("Decisión: No se rechaza H₀ (α =", alpha, ")\n")
      }
    })
    
    output$ph_grafico <- renderPlot({
      x_vals <- seq(-4, 4, length.out = 1000)
      y_vals <- dnorm(x_vals)
      df <- data.frame(x = x_vals, y = y_vals)
      
      g <- ggplot(df, aes(x, y)) +
        geom_line(color = "steelblue", linewidth = 1) +
        theme_minimal() +
        labs(
          title = "Distribución Normal Estándar bajo H₀",
          x = "Valor Z",
          y = "Densidad"
        )
      
      if (tipo == "menor") {
        zc <- qnorm(alpha)
        g <- g + geom_area(data = subset(df, x <= zc), aes(x = x, y = y), fill = "red", alpha = 0.4)
      } else if (tipo == "mayor") {
        zc <- qnorm(1 - alpha)
        g <- g + geom_area(data = subset(df, x >= zc), aes(x = x, y = y), fill = "red", alpha = 0.4)
      } else {
        zc <- qnorm(1 - alpha / 2)
        g <- g + geom_area(data = subset(df, x <= -zc), aes(x = x, y = y), fill = "red", alpha = 0.4) +
          geom_area(data = subset(df, x >= zc), aes(x = x, y = y), fill = "red", alpha = 0.4)
      }
      
      g + geom_vline(xintercept = z, color = "darkgreen", linewidth = 1, linetype = "dashed") +
        annotate("text", x = z, y = max(y_vals) * 0.9, label = paste("Z =", round(z, 2)), color = "darkgreen", angle = 90, vjust = -0.5)
    })
  })
}

shinyApp(ui, server)