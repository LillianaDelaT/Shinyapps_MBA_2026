library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Diferencia de proporciones iguales"),
  
  tags$div(
    style = "font-size: small; font-style: italic; margin-bottom: 15px;",
    "App desarrollada por Liliana De la Torre Desentis"
  ),
  
  tabsetPanel(
    
    tabPanel("Intervalo de confianza",
             withMathJax(),
             helpText("Fórmulas utilizadas:"),
             helpText("$$\\hat{p} = \\frac{n_1 \\hat{p}_1 + n_2 \\hat{p}_2}{n_1 + n_2}$$"),
             helpText("$$\\hat{p}_1 - \\hat{p}_2 \\pm z_{\\alpha/2} \\cdot \\sqrt{\\hat{p} \\hat{q} \\left(\\frac{1}{n_1} + \\frac{1}{n_2} \\right)}$$"),
             
             sidebarLayout(
               sidebarPanel(
                 numericInput("ic_x1", "Número de éxitos muestra 1 (x1):", value = 18, min = 0),
                 numericInput("ic_n1", "Tamaño muestra 1 (n1):", value = 150, min = 1),
                 numericInput("ic_x2", "Número de éxitos muestra 2 (x2):", value = 10, min = 0),
                 numericInput("ic_n2", "Tamaño muestra 2 (n2):", value = 130, min = 1),
                 numericInput("ic_nivel", "Nivel de confianza (%):", value = 95, min = 80, max = 99.9999, step = 0.1),
                 actionButton("ic_calcular", "Calcular")
               ),
               
               mainPanel(
                 verbatimTextOutput("ic_resultados"),
                 plotOutput("ic_grafica", height="400px")
               )
             )
    ),
    
    
    tabPanel("Prueba de hipótesis",
             
             sidebarLayout(
               sidebarPanel(
                 
                 numericInput("ph_x1", "Éxitos muestra 1 (x1):", value = 50, min = 0),
                 numericInput("ph_n1", "Tamaño muestra 1 (n1):", value = 100, min = 1),
                 numericInput("ph_x2", "Éxitos muestra 2 (x2):", value = 45, min = 0),
                 numericInput("ph_n2", "Tamaño muestra 2 (n2):", value = 100, min = 1),
                 numericInput("ph_d0", "Diferencia bajo H₀ (p₀):", value = 0, step = 0.01),
                 
                 selectInput("ph_tipo", "Tipo de prueba (Ha):",
                             choices = c("Mayor" = "mayor",
                                         "Menor" = "menor",
                                         "Diferente" = "dos")),
                 
                 numericInput("ph_nivelconf", "Nivel de confianza:",
                              value = 0.95, min = 0.8, max = 0.9999, step = 0.01),
                 
                 actionButton("ph_calcular", "Realizar prueba")
                 
               ),
               
               mainPanel(
                 
                 withMathJax(
                   helpText("**Estadístico de prueba Z:**"),
                   helpText("$$Z = \\frac{\\hat{p}_1 - \\hat{p}_2 - p_0}{\\sqrt{\\hat{p} (1 - \\hat{p}) \\left( \\frac{1}{n_1} + \\frac{1}{n_2} \\right)}}$$")
                 ),
                 
                 verbatimTextOutput("ph_resultados"),
                 plotOutput("ph_grafico", height="400px")
                 
               )
               
             )
             
    )
    
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$ic_calcular, {
    
    x1 <- input$ic_x1; n1 <- input$ic_n1
    x2 <- input$ic_x2; n2 <- input$ic_n2
    
    if(x1 > n1 | x2 > n2){
      showNotification("Los éxitos no pueden ser mayores que el tamaño de muestra", type="error")
      return()
    }
    
    nc <- input$ic_nivel / 100
    alfa <- 1 - nc
    
    p1 <- x1 / n1
    p2 <- x2 / n2
    diff <- p1 - p2
    
    if(n1*p1 < 5 | n1*(1-p1) < 5 | n2*p2 < 5 | n2*(1-p2) < 5){
      showNotification("Advertencia: la aproximación normal puede no ser adecuada", type="warning")
    }
    
    p_comb <- (x1 + x2) / (n1 + n2)
    
    z_critico <- qnorm(1 - alfa/2)
    
    error_estandar <- sqrt(p_comb * (1 - p_comb) * (1/n1 + 1/n2))
    
    margen_error <- z_critico * error_estandar
    
    lim_inf <- diff - margen_error
    lim_sup <- diff + margen_error
    
    output$ic_resultados <- renderPrint({
      
      cat("p̂1 =", x1,"/",n1,"=", round(p1,4), "\n")
      cat("p̂2 =", x2,"/",n2,"=", round(p2,4), "\n\n")
      
      cat("Diferencia de proporciones =", round(diff, 4), "\n")
      cat("Proporción combinada =", round(p_comb, 4), "\n")
      cat("Valor crítico (z) =", round(z_critico, 4), "\n")
      cat("Error estándar =", round(error_estandar, 4), "\n")
      cat("Margen de error =", round(margen_error, 4), "\n")
      cat("Intervalo de confianza al", input$ic_nivel, "% = [", round(lim_inf, 4), ",", round(lim_sup, 4), "]\n")
      
    })
    
    output$ic_grafica <- renderPlot({
      
      x_vals <- seq(diff - 4*error_estandar, diff + 4*error_estandar, length.out = 1000)
      y_vals <- dnorm(x_vals, mean = diff, sd = error_estandar)
      
      df <- data.frame(x = x_vals, y = y_vals)
      
      ggplot(df, aes(x, y)) +
        geom_line(color = "steelblue", linewidth = 1) +
        geom_vline(xintercept = diff, linetype = "dashed", color = "darkgreen", linewidth = 1) +
        geom_vline(xintercept = c(lim_inf, lim_sup), color = "red", linetype = "dotted", linewidth = 1) +
        geom_area(data = subset(df, x >= lim_inf & x <= lim_sup),
                  aes(x = x, y = y),
                  fill = "skyblue", alpha = 0.5) +
        labs(
          title = paste("Distribución aproximada de (p̂1 − p̂2)\nIntervalo de confianza del", input$ic_nivel, "%"),
          x = "Diferencia de proporciones",
          y = "Densidad"
        ) +
        theme_minimal()
    })
    
  })
  
  
  observeEvent(input$ph_calcular, {
    
    x1 <- input$ph_x1; n1 <- input$ph_n1
    x2 <- input$ph_x2; n2 <- input$ph_n2
    
    if(x1 > n1 | x2 > n2){
      showNotification("Los éxitos no pueden ser mayores que el tamaño de muestra", type="error")
      return()
    }
    
    d0 <- input$ph_d0
    tipo <- input$ph_tipo
    nivelconf <- input$ph_nivelconf
    
    p1 <- x1 / n1
    p2 <- x2 / n2
    
    if(n1*p1 < 5 | n1*(1-p1) < 5 | n2*p2 < 5 | n2*(1-p2) < 5){
      showNotification("Advertencia: la aproximación normal puede no ser adecuada", type="warning")
    }
    
    p_comb <- (x1 + x2) / (n1 + n2)
    
    diff <- p1 - p2
    
    error_estandar <- sqrt(p_comb * (1 - p_comb) * (1/n1 + 1/n2))
    
    z <- (diff - d0) / error_estandar
    
    alpha <- 1 - nivelconf
    
    p_value <- switch(tipo,
                      "menor" = pnorm(z),
                      "mayor" = 1 - pnorm(z),
                      "dos" = 2 * min(pnorm(z), 1 - pnorm(z)))
    
    output$ph_resultados <- renderPrint({
      
      cat("p̂1 =", x1,"/",n1,"=", round(p1,4), "\n")
      cat("p̂2 =", x2,"/",n2,"=", round(p2,4), "\n\n")
      
      cat("Proporción combinada (p̂):", round(p_comb, 4), "\n")
      cat("Diferencia observada (p1 - p2):", round(diff, 4), "\n")
      cat("Estadístico Z:", round(z, 4), "\n")
      cat("Valor p:", sprintf("%.6f", p_value), "\n")
      
      cat(ifelse(p_value < alpha,
                 "Se rechaza H₀\n",
                 "No se rechaza H₀\n"))
    })
    
    output$ph_grafico <- renderPlot({
      
      x_vals <- seq(-4, 4, length.out = 2000)
      y_vals <- dnorm(x_vals)
      
      plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue",
           main = "Distribución Normal Estándar",
           xlab = "Valor de Z", ylab = "Densidad")
      
      if(tipo == "menor"){
        zc <- qnorm(alpha)
        xs <- seq(-4, zc, length.out=500)
        polygon(c(xs, rev(xs)), c(dnorm(xs), rep(0,length(xs))), col="red", density=20)
      } else if(tipo == "mayor"){
        zc <- qnorm(1-alpha)
        xs <- seq(zc, 4, length.out=500)
        polygon(c(xs, rev(xs)), c(dnorm(xs), rep(0,length(xs))), col="red", density=20)
      } else {
        zc <- qnorm(1-alpha/2)
        xs1 <- seq(-4, -zc, length.out=500)
        xs2 <- seq(zc, 4, length.out=500)
        polygon(c(xs1, rev(xs1)), c(dnorm(xs1), rep(0,length(xs1))), col="red", density=20)
        polygon(c(xs2, rev(xs2)), c(dnorm(xs2), rep(0,length(xs2))), col="red", density=20)
      }
      
      abline(v=z, col="green", lwd=2)
      
    })
    
  })
  
}

shinyApp(ui, server)