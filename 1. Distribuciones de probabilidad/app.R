# ============================================================
# APP: CÁLCULO DE PROBABILIDADES Y VALORES CRÍTICOS
# Desarrollada por Liliana De la Torre Desentis
# ============================================================

# ============================================================
# 1. CARGAR PAQUETES
# ============================================================
library(shiny)
library(ggplot2)
library(dplyr)

# ============================================================
# 2. UI (INTERFAZ DE USUARIO)
# ============================================================
ui <- fluidPage(
  
  # Estilos CSS globales para evitar scroll y ajustar tamaño
  tags$head(
    tags$style(
      HTML("
        body { padding-top: 5px; padding-bottom: 5px; font-size: 13px; }
        .well { padding: 10px; margin-bottom: 0px; }
        .form-group { margin-bottom: 6px; }
        hr { margin-top: 6px; margin-bottom: 6px; }
        h3 { margin-top: 0px; margin-bottom: 4px; font-size: 18px; }
        h4 { margin-top: 2px; margin-bottom: 4px; font-size: 14px; }
        pre { padding: 4px; margin-bottom: 4px; font-size: 12px; }
        .formula-box {
          font-family: 'Times New Roman', Times, serif;
          font-size: 18px;
          font-style: italic;
          margin-bottom: 6px;
        }
      ")
    )
  ),
  
  titlePanel(
    div(
      h3("Distribuciones de probabilidad: áreas y valores críticos"),
      p("App desarrollada por Liliana De la Torre Desentis", style = "font-size:11px; color:gray; margin-bottom: 5px;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      selectInput(
        "distribucion",
        "Seleccionar distribución",
        choices = c(
          "Normal",
          "t de Student",
          "Chi-cuadrada",
          "F"
        )
      ),
      
      radioButtons(
        "calculo",
        "¿Qué deseas calcular?",
        choices = c(
          "Probabilidad (área bajo la curva)",
          "Valor crítico"
        ),
        selected = "Probabilidad (área bajo la curva)"
      ),
      
      # PROBABILIDAD
      conditionalPanel(
        condition = "input.calculo == 'Probabilidad (área bajo la curva)'",
        tags$hr(),
        
        conditionalPanel(
          condition = "input.distribucion == 'Normal'",
          numericInput("mean", "Media (μ):", value = 0),
          numericInput("sd", "Desviación estándar (σ):", value = 1, min = 0.0001)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 't de Student'",
          numericInput("df_prob", "Grados de libertad (ν):", value = 10, min = 1, step = 1)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 'Chi-cuadrada'",
          numericInput("df_prob_chi", "Grados de libertad (ν):", value = 10, min = 1, step = 1)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 'F'",
          numericInput("df1_prob", "Grados de libertad del numerador (ν₁):", value = 5, min = 1, step = 1),
          numericInput("df2_prob", "Grados de libertad del denominador (ν₂):", value = 10, min = 1, step = 1)
        ),
        
        selectInput(
          "tipo_area",
          "Tipo de área",
          choices = c(
            "Cola derecha",
            "Cola izquierda",
            "Entre dos valores"
          )
        ),
        
        numericInput("x1", "Valor de X:", value = 0),
        
        conditionalPanel(
          condition = "input.tipo_area == 'Entre dos valores'",
          numericInput("x2", "Segundo valor de X:", value = 1)
        )
      ),
      
      # VALOR CRÍTICO
      conditionalPanel(
        condition = "input.calculo == 'Valor crítico'",
        tags$hr(),
        
        conditionalPanel(
          condition = "input.distribucion == 'Normal'",
          numericInput("mean_vc", "Media (μ):", value = 0),
          numericInput("sd_vc", "Desviación estándar (σ):", value = 1, min = 0.0001)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 't de Student'",
          numericInput("df_vc", "Grados de libertad (ν):", value = 10, min = 1, step = 1)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 'Chi-cuadrada'",
          numericInput("df_vc_chi", "Grados de libertad (ν):", value = 10, min = 1, step = 1)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 'F'",
          numericInput("df1_vc", "Grados de libertad del numerador (ν₁):", value = 5, min = 1, step = 1),
          numericInput("df2_vc", "Grados de libertad del denominador (ν₂):", value = 10, min = 1, step = 1)
        ),
        
        radioButtons(
          "cola",
          "Tipo de cola / prueba",
          choices = c(
            "Cola izquierda",
            "Cola derecha",
            "Dos colas"
          ),
          selected = "Cola derecha"
        ),
        
        numericInput(
          "alpha",
          "Nivel de significancia (α):",
          value = 0.05,
          min = 0.0001,
          max = 0.9999,
          step = 0.01
        )
      ),
      
      actionButton(
        "calcular",
        "Calcular",
        class = "btn-primary",
        style = "width: 100%; margin-top: 5px;"
      )
    ),
    
    mainPanel(
      width = 8,
      uiOutput("info_distribucion"),
      tags$b("Resultado:"),
      verbatimTextOutput("resultado"),
      uiOutput("interpretacion"),
      plotOutput("grafica", height = "280px")
    )
  )
)

# ============================================================
# 3. SERVER (LÓGICA DEL SERVIDOR)
# ============================================================
server <- function(input, output, session) {
  
  resultado <- reactiveVal(NULL)
  valores_criticos <- reactiveVal(NULL)
  
  observeEvent(list(input$distribucion, input$calculo), {
    resultado(NULL)
    valores_criticos(NULL)
  })
  
  # INFORMACIÓN DE LA DISTRIBUCIÓN (FÓRMULAS VÍA HTML DE ALTA CALIDAD)
  output$info_distribucion <- renderUI({
    dist <- input$distribucion
    
    formula_html <- switch(dist,
      "Normal" = "<i>X</i> ~ <i>N</i>(&mu;, &sigma;)",
      "t de Student" = "<i>T</i> ~ <i>t</i><sub>&nu;</sub>",
      "Chi-cuadrada" = "<i>X</i><sup>2</sup> ~ &chi;<sup>2</sup><sub>&nu;</sub>",
      "F" = "<i>F</i> ~ <i>F</i><sub>&nu;<sub>1</sub>, &nu;<sub>2</sub></sub>"
    )
    
    tagList(
      tags$h4(paste("Distribución", dist)),
      tags$div(class = "formula-box", HTML(formula_html))
    )
  })
  
  # CÁLCULOS
  observeEvent(input$calcular, {
    dist <- input$distribucion
    calc <- input$calculo
    
    if (calc == "Probabilidad (área bajo la curva)") {
      tipo <- input$tipo_area
      x1 <- input$x1
      x2 <- if (tipo == "Entre dos valores") input$x2 else NA
      
      if (dist == "Normal") {
        mu <- input$mean; sigma <- input$sd
        if (is.null(sigma) || is.na(sigma) || sigma <= 0) {
          showNotification("La desviación estándar σ debe ser mayor que cero.", type = "error"); return()
        }
      } else if (dist == "t de Student") {
        df_val <- input$df_prob
        if (is.null(df_val) || is.na(df_val) || df_val <= 0) {
          showNotification("Los grados de libertad deben ser mayores que cero.", type = "error"); return()
        }
      } else if (dist == "Chi-cuadrada") {
        df_val <- input$df_prob_chi
        if (is.null(df_val) || is.na(df_val) || df_val <= 0) {
          showNotification("Los grados de libertad deben ser mayores que cero.", type = "error"); return()
        }
        if (x1 < 0 || (tipo == "Entre dos valores" && x2 < 0)) {
          showNotification("La distribución Chi-cuadrada solo admite valores X ≥ 0.", type = "error"); return()
        }
      } else {
        df1_val <- input$df1_prob; df2_val <- input$df2_prob
        if (df1_val <= 0 || df2_val <= 0) {
          showNotification("Los grados de libertad deben ser mayores que cero.", type = "error"); return()
        }
        if (x1 < 0 || (tipo == "Entre dos valores" && x2 < 0)) {
          showNotification("La distribución F solo admite valores X ≥ 0.", type = "error"); return()
        }
      }
      
      if (is.null(x1) || is.na(x1)) {
        showNotification("Debes introducir un valor de X.", type = "error"); return()
      }
      if (tipo == "Entre dos valores") {
        if (is.null(x2) || is.na(x2)) {
          showNotification("Debes introducir ambos valores de X.", type = "error"); return()
        }
        if (x1 >= x2) {
          showNotification("El primer valor debe ser menor que el segundo valor.", type = "error"); return()
        }
      }
      
      if (tipo == "Cola derecha") {
        prob <- switch(dist,
          "Normal" = stats::pnorm(x1, mean = mu, sd = sigma, lower.tail = FALSE),
          "t de Student" = stats::pt(x1, df = df_val, lower.tail = FALSE),
          "Chi-cuadrada" = stats::pchisq(x1, df = df_val, lower.tail = FALSE),
          "F" = stats::pf(x1, df1 = df1_val, df2 = df2_val, lower.tail = FALSE)
        )
        res <- paste0("P(X > ", round(x1, 4), ") = ", format(round(prob, 6), nsmall = 6))
      } else if (tipo == "Cola izquierda") {
        prob <- switch(dist,
          "Normal" = stats::pnorm(x1, mean = mu, sd = sigma),
          "t de Student" = stats::pt(x1, df = df_val),
          "Chi-cuadrada" = stats::pchisq(x1, df = df_val),
          "F" = stats::pf(x1, df1 = df1_val, df2 = df2_val)
        )
        res <- paste0("P(X < ", round(x1, 4), ") = ", format(round(prob, 6), nsmall = 6))
      } else {
        prob <- switch(dist,
          "Normal" = stats::pnorm(x2, mu, sigma) - stats::pnorm(x1, mu, sigma),
          "t de Student" = stats::pt(x2, df_val) - stats::pt(x1, df_val),
          "Chi-cuadrada" = stats::pchisq(x2, df_val) - stats::pchisq(x1, df_val),
          "F" = stats::pf(x2, df1_val, df2_val) - stats::pf(x1, df1_val, df2_val)
        )
        res <- paste0("P(", round(x1, 4), " < X < ", round(x2, 4), ") = ", format(round(prob, 6), nsmall = 6))
      }
      
      resultado(res)
      valores_criticos(NULL)
      
    } else {
      alpha <- input$alpha
      if (is.null(alpha) || is.na(alpha) || alpha <= 0 || alpha >= 1) {
        showNotification("El nivel de significancia α debe estar entre 0 y 1.", type = "error"); return()
      }
      
      if (dist == "Normal") {
        mu <- input$mean_vc; sigma <- input$sd_vc
        if (sigma <= 0) { showNotification("La desviación estándar σ debe ser mayor que cero.", type = "error"); return() }
      } else if (dist == "t de Student") {
        df_val <- input$df_vc
        if (df_val <= 0) { showNotification("Los grados de libertad deben ser mayores que cero.", type = "error"); return() }
      } else if (dist == "Chi-cuadrada") {
        df_val <- input$df_vc_chi
        if (df_val <= 0) { showNotification("Los grados de libertad deben ser mayores que cero.", type = "error"); return() }
      } else {
        df1_val <- input$df1_vc; df2_val <- input$df2_vc
        if (df1_val <= 0 || df2_val <= 0) { showNotification("Los grados de libertad deben ser mayores que cero.", type = "error"); return() }
      }
      
      if (input$cola == "Cola izquierda") {
        cuan <- switch(dist,
          "Normal" = stats::qnorm(alpha, mean = mu, sd = sigma, lower.tail = TRUE),
          "t de Student" = stats::qt(alpha, df = df_val, lower.tail = TRUE),
          "Chi-cuadrada" = stats::qchisq(alpha, df = df_val, lower.tail = TRUE),
          "F" = stats::qf(alpha, df1 = df1_val, df2 = df2_val, lower.tail = TRUE)
        )
        res <- paste0("Valor crítico = ", format(round(cuan, 4), nsmall = 4), "\nÁrea acumulada a la izquierda = ", format(round(alpha, 4), nsmall = 4))
        valores_criticos(c(cuan))
        
      } else if (input$cola == "Cola derecha") {
        cuan <- switch(dist,
          "Normal" = stats::qnorm(alpha, mean = mu, sd = sigma, lower.tail = FALSE),
          "t de Student" = stats::qt(alpha, df = df_val, lower.tail = FALSE),
          "Chi-cuadrada" = stats::qchisq(alpha, df = df_val, lower.tail = FALSE),
          "F" = stats::qf(alpha, df1 = df1_val, df2 = df2_val, lower.tail = FALSE)
        )
        res <- paste0("Valor crítico = ", format(round(cuan, 4), nsmall = 4), "\nÁrea acumulada a la derecha = ", format(round(alpha, 4), nsmall = 4))
        valores_criticos(c(cuan))
        
      } else {
        alpha_2 <- alpha / 2
        inferior <- switch(dist,
          "Normal" = stats::qnorm(alpha_2, mean = mu, sd = sigma),
          "t de Student" = stats::qt(alpha_2, df = df_val),
          "Chi-cuadrada" = stats::qchisq(alpha_2, df = df_val),
          "F" = stats::qf(alpha_2, df1 = df1_val, df2 = df2_val)
        )
        superior <- switch(dist,
          "Normal" = stats::qnorm(1 - alpha_2, mean = mu, sd = sigma),
          "t de Student" = stats::qt(1 - alpha_2, df = df_val),
          "Chi-cuadrada" = stats::qchisq(1 - alpha_2, df = df_val),
          "F" = stats::qf(1 - alpha_2, df1 = df1_val, df2 = df2_val)
        )
        res <- paste0("Valor crítico inferior = ", format(round(inferior, 4), nsmall = 4),
                      "\nValor crítico superior = ", format(round(superior, 4), nsmall = 4),
                      "\nÁrea en cada cola = ", alpha_2)
        valores_criticos(c(inferior, superior))
      }
      
      resultado(res)
    }
  })
  
  output$resultado <- renderPrint({
    req(resultado())
    cat(resultado())
  })
  
  output$interpretacion <- renderUI({
    req(resultado())
    calc <- input$calculo
    
    if (calc == "Probabilidad (área bajo la curva)") {
      tipo <- input$tipo_area
      if (tipo == "Cola derecha") {
        texto <- paste0("El área sombreada representa la probabilidad de obtener un valor mayor o igual a ", round(input$x1, 4), ".")
      } else if (tipo == "Cola izquierda") {
        texto <- paste0("El área sombreada representa la probabilidad de obtener un valor menor o igual a ", round(input$x1, 4), ".")
      } else {
        texto <- paste0("El área sombreada representa la probabilidad de obtener un valor entre ", round(input$x1, 4), " y ", round(input$x2, 4), ".")
      }
    } else {
      if (input$cola == "Dos colas") {
        texto <- paste0("Las zonas sombreadas en naranja representan las regiones de rechazo en ambas colas, sumando un área total de α = ", input$alpha, ".")
      } else {
        texto <- paste0("La zona sombreada en naranja representa la región de rechazo con un área de α = ", input$alpha, ".")
      }
    }
    
    p(tags$b("Interpretación: "), texto, style = "margin-top: 2px; margin-bottom: 5px;")
  })
  
  output$grafica <- renderPlot({
    req(resultado())
    
    dist <- input$distribucion
    calc <- input$calculo
    
    if (dist == "Normal") {
      mu <- if (calc == "Probabilidad (área bajo la curva)") input$mean else input$mean_vc
      sigma <- if (calc == "Probabilidad (área bajo la curva)") input$sd else input$sd_vc
    }
    
    if (dist == "t de Student") {
      df_val <- if (calc == "Probabilidad (área bajo la curva)") input$df_prob else input$df_vc
    } else if (dist == "Chi-cuadrada") {
      df_val <- if (calc == "Probabilidad (área bajo la curva)") input$df_prob_chi else input$df_vc_chi
    } else if (dist == "F") {
      df1_val <- if (calc == "Probabilidad (área bajo la curva)") input$df1_prob else input$df1_vc
      df2_val <- if (calc == "Probabilidad (área bajo la curva)") input$df2_prob else input$df2_vc
    }
    
    if (dist == "Normal") {
      rango_x <- c(mu - 4 * sigma, mu + 4 * sigma)
    } else if (dist == "t de Student") {
      rango_x <- c(stats::qt(0.001, df_val), stats::qt(0.999, df_val))
    } else if (dist == "Chi-cuadrada") {
      rango_x <- c(0, stats::qchisq(0.999, df_val))
    } else {
      rango_x <- c(0, stats::qf(0.999, df1_val, df2_val))
    }
    
    x_vals <- seq(rango_x[1], rango_x[2], length.out = 1000)
    
    densidad <- switch(dist,
      "Normal" = stats::dnorm(x_vals, mean = mu, sd = sigma),
      "t de Student" = stats::dt(x_vals, df = df_val),
      "Chi-cuadrada" = stats::dchisq(x_vals, df = df_val),
      "F" = stats::df(x_vals, df1 = df1_val, df2 = df2_val)
    )
    
    datos <- data.frame(x = x_vals, y = densidad)
    
    p <- ggplot(datos, aes(x = x, y = y)) +
      geom_line(linewidth = 1, color = "#1F4E79") +
      theme_minimal(base_size = 11) +
      labs(x = "x", y = "Densidad") +
      theme(
        plot.margin = margin(2, 2, 2, 2),
        axis.title = element_text(face = "bold")
      )
    
    if (calc == "Probabilidad (área bajo la curva)") {
      tipo <- input$tipo_area
      x1 <- input$x1
      
      if (tipo == "Cola derecha") {
        area_data <- datos[datos$x >= x1, ]
        p <- p +
          geom_area(data = area_data, aes(x = x, y = y), fill = "#87CEEB", alpha = 0.65) +
          geom_vline(xintercept = x1, color = "#C00000", linetype = "dashed", linewidth = 0.8)
          
      } else if (tipo == "Cola izquierda") {
        area_data <- datos[datos$x <= x1, ]
        p <- p +
          geom_area(data = area_data, aes(x = x, y = y), fill = "#87CEEB", alpha = 0.65) +
          geom_vline(xintercept = x1, color = "#C00000", linetype = "dashed", linewidth = 0.8)
          
      } else {
        x2 <- input$x2
        area_data <- datos[datos$x >= x1 & datos$x <= x2, ]
        p <- p +
          geom_area(data = area_data, aes(x = x, y = y), fill = "#87CEEB", alpha = 0.65) +
          geom_vline(xintercept = x1, color = "#C00000", linetype = "dashed", linewidth = 0.8) +
          geom_vline(xintercept = x2, color = "#C00000", linetype = "dashed", linewidth = 0.8)
      }
      
      p <- p + annotate("text", x = mean(rango_x), y = max(densidad) * 0.9, label = resultado(), size = 4, fontface = "bold")
      
    } else {
      criticos <- valores_criticos()
      req(criticos)
      
      if (length(criticos) == 1) {
        vc <- criticos[1]
        area_data <- if (input$cola == "Cola izquierda") datos[datos$x <= vc, ] else datos[datos$x >= vc, ]
        
        p <- p +
          geom_area(data = area_data, aes(x = x, y = y), fill = "#F4B183", alpha = 0.7) +
          geom_vline(xintercept = vc, color = "#C00000", linetype = "dashed", linewidth = 0.8)
          
      } else {
        inferior <- criticos[1]
        superior <- criticos[2]
        area_izq <- datos[datos$x <= inferior, ]
        area_der <- datos[datos$x >= superior, ]
        
        p <- p +
          geom_area(data = area_izq, aes(x = x, y = y), fill = "#F4B183", alpha = 0.7) +
          geom_area(data = area_der, aes(x = x, y = y), fill = "#F4B183", alpha = 0.7) +
          geom_vline(xintercept = inferior, color = "#C00000", linetype = "dashed", linewidth = 0.8) +
          geom_vline(xintercept = superior, color = "#C00000", linetype = "dashed", linewidth = 0.8)
      }
      
      p <- p + annotate("text", x = mean(rango_x), y = max(densidad) * 0.9, label = "Región de rechazo", size = 4, fontface = "bold")
    }
    
    return(p)
  })
}

# ============================================================
# 4. EJECUTAR APP
# ============================================================
shinyApp(ui = ui, server = server)