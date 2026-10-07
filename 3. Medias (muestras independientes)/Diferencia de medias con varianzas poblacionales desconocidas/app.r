library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  # Estilos CSS optimizados para máxima compacidad
  tags$head(
    tags$style(HTML("
      .well { padding: 8px 12px; margin-bottom: 8px; }
      .MathJax { font-size: 80% !important; }
      .form-group { margin-bottom: 4px !important; } 
      .form-control { height: 28px; padding: 2px 6px; font-size: 12px; }
      label { font-size: 11px; margin-bottom: 2px; }
      h5 { margin-top: 4px; margin-bottom: 4px; font-size: 12px; }
      body { zoom: 0.95; }
    "))
  ),
  
  titlePanel(
    div(
      h2("Diferencia de Medias con Varianzas Desconocidas", align = "center"),
      tags$div(
        HTML("<i style='color:#555555; font-size:6  0%;'>App desarrollada por Liliana De la Torre Desentis</i>"),
        align = "center"
      )
    )
  ),
  
  sidebarLayout(
    
    # -------------------------------------------------------------
    # PANEL LATERAL ULTRACOMPACTO
    # -------------------------------------------------------------
    sidebarPanel(
      width = 3,
      
      radioButtons(
        inputId = "varianzas",
        label = strong("Supuesto de Varianzas:"),
        choices = c("Varianzas Iguales (Sp)", "Varianzas Diferentes (Welch)"),
        selected = "Varianzas Iguales (Sp)"
      ),
      hr(style = "margin: 6px 0;"),
      
      # SECCIÓN DE DATOS EN COMPACTO
      fluidRow(
        column(6,
               h5(strong("Muestra 1")),
               numericInput("xbar1", "Media (X̄₁):", value = 50),
               numericInput("s1", "Desv. (s₁):", value = 10, min = 0.0001),
               numericInput("n1", "Tamaño (n₁):", value = 30, min = 2)
        ),
        column(6,
               h5(strong("Muestra 2")),
               numericInput("xbar2", "Media (X̄₂):", value = 45),
               numericInput("s2", "Desv. (s₂):", value = 12, min = 0.0001),
               numericInput("n2", "Tamaño (n₂):", value = 35, min = 2)
        )
      ),
      
      hr(style = "margin: 6px 0;"),
      h5(strong("Parámetros Globales")),
      
      fluidRow(
        column(6, numericInput("conf", "Nivel de confianza (1-α) en (%):", value = 95, min = 50, max = 99.9, step = 0.5)),
        column(6, numericInput("alpha", "Significancia (α) en (%):", value = 5, min = 0.1, max = 50, step = 0.5))
      ),
      
      fluidRow(
        column(6, numericInput("mu0", "Diferencia H0:", value = 0)),
        column(6, selectInput("tipo_ph", "Prueba:", choices = c("Bilateral", "Cola derecha", "Cola izquierda")))
      ),
      
      hr(style = "margin: 6px 0;"),
      h5(strong("Tamaño de Muestra")),
      numericInput("error_tm", "Error Máx. Tol. (E):", value = 2, min = 0.0001)
    ),
    
    # -------------------------------------------------------------
    # PANEL PRINCIPAL CON PESTAÑAS
    # -------------------------------------------------------------
    mainPanel(
      width = 9,
      tabsetPanel(
        
        # --- PESTAÑA 1: INTERVALO DE CONFIANZA ---
        tabPanel("Intervalo de Confianza",
                 br(),
                 fluidRow(
                   column(5,
                          wellPanel(
                            h4("Fórmula Aplicada"),
                            uiOutput("formula_ic")
                          ),
                          wellPanel(
                            h4("Resultados del Intervalo"),
                            uiOutput("resultados_ic")
                          )
                   ),
                   column(7,
                          wellPanel(
                            h4("Gráfica del Intervalo de Confianza"),
                            plotOutput("grafica_ic", height = "380px")
                          )
                   )
                 )
        ),
        
        # --- PESTAÑA 2: PRUEBA DE HIPÓTESIS ---
        tabPanel("Prueba de Hipótesis",
                 br(),
                 fluidRow(
                   column(5,
                          wellPanel(
                            h4("Estadístico de Prueba"),
                            uiOutput("formula_ph")
                          ),
                          wellPanel(
                            h4("Resultados de la Prueba"),
                            uiOutput("resultados_ph")
                          )
                   ),
                   column(7,
                          wellPanel(
                            h4("Gráfica de la Prueba de Hipótesis"),
                            plotOutput("grafica_ph", height = "380px")
                          )
                   )
                 )
        ),
        
        # --- PESTAÑA 3: TAMAÑO DE MUESTRA ---
        tabPanel("Tamaño de Muestra",
                 br(),
                 fluidRow(
                   column(5,
                          wellPanel(
                            h4("Fórmula de Tamaño de Muestra"),
                            withMathJax("$$n = \\frac{Z_{\\alpha/2}^2 (s_1^2 + s_2^2)}{E^2}$$")
                          ),
                          wellPanel(
                            h4("Resultado de Muestra Requerida"),
                            uiOutput("resultados_tm")
                          )
                   ),
                   column(7,
                          wellPanel(
                            h4("Nota Metodológica"),
                            # UI Renderizado mediante HTML sin MathJax en línea para prevenir solapamiento
                            uiOutput("nota_tm")
                          )
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # --- SINCRONIZACIÓN ENTRE CONFIANZA (%) Y SIGNIFICANCIA (%) ---
  observeEvent(input$conf, {
    req(input$conf)
    alpha_calc <- round(100 - input$conf, 2)
    if (is.null(input$alpha) || abs(input$alpha - alpha_calc) > 0.001) {
      updateNumericInput(session, "alpha", value = alpha_calc)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$alpha, {
    req(input$alpha)
    conf_calc <- round(100 - input$alpha, 2)
    if (is.null(input$conf) || abs(input$conf - conf_calc) > 0.001) {
      updateNumericInput(session, "conf", value = conf_calc)
    }
  }, ignoreInit = TRUE)
  
  calc_ic <- reactive({
    req(input$xbar1, input$xbar2, input$s1, input$s2, input$n1, input$n2, input$conf)
    validate(need(input$n1 > 1 && input$n2 > 1, "Muestras deben ser n > 1"))
    
    diff_medias <- input$xbar1 - input$xbar2
    conf_dec <- input$conf / 100
    alpha_ic <- 1 - conf_dec
    
    if (input$varianzas == "Varianzas Iguales (Sp)") {
      gl <- input$n1 + input$n2 - 2
      sp2 <- (((input$n1 - 1) * input$s1^2) + ((input$n2 - 1) * input$s2^2)) / gl
      sp <- sqrt(sp2)
      se <- sp * sqrt((1 / input$n1) + (1 / input$n2))
    } else {
      se <- sqrt((input$s1^2 / input$n1) + (input$s2^2 / input$n2))
      gl <- ((input$s1^2 / input$n1) + (input$s2^2 / input$n2))^2 / 
        (((input$s1^2 / input$n1)^2 / (input$n1 - 1)) + ((input$s2^2 / input$n2)^2 / (input$n2 - 1)))
      sp <- NA
    }
    
    t_crit <- qt(1 - alpha_ic / 2, df = gl)
    margen_error <- t_crit * se
    
    list(
      diff = diff_medias, se = se, sp = sp, gl = gl,
      t_crit = t_crit, me = margen_error,
      li = diff_medias - margen_error, ls = diff_medias + margen_error
    )
  })
  
  calc_ph <- reactive({
    req(input$xbar1, input$xbar2, input$s1, input$s2, input$n1, input$n2, input$alpha, input$mu0)
    validate(need(input$n1 > 1 && input$n2 > 1, "Muestras deben ser n > 1"))
    
    diff_medias <- input$xbar1 - input$xbar2
    alpha_ph <- input$alpha / 100
    
    if (input$varianzas == "Varianzas Iguales (Sp)") {
      gl <- input$n1 + input$n2 - 2
      sp <- sqrt((((input$n1 - 1) * input$s1^2) + ((input$n2 - 1) * input$s2^2)) / gl)
      se <- sp * sqrt((1 / input$n1) + (1 / input$n2))
    } else {
      se <- sqrt((input$s1^2 / input$n1) + (input$s2^2 / input$n2))
      gl <- ((input$s1^2 / input$n1) + (input$s2^2 / input$n2))^2 / 
        (((input$s1^2 / input$n1)^2 / (input$n1 - 1)) + ((input$s2^2 / input$n2)^2 / (input$n2 - 1)))
      sp <- NA
    }
    
    t_calc <- (diff_medias - input$mu0) / se
    
    if (input$tipo_ph == "Bilateral") {
      t_crit <- qt(1 - alpha_ph / 2, df = gl)
      p_val <- 2 * (1 - pt(abs(t_calc), df = gl))
      rechaza <- abs(t_calc) > t_crit
    } else if (input$tipo_ph == "Cola derecha") {
      t_crit <- qt(1 - alpha_ph, df = gl)
      p_val <- 1 - pt(t_calc, df = gl)
      rechaza <- t_calc > t_crit
    } else {
      t_crit <- qt(alpha_ph, df = gl)
      p_val <- pt(t_calc, df = gl)
      rechaza <- t_calc < t_crit
    }
    
    list(t_calc = t_calc, t_crit = t_crit, p_val = p_val, gl = gl, rechaza = rechaza, tipo = input$tipo_ph)
  })
  
  output$formula_ic <- renderUI({
    if (input$varianzas == "Varianzas Iguales (Sp)") {
      withMathJax(
        "$$(\\bar{X}_1-\\bar{X}_2) \\pm t_{\\alpha/2} S_p \\sqrt{\\frac{1}{n_1}+\\frac{1}{n_2}}$$",
        "<b>Donde:</b>",
        "$$S_p = \\sqrt{\\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1+n_2-2}}$$"
      )
    } else {
      withMathJax("$$(\\bar{X}_1-\\bar{X}_2) \\pm t_{\\alpha/2} \\sqrt{\\frac{s_1^2}{n_1}+\\frac{s_2^2}{n_2}}$$")
    }
  })
  
  output$formula_ph <- renderUI({
    if (input$varianzas == "Varianzas Iguales (Sp)") {
      withMathJax(
        "$$t = \\frac{(\\bar{X}_1-\\bar{X}_2)-\\mu_0}{S_p \\sqrt{\\frac{1}{n_1}+\\frac{1}{n_2}}}$$",
        "<b>Donde:</b>",
        "$$S_p = \\sqrt{\\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1+n_2-2}}$$"
      )
    } else {
      withMathJax("$$t = \\frac{(\\bar{X}_1-\\bar{X}_2)-\\mu_0}{\\sqrt{\\frac{s_1^2}{n_1}+\\frac{s_2^2}{n_2}}}$$")
    }
  })
  
  output$resultados_ic <- renderUI({
    res <- calc_ic()
    sp_txt <- if (!is.na(res$sp)) paste0("<b>Sp (Desv. Ponderada):</b> ", round(res$sp, 4), "<br>") else ""
    
    HTML(paste0(
      "<b>Diferencia (X̄₁ - X̄₂):</b> ", round(res$diff, 4), "<br>",
      sp_txt,
      "<b>Error Estándar (SE):</b> ", round(res$se, 4), "<br>",
      "<b>Grados de Libertad:</b> ", round(res$gl, 2), "<br>",
      "<b>Valor Crítico t:</b> ", round(res$t_crit, 4), "<br>",
      "<b>Margen de Error:</b> ", round(res$me, 4), "<br><br>",
      "<b>Intervalo de Confianza:</b><br>",
      "[", round(res$li, 4), " ; ", round(res$ls, 4), "]"
    ))
  })
  
  output$resultados_ph <- renderUI({
    res <- calc_ph()
    decision <- if (res$rechaza) "<span style='color:red;'><b>Se rechaza H₀</b></span>" else "<span style='color:green;'><b>No se rechaza H₀</b></span>"
    
    HTML(paste0(
      "<b>Estadístico t Calculado:</b> ", round(res$t_calc, 4), "<br>",
      "<b>Valor Crítico t:</b> ", round(res$t_crit, 4), "<br>",
      "<b>Grados de Libertad:</b> ", round(res$gl, 2), "<br>",
      "<b>p-value:</b> ", round(res$p_val, 4), "<br><br>",
      "<b>Decisión:</b> ", decision
    ))
  })
  
  output$resultados_tm <- renderUI({
    req(input$s1, input$s2, input$error_tm, input$conf)
    
    alpha <- 1 - (input$conf / 100)
    z_crit <- qnorm(1 - alpha / 2)
    n_calc <- (z_crit^2 * (input$s1^2 + input$s2^2)) / (input$error_tm^2)
    n_final <- ceiling(n_calc)
    
    HTML(paste0(
      "<b>Valor Crítico Z:</b> ", round(z_crit, 4), "<br>",
      "<b>n Calculado (por grupo):</b> ", round(n_calc, 4), "<br><br>",
      "<b>Tamaño Final Requerido (n por grupo):</b> <br>",
      "<h3 style='margin-top:5px;'>", n_final, "</h3>"
    ))
  })
  
  # RENDERIZADO LIMPIO DE LA NOTA METODOLÓGICA CON HTML
  output$nota_tm <- renderUI({
    HTML("Esta fórmula calcula el tamaño de muestra requerido <b>por cada grupo</b> (asumiendo <i>n</i><sub>1</sub> = <i>n</i><sub>2</sub> = <i>n</i>) para estimar la diferencia de dos medias poblacionales con el margen de error <i>E</i> especificado en la barra lateral.")
  })
  
  output$grafica_ic <- renderPlot({
    res <- calc_ic()
    rango <- res$me * 2.5
    x <- seq(res$diff - rango, res$diff + rango, length.out = 1000)
    y <- dnorm(x, mean = res$diff, sd = res$se)
    df <- data.frame(x, y)
    
    ggplot(df, aes(x, y)) +
      geom_line(linewidth = 1) +
      geom_area(data = subset(df, x >= res$li & x <= res$ls), fill = "skyblue", alpha = 0.5) +
      geom_vline(xintercept = c(res$li, res$ls), color = "blue", linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = res$diff, color = "black", linewidth = 1) +
      labs(x = "Diferencia de Medias", y = "Densidad", title = paste0("IC del ", input$conf, "% para (μ₁ - μ₂)")) +
      theme_minimal()
  })
  
  output$grafica_ph <- renderPlot({
    res <- calc_ph()
    lim_x <- max(4, abs(res$t_calc) + 1, abs(res$t_crit) + 1)
    x <- seq(-lim_x, lim_x, length.out = 1000)
    y <- dt(x, df = res$gl)
    df <- data.frame(x, y)
    
    p <- ggplot(df, aes(x, y)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = res$t_calc, color = "blue", linewidth = 1.2, linetype = "solid") +
      labs(x = "Estadístico t", y = "Densidad", title = paste0("Prueba de Hipótesis (", res$tipo, ")")) +
      theme_minimal()
    
    if (res$tipo == "Cola derecha") {
      p <- p + geom_area(data = subset(df, x >= res$t_crit), fill = "red", alpha = 0.4) +
        geom_vline(xintercept = res$t_crit, color = "red", linetype = "dashed")
    } else if (res$tipo == "Cola izquierda") {
      p <- p + geom_area(data = subset(df, x <= res$t_crit), fill = "red", alpha = 0.4) +
        geom_vline(xintercept = res$t_crit, color = "red", linetype = "dashed")
    } else {
      tc <- abs(res$t_crit)
      p <- p + geom_area(data = subset(df, x >= tc), fill = "red", alpha = 0.4) +
        geom_area(data = subset(df, x <= -tc), fill = "red", alpha = 0.4) +
        geom_vline(xintercept = c(-tc, tc), color = "red", linetype = "dashed")
    }
    
    return(p)
  })
}

shinyApp(ui = ui, server = server)