# ============================================================
# MEDIA
# ============================================================
# INSTALACIÓN Y CARGA DE PAQUETES
# ============================================================

paquetes <- c("shiny", "ggplot2", "plotly", "bslib")

# Instalar los paquetes que no estén instalados
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]

if (length(faltantes) > 0) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages(faltantes)
}

# Cargar paquetes
lapply(paquetes, library, character.only = TRUE)


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  withMathJax(),
  
  titlePanel("Inferencia para la media"),
  
  div(
    style = "margin-top:-10px; margin-bottom:15px;",
    tags$em("App desarrollada por Liliana De la Torre Desentis")
  ),
  
  hr(),
  
  selectInput(
    "varianza_tipo",
    "Tipo de varianza",
    c(
      "Varianza poblacional conocida",
      "Varianza poblacional desconocida"
    )
  ),
  
  tabsetPanel(
    
    #========================================================
    # INTERVALO DE CONFIANZA
    #========================================================
    
    tabPanel(
      "Intervalo de confianza",
      
      br(),
      
      fluidRow(
        
        column(
          width = 3,
          
          wellPanel(
            
            h4("Datos"),
            
            uiOutput("panel_datos_ic")
          )
        ),
        
        column(
          width = 3,
          
          wellPanel(
            
            h4("Fórmula"),
            
            uiOutput("formula_ic"),
            
            hr(),
            
            h4("Resultados"),
            
            verbatimTextOutput("res_ic")
          )
        ),
        
        column(
          width = 6,
          
          wellPanel(
            
            h4("Gráfica"),
            
            plotlyOutput(
              "plot_ic",
              height = "550px"
            )
          )
        )
      )
    ),
    
    #========================================================
    # PRUEBA DE HIPÓTESIS
    #========================================================
    
    tabPanel(
      "Prueba de hipótesis",
      
      br(),
      
      fluidRow(
        
        #====================================================
        # DATOS
        #====================================================
        
        column(
          width = 3,
          
          wellPanel(
            
            h4("Datos"),
            
            uiOutput("panel_datos_ph")
          )
        ),
        
        #====================================================
        # RESULTADOS
        #====================================================
        
        column(
          width = 3,
          
          wellPanel(
            
            h4("Fórmula"),
            
            uiOutput("formula_ph"),
            
            hr(),
            
            h4("Resultados"),
            
            verbatimTextOutput("res_ph")
          )
        ),
        
        #====================================================
        # GRÁFICA
        #====================================================
        
        column(
          width = 6,
          
          wellPanel(
            
            h4("Gráfica"),
            
            plotlyOutput(
              "plot_ph",
              height = "550px"
            )
          )
        )
      )
    ),
    
    #========================================================
    # TAMAÑO DE MUESTRA
    #========================================================
    
    tabPanel(
      "Tamaño de muestra",
      
      br(),
      
      fluidRow(
        
        #====================================================
        # DATOS
        #====================================================
        
        column(
          width = 3,
          
          wellPanel(
            
            h4("Datos"),
            
            numericInput(
              "tm_sigma",
              "σ",
              10,
              min = 0.0001
            ),
            
            numericInput(
              "tm_error",
              "Error máximo permitido ε",
              2,
              min = 0.0001
            ),
            
            numericInput(
              "tm_nivel",
              "Nivel de confianza (%)",
              95,
              min = 0.01,
              max = 99.99
            ),
            
            actionButton(
              "calc_tm",
              "Calcular"
            )
          )
        ),
        
        #====================================================
        # FÓRMULA
        #====================================================
        
        column(
          width = 3,
          
          wellPanel(
            
            h4("Fórmula"),
            
            withMathJax(),
            
            helpText(
              HTML(
                "$$n = \\left(\\frac{z_{\\alpha/2}\\sigma}{\\varepsilon}\\right)^2$$"
              )
            )
          )
        ),
        
        #====================================================
        # RESULTADOS
        #====================================================
        
        column(
          width = 6,
          
          wellPanel(
            
            h4("Resultados"),
            
            verbatimTextOutput("res_tm")
          )
        )
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session){
  
  #==========================================================
  # PANEL DATOS IC
  #==========================================================
  
  output$panel_datos_ic <- renderUI({
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      tagList(
        
        numericInput(
          "ic_k_xbar",
          "Media muestral (X̄)",
          50
        ),
        
        numericInput(
          "ic_k_desv",
          "Desviación estándar poblacional (σ)",
          10,
          min = 0.0001
        ),
        
        numericInput(
          "ic_k_n",
          "Tamaño de muestra (n)",
          30,
          min = 2,
          step = 1
        ),
        
        numericInput(
          "ic_k_nivel",
          "Nivel de confianza (%)",
          95,
          min = 0.01,
          max = 99.99
        ),
        
        actionButton(
          "calc_ic",
          "Calcular"
        )
      )
      
    } else {
      
      tagList(
        
        numericInput(
          "ic_d_xbar",
          "Media muestral (X̄)",
          50
        ),
        
        numericInput(
          "ic_d_desv",
          "Desviación estándar muestral (s)",
          10,
          min = 0.0001
        ),
        
        numericInput(
          "ic_d_n",
          "Tamaño de muestra (n)",
          30,
          min = 2,
          step = 1
        ),
        
        numericInput(
          "ic_d_nivel",
          "Nivel de confianza (%)",
          95,
          min = 0.01,
          max = 99.99
        ),
        
        actionButton(
          "calc_ic",
          "Calcular"
        )
      )
    }
  })
  
  
  #==========================================================
  # PANEL DATOS PH
  #==========================================================
  
  output$panel_datos_ph <- renderUI({
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      tagList(
        
        numericInput(
          "ph_k_xbar",
          "Media muestral (X̄)",
          52
        ),
        
        numericInput(
          "ph_k_desv",
          "Desviación estándar poblacional (σ)",
          10,
          min = 0.0001
        ),
        
        numericInput(
          "ph_k_n",
          "Tamaño de muestra (n)",
          30,
          min = 2,
          step = 1
        ),
        
        numericInput(
          "ph_k_mu0",
          "Media hipotética μ₀",
          50
        ),
        
        selectInput(
          "ph_k_alt",
          "Tipo de prueba (Ha):",
          c("μ ≠ μ₀","μ > μ₀","μ < μ₀")
        ),
        
        numericInput(
          "ph_k_alpha",
          "Nivel de significancia (%)",
          5,
          min = 0.01,
          max = 99.99
        ),
        
        actionButton(
          "calc_ph",
          "Calcular"
        )
      )
      
    } else {
      
      tagList(
        
        numericInput(
          "ph_d_xbar",
          "Media muestral (X̄)",
          52
        ),
        
        numericInput(
          "ph_d_desv",
          "Desviación estándar muestral (s)",
          10,
          min = 0.0001
        ),
        
        numericInput(
          "ph_d_n",
          "Tamaño de muestra (n)",
          30,
          min = 2,
          step = 1
        ),
        
        numericInput(
          "ph_d_mu0",
          "Media hipotética μ₀",
          50
        ),
        
        selectInput(
          "ph_d_alt",
          "Tipo de prueba (Ha):",
          c("μ ≠ μ₀","μ > μ₀","μ < μ₀")
        ),
        
        numericInput(
          "ph_d_alpha",
          "Nivel de significancia (%)",
          5,
          min = 0.01,
          max = 99.99
        ),
        
        actionButton(
          "calc_ph",
          "Calcular"
        )
      )
    }
  })
  
  
  #==========================================================
  # FORMULAS
  #==========================================================
  
  output$formula_ic <- renderUI({
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      withMathJax(
        HTML(
          "$$IC=\\bar{x}\\pm z_{\\alpha/2}\\frac{\\sigma}{\\sqrt{n}}$$"
        )
      )
      
    } else {
      
      withMathJax(
        HTML(
          "$$IC=\\bar{x}\\pm t_{\\alpha/2,n-1}\\frac{s}{\\sqrt{n}}$$"
        )
      )
    }
  })
  
  
  output$formula_ph <- renderUI({
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      withMathJax(
        HTML(
          "$$Z=\\frac{\\bar{x}-\\mu_0}{\\sigma/\\sqrt{n}}$$"
        )
      )
      
    } else {
      
      withMathJax(
        HTML(
          "$$t=\\frac{\\bar{x}-\\mu_0}{s/\\sqrt{n}}$$"
        )
      )
    }
  })
  
  
  #==========================================================
  # INTERVALO DE CONFIANZA
  #==========================================================
  
  observeEvent(input$calc_ic,{
    
    #--------------------------------------------------------
    # Obtener valores según tipo de varianza
    #--------------------------------------------------------
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      xbar <- input$ic_k_xbar
      desv <- input$ic_k_desv
      n <- input$ic_k_n
      nivel <- input$ic_k_nivel
      
    } else {
      
      xbar <- input$ic_d_xbar
      desv <- input$ic_d_desv
      n <- input$ic_d_n
      nivel <- input$ic_d_nivel
    }
    
    #--------------------------------------------------------
    # Validación
    #--------------------------------------------------------
    
    if(
      is.null(xbar) ||
      is.null(desv) ||
      is.null(n) ||
      is.null(nivel) ||
      !is.finite(xbar) ||
      !is.finite(desv) ||
      !is.finite(n) ||
      !is.finite(nivel) ||
      desv <= 0 ||
      n < 2 ||
      n != floor(n) ||
      nivel <= 0 ||
      nivel >= 100
    ){
      
      showNotification(
        "Revisa los datos ingresados. Todos deben ser válidos.",
        type = "error"
      )
      
      return()
    }
    
    alpha <- 1 - nivel/100
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      valor <- qnorm(1-alpha/2)
      etiqueta <- "z"
      
    } else {
      
      valor <- qt(1-alpha/2,n-1)
      etiqueta <- "t"
    }
    
    se <- desv/sqrt(n)
    error <- valor*se
    
    li <- xbar-error
    ls <- xbar+error
    
    output$res_ic <- renderPrint({
      
      cat("Media muestral (X̄) =",xbar,"\n")
      cat("Tamaño de muestra (n) =",n,"\n")
      cat("Desviación estándar =",desv,"\n\n")
      
      cat("Nivel de confianza =",nivel,"%\n")
      cat("Nivel de significancia α =",round(alpha,4),"\n\n")
      
      cat("Error estándar =",round(se,4),"\n")
      cat("Valor",etiqueta,"=",round(valor,4),"\n")
      cat("Margen de error =",round(error,4),"\n\n")
      
      cat("Intervalo de confianza:\n")
      cat("(",round(li,4),",",round(ls,4),")\n")
    })
    
    x <- seq(
      xbar - 4*se,
      xbar + 4*se,
      length = 1000
    )
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      y <- dnorm(x,xbar,se)
      
    } else {
      
      y <- dt(
        (x-xbar)/se,
        df=n-1
      )/se
    }
    
    datos <- data.frame(x,y)
    sombreado <- subset(datos,x>=li & x<=ls)
    
    p <- ggplot(datos,aes(x,y))+
      
      geom_line(linewidth=1.2)+
      
      geom_area(
        data=sombreado,
        aes(x,y),
        fill="blue",
        alpha=0.35
      )+
      
      geom_vline(
        xintercept = c(li,ls),
        color="blue",
        linewidth=1.2,
        linetype="dashed"
      )+
      
      geom_vline(
        xintercept = xbar,
        color="darkgreen",
        linetype="dashed",
        linewidth=1
      )+
      
      annotate(
        "text",
        x=li,
        y=max(y)*0.92,
        label=paste0("LI = ",round(li,2)),
        color="blue",
        hjust=1.1
      )+
      
      annotate(
        "text",
        x=ls,
        y=max(y)*0.92,
        label=paste0("LS = ",round(ls,2)),
        color="blue",
        hjust=-0.1
      )+
      
      labs(
        title="Intervalo de confianza para la media",
        subtitle=paste(
          "Distribución de la media muestral | α =",
          round(alpha,4)
        ),
        x="Media muestral",
        y="Densidad"
      )+
      
      theme_minimal(base_size = 14)
    
    output$plot_ic <- renderPlotly({
      ggplotly(p)
    })
  })
  
  
  #==========================================================
  # PRUEBA DE HIPÓTESIS
  #==========================================================
  
  observeEvent(input$calc_ph,{
    
    #--------------------------------------------------------
    # Obtener valores según tipo de varianza
    #--------------------------------------------------------
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      ph_xbar <- input$ph_k_xbar
      ph_desv <- input$ph_k_desv
      ph_n <- input$ph_k_n
      ph_mu0 <- input$ph_k_mu0
      ph_alt <- input$ph_k_alt
      ph_alpha <- input$ph_k_alpha
      
    } else {
      
      ph_xbar <- input$ph_d_xbar
      ph_desv <- input$ph_d_desv
      ph_n <- input$ph_d_n
      ph_mu0 <- input$ph_d_mu0
      ph_alt <- input$ph_d_alt
      ph_alpha <- input$ph_d_alpha
    }
    
    #--------------------------------------------------------
    # Validación
    #--------------------------------------------------------
    
    if(
      is.null(ph_xbar) ||
      is.null(ph_desv) ||
      is.null(ph_n) ||
      is.null(ph_mu0) ||
      is.null(ph_alpha) ||
      !is.finite(ph_xbar) ||
      !is.finite(ph_desv) ||
      !is.finite(ph_n) ||
      !is.finite(ph_mu0) ||
      !is.finite(ph_alpha) ||
      ph_desv <= 0 ||
      ph_n < 2 ||
      ph_n != floor(ph_n) ||
      ph_alpha <= 0 ||
      ph_alpha >= 100
    ){
      
      showNotification(
        "Revisa los datos ingresados. Todos deben ser válidos.",
        type = "error"
      )
      
      return()
    }
    
    alpha <- ph_alpha/100
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      estad <- (ph_xbar-ph_mu0)/
        (ph_desv/sqrt(ph_n))
      
      etiqueta <- "Z"
      
      dist_cdf <- pnorm
      dist_quant <- qnorm
      dens <- dnorm
      
    } else {
      
      gl <- ph_n-1
      
      estad <- (ph_xbar-ph_mu0)/
        (ph_desv/sqrt(ph_n))
      
      etiqueta <- "t"
      
      dist_cdf <- function(x) pt(x,gl)
      dist_quant <- function(p) qt(p,gl)
      dens <- function(x) dt(x,gl)
    }
    
    #========================================================
    # CASOS
    #========================================================
    
    if(ph_alt=="μ ≠ μ₀"){
      
      crit1 <- -dist_quant(1-alpha/2)
      crit2 <- dist_quant(1-alpha/2)
      
      pvalue <- 2*(1-dist_cdf(abs(estad)))
      
      region_texto <- paste0(
        "Rechazar H0 si ",
        etiqueta,
        " < ",
        round(crit1,4),
        " o ",
        etiqueta,
        " > ",
        round(crit2,4)
      )
      
    } else if(ph_alt=="μ > μ₀"){
      
      crit1 <- dist_quant(1-alpha)
      crit2 <- NA
      
      pvalue <- 1-dist_cdf(estad)
      
      region_texto <- paste0(
        "Rechazar H0 si ",
        etiqueta,
        " > ",
        round(crit1,4)
      )
      
    } else {
      
      crit1 <- dist_quant(alpha)
      crit2 <- NA
      
      pvalue <- dist_cdf(estad)
      
      region_texto <- paste0(
        "Rechazar H0 si ",
        etiqueta,
        " < ",
        round(crit1,4)
      )
    }
    
    output$res_ph <- renderPrint({
      
      cat("H0: μ =",ph_mu0,"\n")
      cat("Ha:",ph_alt,"\n\n")
      
      if(ph_alt=="μ ≠ μ₀"){
        
        cat(
          "Valores críticos =",
          round(crit1,4),
          "y",
          round(crit2,4),
          "\n"
        )
        
      } else {
        
        cat(
          "Valor crítico =",
          round(crit1,4),
          "\n"
        )
      }
      
      cat("Región de rechazo:\n")
      cat(region_texto,"\n\n")
      
      cat("Estadístico",etiqueta,"=",round(estad,4),"\n")
      cat("p-value =",round(pvalue,4),"\n")
    })
    
    lim <- max(
      abs(c(estad,crit1,crit2)),
      na.rm=TRUE
    )+2
    
    x <- seq(-lim,lim,length=3000)
    y <- dens(x)
    
    datos <- data.frame(x,y)
    
    #========================================================
    # ÁREAS
    #========================================================
    
    if(ph_alt=="μ ≠ μ₀"){
      
      rechazo_izq <- subset(datos,x <= crit1)
      rechazo_der <- subset(datos,x >= crit2)
      
      pvalor_izq <- subset(datos,x <= -abs(estad))
      pvalor_der <- subset(datos,x >= abs(estad))
      
    } else if(ph_alt=="μ > μ₀"){
      
      rechazo_der <- subset(datos,x >= crit1)
      pvalor_der <- subset(datos,x >= estad)
      
    } else {
      
      rechazo_izq <- subset(datos,x <= crit1)
      pvalor_izq <- subset(datos,x <= estad)
    }
    
    #========================================================
    # GRÁFICA
    #========================================================
    
    p <- ggplot(datos,aes(x,y))+
      
      geom_line(linewidth=1.2)+
      
      {
        if(ph_alt=="μ ≠ μ₀"){
          
          list(
            
            geom_area(
              data=rechazo_izq,
              aes(x,y),
              fill="blue",
              alpha=0.35
            ),
            
            geom_area(
              data=rechazo_der,
              aes(x,y),
              fill="blue",
              alpha=0.35
            ),
            
            geom_area(
              data=pvalor_izq,
              aes(x,y),
              fill="red",
              alpha=0.45
            ),
            
            geom_area(
              data=pvalor_der,
              aes(x,y),
              fill="red",
              alpha=0.45
            )
          )
          
        } else if(ph_alt=="μ > μ₀"){
          
          list(
            
            geom_area(
              data=rechazo_der,
              aes(x,y),
              fill="blue",
              alpha=0.35
            ),
            
            geom_area(
              data=pvalor_der,
              aes(x,y),
              fill="red",
              alpha=0.45
            )
          )
          
        } else {
          
          list(
            
            geom_area(
              data=rechazo_izq,
              aes(x,y),
              fill="blue",
              alpha=0.35
            ),
            
            geom_area(
              data=pvalor_izq,
              aes(x,y),
              fill="red",
              alpha=0.45
            )
          )
        }
      }+
      
      geom_vline(
        xintercept = estad,
        color="red",
        linewidth=1.2
      )+
      
      {
        if(ph_alt=="μ ≠ μ₀"){
          
          list(
            
            geom_vline(
              xintercept = c(crit1,crit2),
              color="blue",
              linewidth=1.2,
              linetype="dashed"
            ),
            
            annotate(
              "text",
              x=crit1,
              y=max(y)*0.92,
              label=paste0(
                "Valor crítico = ",
                round(crit1,2)
              ),
              color="blue",
              hjust=1.1
            ),
            
            annotate(
              "text",
              x=crit2,
              y=max(y)*0.92,
              label=paste0(
                "Valor crítico = ",
                round(crit2,2)
              ),
              color="blue",
              hjust=-0.1
            )
          )
          
        } else {
          
          list(
            
            geom_vline(
              xintercept = crit1,
              color="blue",
              linewidth=1.2,
              linetype="dashed"
            ),
            
            annotate(
              "text",
              x=crit1,
              y=max(y)*0.92,
              label=paste0(
                "Valor crítico = ",
                round(crit1,2)
              ),
              color="blue"
            )
          )
        }
      }+
      
      annotate(
        "text",
        x=estad,
        y=max(y)*0.78,
        label=paste0(
          etiqueta,
          " = ",
          round(estad,2)
        ),
        color="red"
      )+
      
      labs(
        title="Prueba de hipótesis",
        subtitle="Azul: Región de rechazo | Rojo: p-value",
        x="Estadístico",
        y="Densidad"
      )+
      
      theme_minimal(base_size = 14)
    
    output$plot_ph <- renderPlotly({
      ggplotly(p)
    })
  })
  
  
  #==========================================================
  # TAMAÑO DE MUESTRA
  #==========================================================
  
  observeEvent(input$calc_tm,{
    
    #--------------------------------------------------------
    # Validación
    #--------------------------------------------------------
    
    if(
      is.null(input$tm_sigma) ||
      is.null(input$tm_error) ||
      is.null(input$tm_nivel) ||
      !is.finite(input$tm_sigma) ||
      !is.finite(input$tm_error) ||
      !is.finite(input$tm_nivel) ||
      input$tm_sigma <= 0 ||
      input$tm_error <= 0 ||
      input$tm_nivel <= 0 ||
      input$tm_nivel >= 100
    ){
      
      showNotification(
        "Revisa los datos ingresados. Todos deben ser válidos.",
        type = "error"
      )
      
      return()
    }
    
    alpha <- 1-input$tm_nivel/100
    
    z <- qnorm(1-alpha/2)
    
    n <- (z*input$tm_sigma/input$tm_error)^2
    
    output$res_tm <- renderPrint({
      
      cat(
        "Nivel de confianza =",
        input$tm_nivel,
        "%\n"
      )
      
      cat(
        "Nivel de significancia α =",
        round(alpha,4),
        "\n\n"
      )
      
      cat(
        "Valor crítico z =",
        round(z,4),
        "\n\n"
      )
      
      cat(
        "Tamaño mínimo de muestra requerido =",
        ceiling(n)
      )
    })
  })
}

# ============================================================
# EJECUTAR APLICACIÓN
# ============================================================

shinyApp(ui,server)
