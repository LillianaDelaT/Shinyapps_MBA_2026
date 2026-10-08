# ============================================================
# LAUNCHER - MBA - Estadística para Negocios
# Instituto Tecnológico Autónomo de México (ITAM)
# ============================================================

library(shiny)
library(bslib)
library(DT)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(janitor)
library(tidyr)

# ============================================================
# INFORMACIÓN DE LAS APLICACIONES
# ============================================================

categorias <- list(
  list(id="dist", icono="📊", nombre="1. Distribuciones de probabilidad", descripcion="Áreas y valores críticos de diferentes distribuciones de probabilidad.", aplicaciones=list(list(id="distribuciones", nombre="Distribuciones de probabilidad: áreas y valores críticos", descripcion="Cálculo de áreas y valores críticos para diferentes distribuciones de probabilidad."))),
  list(id="exploratorio", icono="🔎", nombre="2. Análisis exploratorio", descripcion="Herramientas para explorar y describir conjuntos de datos.", aplicaciones=list(list(id="exploratorio", nombre="Análisis exploratorio de datos", descripcion="Análisis descriptivo y exploratorio de conjuntos de datos."))),
  list(id="medias_ind", icono="📐", nombre="3. Medias (muestras independientes)", descripcion="Inferencia para la media y comparación de medias de muestras independientes.", aplicaciones=list(
    list(id="media", nombre="3.1 Media", descripcion="Inferencia estadística para una media poblacional."),
    list(id="medias_conocidas", nombre="3.2 Diferencia de medias con varianzas poblacionales conocidas", descripcion="Intervalos de confianza y pruebas de hipótesis para la diferencia de medias con varianzas poblacionales conocidas."),
    list(id="medias_desconocidas", nombre="3.3 Diferencia de medias con varianzas poblacionales desconocidas", descripcion="Intervalos de confianza y pruebas de hipótesis para la diferencia de medias con varianzas poblacionales desconocidas."),
    list(id="mann_whitney", nombre="3.4 Prueba no paramétrica - Mann Whitney", descripcion="Comparación no paramétrica de dos muestras independientes mediante la prueba de Mann-Whitney.")
  )),
  list(id="medias_dep", icono="🔗", nombre="4. Medias de muestras dependientes", descripcion="Comparación de medias para muestras dependientes o pareadas.", aplicaciones=list(
    list(id="pareadas", nombre="4.1 Comparación de medias pareadas", descripcion="Intervalos de confianza y pruebas de hipótesis para la diferencia de medias en muestras pareadas."),
    list(id="wilcoxon", nombre="4.2 Prueba Wilcoxon", descripcion="Prueba no paramétrica para comparar dos muestras dependientes o pareadas.")
  )),
  list(id="anova", icono="📈", nombre="5. ANOVA", descripcion="Análisis de varianza para comparación de medias.", aplicaciones=list(list(id="anova", nombre="5.1 ANOVA", descripcion="Análisis de varianza para comparar las medias de tres o más poblaciones."))),
  list(id="proporciones", icono="◉", nombre="6. Proporciones", descripcion="Inferencia para una proporción y comparación de proporciones.", aplicaciones=list(
    list(id="proporcion", nombre="6.1 Proporción", descripcion="Intervalos de confianza y pruebas de hipótesis para una proporción poblacional."),
    list(id="diferencia_proporciones", nombre="6.2 Diferencia de proporciones", descripcion="Inferencia estadística para la diferencia entre dos proporciones poblacionales."),
    list(id="diferencia_proporciones_iguales", nombre="6.3 Diferencia de proporciones iguales", descripcion="Prueba de hipótesis para comparar dos proporciones poblacionales bajo igualdad de proporciones.")
  )),
  list(id="varianzas", icono="σ²", nombre="7. Varianzas", descripcion="Inferencia para varianzas y comparación de dispersiones.", aplicaciones=list(
    list(id="prueba_no_parametrica_var", nombre="7.1 Prueba de hipótesis no paramétrica", descripcion="Prueba no paramétrica para el análisis y comparación de dispersiones."),
    list(id="varianza_cociente", nombre="7.2 Varianza y cociente de varianzas", descripcion="Intervalos de confianza y pruebas de hipótesis para una varianza y para el cociente de dos varianzas.")
  )),
  list(id="regresion_lineal", icono="📉", nombre="8. Modelos de regresión lineal", descripcion="Análisis de relaciones lineales, modelos de regresión y validación de supuestos.", aplicaciones=list(
    list(id="dispersion_correlacion", nombre="8.1 Diagrama de dispersión / coeficientes de correlación", descripcion="Exploración de la relación entre variables mediante diagramas de dispersión y coeficientes de correlación."),
    list(id="regresion_lineal", nombre="8.2 Modelo de regresión lineal", descripcion="Estimación e interpretación de un modelo de regresión lineal simple."),
    list(id="regresion_multiple", nombre="8.3 Modelo de regresión lineal múltiple", descripcion="Estimación e interpretación de modelos de regresión lineal múltiple."),
    list(id="transformaciones_x", nombre="8.4 Transformaciones en X", descripcion="Análisis de modelos de regresión mediante transformaciones de la variable independiente."),
    list(id="validacion_supuestos", nombre="8.5 Validación de supuestos", descripcion="Herramientas para evaluar los supuestos del modelo de regresión.")
  )),
  list(id="regresion_logistica", icono="🔵", nombre="9. Modelos de regresión logística", descripcion="Modelos para explicar y predecir una variable dependiente binaria.", aplicaciones=list(
    list(id="modelo_logistico", nombre="9.1 Modelo de regresión logística", descripcion="Estimación e interpretación de un modelo de regresión logística."),
    list(id="clasificacion", nombre="9.2 Clasificación", descripcion="Clasificación de observaciones mediante modelos de regresión logística.")
  ))
)

tarjeta_categoria <- function(id, categoria) {
  actionLink(inputId=paste0("categoria_",id), label=tagList(
    div(class="tarjeta-titulo", paste(categoria$icono,categoria$nombre)),
    div(class="tarjeta-descripcion", categoria$descripcion),
    div(class="tarjeta-footer", paste(length(categoria$aplicaciones), ifelse(length(categoria$aplicaciones)==1,"herramienta","herramientas")), span("›",class="flecha"))
  ), class="tarjeta-categoria")
}

tarjeta_aplicacion <- function(id, aplicacion) {
  actionLink(inputId=paste0("app_",id), label=tagList(
    div(class="tarjeta-titulo", aplicacion$nombre),
    div(class="tarjeta-descripcion", aplicacion$descripcion),
    div(class="tarjeta-footer", "Abrir herramienta", span("›",class="flecha"))
  ), class="tarjeta-aplicacion")
}

css <- "
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif; color:#172b4d; }
.navbar { min-height:55px; }
.sidebar { background-color:#f4f4f4; border-right:1px solid #dddddd; }
.sidebar-title { font-size:24px; font-weight:500; color:#172b4d; margin-bottom:4px; }
.sidebar-subtitle { font-size:16px; color:#555555; margin-bottom:25px; }
.sidebar-divider { border-top:1px solid #cfcfcf; margin:20px 0; }
.sidebar-category { font-size:14px; font-weight:600; color:#263238; margin-top:18px; margin-bottom:7px; }
.sidebar-app { display:block; font-size:14px; line-height:1.35; margin-bottom:8px; color:#008f8f !important; text-decoration:underline !important; }
.contenido-principal { padding:35px 40px 25px 40px; }
.titulo-principal { font-size:48px; font-weight:400; line-height:1.1; color:#172b4d; margin-bottom:5px; }
.subtitulo-principal { font-size:26px; font-weight:400; color:#172b4d; margin-bottom:25px; }
.texto-bienvenida { font-size:16px; line-height:1.6; color:#172b4d; margin-bottom:25px; }
.tarjeta-categoria,.tarjeta-aplicacion { display:block; width:100%; min-height:170px; margin-bottom:20px; padding:0 !important; text-align:left; background-color:#ffffff !important; border:1px solid #d9dee3 !important; border-radius:7px !important; color:#212529 !important; text-decoration:none !important; box-shadow:0 2px 5px rgba(0,0,0,0.08); white-space:normal !important; overflow:hidden; transition:box-shadow .2s ease,transform .2s ease,border-color .2s ease; }
.tarjeta-categoria:hover,.tarjeta-aplicacion:hover { background-color:#ffffff !important; border-color:#b8c0c8 !important; color:#212529 !important; text-decoration:none !important; box-shadow:0 6px 16px rgba(0,0,0,0.12); transform:translateY(-2px); }
.tarjeta-titulo { display:block; padding:12px 15px; font-size:1rem; font-weight:600; line-height:1.4; color:#212529 !important; background-color:#f5f6f7 !important; border-bottom:1px solid #dfe3e6; text-decoration:none !important; }
.tarjeta-descripcion { display:block; padding:15px; font-size:.92rem; line-height:1.5; color:#343a40 !important; background-color:#ffffff !important; min-height:65px; text-decoration:none !important; }
.tarjeta-footer { display:flex; justify-content:space-between; align-items:center; padding:5px 15px 13px 15px; font-size:.78rem; font-style:italic; color:#6c757d !important; background-color:#ffffff !important; }
.flecha { font-size:1.35rem; font-style:normal; color:#6c757d !important; }
.boton-regresar { margin-bottom:25px; font-size:15px; }
.placeholder-app { margin-top:25px; padding:35px; background-color:#ffffff; border:1px solid #d9dee3; border-radius:7px; box-shadow:0 2px 5px rgba(0,0,0,.08); }
.placeholder-app h3 { color:#172b4d; margin-top:0; }
.placeholder-app p { color:#555555; font-size:16px; }
.footer-app { margin-top:35px; padding-top:15px; border-top:1px solid #dddddd; font-size:12px; color:#777777; text-align:center; }
"
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(bslib)


# ============================================================
# APLICACIÓN 4.1 - COMPARACIÓN DE MEDIAS PAREADAS
# ============================================================

pareadas_ui <- function(ns) {

  fluidPage(

    titlePanel(
      div(
        "Diferencia de medias para muestras pareadas",
        tags$small(
          "App desarrollada por Liliana De la Torre Desentis",
          style = "display:block; font-style:italic; font-size:40%;"
        )
      )
    ),

    sidebarLayout(

      sidebarPanel(
        fileInput(
          ns("archivo"),
          "Sube un archivo Excel (.xlsx):",
          accept = ".xlsx"
        ),
        uiOutput(ns("columnas"))
      ),

      mainPanel(
        tabsetPanel(

          # ================= INTERVALO =================
          tabPanel(
            "Intervalo de confianza",
            withMathJax(),
            helpText(
              "$$\\bar{d} \\pm t_{\\alpha/2}\\frac{s_d}{\\sqrt{n}}$$"
            ),
            numericInput(
              ns("conf_ic"),
              "Nivel de confianza (%)",
              95,
              min = 1,
              max = 99
            ),
            actionButton(
              ns("calcular_ic"),
              "Calcular",
              class = "btn-primary"
            ),
            hr(),
            verbatimTextOutput(ns("resultado_ic")),
            plotOutput(ns("grafica_ic"))
          ),

          # ================= HIPOTESIS =================
          tabPanel(
            "Prueba de hipótesis",
            selectInput(
              ns("hipotesis"),
              "Hipótesis alternativa:",
              c(
                "Diferente" = "two.sided",
                "Mayor" = "greater",
                "Menor" = "less"
              )
            ),
            numericInput(
              ns("conf_ph"),
              "Nivel de confianza (%)",
              95,
              min = 1,
              max = 99
            ),
            actionButton(
              ns("calcular_ph"),
              "Calcular",
              class = "btn-primary"
            ),
            hr(),
            verbatimTextOutput(ns("resultado_ph")),
            plotOutput(ns("grafica_ph"))
          ),

          # ================= TAMAÑO =================
          tabPanel(
            "Tamaño de muestra",
            numericInput(
              ns("alpha"),
              "Nivel de significancia (α)",
              0.05,
              min = 0.001,
              max = 0.5
            ),
            numericInput(
              ns("poder"),
              "Poder estadístico (1-β)",
              0.80,
              min = 0.5,
              max = 0.99
            ),
            numericInput(
              ns("sd"),
              "Desviación estándar de diferencias (s_d)",
              1,
              min = 0.0001
            ),
            numericInput(
              ns("delta"),
              "Diferencia mínima a detectar (δ)",
              0.5
            ),
            actionButton(
              ns("calc_n"),
              "Calcular",
              class = "btn-primary"
            ),
            hr(),
            verbatimTextOutput(ns("res_n"))
          )
        )
      )
    )
  )
}


pareadas_server <- function(input, output, session) {

  ns <- session$ns

  # ================= DATOS =================
  datos <- reactive({
    req(input$archivo)
    read_excel(input$archivo$datapath)
  })

  output$columnas <- renderUI({
    req(datos())
    cols <- names(datos())

    tagList(
      selectInput(
        ns("col1"),
        "Columna 1 (Antes / Muestra 1)",
        cols
      ),
      selectInput(
        ns("col2"),
        "Columna 2 (Después / Muestra 2)",
        cols,
        selected = cols[min(2, length(cols))]
      )
    )
  })

  difs <- reactive({
    df <- datos()
    req(input$col1, input$col2)

    g1 <- df[[input$col1]]
    g2 <- df[[input$col2]]

    if(!is.numeric(g1) || !is.numeric(g2)) {
      return(NULL)
    }

    df_clean <- na.omit(data.frame(g1, g2))

    if(nrow(df_clean) < 2) {
      return(NULL)
    }

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

    list(
      d = d,
      n = n,
      m = m,
      s = s,
      se = se,
      tcrit = tcrit,
      me = me,
      li = m - me,
      ls = m + me,
      conf = input$conf_ic
    )
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

    ggplot(
      data.frame(d = res$d),
      aes(x = d)
    ) +
      geom_histogram(
        bins = max(5, floor(res$n/3)),
        fill = "skyblue",
        color = "black"
      ) +
      geom_vline(
        xintercept = res$m,
        color = "red",
        linetype = "dashed",
        size = 1
      ) +
      geom_vline(
        xintercept = c(res$li, res$ls),
        color = "darkgreen",
        linetype = "dotted",
        size = 1
      ) +
      theme_minimal() +
      labs(
        title = "Distribución de las diferencias y CI",
        subtitle = "Línea roja: Media | Líneas verdes: Límites IC",
        x = "Diferencias (Col 1 - Col 2)",
        y = "Frecuencia"
      )
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

    prueba <- t.test(
      d,
      alternative = input$hipotesis
    )

    list(
      d = d,
      n = n,
      m = m,
      s = s,
      se = se,
      t_obs = t_obs,
      df = df_val,
      alpha = alpha,
      p_value = prueba$p.value,
      hipotesis = input$hipotesis
    )
  })

  output$resultado_ph <- renderPrint({
    res <- res_ph()

    cat("=====================================\n")
    cat(" Prueba t para muestras pareadas\n")
    cat("=====================================\n")
    cat("Hipótesis nula: μ_d = 0\n")
    cat(
      "Hipótesis alternativa:",
      switch(
        res$hipotesis,
        "two.sided" = "μ_d ≠ 0",
        "greater"   = "μ_d > 0",
        "less"      = "μ_d < 0"
      ),
      "\n\n"
    )

    cat("Nivel de significancia (α):", res$alpha, "\n")
    cat("Tamaño de muestra (n):", res$n, "\n")
    cat("Media de diferencias:", round(res$m, 4), "\n")
    cat("Desviación estándar:", round(res$s, 4), "\n")
    cat("Error estándar:", round(res$se, 4), "\n")
    cat("Grados de libertad:", res$df, "\n")
    cat("t observado:", round(res$t_obs, 4), "\n")
    cat(
      "Valor p:",
      formatC(res$p_value, format = "e", digits = 4),
      "\n\n"
    )

    if(res$p_value < res$alpha){
      cat("🟢 Se rechaza H₀ (Hay evidencia estadísticamente significativa)\n")
    } else {
      cat("🔴 No se rechaza H₀ (No hay suficiente evidencia estadística)\n")
    }
  })

  output$grafica_ph <- renderPlot({
    res <- res_ph()

    lim_x <- max(4, abs(res$t_obs) + 0.5)
    x <- seq(-lim_x, lim_x, length = 1000)
    y <- dt(x, res$df)
    df_plot <- data.frame(x, y)

    g <- ggplot(
      df_plot,
      aes(x, y)
    ) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(
        title = "Distribución t nula con región de rechazo",
        subtitle = "Línea azul: t crítico | Línea roja punteada: t observado",
        x = "Valor t",
        y = "Densidad"
      )

    if(res$hipotesis == "two.sided"){
      tcrit <- qt(1 - res$alpha/2, res$df)

      g <- g +
        geom_area(
          data = subset(df_plot, x <= -tcrit),
          fill = "red",
          alpha = 0.4
        ) +
        geom_area(
          data = subset(df_plot, x >= tcrit),
          fill = "red",
          alpha = 0.4
        ) +
        geom_vline(
          xintercept = c(-tcrit, tcrit),
          color = "blue",
          linetype = "solid"
        )

    } else if(res$hipotesis == "greater"){
      tcrit <- qt(1 - res$alpha, res$df)

      g <- g +
        geom_area(
          data = subset(df_plot, x >= tcrit),
          fill = "red",
          alpha = 0.4
        ) +
        geom_vline(
          xintercept = tcrit,
          color = "blue",
          linetype = "solid"
        )

    } else if(res$hipotesis == "less"){
      tcrit <- qt(res$alpha, res$df)

      g <- g +
        geom_area(
          data = subset(df_plot, x <= tcrit),
          fill = "red",
          alpha = 0.4
        ) +
        geom_vline(
          xintercept = tcrit,
          color = "blue",
          linetype = "solid"
        )
    }

    g +
      geom_vline(
        xintercept = res$t_obs,
        color = "red",
        linetype = "dashed",
        size = 1.2
      )
  })

  # ================= TAMAÑO DE MUESTRA =================
  res_tamano <- eventReactive(input$calc_n, {
    req(input$alpha, input$poder, input$sd, input$delta)

    res <- power.t.test(
      delta = input$delta,
      sd = input$sd,
      sig.level = input$alpha,
      power = input$poder,
      type = "paired",
      alternative = "two.sided"
    )

    ceiling(res$n)
  })

  output$res_n <- renderText({
    n_est <- res_tamano()

    paste0(
      "Muestra requerida (pares de datos): ",
      n_est,
      " observaciones."
    )
  })
}


media_ui <- function() {
  fluidPage(
  
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


}

# ============================================================
# 4.2 PRUEBA DE WILCOXON - MÓDULO
# ============================================================

wilcoxon_ui <- function(ns) {
  fluidPage(
    titlePanel(
      div(
        "Prueba de Wilcoxon (muestras dependientes / pareadas)",
        tags$small(
          "App desarrollada por Liliana De la Torre Desentis",
          style = "display:block; font-style:italic; font-size:40%;"
        )
      )
    ),

    sidebarLayout(
      sidebarPanel(
        fileInput(
          ns("archivo"),
          "Sube un archivo Excel (.xlsx)",
          accept = ".xlsx"
        ),
        uiOutput(ns("columnas_globales"))
      ),

      mainPanel(
        tabsetPanel(

          # ================= IC =================
          tabPanel(
            "Intervalo de confianza",
            numericInput(
              ns("conf_ic"),
              "Nivel de confianza (%)",
              95,
              min = 1,
              max = 99
            ),
            actionButton(
              ns("calc_ic"),
              "Calcular IC",
              class = "btn-primary"
            ),
            hr(),
            verbatimTextOutput(ns("res_ic")),
            plotOutput(ns("plot_ic"))
          ),

          # ================= PRUEBA =================
          tabPanel(
            "Prueba de hipótesis",
            selectInput(
              ns("hip"),
              "Hipótesis alternativa:",
              c(
                "Diferente" = "two.sided",
                "Mayor" = "greater",
                "Menor" = "less"
              )
            ),
            numericInput(
              ns("mu0"),
              "Valor bajo H₀ (mediana de diff):",
              0
            ),
            numericInput(
              ns("alpha"),
              "Nivel de significancia α:",
              0.05,
              min = 0.001,
              max = 0.5
            ),
            actionButton(
              ns("calc_ph"),
              "Calcular Prueba",
              class = "btn-primary"
            ),
            hr(),
            verbatimTextOutput(ns("res_ph")),
            plotOutput(ns("plot_ph"))
          ),

          # ================= TAMAÑO =================
          tabPanel(
            "Tamaño de muestra",
            numericInput(
              ns("alpha_tm"),
              "α",
              0.05,
              min = 0.001,
              max = 0.5
            ),
            numericInput(
              ns("power"),
              "Poder (1-β)",
              0.8,
              min = 0.5,
              max = 0.99
            ),
            numericInput(
              ns("sd"),
              "Desv. estándar estimada (s_d)",
              1,
              min = 0.0001
            ),
            numericInput(
              ns("delta"),
              "Efecto detectable (δ)",
              0.5
            ),
            actionButton(
              ns("calc_tm"),
              "Calcular Muestra",
              class = "btn-primary"
            ),
            hr(),
            verbatimTextOutput(ns("res_tm"))
          ),

          # ================= W =================
          tabPanel(
            "¿Cómo se construye W?",
            actionButton(
              ns("calc_w"),
              "Mostrar procedimiento",
              class = "btn-info"
            ),
            hr(),
            verbatimTextOutput(ns("res_w")),
            tableOutput(ns("tabla_w")),
            plotOutput(ns("plot_w"))
          )
        )
      )
    )
  )
}

wilcoxon_server <- function(input, output, session) {

  ns <- session$ns

  # ================= DATOS =================
  datos <- reactive({
    req(input$archivo)
    read_excel(input$archivo$datapath)
  })

  output$columnas_globales <- renderUI({
    req(datos())
    cols <- names(datos())

    tagList(
      selectInput(
        ns("col1"),
        "Variable X (Muestra 1 / Antes)",
        cols
      ),
      selectInput(
        ns("col2"),
        "Variable Y (Muestra 2 / Después)",
        cols,
        selected = cols[min(2, length(cols))]
      )
    )
  })

  # Preparación y limpieza de diferencias pareadas
  difs_clean <- reactive({
    df <- datos()
    req(input$col1, input$col2)

    x <- df[[input$col1]]
    y <- df[[input$col2]]

    if(!is.numeric(x) || !is.numeric(y)) {
      return(NULL)
    }

    df_paired <- na.omit(data.frame(x = x, y = y))

    if(nrow(df_paired) < 2) {
      return(NULL)
    }

    df_paired$d <- df_paired$x - df_paired$y
    df_paired
  })

  # ================= INTERVALO DE CONFIANZA =================
  res_ic_data <- eventReactive(input$calc_ic, {
    df_p <- difs_clean()
    req(df_p)

    prueba <- wilcox.test(
      df_p$x,
      df_p$y,
      paired = TRUE,
      conf.int = TRUE,
      conf.level = input$conf_ic / 100,
      exact = FALSE
    )

    list(
      df_p = df_p,
      prueba = prueba,
      conf = input$conf_ic
    )
  })

  output$res_ic <- renderPrint({
    res <- res_ic_data()

    cat("=====================================\n")
    cat(" Intervalo de Confianza (Wilcoxon)\n")
    cat("=====================================\n")
    cat("Nivel de confianza:", res$conf, "%\n\n")

    cat(
      "Pseudo-mediana estimada:",
      round(res$prueba$estimate, 4),
      "\n"
    )

    cat(
      "Intervalo de Confianza: [",
      round(res$prueba$conf.int[1], 4),
      ",",
      round(res$prueba$conf.int[2], 4),
      "]\n\n"
    )

    cat("Interpretación:\n")
    cat("Estimación del desplazamiento o pseudo-mediana de la diferencia (X - Y).\n")
  })

  output$plot_ic <- renderPlot({
    res <- res_ic_data()

    ggplot(res$df_p, aes(x = d)) +
      geom_histogram(
        fill = "skyblue",
        color = "black",
        bins = max(5, floor(nrow(res$df_p) / 3))
      ) +
      geom_vline(
        xintercept = res$prueba$estimate,
        color = "red",
        linetype = "dashed",
        size = 1
      ) +
      geom_vline(
        xintercept = c(
          res$prueba$conf.int[1],
          res$prueba$conf.int[2]
        ),
        color = "darkgreen",
        linetype = "dotted",
        size = 1
      ) +
      theme_minimal() +
      labs(
        title = "Distribución de Diferencias (X - Y)",
        subtitle = "Línea roja: Pseudo-mediana | Líneas verdes: Límites IC",
        x = "Diferencia (d)",
        y = "Frecuencia"
      )
  })

  # ================= PRUEBA DE HIPÓTESIS =================
  res_ph_data <- eventReactive(input$calc_ph, {
    df_p <- difs_clean()
    req(df_p)

    prueba <- wilcox.test(
      df_p$x,
      df_p$y,
      paired = TRUE,
      alternative = input$hip,
      mu = input$mu0,
      exact = FALSE
    )

    list(
      df_p = df_p,
      prueba = prueba,
      hip = input$hip,
      mu0 = input$mu0,
      alpha = input$alpha
    )
  })

  output$res_ph <- renderPrint({
    res <- res_ph_data()

    cat("=====================================\n")
    cat(" Prueba de Rangos con Signo de Wilcoxon\n")
    cat("=====================================\n\n")

    cat("Hipótesis:\n")
    cat("H₀: Mediana(X - Y) =", res$mu0, "\n")

    switch(
      res$hip,
      "less" = cat("Hₐ: Mediana(X - Y) <", res$mu0, "\n"),
      "greater" = cat("Hₐ: Mediana(X - Y) >", res$mu0, "\n"),
      "two.sided" = cat("Hₐ: Mediana(X - Y) ≠", res$mu0, "\n")
    )

    cat("\nNivel de significancia (α) =", res$alpha, "\n\n")
    cat(
      "Estadístico V (W+ en R):",
      round(res$prueba$statistic, 4),
      "\n"
    )
    cat(
      "Valor p:",
      formatC(res$prueba$p.value, format = "e", digits = 4),
      "\n\n"
    )

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
      geom_histogram(
        fill = "lightgreen",
        color = "black",
        bins = max(5, floor(nrow(res$df_p) / 3))
      ) +
      geom_vline(
        xintercept = res$mu0,
        color = "blue",
        linetype = "dashed",
        size = 1
      ) +
      geom_vline(
        xintercept = median(res$df_p$d),
        color = "darkred",
        linetype = "solid",
        size = 1
      ) +
      theme_minimal() +
      labs(
        title = "Distribución de las diferencias",
        subtitle = "Línea azul: Valor bajo H0 | Línea roja: Mediana muestral",
        x = "Diferencia (X - Y)",
        y = "Frecuencia"
      )
  })

  # ================= TAMAÑO DE MUESTRA =================
  res_tm_data <- eventReactive(input$calc_tm, {
    req(input$alpha_tm, input$power, input$sd, input$delta)

    # Tamaño paramétrico equivalente (Prueba t pareada)
    res_t <- power.t.test(
      delta = input$delta,
      sd = input$sd,
      sig.level = input$alpha_tm,
      power = input$power,
      type = "paired",
      alternative = "two.sided"
    )

    # Corrección no paramétrica (ARE = 0.864)
    n_wilcoxon <- ceiling(res_t$n / 0.864)
    n_t <- ceiling(res_t$n)

    list(
      n_wilcoxon = n_wilcoxon,
      n_t = n_t
    )
  })

  output$res_tm <- renderText({
    res <- res_tm_data()

    paste0(
      "Muestra requerida para Wilcoxon: ",
      res$n_wilcoxon,
      " pares de datos.\n",
      "(Equivalente prueba t: ",
      res$n_t,
      " pares; ajustado por ARE de Lehmann = 0.864)."
    )
  })

  # ================= CONSTRUCCIÓN PASO A PASO DE W =================
  res_w_data <- eventReactive(input$calc_w, {
    df_p <- difs_clean()
    req(df_p)

    tabla <- df_p %>%
      filter(d != 0) %>%
      mutate(
        abs_d = abs(d),
        rango = rank(abs_d),
        signo = ifelse(d > 0, "+", "-"),
        r_sign = rango * ifelse(d > 0, 1, -1)
      )

    Wp <- sum(tabla$rango[tabla$signo == "+"])
    Wm <- sum(tabla$rango[tabla$signo == "-"])
    W_min <- min(Wp, Wm)

    list(
      tabla = tabla,
      Wp = Wp,
      Wm = Wm,
      W_min = W_min
    )
  })

  output$tabla_w <- renderTable({
    res_w_data()$tabla %>%
      select(x, y, d, abs_d, rango, signo, r_sign) %>%
      rename(
        "X" = x,
        "Y" = y,
        "Diferencia (d)" = d,
        "|d|" = abs_d,
        "Rango" = rango,
        "Signo" = signo,
        "Rango c/ Signo" = r_sign
      )
  })

  output$res_w <- renderPrint({
    res <- res_w_data()

    cat("=====================================\n")
    cat(" Desglose del Estadístico de Wilcoxon\n")
    cat("=====================================\n")
    cat(
      "Suma de rangos positivos (W+ / V en R):",
      res$Wp,
      "\n"
    )
    cat("Suma de rangos negativos (W-):", res$Wm, "\n")
    cat("Mínimo(W+, W-):", res$W_min, "\n\n")
    cat(
      "Nota: R reporta 'V' en wilcox.test, el cual equivale a W+ (",
      res$Wp,
      ").\n"
    )
  })

  output$plot_w <- renderPlot({
    res <- res_w_data()

    ggplot(
      res$tabla,
      aes(
        x = factor(1:nrow(res$tabla)),
        y = r_sign,
        fill = signo
      )
    ) +
      geom_col() +
      scale_fill_manual(
        values = c("+" = "lightblue", "-" = "coral")
      ) +
      theme_minimal() +
      labs(
        title = "Rangos Asignados a las Diferencias",
        x = "Observación (Excluidos d = 0)",
        y = "Rango con Signo",
        fill = "Signo"
      )
  })
}

media_server <- function(input, output, session){
  
  #==========================================================
  # PANEL DATOS IC
  #==========================================================
  
  output$panel_datos_ic <- renderUI({
    
    req(input$varianza_tipo)
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      tagList(
        
        numericInput(
          "xbar",
          "Media muestral (X̄)",
          50
        ),
        
        numericInput(
          "desv",
          "Desviación estándar poblacional (σ)",
          10,
          min = 0.0001
        ),
        
        numericInput(
          "n",
          "Tamaño de muestra (n)",
          30,
          min = 2
        ),
        
        numericInput(
          "nivel",
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
          "xbar",
          "Media muestral (X̄)",
          50
        ),
        
        numericInput(
          "desv",
          "Desviación estándar muestral (s)",
          10,
          min = 0.0001
        ),
        
        numericInput(
          "n",
          "Tamaño de muestra (n)",
          30,
          min = 2
        ),
        
        numericInput(
          "nivel",
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
    
    req(input$varianza_tipo)
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      tagList(
        
        numericInput(
          "ph_xbar",
          "Media muestral (X̄)",
          52
        ),
        
        numericInput(
          "ph_desv",
          "Desviación estándar poblacional (σ)",
          10,
          min = 0.0001
        ),
        
        numericInput(
          "ph_n",
          "Tamaño de muestra (n)",
          30,
          min = 2
        ),
        
        numericInput(
          "ph_mu0",
          "Media hipotética μ₀",
          50
        ),
        
        selectInput(
          "ph_alt",
          "Tipo de prueba (Ha):",
          c("μ ≠ μ₀","μ > μ₀","μ < μ₀")
        ),
        
        numericInput(
          "ph_alpha",
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
          "ph_xbar",
          "Media muestral (X̄)",
          52
        ),
        
        numericInput(
          "ph_desv",
          "Desviación estándar muestral (s)",
          10,
          min = 0.0001
        ),
        
        numericInput(
          "ph_n",
          "Tamaño de muestra (n)",
          30,
          min = 2
        ),
        
        numericInput(
          "ph_mu0",
          "Media hipotética μ₀",
          50
        ),
        
        selectInput(
          "ph_alt",
          "Tipo de prueba (Ha):",
          c("μ ≠ μ₀","μ > μ₀","μ < μ₀")
        ),
        
        numericInput(
          "ph_alpha",
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
    
    req(input$varianza_tipo)
    
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
    
    req(input$varianza_tipo)
    
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
    
    alpha <- 1 - input$nivel/100
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      valor <- qnorm(1-alpha/2)
      etiqueta <- "z"
      
    } else {
      
      valor <- qt(1-alpha/2,input$n-1)
      etiqueta <- "t"
    }
    
    se <- input$desv/sqrt(input$n)
    error <- valor*se
    
    li <- input$xbar-error
    ls <- input$xbar+error
    
    output$res_ic <- renderPrint({
      
      cat("Media muestral (X̄) =",input$xbar,"\n")
      cat("Tamaño de muestra (n) =",input$n,"\n")
      cat("Desviación estándar =",input$desv,"\n\n")
      
      cat("Nivel de confianza =",input$nivel,"%\n")
      cat("Nivel de significancia α =",round(alpha,4),"\n\n")
      
      cat("Error estándar =",round(se,4),"\n")
      cat("Valor",etiqueta,"=",round(valor,4),"\n")
      cat("Margen de error =",round(error,4),"\n\n")
      
      cat("Intervalo de confianza:\n")
      cat("(",round(li,4),",",round(ls,4),")\n")
    })
    
    x <- seq(
      input$xbar - 4*se,
      input$xbar + 4*se,
      length = 1000
    )
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      y <- dnorm(x,input$xbar,se)
      
    } else {
      
      y <- dt(
        (x-input$xbar)/se,
        df=input$n-1
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
        xintercept = input$xbar,
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
    
    alpha <- input$ph_alpha/100
    
    if(input$varianza_tipo ==
       "Varianza poblacional conocida"){
      
      estad <- (input$ph_xbar-input$ph_mu0)/
        (input$ph_desv/sqrt(input$ph_n))
      
      etiqueta <- "Z"
      
      dist_cdf <- pnorm
      dist_quant <- qnorm
      dens <- dnorm
      
    } else {
      
      gl <- input$ph_n-1
      
      estad <- (input$ph_xbar-input$ph_mu0)/
        (input$ph_desv/sqrt(input$ph_n))
      
      etiqueta <- "t"
      
      dist_cdf <- function(x) pt(x,gl)
      dist_quant <- function(p) qt(p,gl)
      dens <- function(x) dt(x,gl)
    }
    
    #========================================================
    # CASOS
    #========================================================
    
    if(input$ph_alt=="μ ≠ μ₀"){
      
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
      
    } else if(input$ph_alt=="μ > μ₀"){
      
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
      
      cat("H0: μ =",input$ph_mu0,"\n")
      cat("Ha:",input$ph_alt,"\n\n")
      
      if(input$ph_alt=="μ ≠ μ₀"){
        
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
    
    lim <- max(abs(c(estad,crit1,crit2)),na.rm=TRUE)+2
    
    x <- seq(-lim,lim,length=3000)
    y <- dens(x)
    
    datos <- data.frame(x,y)
    
    #========================================================
    # ÁREAS
    #========================================================
    
    if(input$ph_alt=="μ ≠ μ₀"){
      
      rechazo_izq <- subset(datos,x <= crit1)
      rechazo_der <- subset(datos,x >= crit2)
      
      pvalor_izq <- subset(datos,x <= -abs(estad))
      pvalor_der <- subset(datos,x >= abs(estad))
      
    } else if(input$ph_alt=="μ > μ₀"){
      
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
        if(input$ph_alt=="μ ≠ μ₀"){
          
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
          
        } else if(input$ph_alt=="μ > μ₀"){
          
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
        if(input$ph_alt=="μ ≠ μ₀"){
          
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
    
    alpha <- 1-input$tm_nivel/100
    
    z <- qnorm(1-alpha/2)
    
    n <- (z*input$tm_sigma/input$tm_error)^2
    
    output$res_tm <- renderPrint({
      
      cat("Nivel de confianza =",input$tm_nivel,"%\n")
      cat("Nivel de significancia α =",round(alpha,4),"\n\n")
      
      cat("Valor crítico z =",round(z,4),"\n\n")
      
      cat(
        "Tamaño mínimo de muestra requerido =",
        ceiling(n)
      )
    })
  })
}


# ============================================================
# MÓDULO - DISTRIBUCIONES DE PROBABILIDAD
# Código original integrado como módulo para evitar conflictos de IDs
# ============================================================

distribuciones_ui <- function(ns) {
  div(class = "app-distribuciones", fluidPage(
  
  # Estilos CSS globales para evitar scroll y ajustar tamaño
  tags$head(
    tags$style(
      HTML("
        .app-distribuciones { padding-top: 5px; padding-bottom: 5px; font-size: 13px; }
        .app-distribuciones .well { padding: 10px; margin-bottom: 0px; }
        .app-distribuciones .form-group { margin-bottom: 6px; }
        .app-distribuciones hr { margin-top: 6px; margin-bottom: 6px; }
        .app-distribuciones h3 { margin-top: 0px; margin-bottom: 4px; font-size: 18px; }
        .app-distribuciones h4 { margin-top: 2px; margin-bottom: 4px; font-size: 14px; }
        .app-distribuciones pre { padding: 4px; margin-bottom: 4px; font-size: 12px; }
        .app-distribuciones .formula-box {
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
        ns("distribucion"),
        "Seleccionar distribución",
        choices = c(
          "Normal",
          "t de Student",
          "Chi-cuadrada",
          "F"
        )
      ),
      
      radioButtons(
        ns("calculo"),
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
        ns = ns,
        tags$hr(),
        
        conditionalPanel(
          condition = "input.distribucion == 'Normal'",
        ns = ns,
          numericInput(ns("mean"), "Media (μ):", value = 0),
          numericInput(ns("sd"), "Desviación estándar (σ):", value = 1, min = 0.0001)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 't de Student'",
        ns = ns,
          numericInput(ns("df_prob"), "Grados de libertad (ν):", value = 10, min = 1, step = 1)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 'Chi-cuadrada'",
        ns = ns,
          numericInput(ns("df_prob_chi"), "Grados de libertad (ν):", value = 10, min = 1, step = 1)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 'F'",
        ns = ns,
          numericInput(ns("df1_prob"), "Grados de libertad del numerador (ν₁):", value = 5, min = 1, step = 1),
          numericInput(ns("df2_prob"), "Grados de libertad del denominador (ν₂):", value = 10, min = 1, step = 1)
        ),
        
        selectInput(
          ns("tipo_area"),
          "Tipo de área",
          choices = c(
            "Cola derecha",
            "Cola izquierda",
            "Entre dos valores"
          )
        ),
        
        numericInput(ns("x1"), "Valor de X:", value = 0),
        
        conditionalPanel(
          condition = "input.tipo_area == 'Entre dos valores'",
        ns = ns,
          numericInput(ns("x2"), "Segundo valor de X:", value = 1)
        )
      ),
      
      # VALOR CRÍTICO
      conditionalPanel(
        condition = "input.calculo == 'Valor crítico'",
        ns = ns,
        tags$hr(),
        
        conditionalPanel(
          condition = "input.distribucion == 'Normal'",
        ns = ns,
          numericInput(ns("mean_vc"), "Media (μ):", value = 0),
          numericInput(ns("sd_vc"), "Desviación estándar (σ):", value = 1, min = 0.0001)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 't de Student'",
        ns = ns,
          numericInput(ns("df_vc"), "Grados de libertad (ν):", value = 10, min = 1, step = 1)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 'Chi-cuadrada'",
        ns = ns,
          numericInput(ns("df_vc_chi"), "Grados de libertad (ν):", value = 10, min = 1, step = 1)
        ),
        
        conditionalPanel(
          condition = "input.distribucion == 'F'",
        ns = ns,
          numericInput(ns("df1_vc"), "Grados de libertad del numerador (ν₁):", value = 5, min = 1, step = 1),
          numericInput(ns("df2_vc"), "Grados de libertad del denominador (ν₂):", value = 10, min = 1, step = 1)
        ),
        
        radioButtons(
          ns("cola"),
          "Tipo de cola / prueba",
          choices = c(
            "Cola izquierda",
            "Cola derecha",
            "Dos colas"
          ),
          selected = "Cola derecha"
        ),
        
        numericInput(
          ns("alpha"),
          "Nivel de significancia (α):",
          value = 0.05,
          min = 0.0001,
          max = 0.9999,
          step = 0.01
        )
      ),
      
      actionButton(
        ns("calcular"),
        "Calcular",
        class = "btn-primary",
        style = "width: 100%; margin-top: 5px;"
      )
    ),
    
    mainPanel(
      width = 8,
      uiOutput(ns("info_distribucion")),
      tags$b("Resultado:"),
      verbatimTextOutput(ns("resultado")),
      uiOutput(ns("interpretacion")),
      plotOutput(ns("grafica"), height = "280px")
    )
  )
)  )
}

distribuciones_server <- function(input, output, session) {
# ============================================================
  
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
# MÓDULO - DIFERENCIA DE MEDIAS CON VARIANZAS POBLACIONALES CONOCIDAS
# Código original integrado como módulo para evitar conflictos de IDs
# ============================================================

medias_conocidas_ui <- function(ns) {
  fluidPage(
  tags$head(tags$style(HTML(css))),
  
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
              numericInput(ns("media1_ic"), "X̄₁", value = 50),
              numericInput(ns("sigma1_ic"), "σ₁", value = 10, min = 0.0001),
              numericInput(ns("n1_ic"), "n₁", value = 30, min = 2, step = 1)
            ),
            column(6,
              h5("Grupo 2", style = "color:#003366; font-size:14px;"),
              numericInput(ns("media2_ic"), "X̄₂", value = 45),
              numericInput(ns("sigma2_ic"), "σ₂", value = 12, min = 0.0001),
              numericInput(ns("n2_ic"), "n₂", value = 35, min = 2, step = 1)
            )
          ),
          
          tags$hr(),
          
          fluidRow(
            column(8, numericInput(ns("conf_ic"), "Nivel de confianza (%)", value = 95, min = 80, max = 99.9, step = 0.1)),
            column(4, style = "margin-top: 22px;", actionButton(ns("calcular_ic"), "Calcular", class = "btn-primary w-100"))
          )
        ),
        
        mainPanel(
          width = 7,
          h4("Resultados"),
          verbatimTextOutput(ns("resultado_ic")),
          plotOutput(ns("grafica_ic"), height = "380px")
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
              numericInput(ns("media1_h"), "X̄₁", value = 50),
              numericInput(ns("sigma1_h"), "σ₁", value = 10, min = 0.0001),
              numericInput(ns("n1_h"), "n₁", value = 30, min = 2, step = 1)
            ),
            column(6,
              h5("Grupo 2", style = "color:#003366; font-size:14px;"),
              numericInput(ns("media2_h"), "X̄₂", value = 45),
              numericInput(ns("sigma2_h"), "σ₂", value = 12, min = 0.0001),
              numericInput(ns("n2_h"), "n₂", value = 35, min = 2, step = 1)
            )
          ),
          
          tags$hr(),
          
          fluidRow(
            column(4, numericInput(ns("d0_h"), "Diff (μ₁-μ₂)", value = 0)),
            column(4, selectInput(ns("tipo_h"), "Prueba (Ha)", choices = c("Menor", "Mayor", "Diferente"))),
            column(4, numericInput(ns("nivelconf_h"), "Significancia (%)", value = 5, min = 0.1, max = 20, step = 0.1))
          ),
          
          div(style = "text-align: right; margin-top: 5px;",
            actionButton(ns("calcular_h"), "Calcular", class = "btn-primary w-100")
          )
        ),
        
        mainPanel(
          width = 7,
          h4("Resultados"),
          verbatimTextOutput(ns("resultado_h")),
          plotOutput(ns("grafica_h"), height = "380px")
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
            column(6, numericInput(ns("sigma1_n"), "σ₁", value = 10, min = 0.0001)),
            column(6, numericInput(ns("sigma2_n"), "σ₂", value = 12, min = 0.0001))
          ),
          
          numericInput(ns("d_n"), "Diferencia mínima detectable (d)", value = 5, min = 0.0001),
          
          fluidRow(
            column(6, numericInput(ns("conf_n"), "Confianza (%)", value = 95, min = 80, max = 99.9, step = 0.1)),
            column(6, numericInput(ns("power_n"), "Potencia (%)", value = 80, min = 50, max = 99.9, step = 0.1))
          ),
          
          br(),
          actionButton(ns("calcular_n"), "Calcular", class = "btn-primary w-100")
        ),
        
        mainPanel(
          width = 7,
          h4("Resultados"),
          verbatimTextOutput(ns("resultado_n"))
        )
      )
    )
  )
)
}

medias_conocidas_server <- function(input, output, session){
  
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

# ============================================================
# MÓDULO - ANÁLISIS EXPLORATORIO
# ============================================================
# Se conserva la lógica original de la aplicación.

library(readxl)
library(DT)
library(tidyr)
library(stringr)
library(corrplot)

exploratorio_ui <- function() {
  fluidPage(
  
  titlePanel("Laboratorio de Análisis Exploratorio de Datos"),
  
  tags$div(
    style="font-size: small; font-style: italic;",
    "App desarrollada por Liliana De la Torre Desentis"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput("file","Sube archivo Excel",accept=".xlsx"),
      
      sliderInput("bins","Número de clases o categorías",10,100,30),
      
      actionButton("calcular","Analizar datos")
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Estadísticas Numéricas",
                 DTOutput("tabla_resumen")),
        
        tabPanel("Histogramas",
                 plotOutput("histograma",height="700px")),
        
        tabPanel("Variables Cualitativas",
                 selectInput("var_cualitativa","Selecciona variable cualitativa",choices=NULL),
                 DTOutput("tabla_cualitativa")),
        
        tabPanel("Gráfica de Barras",
                 selectInput("var_barras","Variable cualitativa",choices=NULL),
                 plotOutput("barras")),
        
        tabPanel("Gráfica de Pastel",
                 selectInput("var_pastel","Variable cualitativa",choices=NULL),
                 plotOutput("pastel")),
        
        tabPanel("Boxplots",
                 plotOutput("boxplots")),
        
        tabPanel("Mapa de Correlaciones",
                 plotOutput("correlaciones")),
        
        tabPanel("Outliers Detectados",
                 DTOutput("tabla_outliers")),
        
        tabPanel("Variables Problemáticas",
                 DTOutput("tabla_problemas")),
        
        tabPanel("Resumen",
                 DTOutput("resumen"))
        
      )
    )
  )
)
}


# ===============================
# SERVER
# ===============================

exploratorio_server <- function(input, output, session){
  
  # ------------------------------
  # Cargar datos
  # ------------------------------
  
  datos <- eventReactive(input$calcular,{
    
    req(input$file)
    
    df <- read_excel(input$file$datapath)
    
    df <- df %>%
      mutate(across(everything(), ~type.convert(.x, as.is = TRUE)))
    
    df
  })
  
  # ------------------------------
  # Clasificación (CORREGIDA)
  # ------------------------------
  
  clasificacion <- reactive({
    
    df <- datos()
    
    numericas <- df %>% select(where(is.numeric))
    
    cualitativas <- df %>%
      select(where(function(x){
        is.character(x) || is.factor(x) ||
          (is.numeric(x) && dplyr::n_distinct(x) <= 10)
      }))
    
    cualitativas <- cualitativas %>%
      mutate(across(everything(), as.factor))
    
    list(
      numericas = numericas,
      cualitativas = cualitativas
    )
  })
  
  # ------------------------------
  # NUMÉRICAS LIMPIAS
  # ------------------------------
  
  numericas_limpias <- reactive({
    
    df <- clasificacion()$numericas
    
    validate(need(ncol(df) > 0, "No hay variables numéricas"))
    
    df %>%
      select(where(~sum(!is.na(.)) > 5)) %>%
      select(where(~var(., na.rm=TRUE) > 0))
  })
  
  # ------------------------------
  # Selectores
  # ------------------------------
  
  observe({
    
    req(clasificacion())
    
    vars <- names(clasificacion()$cualitativas)
    
    if(length(vars) > 0){
      updateSelectInput(session,"var_cualitativa",choices=vars, selected=vars[1])
      updateSelectInput(session,"var_barras",choices=vars, selected=vars[1])
      updateSelectInput(session,"var_pastel",choices=vars, selected=vars[1])
    }
    
  })
  
  # ------------------------------
  # Estadísticas
  # ------------------------------
  
  output$tabla_resumen <- renderDT({
    
    df <- numericas_limpias()
    
    resumen <- df %>%
      summarise(across(everything(), list(
        n = ~sum(!is.na(.)),
        mean = ~mean(.,na.rm=TRUE),
        sd = ~sd(.,na.rm=TRUE),
        var = ~var(.,na.rm=TRUE),
        cv = ~ifelse(abs(mean(., na.rm=TRUE)) < 1e-8, NA,
                     sd(.,na.rm=TRUE)/mean(.,na.rm=TRUE)),
        median = ~median(.,na.rm=TRUE),
        Q1 = ~quantile(.,0.25,na.rm=TRUE),
        Q3 = ~quantile(.,0.75,na.rm=TRUE),
        min = ~min(.,na.rm=TRUE),
        max = ~max(.,na.rm=TRUE)
      ), .names = "{.col}_{.fn}")) %>%
      pivot_longer(everything(),
                   names_to="Variable_Estadistico",
                   values_to="Valor") %>%
      separate(Variable_Estadistico,
               into=c("Variable","Estadistico"),
               sep="_(?=[^_]+$)") %>%
      pivot_wider(names_from="Estadistico",
                  values_from="Valor")
    
    datatable(resumen)
  })
  
  # ------------------------------
  # Histogramas
  # ------------------------------
  
  output$histograma <- renderPlot({
    
    df <- numericas_limpias()
    
    df_long <- df %>%
      pivot_longer(everything(),
                   names_to="Variable",
                   values_to="Valor") %>%
      filter(is.finite(Valor))
    
    ggplot(df_long,aes(x=Valor))+
      geom_histogram(aes(y=after_stat(density)),
                     bins=input$bins,
                     fill="skyblue",
                     color="black")+
      geom_density(color="red",linewidth=1)+
      facet_wrap(~Variable,scales="free")+
      theme_minimal()
  })
  
  # ------------------------------
  # Tabla cualitativa
  # ------------------------------
  
  output$tabla_cualitativa <- renderDT({
    
    req(input$var_cualitativa)
    
    df <- clasificacion()$cualitativas
    
    tabla <- df %>%
      count(.data[[input$var_cualitativa]]) %>%
      rename(Categoria = 1, Frecuencia = n) %>%
      mutate(
        Proporcion = Frecuencia/sum(Frecuencia),
        Porcentaje = round(Proporcion*100,2)
      )
    
    datatable(tabla)
  })
  
  # ------------------------------
  # Barras
  # ------------------------------
  
  output$barras <- renderPlot({
    
    req(input$var_barras)
    
    df <- clasificacion()$cualitativas
    
    ggplot(df,aes(x=.data[[input$var_barras]]))+
      geom_bar(fill="tomato")+
      theme_minimal()+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  # ------------------------------
  # Pastel
  # ------------------------------
  
  output$pastel <- renderPlot({
    
    req(input$var_pastel)
    
    df <- clasificacion()$cualitativas
    
    tabla <- df %>%
      count(.data[[input$var_pastel]]) %>%
      rename(Categoria = 1, Frecuencia = n) %>%
      mutate(
        Prop = Frecuencia/sum(Frecuencia),
        label = paste0(Categoria," (",round(Prop*100,1),"%)")
      )
    
    ggplot(tabla,aes(x="",y=Frecuencia,fill=label))+
      geom_bar(stat="identity",width=1)+
      coord_polar("y")+
      theme_void()
  })
  
  # ------------------------------
  # Boxplots
  # ------------------------------
  
  output$boxplots <- renderPlot({
    
    df <- numericas_limpias()
    
    df_long <- df %>%
      pivot_longer(everything(),
                   names_to="Variable",
                   values_to="Valor")
    
    ggplot(df_long,aes(x=Variable,y=Valor))+
      geom_boxplot(fill="lightblue",outlier.colour="red")+
      theme_minimal()+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  # ------------------------------
  # Correlaciones
  # ------------------------------
  
  output$correlaciones <- renderPlot({
    
    df <- numericas_limpias()
    
    validate(need(ncol(df)>1,"Se necesitan al menos dos variables"))
    
    cor_matrix <- cor(df, use="pairwise.complete.obs")
    
    corrplot(cor_matrix,method="color",type="upper",
             addCoef.col="black",diag=FALSE)
  })
  
  # ------------------------------
  # Outliers
  # ------------------------------
  
  output$tabla_outliers <- renderDT({
    
    df <- numericas_limpias()
    
    outliers_list <- list()
    
    for(v in names(df)){
      
      x <- df[[v]]
      x_clean <- x[is.finite(x)]
      
      if(length(x_clean) < 5) next
      
      Q1 <- quantile(x_clean,0.25)
      Q3 <- quantile(x_clean,0.75)
      IQR <- Q3-Q1
      
      lim_inf <- Q1-1.5*IQR
      lim_sup <- Q3+1.5*IQR
      
      pos <- which(x < lim_inf | x > lim_sup)
      
      if(length(pos)>0){
        outliers_list[[v]] <- data.frame(
          Variable=v,
          Observacion=pos,
          Valor=x[pos]
        )
      }
    }
    
    datatable(bind_rows(outliers_list))
  })
  
  # ------------------------------
  # Variables problemáticas
  # ------------------------------
  
  output$tabla_problemas <- renderDT({
    
    df <- datos()
    
    problemas <- lapply(names(df), function(v){
      
      x <- df[[v]]
      
      na_pct <- mean(is.na(x))*100
      
      varianza <- if(is.numeric(x) && sum(!is.na(x))>1){
        var(x,na.rm=TRUE)
      } else NA
      
      problema <- case_when(
        na_pct > 30 ~ "Muchos NA",
        is.numeric(x) & !is.na(varianza) & varianza == 0 ~ "Varianza cero",
        TRUE ~ "OK"
      )
      
      data.frame(
        Variable=v,
        NA_Porcentaje=round(na_pct,2),
        Varianza=varianza,
        Problema=problema
      )
    })
    
    datatable(bind_rows(problemas))
  })
  
  # ------------------------------
  # Resumen
  # ------------------------------
  
  output$resumen <- renderDT({
    
    df <- datos()
    
    resumen <- data.frame(
      Variable = names(df),
      Tipo = sapply(df, class),
      NA_count = sapply(df, function(x) sum(is.na(x))),
      Unicos = sapply(df, dplyr::n_distinct)
    )
    
    datatable(resumen)
  })
  
}




# ============================================================
# MÓDULO - DIFERENCIA DE MEDIAS CON VARIANZAS POBLACIONALES DESCONOCIDAS
# Conserva la aplicación original: varianzas iguales y Welch
# ============================================================

medias_desconocidas_ui <- function(ns) {
  fluidPage(
    tags$head(
      tags$style(HTML("\n        .well { padding: 10px; margin-bottom: 10px; }\n        .MathJax { font-size: 85% !important; }\n        body { zoom: 0.95; }\n      "))
    ),
    titlePanel(
      div(
        h2("Diferencia de medias de muestras independientes", align = "center"),
        h4("Varianzas poblacionales desconocidas", align = "center", style = "color:#555555;"),
        tags$div(
          HTML("<i style='color:#555555; font-size:75%;'>App desarrollada por Liliana De la Torre Desentis</i>"),
          align = "center"
        )
      )
    ),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        radioButtons(
          inputId = ns("varianzas"),
          label = strong("Supuesto sobre varianzas:"),
          choices = c("Varianzas iguales", "Varianzas diferentes (Welch)"),
          selected = "Varianzas iguales"
        )
      ),
      mainPanel(
        width = 10,
        uiOutput(ns("tabs_ui"))
      )
    )
  )
}

medias_desconocidas_server <- function(input, output, session) {

  ns <- session$ns

  # =====================================================
  # IC - VARIANZAS IGUALES
  # =====================================================

  resultados_ic_eq <- reactive({

    xbar1 <- input$xbar1_ic_eq
    xbar2 <- input$xbar2_ic_eq

    s1 <- input$s1_ic_eq
    s2 <- input$s2_ic_eq

    n1 <- input$n1_ic_eq
    n2 <- input$n2_ic_eq

    conf <- input$conf_ic_eq / 100

    alpha <- 1 - conf

    diff_medias <- xbar1 - xbar2

    sp <- sqrt(
      (
        ((n1 - 1) * s1^2) +
          ((n2 - 1) * s2^2)
      ) /
        (n1 + n2 - 2)
    )

    error_estandar <- sp * sqrt((1/n1) + (1/n2))

    gl <- n1 + n2 - 2

    t_crit <- qt(1 - alpha/2, df = gl)

    margen_error <- t_crit * error_estandar

    li <- diff_medias - margen_error
    ls <- diff_medias + margen_error

    list(
      diff_medias = diff_medias,
      t_crit = t_crit,
      sp = sp,
      error_estandar = error_estandar,
      margen_error = margen_error,
      li = li,
      ls = ls,
      conf = conf
    )

  })

  # =====================================================
  # IC - WELCH
  # =====================================================

  resultados_ic_w <- reactive({

    xbar1 <- input$xbar1_ic_w
    xbar2 <- input$xbar2_ic_w

    s1 <- input$s1_ic_w
    s2 <- input$s2_ic_w

    n1 <- input$n1_ic_w
    n2 <- input$n2_ic_w

    conf <- input$conf_ic_w / 100

    alpha <- 1 - conf

    diff_medias <- xbar1 - xbar2

    error_estandar <- sqrt(
      (s1^2 / n1) +
        (s2^2 / n2)
    )

    gl <- (
      ((s1^2 / n1) + (s2^2 / n2))^2
    ) / (
      (
        ((s1^2 / n1)^2) / (n1 - 1)
      ) +
        (
          ((s2^2 / n2)^2) / (n2 - 1)
        )
    )

    t_crit <- qt(1 - alpha/2, df = gl)

    margen_error <- t_crit * error_estandar

    li <- diff_medias - margen_error
    ls <- diff_medias + margen_error

    list(
      diff_medias = diff_medias,
      t_crit = t_crit,
      gl = gl,
      error_estandar = error_estandar,
      margen_error = margen_error,
      li = li,
      ls = ls,
      conf = conf
    )

  })

  # =====================================================
  # PRUEBA HIPOTESIS IGUALES
  # =====================================================

  resultados_ph_eq <- reactive({

    xbar1 <- input$xbar1_ph_eq
    xbar2 <- input$xbar2_ph_eq
    s1 <- input$s1_ph_eq
    s2 <- input$s2_ph_eq
    n1 <- input$n1_ph_eq
    n2 <- input$n2_ph_eq

    mu0 <- input$mu0_ph_eq

    alpha <- input$alpha_ph_eq

    tipo <- input$tipo_ph_eq

    diff_medias <- xbar1 - xbar2

    sp <- sqrt(
      (((n1 - 1) * s1^2) +
         ((n2 - 1) * s2^2)) /
        (n1 + n2 - 2)
    )

    error_estandar <- sp * sqrt((1/n1) + (1/n2))

    gl <- n1 + n2 - 2

    t_calc <- (diff_medias - mu0) / error_estandar

    if(tipo == "Bilateral"){

      t_crit <- qt(1 - alpha/2, df = gl)

      pvalor <- 2 * (1 - pt(abs(t_calc), df = gl))

      decision <- ifelse(
        abs(t_calc) > t_crit,
        "Se rechaza H0",
        "No se rechaza H0"
      )

    } else if(tipo == "Cola derecha"){

      t_crit <- qt(1 - alpha, df = gl)

      pvalor <- 1 - pt(t_calc, df = gl)

      decision <- ifelse(
        t_calc > t_crit,
        "Se rechaza H0",
        "No se rechaza H0"
      )

    } else {

      t_crit <- qt(alpha, df = gl)

      pvalor <- pt(t_calc, df = gl)

      decision <- ifelse(
        t_calc < t_crit,
        "Se rechaza H0",
        "No se rechaza H0"
      )

    }

    list(
      diff_medias = diff_medias,
      sp = sp,
      error_estandar = error_estandar,
      gl = gl,
      t_calc = t_calc,
      t_crit = t_crit,
      pvalor = pvalor,
      decision = decision
    )

  })

  # =====================================================
  # PRUEBA HIPOTESIS WELCH
  # =====================================================

  resultados_ph_w <- reactive({

    xbar1 <- input$xbar1_ph_w
    xbar2 <- input$xbar2_ph_w
    s1 <- input$s1_ph_w
    s2 <- input$s2_ph_w
    n1 <- input$n1_ph_w
    n2 <- input$n2_ph_w

    mu0 <- input$mu0_ph_w

    alpha <- input$alpha_ph_w

    tipo <- input$tipo_ph_w

    diff_medias <- xbar1 - xbar2

    error_estandar <- sqrt(
      (s1^2 / n1) +
        (s2^2 / n2)
    )

    gl <- (
      ((s1^2 / n1) + (s2^2 / n2))^2
    ) / (
      (((s1^2 / n1)^2) / (n1 - 1)) +
        (((s2^2 / n2)^2) / (n2 - 1))
    )

    t_calc <- (diff_medias - mu0) / error_estandar

    if(tipo == "Bilateral"){

      t_crit <- qt(1 - alpha/2, df = gl)

      pvalor <- 2 * (1 - pt(abs(t_calc), df = gl))

      decision <- ifelse(
        abs(t_calc) > t_crit,
        "Se rechaza H0",
        "No se rechaza H0"
      )

    } else if(tipo == "Cola derecha"){

      t_crit <- qt(1 - alpha, df = gl)

      pvalor <- 1 - pt(t_calc, df = gl)

      decision <- ifelse(
        t_calc > t_crit,
        "Se rechaza H0",
        "No se rechaza H0"
      )

    } else {

      t_crit <- qt(alpha, df = gl)

      pvalor <- pt(t_calc, df = gl)

      decision <- ifelse(
        t_calc < t_crit,
        "Se rechaza H0",
        "No se rechaza H0"
      )

    }

    list(
      diff_medias = diff_medias,
      error_estandar = error_estandar,
      gl = gl,
      t_calc = t_calc,
      t_crit = t_crit,
      pvalor = pvalor,
      decision = decision
    )

  })

  # =====================================================
  # TAMAÑO DE MUESTRA
  # =====================================================

  resultados_tm <- reactive({

    sigma <- input$sigma_tm

    E <- input$error_tm

    conf <- input$conf_tm / 100

    alpha <- 1 - conf

    z <- qnorm(1 - alpha/2)

    n <- (z * sigma / E)^2

    n_final <- ceiling(n)

    list(
      sigma = sigma,
      E = E,
      conf = conf,
      z = z,
      n = n,
      n_final = n_final
    )

  })

# =====================================================
  # RESULTADOS IC IGUALES
  # =====================================================

  output$resultados_ic_eq <- renderUI({

    r <- resultados_ic_eq()

    HTML(
      paste0(

        "<b>Diferencia de medias muestrales:</b> ",
        round(r$diff_medias,4),
        "<br><br>",

        "<b>Valor crítico tα/2:</b> ",
        round(r$t_crit,4),
        "<br><br>",

        "<b>Sp:</b> ",
        round(r$sp,4),
        "<br><br>",

        "<b>Error estándar:</b> ",
        round(r$error_estandar,4),
        "<br><br>",

        "<b>Margen de error:</b> ",
        round(r$margen_error,4),
        "<br><br>",

        "<b>Intervalo de confianza:</b> (",
        round(r$li,4),
        ", ",
        round(r$ls,4),
        ")"

      )
    )

  })

  # =====================================================
  # RESULTADOS IC WELCH
  # =====================================================

  output$resultados_ic_w <- renderUI({

    r <- resultados_ic_w()

    HTML(
      paste0(

        "<b>Diferencia de medias muestrales:</b> ",
        round(r$diff_medias,4),
        "<br><br>",

        "<b>Valor crítico tα/2:</b> ",
        round(r$t_crit,4),
        "<br><br>",

        "<b>Grados de libertad:</b> ",
        round(r$gl,4),
        "<br><br>",

        "<b>Error estándar:</b> ",
        round(r$error_estandar,4),
        "<br><br>",

        "<b>Margen de error:</b> ",
        round(r$margen_error,4),
        "<br><br>",

        "<b>Intervalo de confianza:</b> (",
        round(r$li,4),
        ", ",
        round(r$ls,4),
        ")"

      )
    )

  })

  # =====================================================
  # RESULTADOS PH IGUALES
  # =====================================================

  output$resultados_ph_eq <- renderUI({

    r <- resultados_ph_eq()

    HTML(
      paste0(

        "<b>Diferencia de medias:</b> ",
        round(r$diff_medias,4),
        "<br><br>",

        "<b>Sp:</b> ",
        round(r$sp,4),
        "<br><br>",

        "<b>Error estándar:</b> ",
        round(r$error_estandar,4),
        "<br><br>",

        "<b>t calculada:</b> ",
        round(r$t_calc,4),
        "<br><br>",

        "<b>t crítica:</b> ",
        round(r$t_crit,4),
        "<br><br>",

        "<b>p-value:</b> ",
        round(r$pvalor,4),
        "<br><br>",

        "<b>Decisión:</b> ",
        r$decision

      )
    )

  })

  # =====================================================
  # RESULTADOS PH WELCH
  # =====================================================

  output$resultados_ph_w <- renderUI({

    r <- resultados_ph_w()

    HTML(
      paste0(

        "<b>Diferencia de medias:</b> ",
        round(r$diff_medias,4),
        "<br><br>",

        "<b>Error estándar:</b> ",
        round(r$error_estandar,4),
        "<br><br>",

        "<b>Grados de libertad:</b> ",
        round(r$gl,4),
        "<br><br>",

        "<b>t calculada:</b> ",
        round(r$t_calc,4),
        "<br><br>",

        "<b>t crítica:</b> ",
        round(r$t_crit,4),
        "<br><br>",

        "<b>p-value:</b> ",
        round(r$pvalor,4),
        "<br><br>",

        "<b>Decisión:</b> ",
        r$decision

      )
    )

  })

  # =====================================================
  # FUNCION GRAFICA IC
  # =====================================================

  grafica_ic <- function(r, titulo){

    rango_extra <- r$margen_error * 2

    x <- seq(
      r$li - rango_extra,
      r$ls + rango_extra,
      length.out = 2000
    )

    y <- dnorm(
      x,
      mean = r$diff_medias,
      sd = r$error_estandar
    )

    datos <- data.frame(x, y)

    ymax <- max(y)

    ggplot(datos, aes(x, y)) +

      geom_line(linewidth = 1.2) +

      geom_area(
        data = subset(datos, x >= r$li & x <= r$ls),
        fill = "blue",
        alpha = 0.35
      ) +

      geom_vline(
        xintercept = r$li,
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +

      geom_vline(
        xintercept = r$ls,
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +

      geom_vline(
        xintercept = r$diff_medias,
        color = "black",
        linewidth = 1
      ) +

      annotate(
        "text",
        x = r$li,
        y = ymax * 0.92,
        label = paste0("LI = ", round(r$li,4)),
        color = "red",
        hjust = 1.1,
        size = 4
      ) +

      annotate(
        "text",
        x = r$diff_medias,
        y = ymax * 1.02,
        label = paste0("Centro = ", round(r$diff_medias,4)),
        color = "black",
        size = 4
      ) +

      annotate(
        "text",
        x = r$ls,
        y = ymax * 0.92,
        label = paste0("LS = ", round(r$ls,4)),
        color = "red",
        hjust = -0.1,
        size = 4
      ) +

      labs(
        title = titulo,
        x = "Diferencia de medias",
        y = "Densidad"
      ) +

      theme_minimal(base_size = 12)

  }

# =====================================================
  # GRAFICA PH
  # =====================================================

  
grafica_ph <- function(r){

    x <- seq(-5, 5, length.out = 2000)

    y <- dt(x, df = r$gl)

    datos <- data.frame(x, y)

    ymax <- max(y)

    ggplot(datos, aes(x, y)) +

      geom_line(linewidth = 1.2) +

      # =========================================
      # ÁREA ALFA
      # =========================================

      geom_area(
        data = subset(datos, x >= r$t_crit),
        fill = "red",
        alpha = 0.35
      ) +

      # =========================================
      # ÁREA P-VALUE
      # =========================================

      geom_area(
        data = subset(datos, x >= r$t_calc),
        fill = "blue",
        alpha = 0.35
      ) +

      geom_vline(
        xintercept = r$t_calc,
        color = "blue",
        linewidth = 1.2
      ) +

      geom_vline(
        xintercept = r$t_crit,
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +

      # =========================================
      # ETIQUETA ALFA
      # =========================================

      annotate(
        "text",
        x = r$t_crit,
        y = ymax * 0.95,
        label = paste0("α"),
        color = "red",
        hjust = -0.2,
        size = 5
      ) +

      # =========================================
      # ETIQUETA P-VALUE
      # =========================================

      annotate(
        "text",
        x = r$t_calc,
        y = ymax * 0.75,
        label = "p-value",
        color = "blue",
        hjust = 1.1,
        size = 5
      ) +

      labs(
        title = "Prueba de hipótesis",
        x = "t",
        y = "Densidad"
      ) +

      theme_minimal(base_size = 12)

}


  # =====================================================
  # GRAFICAS IC
  # =====================================================

  output$grafica_ic_eq <- renderPlot({

    r <- resultados_ic_eq()

    grafica_ic(
      r,
      paste0(round(r$conf*100,1),
             "% Intervalo de confianza")
    )

  })

  output$grafica_ic_w <- renderPlot({

    r <- resultados_ic_w()

    grafica_ic(
      r,
      paste0(round(r$conf*100,1),
             "% Intervalo de confianza (Welch)")
    )

  })

  # =====================================================
  # GRAFICAS PH
  # =====================================================

  output$grafica_ph_eq <- renderPlot({
    grafica_ph(resultados_ph_eq())
  })

  output$grafica_ph_w <- renderPlot({
    grafica_ph(resultados_ph_w())
  })

  # =====================================================
  # RESULTADOS TAMAÑO DE MUESTRA
  # =====================================================

  output$resultados_tm <- renderUI({

    r <- resultados_tm()

    HTML(
      paste0(

        "<b>Nivel de confianza:</b> ",
        round(r$conf*100,2),
        "%<br><br>",

        "<b>Valor crítico Z:</b> ",
        round(r$z,4),
        "<br><br>",

        "<b>Error máximo permitido:</b> ",
        round(r$E,4),
        "<br><br>",

        "<b>Tamaño de muestra calculado:</b> ",
        round(r$n,4),
        "<br><br>",

        "<b>Tamaño de muestra final:</b> ",
        r$n_final

      )
    )

  })

  # =====================================================
  # GRAFICA TAMAÑO DE MUESTRA
  # =====================================================

  grafica_tm <- function(r){

    x <- seq(-4, 4, length.out = 2000)

    y <- dnorm(x)

    datos <- data.frame(x, y)

    zcrit <- r$z

    ggplot(datos, aes(x, y)) +

      geom_line(linewidth = 1.2) +

      geom_area(
        data = subset(datos, x >= -zcrit & x <= zcrit),
        fill = "blue",
        alpha = 0.35
      ) +

      geom_vline(
        xintercept = c(-zcrit, zcrit),
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +

      annotate(
        "text",
        x = -zcrit,
        y = 0.35,
        label = paste0("-Z = ", round(zcrit,2)),
        color = "red",
        hjust = 1.2,
        size = 4
      ) +

      annotate(
        "text",
        x = zcrit,
        y = 0.35,
        label = paste0("Z = ", round(zcrit,2)),
        color = "red",
        hjust = -0.2,
        size = 4
      ) +

      labs(
        title = "Distribución normal estándar",
        x = "Z",
        y = "Densidad"
      ) +

      theme_minimal(base_size = 12)

  }

  output$grafica_tm <- renderPlot({

    grafica_tm(resultados_tm())

  })

# =====================================================
  # UI DINAMICA
  # =====================================================

  output$tabs_ui <- renderUI({

    if (input$varianzas == "Varianzas iguales") {

      tabsetPanel(

        # =================================================
        # IC VARIANZAS IGUALES
        # =================================================
        tabPanel(
          "Intervalo de confianza",

          br(),

          fluidRow(

            column(
              6,

              wellPanel(

                h3("Fórmulas"),

                withMathJax(
                  helpText("Estadístico:"),

                  "$$
                  (\\bar{X}_1-\\bar{X}_2)
                  \\pm
                  t_{\\alpha/2}
                  S_p
                  \\sqrt{
                  \\frac{1}{n_1}+
                  \\frac{1}{n_2}
                  }
                  $$
                  "
                ),

                br(),

                withMathJax(
                  helpText("Sp:"),

                  "$$
                  S_p=
                  \\sqrt{
                  \\frac{
                  (n_1-1)s_1^2+
                  (n_2-1)s_2^2
                  }{
                  n_1+n_2-2
                  }
                  }
                  $$
                  "
                )

              )

            ),

            column(
              6,

              wellPanel(

                h3("Datos"),

                fluidRow(

                  column(
                    6,

                    h4("Variable X1"),

                    numericInput(
                      ns("xbar1_ic_eq"),
                      "Media muestral (X̄₁)",
                      value = 50
                    ),

                    numericInput(
                      ns("s1_ic_eq"),
                      "Desviación estándar (s₁)",
                      value = 10
                    ),

                    numericInput(
                      ns("n1_ic_eq"),
                      "Tamaño de muestra (n₁)",
                      value = 30
                    )

                  ),

                  column(
                    6,

                    h4("Variable X2"),

                    numericInput(
                      ns("xbar2_ic_eq"),
                      "Media muestral (X̄₂)",
                      value = 45
                    ),

                    numericInput(
                      ns("s2_ic_eq"),
                      "Desviación estándar (s₂)",
                      value = 12
                    ),

                    numericInput(
                      ns("n2_ic_eq"),
                      "Tamaño de muestra (n₂)",
                      value = 35
                    )

                  )

                ),

                numericInput(
                  ns("conf_ic_eq"),
                  "Nivel de confianza (%)",
                  value = 95
                )

              )

            )

          ),

          fluidRow(

            column(
              4,

              wellPanel(

                h3("Resultados"),

                uiOutput(ns("resultados_ic_eq"))

              )

            ),

            column(
              8,

              wellPanel(

                h3("Gráfica"),

                plotOutput(
                  ns("grafica_ic_eq"),
                  height = "350px"
                )

              )

            )

          )

        ),

        # =================================================
        # PH VARIANZAS IGUALES
        # =================================================

        tabPanel(
          "Prueba de hipótesis",

          br(),

          fluidRow(

            column(
              6,

              wellPanel(

                h3("Fórmulas"),

                withMathJax(
                  helpText("Estadístico:"),

                  "$$
                  t=
                  \\frac{
                  (\\bar X_1-\\bar X_2)-\\mu_0
                  }{
                  S_p
                  \\sqrt{
                  \\frac1{n_1}+
                  \\frac1{n_2}
                  }
                  }
                  $$
                  "
                )

              )

            ),

            column(
              6,

              wellPanel(

                h3("Datos"),

                fluidRow(

                  column(
                    6,

                    numericInput(ns("xbar1_ph_eq"),"Media X̄₁",50),
                    numericInput(ns("s1_ph_eq"),"s₁",10),
                    numericInput(ns("n1_ph_eq"),"n₁",30)

                  ),

                  column(
                    6,

                    numericInput(ns("xbar2_ph_eq"),"Media X̄₂",45),
                    numericInput(ns("s2_ph_eq"),"s₂",12),
                    numericInput(ns("n2_ph_eq"),"n₂",35)

                  )

                ),

                numericInput(
                  ns("mu0_ph_eq"),
                  "Diferencia poblacional (H0)",
                  0
                ),

                numericInput(
                  ns("alpha_ph_eq"),
                  "Nivel de significancia α",
                  0.05
                ),

                selectInput(
                  ns("tipo_ph_eq"),
                  "Tipo de prueba",
                  choices = c(
                    "Bilateral",
                    "Cola derecha",
                    "Cola izquierda"
                  )
                )

              )

            )

          ),

          fluidRow(

            column(
              4,

              wellPanel(
                h3("Resultados"),
                uiOutput(ns("resultados_ph_eq"))
              )

            ),

            column(
              8,

              wellPanel(
                h3("Gráfica"),
                plotOutput(ns("grafica_ph_eq"), height = "350px")
              )

            )

          )

        ),

        tabPanel(
          "Tamaño de muestra",

          br(),

          fluidRow(

            column(
              6,

              wellPanel(

                h3("Fórmulas"),

                withMathJax(
                  helpText("Tamaño de muestra:"),

                  "$$
                  n=
                  \\left(
                  \\frac{Z_{\\alpha/2}\\sigma}{E}
                  \\right)^2
                  $$"
                )

              )

            ),

            column(
              6,

              wellPanel(

                h3("Datos"),

                numericInput(
                  ns("sigma_tm"),
                  "Desviación estándar poblacional (σ)",
                  value = 10
                ),

                numericInput(
                  ns("error_tm"),
                  "Error máximo permitido (E)",
                  value = 2
                ),

                numericInput(
                  ns("conf_tm"),
                  "Nivel de confianza (%)",
                  value = 95
                )

              )

            )

          ),

          fluidRow(

            column(
              4,

              wellPanel(

                h3("Resultados"),

                uiOutput(ns("resultados_tm"))

              )

            )

          )

        )

      )

    } else {

      tabsetPanel(

        # =================================================
        # IC WELCH
        # =================================================

        tabPanel(
          "Intervalo de confianza",

          br(),

          fluidRow(

            column(
              6,

              wellPanel(

                h3("Fórmulas"),

                withMathJax(
                  helpText("Estadístico Welch:"),

                  "$$
                  (\\bar{X}_1-\\bar{X}_2)
                  \\pm
                  t_{\\alpha/2}
                  \\sqrt{
                  \\frac{s_1^2}{n_1}+
                  \\frac{s_2^2}{n_2}
                  }
                  $$
                  "
                )

              )

            ),

            column(
              6,

              wellPanel(

                h3("Datos"),

                fluidRow(

                  column(
                    6,

                    numericInput(ns("xbar1_ic_w"),"Media muestral (X̄₁)",50),
                    numericInput(ns("s1_ic_w"),"Desviación estándar (s₁)",10),
                    numericInput(ns("n1_ic_w"),"Tamaño de muestra (n₁)",30)

                  ),

                  column(
                    6,

                    numericInput(ns("xbar2_ic_w"),"Media muestral (X̄₂)",45),
                    numericInput(ns("s2_ic_w"),"Desviación estándar (s₂)",12),
                    numericInput(ns("n2_ic_w"),"Tamaño de muestra (n₂)",35)

                  )

                ),

                numericInput(
                  ns("conf_ic_w"),
                  "Nivel de confianza (%)",
                  95
                )

              )

            )

          ),

          fluidRow(

            column(
              4,

              wellPanel(
                h3("Resultados"),
                uiOutput(ns("resultados_ic_w"))
              )

            ),

            column(
              8,

              wellPanel(
                h3("Gráfica"),
                plotOutput(ns("grafica_ic_w"), height = "350px")
              )

            )

          )

        ),

        # =================================================
        # PH WELCH
        # =================================================

        tabPanel(
          "Prueba de hipótesis",

          br(),

          fluidRow(

            column(
              6,

              wellPanel(

                h3("Fórmulas"),

                withMathJax(
                  helpText("Estadístico Welch:"),

                  "$$
                  t=
                  \\frac{
                  (\\bar X_1-\\bar X_2)-\\mu_0
                  }{
                  \\sqrt{
                  \\frac{s_1^2}{n_1}+
                  \\frac{s_2^2}{n_2}
                  }
                  }
                  $$
                  "
                )

              )

            ),

            column(
              6,

              wellPanel(

                h3("Datos"),

                fluidRow(

                  column(
                    6,

                    numericInput(ns("xbar1_ph_w"),"Media X̄₁",50),
                    numericInput(ns("s1_ph_w"),"s₁",10),
                    numericInput(ns("n1_ph_w"),"n₁",30)

                  ),

                  column(
                    6,

                    numericInput(ns("xbar2_ph_w"),"Media X̄₂",45),
                    numericInput(ns("s2_ph_w"),"s₂",12),
                    numericInput(ns("n2_ph_w"),"n₂",35)

                  )

                ),

                numericInput(
                  ns("mu0_ph_w"),
                  "Diferencia poblacional (H0)",
                  0
                ),

                numericInput(
                  ns("alpha_ph_w"),
                  "Nivel de significancia α",
                  0.05
                ),

                selectInput(
                  ns("tipo_ph_w"),
                  "Tipo de prueba",
                  choices = c(
                    "Bilateral",
                    "Cola derecha",
                    "Cola izquierda"
                  )
                )

              )

            )

          ),

          fluidRow(

            column(
              4,

              wellPanel(
                h3("Resultados"),
                uiOutput(ns("resultados_ph_w"))
              )

            ),

            column(
              8,

              wellPanel(
                h3("Gráfica"),
                plotOutput(ns("grafica_ph_w"), height = "350px")
              )

            )

          )

        ),

        tabPanel(
          "Tamaño de muestra",

          br(),

          fluidRow(

            column(
              6,

              wellPanel(

                h3("Fórmulas"),

                withMathJax(
                  helpText("Tamaño de muestra:"),

                  "$$
                  n=
                  \\left(
                  \\frac{Z_{\\alpha/2}\\sigma}{E}
                  \\right)^2
                  $$"
                )

              )

            ),

            column(
              6,

              wellPanel(

                h3("Datos"),

                numericInput(
                  ns("sigma_tm"),
                  "Desviación estándar poblacional (σ)",
                  value = 10
                ),

                numericInput(
                  ns("error_tm"),
                  "Error máximo permitido (E)",
                  value = 2
                ),

                numericInput(
                  ns("conf_tm"),
                  "Nivel de confianza (%)",
                  value = 95
                )

              )

            )

          ),

          fluidRow(

            column(
              4,

              wellPanel(

                h3("Resultados"),

                uiOutput(ns("resultados_tm"))

              )

            )

          )

        )

      )

    }

  })

}



# ============================================================
# 3.4 PRUEBA NO PARAMÉTRICA - MANN-WHITNEY
# ============================================================

mann_whitney_ui <- function(ns) {
  fluidPage(
  titlePanel(
    div(
      "3.4 Prueba no paramétrica para comparación de medianas (Mann-Whitney)",
      tags$small(style = "display:block; font-style:italic; font-size:40%;",
                 "App desarrollada por Liliana De la Torre Desentis")
    )
  ),
  
  tabsetPanel(
    
    # ===================== Pestaña 1 =====================
    tabPanel("Prueba Mann-Whitney",
             sidebarLayout(
               sidebarPanel(
                 helpText("Introduce las dos muestras separadas por comas:"),
                 textInput(ns("muestraX"), "Muestra X:", value = "12, 15, 14, 10, 9"),
                 textInput(ns("muestraY"), "Muestra Y:", value = "8, 7, 6, 11, 13"),
                 selectInput(ns("alternativa"), "Tipo de prueba (Hₐ):", 
                             choices = c("dos colas" = "two.sided", 
                                         "X > Y" = "greater", 
                                         "X < Y" = "less")),
                 numericInput(ns("nivel_conf"), "Nivel de confianza (%):", value = 95, min = 80, max = 99.9, step = 0.1),
                 actionButton(ns("calcular1"), "Calcular")
               ),
               
               mainPanel(
                 h4("Estadísticos:"),
                 verbatimTextOutput(ns("estadisticos1")),
                 
                 h4("Resultados de la prueba:"),
                 verbatimTextOutput(ns("resultado1")),
                 
                 h4("Tabla de rangos asignados:"),
                 DTOutput(ns("tabla_rangos1")),
                 
                 h4("Gráfico de comparación:"),
                 plotOutput(ns("grafico1"))
               )
             )
    ),
    
    # ===================== Pestaña 2 =====================
    tabPanel("Prueba Mann-Whitney con n grande",
             sidebarLayout(
               sidebarPanel(
                 textInput(ns("grupo1"), "Grupo 1", "6,8,8,10,10,10,11,11,12,12,12,12,13,13,13,14,14,14,15,15,15,16,17"),
                 textInput(ns("grupo2"), "Grupo 2", "6,7,7,7,7,7,10,10,10,10,12,12,12,13,13,13"),
                 selectInput(ns("hipotesis2"), "Tipo de prueba:",
                             choices = list("Bilateral" = "two.sided", 
                                            "Grupo 1 > Grupo 2" = "greater", 
                                            "Grupo 1 < Grupo 2" = "less")),
                 textInput(ns("nombre1"), "Nombre Grupo 1", "Niños"),
                 textInput(ns("nombre2"), "Nombre Grupo 2", "Niñas"),
                 actionButton(ns("calcular2"), "Calcular prueba")
               ),
               
               mainPanel(
                 verbatimTextOutput(ns("resultado2")),
                 plotOutput(ns("boxplot2"))
               )
             )
    ),
    
    # ===================== Pestaña 3 =====================
    tabPanel("Prueba Mann-Whitney archivo Excel",
             sidebarLayout(
               sidebarPanel(
                 fileInput(ns("archivo"), "Sube un archivo Excel",
                           accept = c(".xlsx", ".xls")),
                 selectInput(ns("hipotesis3"), "Tipo de prueba:",
                             choices = list("Bilateral" = "two.sided", 
                                            "Grupo 1 > Grupo 2" = "greater", 
                                            "Grupo 1 < Grupo 2" = "less")),
                 numericInput(ns("alpha"), "Nivel de significancia (α):", value = 0.05),
                 actionButton(ns("calcular3"), "Calcular prueba")
               ),
               
               mainPanel(
                 verbatimTextOutput(ns("resultado3")),
                 plotOutput(ns("boxplot3"))
               )
             )
    )
  )
)

# ===================== SERVER =====================
}

mann_whitney_server <- function(input, output, session) {
  
  colores_boxplot <- c("lightblue", "salmon")
  
  # --------------------- Pestaña 1 ---------------------
  observeEvent(input$calcular1, {
    
    # 🔹 Limpieza de datos
    x <- as.numeric(trimws(unlist(strsplit(input$muestraX, ","))))
    y <- as.numeric(trimws(unlist(strsplit(input$muestraY, ","))))
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    
    # 🔹 Datos combinados
    datos <- data.frame(
      Valor = c(x, y),
      Grupo = rep(c("X", "Y"), times = c(length(x), length(y)))
    )
    
    # 🔹 Rangos con empates
    datos$Rango <- rank(datos$Valor, ties.method = "average")
    
    # 🔹 Estadísticos
    TX <- sum(datos$Rango[datos$Grupo == "X"]) - (length(x)*(length(x)+1))/2
    TY <- sum(datos$Rango[datos$Grupo == "Y"]) - (length(y)*(length(y)+1))/2
    
    output$estadisticos1 <- renderPrint({
      cat("T_X =", round(TX, 4), "\n")
      cat("T_Y =", round(TY, 4), "\n")
    })
    
    # 🔹 Nivel de significancia
    alpha <- 1 - input$nivel_conf / 100
    
    # 🔹 Exacto o aproximado
    exacto <- (length(x) <= 20 & length(y) <= 20)
    
    prueba <- wilcox.test(
      x, y,
      alternative = input$alternativa,
      conf.level = input$nivel_conf / 100,
      exact = exacto
    )
    
    output$resultado1 <- renderPrint({
      
      cat("Nivel de confianza:", input$nivel_conf, "%\n")
      cat("Método:", ifelse(exacto, "Exacto", "Aproximación normal"), "\n\n")
      
      print(prueba)
      
      cat("\nInterpretación:\n")
      
      if(input$alternativa == "greater"){
        cat("Se evalúa si X tiende a tener valores mayores que Y.\n")
      } else if(input$alternativa == "less"){
        cat("Se evalúa si X tiende a tener valores menores que Y.\n")
      } else {
        cat("Se evalúa si las distribuciones son diferentes.\n")
      }
      
      if (prueba$p.value < alpha) {
        cat("\n🟢 Conclusión:\n")
        cat("Se rechaza H₀: existe evidencia de diferencia en las medianas.\n")
      } else {
        cat("\n🔴 Conclusión:\n")
        cat("No se rechaza H₀: no hay evidencia suficiente.\n")
      }
    })
    
    output$tabla_rangos1 <- renderDT({
      datatable(datos, rownames = FALSE)
    })
    
    output$grafico1 <- renderPlot({
      boxplot(x, y, names = c("X", "Y"),
              col = colores_boxplot,
              main = "Comparación de distribuciones",
              ylab = "Valores")
    })
  })
  
  # --------------------- Pestaña 2 ---------------------
  observeEvent(input$calcular2, {
    
    grupo1 <- as.numeric(trimws(unlist(strsplit(input$grupo1, ","))))
    grupo2 <- as.numeric(trimws(unlist(strsplit(input$grupo2, ","))))
    
    grupo1 <- grupo1[!is.na(grupo1)]
    grupo2 <- grupo2[!is.na(grupo2)]
    
    prueba <- wilcox.test(grupo1, grupo2,
                          alternative = input$hipotesis2,
                          exact = FALSE)
    
    output$resultado2 <- renderPrint({
      print(prueba)
    })
    
    output$boxplot2 <- renderPlot({
      datos <- data.frame(
        valor = c(grupo1, grupo2),
        grupo = factor(c(rep(input$nombre1, length(grupo1)),
                         rep(input$nombre2, length(grupo2))))
      )
      
      boxplot(valor ~ grupo, data = datos,
              col = colores_boxplot,
              main = "Comparación de grupos",
              ylab = "Valores")
    })
  })
  
  # --------------------- Pestaña 3 ---------------------
  observeEvent(input$calcular3, {
    
    req(input$archivo)
    
    datos <- read_excel(input$archivo$datapath)
    
    if (ncol(datos) != 2) {
      output$resultado3 <- renderPrint({
        cat("El archivo debe tener exactamente dos columnas.")
      })
      return()
    }
    
    grupo1 <- datos[[1]]
    grupo2 <- datos[[2]]
    
    prueba <- wilcox.test(grupo1, grupo2,
                          alternative = input$hipotesis3,
                          exact = FALSE)
    
    output$resultado3 <- renderPrint({
      print(prueba)
    })
    
    output$boxplot3 <- renderPlot({
      boxplot(grupo1, grupo2,
              col = colores_boxplot,
              names = colnames(datos),
              main = "Comparación de grupos",
              ylab = "Valores")
    })
  })
}


# ============================================================
# MODULO 5.1 - ANOVA
# ============================================================

anova_ui <- function(ns) {
  fluidPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    titlePanel("Prueba ANOVA de un Factor"),

    sidebarLayout(
      sidebarPanel(
        fileInput(
          ns("file"),
          "Sube tu archivo Excel (.xlsx)",
          accept = c(".xlsx", ".xls")
        ),
        numericInput(
          ns("alpha"),
          "Nivel de significancia (α)",
          value = 0.05,
          min = 0.01,
          max = 0.10,
          step = 0.01
        ),
        actionButton(
          ns("run"),
          "Realizar ANOVA",
          class = "btn-primary w-100"
        )
      ),

      mainPanel(
        tabsetPanel(
          tabPanel(
            "Resultados",
            h4("Resumen Estadístico"),
            verbatimTextOutput(ns("anova_summary")),
            h4("Tabla ANOVA"),
            tableOutput(ns("anova_table")),
            h4("Prueba Post Hoc de Tukey"),
            tableOutput(ns("tukey_result"))
          ),
          tabPanel(
            "Gráficos de Datos y F",
            h4("Distribución de los Grupos"),
            plotOutput(ns("boxplot")),
            h4("Distribución F y Región de Rechazo"),
            plotOutput(ns("f_rejection_plot"))
          ),
          tabPanel(
            "Diagnóstico de Supuestos",
            h4("Gráficos de Residuos"),
            plotOutput(ns("residual_plot"))
          )
        )
      )
    )
  )
}

anova_server <- function(input, output, session) {

  # Carga y limpieza de datos
  data_long <- reactive({
    req(input$file)

    df <- tryCatch({
      read_excel(input$file$datapath)
    }, error = function(e) {
      return(NULL)
    })

    validate(
      need(
        !is.null(df),
        "No se pudo leer el archivo Excel. Asegúrate de que tenga un formato válido."
      )
    )

    df <- janitor::clean_names(df)

    df_long <- df %>%
      pivot_longer(
        cols = everything(),
        names_to = "grupo",
        values_to = "valor"
      ) %>%
      filter(!is.na(valor)) %>%
      mutate(
        valor = as.numeric(valor),
        grupo = as.factor(grupo)
      ) %>%
      filter(!is.na(valor))

    validate(
      need(
        nrow(df_long) > 0,
        "El archivo no contiene datos numéricos válidos."
      ),
      need(
        n_distinct(df_long$grupo) >= 2,
        "Se requieren al menos 2 grupos para realizar ANOVA."
      )
    )

    df_long
  })

  # Cálculo del ANOVA al presionar el botón
  resultado <- eventReactive(input$run, {
    df <- data_long()

    modelo <- aov(valor ~ grupo, data = df)
    resumen <- summary(modelo)[[1]]

    F_value <- resumen[["F value"]][1]
    p_value <- resumen[["Pr(>F)"]][1]
    df1 <- resumen[["Df"]][1]
    df2 <- resumen[["Df"]][2]

    tukey_df <- NULL
    if (!is.na(p_value) && p_value < input$alpha) {
      tukey_obj <- TukeyHSD(modelo)
      tukey_df <- as.data.frame(tukey_obj$grupo)
      tukey_df <- round(tukey_df, 4)
    }

    list(
      modelo = modelo,
      resumen = resumen,
      F_value = F_value,
      p_value = p_value,
      df1 = df1,
      df2 = df2,
      tukey = tukey_df
    )
  })

  output$anova_summary <- renderPrint({
    req(resultado())
    res <- resultado()

    cat("Estadístico F calculado:", round(res$F_value, 4), "\n")
    cat("p-valor:", format.pval(res$p_value, digits = 4), "\n\n")

    if (res$p_value < input$alpha) {
      cat("Decisión: Se rechaza H₀ (α =", input$alpha, ")\n")
      cat(
        "Conclusión: Existen diferencias estadísticamente significativas entre las medias de al menos dos grupos.\n"
      )
    } else {
      cat("Decisión: No se rechaza H₀ (α =", input$alpha, ")\n")
      cat(
        "Conclusión: No hay evidencia suficiente para afirmar que existen diferencias entre las medias.\n"
      )
    }
  })

  output$anova_table <- renderTable({
    req(resultado())
    as.data.frame(resultado()$resumen)
  }, rownames = TRUE, na = "")

  output$tukey_result <- renderTable({
    req(resultado())
    tukey_data <- resultado()$tukey

    validate(
      need(
        !is.null(tukey_data),
        "El p-valor es mayor o igual a α; no se realiza la prueba de Tukey."
      )
    )

    tukey_data
  }, rownames = TRUE)

  output$boxplot <- renderPlot({
    df <- data_long()

    ggplot(df, aes(x = grupo, y = valor, fill = grupo)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE) +
      geom_jitter(width = 0.1, alpha = 0.5) +
      theme_minimal() +
      labs(
        title = "Distribución de Datos por Grupo",
        x = "Grupo",
        y = "Valor"
      )
  })

  output$f_rejection_plot <- renderPlot({
    req(resultado())
    res <- resultado()

    df1 <- res$df1
    df2 <- res$df2
    f_calc <- res$F_value
    f_critico <- qf(1 - input$alpha, df1, df2)

    x_max <- max(f_critico, f_calc, 5) * 1.2

    curve(
      df(x, df1, df2),
      from = 0,
      to = x_max,
      n = 500,
      col = "royalblue",
      lwd = 2,
      ylab = "Densidad",
      xlab = "F",
      main = "Distribución F de Fisher"
    )

    x_rect <- seq(f_critico, x_max, length.out = 200)
    polygon(
      c(f_critico, x_rect, x_max),
      c(0, df(x_rect, df1, df2), 0),
      col = rgb(1, 0, 0, 0.25),
      border = NA
    )

    abline(v = f_critico, col = "red", lwd = 2, lty = 2)
    abline(v = f_calc, col = "darkgreen", lwd = 2)

    legend(
      "topright",
      legend = c(
        paste("F crítico (", round(f_critico, 3), ")"),
        paste("F observado (", round(f_calc, 3), ")")
      ),
      col = c("red", "darkgreen"),
      lty = c(2, 1),
      lwd = 2
    )
  })

  output$residual_plot <- renderPlot({
    req(resultado())
    modelo <- resultado()$modelo

    par(mfrow = c(1, 2))

    plot(
      modelo$fitted.values,
      modelo$residuals,
      main = "Residuos vs Valores Ajustados",
      xlab = "Ajustados",
      ylab = "Residuos",
      pch = 19,
      col = "#2C3E50"
    )
    abline(h = 0, col = "red", lty = 2)

    qqnorm(
      modelo$residuals,
      main = "Normal Q-Q Plot",
      pch = 19,
      col = "#2C3E50"
    )
    qqline(modelo$residuals, col = "red", lwd = 2)
  })
}

ui <- page_navbar(
  title="MBA ITAM - Estadística para Negocios",
  theme=bs_theme(version=5, bootswatch="flatly", primary="#008f8f"),
  header=tags$head(tags$style(HTML(css))),
  sidebar=sidebar(width=300,
    div(class="sidebar-title","Herramientas interactivas"),
    div(class="sidebar-subtitle","ITAM"),
    tags$hr(class="sidebar-divider"),
    actionLink("inicio_sidebar","🏠 Inicio",class="sidebar-app"),
    tags$hr(class="sidebar-divider"),
    lapply(categorias,function(cat) tagList(
      div(class="sidebar-category",cat$nombre),
      lapply(cat$aplicaciones,function(app) actionLink(paste0("sidebar_app_",cat$id,"_",app$id),app$nombre,class="sidebar-app"))
    )),
    tags$br(),
    div(class="footer-app","App desarrollada por Liliana De la Torre Desentis")
  ),
  nav_panel(title="Inicio",value="inicio",
    div(class="contenido-principal",
      h1(class="titulo-principal","MBA ITAM - Estadística para Negocios"),
      h2(class="subtitulo-principal","Herramientas interactivas"),
      p(class="texto-bienvenida",HTML("Bienvenido(a) a <strong>Estadística para Negocios – MBA ITAM</strong>.<br>Selecciona una categoría para comenzar.")),
      uiOutput("contenido"),
      div(class="footer-app","App desarrollada por Liliana De la Torre Desentis")
    )
  )
)


# ============================================================
# MÓDULO 6.1 — PROPORCIÓN
# ============================================================
proporcion_ui <- function(ns) {
  fluidPage(
    titlePanel("Inferencia Estadística para Proporciones"),
    tabsetPanel(
      tabPanel(
        "1. Intervalo de Confianza",
        sidebarLayout(
          sidebarPanel(
            numericInput(ns("ic_n"), "Tamaño de la muestra (n):", value = 100, min = 1),
            numericInput(ns("ic_x"), "Éxitos observados (x o p̂):", value = 45, min = 0),
            sliderInput(ns("ic_nivel"), "Nivel de Confianza (1 - α) %:", min = 80, max = 99, value = 95, step = 1),
            checkboxInput(ns("ic_poblacion_finita"), "¿Ajustar por población finita (FCPF)?", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("ic_poblacion_finita")),
              numericInput(ns("ic_N"), "Tamaño de la población (N):", value = 1000, min = 2)
            ),
            actionButton(ns("ic_calcular"), "Calcular IC", class = "btn-primary")
          ),
          mainPanel(
            verbatimTextOutput(ns("ic_resumen")),
            plotOutput(ns("ic_grafico"))
          )
        )
      ),
      tabPanel(
        "2. Prueba de Hipótesis",
        sidebarLayout(
          sidebarPanel(
            numericInput(ns("ph_n"), "Tamaño de la muestra (n):", value = 100, min = 1),
            numericInput(ns("ph_x"), "Éxitos observados (x o p̂):", value = 40, min = 0),
            numericInput(ns("ph_p0"), "Proporción bajo H₀ (p₀):", value = 0.5, min = 0.0001, max = 0.9999, step = 0.01),
            selectInput(
              inputId = ns("ph_tipo_ha"),
              label = "Hipótesis alternativa (Hₐ):",
              choices = c("Bilateral (p ≠ p₀)" = "two.sided",
                          "Unilateral Derecha (p > p₀)" = "greater",
                          "Unilateral Izquierda (p < p₀)" = "less")
            ),
            selectInput(
              inputId = ns("ph_alpha"),
              label = HTML("Nivel de significancia (&alpha;):"),
              choices = c("0.01" = 0.01, "0.02" = 0.02, "0.05" = 0.05, "0.10" = 0.10),
              selected = 0.05
            ),
            checkboxInput(ns("ph_poblacion_finita"), "¿Ajustar por población finita (FCPF)?", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("ph_poblacion_finita")),
              numericInput(ns("ph_N"), "Tamaño de la población (N):", value = 1000, min = 2)
            ),
            actionButton(ns("ph_calcular"), "Ejecutar Prueba", class = "btn-primary")
          ),
          mainPanel(
            verbatimTextOutput(ns("ph_resultados")),
            plotOutput(ns("ph_grafico"))
          )
        )
      ),
      tabPanel(
        "3. Tamaño de la Muestra",
        sidebarLayout(
          sidebarPanel(
            sliderInput(ns("tm_nivel"), "Nivel de Confianza (1 - α) %:", min = 80, max = 99, value = 95, step = 1),
            numericInput(ns("tm_e"), "Margen de error máximo deseado (E):", value = 0.05, min = 0.001, max = 0.5, step = 0.01),
            numericInput(ns("tm_p"), "Proporción esperada / previa (p):", value = 0.5, min = 0.01, max = 0.99, step = 0.05),
            helpText("Nota: Usar p = 0.50 garantiza la máxima varianza (escenario más conservador)."),
            checkboxInput(ns("tm_conocer_N"), "¿Se conoce el tamaño de la población (N)?", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("tm_conocer_N")),
              numericInput(ns("tm_N"), "Tamaño de la población (N):", value = 1000, min = 2)
            ),
            actionButton(ns("tm_calcular"), "Calcular Tamaño Muestral", class = "btn-primary")
          ),
          mainPanel(
            verbatimTextOutput(ns("tm_resultados"))
          )
        )
      )
    )
  )
}

proporcion_server <- function(input, output, session) {
  
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


# MÓDULO 6.2 — DIFERENCIA DE PROPORCIONES
# ============================================================
library(shiny)
library(ggplot2)

diferencia_proporciones_ui <- function(ns) {
  fluidPage(
  
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
                 numericInput(ns("ic_x1"), "Número de éxitos muestra 1 (x1):", value = 18, min = 0),
                 numericInput(ns("ic_n1"), "Tamaño muestra 1 (n1):", value = 150, min = 1),
                 numericInput(ns("ic_x2"), "Número de éxitos muestra 2 (x2):", value = 10, min = 0),
                 numericInput(ns("ic_n2"), "Tamaño muestra 2 (n2):", value = 130, min = 1),
                 numericInput(ns("ic_nivel"), "Nivel de confianza (%):", value = 95, min = 80, max = 99.999, step = 0.1),
                 actionButton(ns("ic_calcular"), "Calcular", class = "btn-primary")
               ),
               
               mainPanel(
                 verbatimTextOutput(ns("ic_resultados")),
                 plotOutput(ns("ic_grafica"), height = "400px")
               )
             )
    ),
    
    tabPanel("Prueba de hipótesis",
             withMathJax(),
             helpText("Estadístico de prueba Z:"),
             helpText("$$Z = \\frac{(\\hat{p}_1 - \\hat{p}_2) - p_0}{\\text{SE}}$$"),
             
             sidebarLayout(
               sidebarPanel(
                 numericInput(ns("ph_x1"), "Éxitos muestra 1 (x1):", value = 50, min = 0),
                 numericInput(ns("ph_n1"), "Tamaño muestra 1 (n1):", value = 100, min = 1),
                 numericInput(ns("ph_x2"), "Éxitos muestra 2 (x2):", value = 45, min = 0),
                 numericInput(ns("ph_n2"), "Tamaño muestra 2 (n2):", value = 100, min = 1),
                 numericInput(ns("ph_d0"), "Diferencia bajo H₀ (p₀):", value = 0, step = 0.01),
                 
                 selectInput(ns("ph_tipo"), "Tipo de prueba (Ha):",
                             choices = c("Mayor (p1 - p2 > p0)" = "mayor",
                                         "Menor (p1 - p2 < p0)" = "menor",
                                         "Diferente (p1 - p2 ≠ p0)" = "dos")),
                 
                 numericInput(ns("ph_nivelconf"), "Nivel de confianza (%):", value = 95, min = 80, max = 99.999, step = 0.1),
                 
                 actionButton(ns("ph_calcular"), "Realizar prueba", class = "btn-primary")
               ),
               
               mainPanel(
                 verbatimTextOutput(ns("ph_resultados")),
                 plotOutput(ns("ph_grafico"), height = "400px")
               )
             )
    )
  )
)
}

diferencia_proporciones_server <- function(input, output, session) {
  
  # Evento para Intervalo de Confianza
  observeEvent(input$ic_calcular, ignoreNULL = TRUE, {
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
  observeEvent(input$ph_calcular, ignoreNULL = TRUE, {
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




# ============================================================
# MÓDULO 6.3 — DIFERENCIA DE PROPORCIONES IGUALES
# ============================================================
diferencia_proporciones_iguales_ui <- function(ns) {
  fluidPage(
  
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
                 numericInput( ns("ic_x1"), "Número de éxitos muestra 1 (x1):", value = 18, min = 0),
                 numericInput( ns("ic_n1"), "Tamaño muestra 1 (n1):", value = 150, min = 1),
                 numericInput( ns("ic_x2"), "Número de éxitos muestra 2 (x2):", value = 10, min = 0),
                 numericInput( ns("ic_n2"), "Tamaño muestra 2 (n2):", value = 130, min = 1),
                 numericInput( ns("ic_nivel"), "Nivel de confianza (%):", value = 95, min = 80, max = 99.9999, step = 0.1),
                 actionButton( ns("ic_calcular"), "Calcular")
               ),
               
               mainPanel(
                 verbatimTextOutput( ns("ic_resultados")),
                 plotOutput( ns("ic_grafica"), height="400px")
               )
             )
    ),
    
    
    tabPanel("Prueba de hipótesis",
             
             sidebarLayout(
               sidebarPanel(
                 
                 numericInput( ns("ph_x1"), "Éxitos muestra 1 (x1):", value = 50, min = 0),
                 numericInput( ns("ph_n1"), "Tamaño muestra 1 (n1):", value = 100, min = 1),
                 numericInput( ns("ph_x2"), "Éxitos muestra 2 (x2):", value = 45, min = 0),
                 numericInput( ns("ph_n2"), "Tamaño muestra 2 (n2):", value = 100, min = 1),
                 numericInput( ns("ph_d0"), "Diferencia bajo H₀ (p₀):", value = 0, step = 0.01),
                 
                 selectInput( ns("ph_tipo"), "Tipo de prueba (Ha):",
                             choices = c("Mayor" = "mayor",
                                         "Menor" = "menor",
                                         "Diferente" = "dos")),
                 
                 numericInput( ns("ph_nivelconf"), "Nivel de confianza:",
                              value = 0.95, min = 0.8, max = 0.9999, step = 0.01),
                 
                 actionButton( ns("ph_calcular"), "Realizar prueba")
                 
               ),
               
               mainPanel(
                 
                 withMathJax(
                   helpText("**Estadístico de prueba Z:**"),
                   helpText("$$Z = \\frac{\\hat{p}_1 - \\hat{p}_2 - p_0}{\\sqrt{\\hat{p} (1 - \\hat{p}) \\left( \\frac{1}{n_1} + \\frac{1}{n_2} \\right)}}$$")
                 ),
                 
                 verbatimTextOutput( ns("ph_resultados")),
                 plotOutput( ns("ph_grafico"), height="400px")
                 
               )
               
             )
             
    )
    
  )
)
}

diferencia_proporciones_iguales_server <- function(input, output, session) {
  
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


# MÓDULO 7.1 — PRUEBA DE HIPÓTESIS NO PARAMÉTRICA
# ============================================================
prueba_var_validar_datos <- function(df) {
  
  if (is.null(df)) {
    return("No se han cargado datos.")
  }
  
  if (ncol(df) != 2) {
    return(
      "El archivo debe contener exactamente dos columnas numéricas."
    )
  }
  
  if (!all(sapply(df, is.numeric))) {
    return(
      "Las dos columnas del archivo deben ser numéricas."
    )
  }
  
  if (any(is.na(df[[1]])) || any(is.na(df[[2]]))) {
    return(
      "Los datos no deben contener valores faltantes (NA)."
    )
  }
  
  if (any(!is.finite(df[[1]])) ||
      any(!is.finite(df[[2]]))) {
    return(
      "Los datos no deben contener valores infinitos."
    )
  }
  
  if (length(df[[1]]) < 2 ||
      length(df[[2]]) < 2) {
    return(
      "Cada muestra debe contener al menos dos observaciones."
    )
  }
  
  return(NULL)
}


# ------------------------------------------------------------
# Función para obtener las hipótesis
# ------------------------------------------------------------

prueba_var_obtener_hipotesis <- function(alternativa) {
  
  if (alternativa == "mayor") {
    
    return(
      list(
        h0 = "H₀: La dispersión de X no es mayor que la de Y.",
        ha = "H₁: La dispersión de X es mayor que la de Y."
      )
    )
    
  } else if (alternativa == "menor") {
    
    return(
      list(
        h0 = "H₀: La dispersión de X no es menor que la de Y.",
        ha = "H₁: La dispersión de X es menor que la de Y."
      )
    )
    
  } else {
    
    return(
      list(
        h0 = "H₀: No existe diferencia en la dispersión de X y Y.",
        ha = "H₁: Existe diferencia en la dispersión de X y Y."
      )
    )
  }
}


# ------------------------------------------------------------
# Función para realizar el análisis
# ------------------------------------------------------------

prueba_var_calcular_prueba <- function(x, y, alternativa, alpha) {
  
  m <- length(x)
  n <- length(y)
  N <- m + n
  
  # ----------------------------------------------------------
  # Desviaciones absolutas respecto a la media
  # ----------------------------------------------------------
  
  media_x <- mean(x)
  media_y <- mean(y)
  
  dx <- abs(x - media_x)
  dy <- abs(y - media_y)
  
  # ----------------------------------------------------------
  # Rangos conjuntos
  # ----------------------------------------------------------
  
  datos_combinados <- c(dx, dy)
  
  rangos <- rank(
    datos_combinados,
    ties.method = "average"
  )
  
  rangos_x <- rangos[1:m]
  rangos_y <- rangos[(m + 1):N]
  
  # ----------------------------------------------------------
  # Estadísticos U
  # ----------------------------------------------------------
  
  UX <- sum(rangos_x) - m * (m + 1) / 2
  UY <- sum(rangos_y) - n * (n + 1) / 2
  
  # Esperanza
  esperanza <- m * n / 2
  
  # ----------------------------------------------------------
  # Corrección por empates
  # ----------------------------------------------------------
  
  tabla_empates <- table(rangos)
  tamanos_empates <- as.numeric(tabla_empates)
  
  correccion_empates <- sum(
    tamanos_empates^3 - tamanos_empates
  )
  
  var_U <- (
    m * n / 12
  ) * (
    N + 1 -
      correccion_empates / (N * (N - 1))
  )
  
  if (var_U <= 0) {
    
    stop(
      paste0(
        "No es posible calcular la estadística Z porque ",
        "la varianza de U es igual a cero. ",
        "Esto ocurre cuando todos los valores tienen el mismo rango."
      )
    )
  }
  
  # ----------------------------------------------------------
  # Corrección por continuidad
  # ----------------------------------------------------------
  
  if (UX > esperanza) {
    
    ajuste <- 0.5
    
  } else if (UX < esperanza) {
    
    ajuste <- -0.5
    
  } else {
    
    ajuste <- 0
  }
  
  Z <- (
    UX - esperanza - ajuste
  ) / sqrt(var_U)
  
  # ----------------------------------------------------------
  # Valores críticos y p-value
  # ----------------------------------------------------------
  
  if (alternativa == "mayor") {
    
    z_critico <- qnorm(1 - alpha)
    
    p_value <- 1 - pnorm(Z)
    
    region_rechazo <- paste0(
      "Rechazar H₀ si Z > ",
      round(z_critico, 4)
    )
    
    decision <- ifelse(
      Z > z_critico,
      "Se rechaza H₀",
      "No se rechaza H₀"
    )
    
  } else if (alternativa == "menor") {
    
    z_critico <- qnorm(alpha)
    
    p_value <- pnorm(Z)
    
    region_rechazo <- paste0(
      "Rechazar H₀ si Z < ",
      round(z_critico, 4)
    )
    
    decision <- ifelse(
      Z < z_critico,
      "Se rechaza H₀",
      "No se rechaza H₀"
    )
    
  } else {
    
    z_critico <- qnorm(1 - alpha / 2)
    
    p_value <- 2 * pnorm(-abs(Z))
    
    region_rechazo <- paste0(
      "Rechazar H₀ si Z < ",
      round(-z_critico, 4),
      " o Z > ",
      round(z_critico, 4)
    )
    
    decision <- ifelse(
      abs(Z) > z_critico,
      "Se rechaza H₀",
      "No se rechaza H₀"
    )
  }
  
  # ----------------------------------------------------------
  # Conclusión
  # ----------------------------------------------------------
  
  if (alternativa == "mayor") {
    
    if (p_value < alpha) {
      
      conclusion <- paste0(
        "Con un nivel de significancia de ",
        alpha,
        ", existe evidencia estadísticamente significativa ",
        "de que las desviaciones absolutas de X tienden a ser ",
        "mayores que las de Y."
      )
      
    } else {
      
      conclusion <- paste0(
        "Con un nivel de significancia de ",
        alpha,
        ", no existe evidencia estadísticamente significativa ",
        "para concluir que las desviaciones absolutas de X ",
        "tienden a ser mayores que las de Y."
      )
    }
    
  } else if (alternativa == "menor") {
    
    if (p_value < alpha) {
      
      conclusion <- paste0(
        "Con un nivel de significancia de ",
        alpha,
        ", existe evidencia estadísticamente significativa ",
        "de que las desviaciones absolutas de X tienden a ser ",
        "menores que las de Y."
      )
      
    } else {
      
      conclusion <- paste0(
        "Con un nivel de significancia de ",
        alpha,
        ", no existe evidencia estadísticamente significativa ",
        "para concluir que las desviaciones absolutas de X ",
        "tienden a ser menores que las de Y."
      )
    }
    
  } else {
    
    if (p_value < alpha) {
      
      conclusion <- paste0(
        "Con un nivel de significancia de ",
        alpha,
        ", existe evidencia estadísticamente significativa ",
        "de una diferencia entre las distribuciones de las ",
        "desviaciones absolutas de X y Y."
      )
      
    } else {
      
      conclusion <- paste0(
        "Con un nivel de significancia de ",
        alpha,
        ", no existe evidencia estadísticamente significativa ",
        "de una diferencia entre las distribuciones de las ",
        "desviaciones absolutas de X y Y."
      )
    }
  }
  
  # ----------------------------------------------------------
  # Regresar resultados
  # ----------------------------------------------------------
  
  list(
    m = m,
    n = n,
    N = N,
    media_x = media_x,
    media_y = media_y,
    dx = dx,
    dy = dy,
    rangos_x = rangos_x,
    rangos_y = rangos_y,
    UX = UX,
    UY = UY,
    esperanza = esperanza,
    var_U = var_U,
    Z = Z,
    z_critico = z_critico,
    p_value = p_value,
    region_rechazo = region_rechazo,
    decision = decision,
    conclusion = conclusion,
    alpha = alpha,
    alternativa = alternativa
  )
}


# ============================================================
# 3. INTERFAZ DE USUARIO
# ============================================================


prueba_no_parametrica_var_ui <- function(ns) {

fluidPage(
  
  
  titlePanel(
    "Prueba no paramétrica para comparar la dispersión"
  ),
  
  tags$div(
    style = paste(
      "font-size: small;",
      "font-style: italic;",
      "margin-bottom: 15px;"
    ),
    "Mann–Whitney aplicado a desviaciones absolutas. ",
    "App desarrollada por Liliana De la Torre Desentis"
  ),
  
  fluidRow(
    
    column(
      width = 4,
      
      wellPanel(
        
        h4("Datos"),
        
        fileInput(
          ns("archivo"),
          "Seleccione un archivo Excel:",
          accept = c(
            ".xlsx",
            ".xls"
          )
        ),
        
        tags$small(
          "El archivo debe contener exactamente dos columnas numéricas."
        ),
        
        br(),
        br(),
        
        radioButtons(
          ns("alternativa"),
          "Hipótesis alternativa:",
          
          choices = c(
            "X tiene mayor dispersión que Y" = "mayor",
            "X tiene menor dispersión que Y" = "menor",
            "La dispersión de X y Y es diferente" = "diferente"
          ),
          
          selected = "diferente"
        ),
        
        numericInput(
          ns("alpha"),
          "Nivel de significancia (α):",
          value = 0.05,
          min = 0.001,
          max = 0.20,
          step = 0.01
        ),
        
        actionButton(
          ns("calcular"),
          "Calcular",
          class = "btn-primary"
        )
      )
    ),
    
    column(
      width = 8,
      
      tabsetPanel(
        
        # ======================================================
        # TAB 1: INFORMACIÓN
        # ======================================================
        
        tabPanel(
          
          "Información",
          
          br(),
          
          h4("Método"),
          
          p(
            "La prueba de Mann–Whitney se aplica a las ",
            "desviaciones absolutas respecto de la media de ",
            "cada muestra."
          ),
          
          withMathJax(
            
            tags$div(
              
              style = paste(
                "font-size: 16px;",
                "line-height: 2;"
              ),
              
              HTML(
                "$$
                D_{Xi}=|X_i-\\bar X|
                $$"
              ),
              
              HTML(
                "$$
                D_{Yj}=|Y_j-\\bar Y|
                $$"
              ),
              
              HTML(
                "$$
                U_X =
                \\sum R(D_{Xi})
                -
                \\frac{m(m+1)}{2}
                $$"
              ),
              
              HTML(
                "$$
                U_Y =
                \\sum R(D_{Yj})
                -
                \\frac{n(n+1)}{2}
                $$"
              )
            )
          ),
          
          tags$div(
            
            style = paste(
              "background-color: #f8f9fa;",
              "padding: 15px;",
              "border-radius: 5px;",
              "margin-top: 15px;"
            ),
            
            strong("Importante: "),
            
            "Este procedimiento es una comparación ",
            "no paramétrica de las desviaciones absolutas ",
            "respecto de la media. ",
            
            "No constituye una prueba paramétrica directa ",
            "del cociente de varianzas ",
            
            withMathJax(
              HTML(
                "$\\sigma_X^2/\\sigma_Y^2$"
              )
            ),
            
            "."
          )
        ),
        
        
        # ======================================================
        # TAB 2: DATOS
        # ======================================================
        
        tabPanel(
          
          "Datos",
          
          br(),
          
          tableOutput(ns("tabla_datos"))
        ),
        
        
        # ======================================================
        # TAB 3: RESULTADOS
        # ======================================================
        
        tabPanel(
          
          "Resultados",
          
          br(),
          
          h4("Estadísticos descriptivos"),
          
          tableOutput(ns("tabla_descriptivos")),
          
          br(),
          
          h4("Hipótesis"),
          
          verbatimTextOutput(ns("hipotesis")),
          
          br(),
          
          h4("Estadístico de prueba"),
          
          tableOutput(ns("tabla_estadistico")),
          
          br(),
          
          h4("Decisión y conclusión"),
          
          verbatimTextOutput(ns("decision")),
          
          br(),
          
          h4("Nota metodológica"),
          
          p(
            "El valor-p se obtiene mediante la aproximación ",
            "normal de Mann–Whitney, incorporando corrección ",
            "por empates y corrección por continuidad."
          )
        ),
        
        
        # ======================================================
        # TAB 4: GRÁFICA
        # ======================================================
        
        tabPanel(
          
          "Gráfica",
          
          br(),
          
          plotOutput(
            ns("grafica"),
            height = "600px"
          )
        )
      )
    )
  )
)

}

# ============================================================
# 4. SERVIDOR
# ============================================================
prueba_no_parametrica_var_server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Lectura del archivo
  # ----------------------------------------------------------
  
  datos <- reactive({
    
    req(input$archivo)
    
    df <- read_excel(
      input$archivo$datapath
    )
    
    error <- prueba_var_validar_datos(df)
    
    if (!is.null(error)) {
      
      showNotification(
        error,
        type = "error",
        duration = NULL
      )
      
      return(NULL)
    }
    
    df
  })
  
  
  # ----------------------------------------------------------
  # Cálculo
  # ----------------------------------------------------------
  
  resultados <- eventReactive(
    input$calcular,
    {
      
      df <- datos()
      
      req(df)
      
      alpha <- input$alpha
      
      validate(
        need(!is.null(alpha) && is.finite(alpha) && alpha > 0 && alpha < 1,
             "El nivel de significancia α debe estar entre 0 y 1."),
        need(input$alternativa %in% c("mayor", "menor", "diferente"),
             "Seleccione una hipótesis alternativa válida.")
      )
      
      alternativa <- input$alternativa
      
      x <- df[[1]]
      y <- df[[2]]
      
      tryCatch(
        
        {
          
          prueba_var_calcular_prueba(
            x = x,
            y = y,
            alternativa = alternativa,
            alpha = alpha
          )
        },
        
        error = function(e) {
          
          showNotification(
            e$message,
            type = "error",
            duration = NULL
          )
          
          NULL
        }
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # Tabla de datos
  # ----------------------------------------------------------
  
  output$tabla_datos <- renderTable({
    
    df <- datos()
    
    req(df)
    
    r <- resultados()
    
    if (is.null(r)) {
      
      data.frame(
        Mensaje = "Presione 'Calcular' para obtener los rangos."
      )
      
    } else {
      
      tabla_x <- data.frame(
        Grupo = "X",
        Observacion = seq_along(df[[1]]),
        Valor = df[[1]],
        Desviacion_absoluta = r$dx,
        Rango = r$rangos_x
      )
      
      tabla_y <- data.frame(
        Grupo = "Y",
        Observacion = seq_along(df[[2]]),
        Valor = df[[2]],
        Desviacion_absoluta = r$dy,
        Rango = r$rangos_y
      )
      
      rbind(
        tabla_x,
        tabla_y
      )
    }
  },
  
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE,
  digits = 4
  )
  
  
  # ----------------------------------------------------------
  # Tabla descriptiva
  # ----------------------------------------------------------
  
  output$tabla_descriptivos <- renderTable({
    
    r <- resultados()
    
    req(r)
    
    data.frame(
      
      Estadístico = c(
        "Tamaño de muestra",
        "Media",
        "Media de desviaciones absolutas"
      ),
      
      X = c(
        r$m,
        r$media_x,
        mean(r$dx)
      ),
      
      Y = c(
        r$n,
        r$media_y,
        mean(r$dy)
      )
    )
    
  },
  
  striped = TRUE,
  bordered = TRUE,
  digits = 4
  )
  
  
  # ----------------------------------------------------------
  # Hipótesis
  # ----------------------------------------------------------
  
  output$hipotesis <- renderText({
    
    r <- resultados()
    
    req(r)
    
    h <- prueba_var_obtener_hipotesis(
      r$alternativa
    )
    
    paste(
      h$h0,
      h$ha,
      sep = "\n"
    )
  })
  
  
  # ----------------------------------------------------------
  # Tabla del estadístico
  # ----------------------------------------------------------
  
  output$tabla_estadistico <- renderTable({
    
    r <- resultados()
    
    req(r)
    
    if (r$alternativa == "diferente") {
      
      valor_critico <- paste0(
        "±",
        round(r$z_critico, 4)
      )
      
    } else {
      
      valor_critico <- round(
        r$z_critico,
        4
      )
    }
    
    data.frame(
      
      Estadístico = c(
        "Uₓ",
        "Uᵧ",
        "Uₓ + Uᵧ",
        "E(Uₓ)",
        "Var(Uₓ)",
        "Z",
        "Valor crítico",
        "Nivel de significancia",
        "Valor-p"
      ),
      
      Valor = c(
        round(r$UX, 4),
        round(r$UY, 4),
        round(r$UX + r$UY, 4),
        round(r$esperanza, 4),
        round(r$var_U, 4),
        round(r$Z, 4),
        valor_critico,
        r$alpha,
        round(r$p_value, 6)
      )
    )
    
  },
  
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Decisión
  # ----------------------------------------------------------
  
  output$decision <- renderText({
    
    r <- resultados()
    
    req(r)
    
    paste(
      
      "Región de rechazo:",
      r$region_rechazo,
      
      "",
      
      "Decisión:",
      r$decision,
      
      "",
      
      "Conclusión:",
      r$conclusion,
      
      sep = "\n"
    )
  })
  
  
  # ----------------------------------------------------------
  # Gráfica de la distribución normal estándar
  # ----------------------------------------------------------
  
  output$grafica <- renderPlot({
    
    r <- resultados()
    
    req(r)
    
    alpha <- r$alpha
    
    Z <- r$Z
    
    x <- seq(
      -4,
      4,
      length.out = 1000
    )
    
    y <- dnorm(x)
    
    plot(
      x,
      y,
      type = "l",
      lwd = 2,
      xlab = "Z",
      ylab = "Densidad",
      main = "Distribución normal estándar"
    )
    
    # --------------------------------------------------------
    # Regiones críticas
    # --------------------------------------------------------
    
    if (r$alternativa == "mayor") {
      
      zc <- r$z_critico
      
      x_region <- x[x >= zc]
      y_region <- y[x >= zc]
      
      polygon(
        c(
          zc,
          x_region,
          max(x_region)
        ),
        c(
          0,
          y_region,
          0
        ),
        col = "tomato",
        border = NA
      )
      
      abline(
        v = zc,
        col = "red",
        lty = 2,
        lwd = 2
      )
      
    } else if (r$alternativa == "menor") {
      
      zc <- r$z_critico
      
      x_region <- x[x <= zc]
      y_region <- y[x <= zc]
      
      polygon(
        c(
          min(x_region),
          x_region,
          zc
        ),
        c(
          0,
          y_region,
          0
        ),
        col = "tomato",
        border = NA
      )
      
      abline(
        v = zc,
        col = "red",
        lty = 2,
        lwd = 2
      )
      
    } else {
      
      zc <- r$z_critico
      
      x_region_izq <- x[x <= -zc]
      y_region_izq <- y[x <= -zc]
      
      polygon(
        c(
          min(x_region_izq),
          x_region_izq,
          -zc
        ),
        c(
          0,
          y_region_izq,
          0
        ),
        col = "tomato",
        border = NA
      )
      
      x_region_der <- x[x >= zc]
      y_region_der <- y[x >= zc]
      
      polygon(
        c(
          zc,
          x_region_der,
          max(x_region_der)
        ),
        c(
          0,
          y_region_der,
          0
        ),
        col = "tomato",
        border = NA
      )
      
      abline(
        v = c(-zc, zc),
        col = "red",
        lty = 2,
        lwd = 2
      )
    }
    
    # Volver a dibujar la curva
    lines(
      x,
      y,
      lwd = 2
    )
    
    # --------------------------------------------------------
    # Z observado
    # --------------------------------------------------------
    
    abline(
      v = Z,
      col = "black",
      lwd = 3
    )
    
    # --------------------------------------------------------
    # Etiquetas
    # --------------------------------------------------------
    
    usr <- par("usr")
    
    text(
      Z,
      usr[4] * 0.90,
      paste0(
        "Z observado = ",
        round(Z, 3)
      ),
      pos = ifelse(Z >= 0, 4, 2),
      font = 2
    )
    
    # --------------------------------------------------------
    # Leyenda
    # --------------------------------------------------------
    
    legend(
      "topright",
      legend = c(
        "Distribución normal",
        "Región de rechazo",
        "Z observado"
      ),
      lty = c(
        1,
        NA,
        1
      ),
      lwd = c(
        2,
        NA,
        3
      ),
      pch = c(
        NA,
        15,
        NA
      ),
      pt.cex = c(
        NA,
        1.5,
        NA
      ),
      col = c(
        "black",
        "tomato",
        "black"
      ),
      bty = "n"
    )
    
    # --------------------------------------------------------
    # Información debajo de la gráfica
    # --------------------------------------------------------
    
    mtext(
      paste0(
        "α = ",
        r$alpha,
        "     Valor-p = ",
        round(r$p_value, 6)
      ),
      side = 1,
      line = 4
    )
    
    mtext(
      r$decision,
      side = 1,
      line = 5.5,
      font = 2
    )
    
  })
}



# ============================================================
# MÓDULO 7.2 - VARIANZA Y COCIENTE DE VARIANZAS
# ============================================================
# Una varianza: IC y prueba de hipótesis con chi-cuadrada.
# Comparación de varianzas: IC y prueba de hipótesis con F.
# ============================================================

varianza_cociente_ui <- function(ns) {
  fluidPage(
    titlePanel(
      div(
        "Cociente de varianzas",
        tags$small(
          "App desarrollada por Liliana De la Torre Desentis",
          style = "display:block; font-style:italic; font-size:40%;"
        )
      )
    ),

    withMathJax(),

    tabsetPanel(

      # ========================================================
      # CASO 1: UNA VARIANZA
      # ========================================================
      tabPanel(
        "Una varianza",
        br(),
        tabsetPanel(

          # ---------------- INTERVALO DE CONFIANZA ------------
          tabPanel(
            "Intervalo de confianza",
            br(),
            fluidRow(
              column(
                width = 4,
                wellPanel(
                  h4("Datos"),
                  numericInput(ns("s2_ic"), "Varianza muestral (s²)", 100, min = 0.000001),
                  numericInput(ns("n_ic"), "Tamaño de muestra (n)", 30, min = 2, step = 1),
                  numericInput(ns("nivel_ic"), "Nivel de confianza (%)", 95, min = 0.01, max = 99.99),
                  actionButton(ns("calc_ic"), "Calcular", class = "btn-primary")
                )
              ),
              column(
                width = 4,
                wellPanel(
                  h4("Fórmula"),
                  HTML("$$\\frac{(n-1)S^2}{\\sigma^2}\\sim\\chi^2_{n-1}$$"),
                  HTML("$$IC(\\sigma^2)=\\left(\\frac{(n-1)s^2}{\\chi^2_{1-\\alpha/2}},\\frac{(n-1)s^2}{\\chi^2_{\\alpha/2}}\\right)$$"),
                  hr(),
                  h4("Resultados"),
                  verbatimTextOutput(ns("res_ic"))
                )
              ),
              column(
                width = 4,
                wellPanel(
                  h4("Gráfica"),
                  plotOutput(ns("plot_ic"), height = "420px")
                )
              )
            )
          ),

          # ---------------- PRUEBA DE HIPÓTESIS ---------------
          tabPanel(
            "Prueba de hipótesis",
            br(),
            fluidRow(
              column(
                width = 4,
                wellPanel(
                  h4("Datos"),
                  numericInput(ns("s2_ph"), "Varianza muestral (s²)", 100, min = 0.000001),
                  numericInput(ns("n_ph"), "Tamaño de muestra (n)", 30, min = 2, step = 1),
                  numericInput(ns("sigma20_ph"), "Varianza bajo H₀ (σ₀²)", 80, min = 0.000001),
                  selectInput(
                    ns("tipo_ph"), "Tipo de prueba",
                    choices = c("Bilateral" = "two.sided", "Cola derecha" = "greater", "Cola izquierda" = "less")
                  ),
                  numericInput(ns("alpha_ph"), "Nivel de significancia (α)", 0.05, min = 0.0001, max = 0.5),
                  actionButton(ns("calc_ph"), "Calcular", class = "btn-primary")
                )
              ),
              column(
                width = 4,
                wellPanel(
                  h4("Fórmula"),
                  HTML("$$\\chi^2=\\frac{(n-1)s^2}{\\sigma_0^2}$$"),
                  hr(),
                  h4("Resultados"),
                  verbatimTextOutput(ns("res_ph"))
                )
              ),
              column(
                width = 4,
                wellPanel(
                  h4("Gráfica"),
                  plotOutput(ns("plot_ph"), height = "420px")
                )
              )
            )
          )
        )
      ),

      # ========================================================
      # CASO 2: COMPARACIÓN DE VARIANZAS
      # ========================================================
      tabPanel(
        "Comparación de varianzas",
        br(),
        tabsetPanel(

          # ---------------- INTERVALO DE CONFIANZA ------------
          tabPanel(
            "Intervalo de confianza",
            br(),
            fluidRow(
              column(
                width = 4,
                wellPanel(
                  h4("Datos"),
                  numericInput(ns("s21_ic"), "Varianza muestral 1 (s₁²)", 100, min = 0.000001),
                  numericInput(ns("n1_ic"), "Tamaño de muestra 1 (n₁)", 30, min = 2, step = 1),
                  numericInput(ns("s22_ic"), "Varianza muestral 2 (s₂²)", 80, min = 0.000001),
                  numericInput(ns("n2_ic"), "Tamaño de muestra 2 (n₂)", 25, min = 2, step = 1),
                  numericInput(ns("nivel_f_ic"), "Nivel de confianza (%)", 95, min = 0.01, max = 99.99),
                  actionButton(ns("calc_f_ic"), "Calcular", class = "btn-primary")
                )
              ),
              column(
                width = 4,
                wellPanel(
                  h4("Fórmula"),
                  HTML("$$F=\\frac{S_1^2}{S_2^2}$$"),
                  HTML("$$IC\\left(\\frac{\\sigma_1^2}{\\sigma_2^2}\\right)=\\left(\\frac{s_1^2/s_2^2}{F_{1-\\alpha/2}},\\frac{s_1^2/s_2^2}{F_{\\alpha/2}}\\right)$$"),
                  hr(),
                  h4("Resultados"),
                  verbatimTextOutput(ns("res_f_ic"))
                )
              ),
              column(
                width = 4,
                wellPanel(
                  h4("Gráfica"),
                  plotOutput(ns("plot_f_ic"), height = "420px")
                )
              )
            )
          ),

          # ---------------- PRUEBA DE HIPÓTESIS ---------------
          tabPanel(
            "Prueba de hipótesis",
            br(),
            fluidRow(
              column(
                width = 4,
                wellPanel(
                  h4("Datos"),
                  numericInput(ns("s21_ph"), "Varianza muestral 1 (s₁²)", 100, min = 0.000001),
                  numericInput(ns("n1_ph"), "Tamaño de muestra 1 (n₁)", 30, min = 2, step = 1),
                  numericInput(ns("s22_ph"), "Varianza muestral 2 (s₂²)", 80, min = 0.000001),
                  numericInput(ns("n2_ph"), "Tamaño de muestra 2 (n₂)", 25, min = 2, step = 1),
                  numericInput(ns("ratio0_ph"), "Cociente bajo H₀ (σ₁²/σ₂²)", 1, min = 0.000001),
                  selectInput(
                    ns("tipo_f_ph"), "Tipo de prueba",
                    choices = c("Bilateral" = "two.sided", "Cola derecha" = "greater", "Cola izquierda" = "less")
                  ),
                  numericInput(ns("alpha_f_ph"), "Nivel de significancia (α)", 0.05, min = 0.0001, max = 0.5),
                  actionButton(ns("calc_f_ph"), "Calcular", class = "btn-primary")
                )
              ),
              column(
                width = 4,
                wellPanel(
                  h4("Fórmula"),
                  HTML("$$F=\\frac{(s_1^2/s_2^2)}{\\sigma_{10}^2/\\sigma_{20}^2}$$"),
                  hr(),
                  h4("Resultados"),
                  verbatimTextOutput(ns("res_f_ph"))
                )
              ),
              column(
                width = 4,
                wellPanel(
                  h4("Gráfica"),
                  plotOutput(ns("plot_f_ph"), height = "420px")
                )
              )
            )
          )
        )
      )
    )
  )
}

varianza_cociente_server <- function(input, output, session) {

  # ==========================================================
  # UNA VARIANZA - INTERVALO
  # ==========================================================
  ic_var <- eventReactive(input$calc_ic, {
    validate(
      need(is.finite(input$s2_ic) && input$s2_ic > 0, "s² debe ser mayor que 0."),
      need(is.finite(input$n_ic) && input$n_ic >= 2, "n debe ser al menos 2."),
      need(input$nivel_ic > 0 && input$nivel_ic < 100, "El nivel de confianza debe estar entre 0 y 100.")
    )
    n <- input$n_ic
    df <- n - 1
    alpha <- 1 - input$nivel_ic/100
    s2 <- input$s2_ic
    q_inf <- qchisq(1 - alpha/2, df)
    q_sup <- qchisq(alpha/2, df)
    li <- df*s2/q_inf
    ls <- df*s2/q_sup
    list(n=n, df=df, s2=s2, alpha=alpha, conf=input$nivel_ic,
         q_inf=q_inf, q_sup=q_sup, li=li, ls=ls)
  })

  output$res_ic <- renderPrint({
    r <- ic_var()
    cat("=====================================\n")
    cat(" Intervalo de confianza para σ²\n")
    cat("=====================================\n")
    cat("Nivel de confianza:", r$conf, "%\n")
    cat("n =", r$n, "\n")
    cat("Grados de libertad:", r$df, "\n")
    cat("s² =", round(r$s2, 6), "\n\n")
    cat("χ² superior =", round(r$q_inf, 6), "\n")
    cat("χ² inferior =", round(r$q_sup, 6), "\n\n")
    cat("Intervalo de confianza para σ²:\n")
    cat("[", round(r$li, 6), ", ", round(r$ls, 6), "]\n", sep="")
  })

  output$plot_ic <- renderPlot({
    r <- ic_var()
    xmax <- max(qchisq(0.999, r$df), r$q_inf) * 1.05
    x <- seq(0, xmax, length.out=1500)
    y <- dchisq(x, r$df)
    d <- data.frame(x=x,y=y)
    ggplot(d, aes(x,y)) +
      geom_line(linewidth=1.1) +
      geom_area(data=subset(d, x <= r$q_sup), fill="steelblue", alpha=.30) +
      geom_area(data=subset(d, x >= r$q_inf), fill="steelblue", alpha=.30) +
      geom_vline(xintercept=c(r$q_sup,r$q_inf), linetype="dashed", linewidth=1) +
      theme_minimal() +
      labs(title="Distribución χ²", subtitle="Regiones asociadas al intervalo de confianza",
           x="χ²", y="Densidad")
  })

  # ==========================================================
  # UNA VARIANZA - PRUEBA
  # ==========================================================
  ph_var <- eventReactive(input$calc_ph, {
    validate(
      need(input$s2_ph > 0, "s² debe ser mayor que 0."),
      need(input$n_ph >= 2, "n debe ser al menos 2."),
      need(input$sigma20_ph > 0, "σ₀² debe ser mayor que 0."),
      need(input$alpha_ph > 0 && input$alpha_ph < 1, "α debe estar entre 0 y 1.")
    )
    df <- input$n_ph - 1
    estad <- df*input$s2_ph/input$sigma20_ph
    alpha <- input$alpha_ph
    tipo <- input$tipo_ph
    if(tipo == "two.sided") {
      crit_inf <- qchisq(alpha/2, df)
      crit_sup <- qchisq(1-alpha/2, df)
      pval <- 2*min(pchisq(estad,df), 1-pchisq(estad,df))
      region <- paste0("χ² < ", round(crit_inf,6), " o χ² > ", round(crit_sup,6))
    } else if(tipo == "greater") {
      crit_inf <- NA
      crit_sup <- qchisq(1-alpha, df)
      pval <- 1-pchisq(estad,df)
      region <- paste0("χ² > ", round(crit_sup,6))
    } else {
      crit_inf <- qchisq(alpha, df)
      crit_sup <- NA
      pval <- pchisq(estad,df)
      region <- paste0("χ² < ", round(crit_inf,6))
    }
    decision <- if(pval < alpha) "Se rechaza H₀." else "No se rechaza H₀."
    list(df=df, estad=estad, alpha=alpha, tipo=tipo, crit_inf=crit_inf,
         crit_sup=crit_sup, pval=pval, region=region, decision=decision)
  })

  output$res_ph <- renderPrint({
    r <- ph_var()
    cat("=====================================\n")
    cat(" Prueba de hipótesis para σ²\n")
    cat("=====================================\n")
    cat("H₀: σ² = σ₀²\n")
    cat("Hₐ: ", switch(r$tipo, two.sided="σ² ≠ σ₀²", greater="σ² > σ₀²", less="σ² < σ₀²"), "\n\n", sep="")
    cat("Nivel de significancia (α):", r$alpha, "\n")
    cat("Grados de libertad:", r$df, "\n")
    if(!is.na(r$crit_inf)) cat("Valor crítico inferior:", round(r$crit_inf,6), "\n")
    if(!is.na(r$crit_sup)) cat("Valor crítico superior:", round(r$crit_sup,6), "\n")
    cat("Región de rechazo:", r$region, "\n")
    cat("Estadístico χ² =", round(r$estad,6), "\n")
    cat("Valor p =", format.pval(r$pval, digits=6), "\n")
    cat("Decisión:", r$decision, "\n")
  })

  output$plot_ph <- renderPlot({
    r <- ph_var()
    xmax <- max(qchisq(.999, r$df), r$estad, r$crit_sup, na.rm=TRUE) * 1.08
    x <- seq(0,xmax,length.out=1800)
    d <- data.frame(x=x,y=dchisq(x,r$df))
    g <- ggplot(d,aes(x,y)) + geom_line(linewidth=1.1) + theme_minimal() +
      labs(title="Prueba para una varianza", subtitle=paste("Distribución χ² con",r$df,"gl"), x="χ²", y="Densidad")
    if(r$tipo=="two.sided") {
      g <- g + geom_area(data=subset(d,x<=r$crit_inf),fill="steelblue",alpha=.35) +
        geom_area(data=subset(d,x>=r$crit_sup),fill="steelblue",alpha=.35)
    } else if(r$tipo=="greater") {
      g <- g + geom_area(data=subset(d,x>=r$crit_sup),fill="steelblue",alpha=.35)
    } else {
      g <- g + geom_area(data=subset(d,x<=r$crit_inf),fill="steelblue",alpha=.35)
    }
    g + geom_vline(xintercept=r$estad,color="darkgreen",linetype="dashed",linewidth=1.2)
  })

  # ==========================================================
  # COCIENTE - INTERVALO
  # ==========================================================
  ic_f <- eventReactive(input$calc_f_ic, {
    validate(
      need(input$s21_ic > 0 && input$s22_ic > 0, "Las varianzas muestrales deben ser mayores que 0."),
      need(input$n1_ic >= 2 && input$n2_ic >= 2, "Cada tamaño de muestra debe ser al menos 2."),
      need(input$nivel_f_ic > 0 && input$nivel_f_ic < 100, "El nivel de confianza debe estar entre 0 y 100.")
    )
    df1 <- input$n1_ic-1; df2 <- input$n2_ic-1
    alpha <- 1-input$nivel_f_ic/100
    ratio <- input$s21_ic/input$s22_ic
    f_inf <- qf(alpha/2,df1,df2)
    f_sup <- qf(1-alpha/2,df1,df2)
    li <- ratio/f_sup
    ls <- ratio/f_inf
    list(df1=df1,df2=df2,ratio=ratio,alpha=alpha,conf=input$nivel_f_ic,
         f_inf=f_inf,f_sup=f_sup,li=li,ls=ls,
         sd_li=sqrt(li),sd_ls=sqrt(ls))
  })

  output$res_f_ic <- renderPrint({
    r <- ic_f()
    cat("=====================================\n")
    cat(" IC para el cociente de varianzas\n")
    cat("=====================================\n")
    cat("Nivel de confianza:", r$conf, "%\n")
    cat("gl₁ =",r$df1,"   gl₂ =",r$df2,"\n")
    cat("s₁²/s₂² =",round(r$ratio,6),"\n\n")
    cat("F crítico inferior =",round(r$f_inf,6),"\n")
    cat("F crítico superior =",round(r$f_sup,6),"\n\n")
    cat("IC para σ₁²/σ₂²:\n[",round(r$li,6),", ",round(r$ls,6),"]\n",sep="")
    cat("IC para σ₁/σ₂:\n[",round(r$sd_li,6),", ",round(r$sd_ls,6),"]\n",sep="")
  })

  output$plot_f_ic <- renderPlot({
    r <- ic_f()
    # La gráfica muestra el intervalo del parámetro σ₁²/σ₂², no valores F.
    xmax <- max(r$ls*1.25, r$ratio*1.25, 1)
    xmin <- max(0, min(r$li*.75, r$ratio*.75))
    x <- seq(xmin,xmax,length.out=1000)
    y <- dnorm(x,r$ratio,max(r$ratio*.12,0.05))
    d <- data.frame(x=x,y=y)
    ggplot(d,aes(x,y)) + geom_line(linewidth=1.1) +
      geom_area(data=subset(d,x>=r$li & x<=r$ls),fill="steelblue",alpha=.35) +
      geom_vline(xintercept=c(r$li,r$ls),linetype="dashed",linewidth=1) +
      geom_vline(xintercept=r$ratio,color="darkgreen",linetype="dashed",linewidth=1.1) +
      theme_minimal() +
      labs(title="Intervalo para σ₁²/σ₂²", subtitle="La gráfica representa el parámetro, no la distribución F",
           x="σ₁²/σ₂²",y="Escala relativa")
  })

  # ==========================================================
  # COCIENTE - PRUEBA
  # ==========================================================
  ph_f <- eventReactive(input$calc_f_ph, {
    validate(
      need(input$s21_ph > 0 && input$s22_ph > 0, "Las varianzas muestrales deben ser mayores que 0."),
      need(input$n1_ph >= 2 && input$n2_ph >= 2, "Cada tamaño de muestra debe ser al menos 2."),
      need(input$ratio0_ph > 0, "El cociente bajo H₀ debe ser mayor que 0."),
      need(input$alpha_f_ph > 0 && input$alpha_f_ph < 1, "α debe estar entre 0 y 1.")
    )
    df1 <- input$n1_ph-1; df2 <- input$n2_ph-1
    alpha <- input$alpha_f_ph
    ratio <- input$s21_ph/input$s22_ph
    fobs <- ratio/input$ratio0_ph
    tipo <- input$tipo_f_ph
    if(tipo=="two.sided") {
      crit_inf <- qf(alpha/2,df1,df2); crit_sup <- qf(1-alpha/2,df1,df2)
      pval <- 2*min(pf(fobs,df1,df2),1-pf(fobs,df1,df2))
      region <- paste0("F < ",round(crit_inf,6)," o F > ",round(crit_sup,6))
    } else if(tipo=="greater") {
      crit_inf <- NA; crit_sup <- qf(1-alpha,df1,df2)
      pval <- 1-pf(fobs,df1,df2)
      region <- paste0("F > ",round(crit_sup,6))
    } else {
      crit_inf <- qf(alpha,df1,df2); crit_sup <- NA
      pval <- pf(fobs,df1,df2)
      region <- paste0("F < ",round(crit_inf,6))
    }
    decision <- if(pval < alpha) "Se rechaza H₀." else "No se rechaza H₀."
    list(df1=df1,df2=df2,ratio=ratio,ratio0=input$ratio0_ph,fobs=fobs,alpha=alpha,
         tipo=tipo,crit_inf=crit_inf,crit_sup=crit_sup,pval=pval,region=region,decision=decision)
  })

  output$res_f_ph <- renderPrint({
    r <- ph_f()
    cat("=====================================\n")
    cat(" Prueba F para el cociente de varianzas\n")
    cat("=====================================\n")
    cat("H₀: σ₁²/σ₂² =",r$ratio0,"\n")
    cat("Hₐ: ",switch(r$tipo,two.sided="σ₁²/σ₂² ≠ σ₁₀²/σ₂₀²",greater="σ₁²/σ₂² > σ₁₀²/σ₂₀²",less="σ₁²/σ₂² < σ₁₀²/σ₂₀²"),"\n\n",sep="")
    cat("Nivel de significancia (α):",r$alpha,"\n")
    cat("gl₁ =",r$df1,"   gl₂ =",r$df2,"\n")
    if(!is.na(r$crit_inf)) cat("Valor crítico inferior:",round(r$crit_inf,6),"\n")
    if(!is.na(r$crit_sup)) cat("Valor crítico superior:",round(r$crit_sup,6),"\n")
    cat("Región de rechazo:",r$region,"\n")
    cat("Cociente observado s₁²/s₂² =",round(r$ratio,6),"\n")
    cat("Estadístico F =",round(r$fobs,6),"\n")
    cat("Valor p =",format.pval(r$pval,digits=6),"\n")
    cat("Decisión:",r$decision,"\n")
  })

  output$plot_f_ph <- renderPlot({
    r <- ph_f()
    xmax <- max(qf(.999,r$df1,r$df2),r$fobs,r$crit_sup,na.rm=TRUE)*1.08
    xmin <- 0
    x <- seq(xmin,xmax,length.out=1800)
    d <- data.frame(x=x,y=df(x,r$df1,r$df2))
    g <- ggplot(d,aes(x,y))+geom_line(linewidth=1.1)+theme_minimal()+
      labs(title="Prueba F para el cociente de varianzas",
           subtitle=paste("gl₁ =",r$df1,"| gl₂ =",r$df2),x="F",y="Densidad")
    if(r$tipo=="two.sided") {
      g <- g+geom_area(data=subset(d,x<=r$crit_inf),fill="steelblue",alpha=.35)+
        geom_area(data=subset(d,x>=r$crit_sup),fill="steelblue",alpha=.35)
    } else if(r$tipo=="greater") {
      g <- g+geom_area(data=subset(d,x>=r$crit_sup),fill="steelblue",alpha=.35)
    } else {
      g <- g+geom_area(data=subset(d,x<=r$crit_inf),fill="steelblue",alpha=.35)
    }
    g+geom_vline(xintercept=r$fobs,color="darkgreen",linetype="dashed",linewidth=1.2)
  })
}


server <- function(input, output, session) {

  estado <- reactiveVal(list(pagina="inicio",categoria=NULL,aplicacion=NULL))

  # Registrar la lógica de las aplicaciones integradas una sola vez.
  media_server(input, output, session)
  callModule(distribuciones_server, "dist_app")
  callModule(medias_conocidas_server, "medias_conocidas_app")
  callModule(medias_desconocidas_server, "medias_desconocidas_app")
  callModule(mann_whitney_server, "mann_whitney_app")
  callModule(pareadas_server, "pareadas_app")
  callModule(wilcoxon_server, "wilcoxon_app")
  callModule(anova_server, "anova_app")
  callModule(proporcion_server, "proporcion_app")
  callModule(diferencia_proporciones_server, "diferencia_proporciones_app")
  callModule(diferencia_proporciones_iguales_server, "diferencia_proporciones_iguales_app")
  callModule(prueba_no_parametrica_var_server, "prueba_no_parametrica_var_app")
  callModule(varianza_cociente_server, "varianza_cociente_app")
  exploratorio_server(input, output, session)

  output$contenido <- renderUI({
    e <- estado()
    if(e$pagina=="inicio") {
      tarjetas <- lapply(categorias,function(cat) column(width=4,tarjeta_categoria(cat$id,cat)))
      return(tagList(fluidRow(tarjetas[[1]],tarjetas[[2]],tarjetas[[3]]),fluidRow(tarjetas[[4]],tarjetas[[5]],tarjetas[[6]]),fluidRow(tarjetas[[7]],tarjetas[[8]],tarjetas[[9]])))
    }
    if(e$pagina=="categoria") {
      categoria <- categorias[[which(vapply(categorias,function(x) x$id==e$categoria,logical(1)))[1]]] 
      tarjetas <- lapply(categoria$aplicaciones,function(app) column(width=ifelse(length(categoria$aplicaciones)==1,8,6),tarjeta_aplicacion(paste0(categoria$id,"_",app$id),app)))
      return(tagList(actionLink("volver_inicio","‹ Volver a categorías",class="boton-regresar"),h2(class="subtitulo-principal",paste(categoria$icono,categoria$nombre)),p(class="texto-bienvenida",categoria$descripcion),do.call(fluidRow,tarjetas)))
    }
    if(e$pagina=="app") {
      categoria <- categorias[[which(vapply(categorias,function(x) x$id==e$categoria,logical(1)))[1]]] 
      aplicacion <- categoria$aplicaciones[[which(vapply(categoria$aplicaciones,function(x) x$id==e$aplicacion,logical(1)))[1]]] 
      if(e$categoria=="medias_ind" && e$aplicacion=="media") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),media_ui()))
      }
      if(e$categoria=="dist" && e$aplicacion=="distribuciones") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),distribuciones_ui(ns = NS("dist_app"))))
      }
      if(e$categoria=="medias_ind" && e$aplicacion=="medias_conocidas") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),medias_conocidas_ui(ns = NS("medias_conocidas_app"))))
      }
      if(e$categoria=="medias_ind" && e$aplicacion=="medias_desconocidas") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),medias_desconocidas_ui(ns = NS("medias_desconocidas_app"))))
      }
      if(e$categoria=="exploratorio" && e$aplicacion=="exploratorio") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),exploratorio_ui()))
      }
      if(e$categoria=="medias_ind" && e$aplicacion=="mann_whitney") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),mann_whitney_ui(ns = NS("mann_whitney_app"))))
      }
      if(e$categoria=="medias_dep" && e$aplicacion=="pareadas") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),pareadas_ui(ns = NS("pareadas_app"))))
      }
      if(e$categoria=="medias_dep" && e$aplicacion=="wilcoxon") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),wilcoxon_ui(ns = NS("wilcoxon_app"))))
      }
      if(e$categoria=="proporciones" && e$aplicacion=="proporcion") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),proporcion_ui(ns = NS("proporcion_app"))))
      }
      if(e$categoria=="proporciones" && e$aplicacion=="diferencia_proporciones") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),
                       diferencia_proporciones_ui(ns = NS("diferencia_proporciones_app"))))
      }
      if(e$categoria=="proporciones" && e$aplicacion=="diferencia_proporciones_iguales") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),
                       diferencia_proporciones_iguales_ui(ns = NS("diferencia_proporciones_iguales_app"))))
      }
      if(e$categoria=="varianzas" && e$aplicacion=="prueba_no_parametrica_var") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),
                       prueba_no_parametrica_var_ui(ns = NS("prueba_no_parametrica_var_app"))))
      }
      if(e$categoria=="varianzas" && e$aplicacion=="varianza_cociente") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),
                       varianza_cociente_ui(ns = NS("varianza_cociente_app"))))
      }
      if(e$categoria=="anova" && e$aplicacion=="anova") {
        return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),anova_ui(ns = NS("anova_app"))))
      }
      return(tagList(actionLink("volver_categoria","‹ Volver a herramientas",class="boton-regresar"),h2(class="subtitulo-principal",aplicacion$nombre),div(class="placeholder-app",h3("Herramienta interactiva"),p(paste("La aplicación",shQuote(aplicacion$nombre),"se integrará aquí.")),p("Esta pantalla es temporal. Una vez confirmada la aplicación, se incorporará su código original al Launcher."))))
    }
  })

  lapply(categorias,function(cat) {
    observeEvent(input[[paste0("categoria_",cat$id)]],{estado(list(pagina="categoria",categoria=cat$id,aplicacion=NULL))},ignoreInit=TRUE)
    lapply(cat$aplicaciones,function(app) {
      observeEvent(input[[paste0("app_",cat$id,"_",app$id)]],{estado(list(pagina="app",categoria=cat$id,aplicacion=app$id))},ignoreInit=TRUE)
      observeEvent(input[[paste0("sidebar_app_",cat$id,"_",app$id)]],{estado(list(pagina="app",categoria=cat$id,aplicacion=app$id))},ignoreInit=TRUE)
    })
  })

  observeEvent(input$inicio_sidebar,{estado(list(pagina="inicio",categoria=NULL,aplicacion=NULL))},ignoreInit=TRUE)
  observeEvent(input$volver_inicio,{estado(list(pagina="inicio",categoria=NULL,aplicacion=NULL))},ignoreInit=TRUE)
  observeEvent(input$volver_categoria,{e<-estado();estado(list(pagina="categoria",categoria=e$categoria,aplicacion=NULL))},ignoreInit=TRUE)
}

shinyApp(ui,server)
