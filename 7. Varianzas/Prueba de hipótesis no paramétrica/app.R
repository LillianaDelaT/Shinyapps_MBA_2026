# ============================================================
# PRUEBA NO PARAMÉTRICA PARA COMPARAR LA DISPERSIÓN
# Mann-Whitney aplicado a desviaciones absolutas
# ============================================================

# ------------------------------------------------------------
# 1. VERIFICACIÓN E INSTALACIÓN AUTOMÁTICA DE PAQUETES
# ------------------------------------------------------------

paquetes <- c(
  "shiny",
  "readxl",
  "openxlsx",
  "shinythemes"
)

paquetes_faltantes <- paquetes[
  !(paquetes %in% rownames(installed.packages()))
]

if (length(paquetes_faltantes) > 0) {
  
  message(
    "Se instalarán los siguientes paquetes: ",
    paste(paquetes_faltantes, collapse = ", ")
  )
  
  tryCatch({
    
    install.packages(
      paquetes_faltantes,
      dependencies = TRUE,
      repos = "https://cloud.r-project.org"
    )
    
  }, error = function(e) {
    
    stop(
      paste0(
        "No fue posible instalar los paquetes faltantes.\n",
        "Verifique su conexión a Internet y los permisos de instalación.\n\n",
        "Error: ",
        e$message
      )
    )
    
  })
}

# Verificación posterior
paquetes_no_instalados <- paquetes[
  !(paquetes %in% rownames(installed.packages()))
]

if (length(paquetes_no_instalados) > 0) {
  
  stop(
    paste0(
      "Los siguientes paquetes no pudieron instalarse: ",
      paste(paquetes_no_instalados, collapse = ", ")
    )
  )
}

# Cargar paquetes
invisible(
  lapply(
    paquetes,
    library,
    character.only = TRUE
  )
)


# ============================================================
# 2. FUNCIONES AUXILIARES
# ============================================================

# ------------------------------------------------------------
# Validación de los datos
# ------------------------------------------------------------

validar_datos <- function(df) {
  
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

obtener_hipotesis <- function(alternativa) {
  
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

calcular_prueba <- function(x, y, alternativa, alpha) {
  
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

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
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
          "archivo",
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
          "alternativa",
          "Hipótesis alternativa:",
          
          choices = c(
            "X tiene mayor dispersión que Y" = "mayor",
            "X tiene menor dispersión que Y" = "menor",
            "La dispersión de X y Y es diferente" = "diferente"
          ),
          
          selected = "diferente"
        ),
        
        numericInput(
          "alpha",
          "Nivel de significancia (α):",
          value = 0.05,
          min = 0.001,
          max = 0.20,
          step = 0.01
        ),
        
        actionButton(
          "calcular",
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
          
          tableOutput("tabla_datos")
        ),
        
        
        # ======================================================
        # TAB 3: RESULTADOS
        # ======================================================
        
        tabPanel(
          
          "Resultados",
          
          br(),
          
          h4("Estadísticos descriptivos"),
          
          tableOutput("tabla_descriptivos"),
          
          br(),
          
          h4("Hipótesis"),
          
          verbatimTextOutput("hipotesis"),
          
          br(),
          
          h4("Estadístico de prueba"),
          
          tableOutput("tabla_estadistico"),
          
          br(),
          
          h4("Decisión y conclusión"),
          
          verbatimTextOutput("decision"),
          
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
            "grafica",
            height = "600px"
          )
        )
      )
    )
  )
)


# ============================================================
# 4. SERVIDOR
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Lectura del archivo
  # ----------------------------------------------------------
  
  datos <- reactive({
    
    req(input$archivo)
    
    df <- read_excel(
      input$archivo$datapath
    )
    
    error <- validar_datos(df)
    
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
      
      alternativa <- input$alternativa
      
      x <- df[[1]]
      y <- df[[2]]
      
      tryCatch(
        
        {
          
          calcular_prueba(
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
    
    h <- obtener_hipotesis(
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
# 5. EJECUTAR APLICACIÓN
# ============================================================

shinyApp(
  ui = ui,
  server = server
)