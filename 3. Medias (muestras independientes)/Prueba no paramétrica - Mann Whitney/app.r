# ==========================================================
# PRUEBA MANN WHITNEY
# ==========================================================

# ==========================================================
# GESTIÓN Y CARGA AUTOMÁTICA DE PAQUETES
# ==========================================================
paquetes_requeridos <- c("shiny", "DT", "readxl")

cargar_o_instalar <- function(paquete) {
  if (!require(paquete, character.only = TRUE)) {
    message(paste("Instalando paquete faltante:", paquete))
    install.packages(paquete, dependencies = TRUE)
    library(paquete, character.only = TRUE)
  }
}

invisible(lapply(paquetes_requeridos, cargar_o_instalar))

# ==========================================================
# FUNCIONES AUXILIARES
# ==========================================================
obtener_hipotesis <- function(alternativa, g1 = "X", g2 = "Y"){
  if(alternativa == "two.sided"){
    return(list(
      H0 = paste0("H₀: Las distribuciones de ", g1, " y ", g2, " son iguales"),
      Ha = paste0("Hₐ: Las distribuciones de ", g1, " y ", g2, " son diferentes")
    ))
  }
  if(alternativa == "greater"){
    return(list(
      H0 = paste0("H₀: La distribución de ", g1, " no es mayor que la de ", g2),
      Ha = paste0("Hₐ: La distribución de ", g1, " tiende a valores mayores que ", g2)
    ))
  }
  if(alternativa == "less"){
    return(list(
      H0 = paste0("H₀: La distribución de ", g1, " no es menor que la de ", g2),
      Ha = paste0("Hₐ: La distribución de ", g1, " tiende a valores menores que ", g2)
    ))
  }
}

# ==========================================================
# UI
# ==========================================================
ui <- fluidPage(
  
  titlePanel(
    div(
      "Prueba no paramétrica de Mann-Whitney (Wilcoxon Rank-Sum)",
      tags$small(
        style = "display:block; font-style:italic; font-size:40%;",
        "App desarrollada por Liliana De la Torre Desentis"
      )
    )
  ),
  
  tabsetPanel(
    
    # --- PESTAÑA 1 ---
    tabPanel(
      "Prueba Mann-Whitney",
      sidebarLayout(
        sidebarPanel(
          textInput("muestraX", "Muestra X:", value = "12,15,14,10,9"),
          textInput("muestraY", "Muestra Y:", value = "8,7,6,11,13"),
          selectInput("alternativa", "Tipo de prueba (Hₐ):",
                      choices = c("Dos colas" = "two.sided", "X > Y" = "greater", "X < Y" = "less")),
          numericInput("alpha1", "Nivel de significancia (α):", value = 0.05, min = 0.001, max = 0.999, step = 0.01),
          actionButton("calcular1", "Calcular", class = "btn-primary")
        ),
        mainPanel(
          h3("Hipótesis"),
          verbatimTextOutput("hipotesis1"),
          h3("Estadísticos"),
          verbatimTextOutput("estadisticos1"),
          h3("Resultados de la prueba"),
          verbatimTextOutput("resultado1"),
          h3("Tabla de rangos"),
          div(style = "max-width:500px;", DTOutput("tabla_rangos1")),
          h3("Gráfico"),
          plotOutput("grafico1", height = "400px")
        )
      )
    ),
    
    # --- PESTAÑA 2 ---
    tabPanel(
      "Prueba Mann-Whitney (Aproximación Normal)",
      sidebarLayout(
        sidebarPanel(
          textInput("grupo1", "Grupo 1:", "6,8,8,10,10,10,11,11,12,12,12,12,13,13,13,14,14,14,15,15,15,16,17"),
          textInput("grupo2", "Grupo 2:", "6,7,7,7,7,7,10,10,10,10,12,12,12,13,13,13"),
          selectInput("hipotesis2", "Tipo de prueba (Hₐ):",
                      choices = c("Bilateral" = "two.sided", "Grupo 1 > Grupo 2" = "greater", "Grupo 1 < Grupo 2" = "less")),
          numericInput("alpha2", "Nivel de significancia (α):", value = 0.05),
          actionButton("calcular2", "Calcular", class = "btn-primary")
        ),
        mainPanel(
          h3("Hipótesis"),
          verbatimTextOutput("hipotesis2_out"),
          h3("Resultados de la prueba"),
          verbatimTextOutput("resultado2"),
          h3("Tabla de rangos"),
          div(style = "max-width:500px;", DTOutput("tabla_rangos2")),
          h3("Gráfico"),
          plotOutput("boxplot2", height = "400px")
        )
      )
    ),
    
    # --- PESTAÑA 3 ---
    tabPanel(
      "Prueba Mann-Whitney con Excel",
      sidebarLayout(
        sidebarPanel(
          fileInput("archivo", "Sube archivo Excel (.xlsx, .xls)", accept = c(".xlsx", ".xls")),
          selectInput("hipotesis3", "Tipo de prueba (Hₐ):",
                      choices = c("Dos colas" = "two.sided", "Columna 1 > Columna 2" = "greater", "Columna 1 < Columna 2" = "less")),
          numericInput("alpha3", "Nivel de significancia (α):", value = 0.05),
          actionButton("calcular3", "Calcular", class = "btn-primary")
        ),
        mainPanel(
          h3("Hipótesis"),
          verbatimTextOutput("hipotesis3_out"),
          h3("Estadísticos"),
          verbatimTextOutput("estadisticos3"),
          h3("Resultados de la prueba"),
          verbatimTextOutput("resultado3"),
          h3("Tabla de rangos"),
          div(style = "max-width:500px;", DTOutput("tabla_rangos3")),
          h3("Gráfico"),
          plotOutput("grafico3", height = "400px")
        )
      )
    )
  )
)

# ==========================================================
# SERVER
# ==========================================================
server <- function(input, output, session){
  
  colores_boxplot <- c("#AEC6CF", "#FFB3BA")
  
  # --- PESTAÑA 1 ---
  observeEvent(input$calcular1, {
    x <- as.numeric(trimws(unlist(strsplit(input$muestraX, ","))))
    y <- as.numeric(trimws(unlist(strsplit(input$muestraY, ","))))
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    
    datos <- data.frame(
      Valor = c(x, y),
      Grupo = factor(rep(c("X", "Y"), times = c(length(x), length(y))))
    )
    datos$Rango <- rank(datos$Valor, ties.method = "average")
    datos <- datos[order(datos$Valor), ]
    
    RX <- sum(datos$Rango[datos$Grupo == "X"])
    RY <- sum(datos$Rango[datos$Grupo == "Y"])
    
    UX <- RX - (length(x) * (length(x) + 1)) / 2
    UY <- RY - (length(y) * (length(y) + 1)) / 2
    
    prueba <- wilcox.test(x, y, alternative = input$alternativa, exact = TRUE)
    alpha <- input$alpha1
    hip <- obtener_hipotesis(input$alternativa, "X", "Y")
    
    output$hipotesis1 <- renderPrint({
      cat(hip$H0, "\n")
      cat(hip$Ha)
    })
    
    output$estadisticos1 <- renderPrint({
      cat("nX =", length(x), "| nY =", length(y), "\n")
      cat("Suma de Rangos: R_X =", round(RX, 4), "| R_Y =", round(RY, 4), "\n")
      cat("Estadísticos U:  U_X =", round(UX, 4), "| U_Y =", round(UY, 4), "\n")
    })
    
    output$resultado1 <- renderPrint({
      cat("Estadístico W de R (U_X) =", prueba$statistic, "\n")
      cat("Valor-p =", round(prueba$p.value, 5), "\n\n")
      
      if(prueba$p.value < alpha){
        cat("Decisión: Se rechaza H₀ (p-valor < α)\n")
        cat("Existe evidencia significativa para apoyar la hipótesis alternativa.")
      } else {
        cat("Decisión: No se rechaza H₀ (p-valor ≥ α)\n")
        cat("No hay evidencia suficiente para rechazar la hipótesis nula.")
      }
    })
    
    output$tabla_rangos1 <- renderDT({
      datatable(datos, rownames = FALSE, options = list(pageLength = 10, dom = 'tip'))
    })
    
    output$grafico1 <- renderPlot({
      boxplot(Valor ~ Grupo, data = datos, col = colores_boxplot, main = "Comparación de Muestras", ylab = "Valores")
      stripchart(Valor ~ Grupo, data = datos, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col = "darkblue")
    })
  })
  
  # --- PESTAÑA 2 ---
  observeEvent(input$calcular2, {
    grupo1 <- as.numeric(trimws(unlist(strsplit(input$grupo1, ","))))
    grupo2 <- as.numeric(trimws(unlist(strsplit(input$grupo2, ","))))
    grupo1 <- grupo1[!is.na(grupo1)]
    grupo2 <- grupo2[!is.na(grupo2)]
    
    datos <- data.frame(
      Valor = c(grupo1, grupo2),
      Grupo = factor(c(rep("Grupo 1", length(grupo1)), rep("Grupo 2", length(grupo2))))
    )
    datos$Rango <- rank(datos$Valor, ties.method = "average")
    datos <- datos[order(datos$Valor), ]
    
    prueba <- wilcox.test(grupo1, grupo2, alternative = input$hipotesis2, exact = FALSE)
    alpha <- input$alpha2
    hip <- obtener_hipotesis(input$hipotesis2, "Grupo 1", "Grupo 2")
    
    output$hipotesis2_out <- renderPrint({
      cat(hip$H0, "\n")
      cat(hip$Ha)
    })
    
    output$resultado2 <- renderPrint({
      cat("Estadístico W de R =", prueba$statistic, "\n")
      cat("Valor-p =", round(prueba$p.value, 5), "\n\n")
      
      if(prueba$p.value < alpha){
        cat("Decisión: Se rechaza H₀ (p-valor < α)")
      } else {
        cat("Decisión: No se rechaza H₀ (p-valor ≥ α)")
      }
    })
    
    output$tabla_rangos2 <- renderDT({
      datatable(datos, rownames = FALSE, options = list(pageLength = 10, dom = 'tip'))
    })
    
    output$boxplot2 <- renderPlot({
      boxplot(Valor ~ Grupo, data = datos, col = colores_boxplot, main = "Comparación entre Grupos")
    })
  })
  
  # --- PESTAÑA 3 ---
  observeEvent(input$calcular3, {
    req(input$archivo)
    
    datos_excel <- read_excel(input$archivo$datapath)
    
    if (ncol(datos_excel) < 2) {
      output$resultado3 <- renderPrint({ cat("Error: El archivo debe contener al menos 2 columnas.") })
      return()
    }
    
    grupo1 <- na.omit(as.numeric(datos_excel[[1]]))
    grupo2 <- na.omit(as.numeric(datos_excel[[2]]))
    
    nombre1 <- colnames(datos_excel)[1]
    nombre2 <- colnames(datos_excel)[2]
    
    datos <- data.frame(
      Valor = c(grupo1, grupo2),
      Grupo = factor(c(rep(nombre1, length(grupo1)), rep(nombre2, length(grupo2))))
    )
    datos$Rango <- rank(datos$Valor, ties.method = "average")
    datos <- datos[order(datos$Valor), ]
    
    prueba <- wilcox.test(grupo1, grupo2, alternative = input$hipotesis3)
    alpha <- input$alpha3
    hip <- obtener_hipotesis(input$hipotesis3, nombre1, nombre2)
    
    output$hipotesis3_out <- renderPrint({
      cat(hip$H0, "\n")
      cat(hip$Ha)
    })
    
    output$estadisticos3 <- renderPrint({
      cat("Muestra 1:", nombre1, "(n =", length(grupo1), ")\n")
      cat("Muestra 2:", nombre2, "(n =", length(grupo2), ")\n")
    })
    
    output$resultado3 <- renderPrint({
      cat("Estadístico W de R =", prueba$statistic, "\n")
      cat("Valor-p =", round(prueba$p.value, 5), "\n\n")
      
      if(prueba$p.value < alpha){
        cat("Decisión: Se rechaza H₀ (p-valor < α)")
      } else {
        cat("Decisión: No se rechaza H₀ (p-valor ≥ α)")
      }
    })
    
    output$tabla_rangos3 <- renderDT({
      datatable(datos, rownames = FALSE, options = list(pageLength = 10, dom = 'tip'))
    })
    
    output$grafico3 <- renderPlot({
      boxplot(Valor ~ Grupo, data = datos, col = colores_boxplot, main = "Comparación desde Excel")
      stripchart(Valor ~ Grupo, data = datos, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col = "darkblue")
    })
  })
}

# ==========================================================
# RUN APP
# ==========================================================
shinyApp(ui = ui, server = server)