# ----------------------------------------------------------
# Proyecto: Distribución muestral de la media (Aplicación Shiny)
# Autor: Melissa Morales
# Fecha: 2025-11-12
# Descripción:
#   Esta aplicación:
#     1. Genera una población normal simulada de tamaño 2000 con media 100 y sd 20.
#     2. Calcula los parámetros reales de la población simulada.
#     3. Estima el tamaño de muestra n para un margen de error B = 10,
#        con un nivel de confianza del 95%.
#     4. Extrae una muestra aleatoria simple de tamaño n.
#     5. Estima la media poblacional usando el promedio muestral y calcula su error estándar.
# ----------------------------------------------------------

library(shiny)

# ================================
#            UI
# ================================
ui <- fluidPage(
  
  titlePanel("Distribución Muestral de la Media - Proyecto Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Simulación Estadística"),
      helpText("Población: N = 2000, Normal(μ = 100, σ = 20)"),
      helpText("Error permitido (B): 10 unidades"),
      helpText("Nivel de confianza: 95%"),
      br(),
      actionButton("resimular", "Volver a Simular Todo")
    ),
    
    mainPanel(
      h3("Resultados"),
      
      h4("1) Parámetros Poblacionales"),
      verbatimTextOutput("pop_info"),
      
      h4("2) Cálculo del Tamaño de Muestra"),
      verbatimTextOutput("n_info"),
      
      h4("3) Estimación a partir de la Muestra"),
      verbatimTextOutput("sample_info"),
      
      h4("4) Histograma de la Población"),
      plotOutput("pop_hist"),
      
      h4("5) Histograma de la Muestra"),
      plotOutput("sample_hist")
    )
  )
)

# ================================
#            SERVER
# ================================
server <- function(input, output, session) {
  
  simulacion <- eventReactive(input$resimular, ignoreNULL = FALSE, {
    
    # ----- 1) Crear población -----
    N <- 2000
    mu <- 100
    sigma <- 20
    
    set.seed(123)
    poblacion <- rnorm(N, mean = mu, sd = sigma)
    
    media_pop <- mean(poblacion)
    sd_pop <- sd(poblacion)
    
    # ----- 2) Cálculo tamaño de muestra n -----
    B <- 10
    conf <- 0.95
    z <- qnorm((1 + conf)/2)  # ≈ 1.96
    
    # Fórmula n0 = (z * sigma / B)^2
    n0 <- (z * sd_pop / B)^2
    
    # Corrección por población finita
    n_finite <- n0 / (1 + (n0 - 1) / N)
    n <- ceiling(n_finite)
    
    # ----- 3) Extraer muestra -----
    muestra <- sample(poblacion, size = n, replace = FALSE)
    
    # ----- 4) Estimación -----
    media_muestral <- mean(muestra)
    sd_muestral <- sd(muestra)
    se_media <- sd_muestral / sqrt(n)
    
    list(
      N = N,
      mu_teo = mu,
      sigma_teo = sigma,
      poblacion = poblacion,
      media_pop = media_pop,
      sd_pop = sd_pop,
      B = B,
      conf = conf,
      z = z,
      n0 = n0,
      n_finite = n_finite,
      n = n,
      muestra = muestra,
      media_muestral = media_muestral,
      sd_muestral = sd_muestral,
      se_media = se_media
    )
  })
  
  # ----- Mostrar datos -----
  output$pop_info <- renderPrint({
    s <- simulacion()
    cat("Media poblacional (real):", round(s$media_pop, 3), "\n")
    cat("Desviación estándar poblacional (real):", round(s$sd_pop, 3), "\n")
  })
  
  output$n_info <- renderPrint({
    s <- simulacion()
    cat("Valor z:", round(s$z, 3), "\n")
    cat("n0 sin corrección:", round(s$n0, 3), "\n")
    cat("n con corrección:", round(s$n_finite, 3), "\n")
    cat("n final:", s$n, "\n")
  })
  
  output$sample_info <- renderPrint({
    s <- simulacion()
    cat("Tamaño de la muestra (n):", s$n, "\n\n")
    cat("Media muestral:", round(s$media_muestral, 3), "\n")
    cat("Desviación estándar muestral:", round(s$sd_muestral, 3), "\n")
    cat("Error Estándar (SE):", round(s$se_media, 3), "\n")
  })
  
  # ----- Histogramas -----
  output$pop_hist <- renderPlot({
    hist(simulacion()$poblacion,
         main = "Población simulada",
         xlab = "Valores",
         col = "lightblue", border = "white")
  })
  
  output$sample_hist <- renderPlot({
    hist(simulacion()$muestra,
         main = "Muestra extraída",
         xlab = "Valores",
         col = "lightgreen", border = "white")
  })
}

# ================================
#        Ejecutar la App
# ================================
shinyApp(ui, server)
