# Función para calcular todo automáticamente
calcular_proporcion <- function(n, p_observado, p_hipotetico, nivel_confianza = 0.95) {
  # Error estándar basado en p hipotético
  se <- sqrt(p_hipotetico * (1 - p_hipotetico) / n)
  
  # Calcular z
  z <- (p_observado - p_hipotetico) / se
  
  # Calcular la probabilidad acumulada (área bajo la curva hasta ese z)
  prob_acumulada <- pnorm(z)
  
  # Para una prueba de dos colas, calculamos la probabilidad en la cola derecha
  prob_cola_derecha <- 1 - prob_acumulada
  
  # Multiplicamos por 2 para obtener la probabilidad de dos colas (por si la diferencia es en cualquier dirección)
  prob_dos_colas <- 2 * prob_cola_derecha
  
  # Nivel de confianza (por ejemplo 95%)
  alpha <- 1 - nivel_confianza
  
  # Determinar si es significativo
  es_significativo <- prob_dos_colas < alpha
  
  # Imprimir el resultado
  cat("Proporción observada (p sombrero):", p_observado, "\n")
  cat("Proporción hipotética (p):", p_hipotetico, "\n")
  cat("Z:", z, "\n")
  cat("Probabilidad dos colas:", prob_dos_colas, "\n")
  writeLines('PATH="${RTOOLS43_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
  
  if (es_significativo) {
    cat("Es una diferencia significativa al nivel de confianza del", nivel_confianza*100, "%.\n")
  } else {
    cat("No es una diferencia significativa al nivel de confianza del", nivel_confianza*100, "%.\n")
  }
}

# Ejemplo de uso con tus datos: n=500, p_observado=0.60, p_hipotetico=0.55
calcular_proporcion(n = 500, p_observado = 0.60, p_hipotetico = 0.55)