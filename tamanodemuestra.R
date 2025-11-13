# Funciones para tamaño de muestra para MEDIAS

# Para población "infinita" (o muy grande)
n_media_infinita <- function(sigma, B, conf = 0.95) {
  z <- qnorm((1 + conf)/2)
  n0 <- (z * sigma / B)^2
  return(n0)
}

# Corrección para población FINITA
n_media_finita <- function(N, sigma, B, conf = 0.95) {
  n0 <- n_media_infinita(sigma, B, conf)
  n <- n0 / (1 + (n0 - 1)/N)
  return(n)
}

# ===== EJEMPLO =====
# sigma = 12, B = 5, N = 5000, conf 95%

sigma <- 12
B     <- 5
N     <- 5000

n0_inf <- n_media_infinita(sigma, B)      # sin corrección
n_fin  <- n_media_finita(N, sigma, B)     # con corrección

cat("n0 (población infinita):", n0_inf, "\n")
cat("n (población finita):", n_fin, "\n")
