# Funciones para tamaño de muestra para PROPORCIONES

# Población "infinita"
n_prop_infinita <- function(p, B, conf = 0.95) {
  z <- qnorm((1 + conf)/2)
  n0 <- z^2 * p * (1 - p) / B^2
  return(n0)
}

# Población finita
n_prop_finita <- function(N, p, B, conf = 0.95) {
  n0 <- n_prop_infinita(p, B, conf)
  n <- n0 / (1 + (n0 - 1)/N)
  return(n)
}

# ===== EJEMPLO =====
# Proporción esperada p = 0.6, B = 0.05 (5%), N = 500

p <- 0.60
B <- 0.05
N <- 500

n0_inf_p <- n_prop_infinita(p, B)
n_fin_p  <- n_prop_finita(N, p, B)

cat("n0 (proporción, infinita):", n0_inf_p, "\n")
cat("n (proporción, finita):", n_fin_p, "\n")
