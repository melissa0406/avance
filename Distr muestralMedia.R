# Media
xbar <- mean(muestra$y); s <- sd(muestra$y); n <- nrow(muestra)
ic_media <- xbar + c(-1,1) * 1.96 * s/sqrt(n)

# Proporción (éxito=1/0)
p_hat <- mean(muestra$exito==1)
se_p  <- sqrt(p_hat*(1-p_hat)/n)
ic_p  <- p_hat + c(-1,1) * 1.96 * se_p
# ejercicio alzheimer 

# Parámetros conocidos
mu <- 8         # Media poblacional
sigma <- 4       # Desviación estándar poblacional
n <- 30        # Tamaño de la muestra

# Desviación estándar de la distribución muestral
sigma_xbar <- sigma / sqrt(n)

# 1. P(x̄ < 7 )condiciones
prob1 <- pnorm(7, mean = mu, sd = sigma_xbar)

# 2. P(x̄ > 7) = 1 - P(x̄ < 7)
prob2 <- 1 - prob1

# 3. P(x̄ < 7 o x̄ > 9) = P(x̄ < 7) + P(x̄ > 9)
prob3 <- pnorm(7, mean = mu, sd = sigma_xbar) + (1 - pnorm(9, mean = mu, sd = sigma_xbar))

# Mostramos las probabilidades
cat("P(x̄ < 7):", prob1, "\n")
cat("P(x̄ > 7):", prob2, "\n")
cat("P(x̄ < 7 o x̄ > 9):", prob3, "\n")

### DISTRIBUCIÓN MUESTRAL DE LA MEDIA ###

set.seed(123)

# Generamos una población normal artificial
poblacion <- rnorm(100000, mean = 50, sd = 10)

# Tomamos muchas muestras aleatorias simples y guardamos sus medias
n <- 30                   # tamaño de cada muestra
repeticiones <- 2000      # cuántas muestras tomaremos

medias <- replicate(repeticiones, {
  muestra <- sample(poblacion, size = n, replace = FALSE)
  mean(muestra)
})

# Graficamos la distribución de las medias muestrales
hist(medias,
     main = "Distribución muestral de la media",
     col = "skyblue",
     breaks = 30)

mean(medias)      # Debe estar cerca de 50
sd(medias)        # Debe ser aprox. 10/sqrt(30)




