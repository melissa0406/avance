set.seed(123)

# ===== 1) SIMULACIÓN NORMAL Y DISTRIBUCIÓN MUESTRAL DE LA MEDIA =====

mu    <- 100
sigma <- 20

# "Población" grande simulada
poblacion <- rnorm(100000, mean = mu, sd = sigma)

# Parámetros reales
mean(poblacion); sd(poblacion)

# Tomar muchas muestras y guardar las medias
n_muestra    <- 40
repeticiones <- 2000

medias <- replicate(repeticiones, {
  muestra <- sample(poblacion, size = n_muestra, replace = FALSE)
  mean(muestra)
})

# Histograma de las medias muestrales
hist(medias,
     main = "Distribución muestral de la media",
     xlab = "Media muestral",
     breaks = 30)

cat("Media de las medias:", mean(medias), "\n")
cat("DE de las medias:", sd(medias), "\n")
cat("Teórico sigma/sqrt(n):", sigma/sqrt(n_muestra), "\n")


# ===== 2) SIMULACIÓN DE PROPORCIONES (BINOMIAL) =====

# Supongamos p = 0.6, n = 50, muchas muestras
p_real      <- 0.6
n_muestra_p <- 50
reps_p      <- 5000

proporciones <- rbinom(reps_p, size = n_muestra_p, prob = p_real) / n_muestra_p

hist(proporciones,
     main = "Distribución muestral de la proporción",
     xlab = "p sombrerito",
     breaks = 30)

cat("Media de p sombrerito:", mean(proporciones), "\n")
cat("Teórico p:", p_real, "\n")
cat("DE empírica de p sombrerito:", sd(proporciones), "\n")
cat("Teórico sqrt(p(1-p)/n):", sqrt(p_real*(1-p_real)/n_muestra_p), "\n")
