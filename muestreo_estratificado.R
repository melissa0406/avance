set.seed(123)

library(dplyr)
library(tidyr)

# 1) Definimos los estratos
strata <- data.frame(
  estrato = c("A", "B", "C"),
  Nh      = c(3000, 4000, 5000),
  mu      = c(50, 60, 80),
  sigma   = c(10, 20, 30)
)

N <- sum(strata$Nh)

# 2) Creamos la población estratificada
poblacion <- strata %>%
  tidyr::uncount(Nh) %>%
  group_by(estrato) %>%
  mutate(y = rnorm(n(), mean = first(mu), sd = first(sigma))) %>%
  ungroup()

# 3) Fijamos tamaño total de muestra
n_total <- 600

# 4) Calculamos asignación de Neyman
strata <- strata %>%
  mutate(
    Wh = Nh / N,
    Sh = sigma,
    weight_N = Nh * Sh
  ) %>%
  mutate(
    n_neyman = round(n_total * weight_N / sum(weight_N))
  )

strata
sum(strata$n_neyman)   # debería ser ≈ 600

# 5) Extraemos la muestra estratificada con Neyman
#    Usamos un bucle claro: para cada estrato, tomamos n_neyman filas al azar.

muestras_lista <- list()

for (i in seq_len(nrow(strata))) {
  est <- strata$estrato[i]     # nombre del estrato (A, B o C)
  nh  <- strata$n_neyman[i]    # tamaño de muestra para ese estrato
  
  # Filtramos solo ese estrato
  sub_pob <- subset(poblacion, estrato == est)
  
  # Tomamos nh elementos al azar sin reemplazo
  idx <- sample(seq_len(nrow(sub_pob)), size = nh, replace = FALSE)
  
  # Guardamos la submuestra
  muestras_lista[[i]] <- sub_pob[idx, ]
}

# Unimos todas las submuestras en un solo data.frame
muestra_neyman <- bind_rows(muestras_lista)

# Veamos cuántos sacamos por estrato
muestra_neyman %>% count(estrato)

# 6) Estimar media estratificada y error estándar aproximado

# Resumen por estrato en la muestra
resumen_h <- muestra_neyman %>%
  group_by(estrato) %>%
  summarise(
    nh     = n(),
    ybar_h = mean(y),
    Sh2    = var(y),
    .groups = "drop"
  )

# Unimos información de Nh y Wh
tabla_est <- strata %>%
  select(estrato, Nh, Wh, n_neyman) %>%
  left_join(resumen_h, by = "estrato") %>%
  mutate(fh = n_neyman / Nh)

tabla_est

# Media estratificada
ybar_st <- sum(tabla_est$Wh * tabla_est$ybar_h)

# Varianza aproximada de la media estratificada (sin reemplazo)
var_ybar_st <- sum((tabla_est$Wh^2) * (1 - tabla_est$fh) * tabla_est$Sh2 / tabla_est$n_neyman)
se_ybar_st  <- sqrt(var_ybar_st)

cat("Media estratificada estimada:", ybar_st, "\n")
cat("Error estándar aproximado:", se_ybar_st, "\n")
