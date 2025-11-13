set.seed(123)  # Para que los resultados sean reproducibles

# Supongamos que tenemos 1000 clientes (1 a 1000)
clientes <- 1:1000

# Seleccionamos una muestra aleatoria simple de 50
muestra <- sample(clientes, size = 50, replace = FALSE)

# De esos 50, solo responden la mitad (25)
respondieron <- sample(muestra, size = length(muestra) * 0.5)

# Mostramos quiénes respondieron
respondieron


#Opcion 2
set.seed(123)  # Para resultados reproducibles

# Supongamos que tus 10 muestras están numeradas del 1 al 10
muestras <- 1:10

# Barajamos las 10 muestras aleatoriamente
muestras_aleatorias <- sample(muestras)

# Asignamos las primeras 5 al método estándar y las otras 5 al nuevo método
metodo_estandar <- muestras_aleatorias[1:5]
metodo_nuevo   <- muestras_aleatorias[6:10]

# Veamos la asignación
cat("Método estándar:", metodo_estandar, "\n")
cat("Nuevo método:", metodo_nuevo, "\n")
