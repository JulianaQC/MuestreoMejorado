
library(dplyr)

# Definir el tamaño de la población y de la muestra
N <- 30  # Tamaño de la población
n <- 10  # Tamaño de la muestra

# Generar datos con tendencia lineal
set.seed(123)  # Fijar la semilla para reproducibilidad
cbin <- 1:N
ingresos <- 1000 + 150 * cbin + rnorm(N, mean = 0, sd = 200)
gastos <- 500 + 100 * cbin + rnorm(N, mean = 0, sd = 150)

# Crear un data frame con las variables cbin, ingresos y gastos
poblacion <- data.frame(
  cbin = cbin,
  ingresos = ingresos,
  gastos = gastos
)
plot(poblacion$ingresos, poblacion$gastos)
# Imprimir el data frame de la población
print("Data frame de la población:")
print(poblacion)

# Función improved_systematic_sampling para data frames
imp <- function(N, n, poblacion) {
  
  # Verificar que N y n son valores válidos
  if (N < n) {
    stop("El tamaño de la muestra no puede ser mayor que el tamaño de la población.")
  }
  
  # Calcular k
  k <- ceiling(N / n)
  
  # Dividir la población en tres conjuntos
  set1 <- poblacion[1:min((k^2), N), ]
  set2 <- poblacion[(min((k^2), N) + 1):min(((n - 2) * k), N), ]
  set3 <- poblacion[(min(((n - 2) * k), N) + 1):N, ]
  
  # Seleccionar muestras aleatorias de cada conjunto
  r1 <- sample(1:k, 1)
  r2 <- sample(1:k, 1)
  r3 <- sample(1:k, 1)
  
  muestra1 <- set1[seq(r1, nrow(set1), by = k), ]
  muestra2 <- set2[seq(r2, nrow(set2), by = k), ]
  muestra3 <- set3[seq(r3, nrow(set3), by = k), ]
  
  # Combinar las muestras en una sola
  muestra <- bind_rows(muestra1, muestra2, muestra3)
  
  return(muestra)
}

# Aplicar la función improved_systematic_sampling_df
muestra_df <- imp(N, n, poblacion)

# Imprimir la muestra obtenida
print("Muestra obtenida:")
print(muestra_df)
