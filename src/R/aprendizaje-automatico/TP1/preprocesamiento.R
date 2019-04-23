# ---------------------------------------------------------------------------------------#
# ---- Script para pre procesamiento de set de datos para TP1 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambientes ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(dplyr)
require(readr)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- II. Lectura del set de datos y conversion de tipos ----                            
# ---------------------------------------------------------------------------------------#

#
# El set de datos a utilizar fue descargado de:
#
# https://www.kaggle.com/sulianova/cardiovascular-disease-dataset
#
# Contiene los siguientes atributos:
#
#  1. Age | Objective Feature | age | int (days)
#  2. Height | Objective Feature | height | int (cm) |
#  3. Weight | Objective Feature | weight | float (kg) |
#  4. Gender | Objective Feature | gender | categorical code |
#  5. Systolic blood pressure | Examination Feature | ap_hi | int |
#  6. Diastolic blood pressure | Examination Feature | ap_lo | int |
#  7. Cholesterol | Examination Feature | cholesterol | 1: normal, 2: above normal, 3: well above normal |
#  8. Glucose | Examination Feature | gluc | 1: normal, 2: above normal, 3: well above normal |
#  9. Smoking | Subjective Feature | smoke | binary |
# 10. Alcohol intake | Subjective Feature | alco | binary |
# 11. Physical activity | Subjective Feature | active | binary |
# 12. Presence or absence of cardiovascular disease | Target Variable | cardio | binary |
#

# Se leerá el set de datos y se pasará la edad a años dividiendo por 365
# para facilitar el análisis de resultados.
set.datos.original   <- readr::read_delim(file = paste0(getwd(), "/input/cardio_train.csv"), delim = ";")
set.datos.convertido <- set.datos.original %>%
  dplyr::mutate(age = age / 365)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Eliminar outliers extremos en atributos numéricos ----                            
# ---------------------------------------------------------------------------------------#

# Boxplot de variables numéricas con range = 3 (outliers extremos)
boxplot(set.datos.convertido$age, range = 3)
boxplot(set.datos.convertido$height, range = 3)
boxplot(set.datos.convertido$weight, range = 3)
boxplot(set.datos.convertido$ap_hi, range = 3)
boxplot(set.datos.convertido$ap_lo, range = 3)

# Por inspeccion, se nota que las presiones tienen valores muy extremos aparentemente por
# un problema de unidades. Se eliminan esos valores extremos
FiltrarVariable <- function(valores, outlier.range) {
  q             <- stats::quantile(x = valores, probs = c(0.25, 0.5, 0.75))
  umbral.minimo <- q[1] - outlier.range * (q[3] - q[1])
  umbral.maximo <- q[3] + outlier.range * (q[3] - q[1])
  return (ifelse((valores <= umbral.maximo) & (valores >= umbral.minimo), valores, NA))
}

set.datos.filtrado <- set.datos.convertido %>%
  dplyr::mutate(ap_hi = FiltrarVariable(ap_hi, 5),
                ap_lo = FiltrarVariable(ap_lo, 5))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- IV. Discretización de variables numéricas ----                            
# ---------------------------------------------------------------------------------------#

#
# Las variables numéricas a discretizar son:
#
#  1. Age | Objective Feature | age | int (days)
#  2. Height | Objective Feature | height | int (cm) |
#  3. Weight | Objective Feature | weight | float (kg) |
#  5. Systolic blood pressure | Examination Feature | ap_hi | int |
#  6. Diastolic blood pressure | Examination Feature | ap_lo | int |
#
# En algunos casos (edad, altura y peso) se dividen los casos en cuantiles y se toman límites discretos 
# también con el objetivo de mejorar la interpretación de los resultados.
# En otros casos, la discretización se hace por bandas de intervalos de igual longitud. Esto es aplicable
# a valores de presión (ap_hi, ap_lo) donde hay valores discretos muy frecuentes que representa una significativa
# porción de la población lo que hace imposible dividir en deciles, cuartiles o quintiles.
#

DiscretizarPorCuantiles <- function(valores, cantidad.cuantiles) {
  intervalo         <- 1 / cantidad.cuantiles
  cuantiles         <- seq(from = intervalo, to = 1 - intervalo, by = intervalo)
  etiquetas         <- as.character(seq_len(cantidad.cuantiles))
  puntos.corte      <- c(-Inf, stats::quantile(x = valores, probs = cuantiles, na.rm = TRUE), Inf)
  etiquetas.valores <- base::cut(x = valores, breaks = puntos.corte, labels = etiquetas) %>%
    as.integer()
  return (etiquetas.valores)
}

DiscretizarPorIntervalosSturges <- function(valores) {
  histograma        <- graphics::hist(valores, plot = FALSE)
  puntos.corte      <- c(-Inf, histograma$breaks[seq(2, length(histograma$breaks)-1)], Inf)
  etiquetas         <- as.character(seq_len(length(puntos.corte)-1))
  etiquetas.valores <- base::cut(x = valores, breaks = puntos.corte, labels = etiquetas) %>%
    as.integer()
  return (etiquetas.valores)
}

set.datos.discretizado <- set.datos.filtrado %>%
  dplyr::mutate(age = DiscretizarPorCuantiles(age, 5),
                height = DiscretizarPorCuantiles(height, 10),
                weight = DiscretizarPorCuantiles(weight, 10),
                ap_hi = DiscretizarPorIntervalosSturges(ap_hi),
                ap_lo = DiscretizarPorIntervalosSturges(ap_lo))
# ---------------------------------------------------------------------------------------