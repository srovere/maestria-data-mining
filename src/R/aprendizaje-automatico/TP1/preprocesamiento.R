# ---------------------------------------------------------------------------------------#
# ---- Script para pre procesamiento de set de datos para TP1 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambientes ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(dplyr)
require(mice)
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

# Boxplot de variables numéricas con range = 5
boxplot(set.datos.convertido$age, range = 5)
boxplot(set.datos.convertido$height, range = 5)
boxplot(set.datos.convertido$weight, range = 5)
boxplot(set.datos.convertido$ap_hi, range = 5)
boxplot(set.datos.convertido$ap_lo, range = 5)

# Por inspeccion, se nota que las presiones tienen valores muy extremos aparentemente por
# un problema de unidades. Se eliminan esos valores extremos
FiltrarVariable <- function(valores, outlier.range) {
  q             <- stats::quantile(x = valores, probs = c(0.25, 0.5, 0.75))
  umbral.minimo <- q[1] - outlier.range * (q[3] - q[1])
  umbral.maximo <- q[3] + outlier.range * (q[3] - q[1])
  return (ifelse((valores <= umbral.maximo) & (valores >= umbral.minimo), valores, NA))
}

set.datos.filtrado <- set.datos.convertido %>%
  dplyr::mutate(age = FiltrarVariable(age, 5),
                height = FiltrarVariable(height, 5),
                weight = FiltrarVariable(weight, 5),
                ap_hi = FiltrarVariable(ap_hi, 5),
                ap_lo = FiltrarVariable(ap_lo, 5))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- IV. Imputación de datos faltantes ----                            
# ---------------------------------------------------------------------------------------#

# Exploro la cantidad de faltantes por faltantes por variable
mice::md.pattern(set.datos.filtrado)

# Dado que los valores faltantes corresponden a las variables ap_hi y ap_lo,
# realizamos la imputacion de dichos faltantes con el metood MICE
obj.imputacion     <- mice::mice(set.datos.filtrado, m = 5, maxit = 3, method = 'pmm', seed = 0)
set.datos.imputado <- mice::complete(obj.imputacion)

# Realizamos nuevamente los boxplots de ap_hi y ap_lo para ver que no haya valores extremos
boxplot(set.datos.imputado$age, range = 3)
boxplot(set.datos.imputado$height, range = 3)
boxplot(set.datos.imputado$weight, range = 3)
boxplot(set.datos.imputado$ap_hi, range = 3)
boxplot(set.datos.imputado$ap_lo, range = 3)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- V. Discretización de variables numéricas ----                            
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
# NOTA (2019-05-07): A efectos de poder agregar ruido, se dejan sin discretizar las variables
#                    age, height y weight. La discretizacion se hara con el metodo descripto
#                    dentro del script de analisis (python)  

GenerarTablaEtiquetas <- function(puntos.corte, etiquetas) {
  tabla <- data.frame(Etiqueta = etiquetas, 
                      ValorDesde = puntos.corte[seq(from = 1, to = length(puntos.corte)-1)],
                      ValorHasta = puntos.corte[seq(from = 2, to = length(puntos.corte))]) %>%
    dplyr::mutate(ValorDesde = ifelse(is.infinite(ValorDesde), NA, ValorDesde),
                  ValorHasta = ifelse(is.infinite(ValorHasta), NA, ValorHasta))
  return (tabla)
}

Discretizar <- function(valores, puntos.corte, etiquetas) {
  etiquetas.valores <- base::cut(x = valores, breaks = puntos.corte, labels = etiquetas) %>%
    as.integer()
  return (etiquetas.valores)
}

DiscretizarPorCuantiles <- function(valores, cantidad.cuantiles, generar.tabla = FALSE) {
  intervalo         <- 1 / cantidad.cuantiles
  cuantiles         <- seq(from = intervalo, to = 1 - intervalo, by = intervalo)
  etiquetas         <- as.character(seq_len(cantidad.cuantiles))
  puntos.corte      <- c(-Inf, stats::quantile(x = valores, probs = cuantiles, na.rm = TRUE), Inf)
  if (generar.tabla) {
    return (GenerarTablaEtiquetas(puntos.corte, etiquetas))
  } else {
    return (Discretizar(valores, puntos.corte, etiquetas))
  }
}

DiscretizarPorIntervalosSturges <- function(valores, generar.tabla = FALSE) {
  histograma        <- graphics::hist(valores, plot = FALSE)
  puntos.corte      <- c(-Inf, histograma$breaks[seq(2, length(histograma$breaks)-1)], Inf)
  etiquetas         <- as.character(seq_len(length(puntos.corte)-1))
  if (generar.tabla) {
    return (GenerarTablaEtiquetas(puntos.corte, etiquetas))
  } else {
    return (Discretizar(valores, puntos.corte, etiquetas))
  }
}

# Ahora generamos otro set de datos discretizado al que previamente le calculamos el valor
# de BMI = weight / height^2 (height = [m]; eliminando posteriormente weight y height)
set.datos.imputado.bmi <- set.datos.imputado %>%
  dplyr::mutate(bmi = weight / (height/100) ^ 2) %>%
  dplyr::select(id, age, gender, bmi, ap_hi, ap_lo, cholesterol, smoke, alco, active, cardio)

# Generar tablas de referencia
#etiquetas.age    <- DiscretizarPorCuantiles(set.datos.imputado$age, 10, TRUE)
#etiquetas.height <- DiscretizarPorCuantiles(set.datos.imputado$height, 10, TRUE)
#etiquetas.weight <- DiscretizarPorCuantiles(set.datos.imputado$weight, 10, TRUE)
etiquetas.ap_hi  <- DiscretizarPorIntervalosSturges(set.datos.imputado$ap_hi, TRUE)
etiquetas.ap_lo  <- DiscretizarPorIntervalosSturges(set.datos.imputado$ap_lo, TRUE)
#etiquetas.bmi    <- DiscretizarPorCuantiles(set.datos.imputado.bmi$bmi, 10, TRUE)

# Generar set de datos discretizados
set.datos.discretizado <- set.datos.imputado %>%
  dplyr::mutate(#age = DiscretizarPorCuantiles(age, 10),
                #height = DiscretizarPorCuantiles(height, 10),
                #weight = DiscretizarPorCuantiles(weight, 10),
                ap_hi = DiscretizarPorIntervalosSturges(ap_hi),
                ap_lo = DiscretizarPorIntervalosSturges(ap_lo))

# set.datos.discretizado.bmi <- set.datos.imputado.bmi %>%
#   dplyr::mutate(age = DiscretizarPorCuantiles(age, 10),
#                 bmi = DiscretizarPorCuantiles(bmi, 10),
#                 ap_hi = DiscretizarPorIntervalosSturges(ap_hi),
#                 ap_lo = DiscretizarPorIntervalosSturges(ap_lo))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- VI. Almacenamiento de archivo de salida ----                            
# ---------------------------------------------------------------------------------------#
readr::write_delim(x = set.datos.discretizado, path = paste0(getwd(), "/output/SetDatosDiscretizado.csv"), delim = "\t")
#readr::write_delim(x = set.datos.discretizado.bmi, path = paste0(getwd(), "/output/SetDatosDiscretizadoBMI.csv"), delim = "\t")
#readr::write_delim(x = etiquetas.age, path = paste0(getwd(), "/output/EtiquetasAge.csv"), delim = "\t")
#readr::write_delim(x = etiquetas.bmi, path = paste0(getwd(), "/output/EtiquetasBMI.csv"), delim = "\t")
#readr::write_delim(x = etiquetas.height, path = paste0(getwd(), "/output/EtiquetasHeight.csv"), delim = "\t")
#readr::write_delim(x = etiquetas.weight, path = paste0(getwd(), "/output/EtiquetasWeight.csv"), delim = "\t")
readr::write_delim(x = etiquetas.ap_hi, path = paste0(getwd(), "/output/EtiquetasAP_HI.csv"), delim = "\t")
readr::write_delim(x = etiquetas.ap_lo, path = paste0(getwd(), "/output/EtiquetasAP_LO.csv"), delim = "\t")
# ----------------------------------------------------------------------------------------