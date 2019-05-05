# ----------------------------------------------------------------------------------
# --- Practica de laboratorio 03
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Inicializacion del entorno ----
# ---------------------------------------------------------------------------------#
# i. Eliminacion de objetos del ambiente
rm(list = objects())

# ii. Carga de librerias
require(dplyr)
require(ggplot2)
require(GGally)
require(mice)
require(utils)
require(tidyr)
require(VIM)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Lectura de datos de entrada ----
# ---------------------------------------------------------------------------------#
# i. Lectura a data frame
datos.entrada <- utils::read.table(file = paste0("input/LAB03/auto-mpg.data-original.txt")) %>%
  dplyr::as.tbl()
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Exploración de datos faltantes ----
# ---------------------------------------------------------------------------------#
# i. Calculo de datos faltantes
faltantes.por.variable <- datos.entrada %>%
  tidyr::gather(key = Variable, value = Valor) %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(Disponibles = sum(! is.na(Valor)),
                   Faltantes = sum(is.na(Valor)))

# ii. Grafico de torta de variables con datos faltantes
ggplot2::ggplot(data = faltantes.por.variable) +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = "", y = Faltantes, fill = Variable), width = 1, stat = "identity") +
  ggplot2::coord_polar(theta = "y")
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Analizar correlacion de variables faltantes con las demas variables ----
# ---------------------------------------------------------------------------------#
# i. Determinacion de faltantes
variables <- colnames(datos.entrada)
variables.comparables <- purrr::map(
  .x = variables,
  .f = function(variable) {
    return (is.numeric(datos.entrada[[variable]]))
  }
) %>% unlist() %>% which(x = .)
variables.comparables <- variables[variables.comparables]

# ii. Graficar
GGally::ggpairs(data = datos.entrada, columns = variables.comparables)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Imputacion por regresion ----
# ---------------------------------------------------------------------------------#
# i. Obtener modelos lineales e funcion de las demas variables
lm1 <- stats::lm(formula = V1 ~ V2 + V3 + V4 + V5 + V6 + V7 + V8, 
                 data = datos.entrada, na.action = na.omit)
lm4 <- stats::lm(formula = V4 ~ V1 + V2 + V3 + V5 + V6 + V7 + V8, 
                 data = datos.entrada, na.action = na.omit)

# ii. Imputo por regresion
ImputarV1 <- function(V2, V3, V4, V5, V6, V7, V8, lm1) {
  coef <- coefficients(lm1)
  V1   <- coef['V2'] * V2 + 
    coef['V3'] * V3 + 
    coef['V4'] * V4 + 
    coef['V5'] * V5 + 
    coef['V6'] * V6 + 
    coef['V7'] * V7 + 
    coef['V8'] * V8 +
    coef['(Intercept)']
  return (round(V1))  
}
ImputarV4 <- function(V1, V2, V3, V5, V6, V7, V8, lm4) {
  coef <- coefficients(lm4)
  V4   <- coef['V1'] * V1 + 
    coef['V2'] * V2 + 
    coef['V3'] * V3 + 
    coef['V5'] * V5 + 
    coef['V6'] * V6 + 
    coef['V7'] * V7 + 
    coef['V8'] * V8 +
    coef['(Intercept)']
  return (V4)  
}
datos.imputados.regresion <- datos.entrada %>%
  dplyr::mutate(V1 = dplyr::if_else(is.na(V1), ImputarV1(V2, V3, V4, V5, V6, V7, V8, lm1), V1),
                V4 = dplyr::if_else(is.na(V4), ImputarV1(V1, V2, V3, V5, V6, V7, V8, lm4), V4))
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Imputacion por Hot Deck ----
# ---------------------------------------------------------------------------------#
datos.imputados.hot.deck <- VIM::hotdeck(data = datos.entrada, variable = c("V1", "V4")) %>%
  dplyr::select(-V1_imp, -V4_imp)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Imputacion por MICE ----
# ---------------------------------------------------------------------------------#
imputacion.mice <- mice::mice(data = datos.entrada, m = 5, maxit = 10, method = 'pmm', seed = 0)
datos.imputados.mice <- mice::complete(data = imputacion.mice)
# ----------------------------------------------------------------------------------
