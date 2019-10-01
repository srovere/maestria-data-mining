# Set de funciones para hacer feature engineering

# Generación de clase binaria (BAJA+2 = 1, resto = 0)
fe_clase_binaria <- function(clase_ternaria) {
  return (factor(ifelse(clase_ternaria == "BAJA+2", 1, 0)))
}

# Funcion para calcular z-score
fe_zscore <- function(valor) {
  return (as.vector(scale(valor)))
}

# Funcion para calcular percentiles
fe_percentiles <- function(valor) {
  ajuste <- logspline::logspline(prcp.acumulada.referencia, lbound = min(valor), ubound = max(valor), silent = TRUE)
  return (logspline::plogspline(valor, ajuste))
}

# Funcion para generar medias moviles
fe_media_movil <- function(valor, ancho.ventana) {
  return (zoo::rollmean(valor, k = ancho.ventana, fill = NA, align = "right"))
}

# Funcion para calcular minimo de ventana movil
fe_minimo_movil <- function(valor, ancho.ventana) {
  return (zoo::rollapply(valor, width = ancho.ventana, FUN = min, na.rm = TRUE, fill = NA, align = "right"))
}

# Funcion para calcular maximo de ventana movil
fe_maximo_movil <- function(valor, ancho.ventana) {
  return (zoo::rollmax(valor, k = ancho.ventana, fill = NA, align = "right"))
}

# Funcion para calcular tendencia de ventana movil
# La tendencia es la pendiente de una recta de regresion lineal
fe_tendencia_movil <- function(valor, ancho.ventana) {
  funcion_tendencia <- function(y) {
    x <- seq_along(y)
    m <- lm(formula = y ~ x, na.action = na.omit)
    return (coefficients(m)["x"])
  }
  return (zoo::rollapply(valor, width = ancho.ventana, FUN = funcion_tendencia, fill = NA, align = "right"))
}