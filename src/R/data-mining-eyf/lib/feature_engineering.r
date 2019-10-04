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
  return (as.double(logspline::plogspline(valor, ajuste)))
}

# Funcion para generar medias moviles
fe_media_movil <- function(valor, ancho.ventana) {
  return (as.double(zoo::rollmean(valor, k = ancho.ventana, fill = NA, align = "right")))
}

# Funcion para calcular minimo de ventana movil
fe_minimo_movil <- function(valor, ancho.ventana) {
  return (as.double(zoo::rollapply(valor, width = ancho.ventana, FUN = min, na.rm = TRUE, fill = NA, align = "right")))
}

# Funcion para calcular maximo de ventana movil
fe_maximo_movil <- function(valor, ancho.ventana) {
  return (as.double(zoo::rollmax(valor, k = ancho.ventana, fill = NA, align = "right")))
}

# Funcion para calcular tendencia de ventana movil
# La tendencia es la pendiente de una recta de regresion lineal
fe_tendencia_movil <- function(valor, ancho.ventana) {
  funcion_tendencia <- function(y) {
    x <- matrix(seq_along(y))
    m <- .lm.fit(cbind(1, x), y)
    return (coefficients(m)[2])
  }
  return (as.double(zoo::rollapply(valor, width = ancho.ventana, FUN = funcion_tendencia, fill = NA, align = "right")))
}

# Funcion para calcular la cantidad de dias entre 2 fechas (fecha - fecha.referencia)
# fecha_int = fecha sobre la que se quiere averiguar la cantidad de dias de diferencia (en formato YYYYMMDD)
# fecha.referencia = fecha sobre la que se hace la comparacion
fe_dias_diferencia <- function(fecha.int, fecha.referencia) {
  fecha <- as.Date(as.character(fecha.int), format = "%Y%m%d")
  return (as.numeric(difftime(fecha, fecha.referencia, units = "days")))
}