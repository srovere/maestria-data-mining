# Set de funciones para hacer feature engineering

# Generación de clase binaria (BAJA+2 = 1, resto = 0)
fe_clase_binaria <- function(clase_ternaria) {
  return (factor(ifelse(clase_ternaria == "BAJA+2", 1, 0)))
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