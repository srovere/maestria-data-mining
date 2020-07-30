# -----------------------------------------------------------------------------#
# --- PASO 1. Inicializar script ----
# -----------------------------------------------------------------------------#
# a) Borrar entorno
rm(list = ls()); gc()

# b) Cargar paquetes
list.of.packages <- c("dplyr", "raster")
for (pack in list.of.packages) {
  if (!require(pack, character.only = TRUE)) {
    stop("Paquete no instalado: ", pack)
  }
}
rm(list.of.packages, pack); gc()

# c) Definir carpetas
working.directory <- paste0(getwd(), "/tp-teledeteccion2020")
images.directory  <- paste0(working.directory, "/images/final")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Calcular metricas sobre ground truth completa ----
# -----------------------------------------------------------------------------#

inundaciones <- NULL
for (algoritmo in c("CART", "GLM", "RF", "XGB")) {
  # a) Cargar imagenes
  inundacion.simple     <- raster::raster(paste0(images.directory, "/predict_inundacion_simple_", algoritmo, ".tif"))
  inundacion.diferencia <- raster::raster(paste0(images.directory, "/predict_inundacion_diferencia_", algoritmo, ".tif"))
  
  # b) Contar cantidad de pixels con valor = 1
  valores.positivos.simple       <- raster::freq(inundacion.simple, value = 1)
  hectareas.inundadas.simple     <- valores.positivos.simple * raster::xres(inundacion.simple) * raster::yres(inundacion.simple) / 10000
  valores.positivos.diferencia   <- raster::freq(inundacion.diferencia, value = 1)
  hectareas.inundadas.diferencia <- valores.positivos.diferencia * raster::xres(inundacion.diferencia) * raster::yres(inundacion.diferencia) / 10000
  
  # c) Generar DF
  inundaciones <- rbind(inundaciones, data.frame(algoritmo = algoritmo, simple = hectareas.inundadas.simple, diferencia = hectareas.inundadas.diferencia))
}
# ------------------------------------------------------------------------------