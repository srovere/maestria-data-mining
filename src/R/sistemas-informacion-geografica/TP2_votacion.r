# -----------------------------------------------------------------------------#
# --- PASO 1. Inicializar script ----
# -----------------------------------------------------------------------------#
# a) Borrar entorno
rm(list = ls()); gc()

# b) Cargar paquetes
list.of.packages <- c("dplyr", "geojsonsf", "lubridate", "magrittr", "raster", 
                      "rasterVis", "randomForest", "sf", "sp", "utils", 
                      "xgboost", "yaml")
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
# --- PASO 2. Generar raster de votacion ----
# -----------------------------------------------------------------------------#

# a) Definir archivos a utilizar
archivos <- list(
  paste0(images.directory, "/prob_201901_CART.tif"),
  paste0(images.directory, "/prob_201901_GLM.tif"),
  paste0(images.directory, "/prob_201901_RF.tif"),
  paste0(images.directory, "/prob_201901_XGB.tif")
)

# b) Crear stack
stack.probabilidades <- raster::stack(archivos)
  
# c) Calcular votacion segun umbral
umbral   <- 0.5
votacion <- raster::calc(x = stack.probabilidades, fun = function(x) {
  return (length(which(x >= umbral)))
})

# d) Guardar datos
raster::writeRaster(x = votacion, format = "GTiff", 
                    filename = paste0(images.directory, "/Votacion_0.5.tif"))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Eliminar capas de agua y calcular areas inundadas ----
# -----------------------------------------------------------------------------#

# a) Eliminar capas de agua
water.bodies   <- raster::raster(paste0(images.directory, "/GT-WaterBodies-Complete.tif"))
votacion.final <- raster::mask(
  x = votacion,
  mask = water.bodies,
  maskvalue = 1, updatevalue = 0,
  filename = paste0(images.directory, "/Votacion_0.5_final.tif")
)

# b) Calcular areas inundadas
valores.positivos   <- raster::freq(votacion.final, value = 4)
hectareas.inundadas <- valores.positivos * raster::xres(votacion.final) * raster::yres(votacion.final) / 10000
# ------------------------------------------------------------------------------