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

# d) Cargar cuerpos de agua
water.bodies <- raster::raster(paste0(images.directory, "/GT-WaterBodies-Complete.tif"))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Probar XGB con umbral seleccionado ----
# -----------------------------------------------------------------------------#

# a) Cargar imagen de probabilidades de 201901
umbral                <- 0.6
imagen.probabilidades <- raster::raster(paste0(images.directory, "/prob_201901_XGB.tif")) %>%
  raster::mask(x = .,  mask = water.bodies, maskvalue = 1, updatevalue = 0) %>%
  raster::calc(x = ., fun = function(x) {
    return (ifelse(x >= umbral, 1, 0))
  })
raster::writeRaster(x = imagen.probabilidades, format = "GTiff", overwrite = TRUE,
                    filename = paste0(images.directory, "/201901_CO_0.6_XGB.tif"))

# b) Calcular hectareas inundadas
valores.positivos   <- raster::freq(imagen.probabilidades, value = 1)
hectareas.inundadas <- valores.positivos * raster::xres(imagen.probabilidades) * raster::yres(imagen.probabilidades) / 10000
# ------------------------------------------------------------------------------