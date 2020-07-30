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
# --- PASO 2. Hacer smoothing de prediccion 209101 (XGB) ----
# -----------------------------------------------------------------------------#

# a) Cargar imagen de probabilidades de 201901
imagen.probabilidades <- raster::raster(paste0(images.directory, "/prob_201901_XGB.tif"))

# b) Hacer smoothing con la media
imagen.smoothing.mean <- raster::focal(x = imagen.probabilidades,
                                       w = matrix(1/25, nc=5, nr=5))
raster::writeRaster(x = imagen.smoothing.mean, format = "GTiff", overwrite = TRUE,
                    filename = paste0(images.directory, "/prob_201901_XGB_smooth_5x5_mean.tif"))

# b) Hacer smoothing con la media
imagen.smoothing.gauss <- raster::focal(x = imagen.probabilidades,
                                        w = raster::focalWeight(imagen.probabilidades, 7, "Gauss"))
raster::writeRaster(x = imagen.smoothing.gauss, format = "GTiff", overwrite = TRUE,
                    filename = paste0(images.directory, "/prob_201901_XGB_smooth_5x5_gauss.tif"))
# ------------------------------------------------------------------------------