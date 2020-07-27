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

# d) Cargar datos de muestras
load("data/datos_muestras.RData")

# e) Definir funcion para cargar imagen
CargarImagen   <- function(filename) {
  imagen        <- raster::stack(x = filename)
  names(imagen) <- c("b2", "b3", "b4", "b8", "ndwi", "ndvi")
  return (imagen)
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Entrenar el modelo y predecir----
# -----------------------------------------------------------------------------#

# a) Crear modelo usando un arbol de decision CART
set.seed(0)
modelo <- glm(formula = clase ~ ., family = 'binomial', data = datos.muestras)
sufijo <- "GLM"

# b) Prediccion para 201801
raster::beginCluster(n = 8)
prediccion.probabilidades <- raster::clusterR(
  x = CargarImagen(paste0(images.directory, "/201801.tif")), 
  fun = raster::predict,
  args = list(model = modelo, type = 'response'),
  progress = 'text'
)
raster::endCluster()
prediccion.sin.inundacion <- raster::calc(
  x = prediccion.probabilidades,
  fun = function(x) {
    return (ifelse(x >= 0.5, 1, 0))
  }
)
raster::writeRaster(x = prediccion.sin.inundacion, format = "GTiff",
                    filename = paste0(images.directory, "/predict_201801_", sufijo, ".tif"))
raster::removeTmpFiles(h = 0)

# c) Prediccion para 201811
raster::beginCluster(n = 8)
prediccion.probabilidades <- raster::clusterR(
  x = CargarImagen(paste0(images.directory, "/201811.tif")), 
  fun = raster::predict,
  args = list(model = modelo, type = 'response'),
  progress = 'text'
)
raster::endCluster()
prediccion.sin.inundacion2 <- raster::calc(
  x = prediccion.probabilidades,
  fun = function(x) {
    return (ifelse(x >= 0.5, 1, 0))
  }
)
raster::writeRaster(x = prediccion.sin.inundacion2, format = "GTiff",
                    filename = paste0(images.directory, "/predict_201811_", sufijo, ".tif"))
raster::removeTmpFiles(h = 0)

# d) Prediccion para 201901
raster::beginCluster(n = 8)
prediccion.probabilidades <- raster::clusterR(
  x = CargarImagen(paste0(images.directory, "/201901.tif")), 
  fun = raster::predict,
  args = list(model = modelo, type = 'response'),
  progress = 'text'
)
raster::endCluster()
prediccion.con.inundacion <- raster::calc(
  x = prediccion.probabilidades,
  fun = function(x) {
    return (ifelse(x >= 0.5, 1, 0))
  }
)
raster::writeRaster(x = prediccion.con.inundacion, format = "GTiff",
                    filename = paste0(images.directory, "/predict_201901_", sufijo, ".tif"))
raster::removeTmpFiles(h = 0)
# ------------------------------------------------------------------------------

