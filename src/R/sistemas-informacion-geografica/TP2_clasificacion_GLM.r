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

# e) Calcular inundacion simple (201901 menos cuerpos de agua estables)
water.bodies                 <- raster::raster(paste0(images.directory, "/GT-WaterBodies-Complete.tif"))
preduccion.inundacion.simple <- raster::mask(
  x = prediccion.con.inundacion,
  mask = water.bodies,
  maskvalue = 1, updatevalue = 0,
  filename = paste0(images.directory, "/predict_inundacion_simple_", sufijo, ".tif")
)

# f) Considerar diferencia con periodos anteriores
preduccion.inundacion.diferencia <- raster::calc(
  x = raster::stack(prediccion.sin.inundacion, prediccion.sin.inundacion2, preduccion.inundacion.simple), 
  fun = function(x) {
    if (! is.na(x[1]) & ! is.na(x[2]) & ! is.na(x[3])) {
      if (x[3] == 1) {
        # Ahora hay agua. Si antes no había, considerar inundación.
        return (ifelse(x[1] == 1, 0, x[3] - x[1]))
      } else {
        # Ahora no hay agua. No importa lo que había antes
        return (0)
      }
    }
    return (NA)
  }
)
raster::writeRaster(x = preduccion.inundacion.diferencia, format = "GTiff",
                    filename = paste0(images.directory, "/predict_inundacion_diferencia_", sufijo, ".tif"))
raster::removeTmpFiles(h = 0)
# ------------------------------------------------------------------------------