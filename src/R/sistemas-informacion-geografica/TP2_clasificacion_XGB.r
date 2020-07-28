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

# f) Cargar imagen de ground truth positiva para hacer mask
raster.recorte <- raster::raster(paste0(images.directory, "/201801.tif"))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Funcion para generar prediccion ----
# -----------------------------------------------------------------------------#
PredecirRaster <- function(raster.prediccion, raster.recorte, modelo,
                           filename, chunk.size = 1000) {
  # a) Realizar prediccion
  filas      <- raster::nrow(raster.prediccion)
  nchunks    <- ceiling(filas / chunk.size)
  resultados <- c()
  for (nchunk in seq_len(nchunks)) {
    start.time   <- proc.time()[3]
    first.row    <- 1 + (nchunk-1) * chunk.size
    n.rows       <- min(raster::nrow(raster.prediccion) - first.row + 1, chunk.size)
    bloque       <- raster::getValuesBlock(x = raster.prediccion, row = first.row, nrows = n.rows)
    xgb.bloque   <- xgboost::xgb.DMatrix(data = bloque, label = rep(NA, nrow(bloque)))
    prediccion   <- predict(modelo, bloque)
    resultado    <- ifelse(prediccion >= 0.5, 1, 0)
    resultados   <- c(resultados, resultado)
    elapsed.time <- proc.time()[3] - start.time
    cat(sprintf("Chunk %d de %d. Tarde %f segundos\n", nchunk, nchunks, elapsed.time))
    rm(start.time, first.row, n.rows, bloque, xgb.bloque, prediccion, resultado, elapsed.time)
  }
  
  # b) Generar raster y realizar correcciones
  raster.resultado <- raster::raster(raster.prediccion)
  raster.resultado <- raster::setValues(raster.resultado, resultados) %>%
    raster::calc(x = ., fun = function(x) { return (ifelse(x >= 0.5, 1, 0)) }) %>%
    raster::mask(x = ., mask = raster.recorte)

  # c) Guardar raster
  raster::writeRaster(x = raster.resultado, format = "GTiff", filename = filename, overwrite = TRUE)
  rm(filas, nchunks, resultados, raster.resultado)
  return (raster::raster(x = filename))
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Entrenar el modelo y predecir ----
# -----------------------------------------------------------------------------#

# a) Crear modelo usando un arbol de decision CART
set.seed(0)
parametros <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  groy_policy = "lossguide",
  tree_method = 'hist',
  eta = 0.04,
  max_depth = 10
)
xgb.train <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(datos.muestras, -clase)),
                                   label = as.matrix(dplyr::select(datos.muestras, clase)))
modelo    <- xgboost::xgb.train(data = xgb.train, nrounds = 100, varbose = 0,
                                nthread = 8, params = parametros)
sufijo    <- "XGB"

# b) Prediccion para 201801
prediccion.sin.inundacion <- PredecirRaster(
  raster.prediccion = CargarImagen(paste0(images.directory, "/201801.tif")),
  raster.recorte = raster.recorte,
  modelo = modelo,
  filename = paste0(images.directory, "/predict_201801_", sufijo, ".tif")
)

# c) Prediccion para 201811
prediccion.sin.inundacion2 <- PredecirRaster(
  raster.prediccion = CargarImagen(paste0(images.directory, "/201811.tif")),
  raster.recorte = raster.recorte,
  modelo = modelo,
  filename = paste0(images.directory, "/predict_201811_", sufijo, ".tif")
)

# d) Prediccion para 201801
prediccion.con.inundacion <- PredecirRaster(
  raster.prediccion = CargarImagen(paste0(images.directory, "/201901.tif")),
  raster.recorte = raster.recorte,
  modelo = modelo,
  filename = paste0(images.directory, "/predict_201901_", sufijo, ".tif")
)
# ------------------------------------------------------------------------------