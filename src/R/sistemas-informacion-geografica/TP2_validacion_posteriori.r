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

# d) Cargar ground truth
ground.truth <- raster::raster(paste0(images.directory, "/GT-SinCuerposAgua.tif"))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Calcular metricas sobre ground truth completa ----
# -----------------------------------------------------------------------------#

metricas <- NULL
for (algoritmo in c("CART", "GLM", "RF", "XGB")) {
  # a) Cargar imagenes
  imagen.negativa <- raster::raster(paste0(images.directory, "/predict_201801_", algoritmo, ".tif")) %>%
    raster::mask(x = ., mask = ground.truth)
  imagen.positiva <- raster::raster(paste0(images.directory, "/predict_201901_", algoritmo, ".tif")) %>%
    raster::mask(x = ., mask = ground.truth)
  
  # b) Generar matriz de confusion
  predicciones.referencia.positiva <- table(raster::values(imagen.positiva))
  predicciones.referencia.negativa <- table(raster::values(imagen.negativa))
  predicciones                     <- as.factor(c(rep(0, predicciones.referencia.positiva["0"]), rep(1, predicciones.referencia.positiva["1"]),
                                                  rep(0, predicciones.referencia.negativa["0"]), rep(1, predicciones.referencia.negativa["1"])))
  realidad                         <- factor(c(rep(1, sum(predicciones.referencia.positiva)), rep(0, sum(predicciones.referencia.positiva))),
                                             levels = c(0, 1))
  conf.mat                         <- caret::confusionMatrix(data = predicciones, 
                                                             reference = realidad,
                                                             positive = "1")
  
  # c) Calcular metricas
  metricas <- rbind(metricas, 
                    data.frame(algoritmo = algoritmo, accuracy = conf.mat$overall['Accuracy'],
                               f1 = conf.mat$byClass['F1'], kappa = conf.mat$overall['Kappa'], precision = conf.mat$byClass['Precision'],
                               recall = conf.mat$byClass['Recall'])
  )
}
# ------------------------------------------------------------------------------