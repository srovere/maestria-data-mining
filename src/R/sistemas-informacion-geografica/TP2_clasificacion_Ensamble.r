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
# --- PASO 2. Evaluación de ensamble con CV ----
# -----------------------------------------------------------------------------#

# a) Generar los folds
set.seed(0)
folds    <- caret::createFolds(seq_len(nrow(datos.muestras)), k = 5)
metricas <- NULL

# b) Definicion de parametros para XGBoost
parametros <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  grow_policy = "lossguide",
  tree_method = 'hist',
  eta = 0.04,
  max_depth = 10
)

# c) Entrenar y validar con GLM, RF y XGB para cada fold
for (i in seq_along(folds)) {
  # i. Obtener conjuntos de train/validacion
  f           <- folds[[i]]
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  xgb.train   <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(train, -clase)),
                                      label = as.matrix(dplyr::select(train, clase)))
  xgb.test    <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(test, -clase)),
                                      label = as.matrix(dplyr::select(test, clase)))
  
  # Entrenar modelos
  modelo.glm  <- glm(formula = as.factor(clase) ~ ., family = 'binomial', data = train)
  modelo.rf   <- randomForest::randomForest(x = dplyr::select(train, -clase), ntree = 100,
                                            y = as.factor(dplyr::pull(dplyr::select(train, clase))),
                                            importance = TRUE)
  modelo.xgb  <- xgboost::xgb.train(data = xgb.train, verbose = 2, nrounds = 100,
                                    watchlist = list(train = xgb.train, test = xgb.test),
                                    nthread = 8, params = parametros)
  
  # Realizar predicciones
  pred.glm      <- predict(modelo.glm, test, type = 'response')  
  pred.rf       <- predict(modelo.rf, test, type = 'prob')[,"1"]
  pred.xgb      <- predict(modelo.xgb, xgb.test)
  pred.ensamble <- (pred.glm + pred.rf + pred.xgb) / 3
  
  # Obtener resultados (clase) y metricas
  resultados  <- data.frame(predicho = ifelse(pred.ensamble >= 0.5, 1, 0), observado = test$clase)
  conf.mat    <- caret::confusionMatrix(table(resultados))
  metricas    <- rbind(metricas, 
                       data.frame(algoritmo = "Ensamble", fold = i, accuracy = conf.mat$overall['Accuracy'],
                                  f1 = conf.mat$byClass['F1'], kappa = conf.mat$overall['Kappa'], precision = conf.mat$byClass['Precision'],
                                  recall = conf.mat$byClass['Recall'])
  )
}

# d) Calcular metricas promedio
metricas.promedio <- metricas %>%
  dplyr::group_by(algoritmo) %>%
  dplyr::summarise(accuracy = mean(accuracy), f1 = mean(f1), kappa = mean(kappa),
                   precision = mean(precision), recall = mean(recall))
readr::write_csv(x = metricas.promedio, path = "~/metricas.csv")
# ------------------------------------------------------------------------------