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
# --- PASO 2. Definir sitios de muestreo ----
# -----------------------------------------------------------------------------#

# a) Cargar raster de ground truth
ground.truth.agua   <- raster::raster(paste0(images.directory, "/GT-SinCuerposAgua.tif"))
ground.truth.extent <- sf::st_read(dsn = paste0(working.directory, "/ground_truth"), layer = "Flood") %>%
  sf::st_set_crs(x = ., value = 22185) %>%
  raster::extent()

# b) Definir semilla
set.seed(0)

# c) Generar muestra random (se utiliza cualquiera de los 2 rasters)
numero.muestras <- 500000
muestra         <- raster::sampleRandom(ground.truth.agua, size = numero.muestras, 
                                        ext = ground.truth.extent, na.rm = TRUE, sp = TRUE)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Extraer muestras ----
# -----------------------------------------------------------------------------#

# a) Definir funciones para extraer muestra
CargarImagen   <- function(filename) {
  imagen        <- raster::stack(x = filename)
  names(imagen) <- c("b2", "b3", "b4", "b8", "ndwi", "ndvi")
  return (imagen)
}
ExtraerMuestra <- function(filename, muestra, clase) {
  # i. Definir imagen de entrenamiento y nombrar las bandas
  imagen.entrenamiento <- CargarImagen(filename)
  
  # ii. Extraer muestras para los lugares indicados. Eliminar columna ID.
  valores.muestras <- raster::extract(imagen.entrenamiento, muestra, df = TRUE)
  valores.muestras <- valores.muestras[, -1]
  
  # iii. Agregar la informacion de la clase a las muestras. Filtrar NAs
  datos.muestras <- data.frame(clase = clase, valores.muestras) %>%
    dplyr::filter(! is.na(ndvi) & ! is.na(ndwi))
  return (datos.muestras)
}

# b) Extraer muestras para 201801 y 201901
datos.muestras <- rbind(
  ExtraerMuestra(filename = paste0(images.directory, "/201801.tif"), muestra = muestra, clase = 0),
  ExtraerMuestra(filename = paste0(images.directory, "/201901.tif"), muestra = muestra, clase = 1)
)

# c) Guardar muestra
save(muestra, datos.muestras, file = "data/datos_muestras.RData")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Evaluación modelos con CV ----
# -----------------------------------------------------------------------------#

# a) Generar los folds
set.seed(0)
folds    <- caret::createFolds(seq_len(nrow(datos.muestras)), k = 5)
metricas <- NULL

# b) Entrenar y validar con CART para cada fold
for (i in seq_along(folds)) {
  f           <- folds[[i]]
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  modelo.cart <- rpart::rpart(as.factor(clase) ~ ., 
                              data = train, 
                              method = 'class', 
                              control = list(maxdepth = 10))
  pred        <- predict(modelo.cart, test, type = 'class')  
  resultados  <- data.frame(observado = test$clase, predicho = pred)
  conf.mat    <- caret::confusionMatrix(table(resultados), positive = "1")
  metricas    <- rbind(metricas, 
    data.frame(algoritmo = "CART", fold = i, accuracy = conf.mat$overall['Accuracy'],
               f1 = conf.mat$byClass['F1'], kappa = conf.mat$overall['Kappa'], precision = conf.mat$byClass['Precision'],
               recall = conf.mat$byClass['Recall'])
  )  
}

# c) Entrenar y validar con RF para cada fold
for (i in seq_along(folds)) {
  f           <- folds[[i]]
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  modelo.rf   <- randomForest::randomForest(x = dplyr::select(train, -clase), ntree = 100,
                                            y = as.factor(dplyr::pull(dplyr::select(train, clase))),
                                            importance = TRUE)
  pred        <- predict(modelo.rf, test, type = 'class')  
  resultados  <- data.frame(observado = test$clase, predicho = pred)
  conf.mat    <- caret::confusionMatrix(table(resultados))
  metricas    <- rbind(metricas, 
                       data.frame(algoritmo = "RF", fold = i, accuracy = conf.mat$overall['Accuracy'],
                                  f1 = conf.mat$byClass['F1'], kappa = conf.mat$overall['Kappa'], precision = conf.mat$byClass['Precision'],
                                  recall = conf.mat$byClass['Recall'])
  )
}

# d) Entrenar y validar con XGBoost para cada fold
parametros <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  grow_policy = "lossguide",
  tree_method = 'hist',
  eta = 0.04,
  max_depth = 10
)
for (i in seq_along(folds)) {
  f           <- folds[[i]]
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  xgb.train   <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(train, -clase)),
                                      label = as.matrix(dplyr::select(train, clase)))
  xgb.test    <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(test, -clase)),
                                      label = as.matrix(dplyr::select(test, clase)))
  modelo.xgb  <- xgboost::xgb.train(data = xgb.train, verbose = 2, nrounds = 100,
                                    watchlist = list(train = xgb.train, test = xgb.test),
                                    nthread = 8, params = parametros)
  pred        <- predict(modelo.xgb, xgb.test) 
  resultados  <- data.frame(observado = test$clase, predicho = ifelse(pred >= 0.5, 1, 0))
  conf.mat    <- caret::confusionMatrix(table(resultados))
  metricas    <- rbind(metricas, 
                       data.frame(algoritmo = "XGB", fold = i, accuracy = conf.mat$overall['Accuracy'],
                                  f1 = conf.mat$byClass['F1'], kappa = conf.mat$overall['Kappa'], precision = conf.mat$byClass['Precision'],
                                  recall = conf.mat$byClass['Recall'])
  )
}

# e) Entrenar y validar con un GLM para cada fold
for (i in seq_along(folds)) {
  f           <- folds[[i]]
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  modelo.glm  <- glm(formula = as.factor(clase) ~ ., family = 'binomial', data = train)
  pred        <- predict(modelo.glm, test, type = 'response')  
  resultados  <- data.frame(observado = test$clase, predicho = ifelse(pred >= 0.5, 1, 0))
  conf.mat    <- caret::confusionMatrix(table(resultados))
  metricas    <- rbind(metricas, 
                       data.frame(algoritmo = "GLM", fold = i, accuracy = conf.mat$overall['Accuracy'],
                                  f1 = conf.mat$byClass['F1'], kappa = conf.mat$overall['Kappa'], precision = conf.mat$byClass['Precision'],
                                  recall = conf.mat$byClass['Recall'])
  )
}

# f) Calcular metricas promedio
metricas.promedio <- metricas %>%
  dplyr::group_by(algoritmo) %>%
  dplyr::summarise(accuracy = mean(accuracy), f1 = mean(f1), kappa = mean(kappa),
                   precision = mean(precision), recall = mean(recall))
readr::write_csv(x = metricas.promedio, path = "~/metricas.csv")
# ------------------------------------------------------------------------------