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
# --- PASO 2. Referenciar los datos ----
# -----------------------------------------------------------------------------#

# a) Definir clases
clases    <- c("NoAgua", "Agua")
colores   <- c("#543005", "#003c30")
clases.df <- data.frame(id = c(0, 1), nombre = clases)

# b) Definir funcion para cargar raster y asignarle la clase
CargarGroundTruth <- function(filename, clases) {
  ground.truth         <- raster::raster(x = filename)
  ground.truth         <- raster::ratify(ground.truth)
  rat                  <- data.frame(ID = seq(from = 0, to = length(clases) - 1),
                                     clase = clases)
  levels(ground.truth) <- rat
  return (ground.truth)
}

# c) Cargar rasters de clases
ground.truth.agua    <- CargarGroundTruth(filename = paste0(images.directory, "/GT-Positive.tif"),
                                          clases = clases)
ground.truth.no.agua <- CargarGroundTruth(filename = paste0(images.directory, "/GT-Negative.tif"),
                                          clases = clases)

# d) Mostrar conjunto de entrenamiento
plt.gt.agua    <- rasterVis::levelplot(ground.truth.agua, col.regions = colores, main = 'Ground truth (Agua - 2019)')
plt.gt.no.agua <- rasterVis::levelplot(ground.truth.no.agua, col.regions = colores, main = 'Ground truth (No Agua - 2018)')
plot(plt.gt.agua)
plot(plt.gt.no.agua)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Definir sitios de muestreo ----
# -----------------------------------------------------------------------------#

# a) Definir extent de ground truth
ground.truth.extent <- sf::st_read(dsn = paste0(working.directory, "/ground_truth"), layer = "Flood") %>%
  sf::st_set_crs(x = ., value = 22185) %>%
  raster::extent()

# b) Definir semilla
set.seed(0)

# c) Generar muestra random (se utiliza cualquiera de los 2 rasters)
numero.muestras <- 400000
muestra         <- raster::sampleRandom(ground.truth.agua, size = numero.muestras, 
                                        ext = ground.truth.extent, na.rm = TRUE, sp = TRUE)
table(muestra$GT.Positive)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Extraer muestras ----
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

# b) Extraer muestras para 201811 y 201901
datos.muestras <- rbind(
  ExtraerMuestra(filename = paste0(images.directory, "/201801.tif"), muestra = muestra, clase = 0),
  ExtraerMuestra(filename = paste0(images.directory, "/201901.tif"), muestra = muestra, clase = 1)
)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 5. Evaluación modelos con CV ----
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
  modelo.rf   <- randomForest::randomForest(x = dplyr::select(train, -clase), ntree = 500,
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
  groy_policy = "lossguide",
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
  modelo.xgb  <- xgboost::xgb.train(data = xgb.train, verbose = 2, nrounds = 500,
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
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 6. Entrenar el modelo ----
# -----------------------------------------------------------------------------#

# a) Crear modelo usando un arbol de decision CART
set.seed(0)
modelo.cart <- rpart::rpart(as.factor(clase) ~ ., 
                            data = datos.muestras, 
                            method = 'class', 
                            control = list(maxdepth = 10))
plot(modelo.cart, uniform = TRUE, main = "Arbol de clasificacion")
text(modelo.cart, cex = 0.8)

# b) Crear modelo usando RandomForest
set.seed(0)
modelo.rf <- randomForest::randomForest(x = dplyr::select(datos.muestras, -clase), ntree = 500,
                                        y = as.factor(dplyr::pull(dplyr::select(datos.muestras, clase))),
                                        importance = TRUE)
randomForest::varImpPlot(modelo.rf)

# c) Crear modelo usando XGBoost
set.seed(0)
xgb.train  <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(datos.muestras, -clase)),
                                   label = as.matrix(dplyr::select(datos.muestras, clase)))
modelo.xgb <- xgboost::xgb.train(data = xgb.train, nrounds = 500, varbose = 0,
                                 nthread = 8, params = parametros)
xgboost::xgb.plot.importance(xgboost::xgb.importance(model = modelo.xgb))

# d) Crear modelo usando GLM
set.seed(0)
modelo.glm <- glm(formula = clase ~ ., family = 'binomial', data = datos.muestras)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 7. Predecir ----
# -----------------------------------------------------------------------------#

# a) Definicion de modelo
set.seed(0)
modelo <- modelo.rf
sufijo <- "RF"

# b) Prediccion para 201801
raster::beginCluster(n = 8)
prediccion.sin.inundacion <- raster::clusterR(
  x = CargarImagen(paste0(images.directory, "/201801.tif")), 
  fun = raster::predict,
  args = list(model = modelo, type = 'class'),
  filename = paste0(images.directory, "/predict_201801_", sufijo, ".tif"),
  progress = 'text',
  overwrite = TRUE
)
raster::endCluster()
raster::removeTmpFiles(h = 0)

# c) Prediccion para 201811
raster::beginCluster(n = 8)
prediccion.sin.inundacion2 <- raster::clusterR(
  x = CargarImagen(paste0(images.directory, "/201811.tif")), 
  fun = raster::predict,
  args = list(model = modelo, type = 'class'),
  filename = paste0(images.directory, "/predict_201811_", sufijo, ".tif"),
  progress = 'text',
  overwrite = TRUE
)
raster::endCluster()
raster::removeTmpFiles(h = 0)

# d) Prediccion para 201901
raster::beginCluster(n = 8)
prediccion.con.inundacion <- raster::clusterR(
  x = CargarImagen(paste0(images.directory, "/201901.tif")), 
  fun = raster::predict,
  args = list(model = modelo, type = 'class'),
  filename = paste0(images.directory, "/predict_201901_", sufijo, ".tif"),
  progress = 'text',
  overwrite = TRUE
)
raster::endCluster()
raster::removeTmpFiles(h = 0)

# e) Calcular diferencia
prediccion.diferencia <- raster::calc(
  x = raster::stack(prediccion.sin.inundacion, prediccion.sin.inundacion2, prediccion.con.inundacion), 
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
raster::writeRaster(x = prediccion.diferencia, format = "GTiff",
                    filename = paste0(images.directory, "/predict_diferencia_", sufijo, ".tif"))
raster::removeTmpFiles(h = 0)

# f) Eliminar cuerpos de agua estables
water.bodies          <- raster::raster(paste0(images.directory, "/GT-WaterBodies.tif"))
prediccion.inundacion <- raster::mask(
  x = prediccion.diferencia,
  mask = water.bodies,
  maskvalue = 1, updatevalue = 0,
  filename = paste0(images.directory, "/predict_inundacion_", sufijo, ".tif")
)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 8. Validar prediccion en ground truth positiva ----
# -----------------------------------------------------------------------------#

# a) Enmascarar datos
validacion.modelo <- raster::mask(
  x = prediccion.inundacion,
  mask = ground.truth.agua
)

# b) Obtener valores
valores.validacion <- raster::values(x = validacion.modelo)
valores.validacion <- valores.validacion[!is.na(valores.validacion)]
tabla.validacion   <- table(valores.validacion)

# c) Calcular Recall
recall <- tabla.validacion[2] / sum(tabla.validacion)
# ------------------------------------------------------------------------------