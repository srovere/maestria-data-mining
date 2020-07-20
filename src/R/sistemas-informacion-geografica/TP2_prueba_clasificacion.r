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

# a) Cargar raster de clases
water.bodies <- raster::raster(paste0(working.directory, "/ground_truth/WaterBodies.tif"))

# b) Definir clases
clases    <- c("NoAgua", "Agua")
colores   <- c("#543005", "#003c30")
clases.df <- data.frame(id = c(0, 1), nombre = clases)

# c) Asignar clases a raster
water.bodies         <- raster::ratify(water.bodies)
rat                  <- raster::levels(water.bodies)[[1]]
rat$clase            <- clases
levels(water.bodies) <- rat

# d) Mostrar conjunto de entrenamiento
plt.wb <- rasterVis::levelplot(water.bodies, col.regions = colores, main = 'Conjunto de entrenamiento')
plot(plt.wb)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Definir sitios de muestreo ----
# -----------------------------------------------------------------------------#

# a) Definir semilla
set.seed(0)

# b) Generar muestra estratificada
numero.muestras <- 10000
muestra         <- raster::sampleStratified(water.bodies, size = numero.muestras, 
                                            na.rm = TRUE, sp = TRUE)
table(muestra$WaterBodies)
save(sample, file = paste0(working.directory, "/muestra.RData"))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Extraer muestras ----
# -----------------------------------------------------------------------------#

# a) Definir imagen de entrenamiento y nombrar las bandas
imagen.entrenamiento        <- raster::stack(paste0(images.directory, "/201801.tif"))
names(imagen.entrenamiento) <- c("b2", "b3", "b4", "b8", "ndwi", "ndvi")

# b) Extraer muestras para los lugares indicados. Eliminar columna ID.
valores.muestras <- raster::extract(imagen.entrenamiento, muestra, df = TRUE)
valores.muestras <- valores.muestras[, -1]

# c) Agregar la informacion de la clase a las muestras
datos.muestras <- data.frame(clase = muestra$WaterBodies, valores.muestras) %>%
  dplyr::filter(! is.na(ndvi) & ! is.na(ndwi))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 5. Evaluación modelos con CV ----
# -----------------------------------------------------------------------------#

# a) Generar los folds
folds <- caret::createFolds(seq_len(nrow(datos.muestras)), k = 5)

# b) Entrenar y validar con CART para cada fold
suma.accuracy <- 0
suma.kappa    <- 0
for (f in folds) {
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  modelo.cart <- rpart::rpart(as.factor(clase) ~ ., 
                              data = train, 
                              method = 'class', 
                              control = list(maxdepth = 10))
  pred        <- predict(modelo.cart, test, type = 'class')  
  resultados  <- data.frame(observado = test$clase, predicho = pred)
  conf.mat    <- caret::confusionMatrix(table(resultados))
  
  suma.accuracy <- suma.accuracy + conf.mat$overall['Accuracy']
  suma.kappa    <- suma.kappa + conf.mat$overall['Kappa']
}
print(suma.accuracy/length(folds))
print(suma.kappa/length(folds))

# c) Entrenar y validar con RF para cada fold
suma.accuracy <- 0
suma.kappa    <- 0
for (f in folds) {
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  modelo.rf   <- randomForest::randomForest(x = dplyr::select(train, -clase), ntree = 100,
                                            y = as.factor(dplyr::pull(dplyr::select(train, clase))),
                                            importance = TRUE)
  pred        <- predict(modelo.rf, test, type = 'class')  
  resultados  <- data.frame(observado = test$clase, predicho = pred)
  conf.mat    <- caret::confusionMatrix(table(resultados))
  
  suma.accuracy <- suma.accuracy + conf.mat$overall['Accuracy']
  suma.kappa    <- suma.kappa + conf.mat$overall['Kappa']
}
print(suma.accuracy/length(folds))
print(suma.kappa/length(folds))

# d) Entrenar y validar con XGBoost para cada fold
suma.accuracy <- 0
suma.kappa    <- 0
parametros <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  groy_policy = "lossguide",
  tree_method = 'hist',
  eta = 0.04,
  max_depth = 10
)
for (f in folds) {
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
  
  suma.accuracy <- suma.accuracy + conf.mat$overall['Accuracy']
  suma.kappa    <- suma.kappa + conf.mat$overall['Kappa']
}
print(suma.accuracy/length(folds))
print(suma.kappa/length(folds))

# e) Entrenar y validar con un GLM para cada fold
suma.accuracy <- 0
suma.kappa    <- 0
for (f in folds) {
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  modelo.glm  <- glm(formula = as.factor(clase) ~ ndwi+ndvi, family = 'binomial', data = train)
  pred        <- predict(modelo.glm, test, type = 'response')  
  resultados  <- data.frame(observado = test$clase, predicho = ifelse(pred >= 0.5, 1, 0))
  conf.mat    <- caret::confusionMatrix(table(resultados))
  
  suma.accuracy <- suma.accuracy + conf.mat$overall['Accuracy']
  suma.kappa    <- suma.kappa + conf.mat$overall['Kappa']
}
print(suma.accuracy/length(folds))
print(suma.kappa/length(folds))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 6. Entrenar el modelo ----
# -----------------------------------------------------------------------------#

# a) Crear modelo usando un arbol de decision CART
modelo.cart <- rpart::rpart(as.factor(clase) ~ ., 
                            data = datos.muestras, 
                            method = 'class', 
                            control = list(maxdepth = 10))
plot(modelo.cart, uniform = TRUE, main = "Arbol de clasificacion")
text(modelo.cart, cex = 0.8)

# b) Crear modelo usando RandomForest
modelo.rf <- randomForest::randomForest(x = dplyr::select(datos.muestras, -clase), ntree = 100,
                                        y = as.factor(dplyr::pull(dplyr::select(datos.muestras, clase))),
                                        importance = TRUE)
randomForest::varImpPlot(modelo.rf)

# c) Crear modelo usando XGBoost
xgb.train  <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(datos.muestras, -clase)),
                                   label = as.matrix(dplyr::select(datos.muestras, clase)))
modelo.xgb <- xgboost::xgb.train(data = xgb.train, nrounds = 100, varbose = 0,
                                 nthread = 8, params = parametros)
xgboost::xgb.plot.importance(xgboost::xgb.importance(model = modelo.xgb))

# d) Crear modelo usando GLM
modelo.glm <- glm(formula = clase ~ ., family = 'binomial', data = datos.muestras)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 7. Predecir ----
# -----------------------------------------------------------------------------#

# a) Prediccion naive utilizando la misma imagen (pero ahora completa) - CART
raster::beginCluster(n = 8)
prediccion.entrenamiento <- raster::clusterR(
  x = imagen.entrenamiento, 
  fun = raster::predict,
  args = list(model = modelo.cart, type = 'class'),
  filename = paste0(images.directory, "/predict_cart_201801.tif"),
  progress = 'text'
)
raster::endCluster()

# b) Prediccion naive utilizando la misma imagen (pero ahora completa) - RF
raster::beginCluster(n = 8)
prediccion.entrenamiento <- raster::clusterR(
  x = imagen.entrenamiento, 
  fun = raster::predict,
  args = list(model = modelo.rf, type = 'class'),
  filename = paste0(images.directory, "/predict_rf_201801.tif"),
  progress = 'text'
)
raster::endCluster()

# c) Prediccion naive utilizando la misma imagen (pero ahora completa) - GLM
raster::beginCluster(n = 8)
prediccion.entrenamiento <- raster::clusterR(
  x = imagen.entrenamiento, 
  fun = raster::predict,
  args = list(model = modelo.glm),
  filename = paste0(images.directory, "/predict_glm_201801.tif"),
  progress = 'text'
)
raster::endCluster()
# ------------------------------------------------------------------------------