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
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Evaluación modelos con CV ----
# -----------------------------------------------------------------------------#

# a) Generar los folds
set.seed(0)
folds    <- caret::createFolds(seq_len(nrow(datos.muestras)), k = 5)
metricas <- NULL

# b) funcion para Obtener metricas en distintos puntos de corte
MetricasCutoff <- function(algoritmo, fold, obs, probs, cutoff = seq(from = 0.01, to = 0.5, by = 0.01)) {
  metricas <- NULL
  for (corte in cutoff) {
    conf.mat    <- caret::confusionMatrix(data = factor(ifelse(probs >= corte, 1, 0), levels = c(0, 1)),
                                          reference = as.factor(obs),
                                          positive = "1")
    metricas    <- rbind(metricas, 
                         data.frame(algoritmo = algoritmo, fold = fold, cutoff = corte, accuracy = conf.mat$overall['Accuracy'],
                                    f1 = conf.mat$byClass['F1'], kappa = conf.mat$overall['Kappa'], precision = conf.mat$byClass['Precision'],
                                    recall = conf.mat$byClass['Recall'])
                         
    )
  }
  return (metricas)
}

# c) Entrenar y validar con CART para cada fold
for (i in seq_along(folds)) {
  f           <- folds[[i]]
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  modelo.cart <- rpart::rpart(as.factor(clase) ~ ., 
                              data = train, 
                              method = 'class', 
                              control = list(maxdepth = 10))
  pred        <- predict(modelo.cart, test, type = 'prob')[,"1"]
  metricas    <- rbind(metricas, MetricasCutoff(algoritmo = "CART", fold = i, obs = test$clase, probs = pred))
}

# d) Entrenar y validar con RF para cada fold
for (i in seq_along(folds)) {
  f           <- folds[[i]]
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  modelo.rf   <- randomForest::randomForest(x = dplyr::select(train, -clase), ntree = 100,
                                            y = as.factor(dplyr::pull(dplyr::select(train, clase))),
                                            importance = TRUE)
  pred        <- predict(modelo.rf, test, type = 'prob')[,"1"]
  metricas    <- rbind(metricas, MetricasCutoff(algoritmo = "RF", fold = i, obs = test$clase, probs = pred))
}

# e) Entrenar y validar con XGBoost para cada fold
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
  modelo.xgb  <- xgboost::xgb.train(data = xgb.train, nrounds = 100, nthread = 8, params = parametros)
  pred        <- predict(modelo.xgb, xgb.test)
  metricas    <- rbind(metricas, MetricasCutoff(algoritmo = "XGB", fold = i, obs = test$clase, probs = pred))
}

# f) Entrenar y validar con un GLM para cada fold
for (i in seq_along(folds)) {
  f           <- folds[[i]]
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  modelo.glm  <- glm(formula = as.factor(clase) ~ ., family = 'binomial', data = train)
  pred        <- predict(modelo.glm, test, type = 'response')
  metricas    <- rbind(metricas, MetricasCutoff(algoritmo = "GLM", fold = i, obs = test$clase, probs = pred))
}

# g) Entrenar y validar con GLM, RF y XGB para cada fold
for (i in seq_along(folds)) {
  # i. Obtener conjuntos de train/validacion
  f           <- folds[[i]]
  train       <- datos.muestras[-f, ]
  test        <- datos.muestras[f, ]
  xgb.train   <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(train, -clase)),
                                      label = as.matrix(dplyr::select(train, clase)))
  xgb.test    <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(test, -clase)),
                                      label = as.matrix(dplyr::select(test, clase)))
  
  # ii. Entrenar modelos
  modelo.glm  <- glm(formula = as.factor(clase) ~ ., family = 'binomial', data = train)
  modelo.rf   <- randomForest::randomForest(x = dplyr::select(train, -clase), ntree = 100,
                                            y = as.factor(dplyr::pull(dplyr::select(train, clase))),
                                            importance = TRUE)
  modelo.xgb  <- xgboost::xgb.train(data = xgb.train, nrounds = 100, nthread = 8, params = parametros)
  
  # iii. Realizar predicciones
  pred.glm      <- predict(modelo.glm, test, type = 'response')  
  pred.rf       <- predict(modelo.rf, test, type = 'prob')[,"1"]
  pred.xgb      <- predict(modelo.xgb, xgb.test)
  pred.ensamble <- (pred.glm + pred.rf + pred.xgb) / 3
  
  # iv. Obtener resultados (clase) y metricas
  metricas <- rbind(metricas, MetricasCutoff(algoritmo = "Ensamble", fold = i, obs = test$clase, probs = pred.ensamble))
}

# h) Calcular metricas promedio
metricas.promedio <- metricas %>%
  dplyr::group_by(algoritmo, cutoff) %>%
  dplyr::summarise(accuracy = mean(accuracy), f1 = mean(f1), kappa = mean(kappa),
                   precision = mean(precision), recall = mean(recall))
readr::write_csv(x = metricas.promedio, path = "~/metricas_cutoff.csv")

# i) Graficar
metricas.grafico <- tidyr::pivot_longer(data = metricas.promedio, names_to = "metrica", values_to = "valor", 
                                        cols = c(-algoritmo, -cutoff)) %>%
  dplyr::filter(cutoff >= 0.3)
ggplot2::ggplot(data = metricas.grafico) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = cutoff, y = valor, col = metrica)) + 
  ggplot2::facet_wrap(~algoritmo, ncol = 1)
# ------------------------------------------------------------------------------