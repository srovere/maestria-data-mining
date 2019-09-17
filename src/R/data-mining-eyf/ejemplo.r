# -----------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----
# -----------------------------------------------------------------------------#
rm(list = ls()); gc()
Sys.setenv(TZ = "UTC")
list.of.packages <- c("caret", "data.table", "doSNOW", "dplyr", "foreach", 
                      "futile.logger", "parallel", "R6", "rBayesianOptimization",
                      "rpart", "snow", "utils", "xgboost", "yaml")
for (pack in list.of.packages) {
  if (! require(pack, character.only = TRUE)) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}
rm(pack); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- II. Leer archivo de configuracion ----
# -----------------------------------------------------------------------------#
args <- base::commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  archivo.config <- args[1]
} else {
  # No vino el archivo de configuracion por linea de comandos. Utilizo un archivo default
  archivo.config <- paste0(getwd(), "/configuracion.yml")
}

if (! file.exists(archivo.config)) {
  stop(paste0("El archivo de configuracion de ", archivo.config, " no existe\n"))
} else {
  cat(paste0("Leyendo archivo de configuracion ", archivo.config, "...\n"))
  config <- yaml.load_file(archivo.config)
}

rm(archivo.config, args); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- III. Cargar funciones de librerias propias. Crear logger ----
# -----------------------------------------------------------------------------#
source(file = paste0(config$dir$lib, "/io.r"), echo = FALSE)
source(file = paste0(config$dir$lib, "/feature_engineering.r"), echo = FALSE)
source(file = paste0(config$dir$lib, "/model.r"), echo = FALSE)
source(file = paste0(config$dir$lib, "/parameter_search.r"), echo = FALSE)
source(file = paste0(config$dir$lib, "/performance.r"), echo = FALSE)
source(file = paste0(config$dir$lib, "/prediction.r"), echo = FALSE)

logger <- Logger$new(log.level = INFO)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- IV. Leer set de datos y realizar operaciones basicas de FE ----
# -----------------------------------------------------------------------------#
logger$info("Leyendo conjunto de datos")
set.datos <- leer_set_datos(config$dir$input, "201902") %>%
  dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
  dplyr::select(-clase_ternaria)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- IV. Realizar ejecucion de prueba ----
# -----------------------------------------------------------------------------#

# Grid search paralelizado (arboles de decision)
start.time        <- proc.time()
grilla.parametros <- purrr::cross_df(list(
  xval = 0,
  cp = c(0.0001, 0.0005, 0.001), 
  ms = c(20, 50, 100), 
  mb = c(7, 10, 15), 
  md = c(5, 10, 15, 20)
))
resultados.rpart.gs  <- ps_grid_search(set.datos = set.datos, clase = "clase", semillas = config$semillas, 
                                       proporcion_train = 0.7, funcion_modelo = m_arbol_decision, 
                                       funcion_prediccion = pr_basica, grilla.parametros = grilla.parametros)
end.time             <- proc.time()
elapsed.time         <- end.time[3] - start.time[3]
cat("Tiempo:", elapsed.time, "segundos")
resultados.rpart.gs.promedio <- resultados.rpart.gs %>% 
  dplyr::group_by(xval, cp, ms, mb, md) %>% 
  dplyr::summarise(ganancia_promedio = mean(ganancia_test), ganancia_desvio = sd(ganancia_test),
                   roc_auc_promedio = mean(roc_auc_test), roc_auc_desvio = sd(roc_auc_test))

# Optimizacion bayesiana (arboles de decision)
start.time         <- proc.time()
limites.parametros <- list(
  xval = c(0, 0),
  cp = c(0.0001, 0.001),
  ms = c(20, 100),
  mb = c(7, 15),
  md = c(5, 20)
)
resultados.rpart.bo <- ps_bayesian_optimization(set.datos = set.datos, clase = "clase", semillas = config$semillas, 
                                                proporcion_train = 0.7, funcion_modelo = m_arbol_decision, 
                                                funcion_prediccion = pr_basica, limites.parametros = limites.parametros)
end.time          <- proc.time()
elapsed.time      <- end.time[3] - start.time[3]
cat("Tiempo:", elapsed.time, "segundos")  

# 
# Optimizacion bayesiana (XGBoost)
# start.time         <- proc.time()
# limites.parametros <- list(
#   eta = c(0.01, 0.3),
#   max_depth = c(20, 40),
#   gamma = c(1, 5),
#   subsample = c(0.5, 0.8),
#   colsample_bytree = c(0.5, 0.9),
#   nrounds = c(100, 500)
# )
# 
# funcion_modelo    <- m_xgboost_closure(booster = "gbtree", objective = "binary:logistic", eval_metric = "mlogloss")
# resultados.xbg.bo <- ps_bayesian_optimization(set.datos = set.datos, clase = "clase", semillas = config$semillas, 
#                                               proporcion_train = 0.7, funcion_modelo = funcion_modelo, 
#                                               init_points = 2, n_iter = 2,
#                                               funcion_prediccion = pr_xgboost, limites.parametros = limites.parametros)
# end.time          <- proc.time()
# elapsed.time      <- end.time[3] - start.time[3]
# cat("Tiempo:", elapsed.time, "segundos") 

set.seed(config$semillas[1])
train_casos <- caret::createDataPartition(set.datos[, "clase"], p = 0.7, list = FALSE)
train       <- set.datos[  train_casos, ]
test        <- set.datos[ -train_casos, ]
xgb.train   <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(train, -clase)),
                                    label = as.matrix(dplyr::select(train, clase)))
xgb.test    <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(test, -clase)),
                                    label = as.matrix(dplyr::select(test, clase)))

parametros <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "logloss",
  #tree_method = 'gpu_hist',
  eta = 0.01,
  max_depth = 25,
  gamma = 3,
  subsample = 0.75,
  colsample_bytree = 1
)

set.seed(config$semillas[1])
modelo         <- xgboost::xgb.train(data = xgb.train, nrounds = 750, verbose = 2, 
                                     nthread = parallel::detectCores(), params = parametros)
xgb.pred.test  <- data.frame(pred = predict(modelo, xgb.test, reshape = T))
pe_ganancia(probabilidades = xgb.pred.test$pred, clase = test$clase, proporcion = 0.3)

# ------------------------------------------------------------------------------