# -----------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----
# -----------------------------------------------------------------------------#
rm(list = ls()); gc()
Sys.setenv(TZ = "UTC")
list.of.packages <- c("caret", "data.table", "doSNOW", "dplyr", "foreach", 
                      "futile.logger", "GA", "parallel", "R6", "mlrMBO", "rgenoud",
                      "ROCR", "rpart", "snow", "utils", "xgboost", "yaml")
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

# # --- XGBoost - una sola corrida
# start.time  <- proc.time()
# set.seed(config$semillas[1])
# train_casos <- caret::createDataPartition(set.datos[, "clase"], p = 0.7, list = FALSE)
# train       <- set.datos[  train_casos, ]
# test        <- set.datos[ -train_casos, ]
# xgb.train   <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(train, -clase)),
#                                     label = as.matrix(dplyr::select(train, clase)))
# xgb.test    <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(test, -clase)),
#                                     label = as.matrix(dplyr::select(test, clase)))
# 
# parametros <- list(
#   booster = "gbtree",
#   objective = "binary:logistic",
#   groy_policy = "lossguide",
#   tree_method = 'hist',
#   eta = 0.01,
#   max_depth = 10,
#   gamma = 3,
#   subsample = 0.5,
#   colsample_bytree = 1
# )
# 
# set.seed(config$semillas[1])
# modelo         <- xgboost::xgb.train(data = xgb.train, nrounds = 500, verbose = 2, 
#                                      feval = pe_perdida_xgboost, watchlist = list(train = xgb.train, test = xgb.test),
#                                      nthread = parallel::detectCores(), params = parametros)
# xgb.pred.test  <- data.frame(pred = predict(modelo, xgb.test, reshape = T))
# pe_ganancia(probabilidades = xgb.pred.test$pred, clase = test$clase, proporcion = 0.3)
# end.time       <- proc.time()
# elapsed.time   <- end.time[3] - start.time[3]
# logger$info(paste0("Tiempo:", elapsed.time, "segundos"))
# 
# # --- XGBoost con grid search de hiperparametros
# start.time        <- proc.time()
# grilla.parametros <- purrr::cross_df(list(
#   eta = c(0.01, 0.05, 0.1),
#   max_depth = c(10, 15, 20),
#   gamma = c(1, 3, 5),
#   subsample = c(0.5, 0.75, 1),
#   colsample_bytree = c(1)
# ))
# 
# funcion_modelo    <- m_xgboost_closure(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss", 
#                                        tree_method = "hist", grow_policy = "lossguide", nrounds = 500)
# resultados.xgb.gs <- ps_grid_search(set.datos = set.datos, clase = "clase", semillas = config$semillas, 
#                                     proporcion_train = 0.7, funcion_modelo = funcion_modelo, logger = logger,
#                                     funcion_prediccion = pr_xgboost, grilla.parametros = grilla.parametros)
# end.time          <- proc.time()
# elapsed.time      <- end.time[3] - start.time[3]
# logger$info(paste0("Tiempo:", elapsed.time, "segundos"))
# resultados.xgb.gs.promedio <- resultados.xgb.gs %>% 
#   dplyr::group_by(eta, max_depth, gamma, subsample, colsample_bytree, nrounds) %>% 
#   dplyr::summarise(ganancia_promedio = mean(ganancia_test), ganancia_desvio = sd(ganancia_test),
#                    roc_auc_promedio = mean(roc_auc_test), roc_auc_desvio = sd(roc_auc_test))

# --- XGBoost con optimizacion bayesiana
# start.time         <- proc.time()
# limites.parametros <- ParamHelpers::makeParamSet(
#   ParamHelpers::makeNumericParam("eta", lower = 0.001, upper = 0.01),
#   ParamHelpers::makeIntegerParam("max_depth", lower = 10, upper = 20),
#   ParamHelpers::makeNumericParam("gamma", lower = 1, upper = 5),
#   ParamHelpers::makeNumericParam("subsample", lower = 0.5, upper = 1),
#   ParamHelpers::makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
#   ParamHelpers::makeNumericParam("min_child_weight", lower = 1, upper = 10),
#   ParamHelpers::makeNumericParam("alpha", lower = 0, upper = 10),
#   ParamHelpers::makeNumericParam("lambda", lower = 1, upper = 10)
# )
# 
# funcion_modelo    <- m_xgboost_closure(booster = "gbtree", objective = "binary:logistic", eval_metric = pe_perdida_xgboost, 
#                                        tree_method = "hist", grow_policy = "lossguide", nrounds = 500)
# resultados.xgb.bo <- ps_bayesian_optimization(set.datos = set.datos, clase = "clase", semillas = config$semillas, 
#                                               proporcion_train = 0.7, funcion_modelo = funcion_modelo, 
#                                               n_iter = 100, init_points = 5 * length(limites.parametros$pars),
#                                               funcion_prediccion = pr_xgboost, 
#                                               limites.parametros = limites.parametros, logger = logger,
#                                               file_persistence_path = paste0(getwd(), "/output/xgboost.mbo.RData"))
# end.time          <- proc.time()
# elapsed.time      <- end.time[3] - start.time[3]
# logger$info(paste0("Tiempo:", elapsed.time, "segundos"))
# save(resultados.xgb.bo, file = "/home/srovere/xgboost.mbo.RData")

# --- XGBoost con optimizacion basada en algoritmos geneticos
start.time         <- proc.time()
limites.parametros <- list(
  eta = c(0.001, 0.01),
  max_depth = c(10, 20),
  gamma = c(1, 5),
  subsample = c(0.5, 1),
  colsample_bytree = c(0.5, 1),
  min_child_weight = c(1, 10),
  alpha = c(0, 10),
  lambda = c(1, 10)
)

funcion_modelo    <- m_xgboost_closure(booster = "gbtree", objective = "binary:logistic", eval_metric = pe_perdida_xgboost,
                                       tree_method = "hist", grow_policy = "lossguide", nrounds = 500)
resultados.xgb.ga <- ps_ga_optimization(set.datos = set.datos, clase = "clase", semillas = config$semillas,
                                        proporcion_train = 0.7, funcion_modelo = funcion_modelo,
                                        max_iterations = 20, tamano_poblacion = 50, run = 10,
                                        funcion_prediccion = pr_xgboost, limites.parametros = limites.parametros,
                                        logger = logger)
end.time          <- proc.time()
elapsed.time      <- end.time[3] - start.time[3]
logger$info(paste0("Tiempo:", elapsed.time, "segundos"))
save(resultados.xgb.ga, file = "/home/srovere/xgboost.ga.RData")
# ------------------------------------------------------------------------------