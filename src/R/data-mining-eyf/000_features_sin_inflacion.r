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
train <- leer_set_datos_mensuales(paste0(config$dir$input, "/months"), 
                                  fecha.desde = as.Date("2019-02-01"),
                                  fecha.hasta = as.Date("2019-02-01")) %>%
  dplyr::mutate(clase = fe_clase_binaria_unificada(clase_ternaria)) %>%
  dplyr::select(-clase_ternaria)
test <- leer_set_datos_mensuales(paste0(config$dir$input, "/months"), 
                                 fecha.desde = as.Date("2019-04-01"),
                                 fecha.hasta = as.Date("2019-04-01")) %>%
  dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
  dplyr::select(-clase_ternaria)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- IV. Realizar ejecucion de prueba ----
# -----------------------------------------------------------------------------#

# --- XGBoost - una sola corrida
xgb.train <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(train, -clase)),
                                  label = as.matrix(dplyr::select(train, clase)))
xgb.test  <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(test, -clase)),
                                  label = as.matrix(dplyr::select(test, clase)))

parametros <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  groy_policy = "lossguide",
  tree_method = 'hist',
  eta = 0.009235036,
  max_depth = 14,
  gamma = 2.157951,
  subsample =  0.7654432,
  colsample_bytree = 0.7868235,
  min_child_weight = 4.528278
)

set.seed(config$semillas[1])
start.time     <- proc.time()
modelo         <- xgboost::xgb.train(data = xgb.train, nrounds = 500, verbose = 2, maximize = FALSE,
                                     feval = pe_perdida_xgboost,
                                     watchlist = list(train = xgb.train, test = xgb.test),
                                     params = parametros)
xgb.pred.test  <- data.frame(pred = predict(modelo, xgb.test, reshape = T))
pe_maxima_ganancia(probabilidades = xgb.pred.test$pred, clase = test$clase, proporcion = 1)
end.time       <- proc.time()
elapsed.time   <- end.time[3] - start.time[3]
logger$info(paste0("Tiempo:", elapsed.time, "segundos"))

# --- XGBoost con optimizacion basada en algoritmos geneticos
# start.time         <- proc.time()
# limites.parametros <- list(
#   eta = c(0.001, 0.01),
#   max_depth = c(10, 20),
#   gamma = c(1, 5),
#   subsample = c(0.5, 1),
#   colsample_bytree = c(0.5, 1),
#   min_child_weight = c(1, 10),
#   scale_pos_weight = c(1, 100)
# )
# 
# funcion_modelo    <- m_xgboost_closure(booster = "gbtree", objective = "binary:logistic", eval_metric = pe_perdida_xgboost, 
#                                        tree_method = "hist", grow_policy = "lossguide", nrounds = 500)
# resultados.xgb.ga <- ps_ga_optimization(set.datos = train, set.datos.test = test, clase = "clase", semillas = config$semillas, 
#                                         proporcion_train = 0.7, funcion_modelo = funcion_modelo, 
#                                         max_iterations = 10, tamano_poblacion = 20, run = 50,
#                                         funcion_prediccion = pr_xgboost, limites.parametros = limites.parametros, 
#                                         logger = logger)
# end.time          <- proc.time()
# elapsed.time      <- end.time[3] - start.time[3]
# logger$info(paste0("Tiempo:", elapsed.time, "segundos"))
# save(resultados.xgb.ga, file = "/home/santiago/xgboost.ga.RData")
# ------------------------------------------------------------------------------