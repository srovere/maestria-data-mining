# -----------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----
# -----------------------------------------------------------------------------#
rm(list = ls()); gc()
Sys.setenv(TZ = "UTC")
list.of.packages <- c("caret", "data.table", "dplyr", "futile.logger", "GA", "R6", 
                      "ROCR", "utils", "xgboost", "yaml")
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
  archivo.config <- paste0(getwd(), "/configuracion_seleccion_variables.yml")
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
logger$info(paste0("Leyendo conjunto de datos para entrenamiento desde ", config$periodo.train$desde, 
                   " hasta ", config$periodo.train$hasta))
set.datos <- leer_set_datos_mensuales(paste0(config$dir$input), 
                                      fecha.desde = as.Date(config$periodo.train$desde),
                                      fecha.hasta = as.Date(config$periodo.train$hasta)) %>%
  dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
  dplyr::select(-clase_ternaria)

logger$info(paste0("Leyendo conjunto de datos para validacion desde ", config$periodo.test$desde, 
                   " hasta ", config$periodo.test$hasta))
set.datos.test <- leer_set_datos_mensuales(paste0(config$dir$input), 
                                           fecha.desde = as.Date(config$periodo.test$desde),
                                           fecha.hasta = as.Date(config$periodo.test$hasta)) %>%
  dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
  dplyr::select(-clase_ternaria)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- V. Realizar hiperparametrizacion ----
# -----------------------------------------------------------------------------#

# --- XGBoost con optimizacion bayesiana
parametros        <- append(list(
  booster = "gbtree",
  objective = "binary:logistic",
  grow_policy = "lossguide",
  tree_method = 'hist'
), config$hiperparametros)
funcion_modelo    <- m_xgboost_closure(booster = "gbtree", objective = "binary:logistic", eval_metric = pe_perdida_xgboost,
                                       tree_method = "hist", grow_policy = "lossguide", nrounds = config$nrounds)
resultados.xgb.ga <- ps_ga_feature_selection(set.datos = set.datos, set.datos.test = set.datos.test ,clase = "clase", 
                                             semillas = config$semilla, proporcion_train = 1, funcion_modelo = funcion_modelo,
                                             max_iterations = 50, tamano_poblacion = 25, run = 10,
                                             funcion_prediccion = pr_xgboost, parametros = parametros,
                                             logger = logger)
save(resultados.xgb.ga, file = paste0(config$dir$output, "/seleccion_variables.RData"))
# ------------------------------------------------------------------------------