# -----------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----
# -----------------------------------------------------------------------------#
rm(list = ls()); gc()
Sys.setenv(TZ = "UTC")
list.of.packages <- c("caret", "data.table", "dplyr", "futile.logger", "R6", 
                      "magrittr", "mlrMBO", "rgenoud", "ROCR", "utils", "xgboost", 
                      "yaml")
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
  archivo.config <- paste0(getwd(), "/configuracion_hiperparametrizacion.yml")
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

# Si se indico agregar features "extra", cargarlos de archivo
features.extra <- NULL
if (! is.null(config$archivo.features.extra)) {
  features.extra <- base::readRDS(paste0(config$dir$extra, "/", config$archivo.features.extra))
}

# Cargar conjunto de datos de entrenamiento
if (! is.null(config$meses.entrenamiento$lista.meses)) {
  logger$info(paste0("Leyendo conjunto de datos de entrenamiento para los meses ", paste0(config$meses.entrenamiento$lista.meses, collapse = ", ")))
  set.datos <- leer_set_datos_mensuales_meses_varios(input.dir = paste0(config$dir$input), 
                                                     fechas.meses = as.Date(config$meses.entrenamiento$lista.meses),
                                                     proporcion = config$proporcion.undersampling) %>%
    dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
    dplyr::select(-clase_ternaria)
} else {
  logger$info(paste0("Leyendo conjunto de datos de entrenamiento desde ", config$meses.entrenamiento$rango.fechas$desde, 
                     " hasta ", config$meses.entrenamiento$rango.fechas$hasta))
  set.datos <- leer_set_datos_mensuales(input.dir = paste0(config$dir$input), 
                                        fecha.desde = as.Date(config$meses.entrenamiento$rango.fechas$desde),
                                        fecha.hasta = as.Date(config$meses.entrenamiento$rango.fechas$hasta),
                                        proporcion = config$proporcion.undersampling) %>%
    dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
    dplyr::select(-clase_ternaria)
}
if (! is.null(features.extra)) {
  set.datos <- set.datos %>%
    dplyr::left_join(features.extra, by = c("numero_de_cliente", "foto_mes"))
}

# Cargar conjunto de datos de validacion
set.datos.test   <- NULL
proporcion.train <- 1
if (! is.null(config$meses.validacion$rango.fechas$desde) && ! is.null(config$meses.validacion$rango.fechas$hasta)) {
  logger$info(paste0("Leyendo conjunto de datos de validacion desde ", config$meses.validacion$rango.fechas$desde,
                     " hasta ", config$meses.validacion$rango.fechas$hasta))
  set.datos.test <- leer_set_datos_mensuales(paste0(config$dir$input), 
                                             fecha.desde = as.Date(config$meses.validacion$rango.fechas$desde),
                                             fecha.hasta = as.Date(config$meses.validacion$rango.fechas$hasta)) %>%
    dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
    dplyr::select(-clase_ternaria)
  if (! is.null(features.extra)) {
    set.datos.test <- set.datos.test %>%
      dplyr::left_join(features.extra, by = c("numero_de_cliente", "foto_mes"))
  }
} else {
  proporcion.train <- config$proporcion.train
}

# Limpiar memoria
rm(features.extra)
gc(full = TRUE)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- V. Realizar hiperparametrizacion ----
# -----------------------------------------------------------------------------#

# --- XGBoost con optimizacion bayesiana
start.time         <- proc.time()
limites.parametros <- ParamHelpers::makeParamSet(
  ParamHelpers::makeNumericParam("eta", lower = 0.005, upper = 0.1),
  ParamHelpers::makeIntegerParam("max_depth", lower = 5, upper = 20),
  ParamHelpers::makeNumericParam("gamma", lower = 1, upper = 10),
  ParamHelpers::makeNumericParam("subsample", lower = 0.20, upper = 1),
  ParamHelpers::makeNumericParam("colsample_bytree", lower = 0.2, upper = 0.7),
  ParamHelpers::makeNumericParam("min_child_weight", lower = 1, upper = 10),
  ParamHelpers::makeNumericParam("alpha", lower = 0, upper = 10),
  ParamHelpers::makeNumericParam("lambda", lower = 1, upper = 10)
)

funcion_modelo    <- m_xgboost_closure(booster = "gbtree", objective = "binary:logistic", eval_metric = pe_perdida_xgboost,
                                       tree_method = "hist", grow_policy = "lossguide", nrounds = config$nrounds)
resultados.xgb.bo <- ps_bayesian_optimization(set.datos = set.datos, set.datos.test = set.datos.test, clase = "clase", 
                                              semillas = config$semillas, proporcion_train = proporcion.train, funcion_modelo = funcion_modelo,
                                              n_iter = config$iteraciones, init_points = 5 * length(limites.parametros$pars),
                                              funcion_prediccion = pr_xgboost, limites.parametros = limites.parametros, 
                                              logger = logger, file_persistence_interval = config$file.persistence.interval,
                                              file_persistence_path = paste0(config$dir$output, "/xgboost_con_fe.mbo.RData"))
end.time          <- proc.time()
elapsed.time      <- end.time[3] - start.time[3]
logger$info(paste0("Tiempo:", elapsed.time, "segundos"))

parametros.analizados <- resultados.xgb.bo$opt.path$env$path
save(parametros.analizados, file = paste0(config$dir$output, "/hiperparametrizacion_xgboost_con_fe_parametros_analizados.mbo.RData"))
# ------------------------------------------------------------------------------