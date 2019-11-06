# -----------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----
# -----------------------------------------------------------------------------#
rm(list = ls()); gc()
Sys.setenv(TZ = "UTC")
list.of.packages <- c("caret", "data.table", "dplyr", "futile.logger", "R6",
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
  archivo.config <- paste0(getwd(), "/configuracion_linea_muerte.yml")
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
# --- IV. Definir conjunto de hiperparametros ----
# -----------------------------------------------------------------------------#
logger$info("Generando conjunto de hiperparametros")

parametros <- append(list(
  booster = "gbtree",
  objective = "binary:logistic",
  groy_policy = "lossguide",
  tree_method = 'hist'
), config$hiperparametros)

hiperparametros <- as.data.frame(config$hiperparametros)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- V. Realizar ejecucion de linea de muerte ----
# -----------------------------------------------------------------------------#

resultados.linea.muerte <- NULL
for (periodo.test in as.character(seq(from = as.Date(config$fecha.desde), to = as.Date(config$fecha.hasta), by = 'months'))) {
  logger$info(paste0("Leyendo conjunto de datos de train/test para ", periodo.test))  
  
  # Lectura de conjunto de datos de test
  test <- leer_set_datos_mensuales(paste0(config$dir$input, "/months"), 
                                   fecha.desde = as.Date(periodo.test),
                                   fecha.hasta = as.Date(periodo.test)) %>%
    dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
    dplyr::select(-clase_ternaria)
  
  # Lectura de conjunto de datos de train
  train <- leer_set_datos_mensuales(paste0(config$dir$input, "/months"), 
                                    fecha.desde = as.Date(periodo.test) - months(config$offset.meses.train.test + config$meses.entrenamiento - 1),
                                    fecha.hasta = as.Date(periodo.test) - months(config$offset.meses.train.test)) %>%
    dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
    dplyr::select(-clase_ternaria)

  # Definir conjuntos de train/test para XGBoost
  xgb.train <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(train, -clase)),
                                    label = as.matrix(dplyr::select(train, clase)))
  xgb.test  <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(test, -clase)),
                                    label = as.matrix(dplyr::select(test, clase)))
  rm(train)
  gc(full = TRUE)

  # Ejecutar XGBoost para todas las semillas
  resultados.periodo <- purrr::map_dfr(
  	.x = config$semillas,
  	.f = function(semilla) {
  	  logger$info(paste0("... Ejecutando entrenamiento para semilla ", semilla))
  		set.seed(semilla)
  		modelo         <- xgboost::xgb.train(data = xgb.train, nrounds = config$rondas.entrenamiento, 
  		                                     verbose = 2, maximize = FALSE, feval = pe_perdida_xgboost,
  		                                     watchlist = list(train = xgb.train, test = xgb.test),
  		                                     params = parametros)
  		logger$info(paste0("... Calculando ganancia para semilla ", semilla))
  		xgb.pred.test  <- data.frame(pred = predict(modelo, xgb.test, reshape = T))
  		ganancia       <- pe_ganancia(probabilidades = xgb.pred.test$pred, clase = test$clase, proporcion = 1)
  		return (dplyr::bind_cols(
  		  hiperparametros,
  		  data.frame(semilla = semilla, ganancia = ganancia)
  		))
  	}
  ) %>% dplyr::mutate(meses_entrenamiento = config$meses.entrenamiento, periodo_test = as.Date(periodo.test))
  resultados.linea.muerte <- dplyr::bind_rows(resultados.linea.muerte, resultados.periodo)
  
  # Guardar datos a archivo RData
  save(resultados.linea.muerte, file = paste0(config$dir$output, "/LineaMuerte.RData"))
}
# ------------------------------------------------------------------------------
