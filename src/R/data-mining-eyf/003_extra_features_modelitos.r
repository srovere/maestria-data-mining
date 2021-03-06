# -----------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----
# -----------------------------------------------------------------------------#
rm(list = ls()); gc()
Sys.setenv(TZ = "UTC")
list.of.packages <- c("caret", "data.table", "dplyr", "futile.logger", "magrittr",
                      "R6", "ROCR", "utils", "xgboost", "yaml")
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
  archivo.config <- paste0(getwd(), "/configuracion_extra_features_modelitos.yml")
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
source(file = paste0(config$dir$lib, "/performance.r"), echo = FALSE)
source(file = paste0(config$dir$lib, "/prediction.r"), echo = FALSE)

logger <- Logger$new(log.level = INFO)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- IV. Realizar ejecucion de linea de muerte ----
# -----------------------------------------------------------------------------#

# Definir hiperparametros
logger$info("Generando conjunto de hiperparametros")
parametros <- append(list(
  booster = "gbtree",
  objective = "binary:logistic",
  grow_policy = "lossguide",
  tree_method = 'hist'
), config$hiperparametros)

# Ejecutar entrenamiento
resumen.modelo    <- NULL
resultados.modelo <- NULL
for (periodo.test in as.character(seq(from = as.Date(config$fecha.desde), to = as.Date(config$fecha.hasta), by = 'months'))) {
  logger$info(paste0("Leyendo conjunto de datos de train/test para ", periodo.test))  
  
  # Lectura de conjunto de datos de test
  test <- leer_set_datos_mensuales(config$dir$input, 
                                   fecha.desde = as.Date(periodo.test),
                                   fecha.hasta = as.Date(periodo.test)) %>%
    dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
    dplyr::select(-clase_ternaria)
  
  # Lectura de conjunto de datos de train
  mejor.corte <- list("ganancia_mejor" = 0, "prob_corte" = 0.025)
  feval       <- NULL
  if (config$unificar.clases) {
    feval <- pe_perdida_xgboost_clases_unificadas
    train <- leer_set_datos_mensuales(config$dir$input, 
                                      fecha.desde = as.Date(periodo.test) - months(config$offset.meses.train.test + config$meses.entrenamiento - 1),
                                      fecha.hasta = as.Date(periodo.test) - months(config$offset.meses.train.test)) %>%
      dplyr::mutate(clase = fe_clase_binaria_unificada(clase_ternaria)) %>%
      dplyr::select(-clase_ternaria)
  } else {
    feval <- pe_perdida_xgboost
    train <- leer_set_datos_mensuales(config$dir$input, 
                                      fecha.desde = as.Date(periodo.test) - months(config$offset.meses.train.test + config$meses.entrenamiento - 1),
                                      fecha.hasta = as.Date(periodo.test) - months(config$offset.meses.train.test)) %>%
      dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
      dplyr::select(-clase_ternaria)
  }

  # Definir conjuntos de train/test para XGBoost
  xgb.train <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(train, -clase)),
                                    label = as.matrix(dplyr::select(train, clase)))
  xgb.test  <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(test, -clase)),
                                    label = as.matrix(dplyr::select(test, clase)))
  parametros$base_score <- mean(getinfo(xgb.train, "label"))
  
  # Ejecutar XGBoost para toda la semilla indicada
  logger$info("Ejecutando entrenamiento")
	set.seed(config$semilla)
	modelo <- xgboost::xgb.train(data = xgb.train, nrounds = config$rondas.entrenamiento, 
	                             verbose = 2, maximize = FALSE, feval = feval,
	                             watchlist = list(train = xgb.train, test = xgb.test),
	                             params = parametros)
	
	logger$info("Calculando ganancia")
	xgb.pred.test <- data.frame(pred = predict(modelo, xgb.test, reshape = T))
	ganancia      <- pe_ganancia(probabilidades = xgb.pred.test$pred, clase = test$clase, proporcion = 1, punto_corte = mejor.corte$prob_corte)
	
	# Generar resumen
	logger$info("Generando resumen")
	resumen.periodo <- dplyr::bind_cols(
	    data.frame(modelo = config$nombre.modelo, semilla = config$semilla, periodo = periodo.test,
	               punto_corte = mejor.corte$prob_corte, ganancia = ganancia),
	    as.data.frame(parametros)
	)
	resumen.modelo  <- dplyr::bind_rows(resumen.modelo, resumen.periodo)
	
	# Generar dataset de probabilidades
	logger$info("Generando nuevo dataset de probabilidades")
	resultados.periodo <- test %>%
	  dplyr::select(numero_de_cliente, foto_mes) %>%
	  dplyr::mutate(!! paste0("prob_", config$nombre.modelo) := xgb.pred.test$pred)
	resultados.modelo  <- dplyr::bind_rows(resultados.modelo, resultados.periodo)
	
	# Guardar datos a archivo RData
	logger$info("Guardando resultados")
	save(resumen.modelo, resultados.modelo, file = paste0(config$dir$output, "/", config$nombre.modelo, ".RData"))
	
  # Limpiar al maximo la memoria
	logger$info("Limpiando memoria")
  rm(train, test, xgb.train, xgb.test, modelo, xgb.pred.test, ganancia, resultados.periodo, resumen.periodo)
  gc(full = TRUE)
}
# ------------------------------------------------------------------------------