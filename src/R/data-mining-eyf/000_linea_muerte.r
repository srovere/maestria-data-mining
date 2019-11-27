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
  grow_policy = "lossguide",
  tree_method = 'hist'
), config$hiperparametros)
hiperparametros <- as.data.frame(config$hiperparametros)

# Si se definicion un archivo de seleccion de mejores variables, cargarlo
mejores.variables <- NULL
if (! is.null(config$archivo.seleccion.variables)) {
  mejores.atributos <- union("numero_de_cliente", base::readRDS(paste0(config$dir$work, "/", config$archivo.seleccion.variables)))
  mejores.variables <- c(mejores.atributos, "clase")
}

# Si se indico agregar features "extra", cargarlos de archivo
features.extra <- NULL
if (! is.null(config$archivo.features.extra)) {
  features.extra <- base::readRDS(paste0(config$dir$extra, "/", config$archivo.features.extra))
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- V. Realizar ejecucion de linea de muerte ----
# -----------------------------------------------------------------------------#

resultados.linea.muerte     <- NULL
probabilidades.linea.muerte <- NULL
for (periodo.test in as.character(seq(from = as.Date(config$fecha.desde), to = as.Date(config$fecha.hasta), by = 'months'))) {
  logger$info(paste0("Leyendo conjunto de datos de train/test para ", periodo.test))  
  
  # Lectura de conjunto de datos de test
  test <- leer_set_datos_mensuales(config$dir$input, 
                                   fecha.desde = as.Date(periodo.test),
                                   fecha.hasta = as.Date(periodo.test)) %>%
    dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
    dplyr::select(-clase_ternaria)
  if (! is.null(features.extra)) {
    test <- test %>%
      dplyr::left_join(features.extra, by = c("numero_de_cliente", "foto_mes"))
  }
  if (! is.null(mejores.variables)) {
    test <- test[, mejores.variables]
  }
  
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
  if (! is.null(features.extra)) {
    train <- train %>%
      dplyr::left_join(features.extra, by = c("numero_de_cliente", "foto_mes"))
  }
  if (! is.null(mejores.variables)) {
    train <- train[, mejores.variables]
  }

  # Definir conjuntos de train/test para XGBoost
  xgb.train <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(train, -clase)),
                                    label = as.matrix(dplyr::select(train, clase)))
  xgb.test  <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(test, -clase)),
                                    label = as.matrix(dplyr::select(test, clase)))
  parametros$base_score <- mean(getinfo(xgb.train, "label") )
  rm(train)
  gc(full = TRUE)

  # Ejecutar XGBoost para toda la semilla indicada
  logger$info("Ejecutando entrenamiento")
	set.seed(config$semilla)
	modelo <- xgboost::xgb.train(data = xgb.train, nrounds = config$rondas.entrenamiento, 
	                             verbose = 2, maximize = FALSE, feval = feval,
	                             watchlist = list(train = xgb.train, test = xgb.test),
	                             params = parametros)
	
	logger$info("Calculando ganancia y guardando resultados")
	xgb.pred.test           <- data.frame(pred = predict(modelo, xgb.test, reshape = T))
	ganancia                <- pe_ganancia(probabilidades = xgb.pred.test$pred, clase = test$clase, proporcion = 1, punto_corte = mejor.corte$prob_corte)
  resultados.periodo      <- dplyr::bind_cols(
    hiperparametros, 
    data.frame(semilla = config$semilla, ganancia = ganancia, punto_corte = mejor.corte$prob_corte, periodo = periodo.test)
  ) %>% 
    dplyr::mutate(foto_mes = as.integer(format(as.Date(periodo.test), "%Y%m")), modelo = list(modelo))
  probabilidades.periodo  <- test %>%
    dplyr::select(numero_de_cliente, foto_mes, clase) %>%
    dplyr::mutate(probabilidad_baja = xgb.pred.test$pred)
  
  # Consolidar resultados
  resultados.linea.muerte     <- dplyr::bind_rows(resultados.linea.muerte, resultados.periodo)
  probabilidades.linea.muerte <- dplyr::bind_rows(probabilidades.linea.muerte, probabilidades.periodo)
  
  # Guardar datos a archivo RData
  save(resultados.linea.muerte, probabilidades.linea.muerte, file = paste0(config$dir$output, "/LineaMuerte.RData"))
}
# ------------------------------------------------------------------------------