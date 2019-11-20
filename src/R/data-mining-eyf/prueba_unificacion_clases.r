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
  grow_policy = "lossguide",
  tree_method = 'hist'
), config$hiperparametros)

hiperparametros <- as.data.frame(config$hiperparametros)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- V. Definir conjunto de train y test ----
# -----------------------------------------------------------------------------#

# Lectura de conjunto de datos de test
test <- leer_set_datos_mensuales(config$dir$input, 
                                 fecha.desde = as.Date("2019-04-01"),
                                 fecha.hasta = as.Date("2019-04-01")) %>%
  dplyr::mutate(clase = fe_clase_binaria(clase_ternaria)) %>%
  dplyr::select(-clase_ternaria)
  
# Lectura de conjunto de datos de train
train <- leer_set_datos_mensuales(config$dir$input, 
                                  fecha.desde = as.Date("2019-02-01"),
                                  fecha.hasta = as.Date("2019-02-01")) %>%
  dplyr::mutate(clase = fe_clase_binaria_unificada(clase_ternaria)) %>%
  dplyr::select(-clase_ternaria)

# Definir conjuntos de train/test para XGBoost
xgb.train <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(train, -clase)),
                                  label = as.matrix(dplyr::select(train, clase)))
xgb.test  <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(test, -clase)),
                                  label = as.matrix(dplyr::select(test, clase)))
parametros$base_score <- mean(getinfo(xgb.train, "label"))
rm(train)
gc(full = TRUE)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- VI. Definir nueva funcion de ganancia y ejecutar modelo ----
# -----------------------------------------------------------------------------#

# Funcion de perdida para XGBoost
mejor.corte <- list("ganancia_mejor" = 0, "prob_corte" = 0.025 )
pe_perdida_xgboost_clases_unificadas <- function(preds, dtrain) {
  # Obtener clases y pesos y armar un data.table junto con las probabilidades
  
  if (length(preds) == nrow(test)) {
    # Calcular mejor punto de corte
    clases        <- xgboost::getinfo(dtrain, "label")
    tbl           <- as.data.table(cbind(preds, clases))
    colnames(tbl) <- c("prob", "clase")

    # Ordeno la tabla de acuerdo a las probabilidades y calculo el punto de corte
    setorder(tbl, -prob )
    tbl[, ganancia := ifelse(clase == 1, 19500, -500)]
    tbl[, ganacum := cumsum(tbl$ganancia)]
    pos <- which.max(tbl$ganacum)
    vprob_corte <- tbl[pos, prob]

    # Luego para evaluar la ganancia toma todos los valores de tbl tales que peso == 1
    ganancia <- sum((preds > vprob_corte) * ifelse(clases == 1, 19500, -500))

    # Actualizar mejor punto de corte y mejor ganancia
    if (ganancia > mejor.corte$ganancia_mejor) {
      mejor.corte$prob_corte     <<- vprob_corte
      mejor.corte$ganancia_mejor <<- ganancia
    }

    return (list(name = "perdida", value = ifelse(is.na(ganancia), 0, -ganancia)))
  } else {
    # Solamente calcular ganancia (o mejor dicho, la perdida como -ganancia)
    return (pe_perdida_xgboost(preds, dtrain))
  }
}

# Ejecutar XGBoost para toda la semilla indicada
logger$info("Ejecutando entrenamiento")
set.seed(config$semilla)
modelo <- xgboost::xgb.train(data = xgb.train, nrounds = config$rondas.entrenamiento, 
                             verbose = 2, maximize = FALSE, feval = pe_perdida_xgboost_clases_unificadas,
                             watchlist = list(train = xgb.train, test = xgb.test),
                             params = parametros)
	
logger$info("Calculando ganancia y guardando resultados")
xgb.pred.test           <- data.frame(pred = predict(modelo, xgb.test, reshape = T))
ganancia                <- pe_ganancia(probabilidades = xgb.pred.test$pred, clase = test$clase, proporcion = 1, punto_corte = mejor.corte$prob_corte)
# ------------------------------------------------------------------------------