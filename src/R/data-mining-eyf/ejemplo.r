# -----------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----
# -----------------------------------------------------------------------------#
rm(list = ls()); gc()
Sys.setenv(TZ = "UTC")
list.of.packages <- c("caret", "data.table", "doSNOW", "dplyr", "foreach", 
                      "futile.logger", "parallel", "R6", "rBayesianOptimization",
                      "rpart", "snow", "utils", "yaml")
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

# Grid search paralelizado
logger$info("Realizando prueba de ajuste con DT y Grid Search (paralelizado)")

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
                                       grilla.parametros = grilla.parametros)
end.time             <- proc.time()
elapsed.time         <- end.time[3] - start.time[3]
cat("Tiempo:", elapsed.time, "segundos")
resultados.rpart.gs.promedio <- resultados.rpart.gs %>% 
  dplyr::group_by(xval, cp, ms, mb, md) %>% 
  dplyr::summarise(ganancia_promedio = mean(ganancia_test), ganancia_desvio = sd(ganancia_test),
                   roc_auc_promedio = mean(roc_auc_test), roc_auc_desvio = sd(roc_auc_test))

# Optimizacion bayesiana
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
                                                limites.parametros = limites.parametros)
end.time          <- proc.time()
elapsed.time      <- end.time[3] - start.time[3]
cat("Tiempo:", elapsed.time, "segundos")  
# ------------------------------------------------------------------------------