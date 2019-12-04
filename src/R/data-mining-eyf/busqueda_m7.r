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
# --- III. Cargar funciones de librerias propias y datos ----
# -----------------------------------------------------------------------------#
# i. Cargar librerias
source(file = paste0(config$dir$lib, "/performance.r"), echo = FALSE)

# ii. Cargar datos
probabilidades_LM <- readRDS("output/LineasMuerte/LM.rds") %>%
  dplyr::rename(prob_LM = probabilidad_baja)
probabilidades_M1 <- readRDS("output/LineasMuerte/M1.rds") %>%
  dplyr::rename(prob_M1 = probabilidad_baja)
probabilidades_M2 <- readRDS("output/LineasMuerte/M2.rds") %>%
  dplyr::rename(prob_M2 = probabilidad_baja)
probabilidades    <- probabilidades_LM %>%
  dplyr::left_join(probabilidades_M2, by = c("numero_de_cliente", "foto_mes", "clase")) %>%
  dplyr::left_join(probabilidades_M1, by = c("numero_de_cliente", "foto_mes", "clase"))

rm(probabilidades_LM, probabilidades_M1, probabilidades_M2)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- IV. Buscar M7 ----
# -----------------------------------------------------------------------------#

CalcularGananciasTrain <- function(probabilidades, A, B, C) {
  ganancias <- probabilidades %>%
    dplyr::mutate(prob_M7 = (A * prob_LM + B * prob_M1 + C * prob_M2)) %>%
    dplyr::group_by(foto_mes) %>%
    dplyr::summarise(LM = pe_ganancia(prob_LM, clase),
                     M7 = pe_ganancia(prob_M7, clase)) %>%
    dplyr::mutate(valor = 100 * (M7 - LM) / LM) %>%
    dplyr::mutate(referencia = as.character(foto_mes)) %>%
    dplyr::select(referencia, valor) %>%
    tidyr::pivot_wider(names_from = referencia, values_from = valor) %>%
    dplyr::mutate(Promedio = mean(c(.[[1]], .[[2]], .[[3]], .[[4]], .[[5]], .[[6]], .[[7]], .[[8]], .[[9]])),
                  Desvio = sd(c(.[[1]], .[[2]], .[[3]], .[[4]], .[[5]], .[[6]], .[[7]], .[[8]], .[[9]])),
                  CV = Desvio / Promedio, A = A, B = B, C = C)
  return (ganancias)
}
CalcularGanancias <- function(probabilidades, A, B, C) {
  ganancias <- probabilidades %>%
    dplyr::mutate(prob_M7 = (A * prob_LM + B * prob_M1 + C * prob_M2)) %>%
    dplyr::group_by(foto_mes) %>%
    dplyr::summarise(LM = pe_ganancia(prob_LM, clase),
                     M7 = pe_ganancia(prob_M7, clase)) %>%
    dplyr::mutate(valor = 100 * (M7 - LM) / LM) %>%
    dplyr::mutate(referencia = as.character(foto_mes)) %>%
    dplyr::select(referencia, valor) %>%
    tidyr::pivot_wider(names_from = referencia, values_from = valor) %>%
    dplyr::mutate(Promedio = mean(c(.[[1]], .[[2]], .[[3]], .[[4]], .[[5]], .[[6]], .[[7]], .[[8]], .[[9]], .[[10]], .[[11]])),
                  Promedio_2019 = mean(c(.[[8]], .[[9]], .[[10]], .[[11]])),
                  Desvio = sd(c(.[[1]], .[[2]], .[[3]], .[[4]], .[[5]], .[[6]], .[[7]], .[[8]], .[[9]], .[[10]], .[[11]])),
                  CV = Desvio / Promedio, A = A, B = B, C = C)
  return (ganancias)
}

probabilidades.train <- probabilidades %>%
  dplyr::filter(! foto_mes  %in% c(201903, 201904))
paso      <- 0.01
ganancias.train <- purrr::pmap_dfr(
  .l = purrr::transpose(purrr::cross2(seq(from = 0, to = 1, by = paso), seq(from = 0, to = 1, by = paso))),
  .f = function(A, B) {
    if (A + B > 1) {
      return (NULL)
    }
    C <- 1 - (A + B)
    return (CalcularGananciasTrain(probabilidades.train, A, B, C))
  }
) %>% dplyr::filter(Promedio >= 3) 

probabilidades.test <- probabilidades
ganancias.test      <- purrr::pmap_dfr(
  .l = dplyr::select(ganancias.train, A, B, C),
  .f = function(A, B, C) {
    return (CalcularGanancias(probabilidades, A, B, C))
  }
)
# ------------------------------------------------------------------------------