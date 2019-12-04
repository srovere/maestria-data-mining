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
source(file = paste0(config$dir$lib, "/io.r"), echo = FALSE)
source(file = paste0(config$dir$lib, "/feature_engineering.r"), echo = FALSE)
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
rm(probabilidades_LM, probabilidades_M1, probabilidades_M2, probabilidades_M8)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- IV. Generar estadisticas ----
# -----------------------------------------------------------------------------#
# i. Calcular ganancias por modelo
ganancias <- probabilidades %>%
  dplyr::mutate(Periodo = as.Date(sprintf("%d-%02d-01", foto_mes %/% 100, foto_mes %% 100))) %>%
  dplyr::mutate(prob_M3 = (prob_LM+prob_M1)/2,
                prob_M4 = (prob_LM+prob_M2)/2,
                prob_M5 = (prob_M1+prob_M2)/2,
                prob_M6 = (prob_LM+prob_M1+prob_M2)/3) %>%
  dplyr::group_by(Periodo) %>%
  dplyr::summarise(LM = pe_ganancia(prob_LM, clase),
                   M1 = pe_ganancia(prob_M1, clase),
                   M2 = pe_ganancia(prob_M2, clase),
                   M3 = pe_ganancia(prob_M3, clase),
                   M4 = pe_ganancia(prob_M4, clase),
                   M5 = pe_ganancia(prob_M5, clase),
                   M6 = pe_ganancia(prob_M6, clase))

# ii. Calcular porcentajes en relacion a la linea de muerta
porcentajes <- ganancias %>%
  dplyr::mutate(M1 = 100 * (M1 - LM) / LM,
                M2 = 100 * (M2 - LM) / LM,
                M3 = 100 * (M3 - LM) / LM,
                M4 = 100 * (M4 - LM) / LM,
                M5 = 100 * (M5 - LM) / LM,
                M6 = 100 * (M6 - LM) / LM) %>%
  dplyr::select(Periodo, M1, M2, M3, M4, M5, M6)

# iii. Cantidades
cantidades <- probabilidades %>%
  dplyr::mutate(Periodo = as.Date(sprintf("%d-%02d-01", foto_mes %/% 100, foto_mes %% 100))) %>%
  dplyr::mutate(prob_M3 = (prob_LM+prob_M1)/2,
                prob_M4 = (prob_LM+prob_M2)/2,
                prob_M5 = (prob_M1+prob_M2)/2,
                prob_M6 = (prob_LM+prob_M1+prob_M2)/3) %>%
  dplyr::select(-numero_de_cliente, -foto_mes, -clase) %>%
  tidyr::pivot_longer(cols = c(-"Periodo"), names_to = "Modelo", values_to = "probabilidad_baja") %>%
  dplyr::filter(probabilidad_baja >= 0.025) %>%
  dplyr::group_by(Periodo, Modelo) %>%
  dplyr::summarise(Cantidad = dplyr::n()) %>%
  tidyr::pivot_wider(names_from = "Modelo", values_from = "Cantidad")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- V. Graficar ----
# -----------------------------------------------------------------------------#

# i. Grafico de ganancias (eliminamos M1, M2 y M6)
ganancias.grafico <- ganancias %>%
  dplyr::select(-M1, -M2) %>%
  tidyr::pivot_longer(cols = c(-"Periodo"), names_to = "Modelo", values_to = "Ganancia")
ggplot2::ggplot(data = ganancias.grafico) + 
  ggplot2::geom_line(mapping = ggplot2::aes(x = Periodo, y = Ganancia, col = Modelo)) + 
  ggplot2::scale_x_date(date_breaks = "1 month") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90)
  )

# ii. Grafico de porcentajes
porcentajes.grafico <- porcentajes %>%
  dplyr::select(-M1, -M2) %>%
  tidyr::pivot_longer(cols = c(-"Periodo"), names_to = "Modelo", values_to = "Porcentaje")
ggplot2::ggplot(data = porcentajes.grafico) + 
  ggplot2::geom_line(mapping = ggplot2::aes(x = Periodo, y = Porcentaje, col = Modelo)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
  ggplot2::scale_x_date(date_breaks = "1 month") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90)
  )
# ------------------------------------------------------------------------------