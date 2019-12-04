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

# ii. Cargar datos de linea de muerte, M1 y M2.
periodo <- 201906
LM <- readRDS("output/LineasMuerte/SalidaLineaMuerteOriginal.rds") %>%
  dplyr::filter(month == periodo) %>%
  dplyr::select(prob) %>%
  tidyr::unnest(cols = "prob") %>%
  dplyr::rename(numero_de_cliente = id, prob_LM = probs)
M1 <- readRDS("output/LineasMuerte/M1_completo.rds") %>%
  dplyr::filter(foto_mes == periodo) %>%
  dplyr::select(numero_de_cliente, probabilidad_baja) %>%
  dplyr::rename(prob_M1 = probabilidad_baja)
M2 <- readRDS("output/LineasMuerte/M2_completo.rds") %>%
  dplyr::filter(foto_mes == periodo) %>%
  dplyr::select(numero_de_cliente, probabilidad_baja, clase) %>%
  dplyr::rename(prob_M2 = probabilidad_baja)

# Definir M6
M6 <- dplyr::inner_join(M1, M2, by = c("numero_de_cliente")) %>%
  dplyr::inner_join(LM, by = c("numero_de_cliente")) %>%
  dplyr::mutate(probabilidad_baja = (prob_LM+prob_M1+prob_M2)/3) %>%
  dplyr::filter(probabilidad_baja >= 0.025) %>%
  dplyr::select(numero_de_cliente, probabilidad_baja, clase)

# Verificar ganancia (para meses menores a 201906)
if (! all(is.na(M6$clase))) {
  ganancia <- pe_ganancia(M6$probabilidad_baja, M6$clase)
} else {
  ganancia <- NULL
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- IV. Realizar comparaciones por cuantiles y guardar resultados ----
# -----------------------------------------------------------------------------#

# i. Calcular deciles de probabilidad para la linea de muerte
LM.corte               <- dplyr::filter(LM, prob_LM >= 0.025)
paso                   <- 0.05
cuantiles.linea.muerte <- c(0, quantile(x = LM.corte$prob_LM, probs = seq(from = paso, to = 1-paso, by = paso)), 1)
etiquetas.cuantiles    <- paste0("Q", sprintf("%02d", seq_len(length(cuantiles.linea.muerte)-1)))
LM.cuantiles           <- LM.corte %>%
  dplyr::mutate(cuantil = cut(x = prob_LM, breaks = cuantiles.linea.muerte, labels = etiquetas.cuantiles))

# ii. Verificar cuantas coincidencias hay por cuantiles.
M6.clientes   <- M6 %>%
  dplyr::pull(numero_de_cliente)
coincidencias <- purrr::map_dfr(
  .x = levels(LM.cuantiles$cuantil),
  .f = function(cuantil) {
    clientes.cuantil <- LM.cuantiles %>%
      dplyr::filter(cuantil == !! cuantil) %>%
      dplyr::pull(numero_de_cliente)
    cantidad <- length(which(clientes.cuantil %in% M6.clientes))
    return (data.frame(cuantil = cuantil, cantidad = cantidad, porcentaje = 100 * cantidad / length(clientes.cuantil)))
  }
)

# iii. Guardar resultados a enviar por mail
readr::write_csv(x = dplyr::select(M6, numero_de_cliente), path = "output/rovere_mantalian.txt",
                 col_names = FALSE)
# ------------------------------------------------------------------------------