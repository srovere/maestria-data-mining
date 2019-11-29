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

# ii. Cargar datos de linea de muerte.
LM <- readRDS("output/LineasMuerte/SalidaLineaMuerteOriginal.rds") %>%
  dplyr::filter(month == 201906) %>%
  dplyr::select(prob) %>%
  tidyr::unnest(cols = "prob") %>%
  dplyr::filter(probs >= 0.025) %>%
  dplyr::rename(numero_de_cliente = id, probabilidad_baja = probs)

# iii. Cargar datos de Santiago
load("output/LineasMuerte/M1_201906_Santiago.RData")
M1_S <- probabilidades.linea.muerte %>%
  dplyr::select(numero_de_cliente, probabilidad_baja) %>%
  dplyr::rename(prob_M1 = probabilidad_baja)
rm(probabilidades.linea.muerte, resultados.linea.muerte)
load("output/LineasMuerte/M2_201906_Santiago.RData")
M2_S <- probabilidades.linea.muerte %>%
  dplyr::select(numero_de_cliente, probabilidad_baja) %>%
  dplyr::rename(prob_M2 = probabilidad_baja)
rm(probabilidades.linea.muerte, resultados.linea.muerte)

# iv. Cargar datos de Axel
load("output/LineasMuerte/M1_201906_Axel.RData")
M1_A <- probabilidades.linea.muerte %>%
  dplyr::select(numero_de_cliente, probabilidad_baja) %>%
  dplyr::rename(prob_M1 = probabilidad_baja)
rm(probabilidades.linea.muerte, resultados.linea.muerte)
load("output/LineasMuerte/M2_201906_Axel.RData")
M2_A <- probabilidades.linea.muerte %>%
  dplyr::select(numero_de_cliente, probabilidad_baja) %>%
  dplyr::rename(prob_M2 = probabilidad_baja)
rm(probabilidades.linea.muerte, resultados.linea.muerte)

# v. Chequear integridad y definir modelo final
M5_A <- dplyr::inner_join(M1_A, M2_A, by = c("numero_de_cliente")) %>%
  dplyr::mutate(probabilidad_baja = (prob_M1+prob_M2)/2) %>%
  dplyr::filter(probabilidad_baja >= 0.025) %>%
  dplyr::select(numero_de_cliente, probabilidad_baja)
M5_S <- dplyr::inner_join(M1_S, M2_S, by = c("numero_de_cliente")) %>%
  dplyr::mutate(probabilidad_baja = (prob_M1+prob_M2)/2) %>%
  dplyr::filter(probabilidad_baja >= 0.025) %>%
  dplyr::select(numero_de_cliente, probabilidad_baja)
if (all(M5_A == M5_S)) {
  M5 <- M5_S
  rm(M5_S, M5_A, M1_A, M2_A, M1_S, M2_S)
} else {
  warning("Los modelos son diferentes!!")
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- IV. Realizar comparaciones por cuantiles y guardar resultados ----
# -----------------------------------------------------------------------------#

# i. Calcular deciles de probabilidad para la linea de muerte
paso                   <- 0.1
cuantiles.linea.muerte <- c(0, quantile(x = LM$probabilidad_baja, probs = seq(from = paso, to = 1-paso, by = paso)), 1)
etiquetas.cuantiles    <- paste0("Q", sprintf("%02d", seq_len(length(cuantiles.linea.muerte)-1)))
LM.cuantiles           <- LM %>%
  dplyr::mutate(cuantil = cut(x = probabilidad_baja, breaks = cuantiles.linea.muerte, labels = etiquetas.cuantiles))

# ii. Verificar cuantas coincidencias hay por cuantiles.
M5.clientes   <- M5 %>%
  dplyr::pull(numero_de_cliente)
coincidencias <- purrr::map_dfr(
  .x = levels(LM.cuantiles$cuantil),
  .f = function(cuantil) {
    clientes.cuantil <- LM.cuantiles %>%
      dplyr::filter(cuantil == !! cuantil) %>%
      dplyr::pull(numero_de_cliente)
    cantidad <- length(which(clientes.cuantil %in% M5.clientes))
    return (data.frame(cuantil = cuantil, cantidad = cantidad, porcentaje = 100 * cantidad / length(clientes.cuantil)))
  }
)

# iii. Guardar resultados a enviar por mail
readr::write_csv(x = dplyr::select(M5, numero_de_cliente), path = "output/rovere_mantalian.txt",
                 col_names = FALSE)
# ------------------------------------------------------------------------------