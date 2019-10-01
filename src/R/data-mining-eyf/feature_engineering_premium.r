# -----------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----
# -----------------------------------------------------------------------------#
rm(list = ls()); gc()
Sys.setenv(TZ = "UTC")
list.of.packages <- c("caret", "data.table", "dplyr", "futile.logger", "magrittr",
                      "purrr", "R6", "utils", "yaml", "zoo")
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

logger <- Logger$new(log.level = INFO)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- IV. Leer set de datos y realizar operaciones de FE ----
# -----------------------------------------------------------------------------#

# i. Leer set de datos
logger$info("Leyendo conjunto de datos")
set.datos <- leer_set_datos(config$dir$input, "paquete_premium")

# Se calculan operaciones moviles (minimo, maximo y media) a ciertas columnas
set.datos %<>%
  dplyr::arrange(numero_de_cliente, foto_mes) %>%
  dplyr::group_by(numero_de_cliente) %>%
  dplyr::mutate(mcaja_ahorro_dolares_media_6 = fe_media_movil(mcaja_ahorro_dolares, 6),
                mcaja_ahorro_dolares_minimo_6 = fe_minimo_movil(mcaja_ahorro_dolares, 6),
                mcaja_ahorro_dolares_maximo_6 = fe_maximo_movil(mcaja_ahorro_dolares, 6),
                mcaja_ahorro_dolares_tendencia_6 = fe_tendencia_movil(mcaja_ahorro_dolares, 6))

# Guardar archivo como RDS
base::saveRDS(set.datos, file = paste0(config$dir$input, "/premium.rds"))

# ------------------------------------------------------------------------------