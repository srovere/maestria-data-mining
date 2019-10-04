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

# Pasar datos a fechas relativas
set.datos.fechas.relativas <- dtplyr::lazy_dt(set.datos) %>%
  # Pasar fechas a diferencias relativas respecto de foto_mes.
  dplyr::mutate(foto_mes_fecha = as.Date(paste0(foto_mes, "01"), format = '%Y%m%d'),
                Master_Fvencimiento = fe_dias_diferencia(Master_Fvencimiento, foto_mes_fecha),
                Master_Finiciomora = fe_dias_diferencia(Master_Finiciomora, foto_mes_fecha),
                Master_fultimo_cierre = fe_dias_diferencia(Master_fultimo_cierre, foto_mes_fecha),
                Master_fechaalta = fe_dias_diferencia(Master_fechaalta, foto_mes_fecha),
                Visa_Fvencimiento = fe_dias_diferencia(Visa_Fvencimiento, foto_mes_fecha),
                Visa_Finiciomora = fe_dias_diferencia(Visa_Finiciomora, foto_mes_fecha),
                Visa_fultimo_cierre = fe_dias_diferencia(Visa_fultimo_cierre, foto_mes_fecha),
                Visa_fechaalta = fe_dias_diferencia(Visa_fechaalta, foto_mes_fecha)) %>%
  # Elimino el campo foto_mes_fecha
  dplyr::select(-foto_mes_fecha) %>%
  # Ejecutar
  dplyr::collect()

# Ahora se calculan operaciones moviles (minimo, maximo y media) a ciertas columnas
columnas.no.procesables <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
columnas.procesables    <- setdiff(colnames(set.datos.fechas.relativas), columnas.no.procesables)
ventanas                <- c(3, 6, 12)
min.ventana.tendencia   <- 6
combinaciones           <- purrr::cross_df(.l = list(columna = columnas.procesables, ventana = ventanas))
set.datos.historicos    <- dtplyr::lazy_dt(set.datos.fechas.relativas) %>%
  dplyr::arrange(numero_de_cliente, foto_mes) %>%
  dplyr::group_by(numero_de_cliente)
purrr::pwalk(
  .l = combinaciones,
  .f = function(columna, ventana) {
    columna_media  <- paste0(columna, "_media_", ventana)
    columna_minimo <- paste0(columna, "_minimo_", ventana)
    columna_maximo <- paste0(columna, "_maximo_", ventana)
    set.datos.historicos <<- set.datos.historicos %>%
      dplyr::mutate(!! columna_media  := fe_media_movil(!! rlang::sym(columna), ventana),
                    !! columna_minimo := fe_minimo_movil(!! rlang::sym(columna), ventana),
                    !! columna_maximo := fe_maximo_movil(!!  rlang::sym(columna), ventana))
      if (ventana >= min.ventana.tendencia) {
       columna_tendencia <- paste0(columna, "_tendencia_", ventana)
       set.datos.historicos <<- set.datos.historicos %>%
         dplyr::mutate(!! columna_tendencia := fe_tendencia_movil(!! rlang::sym(columna), ventana))
      }
    }
)
set.datos.modificado <- dplyr::collect(set.datos.historicos)

# Guardar archivo como RDS
base::saveRDS(set.datos.modificado, file = paste0(config$dir$input, "/premium.rds"))

# ------------------------------------------------------------------------------