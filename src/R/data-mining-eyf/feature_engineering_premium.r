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
# --- III. Cargar codigo necesario. Crear logger y carpetas ----
# -----------------------------------------------------------------------------#

# i. Carga de codigo
source(file = paste0(config$dir$lib, "/io.r"), echo = FALSE)
source(file = paste0(config$dir$lib, "/feature_engineering.r"), echo = FALSE)

# ii. Creacion de logger
logger <- Logger$new(log.level = INFO)

# iii. Crear carpetas
parts.dir <- paste0(config$dir$input, "/parts")
if (! dir.exists(parts.dir)) {
  dir.create(parts.dir)
}
months.dir <- paste0(config$dir$input, "/months")
if (! dir.exists(months.dir)) {
  dir.create(months.dir)
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- IV. Leer set de datos y realizar operaciones de FE ----
# -----------------------------------------------------------------------------#

# i. Leer set de datos
logger$info("Leyendo conjunto de datos")
set.datos <- leer_set_datos(config$dir$input, "paquete_premium")

# ii. Pasar datos a fechas relativas
set.datos.fechas.relativas <- set.datos %>%
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
  dplyr::select(-foto_mes_fecha)

# iii. Ahora se calculan operaciones moviles (minimo, maximo y media) a ciertas columnas
#      Se van guardando partes del data frame en distintos archivos porque no entra todo en memoria
columnas.no.procesables <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
columnas.procesables    <- setdiff(colnames(set.datos.fechas.relativas), columnas.no.procesables)
ventanas                <- c(3, 6, 12)
min.ventana.tendencia   <- 6
combinaciones           <- purrr::cross_df(.l = list(columna = columnas.procesables, ventana = ventanas)) %>%
  dplyr::mutate(numero = dplyr::row_number())
cantidad.combinaciones  <- nrow(combinaciones)
set.datos.historicos    <- set.datos.fechas.relativas %>%
  dplyr::arrange(numero_de_cliente, foto_mes)
purrr::pwalk(
  .l = combinaciones,
  .f = function(columna, ventana, numero) {
    logger$info(glue::glue("Procesando {columna} con ventana de {ventana} meses ({numero}/{cantidad.combinaciones})"))
    
    # Si el archivo existe, seguir de largo
    output.file <- paste0(parts.dir, "/", columna, ".rds")
    if (! file.exists(output.file)) {
      columna_media  <- paste0(columna, "_media_", ventana)
      columna_minimo <- paste0(columna, "_minimo_", ventana)
      columna_maximo <- paste0(columna, "_maximo_", ventana)
      
      set.datos.historicos.columna <- set.datos.historicos %>%
        dplyr::select(numero_de_cliente, foto_mes, !! columna) %>%
        dplyr::group_by(numero_de_cliente) %>%
        dplyr::mutate(!! columna_media  := fe_media_movil(!! rlang::sym(columna), ventana),
                      !! columna_minimo := fe_minimo_movil(!! rlang::sym(columna), ventana),
                      !! columna_maximo := fe_maximo_movil(!!  rlang::sym(columna), ventana))
        if (ventana >= min.ventana.tendencia) {
         columna_tendencia <- paste0(columna, "_tendencia_", ventana)
         set.datos.historicos.columna <- set.datos.historicos.columna %>%
           dplyr::mutate(!! columna_tendencia := fe_tendencia_movil(!! rlang::sym(columna), ventana))
        }
      
      # Guardar a disco
      base::saveRDS(set.datos.historicos.columna, file = paste0(parts.dir, "/", columna, ".rds"))
      rm(set.datos.historicos.columna)
      gc()
    }
  }
)

# iv. Guardar archivo como RDS mes a mes
foto.meses         <- sort(unique(dplyr::pull(set.datos, foto_mes)))
archivos.variables <- list.files(path = parts.dir, pattern = "*.rds", full.names = TRUE)
set.datos.minimo   <- set.datos %>%
  dplyr::select(dplyr::one_of(columnas.no.procesables))
rm(set.datos.fechas.relativas, set.datos.historicos, set.datos)
purrr::walk(
  .x = foto.meses,
  .f = function(foto.mes) {
    logger$info(glue::glue("Procesando datos de {foto.mes}"))
    
    set.datos.mes <- set.datos.minimo %>%
      dplyr::filter(foto_mes == foto.mes)
    purrr::walk(
      .x = archivos.variables,
      .f = function(archivo.variable) {
        logger$info(glue::glue("... archivo {archivo.variable}"))
        set.datos.variable <- base::readRDS(archivo.variable) %>%
          dplyr::filter(foto_mes == foto.mes)
        set.datos.mes      <<- set.datos.mes %>%
          dplyr::inner_join(set.datos.variable, by = c("numero_de_cliente", "foto_mes"))
        rm(set.datos.variable)
      }
    )
    
    # Guardar a disco
    base::saveRDS(set.datos.mes, file = paste0(months.dir, "/", foto.mes, ".rds"))
    rm(set.datos.mes)
    gc()
  }
)
# ------------------------------------------------------------------------------