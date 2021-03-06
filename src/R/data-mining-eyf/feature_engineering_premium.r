# -----------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----
# -----------------------------------------------------------------------------#
rm(list = ls()); gc()
Sys.setenv(TZ = "UTC")
list.of.packages <- c("caret", "data.table", "dplyr", "futile.logger", "magrittr",
                      "purrr", "R6", "readxl", "utils", "yaml", "zoo")
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
  archivo.config <- paste0(getwd(), "/configuracion_feature_engineering.yml")
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
# --- IV. Leer set de datos, diccionario de datos y archivo de inflacion ----
# -----------------------------------------------------------------------------#

# i. Leer set de datos
logger$info("Leyendo conjunto de datos")
set.datos <- leer_set_datos(config$dir$input, "paquete_premium")

# ii. Leer diccionario de datos
logger$info("Leyendo diccionario de datos")
diccionario.datos <- readxl::read_excel(path = paste0(config$dir$input, "/", config$diccionario.datos))

# iii. Leer archivo de especificacion de inflacion (si es que aplica)
inflacion <- NULL
if (! is.null(config$inflacion)) {
  logger$info("Leyendo especificación de inflación")
  inflacion <- readxl::read_excel(path = paste0(config$dir$input, "/", config$inflacion)) %>%
    dplyr::select(foto_mes, tasa_acumulada) %>%
    dplyr::mutate(foto_mes = as.integer(format(foto_mes, "%Y%m")))
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- V. Transformar fechas y montos (si se especifica inflacion) ----
# -----------------------------------------------------------------------------#

# i. Pasar datos a fechas relativas
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

# ii. Ajustar precios por inflación (si es que es aplicable)
if (! is.null(config$inflacion)) {
  # a. Selecciontar atributos correspondientes a montos expresados en pesos
  atributos.moneda.pesos <- diccionario.datos %>%
    dplyr::filter(unidad == "pesos") %>%
    dplyr::pull(campo)
  
  # b. Eliminar archivos RDS correspondientes a montos expresados en pesos
  if (config$borrar.rds.montos.pesos) {
    purrr::walk(
      .x = atributos.moneda.pesos,
      .f = function(atributo) {
        archivo <- paste0(parts.dir, "/", atributo, ".rds")
        if (file.exists(archivo)) {
          logger$info(glue::glue("Eliminando archivo {archivo}"))
          file.remove(archivo)
        }
      }
    )
  }
  
  # c. Transformar montos en pesos dividiendo por la tasa acumulada de inflación
  set.datos.fechas.relativas <- set.datos.fechas.relativas %>%
    dplyr::inner_join(inflacion, by = "foto_mes")
  purrr::walk(
    .x = atributos.moneda.pesos,
    .f = function(atributo) {
      set.datos.fechas.relativas <<- set.datos.fechas.relativas %>%
        dplyr::mutate(!! atributo := !! rlang::sym(atributo) / tasa_acumulada)
    }
  )
  set.datos.fechas.relativas <- set.datos.fechas.relativas %>%
    dplyr::select(-tasa_acumulada)
}

# iii. Agregar features sugeridos
data.table::setDT(set.datos.fechas.relativas)
set.datos.fechas.relativas[ , mv_cuenta_estado2       := pmax( Master_cuenta_estado,  Visa_cuenta_estado, na.rm = TRUE) ]
set.datos.fechas.relativas[ , mv_marca_atraso         := pmax( Master_marca_atraso, Visa_marca_atraso, na.rm = TRUE) ]
set.datos.fechas.relativas[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
set.datos.fechas.relativas[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
set.datos.fechas.relativas[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
set.datos.fechas.relativas[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
set.datos.fechas.relativas[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_tconsumos            := rowSums( cbind( Master_tconsumos,  Visa_tconsumos) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_tadelantosefectivo   := rowSums( cbind( Master_tadelantosefectivo,  Visa_tadelantosefectivo) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
set.datos.fechas.relativas[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
set.datos.fechas.relativas[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
set.datos.fechas.relativas[ , mvr_msaldototal         := ifelse(mv_mlimitecompra != 0, mv_msaldototal / mv_mlimitecompra, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_msaldopesos         := ifelse(mv_mlimitecompra != 0, mv_msaldopesos / mv_mlimitecompra, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_msaldopesos2        := ifelse(mv_msaldototal != 0, mv_msaldopesos / mv_msaldototal, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_msaldodolares       := ifelse(mv_mlimitecompra != 0, mv_msaldodolares / mv_mlimitecompra, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_msaldodolares2      := ifelse(mv_msaldototal != 0, mv_msaldodolares / mv_msaldototal, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_mconsumospesos      := ifelse(mv_mlimitecompra != 0, mv_mconsumospesos / mv_mlimitecompra, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_mconsumosdolares    := ifelse(mv_mlimitecompra != 0, mv_mconsumosdolares / mv_mlimitecompra, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_madelantopesos      := ifelse(mv_mlimitecompra != 0, mv_madelantopesos / mv_mlimitecompra, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_madelantodolares    := ifelse(mv_mlimitecompra != 0, mv_madelantodolares / mv_mlimitecompra, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_mpagado             := ifelse(mv_mlimitecompra != 0, mv_mpagado / mv_mlimitecompra, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_mpagospesos         := ifelse(mv_mlimitecompra != 0, mv_mpagospesos / mv_mlimitecompra, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_mpagosdolares       := ifelse(mv_mlimitecompra != 0, mv_mpagosdolares / mv_mlimitecompra, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_mconsumototal       := ifelse(mv_mlimitecompra != 0, mv_mconsumototal  / mv_mlimitecompra, as.double(NA)) ]
set.datos.fechas.relativas[ , mvr_mpagominimo         := ifelse(mv_mlimitecompra != 0, mv_mpagominimo  / mv_mlimitecompra, as.double(NA)) ]
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- VI. Generar nuevos atributos a partir de ventanas móviles ----
# -----------------------------------------------------------------------------#

# Ahora se calculan operaciones moviles (minimo, maximo y media) a ciertas columnas
# Se van guardando partes del data frame en distintos archivos porque no entra todo en memoria
columnas.no.procesables <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
columnas.procesables    <- setdiff(colnames(set.datos.fechas.relativas), columnas.no.procesables)
ventanas                <- c(3, 6)
combinaciones           <- data.frame(columna = columnas.procesables, stringsAsFactors = FALSE) %>%
  dplyr::mutate(numero = dplyr::row_number())
cantidad.combinaciones  <- length(columnas.procesables)
set.datos.historicos    <- set.datos.fechas.relativas %>%
  dplyr::arrange(numero_de_cliente, foto_mes)
rm(set.datos.fechas.relativas)
gc(full = TRUE)
purrr::pwalk(
  .l = combinaciones,
  .f = function(columna, numero) {
    logger$info(glue::glue("Procesando {columna} ({numero}/{cantidad.combinaciones})"))
    
    # Si el archivo existe, seguir de largo
    output.file <- paste0(parts.dir, "/", columna, ".rds")
    if (! file.exists(output.file)) {
      set.datos.historicos.columna <- set.datos.historicos %>%
        dplyr::select(numero_de_cliente, foto_mes, !! columna)
      
      purrr::walk(
        .x = ventanas,
        .f = function(ventana) {
          columna_media     <- paste0(columna, "_media_", ventana)
          columna_tendencia <- paste0(columna, "_tendencia_", ventana)
          set.datos.historicos.columna <<- set.datos.historicos.columna %>%
            dplyr::group_by(numero_de_cliente) %>%
            dplyr::mutate(!! columna_media  := fe_media_movil(!! rlang::sym(columna), ventana),
                          !! columna_tendencia := fe_tendencia_movil(!! rlang::sym(columna), ventana))
        }
      )
      
      # Guardar a disco
      base::saveRDS(set.datos.historicos.columna, file = paste0(parts.dir, "/", columna, ".rds"))
      rm(set.datos.historicos.columna)
      gc(full = TRUE)
    }
  }
)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- VII. Generar archivos por mes ----
# -----------------------------------------------------------------------------#

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
    gc(full = TRUE)
  }
)
# ------------------------------------------------------------------------------