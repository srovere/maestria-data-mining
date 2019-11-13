# Set de funciones para lectura de entradas, escritura de salidas y logging
Logger <- R6Class("Logger",
  public = list(
    initialize = function(log.level = INFO, appender.file = NULL) {
      appender <- NULL
      if (! is.null(appender.file)) {
        appender <- futile.logger::appender.file(appender.file)
      }
      layout <- futile.logger::layout.format('[~l] [~t] ~m')
      futile.logger::flog.logger(name = "global-logger", threshold = log.level,
                                 layout = layout, appender = appender)
    },

    debug = function(mensaje) {
      futile.logger::flog.debug(msg = mensaje)
    },
    
    info = function(mensaje) {
      futile.logger::flog.info(msg = mensaje)
    },
    
    warn = function(mensaje) {
      futile.logger::flog.warn(msg = mensaje)
    },
    
    error = function(mensaje) {
      futile.logger::flog.error(msg = mensaje)
      base::stop("Abortando ejecucion")
    }
  )
)

# Funcion para leer set de datos originales
leer_set_datos <- function(input.dir, denominacion) {
  return (data.table::fread(paste0(input.dir, "/", denominacion, ".txt"), header=TRUE, sep="\t"))
}

# Funcion para leer set de datos con feature engineering agrupados a nivel mensual (desde - hasta)
leer_set_datos_mensuales <- function(input.dir, fecha.desde, fecha.hasta) {
  fechas    <- seq(from = fecha.desde, to = fecha.hasta, by = "months")
  set.datos <- purrr::map_dfr(
    .x = fechas,
    .f = function(fecha.mes) {
      archivo.mes <- paste0(format(fecha.mes, "%Y%m"), ".rds")
      return (base::readRDS(paste0(input.dir, "/", archivo.mes)))
    }
  )
  return (set.datos)
}

# Funcion para leer set de datos con feature engineering agrupados a nivel mensual (vector de meses)
leer_set_datos_mensuales_meses_varios <- function(input.dir, fechas.meses) {
  set.datos <- purrr::map_dfr(
    .x = fechas.meses,
    .f = function(fecha.mes) {
      archivo.mes <- paste0(format(fecha.mes, "%Y%m"), ".rds")
      return (base::readRDS(paste0(input.dir, "/", archivo.mes)))
    }
  )
  return (set.datos)
}