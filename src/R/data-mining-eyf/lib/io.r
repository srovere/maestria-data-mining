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

# Funcion para leer set de datos
leer_set_datos <- function(input.dir, denominacion) {
  return (data.table::fread(paste0(input.dir, "/", denominacion, ".txt"), header=TRUE, sep="\t"))
}

