rm(list = objects())

require(dplyr)
require(httr)
require(jsonlite)
require(purrr)

# Estaciones automaticas
estaciones.automaticas <- purrr::map_dfr(
  .x = c(3, 8, 11),
  .f = function(red_id) {
    estaciones.red <- httr::GET(url = glue::glue("https://siat-soba.smn.gob.ar/ws-api/estaciones/{red_id}/Automatica"),
                                config = httr::authenticate(user = "ws-api-user", password = "Dcft^&*(")) %>%
      httr::content(as = "text") %>%
      jsonlite::fromJSON() %>%
      dplyr::mutate(red_id = red_id)
  }
) %>% dplyr::select(red_id, estacion_id, nombre, latitud, longitud, elevacion, marca, modelo)

# Datos
fecha.desde      <- lubridate::format_ISO8601(as.POSIXct("2010-01-01 00:00:00", tz = "UTC"))
fecha.hasta      <- lubridate::format_ISO8601(as.POSIXct("2019-12-31 23:59:59", tz = "UTC"))
datos.estaciones <- purrr::pmap_dfr(
  .l = dplyr::select(estaciones.automaticas, red_id, estacion_id),
  .f = function(red_id, estacion_id) {
    datos.estacion <- httr::GET(url = glue::glue("https://siat-soba.smn.gob.ar/ws-api//registros_alta_resolucion/{red_id}/{estacion_id}/{fecha.desde}/{fecha.hasta}"),
                                config = httr::authenticate(user = "ws-api-user", password = "Dcft^&*(")) %>%
      httr::content(as = "text") %>%
      jsonlite::fromJSON()
      
  }
)

# Guardar
readr::write_delim(x = estaciones.automaticas, delim = "\t", path = "data/estaciones.csv")
readr::write_delim(x = datos.estaciones, delim = "\t", path = "data/variables.csv")