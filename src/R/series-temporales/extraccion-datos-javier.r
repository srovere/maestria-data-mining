rm(list = objects())

require(dplyr)
require(httr)
require(jsonlite)
require(purrr)
require(sf)

# Estaciones automaticas
estaciones <- httr::GET(url = glue::glue("https://api.crc-sas.org/ws-api/estaciones/AR/SMN"),
                        config = httr::authenticate(user = "clima", password = "Dcft^&*(")) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  sf::st_as_sf(coords = c("longitud", "latitud"), crs = 4326)

# Datos de temperatura maxima, minima y precipitaciones
fecha.desde      <- lubridate::format_ISO8601(as.POSIXct("2017-01-01 00:00:00", tz = "UTC"))
fecha.hasta      <- lubridate::format_ISO8601(as.POSIXct("2019-09-30 23:59:59", tz = "UTC"))
variable_id      <- c("tmax", "tmin", "prcp")
combinaciones    <- tidyr::crossing(omm_id = dplyr::pull(estaciones, omm_id), variable_id = variable_id)
datos.estaciones <- purrr::pmap_dfr(
  .l = combinaciones,
  .f = function(omm_id, variable_id) {
    datos.estacion <- httr::GET(url = glue::glue("https://api.crc-sas.org/ws-api/registros_diarios/{omm_id}/{variable_id}/{fecha.desde}/{fecha.hasta}"),
                                config = httr::authenticate(user = "clima", password = "Dcft^&*(")) %>%
      httr::content(as = "text") %>%
      jsonlite::fromJSON()
  }
)

# Guardar
save(estaciones, datos.estaciones, file = "data/DatasetJavier.RData")