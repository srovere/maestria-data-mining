rm(list = objects())

require(dplyr)
require(httr)
require(jsonlite)
require(purrr)
require(sf)

# Estaciones automaticas
estaciones <- httr::GET(url = glue::glue("https://api.crc-sas.org/ws-api/estaciones/AR"),
                                       config = httr::authenticate(user = "clima", password = "Dcft^&*(")) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  sf::st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>%
  dplyr::mutate(tipo = factor(dplyr::if_else(omm_id < 10000000, 'Convencional', 'Automatica')))

# Obtener contorno de Argentina.
zona.estudio <- base::readRDS("data/gadm36_ARG_0_sf.rds")
plot(sf::st_geometry(zona.estudio))
plot(estaciones, add = TRUE)

# Datos de temperatura maxima
fecha.desde      <- lubridate::format_ISO8601(as.POSIXct("1961-01-01 00:00:00", tz = "UTC"))
fecha.hasta      <- lubridate::format_ISO8601(as.POSIXct("2019-12-31 23:59:59", tz = "UTC"))
variable_id      <- "tmax"
datos.estaciones <- purrr::map_dfr(
  .x = dplyr::pull(estaciones, omm_id),
  .f = function(omm_id) {
    datos.estacion <- httr::GET(url = glue::glue("https://api.crc-sas.org/ws-api/registros_diarios/{omm_id}/{variable_id}/{fecha.desde}/{fecha.hasta}"),
                                config = httr::authenticate(user = "clima", password = "Dcft^&*(")) %>%
      httr::content(as = "text") %>%
      jsonlite::fromJSON()
  }
)

# Guardar
save(estaciones, datos.estaciones, file = "data/Dataset.RData")
