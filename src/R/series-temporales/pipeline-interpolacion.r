rm(list = objects())

require(automap)
require(dplyr)
require(gstat)
require(httr)
require(jsonlite)
require(purrr)
require(tidyr)
require(sf)
require(stlplus)
require(stars)

# Cargar funciones necesarias para interpolación espacial
source("funciones-interpolacion.r", echo = FALSE)

# Se lee el dataset
argentina <- base::readRDS("data/gadm36_ARG_0_sf.rds")
load("data/Dataset.RData")

# Seleccionamos el período 2001-2010 a efectos ilustrativos. La idea se basa en que hay estaciones más cortas
# (que o bien arrancaron posteriormente a 1961 o bien se dieron de baja antes de 2019). Para poder 
# evaluar todas sin descargar las estaciones cortas, se hace un análisis año a año donde se 
# remueven aquellas estaciones que no cuentan con una mínima cantidad de datos (ver más adelante).
ano.desde         <- 2001
ano.hasta         <- 2010
series.temporales <- datos.estaciones %>%
  dplyr::mutate(fecha = as.Date(fecha)) %>%
  dplyr::filter(lubridate::year(fecha) %in% seq(from = ano.desde, to = ano.hasta, by = 1))

# Seleccionar series temporales "aceptables"
# Filtrado: quedan estaciones con mas del 75% de los datos y no mas de 10 dias consecutivos de datos faltantes.
fechas       <- tidyr::crossing(omm_id = unique(series.temporales$omm_id),
                                fecha = seq(from = as.Date(sprintf("%d-01-01", ano.desde)), 
                                            to = as.Date(sprintf("%d-12-31", ano.hasta)), by = 'days'))
estadisticas <- series.temporales %>%
  dplyr::right_join(fechas, by = c("omm_id", "fecha")) %>%
  dplyr::group_by(omm_id) %>%
  dplyr::summarise(total = dplyr::n(), disponibles = sum(! is.na(valor)), max_faltantes_consecutivos = MaximosFaltantesConsecutivos(valor)) %>%
  dplyr::mutate(tasa_disponibles = disponibles/total)

estaciones.estudio <- estaciones %>%
  dplyr::inner_join(
    dplyr::filter(estadisticas, (tasa_disponibles >= 0.7) & (max_faltantes_consecutivos <= 15)),
    by = c("omm_id"))

series.estudio <- series.temporales %>%
  dplyr::right_join(fechas, by = c("omm_id", "fecha")) %>%
  dplyr::filter(omm_id %in% unique(estaciones.estudio$omm_id))

# Aplicar descomposicion estacional
series.descompuestas <- purrr::map_dfr(
  .x = unique(series.estudio$omm_id),
  .f = function(omm_id) {
    DescomponerSerieTemporal(dplyr::filter(series.estudio, omm_id == !! omm_id))
  }
)

# Aplicar interpolacion
series.interpoladas <- purrr::map_dfr(
  .x = sort(unique(series.descompuestas$fecha)),
  .f = function(fecha) {
    InterpolarValoresFecha(series.descompuestas, estaciones, fecha)
  }
)

save(series.interpoladas, file = "data/SeriesInterpoladas.RData")