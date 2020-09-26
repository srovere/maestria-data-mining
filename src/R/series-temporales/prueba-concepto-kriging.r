rm(list = objects())

require(automap)
require(dplyr)
require(gstat)
require(httr)
require(jsonlite)
require(purrr)
require(tidyr)
require(sf)
require(stars)

# Se lee el dataset
argentina <- base::readRDS("data/gadm36_ARG_0_sf.rds")
load("data/Dataset.RData")

# Seleccionamos el año 2018
series.temporales <- datos.estaciones %>%
  dplyr::mutate(fecha = as.Date(fecha)) %>%
  dplyr::filter(lubridate::year(fecha) == 2018)

# Funcion para calcular la maxima cantidad de faltantes consecutivos
MaximosFaltantesConsecutivos <- function(valores) {
  resultados <- rle(is.na(valores))
  pos.na     <- which(resultados$values)
  if (length(pos.na) > 0) {
    return (max(resultados$lengths[pos.na]))
  } else {
    return (0)
  }
}

# Filtrado: quedan estaciones con mas del 75% de los datos y no mas de 10 dias consecutivos de datos faltantes.
fechas       <- tidyr::crossing(omm_id = unique(series.temporales$omm_id),
                                fecha = seq(from = as.Date("2018-01-01"), to = as.Date("2018-12-31"), by = 'days'))
estadisticas <- series.temporales %>%
  dplyr::right_join(fechas, by = c("omm_id", "fecha")) %>%
  dplyr::group_by(omm_id) %>%
  dplyr::summarise(total = dplyr::n(), disponibles = sum(! is.na(valor)), max_faltantes_consecutivos = MaximosFaltantesConsecutivos(valor)) %>%
  dplyr::mutate(tasa_disponibles = disponibles/total)

estaciones.estudio <- estaciones %>%
  dplyr::inner_join(
    dplyr::filter(estadisticas, (tasa_disponibles >= 0.75) & (max_faltantes_consecutivos <= 10)),
    by = c("omm_id"))

series.estudio <- series.temporales %>%
  dplyr::right_join(fechas, by = c("omm_id", "fecha")) %>%
  dplyr::filter(omm_id %in% unique(estaciones.estudio$omm_id))

# Definir filtro para capturar media y frecuencia fundamental
N           <- length(unique(series.estudio$fecha))
tiempo      <- seq(from = 1, to = N)
ciclos      <- 1
filtro      <- rep(0, N)
filtro[1:2] <- 1
filtro[N]   <- 1
plot(tiempo, filtro)

# Definicion de Slow Discrete Fourier Transform (ver ?stats::fft)
sdft <- function(z, inverse = FALSE) {
  n <- length(z)
  if(n == 0) return(z)
  k <- 0:(n-1)
  ff <- (if(inverse) 1 else -1) * 2*pi * 1i * k/n
  vapply(1:n, function(h) sum(z * exp(ff*(h-1)), na.rm = TRUE), complex(1))
}

# Para cada estaciones, calcular la FFT, aplicar filtro y aplicar transformada inversa
series.descompuestas <- purrr::map_dfr(
  .x = unique(series.estudio$omm_id),
  .f = function(omm_id) {
    dato.estacion <- series.estudio %>%
      dplyr::filter(omm_id == !! omm_id) %>%
      dplyr::arrange(fecha) %>%
      dplyr::pull(valor)
    
    fft.estacion  <- sdft(dato.estacion)
    fft.filtro    <- fft.estacion * filtro
    dato.filtrado <- as.double(sdft(fft.filtro, inverse = TRUE) / N)
    
    serie.descompuesta <- series.estudio %>%
      dplyr::filter(omm_id == !! omm_id) %>%
      dplyr::arrange(fecha) %>%
      dplyr::mutate(envolvente = dato.filtrado, ruido = valor - envolvente) 
    return (serie.descompuesta)
  }
)

# Se toma el ruido de una fecha, ej: 2018-04-20
fecha        <- as.Date("2018-04-20")
ruidos.fecha <- estaciones.estudio %>%
  dplyr::inner_join(
    dplyr::filter(series.descompuestas, fecha == !! fecha),
    by = c("omm_id")
  )

# Obtener variograma y ajustar modelo
locations  <- dplyr::filter(ruidos.fecha, ! is.na(ruido))
variograma <- gstat::variogram(ruido ~ 1, data = locations)
modelo     <- automap::autofitVariogram(formula = ruido ~ 1, input_data = sf::as_Spatial(locations))
kriging    <- gstat::krige(ruido ~ 1, locations = locations, newdata = ruidos.fecha, model = modelo$var_model) %>%
  dplyr::rename(ruido_prediccion = var1.pred, varianza = var1.var) %>%
  dplyr::mutate(omm_id = dplyr::pull(ruidos.fecha, omm_id), ruido_original = dplyr::pull(ruidos.fecha, ruido), error = ruido_prediccion - ruido_original)

# Se grafica el campo interpolado en una grilla regular  
grilla.regular <- sf::st_make_grid(x = argentina, what = "centers", cellsize = c(0.1, 0.1))
kriging.grilla <- gstat::krige(ruido ~ 1, locations = locations, newdata = grilla.regular, model = modelo$var_model)
kriging.arg    <- as(stars::st_rasterize(sf = kriging.grilla), "Raster") %>%
  raster::mask(x = ., mask = argentina)
plot(kriging.arg)
