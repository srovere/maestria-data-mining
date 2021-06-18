# -----------------------------------------------------------------------------#
# --- PASO 1. Borrar memoria y cargar librerias necesarias ----
# -----------------------------------------------------------------------------#

# i. Limpiar entorno
rm(list = ls()); gc()

# ii. Cargar paquetes
list.of.packages <- c("dplyr", "jsonlite", "magrittr", "purrr", "tidyr", "sirad", 
                      "sf", "xts", "yaml")
for (package in list.of.packages) {
	require(package, character.only = TRUE)
}
rm(package); invisible(gc())

# iii. Cargar librerias necesarias
source("../lib/Task.R", echo = FALSE)

# iv. Cargar archivo de configuracion
config <- yaml::yaml.load_file("configuracion_calculo_radiacion.yml")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Cargar datos ----
# -----------------------------------------------------------------------------#

# Cargar datos de series historicas y realizar modificaciones para informe
load("input/SeriesHistoricas.RData")
stations <- stations %>%
  dplyr::mutate(lat_dec = sf::st_coordinates(geometry)[,'Y'])

# Cargar datos de series sinteticas
simulated_climate <- data.table::fread(file = "input/SeriesSinteticas.csv") %>%
  tibble::as_tibble() %>%
  dplyr::select(realization, station_id, point_id, date, tmax, tmin, prcp)

# Definir data frame de constantes
constantes <- purrr::map_dfr(
  .x = config$constantes$bristow.campbell,
  .f = function(lista) {
    if (length(lista$station_id) > 0) {
      return(tibble::as_tibble(lista))
    }
    return(NULL)
  }
) %>% tidyr::pivot_longer(cols = c(-station_id), names_to = "constante", values_to = "valor")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Definir funcion para calcular datos de radiacion ----
# -----------------------------------------------------------------------------#

EstimarRadiacion <- function(input.value, simulated_climate, constantes) {
  # Cargar libreria con funciones para calcular radiacion por metodo de Bristow-Campbell
  source("lib/Bristow-Campbell.R", echo = FALSE)
  
  # Obtener estacion y sus datos y parametros correspondientes
  estacion           <- input.value
  registros_estacion <- simulated_climate %>%
    dplyr::filter(station_id == estacion$station_id) %>%
    dplyr::mutate(doy = lubridate::yday(date)) %>% 
    dplyr::arrange(date)
  bc.coef            <- constantes %>%
    dplyr::filter(station_id == estacion$station_id) %>%
    dplyr::select(-station_id) %>%
    tibble::deframe() %>%
    as.list()
  
  # Calculamos la radiación extraterrestre y el largo del día en horas para cada día.
  days_of_year       <- sort(unique(dplyr::pull(registros_estacion, "doy")))
  extrat.data        <- sirad::extrat(i = days_of_year, lat = sirad::radians(estacion$latitude))
  extrat.data.t      <- tibble::tibble(doy = days_of_year, 
                                       extrat = extrat.data$ExtraTerrestrialSolarRadiationDaily,
                                       daylength = extrat.data$DayLength)
  
  # Calcular radiacion por metodo de Bristow-Campbell
  registros_estacion <- registros_estacion %>%
    dplyr::inner_join(extrat.data.t, by = c("doy")) %>%
    dplyr::mutate(rad = estimate.bristowcampbell(tmax, tmin, extrat, bc.coef)) %>%
    dplyr::select(-doy, -extrat, -daylength) %>%
    dplyr::arrange(realization, date)

  return(registros_estacion)
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Ejecutar calculo de forma paralela y almacenar resultados ----
# -----------------------------------------------------------------------------#

# i. Crear tarea distribuida
task.calculo.radiacion <- Task$new(func.name = "EstimarRadiacion",
                                   packages = list.of.packages)

# ii. Ejecutar tarea distribuida
simulated_climate_radiacion <- task.calculo.radiacion$run(number.of.processes = parallel::detectCores(),
                                                          input.values = stations, 
                                                          simulated_climate = simulated_climate,
                                                          constantes = constantes)

# iii. Si hay errores, terminar ejecucion
task.calculo.radiacion.errors <- task.calculo.radiacion$getErrors()
if (length(task.calculo.radiacion.errors) > 0) {
  for (error.obj in task.calculo.radiacion.errors) {
    warning(paste0("(station_id=", error.obj$input.value$station_id, "): ", error.obj$error))
  }
  stop("Finalizando script de forma ANORMAL")
}

# iv. Almacentar resultados
simulated_climate <- data.table::rbindlist(simulated_climate_radiacion)
save(simulated_climate, file = "input/SeriesSinteticasConRadiacion.RData")
# ------------------------------------------------------------------------------