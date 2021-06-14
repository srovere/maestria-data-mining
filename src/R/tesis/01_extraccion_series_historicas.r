# -----------------------------------------------------------------------------#
# --- PASO 1. Inicializar entorno ----
# -----------------------------------------------------------------------------#

# i. Borrar objetos existentes en el ambiente
rm(list = ls()); gc()

# ii. Configurar huso horario en UTC
Sys.setenv(TZ = "UTC")

# iii. Cargar paquetes a utilizar
list.of.packages <- c("dplyr", "jsonlite", "magrittr", "purrr", "tidyr", "sf", "yaml")
for (pack in list.of.packages) {
    if (! require(pack, character.only = TRUE)) {
        stop(paste0("Paquete no encontrado: ", pack))
    }
}
rm(pack); gc()

# iv. Cargar archivo de configuración
config <- yaml::yaml.load_file("configuracion_extraccion.yml")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Extraer información ----
# -----------------------------------------------------------------------------#

# i. Definir funciones para realizar extracciones
ConsumirServicioJSON <- function(url, usuario, clave) {
    request  <- httr::GET(url = url,
                          config = httr::authenticate(user = usuario,
                                                      password = clave))
    response <- httr::content(request, as = "text")
    return (jsonlite::fromJSON(response))
}
ConvertirFechaISO8601 <- function(fecha) {
    return (strftime(fecha, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
}

# ii. Extraer metadatos de estaciones
estaciones <- ConsumirServicioJSON(url = sprintf("%s/estaciones", config$api$url),
                                  usuario = config$api$user, clave = config$api$pass) %>%
    # Filtrar estaciones especificadas
    dplyr::filter(omm_id %in% config$extraccion$estaciones$omm_id) %>%
    # Seleccionar atributos
    dplyr::select(omm_id, nombre, latitud, longitud, elevacion)
    
# iii. Extraer registros diarios de tmax, tmin y prcp para cada estación
series_historicas <- purrr::pmap_dfr(
    .l = tidyr::crossing(
            dplyr::select(estaciones, omm_id),
            tibble::tibble(variable_id = config$extraccion$variables)
         ),
    .f = function(omm_id, variable_id) {
        # Definir URL para estacion
        url <- sprintf("%s/registros_diarios/%d/%s/%s/%s", 
                       config$api$url, omm_id, variable_id,
                       ConvertirFechaISO8601(as.Date(config$extraccion$periodo$fecha_desde)),
                       ConvertirFechaISO8601(as.Date(config$extraccion$periodo$fecha_hasta)))
        
        # Extraer datos
        registros_diarios <- ConsumirServicioJSON(url = url, 
                                                  usuario = config$api$user, 
                                                  clave = config$api$pass)
        
        if ("estado" %in% colnames(registros_diarios)) {
            registros_diarios <- registros_diarios %>%
                # Seleccionar solamente registros "aprobados"
                dplyr::filter(estado == 'A') %>%
                # Eliminar columna de estado
                dplyr::select(-estado)
        }   
        
        # Convertir columna fecha a Date
        registros_diarios %<>% dplyr::mutate(fecha = as.Date(fecha))
        
        return(registros_diarios)
    }
)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Transformar información y almacenarla ----
# -----------------------------------------------------------------------------#

# i. Convertir estaciones
stations <- estaciones %>%
    # Cambiar nombres
    dplyr::rename(station_id = omm_id, name = nombre, latitude = latitud, 
                  longitude = longitud, elevation = elevacion) %>%
    # Convertir a objeto SimpleFesture
    sf::st_as_sf(x = ., coords = c("longitude", "latitude"), remove = FALSE, 
                 crs = config$extraccion$proyeccion$latlon) %>%
    # Transformar a coordenadas planaers
    sf::st_transform(x = ., crs = config$extraccion$proyeccion$planar)


# ii. Convertir series climativas
climate <- series_historicas %>%
    # Pasar a formato ancho
    tidyr::pivot_wider(names_from = "variable_id", values_from = "valor") %>%
    # Cambiar nombres
    dplyr::rename(date = "fecha", station_id = "omm_id", tmax = "tmax", tmin = "tmin", prcp = "prcp")

# iii. Almacenar informacion
save(stations, climate, file = "input/SeriesHistoricas.RData")
# ------------------------------------------------------------------------------