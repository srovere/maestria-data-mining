# -----------------------------------------------------------------------------#
# --- PASO 1. Borrar memoria y cargar librerias necesarias ----
# -----------------------------------------------------------------------------#

# i. Limpiar entorno
rm(list = ls()); gc()

# ii. Cargar paquetes
list.of.packages <- c("dplyr", "jsonlite", "magrittr", "purrr", "tidyr", "sirad", 
                      "sf", "xts", "yaml")
for (package in package.list) {
	require(package, character.only = TRUE)
}
rm(package, list.of.packages); invisible(gc())

# iii. Cargar metodo de calculo de radiacion Bristow-Campbell
source("lib/Bristow-Campbell.R", echo = FALSE)

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
    if (length(lista$omm_id) > 0) {
      return(tibble::as_tibble(lista))
    }
    return(NULL)
  }
)
# ------------------------------------------------------------------------------

estimarRadiacion <- function(estaciones, registrosDiarios, ap.cal=NULL, bc.cal=NULL, svk.cal=NULL) {
  if(is.null(ap.cal)) ap.cal <- ap.cal.default;
  if(is.null(bc.cal)) bc.cal <- bc.cal.default;
  if(is.null(svk.cal)) svk.cal <- svk.cal.default;
  
  totalNoEstimado <- 0
  
  ## Llenamos la radiación extraterrestre y la duración del día en horas para cada registro.
  for(i in 1:nrow(estaciones)){
    estacion <- estaciones[i, ]
    
    # Obtenemos los índices de los registros que corresponden a la estación en cuestión.
    registros_idx <- which(registrosDiarios$omm_id == estacion$omm_id)
    
    # Calculamos la radiación extraterrestre y el largo del día en horas para cada día.
    extrat.data <- sirad::extrat(i=sirad::dayOfYear(registrosDiarios[registros_idx, "fecha"]), lat=sirad::radians(estacion$lat_dec))
    
    registrosDiarios[registros_idx, 'extrat'] <- extrat.data$ExtraTerrestrialSolarRadiationDaily
    registrosDiarios[registros_idx, 'daylength'] <- extrat.data$DayLength
    
    registros_estacion <- registrosDiarios[registros_idx, ]
    # Determinamos los índices de los registros que se pueden calcular por cada método.
    bcIndexes <- which(x=(!is.na(registrosDiarios[registros_idx, "tmax"]) &
                            !is.na(registrosDiarios[registros_idx, "tmin"])))
    bcIndexes <- registros_idx[bcIndexes]
    
    svkIndexes <- which(x=(!is.na(registrosDiarios[bcIndexes, "nub"]) &
                             registrosDiarios[bcIndexes, "nub"] <= 8  &
                             registrosDiarios[bcIndexes, "nub"] >= 0))
    svkIndexes <- bcIndexes[svkIndexes]
    
    apIndexes <- which(x=(!is.na(registrosDiarios[registros_idx, "helio"]) &
                            registrosDiarios[registros_idx, "helio"] > 0 &
                            registrosDiarios[registros_idx, "helio"] < registrosDiarios[registros_idx, "daylength"]))
    apIndexes <- registros_idx[apIndexes]
    
    # Filtramos los índices para calcular con los métodos de más precisión (AP > SVK > BC).
    svkIndexes <- svkIndexes[!svkIndexes %in% apIndexes]
    bcIndexes <- bcIndexes[!bcIndexes %in% apIndexes & !bcIndexes %in% svkIndexes]
    
    if(length(apIndexes) > 0) {
      relHelio <- (registrosDiarios[apIndexes,"helio"] / registrosDiarios[apIndexes,"daylength"])
      
      registrosDiarios[apIndexes,"rad"] <- estimate.angstromprescott(rel.helio= relHelio,
                                                                     extrat= registrosDiarios[apIndexes,"extrat"],
                                                                     ap.coef= ap.cal)
      
      registrosDiarios[apIndexes,"metodo"] <- "estimate.angstromprescott"
    }
    
    if(length(svkIndexes) > 0){
      registrosDiarios[svkIndexes,"rad"] <- estimate.supitvankappel(tmax= registrosDiarios[svkIndexes,"tmax"],
                                                                    tmin= registrosDiarios[svkIndexes,"tmin"],
                                                                    cloudcover= registrosDiarios[svkIndexes,"nub"],
                                                                    extrat= registrosDiarios[svkIndexes,"extrat"],
                                                                    svk.coef= svk.cal)
      
      registrosDiarios[svkIndexes,"metodo"] <- "estimate.supitvankappel"
    }
    
    if(length(bcIndexes) > 0) {
      bcEstimate <- estimate.bristowcampbell.xts(xtsdata=xts::xts(registrosDiarios[registros_idx,], order.by=registrosDiarios[registros_idx, "fecha"]),
                                                 days= registrosDiarios[bcIndexes, "fecha"],
                                                 bc.coef= bc.cal)
      
      registrosDiarios[bcIndexes, "rad"] <- bcEstimate
      
      registrosDiarios[bcIndexes,"metodo"] <- "estimate.bristowcampbell.xts"
    }
    
    noEstimados <- length( which(is.na(registrosDiarios[registros_idx,]$rad)))
    
    totalNoEstimado <- totalNoEstimado + noEstimados
    
    # Imprimimos un resumen de la estimación realizada.
    #         writeLines(paste0("Estación ", estacion$omm_id, " (", estacion$nombre, ")", "\n\t ",
    #                           length(bcIndexes), " registros se estiman por BC", "\n\t\t",
    #                           length(svkIndexes), " por SVK\n\t\t",
    #                           length(apIndexes), " por AP\n\t\t",
    #                           "Registros totales: ", length(registros), " (",
    #                           (noEstimados), " no estimados).\n"))
  }
  
  #     writeLines(paste("Total de registros procesados: ", nrow(registrosDiarios)))
  #     writeLines(paste("Total de registros no estimados: ", totalNoEstimado))
  
  l <- list()
  l$not_estimated <- totalNoEstimado
  l$results <- registrosDiarios
  return(l)
}
