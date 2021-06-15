# -----------------------------------------------------------------------------#
# --- PASO 1. Borrar memoria y cargar librerias necesarias ----
# -----------------------------------------------------------------------------#

# i. Limpiar entorno
rm(list = ls()); gc()

# ii. Cargar paquetes
package.list <- c("Cairo", "dplyr", "dplyr", "ggplot2", "lubridate", 
                  "reshape2", "rmarkdown", "tidyr", "yaml", "xts", "magrittr")
for (package in package.list) {
	require(package, character.only = TRUE)
}
rm(package, package.list); invisible(gc())

# iii. Definir cairo para graficos
options(bitmapType="cairo")

# iv. Cargar archivo de configuracion
config <- yaml::yaml.load_file("configuracion_validacion_series_sinteticas.yml")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Cargar datos ----
# -----------------------------------------------------------------------------#

# Cargar datos de series historicas y realizar modificaciones para informe
load("input/SeriesHistoricas.RData")
stations <- stations %>%
  dplyr::mutate(lat_dec = sf::st_coordinates(geometry)[,'Y'])
fit_stations <- stations

# Cargar datos de series sinteticas
simulated_climate <- data.table::fread(file = "input/SeriesSinteticas.csv") %>%
  tibble::as_tibble() %>%
  dplyr::select(realization, station_id, point_id, date, tmax, tmin, prcp)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Definir funciones generales para validacion ----
# -----------------------------------------------------------------------------#

# Funcion para transformar número de estacion astrónomica en nombre
NumeroEstacionANombre <- function(x) {
  season.name <- factor(x, levels = c(1, 2, 3, 4),
                        labels = c("Summer", 'Autumn', 'Winter', 'Spring'))
  
  return(season.name)
}

# Funcion para buscar serie temporal observada de variable
BuscarSerieTemporalObservada <- function(climate, station.id, variable, periodo.desde, periodo.hasta) {
  # 1. Acotar los datos a la variable y periodo seleccionado
  columnas           <- c("date", variable)
  registros.variable <- climate %>%
    dplyr::filter(station_id == station.id) %>%
    dplyr::filter(date >= periodo.desde) %>%
    dplyr::filter(date <= periodo.hasta) %>%
    dplyr::select(one_of(columnas)) %>%
    dplyr::arrange(date) %>%
    as.data.frame()
  
  # 3. Crear objeto XTS y validar completitud de la serie
  fechas     <- seq(from = lubridate::ymd(periodo.desde),
                    to   = lubridate::ymd(periodo.hasta),
                    by   = 'days')
  xts        <- xts::xts(x = registros.variable[, variable],
                         order.by = registros.variable[, "date"])
  colnames(xts) <- "OBS"
  xts.fechas <- index(xts)
  if (! ((length(fechas) == length(xts.fechas)) &&
         (fechas[1] == xts.fechas[1]) &&
         (fechas[length(fechas)] == xts.fechas[length(xts.fechas)]))) {
    warning(paste0("La serie para la estacion ", station.id, " no esta completa"))
  }
  return (xts)
}

# Sobrecarga de funcion de busqueda de realizaciones
BuscarRealizaciones <- function(simulated_climate, stations) {
  # 1. Determinar periodo a analizar
  fechas <- lubridate::ymd(unique(simulated_climate$date))
  
  # 2. Determinar estaciones a analizar
  station.id <- as.vector(unique(stations$station_id))
  
  # 3. Buscar realizaciones
  max_simul    <- max(simulated_climate$realization)
  seq.n        <- seq(from = 1, to = max_simul)
  simulaciones <- list()
  for (i in seq(from = 1, to = length(station.id))) {
    cat(paste0("Estacion ", station.id[i], "\n"))
    r.estacion <- list()
    for (variable in config$variables) {
      
      r.variable <- xts::xts(simulated_climate %>%
                               dplyr::filter(., station_id == station.id[i]) %>%
                               dplyr::select(., station_id, realization, date, variable) %>%
                               tidyr::pivot_wider(., names_from = 'realization', values_from = variable) %>%
                               dplyr::select(., -c('station_id', 'date')), order.by = fechas)
      
      colnames(r.variable) <- paste0("R", seq(from = 1, to = max_simul))
      r.estacion[[variable]] <- r.variable
    }
    simulaciones[[as.character(station.id[i])]] <- r.estacion
  }
  
  realizaciones <- list(simulaciones = simulaciones, 
                        estaciones = station.id, 
                        periodo = list(desde = min(fechas), hasta = max(fechas)))
  return (realizaciones)
}

# Generacion de DataFrame largo
AsLongDataFrame <- function(xts.obj, 
                            periodo.mapping.func.name = NULL, 
                            periodo.mapping.func.params = list(), 
                            group.by.func.name = NULL, 
                            group.by.func.params = list(),
                            preservar.serie = TRUE) {
  if (! is.null(periodo.mapping.func.name)) {
    fechas.obj <- index(xts.obj)
    periodo.mapping.func.params$x <- index(xts.obj)
    periodos.obj <- do.call(periodo.mapping.func.name, periodo.mapping.func.params)
    df.obj <- data.frame(coredata(xts.obj)) %>%
      dplyr::mutate(periodo = periodos.obj) %>%
      reshape2::melt(
        id.vars=c("periodo"),
        variable.name="serie",
        value.name="valor",
        factorsAsStrings=FALSE)
    
    if (! is.null(group.by.func.name)) {
      summarise.function <- paste0(group.by.func.name, "(valor",
        lapply(names(group.by.func.params), FUN = function(x, l) {
          return (paste0(", ", x, "=", l[[x]]))    
        }, l = group.by.func.params), ")")
      
      if (preservar.serie) {
        df.obj <- df.obj %>%
          dplyr::group_by(periodo, serie) %>%
          dplyr::summarise(valor_agregado = !! rlang::parse_expr(summarise.function)) %>%
          dplyr::rename(valor = valor_agregado)
      } else {
        df.obj <- df.obj %>%
          dplyr::group_by(periodo) %>%
          dplyr::summarise(valor_agregado = !! rlang::parse_expr(summarise.function)) %>%
          dplyr::rename(valor = valor_agregado)
      }
    }
  } else {
    df.obj <- data.frame(coredata(xts.obj)) %>%
      dplyr::mutate(fecha = index(xts.obj)) %>%
      reshape2::melt(
        id.vars=c("fecha"),
        variable.name="serie",
        value.name="valor",
        factorsAsStrings=FALSE)
  }
  
  return (df.obj)
}

# Funcion para calcular cantidad de dias lluviosos en un periodo
IsWetDay <- function(x) {
  return (ifelse(x > config$umbral.precipitacion, 1, 0))
}
WetDays <- function(x, na.rm = TRUE) {
  dias.lluviosos <- length(which(IsWetDay(x) == 1))
  return (dias.lluviosos)
}
WetDayProbability <- function(x, na.rm = TRUE) {
  dias.lluviosos <- WetDays(x, na.rm)
  dias.con.datos <- length(which(! is.na(x)))
  return (dias.lluviosos / dias.con.datos)
}

# Funcion para completar una serie XTS (cuando la misma tiene huecos)
CompletarSerieXTS <- function(xts.irregular, primera.fecha, ultima.fecha, valor.omitido = NA) {
  xts.all.dates  <- seq(primera.fecha, ultima.fecha, by="day")
  xts.all.values <- rep(NA, length.out=length(xts.all.dates))
  xts.all.nas    <- xts::xts(xts.all.values, order.by=xts.all.dates)
  xts.merge      <- xts::merge.xts(xts.irregular, xts.all.nas)
  
  names(xts.merge) <- c("xts.irregular", "xts.all.nas")
  if (is.null(xts.merge$xts.irregular)) {
    xts.regular <- xts.all.nas
  } else {
    xts.regular <- xts.merge$xts.irregular
  }
  if (! is.na(valor.omitido)) {
    coredata(xts.regular)[is.na(xts.regular)] <- valor.omitido
  }
  return (xts.regular)
}

# Funcion para identificar eventos de lluvia
# tipo.evento = {1, 0} con 1 = dia lluvioso, 0 = dia seco
# funcion.agregacion = funcion para agrupar los dias en periodos (ej: quarter)
# na.action = { "exclude.na", "exclude.prev", "exclude.next", "exclude.all" } donde
#   exclude.na   = se ecluyen solamente las secuencias de NA
#   exclude.prev = se ecluyen las secuencias de NA y la secuencia anterior
#   exclude.next = se ecluyen las secuencias de NA y la secuencia posterior
#   exclude.all  = se ecluyen las secuencias de NA y las secuencias anterior y posterior
# umbral.lluvia = umbral de precipiracion para distinguir dias lluviosos de dias secos
IdentificarEventosLluvia <- function(xts.lluvia, tipo.evento = 1, funcion.agregacion = lubridate::quarter,
                                     na.action = "exclude.all", umbral.lluvia = 0.1) {
  # 1. Completar serie XTS
  missing.value       <- -1
  fechas.xts.lluvia   <- index(xts.lluvia)
  xts.lluvia.completa <- CompletarSerieXTS(xts.lluvia, fechas.xts.lluvia[1], fechas.xts.lluvia[length(xts.lluvia)], valor.omitido = missing.value)
  
  # 2. Transformar serie XTS a data frame
  df.lluvia.completa <- data.frame(fecha = index(xts.lluvia.completa), prcp = as.vector(coredata(xts.lluvia.completa)))
  
  # 3. Identificar eventos lluviosos y secos
  df.eventos <- df.lluvia.completa %>%
    dplyr::mutate(evento = ifelse(prcp >= umbral.lluvia, 1, ifelse(prcp >= 0, 0, missing.value))) %>%
    dplyr::select(fecha, evento)
  primera.fecha <- df.eventos[1, "fecha"]
  
  # 4. Identificar secuencias de eventos
  secuencias.eventos <- rle(df.eventos[,"evento"])
  tipo.eventos       <- secuencias.eventos[["values"]]
  longitud.eventos   <- secuencias.eventos[["lengths"]]
  
  # 5. Generar data frame con (fecha.inicio, evento, duracion)
  df.secuencias.duracion <- data.frame(evento = tipo.eventos, duracion = longitud.eventos,
                                       cumsum.duracion = cumsum(longitud.eventos)) %>%
    dplyr::mutate(offset = cumsum.duracion - duracion)
  df.secuencias.duracion[,"fecha"] <- primera.fecha + df.secuencias.duracion[,"offset"]
  tabla.secuencias       <- df.secuencias.duracion %>%
    dplyr::select(fecha, evento, duracion)
  
  # 6. Identificar filas a filtrar por NAs
  filas.na        <- which(tabla.secuencias[,"evento"] == missing.value)
  filas.exclusion <- NULL
  if (na.action == "exclude.na") {
    filas.exclusion <- filas.na
  } else if (na.action == "exclude.prev") {
    filas.anteriores <- filas.na - 1
    filas.anteriores <- filas.anteriores[filas.anteriores > 0]
    filas.exclusion  <- sort(c(filas.anteriores, filas.na))
  } else if (na.action == "exclude.next") {
    filas.posteriores <- filas.na + 1
    filas.posteriones <- filas.posteriores[filas.posteriores <= nrow(tabla.secuencias)]
    filas.exclusion   <- sort(c(filas.na, filas.posteriores))
  } else if (na.action == "exclude.all") {
    filas.anteriores <- filas.na - 1
    filas.anteriores <- filas.anteriores[filas.anteriores > 0]
    filas.posteriores <- filas.na + 1
    filas.posteriones <- filas.posteriores[filas.posteriores <= nrow(tabla.secuencias)]
    filas.exclusion   <- sort(c(filas.anteriores, filas.na, filas.posteriones))
  } else {
    stop(paste0("Metodo de exclusion de NA desconocido: ", na.action))
  }
  filas.exclusion  <- unique(filas.exclusion)
  
  # 7. Filtrar filas de NAs y por tipo de evento seleccionado.
  if (length(filas.exclusion) > 0) {
    tabla.secuencias <- tabla.secuencias[-filas.exclusion,]
  }
  tabla.secuencias.evento <- tabla.secuencias %>% dplyr::filter(evento == tipo.evento)
  
  
  # 8. Agrupar por funcion de agregacion
  tabla.secuencias.agregado <- tabla.secuencias.evento %>%
    dplyr::mutate(grupo = do.call(funcion.agregacion, list(x=fecha, fiscal_start = 12))) %>%
    dplyr::select(grupo, duracion) %>%
    dplyr::group_by(grupo, duracion) %>%
    dplyr::summarise(total = n())
  
  return (tabla.secuencias.agregado)
}

# Funcion para generar muestra a partir de tabla de eventos
GenerarMuestraEventos <- function(tabla.eventos) {
  muestras <- NULL
  for (valor.grupo in unique(unlist(tabla.eventos[,"grupo"]))) {
    datos.grupo <- tabla.eventos %>%
      dplyr::filter(grupo == valor.grupo)
    
    muestras.grupo <- c()
    for (i in seq(from=1, to=nrow(datos.grupo))) {
      duracion <- as.integer(datos.grupo[i,"duracion"])
      total    <- as.integer(datos.grupo[i,"total"])
      muestras.grupo <- c(muestras.grupo, rep(x = duracion, times = total))
    }
    if (is.null(muestras)) {
      muestras <- data.frame(grupo = valor.grupo, muestra = muestras.grupo)
    } else {
      muestras <- rbind(muestras, data.frame(grupo = valor.grupo, muestra = muestras.grupo))
    }
  }
  return (muestras)
}

# Devuelve el nombre completo de una variable
GetVariableName <- function(variable, with.units = FALSE) {
  variables <- list(prcp = "Rainfall", tmin = "Minimum temperature", tmax = "Maximum temperature")
  units     <- list(prcp = " (mm)", tmin = " (ºC)", tmax = " (ºC)")
  str       <- paste0(variables[[variable]], ifelse(with.units, units[[variable]], ""))
  return (str)
}

# Devuelve el nombre de una funcion de agregacion
GetGroupByFunctionName <- function(group.by.func.name) {
  return (gsub('\\.GlobalEnv\\$', '', group.by.func.name))
}

# Obtiene la autocorrelacion para un mes de la serie
GetAutocorrelation <- function(xts.series, a_month = NULL, na.action = na.pass) {
  acr.values <- c()
  if (! is.null(a_month)) {
    indices <- which(month(index(xts.series)) == a_month)
  }
  for (col in seq(from = 1, to = ncol(xts.series))) {
    x          <- xts.series[,col]
    if (! is.null(a_month)) {
      x[-indices,] <- NA
    }
    a          <- stats::acf(x = as.vector(coredata(x)),
                             lag.max = 1,
                             type = "correlation",
                             na.action = na.action,
                             plot = FALSE)
    acr.values <- c(acr.values, a$acf[2,1,1])
  }
  return (acr.values)
}

# Obtiene la correlacion cruzada (Lag-0) entre 2 variables para un mes de la serie
GetCrossCorrelation <- function(xts.series.1, xts.series.2, a_month = NULL, na.action = na.pass) {
  ccr.values <- c()
  if (! is.null(a_month)) {
    indices <- which(month(index(xts.series.1)) == a_month)
  }
  for (col in seq(from = 1, to = ncol(xts.series.1))) {
    x          <- xts.series.1[,col]
    y          <- xts.series.2[,col]
    if (! is.null(a_month)) {
      x[-indices,] <- NA
      y[-indices,] <- NA
    }
    x          <- as.vector(coredata(x))
    y          <- as.vector(coredata(y))
    a          <- stats::ccf(x = x,
                             y = y,
                             lag.max = 0,
                             type = "correlation",
                             na.action = na.action,
                             plot = FALSE)
    ccr.values <- c(ccr.values, as.double(a$acf))
  }
  return (ccr.values)
}

# Funcion para identificar eventos de temperatura
# na.action = { "exclude.na", "exclude.prev", "exclude.next", "exclude.all" } donde
#   exclude.na   = se ecluyen solamente las secuencias de NA
#   exclude.prev = se ecluyen las secuencias de NA y la secuencia anterior
#   exclude.next = se ecluyen las secuencias de NA y la secuencia posterior
#   exclude.all  = se ecluyen las secuencias de NA y las secuencias anterior y posterior
# umbral.temperatura.minima = umbral de temperatura minima
# umbral.temperatura.maxima = umbral de temperatura maxima
IdentificarEventosTemperatura <- function(xts.temperatura, meses, umbral.temperatura.minima = NULL, umbral.temperatura.maxima = NULL, min.dias = 3, na.action = "exclude.all") {
  # 1. Completar serie XTS
  missing.value            <- -9999
  fechas.xts.temperatura   <- index(xts.temperatura)
  xts.temperatura.completa <- CompletarSerieXTS(xts.temperatura, fechas.xts.temperatura[1], fechas.xts.temperatura[length(xts.temperatura)], valor.omitido = missing.value)
  
  # 2. Transformar serie XTS a data frame
  df.temperatura.completa <- data.frame(fecha = index(xts.temperatura.completa), temp = as.vector(coredata(xts.temperatura.completa)))
  
  # 3. Identificar eventos
  df.eventos <- df.temperatura.completa %>%
    dplyr::mutate(evento = ifelse(temp != missing.value, TRUE, NA))
  if (! is.null(umbral.temperatura.minima)) {
    df.eventos$evento <- ifelse(! is.na(df.eventos$evento), df.eventos$evento & (df.eventos$temp <= umbral.temperatura.minima), NA)
  }
  if (! is.null(umbral.temperatura.maxima)) {
    df.eventos$evento <- ifelse(! is.na(df.eventos$evento), df.eventos$evento & (df.eventos$temp >= umbral.temperatura.maxima), NA)
  }
  df.eventos <- df.eventos %>%
    dplyr::select(fecha, evento)
  primera.fecha <- df.eventos[1, "fecha"]
  
  # 4. Identificar secuencias de eventos
  secuencias.eventos <- rle(df.eventos[,"evento"])
  tipo.eventos       <- secuencias.eventos[["values"]]
  longitud.eventos   <- secuencias.eventos[["lengths"]]
  
  # 5. Generar data frame con (fecha.inicio, evento, duracion)
  df.secuencias.duracion <- data.frame(evento = tipo.eventos, duracion = longitud.eventos,
                                       cumsum.duracion = cumsum(longitud.eventos)) %>%
    dplyr::mutate(offset = cumsum.duracion - duracion)
  df.secuencias.duracion[,"fecha"] <- primera.fecha + df.secuencias.duracion[,"offset"]
  tabla.secuencias       <- df.secuencias.duracion %>%
    dplyr::select(fecha, evento, duracion)
  
  # 6. Identificar filas a filtrar por NAs
  filas.na        <- which(is.na(tabla.secuencias[,"evento"]))
  filas.exclusion <- NULL
  if (na.action == "exclude.na") {
    filas.exclusion <- filas.na
  } else if (na.action == "exclude.prev") {
    filas.anteriores <- filas.na - 1
    filas.anteriores <- filas.anteriores[filas.anteriores > 0]
    filas.exclusion  <- sort(c(filas.anteriores, filas.na))
  } else if (na.action == "exclude.next") {
    filas.posteriores <- filas.na + 1
    filas.posteriones <- filas.posteriores[filas.posteriores <= nrow(tabla.secuencias)]
    filas.exclusion   <- sort(c(filas.na, filas.posteriores))
  } else if (na.action == "exclude.all") {
    filas.anteriores <- filas.na - 1
    filas.anteriores <- filas.anteriores[filas.anteriores > 0]
    filas.posteriores <- filas.na + 1
    filas.posteriones <- filas.posteriores[filas.posteriores <= nrow(tabla.secuencias)]
    filas.exclusion   <- sort(c(filas.anteriores, filas.na, filas.posteriones))
  } else {
    stop(paste0("Metodo de exclusion de NA desconocido: ", na.action))
  }
  filas.exclusion  <- unique(filas.exclusion)
  
  # 7. Filtrar filas de NAs y aquellas que no cumplan con el requisito del evento
  if (length(filas.exclusion) > 0) {
    tabla.secuencias <- tabla.secuencias[-filas.exclusion,]
  }
  tabla.secuencias.evento <- tabla.secuencias %>% 
    dplyr::filter(evento == TRUE) %>%
    dplyr::filter(month(fecha) %in% meses) %>%
    dplyr::filter(duracion >= min.dias) %>%
    dplyr::select(fecha, duracion)
  
  return (tabla.secuencias.evento)
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Definir funciones para generar graficos ----
# -----------------------------------------------------------------------------#

GenerarQQPlotPorPeriodo <- function(xts.observacion, xts.realizaciones, station.id, variable, percentile.threshold.extreme, periodo.mapping.func.name, title, subtitle) {
  df.sim <- AsLongDataFrame(xts.obj = xts.realizaciones)
  df.obs <- AsLongDataFrame(xts.obj = xts.observacion)
  df.tot <- NULL
  qs     <- unique(do.call(periodo.mapping.func.name, list(x=df.sim$fecha)))
  for (q in qs) {
    df.sim.q <- df.sim %>%
      dplyr::filter(quarter(fecha, fiscal_start = 12) == q)
    df.obs.q <- df.obs %>%
      dplyr::filter(quarter(fecha, fiscal_start = 12) == q)
    percentile.threshold <- quantile(df.obs.q$valor, c(1 - percentile.threshold.extreme, percentile.threshold.extreme), na.rm = TRUE)
    df.tot.q <- as.data.frame(stats::qqplot(x = df.obs.q$valor, y = df.sim.q$valor, plot.it=FALSE))
    df.tot.q$q <- q
    df.tot.q$season <- NumeroEstacionANombre(df.tot.q$q)
    df.tot.q$extreme <- if_else(df.tot.q$x >= percentile.threshold[1] & df.tot.q$x <= percentile.threshold[2],
                                'normal', 'extreme')
    
    if (is.null(df.tot)) {
      df.tot <- df.tot.q
    } else {
      df.tot <- rbind(df.tot, df.tot.q)
    }
  }
  g <- ggplot2::ggplot(data = df.tot) +
    ggplot2::geom_point(aes(x = x, y = y, color = extreme)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "#d53e4f") +
    ggplot2::scale_color_manual(name = "", labels = c(""), values = c('red', "#3288bd")) +
    ggplot2::facet_wrap(~ season, scales = "free") +
    ggplot2::labs(x = paste0("Observed ", GetVariableName(variable, TRUE)), y = paste0("Simulated ", GetVariableName(variable, TRUE)), title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return (g)
}

GenerarPDFPrecipitacionesDiasHumedos <- function(xts.observacion, xts.realizaciones, title, subtitle) {
  prcp.trim.obs <- data.frame(Date = index(xts.observacion), DailyRainfall = as.vector(coredata(xts.observacion))) %>%
    dplyr::mutate(Year = year(Date), Quarter = quarter(Date, fiscal_start = 12), Realization = "Observed") %>%
    dplyr::filter(DailyRainfall > 0.5) %>%
    dplyr::select(Date, Quarter, Year, DailyRainfall, Realization)
  
  prcp.trim.sim <- cbind(Date = index(xts.realizaciones), as.data.frame(xts.realizaciones)) %>%
    tidyr::gather(key = R, value = DailyRainfall, -Date) %>%
    dplyr::mutate(Realization = "Simulated") %>%
    dplyr::mutate(Year = year(Date), Quarter = quarter(Date, fiscal_start = 12)) %>%
    dplyr::filter(DailyRainfall > 0.5) %>%
    dplyr::filter(Year %in% unique(prcp.trim.obs$Year)) %>%
    dplyr::select(Date, Quarter, Year, DailyRainfall, Realization)
  prcp.trim.all <- rbind(prcp.trim.obs, prcp.trim.sim)
  
  # Create facet labels
  labels = c("Summer", 'Autumn', 'Winter', 'Spring')
  names(labels) <- c("1", "2", "3", "4")
  
  g <- ggplot2::ggplot(data = prcp.trim.all) +
    ggplot2::geom_density(mapping = aes(x = DailyRainfall, fill = Realization), alpha = 0.5) +
    ggplot2::facet_wrap(~Quarter, nrow = 1, scales = "free", labeller = labeller(Quarter = labels)) + 
    ggplot2::scale_x_log10() +
    ggplot2::scale_fill_manual(name = "", values = c("Simulated" = "#3288bd", "Observed" = "#d53e4f")) +
    ggplot2::labs(x = "Daily rainfall (mm)", y = "Count", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return (g)
}

GenerarHistogramaMediaTemporal <- function(xts.observacion, xts.realizaciones, station.id, variable, title, subtitle) {
  df.sim <- data.frame(x = apply(X = coredata(xts.realizaciones), MARGIN = 2, FUN = mean, na.rm = TRUE))
  g <- ggplot2::ggplot(mapping = aes(x)) +
    ggplot2::geom_histogram(data = df.sim, aes(fill = "sim"), bins = 10) +
    ggplot2::geom_vline(data = data.frame(col = "obs", media = mean(coredata(xts.observacion), na.rm = TRUE)), 
               aes(colour = col, xintercept = media)) +
    ggplot2::scale_fill_manual(name = "", labels = c("Simulated"), values = c("sim" = "#3288bd")) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed"), values = c("obs" = "#d53e4f")) +
    ggplot2::labs(x = GetVariableName(variable, TRUE), y = "Frequency", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return (g)
}

GenerarCDFPorPeriodo <- function(xts.observacion, xts.realizaciones, station.id, variable, periodo.mapping.func.name, title, subtitle) {
  df.sim <- AsLongDataFrame(xts.obj = xts.realizaciones, periodo.mapping.func.name = periodo.mapping.func.name)
  df.obs <- AsLongDataFrame(xts.obj = xts.observacion, periodo.mapping.func.name = periodo.mapping.func.name)
  g <- ggplot2::ggplot()
  for (r in unique(df.sim$serie)) {
    df.sim.r <- df.sim %>% dplyr::filter(serie == r)
    g <- g + ggplot2::stat_ecdf(data = df.sim.r, mapping = aes(x = valor, color = "sim.r"))
  }
  
  if (periodo.mapping.func.name == 'quarter') {
    # Create facet labels
    labels = c("Summer", 'Autumn', 'Winter', 'Spring')
    names(labels) <- c("1", "2", "3", "4")
  } else {
    # Create facet labels
    labels = c("Jan", 'Feb', 'Mar', 'Apr', "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dec")
    names(labels) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
  }
  
  g <- g +
    ggplot2::stat_ecdf(data = df.obs, mapping = aes(x = valor, color = "obs")) +
    ggplot2::stat_ecdf(data = df.sim, mapping = aes(x = valor, color = "sim")) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed", "Simulated", "Simulated (range)"), values = c("obs" = "#d53e4f", "sim" = "#3288bd", "sim.r" = "#cdcdcd")) +
    ggplot2::facet_wrap(~ periodo, scales = "free",  labeller = labeller(periodo = labels)) +
    ggplot2::labs(x = GetVariableName(variable, TRUE), y = "Probability", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return (g)
}

GenerarBoxplotPorMes <- function(xts.observacion, xts.realizaciones, station.id, variable, group.by.func.name, title, subtitle, y.title = GetVariableName(variable, TRUE), acumular.primero.por.mes = FALSE) {
  if (acumular.primero.por.mes) {
    df.obs <- AsLongDataFrame(xts.obj = xts.observacion) %>%
      dplyr::mutate(ano = year(fecha), mes = month(fecha)) %>%
      dplyr::group_by(serie, ano, mes) %>%
      dplyr::summarise(valor.acumulado = sum(valor, na.rm = TRUE)) %>%
      dplyr::group_by(serie, mes) %>%
      dplyr::summarise(valor = do.call(group.by.func.name, list(x = valor.acumulado))) %>%
      dplyr::mutate(fecha = lubridate::ymd(sprintf("%d-%d-%d", lubridate::year(Sys.Date()), mes, 1))) %>%
      dplyr::select(fecha, serie, valor)
    df.sim <- AsLongDataFrame(xts.obj = xts.realizaciones) %>%
      dplyr::mutate(ano = year(fecha), mes = month(fecha)) %>%
      dplyr::group_by(serie, ano, mes) %>%
      dplyr::summarise(valor.acumulado = sum(valor, na.rm = TRUE)) %>%
      dplyr::group_by(serie, mes) %>%
      dplyr::summarise(valor = do.call(group.by.func.name, list(x = valor.acumulado))) %>%
      dplyr::mutate(fecha = lubridate::ymd(sprintf("%d-%d-%d", lubridate::year(Sys.Date()), mes, 1))) %>%
      dplyr::select(fecha, serie, valor)                          
  } else {
    df.sim <- AsLongDataFrame(xts.obj = xts.realizaciones,
                              periodo.mapping.func.name = "month",
                              periodo.mapping.func.params = list(),
                              group.by.func.name = group.by.func.name,
                              group.by.func.params = list(na.rm = TRUE)) %>% 
      dplyr::mutate(fecha = lubridate::ymd(sprintf("%d-%d-%d", lubridate::year(Sys.Date()), periodo, 1)))
    df.obs <- AsLongDataFrame(xts.obj = xts.observacion,
                              periodo.mapping.func.name = "month",
                              periodo.mapping.func.params = list(),
                              group.by.func.name = group.by.func.name,
                              group.by.func.params = list(na.rm = TRUE)) %>% 
      dplyr::mutate(fecha = lubridate::ymd(sprintf("%d-%d-%d", lubridate::year(Sys.Date()), periodo, 1)))
  }
  
  g <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = df.sim, mapping = aes(group = fecha, x = fecha, y = valor, colour = "sim")) +
    ggplot2::geom_point(data = df.obs, mapping = aes(x = fecha, y = valor, colour = "obs"), size = 3, shape = 1) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed", "Simulated"), values = c("obs" = "#d53e4f", "sim" = "#3288bd")) +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ggplot2::labs(x = "Month", y = y.title, title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

GenerarBoxplotPorTrimestreAno <- function(xts.observacion, xts.realizaciones, station.id, variable, group.by.func.name, title, subtitle) {
  df.sim <- AsLongDataFrame(xts.obj = xts.realizaciones,
                            periodo.mapping.func.name = "quarter",
                            periodo.mapping.func.params = list(with_year = TRUE),
                            group.by.func.name = group.by.func.name,
                            group.by.func.params = list(na.rm = TRUE)) %>% dplyr::mutate(q = substring(periodo, 6))
  df.obs <- AsLongDataFrame(xts.obj = xts.observacion,
                            periodo.mapping.func.name = "quarter",
                            periodo.mapping.func.params = list(with_year = TRUE),
                            group.by.func.name = group.by.func.name,
                            group.by.func.params = list(na.rm = TRUE)) %>% dplyr::mutate(q = substring(periodo, 6))
  
  # Create facet labels
  labels = c("Summer", 'Autumn', 'Winter', 'Spring')
  names(labels) <- c("1", "2", "3", "4")
  
  g <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = df.sim, mapping = aes(group = periodo, x = periodo, y = valor, colour = "sim")) +
    ggplot2::geom_point(data = df.obs, mapping = aes(x = periodo, y = valor, colour = "obs"), size = 3, shape = 1) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed", "Simulated"), values = c("obs" = "#d53e4f", "sim" = "#3288bd")) +
    ggplot2::facet_grid(q ~ ., scale = "free", labeller = labeller(q = labels)) +
    ggplot2::labs(x = "Year", y = GetVariableName(variable, TRUE), title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  return (g)
}

GenerarBoxplotAutocorrelaciones <- function(xts.observacion, xts.realizaciones, station.id, variable, title, subtitle) {
  df.corrs   <- NULL
  serie.col  <- paste0("R", seq(from = 1, to = ncol(xts.realizaciones)))
  for (m in 1:12) {
    fake.date <- lubridate::ymd(sprintf("%d-%d-%d", lubridate::year(Sys.Date()), m, 1))
    acr.obs <- GetAutocorrelation(xts.observacion, m)
    if (is.null(df.corrs)) {
      df.corrs <- data.frame(serie = "OBS", mes = fake.date, valor = acr.obs)  
    } else {
      df.corrs <- rbind(df.corrs, data.frame(serie = "OBS", mes = fake.date, valor = acr.obs))
    }
    
    acr.sim  <- GetAutocorrelation(xts.realizaciones, m)
    df.corrs <- rbind(df.corrs, data.frame(serie = serie.col, mes = fake.date, valor = acr.sim))
  }
  
  g <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = df.corrs[df.corrs$serie!="OBS",], mapping = aes(group = mes, x = mes, y = valor, colour = "sim")) +
    ggplot2::geom_point(data = df.corrs[df.corrs$serie=="OBS",], mapping = aes(x = mes, y = valor, colour = "obs"), size = 3, shape = 1) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed", "Simulated"), values = c("obs" = "#d53e4f", "sim" = "#3288bd")) +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ggplot2::labs(x = "Month", y = "Autocorrelation", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return (g)
}

GenerarBoxplotCorrelacionCruzadaTemperatura <- function(xts.observacion, xts.realizaciones, station.id, title, subtitle) {
  df.corrs   <- NULL
  serie.col  <- paste0("R", seq(from = 1, to = ncol(xts.realizaciones$tmin)))
  for (m in 1:12) {
    fake.date <- lubridate::ymd(sprintf("%d-%d-%d", lubridate::year(Sys.Date()), m, 1))
    ccr.obs <- GetCrossCorrelation(xts.observacion$tmin, xts.observacion$tmax, m)
    if (is.null(df.corrs)) {
      df.corrs <- data.frame(serie = "OBS", mes = fake.date, valor = ccr.obs)  
    } else {
      df.corrs <- rbind(df.corrs, data.frame(serie = "OBS", mes = fake.date, valor = ccr.obs))
    }
    
    ccr.sim  <- GetCrossCorrelation(xts.realizaciones$tmin, xts.realizaciones$tmax, m)
    df.corrs <- rbind(df.corrs, data.frame(serie = serie.col, mes = fake.date, valor = ccr.sim))
  }
  
  g <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = df.corrs[df.corrs$serie!="OBS",], mapping = aes(group = mes, x = mes, y = valor, colour = "sim")) +
    ggplot2::geom_point(data = df.corrs[df.corrs$serie=="OBS",], mapping = aes(x = mes, y = valor, colour = "obs"), size = 3, shape = 1) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed", "Simulated"), values = c("obs" = "#d53e4f", "sim" = "#3288bd")) +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ggplot2::labs(x = "Month", y = "Cross-correlation", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return (g)
}

GenerarBoxplotDiferenciasSimuladas <- function(xts.observacion, xts.realizaciones, station.id, variable, title, subtitle) {
  df.obs <- AsLongDataFrame(xts.obj = xts.observacion,
                            periodo.mapping.func.name = "month",
                            periodo.mapping.func.params = list(),
                            group.by.func.name = "mean",
                            group.by.func.params = list(na.rm = TRUE)) %>%
    dplyr::rename(valor.observado = valor) %>%
    dplyr::select(periodo, valor.observado)
  df.sim <- AsLongDataFrame(xts.obj = xts.realizaciones,
                            periodo.mapping.func.name = "month",
                            periodo.mapping.func.params = list(),
                            group.by.func.name = "mean",
                            group.by.func.params = list(na.rm = TRUE)) %>%
    dplyr::rename(valor.simulado = valor)
  df.dif <- df.sim %>%
    dplyr::inner_join(df.obs, by = "periodo") %>%
    dplyr::mutate(diferencia = valor.simulado - valor.observado) %>%
    dplyr::mutate(fecha = lubridate::ymd(sprintf("%d-%d-%d", lubridate::year(Sys.Date()), periodo, 1)))
    
  g <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = df.dif, mapping = aes(group = fecha, x = fecha, y = diferencia, colour = "sim")) +
    ggplot2::geom_hline(yintercept = 0, colour = "red") +
    ggplot2::scale_color_manual(name = "", labels = c("Simulated"), values = c("sim" = "#3288bd")) +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ggplot2::labs(x = "Month", y = paste0("Difference in ", GetVariableName(variable, TRUE)), title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  return (g)
}

GenerarCDFEventosLluvia <- function(xts.observacion, xts.realizaciones, station.id, tipo.evento.id, title, subtitle) {
  # Identificar eventos de lluvia
  eventos.observados <- IdentificarEventosLluvia(xts.observacion, tipo.evento.id)
  eventos.realizados <- list()
  for (j in seq(from=1, to=ncol(xts.realizaciones))) {
    eventos.realizados[[j]] <- IdentificarEventosLluvia(xts.realizaciones[,j], tipo.evento.id)
  }
  
  # Construir data frame consolidando datos
  muestras           <- GenerarMuestraEventos(eventos.observados)
  muestras[,"serie"] <- "obs"
  for (i in seq(from=1, to=length(eventos.realizados))) {
    muestras.realizacion           <- GenerarMuestraEventos(eventos.realizados[[i]])
    muestras.realizacion[,"serie"] <- i
    muestras                       <- rbind(muestras, muestras.realizacion)
  }
  
  # Create facet labels
  labels = c("Summer", 'Autumn', 'Winter', 'Spring')
  names(labels) <- c("1", "2", "3", "4")
  
  # Generar grafico
  g <- ggplot2::ggplot()
  for (r in unique(muestras$serie)) {
    df.sim.r <- muestras %>% dplyr::filter(serie == r)
    g <- g + ggplot2::stat_ecdf(data = df.sim.r, mapping = aes(x = muestra, color = "sim"))
  }
  df.obs <- muestras %>% dplyr::filter(serie == "obs")
  g <- g +
    ggplot2::stat_ecdf(data = df.obs, mapping = aes(x = muestra, color = "obs")) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed", "Simulated (range)"), values = c("obs" = "#d53e4f", "sim" = "#cdcdcd")) +
    ggplot2::facet_wrap(~ grupo, scales = "free", labeller = labeller(grupo = labels)) +
    ggplot2::labs(x = "Event duration (days)", y = "Probability density", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )

  return (g)
}

GenerarPDFEventosLluvia <- function(xts.observacion, xts.realizaciones, station.id, tipo.evento.id, title, subtitle) {
  # Identificar eventos de lluvia
  eventos.observados <- IdentificarEventosLluvia(xts.observacion, tipo.evento.id)
  eventos.realizados <- list()
  for (j in seq(from=1, to=ncol(xts.realizaciones))) {
    eventos.realizados[[j]] <- IdentificarEventosLluvia(xts.realizaciones[,j], tipo.evento.id)
  }
  
  # Construir data frame consolidando datos
  muestras           <- GenerarMuestraEventos(eventos.observados)
  muestras[,"serie"] <- "obs"
  for (i in seq(from=1, to=length(eventos.realizados))) {
    muestras.realizacion           <- GenerarMuestraEventos(eventos.realizados[[i]])
    muestras.realizacion[,"serie"] <- i
    muestras                       <- rbind(muestras, muestras.realizacion)
  }
  
  # Create facet labels
  labels = c("Summer", 'Autumn', 'Winter', 'Spring')
  names(labels) <- c("1", "2", "3", "4")
  
  # Generar grafico
  g <- ggplot2::ggplot()
  for (r in unique(muestras$serie)) {
    df.sim.r <- muestras %>% dplyr::filter(serie == r)
    g <- g + ggplot2::geom_density(data = df.sim.r, mapping = aes(x = muestra, color = "sim"))
  }
  df.obs <- muestras %>% dplyr::filter(serie == "obs")
  g <- g +
    ggplot2::geom_density(data = df.obs, mapping = aes(x = muestra, color = "obs")) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed", "Simulated (range)"), values = c("obs" = "#d53e4f", "sim" = "#cdcdcd")) +
    ggplot2::facet_wrap(~ grupo, scales = "free", labeller = labeller(grupo = labels)) +
    ggplot2::labs(x = "Event duration (days)", y = "Probability density", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  return (g)
}

GenerarPDFPorPeriodo <- function(xts.observacion, xts.realizaciones, station.id, variable, periodo.mapping.func.name, valor.umbral = NULL, title, subtitle) {
  # Generar dataframes
  df.sim <- AsLongDataFrame(xts.obj = xts.realizaciones, periodo.mapping.func.name = periodo.mapping.func.name)
  df.obs <- AsLongDataFrame(xts.obj = xts.observacion, periodo.mapping.func.name = periodo.mapping.func.name)
  if (! is.null(valor.umbral)) {
    df.sim <- df.sim %>% dplyr::filter(valor > valor.umbral)
    df.obs <- df.obs %>% dplyr::filter(valor > valor.umbral)
  }
  
  # Generar PDFs
  g <- ggplot2::ggplot()
  for (r in unique(df.sim$serie)) {
    df.sim.r <- df.sim %>% dplyr::filter(serie == r)
    g <- g + ggplot2::geom_density(data = df.sim.r, mapping = aes(x = valor, color = "sim"))
  }
  
  if (periodo.mapping.func.name == 'quarter') {
    # Create facet labels
    labels = c("Summer", 'Autumn', 'Winter', 'Spring')
    names(labels) <- c("1", "2", "3", "4")
  } else {
    # Create facet labels
    labels = c("Jan", 'Feb', 'Mar', 'Apr', "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dec")
    names(labels) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
  }
  g <- g +
    ggplot2::geom_density(data = df.obs, mapping = aes(x = valor, color = "obs")) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed", "Simulated (range)"), values = c("obs" = "#d53e4f", "sim" = "#cdcdcd")) +
    ggplot2::scale_x_log10(breaks = c(1, 10, 100, 1000)) +
    ggplot2::facet_wrap(~ periodo, scales = "free", labeller = labeller(periodo = labels)) +
    ggplot2::labs(x = GetVariableName(variable, TRUE), y = "Probability density", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  return (g)
}

GenerarBoxplotRangoTemperatura <- function(xts.observacion, xts.realizaciones, station.id, title, subtitle) {
  # a. Generar data frame para datos observados
  df.obs <- AsLongDataFrame(xts.obj = xts.observacion$tmax) %>%
    dplyr::rename(tmax = valor) %>%
    dplyr::inner_join(cbind(AsLongDataFrame(xts.obj = xts.observacion$tmin) %>%
                              dplyr::rename(tmin = valor)
                            ), by = c("fecha", "serie")) %>%
    dplyr::inner_join(cbind(AsLongDataFrame(xts.obj = xts.observacion$prcp) %>%
                              dplyr::rename(prcp = valor)), by = c("fecha", "serie")) %>%
    dplyr::mutate(mes = lubridate::ymd(sprintf("%d-%d-%d", lubridate::year(Sys.Date()), month(fecha), 1)), rango = tmax - tmin, tipo = ifelse(IsWetDay(prcp), "Wet days", "Dry days")) %>%
    dplyr::select(mes, serie, rango, tipo)
  df.obs$serie <- "OBS"
  df.obs.all <- df.obs
  df.obs.all$tipo <- "All days"
  df.obs.grouped <- rbind(df.obs.all, df.obs) %>%
    dplyr::filter(! is.na(tipo)) %>%
    dplyr::group_by(mes, serie, tipo) %>%
    dplyr::summarise(rango.promedio = mean(rango, na.rm = TRUE))
  
  # b. Generar data frame para datos simulados
  df.sim <- AsLongDataFrame(xts.obj = xts.realizaciones$tmax) %>%
    dplyr::rename(tmax = valor) %>%
    dplyr::inner_join(cbind(AsLongDataFrame(xts.obj = xts.realizaciones$tmin) %>%
                              dplyr::rename(tmin = valor)
    ), by = c("fecha", "serie")) %>%
    dplyr::inner_join(cbind(AsLongDataFrame(xts.obj = xts.realizaciones$prcp) %>%
                              dplyr::rename(prcp = valor)), by = c("fecha", "serie")) %>%
    dplyr::mutate(mes = lubridate::ymd(sprintf("%d-%d-%d", lubridate::year(Sys.Date()), month(fecha), 1)), rango = tmax - tmin, tipo = ifelse(IsWetDay(prcp), "Wet days", "Dry days")) %>%
    dplyr::select(mes, serie, rango, tipo)
  df.sim.all <- df.sim
  df.sim.all$tipo <- "All days"
  df.sim.grouped <- rbind(df.sim.all, df.sim) %>%
    dplyr::filter(! is.na(tipo)) %>%
    dplyr::group_by(mes, serie, tipo) %>%
    dplyr::summarise(rango.promedio = mean(rango, na.rm = TRUE))
  
  # c. Generar grafico
  g <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = df.sim.grouped, mapping = aes(group = mes, x = mes, y = rango.promedio, colour = "sim")) +
    ggplot2::geom_point(data = df.obs.grouped, mapping = aes(x = mes, y = rango.promedio, colour = "obs"), size = 3, shape = 1) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed", "Simulated"), values = c("obs" = "#d53e4f", "sim" = "#3288bd")) +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ggplot2::facet_wrap(~ tipo, ncol = 1, scales = "free") +
    ggplot2::labs(x = "Month", y = "Mean daily temperature range (ºC)", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  return (g)
}

GenerarHistogramaDiasConHeladas <- function(xts.observacion, xts.realizaciones, station.id, title, subtitle) {
  df.obs <- AsLongDataFrame(xts.obj = xts.observacion) %>%
    dplyr::filter(! is.na(valor)) %>%
    dplyr::mutate(ano = year(fecha), helada = ifelse(valor <= 0, 1, 0)) %>%
    dplyr::group_by(serie, ano) %>%
    dplyr::summarise(total = sum(helada)) %>%
    dplyr::group_by(serie) %>%
    dplyr::summarise(promedio = mean(total))
  df.sim <- AsLongDataFrame(xts.obj = xts.realizaciones) %>%
    dplyr::filter(! is.na(valor)) %>%
    dplyr::mutate(ano = year(fecha), helada = ifelse(valor <= 0, 1, 0)) %>%
    dplyr::group_by(serie, ano) %>%
    dplyr::summarise(total = sum(helada)) %>%
    dplyr::group_by(serie) %>%
    dplyr::summarise(promedio = mean(total))
  
  g <- ggplot2::ggplot(mapping = aes(x = promedio)) +
    ggplot2::geom_histogram(data = df.sim, aes(fill = "sim"), bins = 10) +
    ggplot2::geom_vline(data = df.obs, aes(colour = "obs", xintercept = promedio)) +
    ggplot2::scale_fill_manual(name = "", labels = c("Simulated"), values = c("sim" = "#3288bd")) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed"), values = c("obs" = "#d53e4f")) +
    ggplot2::labs(x = "Mean number of freezing days", y = "Frequency", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return (g)
}

GenerarHistogramaDiasConHeladasPorTrimestre <- function(xts.observacion, xts.realizaciones, station.id, title, subtitle) {
  df.obs <- AsLongDataFrame(xts.obj = xts.observacion) %>%
    dplyr::filter(! is.na(valor)) %>%
    dplyr::mutate(ano = year(fecha), q = quarter(fecha, fiscal_start = 12), helada = ifelse(valor <= 0, 1, 0)) %>%
    dplyr::group_by(serie, ano, q) %>%
    dplyr::summarise(total = sum(helada)) %>%
    dplyr::group_by(serie, q) %>%
    dplyr::summarise(promedio = mean(total))
  df.sim <- AsLongDataFrame(xts.obj = xts.realizaciones) %>%
    dplyr::filter(! is.na(valor)) %>%
    dplyr::mutate(ano = year(fecha), q = quarter(fecha, fiscal_start = 12), helada = ifelse(valor <= 0, 1, 0)) %>%
    dplyr::group_by(serie, ano, q) %>%
    dplyr::summarise(total = sum(helada)) %>%
    dplyr::group_by(serie, q) %>%
    dplyr::summarise(promedio = mean(total))
  
  # Create facet labels
  labels = c("Summer", 'Autumn', 'Winter', 'Spring')
  names(labels) <- c("1", "2", "3", "4")
  
  g <- ggplot2::ggplot(mapping = aes(x = promedio)) +
    ggplot2::geom_histogram(data = df.sim, aes(fill = "sim"), bins = 10) +
    ggplot2::geom_vline(data = df.obs, aes(colour = "obs", xintercept = promedio)) +
    ggplot2::facet_wrap(~q, scales = "free_y", labeller = labeller(q = labels)) + 
    ggplot2::scale_fill_manual(name = "", labels = c("Simulated"), values = c("sim" = "#3288bd")) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed"), values = c("obs" = "#d53e4f")) +
    ggplot2::labs(x = "Mean number of freezing days", y = "Frequency", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return (g)
}

GenerarHistogramaPeriodosCalidosFrios <- function(xts.observacion, xts.realizaciones, station.id, latitud, variable, title, subtitle) {
  # a. Definir meses a analizar segun latitud y variable
  if (variable == "tmax") {
    # Hot spells: Q90
    cuantil.prob <- 0.9
    if (latitud < 0) {
      # Hemisferio sur: meses calidos
      meses <- c(10, 11, 12, 1, 2, 3)
    } else {
      # Hemisferio norte: meses calidos
      meses <- c(4, 5, 6, 7, 8, 9)
    }
  } else if (variable == "tmin") {
    # Cold spells: Q10
    cuantil.prob <- 0.1
    if (latitud < 0) {
      # Hemisferio sur: meses frios
      meses <- c(4, 5, 6, 7, 8, 9)
    } else {
      # Hemisferio norte: meses frios
      meses <- c(10, 11, 12, 1, 2, 3)
    }
  } else {
    stop(paste0("Variable invalida: ", variable))
  }
  
  # b. Definir data frames de datos simulados y observados
  df.obs <- AsLongDataFrame(xts.obj = xts.observacion,
                            periodo.mapping.func.name = "month") %>%
    dplyr::filter(periodo %in% meses) %>%
    dplyr::filter(! is.na(valor))
  
  # c. Buscar cuantil historico observado para meses definidos
  cuantil.valor <- as.double(stats::quantile(x = df.obs$valor, probs = cuantil.prob))
  
  # d. Identificar eventos y quedarme con aquellos que cumplan con un minimo de min.dias
  umbral.temperatura.minima <-NULL
  if (variable == "tmin") {
    umbral.temperatura.minima = cuantil.valor
  }
  umbral.temperatura.maxima <-NULL
  if (variable == "tmax") {
    umbral.temperatura.maxima = cuantil.valor
  }
  eventos.observados <- IdentificarEventosTemperatura(xts.observacion, meses, umbral.temperatura.minima, umbral.temperatura.maxima)
  eventos.simulados  <- NULL
  for (j in seq(from=1, to=ncol(xts.realizaciones))) {
    er <- IdentificarEventosTemperatura(xts.realizaciones[,j], meses, umbral.temperatura.minima, umbral.temperatura.maxima)
    eventos.simulados <- rbind(eventos.simulados, data.frame(serie = "SIM", total = nrow(er)))
  }
  
  # e. Graficar
  g <- ggplot2::ggplot(mapping = aes(x = total)) +
    ggplot2::geom_histogram(data = eventos.simulados, aes(fill = "sim"), bins = 10) +
    ggplot2::geom_vline(data = data.frame(col = "obs", total = nrow(eventos.observados)), 
                        aes(colour = col, xintercept = total)) +
    ggplot2::scale_fill_manual(name = "", labels = c("Simulated"), values = c("sim" = "#3288bd")) +
    ggplot2::scale_color_manual(name = "", labels = c("Observed"), values = c("obs" = "#d53e4f")) +
    ggplot2::labs(x = paste0("Number of ", ifelse(variable == "tmax", "hot", "cold"), " spells"), y = "Frequency", title = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 5. Efectuar validacion y almacenar resultados ----
# -----------------------------------------------------------------------------#

# Definicion del umbral de lluvia
umbral.lluvia = config$umbral.precipitacion

# Definir directorio de salida
output.dir    <- "output/"
realizaciones <- BuscarRealizaciones(simulated_climate = simulated_climate, stations)
simulaciones  <- realizaciones$simulaciones
periodo       <- realizaciones$periodo
estaciones    <- as.character(realizaciones$estaciones)
plots         <- list()
for (station.id in estaciones) {
  cat(paste0("Procesando estacion ", station.id, "\n"))
  station.data      <- stations %>% dplyr::filter(station_id == station.id)
  plots.estacion    <- list()
  xts.observacion   <- list()
  xts.realizaciones <- list()
  subtitle          <- paste0("Station ", station.id, 
                             ifelse(station.id %in% fit_stations$station_id,
                                    " (used in model fitting)",
                                    " (not used in model fitting)"))

  # Busqueda de datos por variables para la estacion  
  for (variable in config$variables) {
    xts.realizaciones[[variable]] <- simulaciones[[station.id]][[variable]]
    indices.realizaciones         <- index(xts.realizaciones[[variable]])
    xts.observacion[[variable]]   <- CompletarSerieXTS(BuscarSerieTemporalObservada(climate, station.id, variable, periodo$desde, periodo$hasta), indices.realizaciones[1], indices.realizaciones[length(indices.realizaciones)])
    indices.na                    <- which(is.na(xts.observacion[[variable]]))
    xts.realizaciones[[variable]][indices.na,] <- NA # Para evitar bias, pongo NA en las fechas donde no hay observacion
  }
    
  # Plots por variable
  for (variable in config$variables) {
    cat(paste0("... variable ", variable, "\n"))
    plots.variable <- list()
    
    # Para el caso de precipitacion, se ponen primero los graficos de ocurrencia y luego los de montos
    if (variable == "prcp") {
      # a. Probabilidad de dia lluvioso
      plots.variable[[length(plots.variable)+1]] <- GenerarBoxplotPorMes(xts.observacion[[variable]], 
                                                                         xts.realizaciones[[variable]], 
                                                                         station.id = station.id, 
                                                                         variable = variable, 
                                                                         group.by.func.name = ".GlobalEnv$WetDayProbability",
                                                                         title = paste0("Probability of a wet day by month"),
                                                                         y.title = "Probability of a wet day",
                                                                         subtitle = subtitle)

      # b. Dias lluviosos por trimestre/ano
      plots.variable[[length(plots.variable)+1]] <- GenerarBoxplotPorTrimestreAno(xts.observacion[[variable]], 
                                                                                  xts.realizaciones[[variable]], 
                                                                                  station.id = station.id, 
                                                                                  variable = variable, 
                                                                                  group.by.func.name = ".GlobalEnv$WetDays",
                                                                                  title = "Wet days by quarter/year",
                                                                                  subtitle = subtitle)
    
      # c. Rachas de dias lluviosos y secos
      plots.variable[[length(plots.variable)+1]] <- GenerarCDFEventosLluvia(xts.observacion[[variable]], 
                                                                            xts.realizaciones[[variable]], 
                                                                            station.id = station.id, 
                                                                            tipo.evento.id = 1, 
                                                                            title = "Wet days spells CDF",
                                                                            subtitle = subtitle)
      plots.variable[[length(plots.variable)+1]] <- GenerarCDFEventosLluvia(xts.observacion[[variable]], 
                                                                            xts.realizaciones[[variable]], 
                                                                            station.id = station.id, 
                                                                            tipo.evento.id = 0, 
                                                                            title = "Dry days spells CDF",
                                                                            subtitle = subtitle)
      # c. Rachas de dias lluviosos y secos
      plots.variable[[length(plots.variable)+1]] <- GenerarPDFEventosLluvia(xts.observacion[[variable]], 
                                                                            xts.realizaciones[[variable]], 
                                                                            station.id = station.id, 
                                                                            tipo.evento.id = 1, 
                                                                            title = "Wet days spells PDF",
                                                                            subtitle = subtitle)
      plots.variable[[length(plots.variable)+1]] <- GenerarPDFEventosLluvia(xts.observacion[[variable]], 
                                                                            xts.realizaciones[[variable]], 
                                                                            station.id = station.id, 
                                                                            tipo.evento.id = 0, 
                                                                            title = "Dry days spells PDF",
                                                                            subtitle = subtitle)
    
      # d. Boxplot de dias humedos por mes (sin anio y en 1 solo grafico)
      plots.variable[[length(plots.variable)+1]] <- GenerarBoxplotPorMes(xts.observacion[[variable]], 
                                                                         xts.realizaciones[[variable]], 
                                                                         station.id = station.id, 
                                                                         variable = variable, 
                                                                         group.by.func.name = ".GlobalEnv$WetDays",
                                                                         title = paste0("Total wet days by month"),
                                                                         subtitle = subtitle,
                                                                         y.title = "Total wet days in sequence")
    }
    
    # e. QQPLOT observado vs simulado
    plots.variable[[length(plots.variable)+1]] <- GenerarQQPlotPorPeriodo(xts.observacion[[variable]], 
                                                                          xts.realizaciones[[variable]], 
                                                                          station.id = station.id, 
                                                                          variable = variable, 
                                                                          percentile.threshold.extreme = 0.99,
                                                                          periodo.mapping.func.name = "quarter",
                                                                          title = paste0("Quantile-Quantile plot of daily ", tolower(GetVariableName(variable, FALSE)), " by quarter"),
                                                                          subtitle = subtitle)
    if (variable == "prcp") {
      plots.variable[[length(plots.variable)+1]] <- GenerarPDFPrecipitacionesDiasHumedos(xts.observacion[[variable]], 
                                                                                         xts.realizaciones[[variable]], 
                                                                                         title = paste0("PDF of daily rainfall in wet days by quarter"),
                                                                                         subtitle = subtitle)
    }
      
    # f. Histograma de media temporal
    if (variable != "prcp") {
      plots.variable[[length(plots.variable)+1]] <- GenerarHistogramaMediaTemporal(xts.observacion[[variable]], 
                                                                                   xts.realizaciones[[variable]], 
                                                                                   station.id = station.id, 
                                                                                   variable = variable,
                                                                                   title = paste0(GetVariableName(variable, FALSE), " temporal mean"),
                                                                                   subtitle = subtitle)
    }
  
    # g. CDF de realizaciones vs. CDF de observaciones
    if (variable != "prcp") {
      plots.variable[[length(plots.variable)+1]] <- GenerarCDFPorPeriodo(xts.observacion[[variable]], 
                                                                         xts.realizaciones[[variable]], 
                                                                         station.id = station.id, 
                                                                         variable = variable,
                                                                         periodo.mapping.func.name =  "quarter",
                                                                         title = paste0(GetVariableName(variable, FALSE), " CDF by quarter"),
                                                                         subtitle = subtitle)
    }
  
    # h. Boxplot de media por trimestre/ano (media para temperaturas y suma/dias lluviosos para precipitaciones)
    plots.variable[[length(plots.variable)+1]] <- GenerarBoxplotPorTrimestreAno(xts.observacion[[variable]], 
                                                                                xts.realizaciones[[variable]], 
                                                                                station.id = station.id, 
                                                                                variable = variable, 
                                                                                group.by.func.name = ifelse(variable == "prcp", "sum", "mean"),
                                                                                title = ifelse(variable == "prcp",
                                                                                               paste0("Accumulated rainfall by quarter/year"),
                                                                                               paste0("Mean ", tolower(GetVariableName(variable, FALSE)), " by quarter/year")),
                                                                                subtitle = subtitle)
                                                                                   
    
    
    # i. Autocorrelacion (Lag-1) por mes de valores simulados (boxplot) vs observados (point)
    if (variable != "prcp") {
      plots.variable[[length(plots.variable)+1]] <- GenerarBoxplotAutocorrelaciones(xts.observacion[[variable]], 
                                                                                    xts.realizaciones[[variable]], 
                                                                                    station.id = station.id, 
                                                                                    variable = variable,
                                                                                    title = paste0("Lag-1 autocorrelation of ", tolower(GetVariableName(variable, FALSE)), " by month"),
                                                                                    subtitle = subtitle)
    }
    
    # j. PDF de lluvia diaria por mes
    if (variable == "prcp") {
      plots.variable[[length(plots.variable)+1]] <- GenerarPDFPorPeriodo(xts.observacion[[variable]], 
                                                                         xts.realizaciones[[variable]], 
                                                                         station.id = station.id, 
                                                                         variable = variable, 
                                                                         periodo.mapping.func.name = "month", 
                                                                         valor.umbral = config$umbral.precipitacion,
                                                                         title = paste0(GetVariableName(variable, FALSE), " PDF by month"),
                                                                         subtitle = subtitle)
    }
    
    # k. Boxplot de SD/MAD de dias lluviosos por mes
    if (variable == "prcp") {
      plots.variable[[length(plots.variable)+1]] <- GenerarBoxplotPorMes(xts.observacion[[variable]], 
                                                                         xts.realizaciones[[variable]], 
                                                                         station.id = station.id, 
                                                                         variable = variable, 
                                                                         group.by.func.name = "sd",
                                                                         title = "Standard deviation of accumulated rainfall by month",
                                                                         subtitle = subtitle, 
                                                                         acumular.primero.por.mes = TRUE)
      plots.variable[[length(plots.variable)+1]] <- GenerarBoxplotPorMes(xts.observacion[[variable]], 
                                                                         xts.realizaciones[[variable]], 
                                                                         station.id = station.id, 
                                                                         variable = variable, 
                                                                         group.by.func.name = "mad",
                                                                         title = "Median absolute deviation of accumulated rainfall by month",
                                                                         subtitle = subtitle, 
                                                                         acumular.primero.por.mes = TRUE)
    }
    
    # l. Dias con heladas
    if (variable == "tmin") {
      plots.variable[[length(plots.variable)+1]] <- GenerarHistogramaDiasConHeladas(xts.observacion[[variable]],
                                                                                    xts.realizaciones[[variable]], 
                                                                                    station.id = station.id, 
                                                                                    title = "Mean number of freezing days per year",
                                                                                    subtitle = subtitle)
      plots.variable[[length(plots.variable)+1]] <- GenerarHistogramaDiasConHeladasPorTrimestre(xts.observacion[[variable]],
                                                                                                xts.realizaciones[[variable]], 
                                                                                                station.id = station.id, 
                                                                                                title = "Mean number of freezing days per year/quarter",
                                                                                                subtitle = subtitle)
    }
    
    # m. Diferencias entre media mensual de temperatura simulada y observada
    if (variable != "prcp") {
      plots.variable[[length(plots.variable)+1]] <- GenerarBoxplotDiferenciasSimuladas(xts.observacion = xts.observacion[[variable]],
                                                                                       xts.realizaciones = xts.realizaciones[[variable]],
                                                                                       station.id = station.id,
                                                                                       variable = variable,
                                                                                       title = paste0("Differences between simulated and historical monthly mean"), 
                                                                                       subtitle = subtitle)
    }
    
    # n. Hot/cold spells
    if (variable == "tmax") {
      plots.variable[[length(plots.variable)+1]] <- GenerarHistogramaPeriodosCalidosFrios(xts.observacion = xts.observacion[[variable]],
                                                                                          xts.realizaciones = xts.realizaciones[[variable]],
                                                                                          station.id = station.id,  
                                                                                          latitud = station.data$lat_dec,
                                                                                          variable = variable,
                                                                                          title = paste0("Histogram of hot spells (3+ days)"), 
                                                                                          subtitle = subtitle)
    } else if (variable == "tmin") {
      plots.variable[[length(plots.variable)+1]] <- GenerarHistogramaPeriodosCalidosFrios(xts.observacion = xts.observacion[[variable]],
                                                                                          xts.realizaciones = xts.realizaciones[[variable]],
                                                                                          station.id = station.id,  
                                                                                          latitud = station.data$lat_dec,
                                                                                          variable = variable,
                                                                                          title = paste0("Histogram of cold spells (3+ days)"), 
                                                                                          subtitle = subtitle)
    }
    
    plots.estacion[[variable]] <- plots.variable
  }
  
  # Plots cruzados de variables
  cat("... other plots\n")
  plots.variable <- list()
  
  # m. Correlacion cruzada diaria (Lag-0) entre temperatura minima y maxima
  plots.variable[[length(plots.variable)+1]] <- GenerarBoxplotCorrelacionCruzadaTemperatura(xts.observacion, 
                                                                                            xts.realizaciones, 
                                                                                            station.id = station.id,
                                                                                            title = "Lag-0 cross-correlation between tmin/tmax by month",
                                                                                            subtitle = subtitle)
  
  # n. Rango de temperatura diaria promedio
  plots.variable[[length(plots.variable)+1]] <- GenerarBoxplotRangoTemperatura(xts.observacion, 
                                                                               xts.realizaciones, 
                                                                               station.id = station.id,
                                                                               title = "Mean daily temperature range by month",
                                                                               subtitle = subtitle)
  
  
  plots.estacion$other <- plots.variable
  plots[[station.id]]      <- plots.estacion
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 6. Generar reporte ----
# -----------------------------------------------------------------------------#
variables <- c("Rainfall" = "prcp", "Minimum temperature" = "tmin", "Maximum temperature" = "tmax", "Other" = "other")
rmarkdown::render(
  input = "informe_validacion_series_sinteticas.rmd",
  output_file = config$output.file,
  output_dir = output.dir,
  params = list(
    fecha.generacion = format(Sys.Date(), format="%B %d, %Y")
  )
)
# ------------------------------------------------------------------------------