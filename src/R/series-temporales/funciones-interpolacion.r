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

# Definicion de Slow Discrete Fourier Transform (ver ?stats::fft)
SlowDiscreteFourierTransform <- function(z, inverse = FALSE) {
  n <- length(z)
  if(n == 0) return(z)
  k <- 0:(n-1)
  ff <- (if(inverse) 1 else -1) * 2*pi * 1i * k/n
  vapply(1:n, function(h) sum(z * exp(ff*(h-1)), na.rm = TRUE), complex(1))
}

# Funcion para decomponer una serie temporal en
# componentes: estacional (S), tendencia (T) y residuo (R)
# La serie debe tener la columna fecha (Data) y valor (double)
DescomponerSerieTemporal <- function(serie.temporal) {
  # Sealizar decomposicion
  objeto.stl <- stlplus::stlplus(x = dplyr::pull(serie.temporal, valor),
                                 t = dplyr::pull(serie.temporal, fecha),
                                 n.p = 365, s.window = 'periodic')
  
  # Devolver serie descompuesta
  serie.descompuesta <- dplyr::bind_cols(
    serie.temporal,
    dplyr::select(objeto.stl$data, seasonal, trend, remainder)
  )
  return (serie.descompuesta)
}

# Funcion para desestacionalizar una serie temporal
# Serie temporal es una data frame que al menos tiene
# las columnas fecha (Date) y valor (double)
DesestacionalizarSerieTemporal <- function(serie.temporal, anos) {
  # Definir filtro para capturar media y frecuencia fundamental
  N                <- length(unique(serie.temporal$fecha))
  tiempo           <- seq(from = 1, to = N)
  ciclos           <- 1
  filtro           <- rep(0, N)
  filtro[1]        <- 1
  filtro[anos+1]   <- 1
  filtro[N-anos+1] <- 1
  
  # Aplicar descomposicion
  valores <- serie.temporal %>%
    dplyr::arrange(fecha) %>%
    dplyr::pull(valor)
  
  fft.serie         <- SlowDiscreteFourierTransform(valores)
  fft.filtro        <- fft.serie * filtro
  valores.filtrados <- as.double(SlowDiscreteFourierTransform(fft.filtro, inverse = TRUE) / N)
  
  serie.descompuesta <- serie.temporal %>%
    dplyr::arrange(fecha) %>%
    dplyr::mutate(envolvente = valores.filtrados, ruido = valor - envolvente) 
  return (serie.descompuesta)
}

# Función para interpolar datos en una fecha determinada
# Series temporales es una data frame que al menos tiene
# las columnas omm_id (integer), fecha (Date), valor (double), 
# sesonal (double), trend (double) y remainder (double)
# Estaciones es un objeto sf de puntos que tiene al menos el campo 
# omm_id (ID, integer)
InterpolarValoresFecha <- function(series.temporales, estaciones, fecha) {
  # Obtener ruidos
  residuos.fecha <- estaciones %>%
    dplyr::inner_join(
      dplyr::filter(series.temporales, fecha == !! fecha),
      by = c("omm_id")
    )

  # Obtener variograma, ajustar modelo e interpolar
  locations  <- dplyr::filter(residuos.fecha, ! is.na(remainder))
  variograma <- gstat::variogram(remainder ~ 1, data = locations)
  modelo     <- automap::autofitVariogram(formula = remainder ~ 1, 
                                          input_data = sf::as_Spatial(locations))
  kriging    <- gstat::krige(remainder ~ 1, locations = locations, 
                             newdata = residuos.fecha, model = modelo$var_model) %>%
    dplyr::rename(prediccion = var1.pred, varianza = var1.var) %>%
    dplyr::mutate(omm_id = dplyr::pull(residuos.fecha, omm_id)) %>%
    dplyr::select(omm_id, prediccion, varianza)
  
  # Hacer join con datos originales y componer nuevamente el valor
  series.interpoladas <- series.temporales %>%
    dplyr::filter(fecha == !! fecha) %>%
    dplyr::mutate(omm_id = dplyr::pull(residuos.fecha, omm_id)) %>%
    dplyr::inner_join(kriging, by = c("omm_id")) %>%
    dplyr::mutate(valor_interpolado = seasonal + trend + 
                    dplyr::if_else(! is.na(remainder), remainder, prediccion))

  return (series.interpoladas)
}