# Carga de paquetes necesarios para hacer los requests a la API y graficar resultados
require(Cairo)
require(dplyr)
require(geojsonsf)
require(ggplot2)
require(glue)
require(httr)
require(jsonlite)
require(knitr)
require(lubridate)
require(ncdf4)
require(raster)
require(tidyr)
require(sf)

# Uso de paquete Cairo para generar gráficos
options(bitmapType = "cairo")

# Función para acceder a un servicio web definido por una URL utilizando el método GET.
# Devuelve la respuesta como texto plano.
ConsumirServicioGET <- function(url, usuario, clave) {
  req  <- httr::GET(url = url, 
                    config = httr::authenticate(user = usuario, 
                                                password = clave))
  return (httr::content(req, as = "text"))
}

# Función para acceder a un servicio web definido por una URL utilizando el método POST.
# Devuelve la respuesta como raw.
ConsumirServicioPOST <- function(url, usuario, clave, body) {
  req  <- httr::POST(url = url, 
                     config = httr::authenticate(user = usuario, 
                                                 password = clave),
                     body = body, encode = "json")
  return (httr::content(req, as = "raw"))
}

# Función para acceder a un servicio web definido por una URL utilizando un usuario y clave.
# Asumiendo que la respuesta es un string JSON, hace la conversión del mismo a Data Frame.
ConsumirServicioJSON <- function(url, usuario, clave) {
  respuesta <- ConsumirServicioGET(url, usuario, clave)
  return (jsonlite::fromJSON(respuesta))
}

# Función para acceder a un servicio web definido por una URL utilizando un usuario y clave.
# Se envía un archivo GeoJSON para realizar la consulta en un área determinada.
# La respuesta se devuelve con un objeto de tipo raster.
ConsumirServicioEspacial <- function(url, usuario, clave, archivo.geojson.zona) {
  # a. Obtener datos y guardarlos en un archivo temporal
  zona.geojson     <- readr::read_file(file = archivo.geojson.zona)
  respuesta        <- ConsumirServicioPOST(url = url, usuario = usuario, clave = clave,
                                           body = list(zona.geojson = zona.geojson))
  archivo.temporal <- base::tempfile("raster_ws_api")
  un.archivo       <- base::file(archivo.temporal, "wb")
  base::writeBin(respuesta, un.archivo)
  close(un.archivo)
  
  # b. Obtener CRS y fechas del NetCDF
  archivo.nc <- ncdf4::nc_open(filename = archivo.temporal)
  prj4string <- ncdf4::ncatt_get(archivo.nc, 0, "crs")$value
  fechas     <- NULL
  tryCatch({
    fechas <- as.Date(ncdf4::ncvar_get(archivo.nc, "time"), origin = as.Date("1970-01-01"))  
  }, error = function(e) {
    # No hay variable de tiempo porque es un solo layer. Poner como fecha el atributo start_date
    fechas <<- as.Date(ncdf4::ncatt_get(archivo.nc, 0, "start_date")$value)
  }, finally = { 
    ncdf4::nc_close(archivo.nc)
  })
  
  # c. Convertir a raster y borrar archivo temporal
  un.raster <- raster::stack(x = archivo.temporal) %>%
    raster::readAll()
  raster::crs(un.raster) <- prj4string
  if (raster::nlayers(un.raster) > 1) {
    names(un.raster) <- as.character(fechas)
  }
  un.raster <- raster::setZ(un.raster, fechas)
  file.remove(archivo.temporal)  
  return (un.raster)
}

# Convierte una fecha a formato IS0 8601 (YYYY-MM-DDTHH:mm:ss) utilizando el huso horario GMT-0.
# Este es formato un estándar para representar fechas como una cadena de caracteres [7].
ConvertirFechaISO8601 <- function(fecha) {
  return (strftime(fecha, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
}

base.url        <- 'https://api.crc-sas.org/ws-api'
usuario.default <- 'clima'
clave.default   <- 'Dcft^&*('

############### Ejemplos ##################

# Estaciones del SMN
url.estaciones.smn <- glue::glue("{base.url}/estaciones/AR/SMN")
estaciones.smn     <- ConsumirServicioJSON(url.estaciones.smn, usuario = usuario.default, clave = clave.default) %>%
  sf::st_as_sf(x = ., coords = c("longitud", "latitud"), crs = 4326)

# Datos de Pehuajó
fecha.desde           <- ConvertirFechaISO8601(as.Date("1961-01-01", tz = UTC))
fecha.hasta           <- ConvertirFechaISO8601(as.Date("2019-12-31", tz = UTC))
url.registros.diarios <- glue::glue("{base.url}/registros_diarios/87544/{fecha.desde}/{fecha.hasta}")
registros.largo       <- ConsumirServicioJSON(url = url.registros.diarios,
                                              usuario = usuario.default, clave = clave.default) %>%
  dplyr::mutate(fecha = as.Date(fecha))
registros.ancho       <- tidyr::spread(registros.largo, key = variable_id, value = valor)

# Temperatura maxima
registros.test <- registros.ancho %>%
  dplyr::filter(lubridate::year(fecha) == 2013)
ggplot2::ggplot(data = registros.test) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = fecha, y = tmax)) +
  ggplot2::labs(x = "Fecha", y = "Temperatura máxima (ºC)", title = "Temperatura máxima diaria", subtitle = "Pehuajó (87544), 2013") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )

# FFT
N        <- nrow(registros.test)
tiempo   <- seq_len(N)
fft.tmax <- fft(registros.test$tmax)
plot(Mod(fft.tmax), type = 'l')
plot(Mod(fft.tmax)[1:32], type = 'b')
plot(Mod(fft.tmax)[(N-30):N], type = 'b')
plot(Mod(fft.tmax)[-1], type = 'b')

# Le sacamos el nivel medio y la componente estacional
fft.filtro      <- rep(1, length(fft.tmax))
fft.filtro[1:2] <- 0
fft.filtro[N]   <- 0
fft.final       <- fft.tmax * fft.filtro

# Transformada inversa: queda ruido gaussiano
ruido <- Re(fft(fft.final, inverse = TRUE)/length(fft.tmax))
plot(tiempo, tmax.filtrada, type = 'b')
hist(ruido)
shapiro.test(ruido)

# Busqueda de outliers
boxplot(ruido)
boxplot(ruido, range = 3)

meses        <- lubridate::month(registros.test$fecha)
ruidos.meses <- data.frame(mes = meses, ruido = ruido)

boxplot(ruido ~ mes, data = ruidos.meses)


# Lectura de datos originales
datos.originales <- readr::read_delim(file = "data/87544_original_2014.csv", delim = "\t", na = c("", "\\N"), col_types = "iDdddddddddddddd")
colnames(datos.originales) <- tolower(colnames(datos.originales))
datos.originales.test <- datos.originales %>%
  dplyr::filter(lubridate::year(fecha) == 2013)

# Grafico de datos originales
ggplot2::ggplot(data = datos.originales.test) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = fecha, y = tmax)) +
  ggplot2::labs(x = "Fecha", y = "Temperatura máxima (ºC)", title = "Temperatura máxima diaria", subtitle = "Pehuajó (87544), 2013") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
