# ---------------------------------------------------------------------------------------#
# ---- Script para integración de set de datos para TP1 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(dplyr)
require(magrittr)
require(mongolite)
require(purrr)
require(scatterpie)
require(sf)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- II. Lectura del set de datos desde mongo ----                            
# ---------------------------------------------------------------------------------------#

# i. Buscar productos
productos.conn     <- mongolite::mongo(url = "mongodb://localhost", db = "PreciosClaros", collection = "productos")
productos.original <- productos.conn$find()
productos.conn$disconnect()
rm(productos.conn)

# ii. Buscar sucursales
sucursales.conn     <- mongolite::mongo(url = "mongodb://localhost", db = "PreciosClaros", collection = "sucursales")
sucursales.original <- sucursales.conn$find()
sucursales.conn$disconnect()
rm(sucursales.conn)

# iii. Buscar precios
precios.conn     <- mongolite::mongo(url = "mongodb://localhost", db = "PreciosClaros", collection = "precios")
precios.original <- precios.conn$find()
precios.conn$disconnect()
rm(precios.conn)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Lectura de shapes de barrios de Capital Federal ----                            
# ---------------------------------------------------------------------------------------#

# URL: https://data.buenosaires.gob.ar/dataset/barrios
# Fuente: Barrios (SHP)
barrios <- sf::st_read(dsn = paste0(getwd(), "/input"), layer = "barrios_badata") %>%
  dplyr::arrange(BARRIO) %>%
  dplyr::mutate(barrioId = dplyr::row_number()) %>%
  dplyr::rename(nombre = BARRIO, comuna = COMUNA, area = AREA, perimetro = PERIMETRO) %>%
  dplyr::select(barrioId, comuna, nombre, area, perimetro)

# Comunas
comunas <- aggregate(x = dplyr::select(barrios, comuna), by = list(barrios$comuna), FUN = min) %>%
  dplyr::select(comuna)
centroides.comunas <- as.data.frame(sf::st_coordinates(sf::st_centroid(comunas)))
comunas %<>% 
  dplyr::bind_cols(centroides.comunas) %>%
  dplyr::rename(centro_x = X, centro_y = Y)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- IV. Transformación de datos ----                            
# ---------------------------------------------------------------------------------------#

# i. Productos
productos <- productos.original %>%
  dplyr::as.tbl() %>%
  dplyr::rename(productoId = id) %>%
  dplyr::mutate(marca = as.factor(marca)) %>%
  dplyr::select(productoId, nombre, marca, presentacion)

# ii. Comercios y banderas
comercios <- sucursales.original %>%
  dplyr::as.tbl() %>%
  dplyr::distinct(comercioId, comercioRazonSocial) %>%
  dplyr::rename(razonSocial = comercioRazonSocial) %>%
  dplyr::arrange(comercioId)
banderas <- sucursales.original %>%
  dplyr::as.tbl() %>%
  dplyr::distinct(comercioId, banderaId, banderaDescripcion) %>%
  dplyr::rename(descripcion = banderaDescripcion) %>%
  dplyr::arrange(comercioId, banderaId)

# iii. Sucursales (normalizacion y geolocalizacion)
#      Las coordenadas estan en Lat-Lon. Hay que convertirlas a planares para que tengan el mismo CRS.
sucursales <- sucursales.original %>%
  dplyr::as.tbl() %>%
  dplyr::rename(tipo = sucursalTipo, nombre = sucursalNombre,
                latitud = lat, longitud = lng) %>%
  dplyr::select(comercioId, banderaId, sucursalId, tipo, direccion, latitud, longitud) %>%
  sf::st_as_sf(coords = c('longitud', 'latitud'), crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') %>%
  sf::st_transform(crs = sf::st_crs(barrios))
sucursales %<>% dplyr::mutate(barrioId = as.integer(sf::st_within(x = sucursales, y = barrios)))

# Las sucursales que caen fuera del borde de CABA las asociamos con el barrio mas cercano
sucursales.borde <- sucursales %>%
  dplyr::filter(is.na(barrioId)) %>%
  dplyr::mutate(barrioCercanoId = sf::st_nearest_feature(x = ., y = barrios)) %>%
  dplyr::select(sucursalId, comercioId, banderaId, barrioCercanoId) %>%
  sf::st_set_geometry(NULL)
sucursales %<>% 
  dplyr::left_join(sucursales.borde) %>%
  dplyr::mutate(barrioId = dplyr::if_else(! is.na(barrioId), barrioId, barrioCercanoId)) %>%
  dplyr::select(-barrioCercanoId)

# v. Pasar todas las coordenadas a Lat-Lon
barrios    %<>% sf::st_transform(crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
sucursales %<>% sf::st_transform(crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# vi. Precios
precios <- precios.original %>%
  dplyr::as.tbl() %>%
  dplyr::mutate(comercioId = as.integer(stringr::str_match(string = sucursal, pattern = "(\\d+)-(\\d+)-(\\d+)")[, 2]),
                banderaId = as.integer(stringr::str_match(string = sucursal, pattern = "(\\d+)-(\\d+)-(\\d+)")[, 3]),
                sucursalId = stringr::str_match(string = sucursal, pattern = "(\\d+)-(\\d+)-(\\d+)")[, 4]) %>%
  dplyr::rename(productoId = producto) %>%
  dplyr::select(productoId, comercioId, banderaId, sucursalId, fecha, medicion, precio)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- V. Generacion de graficos ----                            
# ---------------------------------------------------------------------------------------#

# I. Sucursales por barrio
sucursales.por.barrio <- sucursales %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::group_by(barrioId) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::inner_join(barrios, by = c("barrioId"))

grafico.sucursales.barrio <- ggplot2::ggplot(data = sucursales.por.barrio) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = cantidad)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Cantidad de sucursales por barrio") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# II. Sucursales por comuna
sucursales.por.comuna <- sf::st_set_geometry(sucursales, NULL) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(comuna) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::inner_join(comunas, by = c("comuna"))

grafico.sucursales.comuna <- ggplot2::ggplot(data = sucursales.por.comuna) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = cantidad)) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = centro_x, y = centro_y, label = comuna)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Cantidad de sucursales por comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# III. Sucursales por comuna y tipo de sucursal
sucursales.por.tipo.comuna <- sf::st_set_geometry(sucursales, NULL) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(comuna, tipo) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  tidyr::spread(key = tipo, value = cantidad) %>%
  dplyr::inner_join(comunas, by = c("comuna"))

grafico.sucursales.tipo.comuna <-ggplot2::ggplot() +
  ggplot2::geom_sf(data = sucursales.por.tipo.comuna, mapping = ggplot2::aes()) +
  scatterpie::geom_scatterpie(mapping = ggplot2::aes(x = centro_x, y = centro_y, group = comuna, r = 0.01),
                              data = sucursales.por.tipo.comuna, cols = unique(sucursales$tipo)) +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Proporción de sucursales por tipo y comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# IV. Cantidad de precios por barrio
precios.por.barrio <- precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(barrioId) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::right_join(barrios, by = c("barrioId"))

grafico.sucursales.barrio <- ggplot2::ggplot(data = precios.por.barrio) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = cantidad)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Cantidad de precios relevados por barrio") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# V. Cantidad de precios por comuna
precios.por.comuna <- precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(comuna) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::inner_join(comunas, by = c("comuna"))

grafico.sucursales.comuna <- ggplot2::ggplot(data = precios.por.comuna) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = cantidad)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Cantidad de precios relevados por comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# V. Cantidad de precios/sucursales por comuna
ratio.precios.sucursales.por.comuna <- precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(comuna) %>%
  dplyr::summarise(cantidad_precios = dplyr::n()) %>%
  dplyr::inner_join(dplyr::select(sucursales.por.comuna, comuna, cantidad)) %>%
  dplyr::rename(cantidad_sucursales = cantidad) %>%
  dplyr::mutate(ratio_precios_sucursales = cantidad_precios / cantidad_sucursales) %>%
  dplyr::inner_join(comunas, by = c("comuna"))

grafico.precios.sucursales.comuna <- ggplot2::ggplot(data = ratio.precios.sucursales.por.comuna) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = ratio_precios_sucursales)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Proporción de precios/sucursales relevados por comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- VI. Almacenamiento de resultados ----                            
# ---------------------------------------------------------------------------------------#
save(barrios, productos, sucursales, precios, file = paste0(getwd(), "/input/PreciosClaros.RData"))
# ----------------------------------------------------------------------------------------