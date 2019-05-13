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
  sf::st_transform(crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') %>%
  dplyr::select(comuna)
centroides.comunas <- as.data.frame(sf::st_coordinates(sf::st_centroid(comunas)))
comunas %<>% 
  dplyr::bind_cols(centroides.comunas) %>%
  dplyr::rename(centro_x = X, centro_y = Y) %>%
  sf::st_transform(crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# Graficos
grafico.barrios <- ggplot2::ggplot(data = barrios) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = barrioId)) +
  ggplot2::scale_fill_distiller(type = "div", palette = "Spectral") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Barrios de C.A.B.A") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'none',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

grafico.comunas <- ggplot2::ggplot(data = comunas) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = comuna)) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = centro_x, y = centro_y, label = comuna)) +
  ggplot2::scale_fill_distiller(type = "div", palette = "Spectral") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Comunas de C.A.B.A") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'none',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )
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
  dplyr::filter(id %in% precios.original$sucursal) %>%
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
# ---- V. Eliminación de outliers de precios y almacenamiento de resultados ----                            
# ---------------------------------------------------------------------------------------#

# i. Calculamos estadisticas de precios por producto y medicion
estadisticas.precios <- precios %>%
  dplyr::group_by(productoId, medicion) %>%
  dplyr::summarize(media = mean(precio, na.rm = TRUE),
                   std = sd(precio, na.rm = TRUE),
                   maximo = max(precio, na.rm = TRUE),
                   minimo = min(precio, na.rm = TRUE),
                   cantidad = dplyr::n())

# ii. Calculamos scores de precios por producto y medición para llevarlos a unidades comunes
#     que nos permitan detectar valores atípicos
precios.scores <- precios %>%
  dplyr::inner_join(estadisticas.precios, by = c("productoId", "medicion")) %>%
  dplyr::mutate(score = dplyr::if_else(std > 0, (precio - media)/std, 0))

# iii. Buscamos outliers con rango = 3.
rango.outliers   <- 3
scores           <- dplyr::pull(precios.scores, score)
score.maximo     <- quantile(scores, 0.75, na.rm = TRUE) + rango.outliers * IQR(scores, na.rm = TRUE)
score.minimo     <- quantile(scores, 0.25, na.rm = TRUE) - rango.outliers * IQR(scores, na.rm = TRUE)

# iv. Eliminamos esos outliers. Nos quedan datos en abundancia para seguir haciendo los análisis.
#     Por tal motivo, no es necesario realizar imputaciones de datos faltantes.
precios.filtrados <- precios.scores %>%
  dplyr::filter((score >= score.minimo) & (score <= score.maximo)) %>%
  dplyr::select(productoId, comercioId, banderaId, sucursalId, fecha, medicion, precio)

# v. Finalmente, reconstruimos la tabla de precios y agregamos scores por producto y por producto/medición
#    Los scores se recalculan nuevamente sin los outliers iniciales.
estadisticas.precios.productos <- precios %>%
  dplyr::group_by(productoId) %>%
  dplyr::summarize(media_producto = mean(precio, na.rm = TRUE),
                   std_producto = sd(precio, na.rm = TRUE))
estadisticas.precios.productos.medicion <- precios %>%
  dplyr::group_by(productoId, medicion) %>%
  dplyr::summarize(media_producto_medicion = mean(precio, na.rm = TRUE),
                   std_producto_medicion = sd(precio, na.rm = TRUE))

precios <- precios.filtrados %>%
  dplyr::inner_join(estadisticas.precios.productos, by = c("productoId")) %>%
  dplyr::inner_join(estadisticas.precios.productos.medicion, by = c("productoId", "medicion")) %>%
  dplyr::mutate(score_producto = dplyr::if_else(std_producto > 0, (precio - media_producto)/std_producto, 0),
                score_producto_medicion = dplyr::if_else(std_producto_medicion > 0, 
                                                         (precio - media_producto_medicion)/std_producto_medicion, 0)) %>%
  dplyr::select(productoId, comercioId, banderaId, sucursalId, fecha, medicion, precio, score_producto, score_producto_medicion)

# vi. Gráficos
grafico.outliers.inicial <- ggplot2::ggplot(data = precios.scores) +
  ggplot2::geom_boxplot(mapping = ggplot2::aes(x = "", y = score), na.rm=TRUE) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "", y = "", title = "Scores por producto y medición",
                subtitle = "Boxplot de acuerdo a datos originales") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'none',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
  )
grafico.outliers.final <- ggplot2::ggplot(data = precios) +
  ggplot2::geom_boxplot(mapping = ggplot2::aes(x = "", y = score_producto_medicion), na.rm=TRUE) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "", y = "", title = "",
                subtitle = "Boxplot de acuerdo a datos filtrados") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'none',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
  )  

# vii. Almacenamiento de variables de interés para realizar el análisis exploratorio y el informe
save(comercios, banderas, barrios, comunas, productos, sucursales, precios, 
     file = paste0(getwd(), "/input/PreciosClaros.RData"))
save(grafico.barrios, grafico.comunas, grafico.outliers.inicial, grafico.outliers.final,
     file = paste0(getwd(), "/output/GraficosPreparacion.RData"))
# ----------------------------------------------------------------------------------------