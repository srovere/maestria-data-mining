# Borrar variables del ambiente
rm(list = objects())

# Carga de paquetes necesarios para hacer los gráficos
require(Cairo)
require(dplyr)
require(leaflet)
require(sf)
require(shiny)
require(shinycssloaders)
require(shinydashboard)
require(shinyjs)
require(sp)

# Uso de Cairo para renderizar los gráficos
options(bitmapType = "cairo")

# Cargar datos de barrios
barrios <- sf::st_read(dsn = "data", layer = "barrios_badata") %>%
  dplyr::mutate(barrio_id = dplyr::row_number(), area = AREA/1000000) %>%
  dplyr::rename(comuna = COMUNA, nombre = BARRIO) %>%
  dplyr::select(barrio_id, comuna, nombre, area)

# Cargar contorno de CABA, obtener geometria
contorno <- geojsonsf::geojson_sf("data/contorno_caba.geojson") %>%
  sf::st_transform(crs = sf::st_crs(barrios)) %>%
  sf::st_geometry()

# Cargar datos de establecimientos y determinar a que barrio pertenece cada uno de ellos
# Me quedo con las escuelas estatales de nivel primario
establecimientos <- sf::st_read(dsn = "data", layer = "establecimientos-educativos") %>%
  dplyr::mutate(barrio_id = as.integer(unlist(sf::st_intersects(., barrios)))) %>%
  dplyr::filter(SECTOR == 1 & stringr::str_detect(string = NIVMOD, pattern = "PriCom")) %>%
  dplyr::rename(establecimiento_id = OBJECTID)

# Determinar poligonos de influencia para los establecimientos
influencia.establecimientos <- sf::st_intersection(sf::st_cast(sf::st_voronoi(sf::st_union(establecimientos), contorno)), contorno) %>%
  sf::st_as_sf() %>%
  dplyr::rename(geometry = x) %>%
  dplyr::mutate(poligono_influencia_id = dplyr::row_number())

# Determinar pertenencia de establecimientos a poligonos
establecimientos <- establecimientos %>%
  dplyr::mutate(poligono_influencia_id = as.integer(sf::st_intersects(establecimientos, influencia.establecimientos)))

# Cargar datos censales, pasar a proyeccion planar y determinar a que barrio pertenece cada uno de ellos
# Dado que los radios censales estan incluidos dentro de un barrio, entonces para evitar problemas de proyeccion
# se determinarar el barrio en base a la ubicacion del centroide. Se utiliza st_nearest_feature porque para
# 4 casos en particular, el centroide cae fuera del poligono de barrios (por los "huecos" que tiene)
# Como el archivo de censos tiene mal 2 poligonos, los extraigo del geojson y luego hago join con los datos censales.
# Utilizando los centroides de los radios censales, determinar la pertenencia a los poligonos de influencia.
radios.censales <- geojsonsf::geojson_sf("data/caba_radios_censales.geojson") %>%
  sf::st_transform(crs = sf::st_crs(barrios)) %>%
  dplyr::select(-BARRIO)
datos.censales <- sf::st_read(dsn = "data", layer = "censo_2010_caba") %>%
  sf::st_transform(crs = sf::st_crs(barrios)) %>%
  dplyr::select(CO_FRAC_RA, T_VARON, T_MUJER, T_VIVIENDA, V_PARTICUL, V_COLECTIV) %>%
  dplyr::rename(RADIO_ID = CO_FRAC_RA) %>%
  sf::st_set_geometry(NULL)
censo <- dplyr::inner_join(radios.censales, datos.censales, by = c("RADIO_ID"))
centroides.radios.censales <- sf::st_centroid(censo) %>%
  dplyr::mutate(barrio_id = as.integer(sf::st_nearest_feature(., barrios)),
                poligono_influencia_id = as.integer(sf::st_intersects(., influencia.establecimientos))) %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::select(RADIO_ID, barrio_id, poligono_influencia_id) %>%
  dplyr::filter(! is.na(barrio_id) & ! is.null(poligono_influencia_id))
censo <- censo %>%
  dplyr::inner_join(centroides.radios.censales, by = "RADIO_ID")
rm(radios.censales, datos.censales, centroides.radios.censales)

# Definicion de proyeccion lat-long
proj4string.latlon <- sf::st_crs(sf::st_read(dsn = "data", layer = "censo_2010_caba"))