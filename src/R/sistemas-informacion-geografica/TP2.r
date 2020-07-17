# -----------------------------------------------------------------------------#
# --- PASO 1. Cargar paquetes necesarios ----
# -----------------------------------------------------------------------------#
rm(list = ls()); gc()
list.of.packages <- c("dplyr", "geojsonsf", "lubridate", "magrittr", "raster", 
                      "sen2r", "sf", "sp", "utils", "yaml")
for (pack in list.of.packages) {
  if (!require(pack, character.only = TRUE)) {
    stop("Paquete no instalado: ", pack)
  }
}
options(timeout = 300)
rm(list.of.packages, pack); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Leer archivo de configuracion ----
# -----------------------------------------------------------------------------#
args <- base::commandArgs(trailingOnly = TRUE)
archivo.config <- args[1]
if (is.na(archivo.config)) {
  # No vino el archivo de configuracion por linea de comandos. Utilizo un archivo default
  archivo.config <- paste0(getwd(), "/configuracion_ndvi.yml")
}

if (! file.exists(archivo.config)) {
  stop(paste0("El archivo de configuracion de ", archivo.config, " no existe\n"))
} else {
  cat(paste0("Leyendo archivo de configuracion ", archivo.config, "...\n"))
  config <- yaml.load_file(archivo.config)
}

rm(archivo.config, args); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Inicializar script y determinar AOI ----
# -----------------------------------------------------------------------------#

# Definir directorios de trabajo
working.directory <- paste0(getwd(), "/tp-teledeteccion2020")

# Los datos de ground truth están en EPSG:4326 o EPSG:22185 (POSGAR 94 / Argentina 5)
# Definir area de interes
area.of.interest <- geojsonsf::geojson_sf(paste0(working.directory, "/ground_truth/poligono_referencia.geojson")) %>%
  sf::st_set_crs(x = ., value = 22185)

# Cuerpos de agua estables
water.bodies <- geojsonsf::geojson_sf(paste0(working.directory, "/ground_truth/aguas_continentales_a.geojson")) %>%
  sf::st_set_crs(x = ., value = 4326) %>%
  sf::st_transform(x = ., 22185) %>%
  sf::st_intersection(area.of.interest)
sf::st_write(obj = water.bodies, dsn = paste0(working.directory, "/ground_truth"), 
             layer = "WaterBodies", driver = "ESRI shapefile", 
             update = TRUE, delete_layer = TRUE)

# Ground truth
ground.truth <- geojsonsf::geojson_sf(paste0(working.directory, "/ground_truth/flood.geojson")) %>%
  sf::st_set_crs(x = ., value = 22185)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Chuks de codigo para hacer distintas operaciones ----
# -----------------------------------------------------------------------------#

# Carpeta de imagenes satelitales
images.directory <- paste0(working.directory, "/images/masked")

# Pasar area de interes a Lat-Lon para enmascarar imagenes
# Enero de 2019
area.of.interest.latlon <- sf::st_transform(area.of.interest, crs = 4326)
s <- raster::stack(x = paste0(images.directory, "/merged/201901.tif")) %>%
  raster::crop(x = ., y = raster::extent(area.of.interest.latlon)) %>%
  raster::mask(x = ., mask = area.of.interest.latlon)
raster::writeRaster(x = s, filename = paste0(images.directory, "/masked/201901.tif"), format = "GTiff")
raster::projectRaster(from = s, filename = paste0(images.directory, "/masked/201901_epsg_22185.tif"), 
                      crs = raster::crs(area.of.interest))

# B2 (blue)
# B3 (green)
# B4 (red)
# B8 (nir)
datos.raster <- raster::stack(paste0(images.directory, "/201811_epsg_22185.tif"))
green        <- datos.raster[[2]]
red          <- datos.raster[[3]]
nir          <- datos.raster[[4]]

# Calculo de NDVI y NWDI
ndwi         <- (green - nir) / (green + nir)
ndvi         <- (nir - red) / (nir + red)

# Agregar layers
raster.con.indices        <- raster::addLayer(datos.raster, ndwi, ndvi)
names(raster.con.indices) <- c("b2", "b3", "b4", "b8", "ndwi", "ndvi")

# Guardar raster
raster::writeRaster(x = raster.con.indices, 
                    filename = paste0(working.directory, "/images/indices/201811.tif"), 
                    format = "GTiff")
# ------------------------------------------------------------------------------

water.bodies <- geojsonsf::geojson_sf(paste0(working.directory, "/ground_truth/aguas_continentales_a.geojson")) %>%
  sf::st_set_crs(x = ., value = 4326) %>%
  sf::st_transform(x = ., 22185) %>%
  sf::st_intersection(area.of.interest) %>%
  lwgeom::st_make_valid() %>%
  dplyr::mutate(Objeto = factor(Objeto),
                Tipo = as.integer(Objeto))
sf::st_write(obj = water.bodies, dsn = paste0(working.directory, "/ground_truth"), 
             layer = "WaterBodies", driver = "ESRI shapefile", 
             update = TRUE, delete_layer = TRUE)

water.bodies.raster <- raster::rasterize(x = sf::as_Spatial(water.bodies), y = imagen.entrenamiento,
                                         field = 'Tipo', fun = 'count', background = 4)
raster::writeRaster(x = water.bodies.raster, 
                    filename = paste0(working.directory, "/ground_truth/WaterBodies.tif"), 
                    format = "GTiff")
