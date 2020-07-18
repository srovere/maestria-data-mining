# -----------------------------------------------------------------------------#
# --- PASO 1. Cargar paquetes necesarios ----
# -----------------------------------------------------------------------------#
rm(list = ls()); gc()
list.of.packages <- c("dplyr", "geojsonsf", "purrr", "raster", "sf", "sp")
for (pack in list.of.packages) {
  if (!require(pack, character.only = TRUE)) {
    stop("Paquete no instalado: ", pack)
  }
}
options(timeout = 300)
rm(list.of.packages, pack); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Inicializar script y determinar AOI ----
# -----------------------------------------------------------------------------#

# Definir directorios de trabajo
working.directory <- paste0(getwd(), "/tp-teledeteccion2020")
images.directory <- paste0(working.directory, "/images")
final.directory <- paste0(images.directory, "/indices")

# Los datos de ground truth están en EPSG:4326 o EPSG:22185 (POSGAR 94 / Argentina 5)
# Definir area de interes
area.of.interest <- geojsonsf::geojson_sf(paste0(working.directory, "/ground_truth/poligono_referencia.geojson")) %>%
  sf::st_set_crs(x = ., value = 22185)

# Pasar AOI a Lat/Lon
area.of.interest.latlon <- sf::st_transform(area.of.interest, crs = 4326)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Procesar imagenes ----
# -----------------------------------------------------------------------------#

# a) Buscar periodos
periodos <- list.files(path = images.directory, pattern = "\\d{6}")

# b) Crear carpeta de imagenes finales
if (! dir.exists(final.directory)) {
  dir.create(final.directory)
}

# c) Procesar periodo por periodo
for (period in periodos) {
  # i. Cargar imagenes
  period.directory <- paste0(images.directory, "/", period)
  imagenes         <- list.files(path = period.directory, pattern = "*.tif", full.names = TRUE)
  rasters.imagenes <- purrr::map(.x = imagenes, .f = raster::stack)
  
  # ii. Armar mosaico (merge)
  mosaico.periodo <- do.call(what = raster::mosaic, args = rasters.imagenes)
  
  # iii. Acotar mosaico a AOI
  mosaico.aoi <- mosaico.periodo %>%
    raster::crop(x = ., y = raster::extent(area.of.interest.latlon)) %>%
    raster::mask(x = ., mask = area.of.interest.latlon)
  
  # iv. Reproyectar en POSGAR 94 faja 5
  mosaic.reprojected <- raster::projectRaster(from = mosaico.aoi, crs = raster::crs(area.of.interest))
  
  # v. Calcular NDWI y NDVI
  blue  <- mosaic.reprojected[[1]]
  green <- mosaic.reprojected[[2]]
  red   <- mosaic.reprojected[[3]]
  nir   <- mosaic.reprojected[[4]]
  ndwi  <- (green - nir) / (green + nir)
  ndvi  <- (nir - red) / (nir + red)
  
  # vi. Agregar layers y guardar raster final
  raster.con.indices        <- raster::addLayer(mosaic.reprojected, ndwi, ndvi)
  names(raster.con.indices) <- c("b2", "b3", "b4", "b8", "ndwi", "ndvi")
  raster::writeRaster(x = raster.con.indices, format = "GTiff",
                      filename = paste0(final.directory, "/", period, ".tif"))
}
# ------------------------------------------------------------------------------