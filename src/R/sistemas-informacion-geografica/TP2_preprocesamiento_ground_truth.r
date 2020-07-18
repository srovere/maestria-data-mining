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
final.directory <- paste0(images.directory, "/final")

# Los datos de ground truth están en EPSG:4326 o EPSG:22185 (POSGAR 94 / Argentina 5)
# Definir area de interes
area.of.interest <- geojsonsf::geojson_sf(paste0(working.directory, "/ground_truth/poligono_referencia.geojson")) %>%
  sf::st_set_crs(x = ., value = 22185)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Rasterizar cuerpos de agua ----
# -----------------------------------------------------------------------------#

# a) Obtener imagen raster para capturar grilla
imagen.ejemplo <- raster::stack(paste0(final.directory, "/201801.tif"))

# b) Cargar, reproyectar, intersectar y corregir
water.bodies <- geojsonsf::geojson_sf(paste0(working.directory, "/ground_truth/aguas_continentales_a.geojson")) %>%
  sf::st_set_crs(x = ., value = 4326) %>%
  sf::st_transform(x = ., 22185) %>%
  sf::st_intersection(area.of.interest) %>%
  lwgeom::st_make_valid() %>%
  dplyr::mutate(Tipo = factor("Agua"))

# c) Transformar a raster
water.bodies.raster <- raster::rasterize(x = sf::as_Spatial(water.bodies), y = imagen.ejemplo,
                                         field = 'Tipo', fun = 'last', background = 0)

# d) Guardar
raster::writeRaster(x = water.bodies.raster, format = "GTiff", overwrite = TRUE,
                    filename = paste0(final.directory, "/GT-WaterBodies.tif"))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Rasterizar verdad de campo ----
# -----------------------------------------------------------------------------#

# a) Cargar, reproyectar, intersectar y corregir
cuerpos.agua <- geojsonsf::geojson_sf(paste0(working.directory, "/ground_truth/flood.geojson")) %>%
  sf::st_set_crs(x = ., value = 22185) %>%
  sf::st_intersection(area.of.interest) %>%
  lwgeom::st_make_valid() %>%
  dplyr::mutate(Tipo = factor("Agua"))

# b) Transformar a raster (clase positiva)
ground.truth.positiva <- raster::rasterize(x = sf::as_Spatial(cuerpos.agua), y = imagen.ejemplo,
                                         field = 'Tipo', fun = 'last', background = NA,
                                         paste0(final.directory, "/GT-Positive.tif"))

# c) Transformar imagen positiva en negativa
ground.truth.negativa <- raster::calc(x = ground.truth.positiva, fun = function(x) {
  return (ifelse(! is.na(x), 0, as.integer(NA)))
}, filename = paste0(final.directory, "/GT-Negative.tif"))
# ------------------------------------------------------------------------------