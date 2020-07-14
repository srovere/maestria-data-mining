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

# Inicializar conexion con Copernicus Open Access Hub
tryCatch({
  # Primero se hace login en la ubicacion default (sino s2_download no funciona)
  sen2r::write_scihub_login(username = config$credenciales$user,
                            password = config$credenciales$pass)
  
  # Luego se hace lo mismo en un path especifico
  sen2r::write_scihub_login(username = config$credenciales$user,
                            password = config$credenciales$pass,
                            apihub_path = paste0(working.directory, "/apihub.txt"))
}, error = function(e) {
  DBI::dbDisconnect(con)
  script$stop(paste0("Error al realizar login: ", as.character(e)))  
})


# Los datos de ground truth están en EPSG:4326 o EPSG:22185 (POSGAR 94 / Argentina 5)
# Definir area de interes
area.of.interest <- geojsonsf::geojson_sf(paste0(working.directory, "/ground_truth/poligono_referencia.geojson")) %>%
  sf::st_set_crs(x = ., value = 22185)

# Cuerpos de agua estables
water.bodies <- geojsonsf::geojson_sf(paste0(working.directory, "/ground_truth/aguas_continentales_a.geojson")) %>%
  sf::st_transform(x = ., crs = 22185) %>%
  sf::st_intersection(area.of.interest)

# Ground truth
ground.truth <- geojsonsf::geojson_sf(paste0(working.directory, "/ground_truth/flood.geojson")) %>%
  sf::st_set_crs(x = ., value = 22185)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Abrir datos de Enero de 2019  ----
# -----------------------------------------------------------------------------#

# Carpeta de imagenes satelitales
images.directory <- paste0(working.directory, "/images")

# Pasar area de interes a Lat-Lon para enmascarar imagenes
# Enero de 2019
area.of.interest.latlon <- sf::st_transform(area.of.interest, crs = 4326)
s <- raster::stack(x = paste0(images.directory, "/merged/201811.tif")) %>%
  raster::crop(x = ., y = raster::extent(area.of.interest.latlon)) %>%
  raster::mask(x = ., mask = area.of.interest.latlon)
raster::writeRaster(x = s, filename = paste0(images.directory, "/masked/201811.tif"), format = "GTiff")
raster::projectRaster(from = s, filename = paste0(images.directory, "/masked/201811_epsg_22185.tif"), 
                      crs = raster::crs(area.of.interest))

  #raster::projectRaster(crs = sf::st_crs(area.of.interest)$proj4string)
  #raster::crop(x = ., y = raster::ex)
  


# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Descargar datos de sentinel-2  ----
# -----------------------------------------------------------------------------#

# # Pasar area de interes a UTM 21S
# area.of.interest.utm <- sf::st_transform(area.of.interest, crs = 32721)
# 
# # Obtener registros para descargar
# registros <- sen2r::s2_list(spatial_extent = sf::st_geometry(sf::st_transform(x = area.of.interest, crs = config$proj4string$latlon)), 
#                             time_interval = c(as.Date("2019-01-01"), as.Date("2019-01-31")),
#                             level = config$producto$nivel.aceptacion,
#                             max_cloud = 10,
#                             apihub = paste0(working.directory, "/apihub.txt"))
# 
# # Determinar datos faltantes
# output.directory <- paste0(working.directory, "/sentinel-2")
# existentes       <- list.files(output.directory)
# faltantes        <- registros[! (names(registros) %in% existentes)]
# 
# # Descargar datos faltantes
# sen2r::s2_download(s2_prodlist = faltantes, 
#                    apihub = paste0(working.directory, "/apihub.txt"),
#                    outdir = output.directory)
#   
# # Preparar datos para ensamblarlos como rasters
# translate.output <- purrr::map(
#   .x = list.files(path = output.directory, pattern = "^S2", full.names = TRUE),
#   .f = function(directorio) {
#     sen2r::s2_translate(infile = directorio, outdir = output.directory, format = "GTiff", 
#                         prod_type = c("BOA", "SCL"))
#   }
# ) %>% unlist()
# 
# # Hacer merge de los tiles si los hubiera
# merge.output <- sen2r::s2_merge(infiles = translate.output, outdir = output.directory, format = "GTiff")
#   
# # Enmascar para eliminar pixels fuera del campo
# clipped.output <- paste0(merge.output, "_masked.tif")
# sen2r::gdal_warp(srcfiles = merge.output, dstfiles = clipped.output, mask = area.of.interest.utm, overwrite = TRUE)
# file.rename(from = clipped.output, to = merge.output)
#   
#   # Enmascar para eliminar pixels nubosos
#   # masked.output <- sen2r::s2_mask(infiles = merge.output[1], maskfiles = merge.output[2], 
#   #                                 mask_type = config$mascara, outdir = config$dir$work, overwrite = TRUE)
#   
#   # Determinar la clasificación del suelo
#   soil.classification <- raster::raster(x = merge.output[2]) %>%
#     raster::crop(x = ., y = raster::extent(area.of.interest.utm)) %>%
#     raster::mask(x = ., mask = area.of.interest.utm)
#   
#   # Calcular NDWI
#   ndwi.output <- sen2r::s2_calcindices(infiles = merge.output, indices = "NDWI", overwrite = TRUE, 
#                                        format = "GTiff", outdir = working.directory, dataType = "Float32")
#   raster.ndwi <- raster::raster(ndwi.output)
#   
#   if (! is.null(ndvi.output)) {
#     # Transformar a raster y obtener la fecha
#     raster.campo <- raster::raster(ndvi.output)
#     fecha        <- as.Date(stringr::str_match(string = names(raster.campo), pattern = "^\\w+_(\\d{8})")[,2], format = "%Y%m%d")
#     
#     # Extraer datos dentro del campo y contar la cantidad de NAs
#     datos.campo   <- unlist(raster::extract(x = raster.campo, y = campo.utm))
#     porcentaje.na <- 100 * length(which(is.na(datos.campo))) / length(datos.campo)
#     
#     if (porcentaje.na < config$maximo.porcentaje.nubes$campo) {
#       # Guardar raster en base de datos
#       script$info(paste0("... Guardando datos para fecha ", fecha))
#       tryCatch({
#         ano.inicio.campana <- ifelse(lubridate::month(fecha) <= 4, lubridate::year(fecha) - 1, lubridate::year(fecha))
#         campana            <- paste0(ano.inicio.campana, "-", (ano.inicio.campana+1))
#         capa.datos.facade$insertarRaster(campo = campo, raster.capa = raster.campo, nombre = 'ndvi', 
#                                          cultivo = NA, campana = campana, variable = 'NDVI', fecha = fecha)
#       }, error = function(e) {
#         script$error(paste0("Error al insertar el raster de NDVI: ", as.character(e)))
#       })
#     } else {
#       script$info(paste0("... Descartando datos por cantidad de nubes ", directorio))
#     }
#     
#     rm(raster.campo, fecha, datos.campo, porcentaje.na)  
#   } else {
#     script$info(paste0("... Descartando datos por producir archivo de NDVI nulo ", directorio))
#   }    
#   
#   # Limiar variables
#   rm(translate.output, merge.output, clipped.output, masked.output, ndvi.output)
#   gc(full = TRUE)
# }
# # ------------------------------------------------------------------------------