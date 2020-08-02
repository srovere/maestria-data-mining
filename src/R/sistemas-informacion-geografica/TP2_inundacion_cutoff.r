# -----------------------------------------------------------------------------#
# --- PASO 1. Inicializar script ----
# -----------------------------------------------------------------------------#
# a) Borrar entorno
rm(list = ls()); gc()

# b) Cargar paquetes
list.of.packages <- c("dplyr", "geojsonsf", "lubridate", "magrittr", "raster", 
                      "rasterVis", "randomForest", "sf", "sp", "utils", 
                      "xgboost", "yaml")
for (pack in list.of.packages) {
  if (!require(pack, character.only = TRUE)) {
    stop("Paquete no instalado: ", pack)
  }
}
rm(list.of.packages, pack); gc()

# c) Definir carpetas
working.directory <- paste0(getwd(), "/tp-teledeteccion2020")
images.directory  <- paste0(working.directory, "/images/final")

# d) Cargar cuerpos de agua
water.bodies <- raster::raster(paste0(images.directory, "/GT-WaterBodies-Complete.tif"))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Probar XGB con umbral seleccionado ----
# -----------------------------------------------------------------------------#

# a) Buscar archivos y determinar modelos.
archivos <- list.files(path = images.directory, pattern = "prob_201901_*") %>%
  stringr::str_match(string = ., pattern = "prob_201901_(.+?)\\.tif") %>%
  as.data.frame()
colnames(archivos) <- c("archivo", "modelo")

# b) Calcular areas inundadas para cada umbral
inundaciones <- NULL
for (i in seq_len(nrow(archivos))) {
  # i. Cargar imagen de probabilidades de 201901, eliminar areas inundadas
  archivo               <- paste0(images.directory, "/", as.character(archivos[i, "archivo"]))
  modelo                <- as.character(archivos[i, "modelo"])
  imagen.probabilidades <- raster::raster(x = archivo) %>%
    raster::mask(x = .,  mask = water.bodies, maskvalue = 1, updatevalue = 0)
  
  # ii. Seleccionar pixels inudados de acuerdo a umbral.
  for (umbral in c(0.5, 0.62)) {
    imagen.umbral <- raster::calc(x = imagen.probabilidades, fun = function(x) {
      return (ifelse(x >= umbral, 1, 0))
    })
  
    # iii. Calcular hectareas inundadas
    valores.positivos   <- raster::freq(imagen.umbral, value = 1)
    hectareas.inundadas <- valores.positivos * raster::xres(imagen.umbral) * raster::yres(imagen.umbral) / 10000
    inundaciones        <- rbind(inundaciones, data.frame(modelo = modelo, umbral = umbral, hectareas = hectareas.inundadas))
  }
}

# c) Guardar datos
readr::write_csv(x = inundaciones, path = "data/areas_inundadas.csv")
# ------------------------------------------------------------------------------