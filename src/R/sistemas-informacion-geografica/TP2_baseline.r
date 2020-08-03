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

# d) Cargar datos de muestras
load("data/datos_muestras.RData")


# e) Definir funcion para cargar imagen
CargarImagen   <- function(filename) {
  imagen        <- raster::stack(x = filename)
  names(imagen) <- c("b2", "b3", "b4", "b8", "ndwi", "ndvi")
  return (imagen)
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Evaluar metricas para cada umbral de NDWI ----
# -----------------------------------------------------------------------------#

# a) funcion para Obtener metricas en distintos puntos de corte
MetricasCutoff <- function(algoritmo, fold, obs, probs, cutoff = seq(from = -0.99, to = 0.99, by = 0.01)) {
  metricas <- NULL
  for (corte in cutoff) {
    conf.mat    <- caret::confusionMatrix(data = factor(ifelse(probs >= corte, 1, 0)),
                                          reference = as.factor(obs),
                                          positive = "1")
    metricas    <- rbind(metricas, 
                         data.frame(algoritmo = algoritmo, fold = fold, cutoff = corte, accuracy = conf.mat$overall['Accuracy'],
                                    f1 = conf.mat$byClass['F1'], kappa = conf.mat$overall['Kappa'], precision = conf.mat$byClass['Precision'],
                                    recall = conf.mat$byClass['Recall'], specificity = conf.mat$byClass['Specificity'])
                         
    )
  }
  return (metricas)
}

# b) Buscar metricas para cada umbral
metricas <- MetricasCutoff(algoritmo = "McFeeters", fold = 1, obs = datos.muestras$clase,
                           probs = datos.muestras$ndwi)
readr::write_csv(x = metricas, path = "data/metricas_baseline.csv")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Para la metrica con mejor F1, evaluar inundacion ----
# -----------------------------------------------------------------------------#

# a) Elegir el mejor umbral para la metrica F1
umbral <- dplyr::arrange(metricas, dplyr::desc(f1))[1, "cutoff"]

# b) Generar prediccion
imagen.ndwi         <- CargarImagen(paste0(images.directory, "/201901.tif"))[["ndwi"]]
prediccion.baseline <- raster::calc(
  x = imagen.ndwi, 
  fun = function(x) {
    return (ifelse(x >= umbral, 1, 0))
  }
)

# c) Calcular inundacion simple (201901 menos cuerpos de agua estables)
water.bodies          <- raster::raster(paste0(images.directory, "/GT-WaterBodies-Complete.tif"))
prediccion.inundacion <- raster::mask(
  x = prediccion.baseline,
  mask = water.bodies,
  maskvalue = 1, updatevalue = 0,
  filename = paste0(images.directory, "/predict_inundacion_baseline.tif")
)

# d) Calcular areas inundadas
valores.positivos   <- raster::freq(prediccion.inundacion, value = 1)
hectareas.inundadas <- valores.positivos * raster::xres(prediccion.inundacion) * raster::yres(prediccion.inundacion) / 10000
# ------------------------------------------------------------------------------