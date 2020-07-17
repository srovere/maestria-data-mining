# -----------------------------------------------------------------------------#
# --- PASO 1. Inicializar script ----
# -----------------------------------------------------------------------------#
# a) Borrar entorno
rm(list = ls()); gc()

# b) Cargar paquetes
list.of.packages <- c("dplyr", "geojsonsf", "lubridate", "magrittr", "raster", 
                      "rasterVis", "randomForest", "sf", "sp", "utils", "yaml")
for (pack in list.of.packages) {
  if (!require(pack, character.only = TRUE)) {
    stop("Paquete no instalado: ", pack)
  }
}
rm(list.of.packages, pack); gc()

# c) Definir carpetas
working.directory <- paste0(getwd(), "/tp-teledeteccion2020")
images.directory  <- paste0(working.directory, "/images/indices")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Referenciar los datos ----
# -----------------------------------------------------------------------------#

# a) Cargar raster de clases
water.bodies <- raster::raster(paste0(working.directory, "/ground_truth/WaterBodies.tif"))

# b) Definir clases
clases    <- c("Espejo de agua", "Humedal", "No agua")
colores   <- c("#1f78b4", "#1b9e77", "#4d4d4d")
clases.df <- data.frame(id = c(1, 2, 4), nombre = clases)

# c) Asignar clases a raster
water.bodies         <- raster::ratify(water.bodies)
rat                  <- raster::levels(water.bodies)[[1]]
rat$clase            <- clases
levels(water.bodies) <- rat

# d) Mostrar conjunto de entrenamiento
plt.wb <- rasterVis::levelplot(water.bodies, col.regions = colores, main = 'Conjunto de entrenamiento')
plot(plt.wb)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Definir sitios de muestreo ----
# -----------------------------------------------------------------------------#

# a) Definir semilla
set.seed(0)

# b) Generar muestra estratificada
numero.muestras <- 10000
muestra         <- raster::sampleStratified(water.bodies, size = numero.muestras, 
                                            na.rm = TRUE, sp = TRUE)
table(muestra$WaterBodies)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Extraer muestras ----
# -----------------------------------------------------------------------------#

# a) Definir imagen de entrenamiento y nombrar las bandas
imagen.entrenamiento        <- raster::stack(paste0(images.directory, "/201801.tif"))
names(imagen.entrenamiento) <- c("b2", "b3", "b4", "b8", "ndwi", "ndvi")

# b) Extraer muestras para los lugares indicados. Eliminar columna ID.
valores.muestras <- raster::extract(imagen.entrenamiento, muestra, df = TRUE)
valores.muestras <- valores.muestras[, -1]

# c) Agregar la informacion de la clase a las muestras
datos.muestras <- data.frame(clase = muestra$WaterBodies, valores.muestras) %>%
  dplyr::filter(! is.na(ndvi) & ! is.na(ndwi))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 5. Entrenar el modelo ----
# -----------------------------------------------------------------------------#

# a) Crear modelo usando un arbol de decision CART
modelo.cart <- rpart::rpart(as.factor(clase) ~ ., 
                            data = datos.muestras, 
                            method = 'class', 
                            maxdepth = 10)
plot(modelo.cart, uniform = TRUE, main = "Arbol de clasificacion")
text(modelo.cart, cex = 0.8)


# b) Crear modelo usando RandomForest con los parametros base
modelo.rf <- randomForest::randomForest(x = dplyr::select(datos.muestras, -clase), 
                                        y = as.factor(dplyr::pull(dplyr::select(datos.muestras, clase))),
                                        importance = TRUE)
randomForest::varImpPlot(modelo.rf)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 6. Predecir ----
# -----------------------------------------------------------------------------#

# a) Prediccion naive utilizando la misma imagen (pero ahora completa)
prediccion.entrenamiento <- raster::predict(object = imagen.entrenamiento, 
                                            model = modelo.rf, 
                                            type = 'class')
# ------------------------------------------------------------------------------