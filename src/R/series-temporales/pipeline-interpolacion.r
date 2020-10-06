rm(list = objects())

require(automap)
require(Cairo)
require(dplyr)
require(ggplotify)
require(ggpmisc)
require(gstat)
require(httr)
require(jsonlite)
require(purrr)
require(tidyr)
require(sf)
require(stlplus)
require(stars)

# Definir bitmapType
options(bitmapType = "cairo")

# Cargar funciones necesarias para interpolación espacial
source("funciones-interpolacion.r", echo = FALSE)

# Se lee el dataset
argentina <- base::readRDS("data/gadm36_ARG_0_sf.rds")
load("data/Dataset.RData")

# Seleccionamos el período 2001-2010 a efectos ilustrativos. La idea se basa en que hay estaciones más cortas
# (que o bien arrancaron posteriormente a 1961 o bien se dieron de baja antes de 2019). Para poder 
# evaluar todas sin descargar las estaciones cortas, se hace un análisis año a año donde se 
# remueven aquellas estaciones que no cuentan con una mínima cantidad de datos (ver más adelante).
ano.desde         <- 2001
ano.hasta         <- 2010
series.temporales <- datos.estaciones %>%
  dplyr::mutate(fecha = as.Date(fecha)) %>%
  dplyr::filter(lubridate::year(fecha) %in% seq(from = ano.desde, to = ano.hasta, by = 1))

# Seleccionar series temporales "aceptables"
# Filtrado: quedan estaciones con mas del 75% de los datos y no mas de 10 dias consecutivos de datos faltantes.
fechas       <- tidyr::crossing(omm_id = unique(series.temporales$omm_id),
                                fecha = seq(from = as.Date(sprintf("%d-01-01", ano.desde)), 
                                            to = as.Date(sprintf("%d-12-31", ano.hasta)), by = 'days'))
estadisticas <- series.temporales %>%
  dplyr::right_join(fechas, by = c("omm_id", "fecha")) %>%
  dplyr::group_by(omm_id) %>%
  dplyr::summarise(total = dplyr::n(), disponibles = sum(! is.na(valor)), max_faltantes_consecutivos = MaximosFaltantesConsecutivos(valor)) %>%
  dplyr::mutate(tasa_disponibles = disponibles/total)

estaciones.estudio <- estaciones %>%
  dplyr::inner_join(
    dplyr::filter(estadisticas, (tasa_disponibles >= 0.7) & (max_faltantes_consecutivos <= 15)),
    by = c("omm_id"))

series.estudio <- series.temporales %>%
  dplyr::right_join(fechas, by = c("omm_id", "fecha")) %>%
  dplyr::filter(omm_id %in% unique(estaciones.estudio$omm_id))

# Aplicar descomposicion estacional
series.descompuestas <- purrr::map_dfr(
  .x = unique(series.estudio$omm_id),
  .f = function(omm_id) {
    DescomponerSerieTemporal(dplyr::filter(series.estudio, omm_id == !! omm_id))
  }
)

# Aplicar interpolacion
series.interpoladas <- purrr::map_dfr(
  .x = sort(unique(series.descompuestas$fecha)),
  .f = function(fecha) {
    InterpolarValoresFecha(series.descompuestas, estaciones, fecha)
  }
)

# Guardar resultados
save(series.interpoladas, file = "data/SeriesInterpoladas.RData")

# Generar gráficos para reporte en base a Bariloche (87800)
serie.seleccionada <- dplyr::filter(series.interpoladas, omm_id == 87800 & lubridate::year(fecha) >= 2009) %>%
  dplyr::mutate(color = factor(dplyr::if_else(! is.na(valor), "Real", "Imputado"), levels = c("Real", "Imputado"))) %>%
  dplyr::select(fecha, color, seasonal, trend, remainder, valor_interpolado) %>%
  tidyr::pivot_longer(cols = c(seasonal, trend, remainder, valor_interpolado), names_to = 'variable', values_to = 'valor') %>%
  dplyr::mutate(variable = factor(variable, levels = c("trend", "seasonal", "remainder", "valor_interpolado")))

# Descomposición STL
facetas   <- list(
  "trend" = "Tendencia",
  "seasonal" = "Estacionalidad",
  "remainder" = "Residuo",
  "valor_interpolado" = "Serie completa"
)
facetador <- function(string) {
  purrr::map(string, ~facetas[[.x]])
}
ggplot2::ggplot(data = serie.seleccionada) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = fecha, y = valor, col = color, size = color)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = fecha, y = valor), colour = 'grey60', size = 0.1) +
  ggplot2::facet_wrap(~variable, scales = 'free', ncol = 2, labeller = ggplot2::labeller(variable = facetador)) +
  ggplot2::scale_colour_manual(values = c("Real" = "grey50", "Imputado" = "tomato")) +
  ggplot2::scale_size_manual(values = c("Real" = 0.1, "Imputado" = 0.5)) +
  ggplot2::labs(x = 'Fecha', y = 'Temperatura (ºC)', title = 'Descomposición STL aditiva de temperatura máxima',
                subtitle = 'El Bolsón Aero (Bariloche, Argentina)', col = "Dato") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    legend.position = 'bottom'
  ) + ggplot2::guides(size = FALSE) +
  ggplot2::ggsave(filename = "data/DescomposicionSTL.png", device = "png", dpi = 150, width = 8, height = 8)

# Interpolación para El Bolsón para 2009-01-04
fecha    <- as.Date("2009-08-23")
residuos <- estaciones.estudio %>%
  dplyr::inner_join(
    dplyr::filter(series.descompuestas, fecha == !! fecha),
    by = c("omm_id")
  )

# Obtener variograma y ajustar modelo
argentina  <- base::readRDS("data/gadm36_ARG_0_sf.rds")
locations  <- dplyr::filter(residuos, ! is.na(remainder))
variograma <- gstat::variogram(remainder ~ 1, data = locations)
modelo     <- automap::autofitVariogram(formula = remainder ~ 1, input_data = sf::as_Spatial(locations))
kriging    <- gstat::krige(remainder ~ 1, locations = locations, newdata = residuos, model = modelo$var_model) %>%
  dplyr::rename(residuo_prediccion = var1.pred, varianza = var1.var) %>%
  dplyr::mutate(omm_id = dplyr::pull(residuos, omm_id), residuo_original = dplyr::pull(residuos, remainder), error = residuo_prediccion - residuo_original)

# Se grafica el campo interpolado en una grilla regular  
grilla.regular <- sf::st_make_grid(x = argentina, what = "centers", cellsize = c(0.1, 0.1))
kriging.grilla <- gstat::krige(remainder ~ 1, locations = locations, newdata = grilla.regular, model = modelo$var_model)
kriging.arg    <- as(stars::st_rasterize(sf = kriging.grilla), "Raster") %>%
  raster::mask(x = ., mask = argentina)

# Variograma
grafico.variograma      <- plot(modelo, plotit = FALSE)
grafico.variograma$main <- ""
grafico.variograma$xlab <- ""
grafico.variograma$ylab <- ""
grafico.variograma      <- ggplotify::as.ggplot(grafico.variograma)

# Definir insets
inset.tibble <- tibble::tibble(
  x = c(0.99), y = c(0.01),
  vp.width = c(0.5), vp.height = c(0.5),
  plot = list(grafico.variograma)
)

# Mapa de residuos de temperatura
valores.raster <- as.data.frame(raster::rasterToPoints(kriging.arg))
ggplot2::ggplot() +
  ggpmisc::geom_plot_npc(data = inset.tibble,
                       mapping = ggplot2::aes(npcx = x, npcy = y, label = plot, 
                                              vp.width = vp.width, vp.height = vp.height)) +
  ggplot2::geom_raster(data = valores.raster, colour = NA,
                       mapping = ggplot2::aes(x = x, y = y, fill = layer)) +
  ggplot2::geom_sf(data = argentina, colour = "black", fill = NA, size = 0.2) +
  ggplot2::coord_sf(xlim = c(-75, -25), ylim = c(-55, -20)) +
  ggplot2::annotate(geom = "text", x = -36, y = -56, label = "Distancia (km)") +
  ggplot2::annotate(geom = "text", x = -50.5, y = -46.5, label = "Semi-varianza", angle = 90) +
  ggplot2::annotate(geom = "text", x = -37, y = -37.5, label = "Variograma empírico y ajustado", size = 4) +
  ggplot2::labs(x = "", y = "", title = "Interpolación de temperatura máxima",
                subtitle = paste0("El Bolsón Aero (Bariloche, Argentina) - ", format(fecha, "%d/%m/%Y"))) +
  ggplot2::scale_fill_distiller(name = "Residuos (ºC)", palette = "RdYlBu") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(vjust = -12, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(vjust = -15, hjust = 0.5),
    panel.grid.major = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(colour = "white", fill = "grey50"),
    legend.position = c(0.9, 0.8),
    legend.background = ggplot2::element_rect(fill = "grey90", size = 0.1, linetype = "solid", colour = "black")
  ) + ggplot2::ggsave(filename = "data/Interpolacion.png", device = "png", dpi = 300, width = 8, height = 8)
