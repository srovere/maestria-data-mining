# ---------------------------------------------------------------------------------------#
# ---- Script para análisis de set de datos para TP1 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(dplyr)
require(ggplot2)
require(gridExtra)
require(magrittr)
require(purrr)
require(scatterpie)
require(sf)
require(tidyr)

load(file = paste0(getwd(), "/input/PreciosClaros.RData"))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- II. Generacion de gráficos de distribución espacial de sucursales ----                            
# ---------------------------------------------------------------------------------------#

# I. Sucursales por barrio
sucursales.por.barrio <- sucursales %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::group_by(barrioId) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::inner_join(barrios, by = c("barrioId"))

grafico.sucursales.barrio <- ggplot2::ggplot(data = sucursales.por.barrio) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = cantidad)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Cantidad de sucursales por barrio") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# II. Sucursales por comuna
sucursales.por.comuna <- sf::st_set_geometry(sucursales, NULL) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(comuna) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::inner_join(comunas, by = c("comuna"))

grafico.sucursales.comuna <- ggplot2::ggplot(data = sucursales.por.comuna) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = cantidad)) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = centro_x, y = centro_y, label = comuna)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Cantidad de sucursales por comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# III. Sucursales por comuna y tipo de sucursal
sucursales.por.tipo.comuna <- sf::st_set_geometry(sucursales, NULL) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(comuna, tipo) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  tidyr::spread(key = tipo, value = cantidad) %>%
  dplyr::inner_join(comunas, by = c("comuna"))

grafico.sucursales.tipo.comuna <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = sucursales.por.tipo.comuna, mapping = ggplot2::aes()) +
  scatterpie::geom_scatterpie(mapping = ggplot2::aes(x = centro_x, y = centro_y, group = comuna, r = 0.01),
                              data = sucursales.por.tipo.comuna, cols = unique(sucursales$tipo)) +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Proporción de sucursales por tipo y comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

rm(sucursales.por.barrio, sucursales.por.tipo.comuna)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Generacion de gráficos de distribución espacial de datos relevados ----                            
# ---------------------------------------------------------------------------------------#

# I. Cantidad de precios por barrio
precios.por.barrio <- precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(barrioId) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::right_join(barrios, by = c("barrioId"))

grafico.precios.barrio <- ggplot2::ggplot(data = precios.por.barrio) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = cantidad)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Cantidad de precios relevados por barrio") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# II. Cantidad de precios por comuna
precios.por.comuna <- precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(comuna) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::inner_join(comunas, by = c("comuna"))

grafico.precios.comuna <- ggplot2::ggplot(data = precios.por.comuna) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = cantidad)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Cantidad de precios relevados por comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# III. Cantidad de precios/sucursales por comuna
ratio.precios.sucursales.por.comuna <- precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(comuna) %>%
  dplyr::summarise(cantidad_precios = dplyr::n()) %>%
  dplyr::inner_join(dplyr::select(sucursales.por.comuna, comuna, cantidad)) %>%
  dplyr::rename(cantidad_sucursales = cantidad) %>%
  dplyr::mutate(ratio_precios_sucursales = cantidad_precios / cantidad_sucursales) %>%
  dplyr::inner_join(comunas, by = c("comuna"))

grafico.precios.sucursales.comuna <- ggplot2::ggplot(data = ratio.precios.sucursales.por.comuna) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = ratio_precios_sucursales)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "Longitud", y = "Latitud", fill = "",
                title = "Proporción de precios/sucursales relevados por comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

rm(sucursales.por.comuna, precios.por.barrio, precios.por.comuna, ratio.precios.sucursales.por.comuna)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- IV. Generacion de gráficos de distribución temporal de datos relevados ----                            
# ---------------------------------------------------------------------------------------#

# i. Mediciones realizadas
cantidad.datos.relevados <- precios %>%
  dplyr::group_by(medicion) %>%
  dplyr::summarise(fecha_desde = min(fecha), fecha_hasta = max(fecha),
                   cantidad_datos = dplyr::n()) %>%
  dplyr::arrange(medicion)

grafico.cantidad.datos.relevados <- ggplot2::ggplot(data = cantidad.datos.relevados) +
  ggplot2::geom_rect(mapping = ggplot2::aes(xmin = fecha_desde, xmax = fecha_hasta,
                                            ymin = 0, ymax = cantidad_datos, fill = as.factor(medicion))) +
  ggplot2::scale_x_datetime(date_breaks = '7 days', date_labels = '%d/%m/%Y') +
  ggplot2::labs(x = "Fecha", y = "Cantidad de datos relevados",
                title = "Cantidad de datos relevados por cada medición",
                fill = "Nº de medición") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

rm(cantidad.datos.relevados)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- V. Generacion de gráficos de evolución temporal de datos relevados ----                            
# ---------------------------------------------------------------------------------------#

# i. Data frame ancho para analizar evolucion de precios por producto y sucursal
mediciones        <- sort(unique(precios$medicion))
evolucion.precios <- precios %>%
  dplyr::select(-fecha) %>%
  tidyr::spread(key = medicion, value = precio)

# ii. Cálculo de evolucion de precios para cada medición
p
# ----------------------------------------------------------------------------------------