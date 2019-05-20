# ---------------------------------------------------------------------------------------#
# ---- Script para análisis de set de datos para TP1 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(dplyr)
require(ggplot2)
require(ggrepel)
require(gridExtra)
require(magrittr)
require(plotly)
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
  ggplot2::labs(x = "", y = "", fill = "",
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
  ggplot2::labs(x = "", y = "", fill = "",
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
  ggplot2::labs(x = "", y = "", fill = "",
                title = "Proporción de sucursales por tipo y comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

tipo.sucursales         <- sucursales %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::group_by(tipo) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::mutate(porcentaje = cantidad / sum(cantidad))
grafico.sucursales.tipo <- ggplot2::ggplot(data = tipo.sucursales) +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = "", y = porcentaje, fill = tipo), stat = 'identity', width = 1) + 
  ggplot2::coord_polar("y", 0) +
  ggplot2::labs(x = "", y = "", fill = "",
                title = "Proporción de sucursales por tipo") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
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
  ggplot2::labs(x = "", y = "", fill = "",
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
  ggplot2::labs(x = "", y = "", fill = "",
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
  ggplot2::labs(x = "", y = "", fill = "",
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
# ---- V. Cálculo de evolución temporal de datos relevados ----                            
# ---------------------------------------------------------------------------------------#

# i. Data frame ancho para analizar evolucion de precios por producto y sucursal
mediciones        <- as.character(sort(unique(precios$medicion)))
evolucion.precios <- precios %>%
  dplyr::select(-fecha, -score_producto, -score_producto_medicion) %>%
  tidyr::spread(key = medicion, value = precio)

# ii. Cálculo de evolucion de precios para cada medición
evolucion.precios %<>%
  dplyr::bind_cols(
    purrr::map2_dfc(
      .x = head(mediciones, n = length(mediciones) - 1),
      .y = tail(mediciones, n = length(mediciones) - 1),
      .f = function(medicion.inicial, medicion.final) {
        columna.evolucion <- paste0("evolucion_", medicion.inicial, "_", medicion.final)
        return (evolucion.precios %>% 
                  dplyr::mutate(!! columna.evolucion := 100.0 * (!!rlang::sym(medicion.final) - !!rlang::sym(medicion.inicial)) / !!rlang::sym(medicion.inicial)) %>%
                  dplyr::select(!! columna.evolucion)
        )
      }
    )
  )

# iii. Pasar las evoluciones a formato largo
evoluciones.precio.largo <- evolucion.precios %>%
  dplyr::select(-mediciones) %>%
  tidyr::gather(key = nombre_evolucion, value = evolucion_porcentual, -productoId, -comercioId, -banderaId, -sucursalId) %>%
  tidyr::separate(col = nombre_evolucion, into = c(NA, 'medicion_inicial', 'medicion_final'), sep = '_') %>%
  dplyr::filter(! is.na(evolucion_porcentual))

# iv. Analisis de precios cuyo valor tenemos de punta a punta.
evolucion.punta.a.punta <- evolucion.precios %>%
  dplyr::mutate(evolucion_porcentual_total = 100 * (`10` - `1`) / `1`) %>%
  dplyr::filter(! is.na(evolucion_porcentual_total))

# v. Variacion segun IPC GCBA
canasta.mediciones <- canasta %>%
  dplyr::filter((fecha >= as.Date("2018-10-01")) & (fecha <= as.Date("2019-02-01"))) %>%
  dplyr::mutate(total_anterior = dplyr::lag(total, n = 1)) %>%
  dplyr::mutate(diferencia_relativa = (total - total_anterior)/total_anterior) %>%
  dplyr::filter(! is.na(diferencia_relativa))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- VI. Generación de gráficos de evolución temporal de precios ----                            
# ---------------------------------------------------------------------------------------#

# i. Boxplot general
media.alimentos           <- 100 * (tail(cumprod(canasta.mediciones$diferencia_relativa+1), n = 1) - 1)
media.ipc.gcba            <- 100 * (tail(cumprod(c(2.8, 2.4, 3.8, 3.4)/100 + 1), n = 1) - 1)
media.evolucion           <- mean(evolucion.punta.a.punta$evolucion_porcentual_total, na.rm = TRUE)
mediana.evolucion         <- median(evolucion.punta.a.punta$evolucion_porcentual_total, na.rm = TRUE)
grafico.evolucion.general <- ggplot2::ggplot(data = evolucion.punta.a.punta) +
  ggplot2::geom_boxplot(mapping = ggplot2::aes(x = 'C.A.B.A', y = evolucion_porcentual_total)) +
  ggrepel::geom_label_repel(data = data.frame(media = media.evolucion),
                            mapping = ggplot2::aes(x = 'C.A.B.A', y = media, 
                                                   label = sprintf("Media (Precios Claros): %.2f%%", media)),
                            nudge_x = 0.5, nudge_y = 10) +
  ggrepel::geom_label_repel(data = data.frame(mediana = mediana.evolucion),
                            mapping = ggplot2::aes(x = 'C.A.B.A', y = mediana, 
                                                   label = sprintf("Mediana (Precios Claros): %.2f%%", mediana)),
                            nudge_x = -0.5, nudge_y = 10) +
  ggrepel::geom_label_repel(data = data.frame(alimentos = media.alimentos),
                            mapping = ggplot2::aes(x = 'C.A.B.A', y = alimentos, 
                                                   label = sprintf("Alimentos (IPC GCBA): %.2f%%", alimentos)),
                            nudge_x = 0.25, nudge_y = 50) +
  ggrepel::geom_label_repel(data = data.frame(bienes_servicios = media.ipc.gcba),
                            mapping = ggplot2::aes(x = 'C.A.B.A', y = bienes_servicios, 
                                                   label = sprintf("Bienes y servicios (IPC GCBA): %.2f%%", bienes_servicios)),
                            nudge_x = -0.25, nudge_y = 50) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "", y = "Diferencia porcentual", 
                title = "Evolución porcentual de precios para C.A.B.A",
                subtitle = "Diferencia medida entra la última y la primera medición") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )

# ii. Boxplots por tipo de sucursal
evolucion.por.tipo.sucursal <- evolucion.punta.a.punta %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::select(tipo, evolucion_porcentual_total)
estadisticas.por.tipo.sucursal <- evolucion.por.tipo.sucursal %>%
  dplyr::group_by(tipo) %>% 
  dplyr::summarize(mediana_evolucion_porcentual_total = median(evolucion_porcentual_total),
                   media_evolucion_porcentual_total = mean(evolucion_porcentual_total))
grafico.boxplots.evolucion.por.tipo.sucursal <- ggplot2::ggplot(data = evolucion.por.tipo.sucursal) +
  ggplot2::geom_boxplot(mapping = ggplot2::aes(x = as.factor(tipo), y = evolucion_porcentual_total, 
                                               group = as.factor(tipo), fill = as.factor(tipo))) +
  ggrepel::geom_label_repel(data = estadisticas.por.tipo.sucursal,
                            mapping = ggplot2::aes(x = tipo, y = media_evolucion_porcentual_total, 
                                                   label = sprintf("Media (Precios Claros): %.2f%%", media_evolucion_porcentual_total)),
                            nudge_x = 0.25, nudge_y = 50) +
  ggrepel::geom_label_repel(data = estadisticas.por.tipo.sucursal,
                            mapping = ggplot2::aes(x = tipo, y = mediana_evolucion_porcentual_total, 
                                                   label = sprintf("Mediana (Precios Claros): %.2f%%", mediana_evolucion_porcentual_total)),
                            nudge_x = -0.25, nudge_y = 50) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "Tipo de sucursal", y = "Diferencia porcentual", fill = "",
                title = "Evolución porcentual de precios por tipo de sucursal",
                subtitle = "Diferencia medida entra la última y la primera medición") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )

# iii. Mapas por comuna
grafico.mediana.evolucion.por.comuna <- ggplot2::ggplot(data = estadisticas.por.comuna) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = mediana_evolucion_porcentual_total)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "", y = "", fill = "",
                title = "Mediana de evolución porcentual de precios",
                subtitle = "Diferencia medida entra la última y la primera medición") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )

grafico.media.evolucion.por.comuna <- ggplot2::ggplot(data = estadisticas.por.comuna) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = media_evolucion_porcentual_total)) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "", y = "", fill = "",
                title = "Media de evolución porcentual de precios",
                subtitle = "Diferencia medida entra la última y la primera medición") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )

# iv. Boxplots por comercio
evolucion.por.comercio <- evolucion.punta.a.punta %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(banderas, by = c("comercioId", "banderaId")) %>%
  dplyr::rename(bandera = descripcion) %>%
  dplyr::inner_join(comercios, by = c("comercioId")) %>%
  dplyr::mutate(comercio = paste0(razonSocial, " - ", bandera)) %>%
  dplyr::group_by(comercio) %>%
  dplyr::summarize(Mediana = median(evolucion_porcentual_total),
                   Media = mean(evolucion_porcentual_total)) %>%
  dplyr::arrange(Mediana)
evolucion.por.comercio$comercio <- factor(evolucion.por.comercio$comercio, levels = evolucion.por.comercio$comercio)
grafico.evolucion.por.comercio <- ggplot2::ggplot(data = tidyr::gather(evolucion.por.comercio, key = metrica, value = valor, -comercio)) +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = comercio, y = valor, group = metrica, fill = metrica), 
                    stat = 'identity', position = 'dodge') +
  ggplot2::geom_text(data = evolucion.por.comercio,
                     mapping = ggplot2::aes(x = comercio, y = Mediana,
                                            label = sprintf("%0.2f%%", round(Mediana, digits = 2))), 
                     color = "black", size = 3.5, hjust = 2, vjust = -0.5) +
  ggplot2::geom_text(data = evolucion.por.comercio,
                     mapping = ggplot2::aes(x = comercio, y = Media,
                                            label = sprintf("%0.2f%%", round(Media, digits = 2))), 
                     color = "black", size = 3.5, hjust = 2, vjust = +1.5) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "", y = "Diferencia porcentual", fill = "Métrica",
                title = "Evolución porcentual de precios por tipo de sucursal",
                subtitle = "Diferencia medida entra la última y la primera medición") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- VII. Comparación de precios por sucursal, comuna y tipo de sucursal ----                            
# ---------------------------------------------------------------------------------------#

# i. Precios por sucursal
zscore.precios.sucursal <- sucursales %>%
  dplyr::inner_join(
    precios %>%
      dplyr::group_by(comercioId, banderaId, sucursalId) %>%
      dplyr::summarise(zscore_precio_mediana = median(score_producto)),
    by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::arrange(zscore_precio_mediana)

# ii. Precios por comuna
precios.comuna <- precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId"))
estadisticas.comuna.medicion <- precios.comuna %>%
  dplyr::group_by(comuna, medicion) %>%
  dplyr::summarise(mediana = median(score_producto_medicion), media = mean(score_producto_medicion)) %>%
  dplyr::right_join(comunas, by = c("comuna"))
 
grafico.scores.precios.comunas <- ggplot2::ggplot(data = estadisticas.comuna.medicion) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = media)) +
  ggplot2::facet_wrap(~medicion, ncol = 5) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "", y = "", fill = "",
                title = "Score de precios relevados por comuna",
                subtitle = "Evolución a lo largo de las mediciones") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  ) +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 10 , label.position = "bottom"))

# iii. Precios por comercio
precios.comercio <- precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::group_by(comercioId, banderaId, medicion) %>%
  dplyr::summarise(score_mediana_comercio_medicion = median(score_producto_medicion)) %>%
  dplyr::group_by(medicion) %>%
  dplyr::mutate(ranking = rank(score_mediana_comercio_medicion)) %>%
  dplyr::select(comercioId, banderaId, medicion, ranking) %>%
  dplyr::inner_join(banderas, by = c("comercioId", "banderaId")) %>%
  dplyr::rename(bandera = descripcion) %>%
  dplyr::inner_join(comercios, by = c("comercioId")) %>%
  dplyr::mutate(comercio = paste0(razonSocial, " - ", bandera))

grafico.ranking.precios.comercio <- ggplot2::ggplot(data = precios.comercio) +
  ggplot2::geom_tile(mapping = ggplot2::aes(x = medicion, y = comercio, fill = ranking)) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = medicion, y = comercio, label = ranking)) +
  ggplot2::scale_x_continuous(breaks = sort(unique(precios.comercio$medicion))) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1, breaks = sort(unique(precios.comercio$ranking)),
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "Medición", y = "Empresa", fill = "",
                title = "Ranking de precios por comercio y medición",
                subtitle = "Evolución a lo largo de las mediciones") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 20 , label.position = "bottom"))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- ??. Almacenamiento de variables necesarias para el informe ----                            
# ---------------------------------------------------------------------------------------#

save(grafico.sucursales.barrio, grafico.sucursales.comuna, grafico.sucursales.tipo.comuna, grafico.sucursales.tipo,
     grafico.precios.barrio, grafico.precios.comuna, grafico.precios.sucursales.comuna, grafico.cantidad.datos.relevados,
     zscore.precios.sucursal, grafico.scores.precios.comunas, grafico.ranking.precios.comercio,
     grafico.evolucion.general, grafico.media.evolucion.por.comuna, grafico.mediana.evolucion.por.comuna,
     grafico.boxplots.evolucion.por.tipo.sucursal, grafico.evolucion.por.comercio,
     file = paste0(getwd(), "/output/Informe.RData"))
# ----------------------------------------------------------------------------------------
