# ---------------------------------------------------------------------------------------#
# ---- Script para análisis de set de datos para TP2 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(ca)
require(dplyr)
require(factoextra)
require(ggplot2)
require(ggrepel)
require(gridExtra)
require(magrittr)
require(purrr)
require(scatterpie)
require(stringr)
require(sf)
require(tidyr)
require(tm)

load(file = paste0(getwd(), "/input/PreciosClaros.RData"))
load(file = paste0(getwd(), "/input/ReglasAsociacion.RData"))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- II. Analisis de nivel de precios por comunas/comercios (todo el periodo) ----                            
# ---------------------------------------------------------------------------------------#

# i. Pasar a formato largo
nivel.precios <- precios.asociacion %>%
  dplyr::select(productoId, comercioId, banderaId, sucursalId, DPR1, DPR2, DPR3, DPR4, DPRT) %>%
  dplyr::rename(`1` = DPR1, `2` = DPR2, `3` = DPR3, `4` = DPR4) %>%
  tidyr::gather(key = periodo, value = nivelPrecio, -productoId, -comercioId, -banderaId, -sucursalId) %>%
  dplyr::mutate(nivelPrecio = factor(nivelPrecio, levels = base::levels(precios.asociacion$DPRT)))

# ii Por comuna
nivel.precios.comuna <- nivel.precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(comuna, periodo) %>%
  dplyr::mutate(total = dplyr::n()) %>%
  dplyr::group_by(comuna, periodo, nivelPrecio, total) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::mutate(porcentaje = 100 * cantidad / total)
nivel.precios.comuna.general <- nivel.precios.comuna %>%
  dplyr::ungroup() %>%
  dplyr::filter(periodo == "DPRT") %>%
  dplyr::select(comuna, nivelPrecio, porcentaje) %>%
  tidyr::spread(key = nivelPrecio, value = porcentaje) %>%
  dplyr::inner_join(comunas, by = c("comuna"))

colores                        <- rev(RColorBrewer::brewer.pal(7, "RdYlGn"))
names(colores)                 <- levels(nivel.precios.comuna$nivelPrecio)
grafico.precios.comuna.general <- ggplot2::ggplot(data = nivel.precios.comuna.general) +
  ggplot2::geom_sf() +
  scatterpie::geom_scatterpie(mapping = ggplot2::aes(x = centro_x, y = centro_y, group = comuna, r = 0.01, ),
                              data = nivel.precios.comuna.general, 
                              cols = names(colores)) +
  ggrepel::geom_label_repel(mapping = ggplot2::aes(x = centro_x, y = centro_y, label = comuna),
                            nudge_x = 0.005, nudge_y = 0.005, alpha = 0.75) +
  ggplot2::scale_fill_manual(name = "Precio", values = colores) +
  ggplot2::labs(x = "", y = "", title = "Ocurrencia de categorías de precios por comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )

# Analisis de correspondencia simple de nivel de precios vs. comuna
matriz.acs.nivel.precios.comuna <- nivel.precios.comuna.general %>%
  dplyr::select(-centro_x, -centro_y, -geometry) %>%
  as.data.frame()
rownames(matriz.acs.nivel.precios.comuna) <- as.character(dplyr::pull(matriz.acs.nivel.precios.comuna, comuna))
matriz.acs.nivel.precios.comuna %<>% dplyr::select(-comuna)
ca.nivel.precios.comuna <- ca::ca(matriz.acs.nivel.precios.comuna, graph = FALSE)

factoextra::fviz_contrib(ca.nivel.precios.comuna, choice = "row", axes = 1)
factoextra::fviz_contrib(ca.nivel.precios.comuna, choice = "col", axes = 1)
factoextra::fviz_ca_biplot(ca.nivel.precios.comuna, repel = TRUE) 

# iii Por comercio
nivel.precios.comercio <- nivel.precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::group_by(comercioId, banderaId, periodo) %>%
  dplyr::mutate(total = dplyr::n()) %>%
  dplyr::group_by(comercioId, banderaId, periodo, nivelPrecio, total) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::mutate(porcentaje = 100 * cantidad / total) %>%
  dplyr::inner_join(banderas, NULL, by = c("comercioId", "banderaId")) %>%
  dplyr::inner_join(comercios, NULL, by = c("comercioId")) %>%
  dplyr::mutate(comercio = paste0(razonSocial, " - ", descripcion)) %>%
  dplyr::select(comercio, periodo, nivelPrecio, total, cantidad, porcentaje)
nivel.precios.comercio.general <- nivel.precios.comercio %>%
  dplyr::ungroup() %>%
  dplyr::filter(periodo == "DPRT") %>%
  dplyr::select(comercio, nivelPrecio, porcentaje) %>%
  tidyr::spread(key = nivelPrecio, value = porcentaje)

# Analisis de correspondencia simple de nivel de precios vs. comuna
matriz.acs.nivel.precios.comercio <- nivel.precios.comercio.general %>%
  as.data.frame()
rownames(matriz.acs.nivel.precios.comercio) <- dplyr::pull(matriz.acs.nivel.precios.comercio, comercio)
matriz.acs.nivel.precios.comercio %<>% dplyr::select(-comercio)
ca.nivel.precios.comercio <- ca::ca(matriz.acs.nivel.precios.comercio, graph = FALSE)

factoextra::fviz_contrib(ca.nivel.precios.comercio, choice = "row", axes = 1)
factoextra::fviz_contrib(ca.nivel.precios.comercio, choice = "col", axes = 1)
factoextra::fviz_ca_biplot(ca.nivel.precios.comercio, repel = TRUE) 
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Analisis de aumento de precios por comunas/comercios (todo el periodo) ----                            
# ---------------------------------------------------------------------------------------#

# i. Pasar a formato largo
nivel.aumento.precios <- precios.asociacion %>%
  dplyr::select(productoId, comercioId, banderaId, sucursalId, DV1, DV2, DV3, DVT) %>%
  dplyr::rename(`1` = DV1, `2` = DV2, `3` = DV3) %>%
  tidyr::gather(key = periodo, value = nivelAumentoPrecio, -productoId, -comercioId, -banderaId, -sucursalId) %>%
  dplyr::mutate(nivelAumentoPrecio = factor(nivelAumentoPrecio, levels = base::levels(precios.asociacion$DVT)))

# ii Por comuna
nivel.aumento.precios.comuna <- nivel.aumento.precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::group_by(comuna, periodo) %>%
  dplyr::mutate(total = dplyr::n()) %>%
  dplyr::group_by(comuna, periodo, nivelAumentoPrecio, total) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::mutate(porcentaje = 100 * cantidad / total)
nivel.aumento.precios.comuna.general <- nivel.aumento.precios.comuna %>%
  dplyr::ungroup() %>%
  dplyr::filter(periodo == "DVT") %>%
  dplyr::select(comuna, nivelAumentoPrecio, porcentaje) %>%
  tidyr::spread(key = nivelAumentoPrecio, value = porcentaje) %>%
  dplyr::inner_join(comunas, by = c("comuna"))

colores                                <- rev(RColorBrewer::brewer.pal(7, "RdYlGn"))
names(colores)                         <- levels(nivel.aumento.precios.comuna$nivelAumentoPrecio)
grafico.aumento.precios.comuna.general <- ggplot2::ggplot(data = nivel.aumento.precios.comuna.general) +
  ggplot2::geom_sf() +
  scatterpie::geom_scatterpie(mapping = ggplot2::aes(x = centro_x, y = centro_y, group = comuna, r = 0.01, ),
                              data = nivel.aumento.precios.comuna.general, 
                              cols = names(colores)) +
  ggrepel::geom_label_repel(mapping = ggplot2::aes(x = centro_x, y = centro_y, label = comuna),
                            nudge_x = 0.005, nudge_y = 0.005, alpha = 0.75) +
  ggplot2::scale_fill_manual(name = "Nivel de aumento de precio", values = colores) +
  ggplot2::labs(x = "", y = "", title = "Ocurrencia de categorías de aumentos de precio por comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )

# Analisis de correspondencia simple de nivel de aumento de precios vs. comuna
matriz.acs.nivel.aumento.precios.comuna <- nivel.aumento.precios.comuna.general %>%
  dplyr::select(-centro_x, -centro_y, -geometry) %>%
  as.data.frame()
rownames(matriz.acs.nivel.aumento.precios.comuna) <- as.character(dplyr::pull(matriz.acs.nivel.aumento.precios.comuna, comuna))
matriz.acs.nivel.aumento.precios.comuna %<>% dplyr::select(-comuna)
ca.nivel.aumento.precios.comuna <- ca::ca(matriz.acs.nivel.aumento.precios.comuna, graph = FALSE)

factoextra::fviz_contrib(ca.nivel.aumento.precios.comuna, choice = "row", axes = 1)
factoextra::fviz_contrib(ca.nivel.aumento.precios.comuna, choice = "col", axes = 1)
factoextra::fviz_ca_biplot(ca.nivel.aumento.precios.comuna, repel = TRUE) 

# iii Por comercio
nivel.aumento.precios.comercio <- nivel.aumento.precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::group_by(comercioId, banderaId, periodo) %>%
  dplyr::mutate(total = dplyr::n()) %>%
  dplyr::group_by(comercioId, banderaId, periodo, nivelAumentoPrecio, total) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::mutate(porcentaje = 100 * cantidad / total) %>%
  dplyr::inner_join(banderas, NULL, by = c("comercioId", "banderaId")) %>%
  dplyr::inner_join(comercios, NULL, by = c("comercioId")) %>%
  dplyr::mutate(comercio = paste0(razonSocial, " - ", descripcion)) %>%
  dplyr::select(comercio, periodo, nivelAumentoPrecio, total, cantidad, porcentaje)
nivel.aumento.precios.comercio.general <- nivel.aumento.precios.comercio %>%
  dplyr::ungroup() %>%
  dplyr::filter(periodo == "DVT") %>%
  dplyr::select(comercio, nivelAumentoPrecio, porcentaje) %>%
  tidyr::spread(key = nivelAumentoPrecio, value = porcentaje)

# Analisis de correspondencia simple de nivel de aumento de precios vs. comuna
matriz.acs.nivel.aumento.precios.comercio <- nivel.aumento.precios.comercio.general %>%
  as.data.frame()
rownames(matriz.acs.nivel.aumento.precios.comercio) <- dplyr::pull(matriz.acs.nivel.aumento.precios.comercio, comercio)
matriz.acs.nivel.aumento.precios.comercio %<>% dplyr::select(-comercio)
ca.nivel.aumento.precios.comercio <- ca::ca(matriz.acs.nivel.aumento.precios.comercio, graph = FALSE)

factoextra::fviz_contrib(ca.nivel.aumento.precios.comercio, choice = "row", axes = 1)
factoextra::fviz_contrib(ca.nivel.aumento.precios.comercio, choice = "col", axes = 1)
factoextra::fviz_ca_biplot(ca.nivel.aumento.precios.comercio, repel = TRUE) 
# ----------------------------------------------------------------------------------------