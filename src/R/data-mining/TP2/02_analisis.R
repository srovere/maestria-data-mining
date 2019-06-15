# ---------------------------------------------------------------------------------------#
# ---- Script para análisis de set de datos para TP2 ----                            
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
# ---- II. Analisis de nivel de precios por comunas/comercios ----                            
# ---------------------------------------------------------------------------------------#

# i. Pasar a formato largo
nivel.precios <- precios.asociacion %>%
  dplyr::select(productoId, comercioId, banderaId, sucursalId, DPR1, DPR2, DPR3, DPR4, DPRT) %>%
  dplyr::rename(`1` = DPR1, `2` = DPR2, `3` = DPR3, `4` = DPR4) %>%
  tidyr::gather(key = periodo, value = nivelPrecio, -productoId, -comercioId, -banderaId, -sucursalId) %>%
  dplyr::mutate(nivelPrecio = factor(nivelPrecio, levels = base::levels(precios.asociacion$DPRT)))

# i. a) Por comuna
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
nivel.precios.comuna.periodo <- nivel.precios.comuna %>%
  dplyr::ungroup() %>%
  dplyr::filter(periodo != "DPRT") %>%
  dplyr::select(comuna, periodo, nivelPrecio, porcentaje) %>%
  tidyr::spread(key = nivelPrecio, value = porcentaje) %>%
  dplyr::inner_join(comunas, by = c("comuna"))

colores                        <- rev(RColorBrewer::brewer.pal(7, "RdYlGn"))
names(colores)                 <- levels(nivel.precios.comuna$nivelPrecio)
grafico.precios.comuna.general <- ggplot2::ggplot(data = nivel.precios.comuna.general) +
  ggplot2::geom_sf() +
  scatterpie::geom_scatterpie(mapping = ggplot2::aes(x = centro_x, y = centro_y, group = comuna, r = 0.01, ),
                              data = nivel.precios.comuna.general, 
                              cols = names(colores)) +
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
grafico.precios.comuna.periodo <- ggplot2::ggplot(data = nivel.precios.comuna.periodo) +
  ggplot2::geom_sf() +
  scatterpie::geom_scatterpie(mapping = ggplot2::aes(x = centro_x, y = centro_y, group = comuna, r = 0.01, ),
                              data = nivel.precios.comuna.periodo, 
                              cols = names(colores)) +
  ggplot2::facet_wrap(~periodo, ncol = 2) +
  ggplot2::scale_fill_manual(name = "Precio", values = colores) +
  ggplot2::labs(x = "", y = "", title = "Ocurrencia de categorías de precios por comuna y período") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )
# ----------------------------------------------------------------------------------------
