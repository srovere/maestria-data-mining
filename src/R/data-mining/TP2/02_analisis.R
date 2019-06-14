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
# ---- II. Analisis de nivel de precios por barrios/comunas/comercios ----                            
# ---------------------------------------------------------------------------------------#

# i. Contar la ocurrencia de cada categoria de nivel de precios por barrio/comuna/comercio y periodo
nivel.precios <- precios.asociacion %>%
  dplyr::select(productoId, comercioId, banderaId, sucursalId, DPR1, DPR2, DPR3, DPR4, DPRT) %>%
  dplyr::rename(`1` = DPR1, `2` = DPR2, `3` = DPR3, `4` = DPR4) %>%
  tidyr::gather(key = periodo, value = nivelPrecio, -productoId, -comercioId, -banderaId, -sucursalId) %>%
  dplyr::mutate(nivelPrecio = factor(nivelPrecio, levels = base::levels(precios.asociacion$DPRT)))

# i. a) Por barrio
nivel.precios.barrio <- nivel.precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::group_by(barrioId, periodo, nivelPrecio) %>%
  dplyr::summarise(cantidad = dplyr::n())
nivel.precios.barrio <- barrios %>%
  dplyr::inner_join(nivel.precios.barrio, by = c("barrioId"))
grafico.precios.barrio <- ggplot2::ggplot(data = nivel.precios.barrio) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = cantidad)) +
  ggplot2::facet_grid(periodo ~ nivelPrecio) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(x = "", y = "", fill = "",
                title = "Ocurrencia de categorías de precios por período y barrio") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  ) +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 30 , label.position = "bottom"))
# ----------------------------------------------------------------------------------------
