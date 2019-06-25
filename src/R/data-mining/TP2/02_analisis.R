# ---------------------------------------------------------------------------------------#
# ---- Script para análisis de set de datos para TP2 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(arules)
require(arulesViz)
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

load(file = paste0(getwd(), "/input/PreciosClaros.RData"))
load(file = paste0(getwd(), "/input/ReglasAsociacion.RData"))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- II. Armado de reglas de asociacion integrando todos los datos ----                            
# ---------------------------------------------------------------------------------------#

# Consolidar comunas, comercios, niveles de precios, varaciones de precios y productos (palabras)
# Para no tener los comercios ni las comunas tan atomizadas (15 comunas y 11 comercios con precios relevados),
# se van a agregar las comunas en zonas
regiones <- data.frame(comuna = c(4, 8, 9, 1, 3, 5, 6, 7, 10, 11, 2, 12, 13, 14, 15),
                       zona = c(rep("Sur", 3), rep("Centro", 7), rep("Norte", 5)))       

# Consolidacion
datos.consolidados <- precios.asociacion %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::inner_join(regiones, by = c("comuna")) %>%
  dplyr::inner_join(banderas, by = c("comercioId", "banderaId")) %>%
  dplyr::inner_join(comercios, by = c("comercioId")) %>%
  dplyr::mutate(comercio = factor(razonSocial), tipo = factor(tipo))

# Generacion de transacciones y reglas
transacciones <- datos.consolidados %>%
  dplyr::select(zona, comercio, tipo, DPRT, DVT) %>%
  as("transactions")
reglas <- sort(arules::apriori(data = transacciones,
                               parameter = list(support = 0.01, confidence = 0.6, target = "rules", maxlen = ncol(transacciones))),
               by = "confidence", decreasing = TRUE)

# Reglas de tipos de sucursal
reglas.tipo <- subset(reglas,
                      subset = ((lhs %pin% "tipo") | (rhs %pin% "tipo")) & (lift > 2))
arules::inspect(head(reglas.tipo, 10))

# Reglas de comercios
reglas.comercios <- subset(reglas,
                           subset = ((lhs %pin% "comercio") | (rhs %pin% "comercio")) & (lift > 2))
arules::inspect(head(reglas.comercios, 10))

# Reglas de zonas
reglas.zonas <- subset(reglas,
                       subset = ((lhs %pin% "zona") | (rhs %pin% "zona")) & (lift > 2))
arules::inspect(head(reglas.zonas, 10))

# Reglas de niveles de precios
reglas.nivel.precio <- subset(reglas,
                              subset = ((lhs %pin% "DPRT") | (rhs %pin% "DPRT")) & (lift > 2))
arules::inspect(head(reglas.nivel.precio, 10))

# Reglas de variaciones de precios
reglas.variacion.precio <- subset(reglas,
                                  subset = ((lhs %pin% "DVT") | (rhs %pin% "DVT")) & (lift > 2))
arules::inspect(head(reglas.variacion.precio, 10))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Reglas de asociación para productos ----                            
# ---------------------------------------------------------------------------------------#

# Seleccionar productos que sean bebidas comunes (aguas, jugos, gaseosas y vinos)
terminos       <- paste0("termino_", c("cerveza", "vino", "agua", "bebida", "gaseosa"))
bebidas        <- apply(X = matriz.presencia.ausencia[, terminos], MARGIN = 1, 
                        FUN = function(fila) { return (any(! is.na(fila))) })
matriz.bebidas <- matriz.presencia.ausencia[bebidas, terminos]
datos.bebidas  <- as.data.frame(matriz.bebidas) %>%
  dplyr::mutate(productoId = rownames(.))

# Generar transacciones asociadas a bebidas
transacciones.bebidas <- datos.consolidados %>%
  dplyr::select(productoId, zona, comercio, DPRT, DVT) %>%
  dplyr::inner_join(datos.bebidas, by = c("productoId")) %>%
  dplyr::select(-productoId) %>%
  as("transactions")

# Generar reglas
reglas.bebidas <- sort(arules::apriori(data = transacciones.bebidas,
                                       parameter = list(support = 0.02, confidence = 0.6, target = "rules", maxlen = 20)),
                       by = "confidence", decreasing = TRUE) %>%
  subset(x = ., subset = ((lhs %pin% "termino")) & (lift > 2))
arules::inspect(head(reglas.bebidas, 10))

# Visualizaciones
plot(reglas.bebidas, method = "grouped")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- IV. Reglas de asociación para variaciones de precios ----                            
# ---------------------------------------------------------------------------------------#

# Generar transacciones asociadas a bebidas
transacciones.precios <- datos.consolidados %>%
  dplyr::select(zona, comercio, tipo, DPR1, DPR2, DPR2, DPR2, DPRT, DV1, DV2, DV3, DVT) %>%
  as("transactions")

# Generar reglas
reglas.precios <- sort(arules::apriori(data = transacciones.precios,
                                       parameter = list(support = 0.01, confidence = 0.6, target = "rules", maxlen = 20)),
                       by = "support", decreasing = TRUE) %>%
  subset(x = ., subset = (lift > 2))
arules::inspect(head(reglas.precios, 10))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- V. Validacion de precios usando analisis de correspondencia ----                            
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

#factoextra::fviz_contrib(ca.nivel.precios.comuna, choice = "row", axes = 1)
#factoextra::fviz_contrib(ca.nivel.precios.comuna, choice = "col", axes = 1)
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

#factoextra::fviz_contrib(ca.nivel.precios.comercio, choice = "row", axes = 1)
#factoextra::fviz_contrib(ca.nivel.precios.comercio, choice = "col", axes = 1)
factoextra::fviz_ca_biplot(ca.nivel.precios.comercio, repel = TRUE) 
# ----------------------------------------------------------------------------------------