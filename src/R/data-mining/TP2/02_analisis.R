# ---------------------------------------------------------------------------------------#
# ---- Script para análisis de set de datos para TP2 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
# Limpiar ambiente
rm(list = objects())

# Cargar paquetes
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

# Cargar datos preprocesados
load(file = paste0(getwd(), "/input/PreciosClaros.RData"))
load(file = paste0(getwd(), "/input/ReglasAsociacion.RData"))

# Funcion para pasar reglas a data frame
ReglasADataFrame <- function(reglas) {
  reglas.data.frame = data.frame(
    lhs = labels(lhs(reglas)),
    rhs = labels(rhs(reglas)),
    stringsAsFactors = TRUE
  ) %>%
  dplyr::bind_cols(reglas@quality)
  return (reglas.data.frame)
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- II. Armado de reglas de asociacion integrando todos los datos ----                            
# ---------------------------------------------------------------------------------------#

# Consolidar comunas, comercios, niveles de precios, varaciones de precios y productos (palabras)
# Para no tener los comercios ni las comunas tan atomizadas (15 comunas y 11 comercios con precios relevados),
# se van a agregar las comunas en zonas
regiones <- data.frame(comuna = c(1, 2, 3, 5, 6, 7, 15, 4, 8, 9, 10, 11, 12, 13, 14),
                       zona = c(rep("Este", 3), rep("Centro", 4), rep("Sur", 3), rep("Oeste", 2), rep("Norte", 3)),
                       stringsAsFactors = TRUE)  
grafico.regiones <- ggplot2::ggplot(data = dplyr::inner_join(comunas, regiones, by = c("comuna"))) +
  ggplot2::geom_sf(mapping = ggplot2::aes(fill = zona)) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = centro_x, y = centro_y, label = comuna)) +
  ggplot2::scale_fill_brewer(type = "qual", palette = "Set1") +
  ggplot2::labs(x = "", y = "", fill = "",
                title = "Zonificación de comunas") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )

# Redefinicion de nombres de comercios
comercios.cortos <- data.frame(
  comercioId = c(3, 4, 9, 10, 11, 12, 15, 19, 23, 29),
  razonSocial = c("Deheza", "Lima", "Jumbo", "Carrefour", "Walmart", "Coto", "Dia", "OES", "PAE", "Josimar"),
  stringsAsFactors = FALSE
)

# Consolidacion
datos.consolidados <- precios.asociacion %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(sf::st_set_geometry(barrios, NULL), by = c("barrioId")) %>%
  dplyr::inner_join(regiones, by = c("comuna")) %>%
  dplyr::inner_join(banderas, by = c("comercioId", "banderaId")) %>%
  dplyr::inner_join(comercios.cortos, by = c("comercioId")) %>%
  dplyr::mutate(comercio = factor(razonSocial), tipo = factor(tipo))

# Grafico de porcentajes de datos por region y comercio
grafico.porcentaje.datos.zona <- ggplot2::ggplot(data = dplyr::group_by(datos.consolidados, zona) %>% 
                                                   dplyr::summarise(cantidad = dplyr::n()) %>%
                                                   dplyr::mutate(porcentaje = 100 * cantidad / sum(cantidad))) +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = "", y = porcentaje, fill = zona), stat = 'identity', width = 1) +
  ggplot2::scale_fill_brewer(type = "qual", palette = "Set1") +
  ggplot2::coord_polar("y", 0) +
  ggplot2::labs(x = "", y = "", fill = "",
                title = "Proporción de datos por zona") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )
grafico.porcentaje.datos.comercio <- ggplot2::ggplot(data = dplyr::group_by(datos.consolidados, comercio) %>% 
                                                       dplyr::summarise(cantidad = dplyr::n()) %>%
                                                       dplyr::mutate(porcentaje = 100 * cantidad / sum(cantidad))) +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = "", y = porcentaje, fill = comercio), stat = 'identity', width = 1) +
  ggplot2::scale_fill_brewer(type = "qual", palette = "Set1") +
  ggplot2::coord_polar("y", 0) +
  ggplot2::labs(x = "", y = "", fill = "",
                title = "Proporción de datos por comercio") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )

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
plot(reglas.tipo, method = "matrix", measure = "confidence")

# Reglas de comercios
reglas.comercios <- subset(reglas,
                           subset = ((lhs %pin% "comercio") | (rhs %pin% "comercio")) & (lift > 2))
arules::inspect(head(reglas.comercios, 10))
plot(reglas.comercios, method = "matrix", measure = "confidence")

# Reglas de zonas
reglas.zonas <- subset(reglas,
                       subset = ((lhs %pin% "zona") | (rhs %pin% "zona")) & (lift > 2))
arules::inspect(head(reglas.zonas, 10))
plot(reglas.zonas, method = "matrix", measure = "confidence")

# Reglas de niveles de precios
reglas.nivel.precio <- subset(reglas,
                              subset = ((lhs %pin% "DPRT") | (rhs %pin% "DPRT")) & (lift > 2))
arules::inspect(head(reglas.nivel.precio, 10))
plot(reglas.nivel.precio, method = "matrix", measure = "confidence")

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
arules::inspect(head(reglas.bebidas, 20))
plot(reglas.bebidas, method = "matrix")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- IV. Reglas de asociación para variaciones de precios ----                            
# ---------------------------------------------------------------------------------------#

# Generar transacciones asociadas a variaciones de precios
transacciones.precios <- datos.consolidados %>%
  dplyr::select(zona, comercio, tipo, DPR1, DPR2, DPR3, DPR4, DV1, DV2, DV3) %>%
  as("transactions")

# Generar reglas
reglas.precios <- sort(arules::apriori(data = transacciones.precios,
                                       parameter = list(support = 0.02, confidence = 0.6, target = "rules", maxlen = 20)),
                       by = "confidence", decreasing = TRUE) %>%
  subset(x = ., subset = (rhs %pin% 'DV3') & ((lhs %pin% 'zona') | (lhs %pin% 'comercio') | (lhs %pin% 'tipo')) & (lift > 2) & (confidence < 1))
arules::inspect(head(reglas.precios, 30))
plot(reglas.precios, method = "matrix", measure = "confidence")

# Preparacion para hacer analisis de correspondencia entre periodos y aumento de precios
periodos.variacion.precios <- datos.consolidados %>%
  dplyr::select(DV1, DV2, DV3) %>%
  tidyr::gather(key = Periodo, value = Variacion) %>%
  dplyr::group_by(Periodo, Variacion) %>%
  dplyr::summarise(Cantidad = dplyr::n()) %>%
  tidyr::spread(key = Periodo, value = Cantidad) %>%
  as.data.frame()
rownames(periodos.variacion.precios) <- periodos.variacion.precios$Variacion
periodos.variacion.precios <- periodos.variacion.precios %>%
  dplyr::select(-Variacion)

# Graficamos los porcentajes de cassos por categorias y periodos
porcentajes.periodo.variacion <- datos.consolidados %>%
  dplyr::select(DV1, DV2, DV3) %>%
  tidyr::gather(key = Periodo, value = Variacion) %>%
  dplyr::mutate(Variacion = factor(Variacion, levels = levels(datos.consolidados$DVT))) %>%
  dplyr::group_by(Periodo, Variacion) %>%
  dplyr::summarise(Cantidad = dplyr::n()) %>%
  dplyr::group_by(Periodo) %>%
  dplyr::mutate(Porcentaje = 100 * Cantidad / sum(Cantidad))
grafico.periodo.variacion <- ggplot2::ggplot(data = porcentajes.periodo.variacion) +
  ggplot2::geom_tile(mapping = ggplot2::aes(x = Periodo, y = Variacion, fill = Porcentaje)) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = Periodo, y = Variacion, label = sprintf("%.2f%%", Porcentaje))) +
  ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
                                direction = 1, option = "D", values = NULL, space = "Lab",
                                na.value = "white", guide = "colourbar", aesthetics = "fill") +
  ggplot2::labs(y = "Nivel de aumento de precios", x = "Interperíodo", fill = "",
                title = "Mapa de calor de categorías de variación de precios",
                subtitle = "Evolución a lo largo de los 3 interperíodos") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 20 , label.position = "bottom"))
  
# Analisis de correspondencia
ca.periodos.variacion.precios <- ca::ca(periodos.variacion.precios, graph = FALSE)
factoextra::fviz_contrib(ca.periodos.variacion.precios, choice = "row", axes = 1)
factoextra::fviz_contrib(ca.periodos.variacion.precios, choice = "col", axes = 1)
grafico.ca.periodos.variacion <- factoextra::fviz_ca_biplot(ca.periodos.variacion.precios, repel = TRUE) 

# En base a este analisis, redefinimos los grupos en Maniene, Variacion Leve, Variacion Moderada, Variacion Fuerte
periodos.variacion.precios.reagrupado <- datos.consolidados %>%
  dplyr::select(DV1, DV2, DV3) %>%
  tidyr::gather(key = Periodo, value = Variacion) %>%
  dplyr::mutate(Variacion = factor(Variacion, levels = levels(datos.consolidados$DVT))) %>%
  dplyr::mutate(VariacionReagrupada = forcats::fct_collapse(
    Variacion,
    "Mantiene" = c("Mantiene"),
    "Variación Leve" = c("Aumento Leve", "Disminución Leve"),
    "Variación Moderada" = c("Aumento Moderado", "Disminución Moderada"),
    "Variación Fuerte" = c("Aumento Fuerte", "Disminución Fuerte")
  )) %>%
  dplyr::group_by(Periodo, VariacionReagrupada) %>%
  dplyr::summarise(Cantidad = dplyr::n()) %>%
  tidyr::spread(key = Periodo, value = Cantidad) %>%
  as.data.frame()
rownames(periodos.variacion.precios.reagrupado) <- periodos.variacion.precios.reagrupado$VariacionReagrupada
periodos.variacion.precios.reagrupado <- periodos.variacion.precios.reagrupado %>%
  dplyr::select(-VariacionReagrupada)

# Nuevo analisis de correspondencia
nuevo.ca.periodos.variacion.precios <- ca::ca(periodos.variacion.precios.reagrupado, graph = FALSE)
factoextra::fviz_contrib(nuevo.ca.periodos.variacion.precios, choice = "row", axes = 1)
factoextra::fviz_contrib(nuevo.ca.periodos.variacion.precios, choice = "col", axes = 1)
grafico.ca.periodos.variacion.nuevo <- factoextra::fviz_ca_biplot(nuevo.ca.periodos.variacion.precios, repel = TRUE) 

# Vuenas reglas de asociacion para la variacion de precios
transacciones.precios.modificadas <- datos.consolidados %>%
  dplyr::select(zona, comercio, tipo, DPR1, DPR2, DPR3, DPR4, DV1, DV2, DV3) %>%
  dplyr::mutate(
    DV1 = forcats::fct_collapse(DV1,
      "Mantiene" = c("Mantiene"),
      "Variación Leve" = c("Aumento Leve", "Disminución Leve"),
      "Variación Moderada" = c("Aumento Moderado", "Disminución Moderada"),
      "Variación Fuerte" = c("Aumento Fuerte", "Disminución Fuerte")
    ),
    DV2 = forcats::fct_collapse(DV2,
      "Mantiene" = c("Mantiene"),
      "Variación Leve" = c("Aumento Leve", "Disminución Leve"),
      "Variación Moderada" = c("Aumento Moderado", "Disminución Moderada"),
      "Variación Fuerte" = c("Aumento Fuerte", "Disminución Fuerte")
    ),
    DV3 = forcats::fct_collapse(DV3,
      "Mantiene" = c("Mantiene"),
      "Variación Leve" = c("Aumento Leve", "Disminución Leve"),
      "Variación Moderada" = c("Aumento Moderado", "Disminución Moderada"),
      "Variación Fuerte" = c("Aumento Fuerte", "Disminución Fuerte")
    )
  ) %>%
  as("transactions")

# Generar reglas
reglas.precios.modificadas <- sort(arules::apriori(data = transacciones.precios.modificadas,
                                                   parameter = list(support = 0.02, confidence = 0.6, target = "rules", maxlen = 20)),
                                   by = "confidence", decreasing = TRUE) %>%
  subset(x = ., subset = (rhs %pin% 'DV3') & ((lhs %pin% 'zona') | (lhs %pin% 'comercio') | (lhs %pin% 'tipo')) & (lift > 2) & (confidence < 1))
arules::inspect(head(reglas.precios.modificadas, 30))
plot(reglas.precios.modificadas, method = "matrix", measure = "confidence")
reglas.precios.df <- ReglasADataFrame(reglas.precios.modificadas)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- V. Reglas de asociación para predicciones ----                            
# ---------------------------------------------------------------------------------------#

# Generar transacciones asociadas a bebidas
transacciones.prediccion.precios <- datos.consolidados %>%
  dplyr::select(zona, comercio, tipo, DPR1, DPR2, DPR3, DPR4) %>%
  as("transactions")

# Generar reglas
reglas.prediccion.precios <- sort(arules::apriori(data = transacciones.prediccion.precios,
                                       parameter = list(support = 0.02, confidence = 0.6, target = "rules", maxlen = 20)),
                       by = "confidence", decreasing = TRUE) %>%
  subset(x = ., subset = (rhs %pin% 'DPR4') & (lift > 2))
arules::inspect(head(reglas.prediccion.precios, 30))
plot(reglas.precios, method = "matrix", measure = "confidence")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- VI. Validacion de precios usando analisis de correspondencia ----                            
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

# Analisis de correspondencia simple de nivel de precios vs. comuna
matriz.acs.nivel.precios.comuna <- nivel.precios.comuna.general %>%
  dplyr::select(-centro_x, -centro_y, -geometry) %>%
  as.data.frame()
rownames(matriz.acs.nivel.precios.comuna) <- as.character(dplyr::pull(matriz.acs.nivel.precios.comuna, comuna))
matriz.acs.nivel.precios.comuna %<>% dplyr::select(-comuna)
ca.nivel.precios.comuna <- ca::ca(matriz.acs.nivel.precios.comuna, graph = FALSE)

#factoextra::fviz_contrib(ca.nivel.precios.comuna, choice = "row", axes = 1)
#factoextra::fviz_contrib(ca.nivel.precios.comuna, choice = "col", axes = 1)
grafico.ca.nivel.precios.comuna <- factoextra::fviz_ca_biplot(ca.nivel.precios.comuna, repel = TRUE) +
  ggplot2::labs(x = sprintf("Dimensión 1 (%.2f%%)", 100*ca.nivel.precios.comuna$sv[1]^2/sum(ca.nivel.precios.comuna$sv^2)), 
                y = sprintf("Dimensión 2 (%.2f%%)", 100*ca.nivel.precios.comuna$sv[2]^2/sum(ca.nivel.precios.comuna$sv^2)), fill = "",
                subtitle = "Análisis de correspondencia", title = "Variación de precios por comuna") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )

# iii Por comercio
nivel.precios.comercio <- nivel.precios %>%
  dplyr::inner_join(sf::st_set_geometry(sucursales, NULL), by = c("comercioId", "banderaId", "sucursalId")) %>%
  dplyr::group_by(comercioId, banderaId, periodo) %>%
  dplyr::mutate(total = dplyr::n()) %>%
  dplyr::group_by(comercioId, banderaId, periodo, nivelPrecio, total) %>%
  dplyr::summarise(cantidad = dplyr::n()) %>%
  dplyr::mutate(porcentaje = 100 * cantidad / total) %>%
  dplyr::inner_join(banderas, NULL, by = c("comercioId", "banderaId")) %>%
  dplyr::inner_join(comercios.cortos, NULL, by = c("comercioId")) %>%
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
grafico.ca.nivel.precios.comercio <- factoextra::fviz_ca_biplot(ca.nivel.precios.comercio, repel = TRUE) +
  ggplot2::labs(x = sprintf("Dimensión 1 (%.2f%%)", 100*ca.nivel.precios.comercio$sv[1]^2/sum(ca.nivel.precios.comercio$sv^2)), 
                y = sprintf("Dimensión 2 (%.2f%%)", 100*ca.nivel.precios.comercio$sv[2]^2/sum(ca.nivel.precios.comercio$sv^2)), fill = "",
                subtitle = "Análisis de correspondencia", title = "Variación de precios por comercio") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- ??. Almacenar resultados ----                            
# ---------------------------------------------------------------------------------------#

save(grafico.regiones, grafico.porcentaje.datos.zona, grafico.porcentaje.datos.comercio,
     grafico.ca.nivel.precios.comercio, grafico.ca.nivel.precios.comuna,
     file = "output/Resultados.RData")
# ----------------------------------------------------------------------------------------