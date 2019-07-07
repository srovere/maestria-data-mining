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
    legend.text = ggplot2::element_text(size = 8),
    text = ggplot2::element_text(size = 8),
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
    legend.text = ggplot2::element_text(size = 8),
    text = ggplot2::element_text(size = 8),
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
    legend.text = ggplot2::element_text(size = 8),
    text = ggplot2::element_text(size = 8),
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

# Exploracion de reglas generales
reglas.generales <- subset(reglas,
                      subset = ((lhs %pin% "tipo") | (rhs %pin% "tipo") | 
                                  (lhs %pin% "comercio") | (rhs %pin% "comercio") |
                                  (lhs %pin% "zona") | (rhs %pin% "zona")) & (lift > 1.5)) %>%
  ReglasADataFrame()
reglas.generales.seleccionadas <- reglas.generales[c(5, 11, 23, 37, 41, 63, 71),]
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Reglas de asociación para variaciones de precios ----                            
# ---------------------------------------------------------------------------------------#

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
                title = "Evolución de precios para cada interperíodo") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    legend.text = ggplot2::element_text(size = 8),
    text = ggplot2::element_text(size = 9),
    axis.text = ggplot2::element_text(size = 9),
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = 20 , label.position = "right"))
  
# Analisis de correspondencia
ca.periodos.variacion.precios <- ca::ca(periodos.variacion.precios, graph = FALSE)
factoextra::fviz_contrib(ca.periodos.variacion.precios, choice = "row", axes = 1)
factoextra::fviz_contrib(ca.periodos.variacion.precios, choice = "col", axes = 1)
grafico.ca.periodos.variacion <- factoextra::fviz_ca_biplot(ca.periodos.variacion.precios, repel = TRUE) +
  ggplot2::labs(x = sprintf("Dimensión 1 (%.2f%%)", 100*ca.periodos.variacion.precios$sv[1]^2/sum(ca.periodos.variacion.precios$sv^2)), 
                y = sprintf("Dimensión 2 (%.2f%%)", 100*ca.periodos.variacion.precios$sv[2]^2/sum(ca.periodos.variacion.precios$sv^2)), fill = "",
                title = "Análisis de correspondencia de variación de precios por interperíodo") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    legend.text = ggplot2::element_text(size = 8),
    text = ggplot2::element_text(size = 9),
    axis.text = ggplot2::element_text(size = 8),
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )

# Reglas de asociacion a partir de categorias de varacion de precios originales
transacciones.precios <- datos.consolidados %>%
  dplyr::select(zona, comercio, DV1, DV2, DV3) %>%
  as("transactions")

# Generar reglas
reglas.precios <- sort(arules::apriori(data = transacciones.precios,
                                       parameter = list(support = 0.02, confidence = 0.6, target = "rules", maxlen = 7)),
                       by = "confidence", decreasing = TRUE) %>%
  subset(x = ., subset = (rhs %pin% 'DV3') & ((lhs %pin% 'DV1') | (lhs %pin% 'DV2')) & (lift > 1.5) & (confidence < 1)) %>%
  ReglasADataFrame()
reglas.precios.seleccionadas <- reglas.precios[c(1, 2, 7, 11, 12, 17),]
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- IV. Reglas de asociación para productos ----                            
# ---------------------------------------------------------------------------------------#

# Seleccionar productos de interes: gaseosas, cervezas y vinos.
terminos       <- paste0("termino_", c("cerveza", "vino", "gaseosa"))
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
                                       parameter = list(support = 0.02, confidence = 0.7, target = "rules", maxlen = 20)),
                       by = "confidence", decreasing = TRUE) %>%
  subset(x = ., subset = ((lhs %pin% "termino")) & (lift > 1.5)) %>%
  ReglasADataFrame()
reglas.vino    <- dplyr::filter(reglas.bebidas, ! is.na(stringr::str_match(lhs, 'vino')[,1]))
reglas.gaseosa <- dplyr::filter(reglas.bebidas, ! is.na(stringr::str_match(lhs, 'gaseosa')[,1]))
reglas.cerveza <- dplyr::filter(reglas.bebidas, ! is.na(stringr::str_match(lhs, 'cerveza')[,1]))

reglas.bebidas.seleccionadas <- reglas.vino[c(7,8,16,17,25),] %>%
  dplyr::bind_rows(reglas.gaseosa[c(1,2,3),]) %>%
  dplyr::bind_rows(reglas.cerveza) %>%
  dplyr::arrange(dplyr::desc(confidence), dplyr::desc(lift), dplyr::desc(support))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- V. Reglas de asociación para predicciones ----                            
# ---------------------------------------------------------------------------------------#

# Generar transacciones asociadas primeros 3 periodos y verificar si podemos encontrar las 
# mismas reglas en el 4 periodo y que metricas tienen las mismas.
reglas.periodos <- purrr::map_dfr(
  .x = seq(from = 1, to = 4),
  .f = function(periodo) {
    precio.periodo <- paste0("DPR", periodo)
    transacciones.periodo <- datos.consolidados %>%
      dplyr::select(zona, comercio, tipo, !! precio.periodo, DPRT, DVT) %>%
      dplyr::rename(DPRPer = !! precio.periodo) %>%
      as("transactions")  
    reglas.periodo <- sort(arules::apriori(data = transacciones.periodo,
                                           parameter = list(support = 0.02, confidence = 0.7, target = "rules", maxlen = 20)),
                                      by = "confidence", decreasing = TRUE) %>%
      subset(x = ., subset = ((lhs %pin% "DPRPer") | (rhs %pin% "DPRPer")) & (lift > 1.5)) %>%
      ReglasADataFrame(.) %>%
      dplyr::mutate(periodo = periodo)
    return (reglas.periodo)
  }
)
  
# Buscar las reglas de los periodos 1 a 3 que esten tambien en el periodo 4
reglas.comunes <- dplyr::filter(reglas.periodos, periodo == 4)
purrr::walk(
  .x = seq(from = 1, to = 3),
  .f = function(periodo) {
    reglas.periodo <- dplyr::filter(reglas.periodos, periodo == !! periodo) %>%
      dplyr::select(lhs, rhs)
    reglas.comunes <<- reglas.comunes %>%
      dplyr::inner_join(reglas.periodo, by = c("lhs", "rhs"))
  }
)

# Seleccion de reglas y agregado de metricas
reglas.prediccion.seleccionadas <- reglas.comunes[c(4, 9, 13, 15, 16, 17, 20), ] %>%
  dplyr::select(lhs, rhs) %>%
  dplyr::inner_join(reglas.periodos, by = c("lhs", "rhs")) %>%
  dplyr::mutate(regla = paste0(lhs, " => ", rhs)) %>%
  dplyr::select(-count) %>%
  tidyr::gather(key = metrica, value = valor, -lhs, -rhs, -regla, -periodo)

# Grafico de evolucion de metricas
grafico.evolucion.metricas <- ggplot2::ggplot(data = reglas.prediccion.seleccionadas) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = periodo, y = valor, col = regla)) +
  ggplot2::scale_colour_brewer(type = "qual", palette = "Paired") +
  ggplot2::labs(x = "Período", y = "Valor", col = "") +
  ggplot2::facet_wrap(~ metrica, scales = "free", nrow = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- VI. Validacion de precios usando analisis de correspondencia ----                            
# ---------------------------------------------------------------------------------------#

# Redefinicion de nombres de unidades de negocio a fin de mejorar legibilidad de graficos
unidades.negocio <- c("Deheza", "Lima", "Vea", "Disco", "Jumbo", "Carrefour H", "Carrefour M", 
                      "Carrefour E", "Changomas", "Walmart SC", "Coto", "Dia", "Full", "Axion", "Josimar")
banderas         <- banderas %>%
  dplyr::mutate(unidad_negocio = unidades.negocio)

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
    legend.text = ggplot2::element_text(size = 8),
    text = ggplot2::element_text(size = 9),
    axis.text = ggplot2::element_text(size = 8),
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
  dplyr::mutate(comercio = unidad_negocio) %>%
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
    legend.text = ggplot2::element_text(size = 8),
    text = ggplot2::element_text(size = 9),
    axis.text = ggplot2::element_text(size = 8),
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- VII. Almacenar resultados ----                            
# ---------------------------------------------------------------------------------------#

save(grafico.regiones, grafico.porcentaje.datos.zona, grafico.porcentaje.datos.comercio,
     grafico.ca.nivel.precios.comercio, grafico.ca.nivel.precios.comuna,
     grafico.evolucion.metricas, grafico.ca.periodos.variacion, grafico.periodo.variacion,
     reglas.generales.seleccionadas, reglas.precios.seleccionadas,
     reglas.bebidas.seleccionadas,
     file = "output/Resultados.RData")
# ----------------------------------------------------------------------------------------