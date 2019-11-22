# ---------------------------------------------------------------------------------------#
# ---- Script para integración de set de datos para TP2 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(Cairo)
require(caret)
require(dplyr)
require(ggplot2)
require(ggbiplot)
require(GGally)
require(magrittr)
require(MASS)
require(purrr)
require(readr)
require(stringr)
require(tibble)
require(tidyr)

options(bitmapType = "cairo")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- II. Lectura de archivos ----                            
# ---------------------------------------------------------------------------------------#

archivo.rdata <- paste0("input/EstadiosSueno.RData")
if (! file.exists(archivo.rdata)) {
  # i. Buscar archivos de entrada
  archivos.entrada <- base::list.files(path = "input/DataSujetos", pattern = "*.csv") %>%
    stringr::str_match(string = ., pattern = "(\\w+)_suj(\\d+)") %>%
    as.data.frame() %>%
    dplyr::mutate(V1 = paste0(V1, ".csv"))
  colnames(archivos.entrada) <- c("archivo", "estadio", "sujeto")
  
  # ii. Leer contenido de archivos
  datos.entrada <- purrr::pmap_dfr(
    .l = archivos.entrada,
    .f = function(archivo, estadio, sujeto) {
      datos.archivo <- readr::read_csv(file = paste0("input/DataSujetos/", archivo), col_names = FALSE) %>%
        dplyr::mutate(estadio = estadio, sujeto = sujeto)
      return (datos.archivo)
    }
  )
  save(datos.entrada, file = archivo.rdata)
  rm(archivos.entrada)
} else {
  load(archivo.rdata)  
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Generar grafos binarios para distintos puntos de corte ----                            
# ---------------------------------------------------------------------------------------#

# Definir función para generar un grafo binario a partir de uno pesado
ObtenerGrafoBinario <- function(matriz.correlacion, punto.corte) {
  # Creamos una matriz de adyacencia de la misma dimension que la
  # matriz de correlacion. Inicializamos con ceros (sin conexion)
  matriz.adyacencia <- matrix(data = 0, nrow = nrow(matriz.correlacion),
                              ncol = ncol(matriz.correlacion))
  
  # A aquellas aritas cuyo peso supere el umbral, le asignamos un 1
  matriz.adyacencia[which(matriz.correlacion >= punto.corte)] <- 1
  
  # Se crea el grafo a partir de la matriz de adyacencia.
  # Se eliminan los 1s de la diagonal para que no haya loops
  graph.adjacency(matriz.adyacencia, mode = "undirected", diag = FALSE)
}

# Para cada individuo, estado de sueño y umbral, obtener un grafo
grafos <- purrr::pmap_dfr(
  .l = dplyr::distinct(datos.entrada, estadio, sujeto),
  .f = function(estadio, sujeto) {
    matriz.correlacion <- datos.entrada %>%
      dplyr::filter(estadio == !! estadio & sujeto == !! sujeto) %>%
      dplyr::select(-estadio, -sujeto) %>%
      as.matrix()
    
    # Generar grafos para distintos umbrales
    grafos.estadio.sujeto <- purrr::map_dfr(
      .x = seq(from = 0, to = 1, by = 0.01),
      .f = function(punto.corte) {
        return(dplyr::tibble(
          estadio = estadio,
          sujeto = sujeto,
          punto_corte = punto.corte,
          grafo = list(ObtenerGrafoBinario(matriz.correlacion, punto.corte))
        ))
      }    
    )
  }
)

# Guardar grafos a un archivo RData
save(grafos, file = "output/Grafos.RData")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- Tarea 1 ----                            
# ---------------------------------------------------------------------------------------#

# Para cada grafo, obtener:
# - Grado medio
# - Excentricidad promedio
# - Cercania (closeness) promedio
# - Intermediacion (betweeness) promedio
# - Centralidad de autovector promedio
# - Longitud caracteristica
# - Coeficiente de clustering

metricas <- grafos %>%
  dplyr::mutate(
    grado = purrr::map(grafo, ~mean(igraph::degree(.x))),
    excentricidad = purrr::map(grafo, ~mean(igraph::eccentricity(.x))),
    cercania = purrr::map(grafo, ~mean(igraph::closeness(.x))),
    intermediacion = purrr::map(grafo, ~mean(igraph::betweenness(.x))),
    autovector = purrr::map(grafo, ~mean(igraph::eigen_centrality(.x)[["vector"]])),
    longitud_caracteristica = purrr::map(grafo, ~igraph::mean_distance(.x)),
    coeficiente_clustering = purrr::map(grafo, ~igraph::transitivity(.x, type = "average"))
  ) %>% dplyr::select(-grafo)

# Para cada estadio, metrica y punto de corte, calcular media y desvio de cada metrica
estadisticas <- metricas %>%
  tidyr::pivot_longer(cols = c(-estadio, -sujeto, -punto_corte), names_to = "metrica", values_to = "valor") %>%
  dplyr::mutate(valor = unlist(valor)) %>%
  dplyr::group_by(estadio, metrica, punto_corte) %>%
  dplyr::summarise(media = mean(valor, na.rm = TRUE),
                   desvio = sd(valor, na.rm = TRUE))

# Guardar metricas y estadisticas a archivo RData
save(metricas, estadisticas, file = "output/Metricas.RData")

# Etiquetas para metricas
etiquetas_metricas <- c(
  grado = "Grado",
  excentricidad = "Excentricidad",
  cercania = "Cercanía",
  intermediacion = "Intermediación",
  autovector = "Centralización de autovector",
  longitud_caracteristica = "Longitud característica",
  coeficiente_clustering = "Coeficiente de clustering"
)

# Generar grafico
ggplot2::ggplot(data = estadisticas) +
  ggplot2::geom_errorbar(mapping = ggplot2::aes(x = punto_corte, ymin = media - desvio, ymax = media + desvio, col = estadio)) +
  ggplot2::facet_wrap(~metrica, scales = "free", ncol = 2, labeller = ggplot2::labeller(metrica = etiquetas_metricas)) +
  ggplot2::labs(x = "Densidad de corte", y = "Valor de la métrica", col = "Estadío de sueño",
                title = "Métricas de grafos según de densidad de corte",
                subtitle = "Valores medios y desvíos") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- Tarea 2 ----                            
# ---------------------------------------------------------------------------------------#

# Calcular metricas pedidas
metricas.louvain <- purrr::pmap_dfr(
  .l = dplyr::distinct(grafos, estadio, sujeto, punto_corte),
  .f = function(estadio, sujeto, punto_corte) {
    grafo.original <- grafos %>%
      dplyr::filter(estadio == !! estadio & sujeto == !! sujeto & punto_corte == punto_corte) %>%
      dplyr::pull(grafo)
    grafo.original <- grafo.original[[1]]
    
    # Obtendo las comunidades con el algoritmo de Louvain.
    # Luego calculo la modularidad y la cantidad de comunidades.
    comunidad.original   <- igraph::cluster_louvain(grafo.original)  
    modularidad.original <- igraph::modularity(grafo.original, comunidad.original$membership)
    numero.com.original  <- length(unique(comunidad.original$membership))
    
    # Ahora genero una red random con la misma distribucion de grado que el grafo inicial.
    # Obntengo las comunidades y calculo modularidad y cantidad de comunidades.
    grafo.random       <- igraph::sample_degseq(out.deg = igraph::degree(grafo.original))
    comunidad.random   <- igraph::cluster_louvain(grafo.random)  
    modularidad.random <- igraph::modularity(grafo.random, comunidad.random$membership)
    numero.com.random  <- length(unique(comunidad.random$membership))
    
    return (dplyr::bind_rows(
      data.frame(estadio = estadio, sujeto = sujeto, punto_corte = punto_corte, grafo = 'Original',
                 modularidad = modularidad.original, numero_comunidades = numero.com.original),
      data.frame(estadio = estadio, sujeto = sujeto, punto_corte = punto_corte, grafo = 'Random',
                 modularidad = modularidad.random, numero_comunidades = numero.com.random)
    )) 
  }
) %>% tidyr::pivot_longer(names_to = "metrica", values_to = "valor", cols = c("modularidad", "numero_comunidades"))

# Calcular estadisticas
estadisticas.louvain <- metricas.louvain %>%
  dplyr::group_by(estadio, punto_corte, grafo, metrica) %>%
  dplyr::summarise(media = mean(valor, na.rm = TRUE),
                   desvio = sd(valor, na.rm = TRUE))

# Guardar metricas y estadisticas a archivo RData
save(metricas.louvain, estadisticas.louvain, file = "output/MetricasLouvain.RData")

# Etiquetas para metricas
etiquetas_metricas_louvain <- c(
  modularidad = "Modularidad",
  numero_comunidades = "Número de comunidades"
)
 
# Generar grafico comparativo
ggplot2::ggplot(data = estadisticas.louvain) +
  ggplot2::geom_errorbar(mapping = ggplot2::aes(x = punto_corte, ymin = media - desvio, ymax = media + desvio, col = grafo)) +
  ggplot2::facet_grid(metrica~estadio, scales = "free", labeller = ggplot2::labeller(metrica = etiquetas_metricas_louvain)) +
  ggplot2::labs(x = "Densidad de corte", y = "Valor de la métrica", col = "Tipo de grafo",
                title = "Métricas de modularidad para grafos según de densidad de corte",
                subtitle = "Valores medios y desvíos") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- Opcional 1 (Tarea 2) ----                            
# ---------------------------------------------------------------------------------------#

# Calcular metricas pedidas
metricas.girvan.newman <- purrr::pmap_dfr(
  .l = dplyr::distinct(grafos, estadio, sujeto, punto_corte),
  .f = function(estadio, sujeto, punto_corte) {
    grafo.original <- grafos %>%
      dplyr::filter(estadio == !! estadio & sujeto == !! sujeto & punto_corte == punto_corte) %>%
      dplyr::pull(grafo)
    grafo.original <- grafo.original[[1]]
    
    # Obtendo las comunidades con el algoritmo de Louvain.
    # Luego calculo la modularidad y la cantidad de comunidades.
    comunidad.original   <- igraph::cluster_edge_betweenness(grafo.original)
    modularidad.original <- igraph::modularity(grafo.original, comunidad.original$membership)
    numero.com.original  <- length(unique(comunidad.original$membership))
    
    # Ahora genero una red random con la misma distribucion de grado que el grafo inicial.
    # Obntengo las comunidades y calculo modularidad y cantidad de comunidades.
    grafo.random       <- igraph::sample_degseq(out.deg = igraph::degree(grafo.original))
    comunidad.random   <- igraph::cluster_louvain(grafo.random)  
    modularidad.random <- igraph::modularity(grafo.random, comunidad.random$membership)
    numero.com.random  <- length(unique(comunidad.random$membership))
    
    return (dplyr::bind_rows(
      data.frame(estadio = estadio, sujeto = sujeto, punto_corte = punto_corte, grafo = 'Original',
                 modularidad = modularidad.original, numero_comunidades = numero.com.original),
      data.frame(estadio = estadio, sujeto = sujeto, punto_corte = punto_corte, grafo = 'Random',
                 modularidad = modularidad.random, numero_comunidades = numero.com.random)
    )) 
  }
) %>% tidyr::pivot_longer(names_to = "metrica", values_to = "valor", cols = c("modularidad", "numero_comunidades"))

# Calcular estadisticas
estadisticas.girvan.newman <- metricas.girvan.newman %>%
  dplyr::group_by(estadio, punto_corte, grafo, metrica) %>%
  dplyr::summarise(media = mean(valor, na.rm = TRUE),
                   desvio = sd(valor, na.rm = TRUE))

# Guardar metricas y estadisticas a archivo RData
save(metricas.girvan.newman, estadisticas.girvan.newman, file = "output/MetricasGirvanNewman.RData")

# Etiquetas para metricas
etiquetas_metricas_girvan_newman <- c(
  modularidad = "Modularidad",
  numero_comunidades = "Número de comunidades"
)

# Generar grafico comparativo
ggplot2::ggplot(data = estadisticas.girvan.newman) +
  ggplot2::geom_errorbar(mapping = ggplot2::aes(x = punto_corte, ymin = media - desvio, ymax = media + desvio, col = grafo)) +
  ggplot2::facet_grid(metrica~estadio, scales = "free", labeller = ggplot2::labeller(metrica = etiquetas_metricas_girvan_newman)) +
  ggplot2::labs(x = "Densidad de corte", y = "Valor de la métrica", col = "Tipo de grafo",
                title = "Métricas de modularidad para grafos según de densidad de corte",
                subtitle = "Valores medios y desvíos") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
# ----------------------------------------------------------------------------------------
