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
require(igraph)
require(magrittr)
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
  igraph::graph.adjacency(matriz.adyacencia, mode = "undirected", diag = FALSE)
}

# Obtener grafos para distintas densidades segun codigo subido por profesores
ObtenerGrafosDensidades <- function(matriz.correlacion, estadio, sujeto, densidades) {
  dimensiones   <- dim(matriz.correlacion)
  max.links     <- (dimensiones[1] * (dimensiones[2] - 1))/2
  correlaciones <- sort(matriz.correlacion[lower.tri(matriz.correlacion)], decreasing = TRUE)
  grafos        <- purrr::map_dfr(
    .x = densidades,
    .f = function(densidad) {
      idx    <- as.integer(densidad * max.links)
      umbral <- correlaciones[idx]
      grafo  <- ObtenerGrafoBinario(matriz.correlacion, umbral)
      return (dplyr::tibble(
        estadio = estadio,
        sujeto = sujeto,
        densidad = densidad,
        grafo = list(grafo)
      ))
    }
  )
}


if (file.exists("output/Grafos.RData")) {
  load(file = "output/Grafos.RData")
} else {
  # Para cada individuo, estado de sueño y umbral, obtener un grafo
  grafos <- purrr::pmap_dfr(
    .l = dplyr::distinct(datos.entrada, estadio, sujeto),
    .f = function(estadio, sujeto) {
      matriz.correlacion <- datos.entrada %>%
        dplyr::filter(estadio == !! estadio & sujeto == !! sujeto) %>%
        dplyr::select(-estadio, -sujeto) %>%
        as.matrix()
      
      # Generar grafos para distintas densidades
      densidades <- seq(from = 0, to = 0.5, by = 0.005)
      return (ObtenerGrafosDensidades(matriz.correlacion, estadio, sujeto, densidades))
    }
  )

  # Guardar grafos a un archivo RData
  save(grafos, file = "output/Grafos.RData")
}
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

if (file.exists("output/Metricas.RData")) {
  load(file = "output/Metricas.RData")
} else {
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
    tidyr::pivot_longer(cols = c(-estadio, -sujeto, -densidad), 
                        names_to = "metrica", values_to = "valor") %>%
    dplyr::mutate(valor = unlist(valor)) %>%
    dplyr::group_by(estadio, metrica, densidad) %>%
    dplyr::summarise(media = mean(valor, na.rm = TRUE),
                     desvio = sd(valor, na.rm = TRUE))
  
  # Guardar metricas y estadisticas a archivo RData
  save(metricas, estadisticas, file = "output/Metricas.RData")
}

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
  ggplot2::geom_errorbar(mapping = ggplot2::aes(x = densidad, ymin = media - desvio, ymax = media + desvio, col = estadio)) +
  ggplot2::facet_wrap(~metrica, scales = "free", ncol = 2, labeller = ggplot2::labeller(metrica = etiquetas_metricas)) +
  ggplot2::labs(x = "Densidad de aristas", y = "Valor de la métrica", col = "Estadío de sueño",
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

if (file.exists("output/MetricasLouvain.RData")) {
  load(file = "output/MetricasLouvain.RData")
} else {
  # Calcular metricas pedidas
  metricas.louvain <- purrr::pmap_dfr(
    .l = dplyr::distinct(grafos, estadio, sujeto, densidad),
    .f = function(estadio, sujeto, densidad) {
      grafo <- grafos %>%
        dplyr::filter(estadio == !! estadio & sujeto == !! sujeto & densidad == !! densidad) %>%
        dplyr::select(densidad, grafo)
      grafo.original <- grafo[1, ]$grafo[[1]]
      densidad       <- grafo[1, ]$densidad
      
      # Obtengo las comunidades con el algoritmo de Louvain.
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
        data.frame(estadio = estadio, sujeto = sujeto, grafo = 'Original', densidad = densidad,
                   modularidad = modularidad.original, numero_comunidades = numero.com.original),
        data.frame(estadio = estadio, sujeto = sujeto, grafo = 'Random', densidad = densidad,
                   modularidad = modularidad.random, numero_comunidades = numero.com.random)
      )) 
    }
  ) %>% tidyr::pivot_longer(names_to = "metrica", values_to = "valor", cols = c("modularidad", "numero_comunidades"))
  
  # Calcular estadisticas
  estadisticas.louvain <- metricas.louvain %>%
    dplyr::group_by(estadio, densidad, grafo, metrica) %>%
    dplyr::summarise(media = mean(valor, na.rm = TRUE),
                     desvio = sd(valor, na.rm = TRUE))
  
  # Guardar metricas y estadisticas a archivo RData
  save(metricas.louvain, estadisticas.louvain, file = "output/MetricasLouvain.RData")
}
  
# Etiquetas para metricas
etiquetas_metricas_louvain <- c(
  modularidad = "Modularidad",
  numero_comunidades = "Número de comunidades"
)
 
# Generar grafico comparativo
ggplot2::ggplot(data = estadisticas.louvain) +
  ggplot2::geom_errorbar(mapping = ggplot2::aes(x = densidad, ymin = media - desvio, ymax = media + desvio, col = grafo)) +
  ggplot2::facet_grid(metrica~estadio, scales = "free", labeller = ggplot2::labeller(metrica = etiquetas_metricas_louvain)) +
  ggplot2::labs(x = "Densidad de aristas", y = "Valor de la métrica", col = "Tipo de grafo",
                title = "Métricas de modularidad para grafos según de densidad de corte",
                subtitle = "Valores medios y desvíos") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_text(angle = 90)
  )
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- Opcional 1 (Tarea 2) ----                            
# ---------------------------------------------------------------------------------------#

if (file.exists("output/MetricasClausetNewmanMoore.RData")) {
  load(file = "output/MetricasClausetNewmanMoore.RData")
} else {
  # Calcular metricas pedidas
  progress.bar                  <- txtProgressBar(min = 0, max = nrow(grafos))
  metricas.clauset.newman.moore <- NULL
  for (nivel.progreso in seq(from = 1, to = nrow(grafos))) {
    estadio        <- grafos[nivel.progreso, ]$estadio
    sujeto         <- grafos[nivel.progreso, ]$sujeto
    densidad       <- grafos[nivel.progreso, ]$densidad
    grafo.original <- as.list(grafos[nivel.progreso, ]$grafo)[[1]]
      
    # Obtengo las comunidades con el algoritmo de Clauset-Newman-Moore
    # Luego calculo la modularidad y la cantidad de comunidades.
    comunidad.original   <- igraph::cluster_fast_greedy(grafo.original)
    modularidad.original <- igraph::modularity(grafo.original, comunidad.original$membership)
    numero.com.original  <- length(unique(comunidad.original$membership))
    
    # Incrementar barra de progreso
    setTxtProgressBar(progress.bar, value = nivel.progreso)

    if (! is.null(metricas.clauset.newman.moore)) {
      metricas.clauset.newman.moore <- rbind(
        metricas.clauset.newman.moore,
        (data.frame(estadio = estadio, sujeto = sujeto, grafo = 'Original', densidad = densidad,
                    modularidad = modularidad.original, numero_comunidades = numero.com.original))
      )
    } else {
      metricas.clauset.newman.moore <- (data.frame(estadio = estadio, sujeto = sujeto, grafo = 'Original', densidad = densidad,
                                                   modularidad = modularidad.original, numero_comunidades = numero.com.original))
    }
  }
  metricas.clauset.newman.moore %<>% 
    tidyr::pivot_longer(names_to = "metrica", values_to = "valor", cols = c("modularidad", "numero_comunidades"))
  
  # Calcular estadisticas
  estadisticas.clauset.newman.moore <- metricas.clauset.newman.moore %>%
    dplyr::group_by(estadio, densidad, grafo, metrica) %>%
    dplyr::summarise(media = mean(valor, na.rm = TRUE),
                     desvio = sd(valor, na.rm = TRUE))
  
  # Guardar metricas y estadisticas a archivo RData
  save(metricas.clauset.newman.moore, estadisticas.clauset.newman.moore, file = "output/MetricasClausetNewmanMoore.RData")
}

# Etiquetas para metricas
etiquetas_metricas_clauset_newman_moore <- c(
  modularidad = "Modularidad",
  numero_comunidades = "Número de comunidades"
)

# Generar grafico comparativo entre Louvain y Clauset-Newman-Moore
estadisticas.louvain.clauset <- dplyr::bind_rows(
  estadisticas.louvain %>%
    dplyr::ungroup() %>%
    dplyr::filter(grafo == "Original") %>%
    dplyr::mutate(algoritmo = "Louvain") %>%
    dplyr::select(-grafo),
  estadisticas.clauset.newman.moore %>%
    dplyr::ungroup() %>%
    dplyr::mutate(algoritmo = "Clauset-Girvan-Moore") %>%
    dplyr::select(-grafo)
)
ggplot2::ggplot(data = estadisticas.louvain.clauset) +
  ggplot2::geom_errorbar(mapping = ggplot2::aes(x = densidad, ymin = media - desvio, ymax = media + desvio, col = algoritmo)) +
  ggplot2::facet_grid(metrica~estadio, scales = "free", labeller = ggplot2::labeller(metrica = etiquetas_metricas_louvain)) +
  ggplot2::labs(x = "Densidad de aristas", y = "Valor de la métrica", col = "Algoritmo",
                title = "Métricas de modularidad para grafos según de densidad de corte",
                subtitle = "Valores medios y desvíos") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_text(angle = 90)
  )
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- Tarea 3 ----                            
# ---------------------------------------------------------------------------------------#

modularidad <- metricas.louvain %>%
  dplyr::filter(grafo == "Original" & metrica == "modularidad") %>%
  dplyr::select(-grafo, -metrica) %>%
  tidyr::pivot_wider(names_from = "estadio", values_from = "valor") %>%
  dplyr::mutate(N1 = N1 - W, N2 = N2 - W, N3 = N3 - W) %>%
  dplyr::select(-W) %>%
  dplyr::group_by(densidad) %>%
  dplyr::summarise(
    p_valor_N1 = t.test(x = unlist(N1), alternative = "greater")$p.value,
    p_valor_N2 = t.test(x = unlist(N2), alternative = "greater")$p.value,
    p_valor_N3 = t.test(x = unlist(N3), alternative = "greater")$p.value
  ) %>%
  dplyr::select(densidad, p_valor_N1, p_valor_N2, p_valor_N3)

t.tests <-  metricas.louvain %>%
  dplyr::filter(grafo == "Original") %>%
  dplyr::select(-grafo, -sujeto) %>%
  dplyr::group_by(densidad, estadio, metrica) %>%
  dplyr::summarise(valores = list(valor)) %>%
  tidyr::pivot_wider(names_from = "estadio", values_from = "valores") %>%
  dplyr::mutate(
    p_valor_N1 = t.test(x = unlist(N1), y = unlist(W), paired = FALSE)$p.value,
    p_valor_N2 = t.test(x = unlist(N2), y = unlist(W), paired = FALSE)$p.value,
    p_valor_N3 = t.test(x = unlist(N3), y = unlist(W), paired = FALSE)$p.value
  ) %>%
  dplyr::select(densidad, metrica, p_valor_N1, p_valor_N2, p_valor_N3)

# Generar grafico comparativo
datos.grafico.comparacion.estadios <- estadisticas.louvain %>%
  dplyr::filter(grafo == "Original") %>%
  dplyr::select(-grafo)
ggplot2::ggplot() +
  ggplot2::geom_errorbar(
    data = dplyr::filter(datos.grafico.comparacion.estadios, estadio %in% c('N1', 'W')),
    mapping = ggplot2::aes(x = densidad, ymin = media - desvio, ymax = media + desvio, col = estadio)
  ) +
  ggplot2::facet_wrap(~metrica, scales = "free", ncol = 1,
                      labeller = ggplot2::labeller(metrica = etiquetas_metricas_louvain)) +
  ggplot2::labs(x = "Densidad de aristas", y = "Valor de la métrica", col = "Tipo de grafo",
                title = "Métricas de modularidad para grafos según de densidad de corte",
                subtitle = "Valores medios y desvíos") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_text(angle = 90)
  )
ggplot2::ggplot() +
  ggplot2::geom_errorbar(
    data = dplyr::filter(datos.grafico.comparacion.estadios, estadio %in% c('N2', 'W')),
    mapping = ggplot2::aes(x = densidad, ymin = media - desvio, ymax = media + desvio, col = estadio)
  ) +
  ggplot2::facet_wrap(~metrica, scales = "free", ncol = 1,
                      labeller = ggplot2::labeller(metrica = etiquetas_metricas_louvain)) +
  ggplot2::labs(x = "Densidad de aristas", y = "Valor de la métrica", col = "Tipo de grafo",
                title = "Métricas de modularidad para grafos según de densidad de corte",
                subtitle = "Valores medios y desvíos") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_text(angle = 90)
  )
ggplot2::ggplot() +
  ggplot2::geom_errorbar(
    data = dplyr::filter(datos.grafico.comparacion.estadios, estadio %in% c('N3', 'W')),
    mapping = ggplot2::aes(x = densidad, ymin = media - desvio, ymax = media + desvio, col = estadio)
  ) +
  ggplot2::facet_wrap(~metrica, scales = "free", ncol = 1,
                      labeller = ggplot2::labeller(metrica = etiquetas_metricas_louvain)) +
  ggplot2::labs(x = "Densidad de aristas", y = "Valor de la métrica", col = "Tipo de grafo",
                title = "Métricas de modularidad para grafos según de densidad de corte",
                subtitle = "Valores medios y desvíos") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_text(angle = 90)
  )
# ----------------------------------------------------------------------------------------