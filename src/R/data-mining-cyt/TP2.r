# ---------------------------------------------------------------------------------------#
# ---- Script para integración de set de datos para TP2 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

list.of.packages <- c("Cairo", "caret", "doSNOW", "dplyr", "foreach", "ggplot2", "gtools", 
                      "igraph", "iterators", "magrittr", "mclust", "purrr", "readr", "snow", 
                      "stringr", "tibble", "tidyr")
for (pack in list.of.packages) {
  if (!require(pack, character.only = TRUE)) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}

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
  
  # Grafos promedio para ciertas densidades (0.075, 0.25 y 0.5)
  grafos.promedio <- purrr::pmap_dfr(
    .l = dplyr::distinct(datos.entrada, estadio),
    .f = function(estadio) {
      sujetos <- datos.entrada %>%
        dplyr::filter(estadio == !! estadio) %>%
        dplyr::distinct(sujeto) %>%
        dplyr::pull(sujeto)
      matriz.promedio <- NULL
      for (sujeto in sujetos) {
        matriz.correlacion <- datos.entrada %>%
          dplyr::filter(estadio == !! estadio & sujeto == !! sujeto) %>%
          dplyr::select(-estadio, -sujeto) %>%
          as.matrix()
        if (! is.null(matriz.promedio)) {
          matriz.promedio <- matriz.promedio + matriz.correlacion
        } else {
          matriz.promedio <- matriz.correlacion
        }
      }
      matriz.promedio <- matriz.promedio / length(sujetos)
      
      # Generar grafos para distintas densidades
      densidades <- c(0.075, 0.25, 0.5)
      return (ObtenerGrafosDensidades(matriz.promedio, estadio, NA, densidades))
    }
  ) %>% dplyr::select(-sujeto)

  # Guardar grafos a un archivo RData
  save(grafos, grafos.promedio, file = "output/Grafos.RData")
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

# Generar grafico de metricas
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

# TODO: Mostrar algunos grafos promedio

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
        dplyr::tibble(estadio = estadio, sujeto = sujeto, grafo = 'Original', densidad = densidad,
                      modularidad = modularidad.original, numero_comunidades = numero.com.original, 
                      membresias = list(comunidad.original$membership)),
        dplyr::tibble(estadio = estadio, sujeto = sujeto, grafo = 'Random', densidad = densidad,
                      modularidad = modularidad.random, numero_comunidades = numero.com.random, membrerias = NA)
      )) 
    }
  )
  
  # Obtener membresidas
  membresias <- metricas.louvain %>%
    dplyr::filter(grafo == "Original") %>%
    dplyr::select(estadio, sujeto, densidad, membresias)
  
  # Quitar membrerias de metricas y pasar a data frame largo
  metricas.louvain <- metricas.louvain %>% 
    dplyr::select(-membresias) %>%
    tidyr::pivot_longer(names_to = "metrica", values_to = "valor", cols = c("modularidad", "numero_comunidades"))
  
  # Calcular estadisticas
  estadisticas.louvain <- metricas.louvain %>%
    dplyr::group_by(estadio, densidad, grafo, metrica) %>%
    dplyr::summarise(media = mean(valor, na.rm = TRUE),
                     desvio = sd(valor, na.rm = TRUE))
  
  # Guardar metricas y estadisticas a archivo RData
  save(metricas.louvain, estadisticas.louvain, membresias, file = "output/MetricasLouvain.RData")
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

# t-tests para modularidad
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

# t-tests para numero de comunidades
numero.comunidades <- metricas.louvain %>%
  dplyr::filter(grafo == "Original" & metrica == "numero_comunidades") %>%
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

# ---------------------------------------------------------------------------------------#
# ---- Tarea 4 ----                            
# ---------------------------------------------------------------------------------------#

# funcion para calcular indice Rand ajustado entre membresias
CalcularRandAjustadoMembresias <- function(membresias.estadio, combinaciones, estadio) {
  indices.rand.estadio <- combinaciones %>%
    dplyr::inner_join(membresias.estadio, by = c("sujeto_x" = "sujeto", "densidad")) %>%
    dplyr::rename(membresias_x = membresias) %>%
    dplyr::inner_join(membresias.estadio, by = c("sujeto_y" = "sujeto", "densidad")) %>%
    dplyr::rename(membresias_y = membresias) %>%
    dplyr::mutate(rand_ajustado = purrr::map2(
      .x = membresias_x,
      .y = membresias_y,
      .f = function(mx, my) {
        return (mclust::adjustedRandIndex(unlist(mx), unlist(my)))  
      }
    ) %>% unlist()) %>%
    dplyr::mutate(estadio = estadio) %>%
    dplyr::select(estadio, densidad, sujeto_x, sujeto_y, rand_ajustado)
  return (indices.rand.estadio)
}

if (file.exists("output/IndicesRandMembresias.RData")) {
  load(file = "output/IndicesRandMembresias.RData")
} else {
  # Variables comunes
  sujetos <- membresias %>%
    dplyr::pull(sujeto) %>%
    unique()
  pares.sujetos <- gtools::combinations(n = length(sujetos), r = 2, v = sujetos) %>%
    as.data.frame() %>%
    dplyr::rename(sujeto_x = V1, sujeto_y = V2)
  densidades <- membresias %>%
    dplyr::distinct(densidad)
  combinaciones <- tidyr::crossing(pares.sujetos, densidades)
  
  # 1-2. Para todos los pares de sujetos de Nx se calcula el indice de Rand ajustado
  #      de las comunidades. Esto debe hacerse para todas las densidades
  indices.rand <- purrr::map_dfr(
    .x = list('N1', 'N2', 'N3', 'W'),
    .f = function(estadio) {
      # Seleccionar los datos de membresia para ese estadio
      membresias.estadio <- membresias %>%
        dplyr::filter(estadio == !! estadio) %>%
        dplyr::select(-estadio)
      
      # Para cada densidad, calcular el indice Rand ajustado para las membresias
      # de cada combinacion de sujetos
      return (CalcularRandAjustadoMembresias(membresias.estadio, combinaciones, estadio))
    }
  )
  
  # 3. Calcular promedio y desvio de los indices Rand para cada estadio y densidad
  estadisticas.rand.observadas <- indices.rand %>%
    dplyr::group_by(estadio, densidad) %>%
    dplyr::summarise(media = mean(rand_ajustado, na.rm = TRUE),
                     desvio = sd(rand_ajustado, na.rm = TRUE))
  
  # 4. Permutaciones: Se realizan N permutaciones entre los estadios Nx-W para calcular nuevamente las medias de
  #    Rand ajustado. Esto se hace paralelizado porque tarda muchisimo.
  set.seed(0)
  numero.permutaciones    <- 1000
  semillas                <- sample(x = seq(from = 1, to = 1000000), size = 3 * numero.permutaciones)
  permutaciones.estadios  <- purrr::cross_df(list(permutacion = seq(1, numero.permutaciones), estadio = c('N1', 'N2', 'N3'))) %>%
    dplyr::mutate(semilla = semillas)
  
  input.values            <- iterators::iter(obj = permutaciones.estadios, by = 'row')
  progressBar             <- utils::txtProgressBar(max = nrow(permutaciones.estadios), style = 3)
  progressBarFunction     <- function(n) {
    setTxtProgressBar(progressBar, n)
  }
  snowOptions             <- list(progress = progressBarFunction)
  cluster                 <- snow::makeCluster(type = "SOCK", spec = rep('localhost', length.out = parallel::detectCores()))
  doSNOW::registerDoSNOW(cluster)
  indices.rand.permutados <- foreach::foreach(input.value = input.values, .options.snow = snowOptions,
                                             .packages = list.of.packages,
                                             .errorhandling = 'pass', .verbose = FALSE) %dopar% {
    # Obtener permutaciones de sujetos de N con sujetos de W
    set.seed(input.value$semilla)
    estadio             <- input.value$estadio
    permutacion         <- input.value$permutacion
    sujetos.permutacion <- membresias %>%
      dplyr::distinct(sujeto) %>%
      dplyr::mutate(permutar = as.logical(rbinom(n = length(sujetos), size = 1, prob = 0.5)))
    
    # Seleccionar los datos de membresia para ese estadio
    estadios           <- c(estadio, 'W')
    membresias.estadio <- membresias %>%
      dplyr::filter(estadio %in% estadios) %>%
      tidyr::pivot_wider(names_from = "estadio", values_from = "membresias") %>%
      dplyr::rename(N = !! estadio) %>%
      dplyr::inner_join(sujetos.permutacion, by = c("sujeto")) %>%
      dplyr::mutate(nuevo_N := dplyr::if_else(permutar, W, N)) %>%
      dplyr::select(-N, -W, -permutar) %>%
      dplyr::rename(membresias = nuevo_N)
    
    # Para cada densidad, calcular el indice Rand ajustado para las membresias
    # de cada combinacion de sujetos
    indices.rand.estadio <- CalcularRandAjustadoMembresias(membresias.estadio, combinaciones, estadio) %>%
      dplyr::mutate(permutacion = permutacion) %>%
      dplyr::select(estadio, permutacion, densidad, sujeto_x, sujeto_y, rand_ajustado)
    return (indices.rand.estadio)
  }
  snow::stopCluster(cluster)
  
  # Guardar los datos calculados
  save(indices.rand, estadisticas.rand.observadas, estadisticas.rand.permutadas, file = "output/IndicesRandMembresias.RData")
}

# Graficar valores observados
ggplot2::ggplot(data = dplyr::filter(estadisticas.rand.observadas, estadio %in% c("N2", "W"))) +
  ggplot2::geom_errorbar(mapping = ggplot2::aes(x = densidad, ymin = media - desvio, ymax = media + desvio, col = estadio)) +
  ggplot2::labs(x = "Densidad de aristas", y = "Rand ajustado", col = "Estadío",
                title = "Índices de rand ajustados para membresías de sujetos",
                subtitle = "Valores medios y desvío para cada densidad y estadío de sueño") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'bottom',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.text.x = ggplot2::element_text(angle = 90)
  )
# ----------------------------------------------------------------------------------------