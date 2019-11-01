# ---------------------------------------------------------------------------------------#
# ---- Script para integración de set de datos para TP1 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(bestNormalize)
require(Cairo)
require(caret)
require(cluster)
require(dplyr)
require(ggplot2)
require(ggbiplot)
require(GGally)
require(magrittr)
require(mclustcomp)
require(MASS)
require(purrr)
require(readr)
require(tibble)
require(tidyr)
require(umap)

# Definir si se va a normalizar y/o estandarizar
normalizar   <- FALSE
estandarizar <- FALSE
usar.umap    <- TRUE

options(bitmapType = "cairo")
# ----------------------------------------------------------------------------------------

# opciones <- data.frame(normalizar = c(TRUE, TRUE, FALSE, FALSE, FALSE),
#                        estandarizar = c(TRUE, FALSE, TRUE, FALSE, FALSE),
#                        usar.umap = c(FALSE, FALSE, FALSE, FALSE, TRUE))
# 
# exploracion <- purrr::pmap(
#   .l = opciones,
#   .f = function(normalizar, estandarizar, usar.umap) {
#     resultados.opcion <- list()
      
# ---------------------------------------------------------------------------------------#
# ---- II. Lectura de metadatos y set de audio_analisis  ----                            
# ---------------------------------------------------------------------------------------#

# i. Metadatos
metadatos <- readr::read_csv(file = "input/metadata.csv")

# ii. Datos de audio_features
audio.analisis.rdata <- paste0(getwd(), "/input/audio_analysis.RData")
if (! file.exists(audio.analisis.rdata)) {
  audio.analysis <- purrr::map_dfr(
    .x = dplyr::pull(metadatos, id),
    .f = function(id) {
      timbres.file  <- paste0(getwd(), "/input/audio_analysis/timbre/", id, ".csv")
      pitches.file <- paste0(getwd(), "/input/audio_analysis/pitches/", id, ".csv")
      
      if (file.exists(timbres.file) && file.exists(pitches.file)) {
        # Timbres
        timbres  <- readr::read_csv(file = timbres.file)
        columnas <- setdiff(colnames(timbres), "start")
        timbres.agregado <- dplyr::bind_cols(
          dplyr::select(dplyr::summarise_all(timbres, funs(mean)), -start),
          dplyr::select(dplyr::summarise_all(timbres, funs(sd)), -start)
        )
        colnames(timbres.agregado) <- c(paste0("timbre_media_", columnas), paste0("timbre_desvio_", columnas))
        
        # Pitches
        pitches  <- readr::read_csv(file = pitches.file)
        columnas <- setdiff(colnames(pitches), "start")
        pitches.agregado <- dplyr::bind_cols(
          dplyr::select(dplyr::summarise_all(pitches, funs(mean)), -start),
          dplyr::select(dplyr::summarise_all(pitches, funs(sd)), -start)
        )
        colnames(pitches.agregado) <- c(paste0("pitch_media_", columnas), paste0("pitch_desvio_", columnas))
       
        return (dplyr::bind_cols(
          data.frame(id = id), timbres.agregado, pitches.agregado
        ))
      } else {
        return (NULL)
      }
    }
  )
  
  save(audio.analysis, file = audio.analisis.rdata)
} else {
  load(audio.analisis.rdata)
}

# iii. Escalar/normalizar los datos
if (normalizar) {
  audio.analysis <- purrr::map_dfc(
    .x = colnames(audio.analysis),
    .f = function(atributo) {
      valores <- dplyr::pull(audio.analysis, !! atributo)
      if (atributo == "id") {
        return (data.frame(id = valores))
      } else {
        valores.normalizado <- bestNormalize::bestNormalize(x = valores, 
                                                            standardize = estandarizar,
                                                            allow_orderNorm = FALSE)
        df.norm             <- data.frame(att = valores.normalizado$x.t)
        colnames(df.norm)   <- c(atributo)
        return (df.norm)
      }
    }
  )
} else if (estandarizar) {
  audio.analysis <- audio.analysis %>%
    dplyr::mutate_if(is.numeric, scale)
}
# GGally::ggpairs(data = audio.analysis, columns = 2:ncol(audio.analysis))

# iv. Pasaje a matriz
m.audio.analysis           <- audio.analysis
rownames(m.audio.analysis) <- m.audio.analysis$id
m.audio.analysis           <- m.audio.analysis %>%
  dplyr::select(-id) %>%
  as.matrix()
if (usar.umap) {
  m.audio.analysis <- umap::umap(m.audio.analysis)$layout
}

# v. Datos para validacion externa con atributo "genero"
clase <- "genre"
audio.analysis.clase <- audio.analysis %>%
  dplyr::inner_join(dplyr::select(metadatos, id, !! clase), by = "id") %>%
  dplyr::mutate(clase = as.factor(!! rlang::sym(clase))) %>%
  dplyr::select(- !! clase)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Lectura del set de audio_features y combinar  ----                            
# ---------------------------------------------------------------------------------------#

# i. Lectura y filtrado
audio.features               <- readr::read_csv(file = "input/audio_features.csv") %>%
  dplyr::select(id, acousticness, danceability, energy, instrumentalness, liveness, 
                loudness, speechiness, tempo, valence) %>%
  as.data.frame() %>%
  dplyr::filter(id %in% dplyr::pull(audio.analysis, id))

# ii. Escalar/normalizar los datos
if (normalizar) {
  audio.features <- purrr::map_dfc(
      .x = colnames(audio.features),
      .f = function(atributo) {
        valores <- dplyr::pull(audio.features, !! atributo)
        if (atributo == "id") {
          return (data.frame(id = valores))
        } else {
          valores.normalizado <- bestNormalize::bestNormalize(x = valores, 
                                                              standardize = estandarizar,
                                                              allow_orderNorm = FALSE)
          df.norm             <- data.frame(att = valores.normalizado$x.t)
          colnames(df.norm)   <- c(atributo)
          return (df.norm)
        }
      }
    )
} else if (estandarizar) {
  audio.features <- audio.features %>%
    dplyr::mutate_if(is.numeric, scale)
}
# GGally::ggpairs(data = audio.features, columns = 2:ncol(audio.features))

# iii. Pasaje a matriz
m.audio.features           <- audio.features
rownames(m.audio.features) <- m.audio.features$id
m.audio.features           <- m.audio.features %>%
  dplyr::select(-id) %>%
  as.matrix()
if (usar.umap) {
  m.audio.features <- umap::umap(m.audio.features)$layout
}

# iv. Datos para validacion externa con atributo "genero"
clase <- "genre"
audio.features.clase <- audio.features %>%
  dplyr::inner_join(dplyr::select(metadatos, id, !! clase), by = "id") %>%
  dplyr::mutate(clase = as.factor(!! rlang::sym(clase))) %>%
  dplyr::select(- !! clase)

# v. Combinar atributos
audio.combinado <- audio.features %>%
  dplyr::inner_join(audio.analysis)
audio.combinado.clase <- audio.features.clase %>%
  dplyr::inner_join(audio.analysis.clase)
m.audio.combinado <- audio.combinado
rownames(m.audio.combinado) <- m.audio.combinado$id
m.audio.combinado <- m.audio.combinado %>%
  dplyr::select(-id) %>%
  as.matrix()
if (usar.umap) {
  m.audio.combinado <- umap::umap(m.audio.combinado)$layout
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- A1. Exploracion de hiperparámetros audio_features ----                            
# ---------------------------------------------------------------------------------------#

# Calculamos la matriz de distancia entre individuos (euclídea)
matriz.distancia <- dist(x = m.audio.features, method = "euclidean")

# Clusters con metodos de calculo de distancia entre clusters
hc_complete <- hclust(d = matriz.distancia, method = "complete")
hc_average  <- hclust(d = matriz.distancia, method = "average")
hc_single   <- hclust(d = matriz.distancia, method = "single")
hc_ward.D2  <- hclust(d = matriz.distancia, method = "ward.D2")
hc_ward.D   <- hclust(d = matriz.distancia, method = "ward.D")
hc_centroid <- hclust(d = matriz.distancia, method = "centroid")

# Calculamos los coeficientes de correlación cofenética
coeficientes.correlacion.cofenetica <- c(
  "complete" = cor(x = matriz.distancia, cophenetic(hc_complete)),
  "average" = cor(x = matriz.distancia, cophenetic(hc_average)),
  "single" = cor(x = matriz.distancia, cophenetic(hc_single)),
  "ward.D" = cor(x = matriz.distancia, cophenetic(hc_ward.D2)),
  "ward.D2" = cor(x = matriz.distancia, cophenetic(hc_ward.D)),
  "centroid" = cor(x = matriz.distancia, cophenetic(hc_centroid))
)
print(coeficientes.correlacion.cofenetica)
mejor.metodo       <- names(coeficientes.correlacion.cofenetica)[
  which(coeficientes.correlacion.cofenetica == max(coeficientes.correlacion.cofenetica))
]

# Usamos Complete Linkage
cluster.jerarquico <- stats::hclust(d = matriz.distancia, method = mejor.metodo)
n.clusters         <- seq(from = 2, to = 20)
medias.silhouette  <- purrr::map_dfr(
  .x = n.clusters,
  .f = function(k) {
    silhouette.k    <- silhouette(cutree(cluster.jerarquico, k), matriz.distancia)
    return (data.frame(k = k, silhouette = mean(silhouette.k[, "sil_width"])))
  }
)

# Graficamos el valor del índice de Silhouette para cada valor de k
ggplot2::ggplot(data = medias.silhouette) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = k, y = silhouette))

# Guardar variables del clustering
cluster.jerarquico.features <- cluster.jerarquico
matriz.distancia.features   <- matriz.distancia
# ----------------------------------------------------------------------------------------

    # resultados.opcion <- append(
    #   resultados.opcion,
    #   list(list(
    #     conjunto = "Audio Features",
    #     umap = usar.umap,
    #     normalizar = normalizar,
    #     estandarizar = estandarizar,
    #     correlacion_cofenetica = coeficientes.correlacion.cofenetica,
    #     silhouette = medias.silhouette,
    #     cluster = cluster.jerarquico
    #   ))
    # )

# ---------------------------------------------------------------------------------------#
# ---- A2. Exploracion de hiperparámetros audio_analysis ----                            
# ---------------------------------------------------------------------------------------#

# Calculamos la matriz de distancia entre individuos (euclídea)
matriz.distancia <- dist(x = m.audio.analysis, method = "euclidean")

# Clusters con metodos de calculo de distancia entre clusters
hc_complete <- hclust(d = matriz.distancia, method = "complete")
hc_average  <- hclust(d = matriz.distancia, method = "average")
hc_single   <- hclust(d = matriz.distancia, method = "single")
hc_ward.D2  <- hclust(d = matriz.distancia, method = "ward.D2")
hc_ward.D   <- hclust(d = matriz.distancia, method = "ward.D")
hc_centroid <- hclust(d = matriz.distancia, method = "centroid")

# Calculamos los coeficientes de correlación cofenética
coeficientes.correlacion.cofenetica <- c(
  "complete" = cor(x = matriz.distancia, cophenetic(hc_complete)),
  "average" = cor(x = matriz.distancia, cophenetic(hc_average)),
  "single" = cor(x = matriz.distancia, cophenetic(hc_single)),
  "ward.D" = cor(x = matriz.distancia, cophenetic(hc_ward.D2)),
  "ward.D2" = cor(x = matriz.distancia, cophenetic(hc_ward.D)),
  "centroid" = cor(x = matriz.distancia, cophenetic(hc_centroid))
)
print(coeficientes.correlacion.cofenetica)
mejor.metodo <- names(coeficientes.correlacion.cofenetica)[
  which(coeficientes.correlacion.cofenetica == max(coeficientes.correlacion.cofenetica))
]

# Usamos Complete Linkage
cluster.jerarquico <- stats::hclust(d = matriz.distancia, method = mejor.metodo)
n.clusters         <- seq(from = 2, to = 20)
medias.silhouette  <- purrr::map_dfr(
  .x = n.clusters,
  .f = function(k) {
    silhouette.k    <- silhouette(cutree(cluster.jerarquico, k), matriz.distancia)
    return (data.frame(k = k, silhouette = mean(silhouette.k[, "sil_width"])))
  }
)

# Graficamos el valor del índice de Silhouette para cada valor de k
ggplot2::ggplot(data = medias.silhouette) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = k, y = silhouette))

# Guardar variables del clustering
cluster.jerarquico.analysis <- cluster.jerarquico
matriz.distancia.analysis   <- matriz.distancia
# ----------------------------------------------------------------------------------------

    # resultados.opcion <- append(
    #   resultados.opcion,
    #   list(list(
    #     conjunto = "Audio Analysis",
    #     umap = usar.umap,
    #     normalizar = normalizar,
    #     estandarizar = estandarizar,
    #     correlacion_cofenetica = coeficientes.correlacion.cofenetica,
    #     silhouette = medias.silhouette,
    #     cluster = cluster.jerarquico
    #   ))
    # )

# ---------------------------------------------------------------------------------------#
# ---- A3. Exploracion de hiperparámetros audio_analysis + audio_features ----                            
# ---------------------------------------------------------------------------------------#

# Calculamos la matriz de distancia entre individuos (euclídea)
matriz.distancia <- dist(x = m.audio.combinado, method = "euclidean")

# Clusters con metodos de calculo de distancia entre clusters
hc_complete <- hclust(d = matriz.distancia, method = "complete")
hc_average  <- hclust(d = matriz.distancia, method = "average")
hc_single   <- hclust(d = matriz.distancia, method = "single")
hc_ward.D2  <- hclust(d = matriz.distancia, method = "ward.D2")
hc_ward.D   <- hclust(d = matriz.distancia, method = "ward.D")
hc_centroid <- hclust(d = matriz.distancia, method = "centroid")

# Calculamos los coeficientes de correlación cofenética
coeficientes.correlacion.cofenetica <- c(
  "complete" = cor(x = matriz.distancia, cophenetic(hc_complete)),
  "average" = cor(x = matriz.distancia, cophenetic(hc_average)),
  "single" = cor(x = matriz.distancia, cophenetic(hc_single)),
  "ward.D" = cor(x = matriz.distancia, cophenetic(hc_ward.D2)),
  "ward.D2" = cor(x = matriz.distancia, cophenetic(hc_ward.D)),
  "centroid" = cor(x = matriz.distancia, cophenetic(hc_centroid))
)
print(coeficientes.correlacion.cofenetica)
mejor.metodo <- names(coeficientes.correlacion.cofenetica)[
  which(coeficientes.correlacion.cofenetica == max(coeficientes.correlacion.cofenetica))
]

# Usamos Complete Linkage
cluster.jerarquico <- stats::hclust(d = matriz.distancia, method = mejor.metodo)
n.clusters         <- seq(from = 2, to = 20)
medias.silhouette  <- purrr::map_dfr(
  .x = n.clusters,
  .f = function(k) {
    silhouette.k    <- silhouette(cutree(cluster.jerarquico, k), matriz.distancia)
    return (data.frame(k = k, silhouette = mean(silhouette.k[, "sil_width"])))
  }
)

# Graficamos el valor del índice de Silhouette para cada valor de k
ggplot2::ggplot(data = medias.silhouette) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = k, y = silhouette))

# Guardar variables del clustering
cluster.jerarquico.combinado <- cluster.jerarquico
matriz.distancia.combinado   <- matriz.distancia
# ----------------------------------------------------------------------------------------

#     resultados.opcion <- append(
#       resultados.opcion,
#       list(list(
#         conjunto = "Combinado",
#         umap = usar.umap,
#         normalizar = normalizar,
#         estandarizar = estandarizar,
#         correlacion_cofenetica = coeficientes.correlacion.cofenetica,
#         silhouette = medias.silhouette,
#         cluster = cluster.jerarquico
#       ))
#     )
# 
#     return (resultados.opcion)
#   }
# )
# exploracion <- purrr::flatten(exploracion)
# save(exploracion, file = paste0(getwd(), "/output/exploracion.RData"))

# ---------------------------------------------------------------------------------------#
# ---- A4. Evalucion interna de clusters ----                            
# ---------------------------------------------------------------------------------------#

# # Coeficiente de correlacion cofenetico
# df.ccc <- purrr::map_dfr(
#   .x = seq_along(exploracion),
#   .f = function(seq_index) {
#     exp <- exploracion[[seq_index]]
#     df  <- data.frame(metodo = names(exp$correlacion_cofenetica),
#                       valor = as.double(exp$correlacion_cofenetica)) %>%
#       dplyr::mutate(normalizar = exp$normalizar, estandarizar = exp$estandarizar,
#                     umap = exp$umap, conjunto = exp$conjunto)
#     return (df)
#   }
# ) %>% dplyr::mutate(transformacion = dplyr::case_when(
#   umap ~ "UMAP",
#   normalizar & estandarizar ~ "Normalizado y estandarizado",
#   normalizar & ! estandarizar ~ "Normalizado",
#   ! normalizar & estandarizar ~ "Estandarizado",
#   TRUE ~ "Original"
# )) %>% dplyr::mutate(conjunto = as.factor(conjunto),
#                      metodo = as.factor(metodo),
#                      transformacion = as.factor(transformacion))
# 
# ggplot2::ggplot(df.ccc) +
#   ggplot2::geom_tile(mapping = ggplot2::aes(x = metodo, y = transformacion, fill = valor)) +
#   ggplot2::scale_fill_viridis_c(alpha = 1, begin = 0, end = 1,
#                                 direction = 1, option = "D", values = NULL, space = "Lab",
#                                 na.value = "white", guide = "colourbar", aesthetics = "fill") +
#   ggplot2::geom_text(mapping = ggplot2::aes(x = metodo, y = transformacion,
#                                             label = sprintf("%.3f", valor))) +
#   ggplot2::facet_wrap(~conjunto, ncol = 1) +
#   ggplot2::labs(x = "Métrica de distancia intraclusters", y = "Transformación aplicada",
#                 title = "Coeficiente de correlación cofenético",
#                 subtitle = "Análisis para diversos conjuntos de datos y métricas",
#                 fill = "Coeficiente de correlación cofenético") +
#   ggplot2::theme_bw() +
#   ggplot2::theme(
#     legend.position = 'bottom',
#     plot.title = ggplot2::element_text(hjust = 0.5),
#     plot.subtitle = ggplot2::element_text(hjust = 0.5)
#   ) + ggplot2::ggsave(filename = paste0("output/CCC.png"), dpi = 1000, width = 8, height = 6)
#  
# # Indice de Silhouette
# df.sil <- purrr::map_dfr(
#   .x = seq_along(exploracion),
#   .f = function(seq_index) {
#     exp <- exploracion[[seq_index]]
#     df  <- exp$silhouette %>%
#       dplyr::mutate(normalizar = exp$normalizar, estandarizar = exp$estandarizar,
#                     umap = exp$umap, conjunto = exp$conjunto)
#     return (df)
#   }
# ) %>% dplyr::mutate(transformacion = dplyr::case_when(
#   umap ~ "UMAP",
#   normalizar & estandarizar ~ "Normalizado y estandarizado",
#   normalizar & ! estandarizar ~ "Normalizado",
#   ! normalizar & estandarizar ~ "Estandarizado",
#   TRUE ~ "Original"
# )) %>% dplyr::mutate(conjunto = as.factor(conjunto),
#                      transformacion = as.factor(transformacion))
# 
# ggplot2::ggplot(df.sil) +
#   ggplot2::geom_line(mapping = ggplot2::aes(x = k, y = silhouette, col = transformacion)) +
#   ggplot2::facet_wrap(~conjunto, ncol = 1) +
#   ggplot2::labs(x = "Cantidad de clusters", y = "Índice de Silhouette",
#                 title = "Índice de Silhouette para distintos clusterings",
#                 subtitle = "Análisis para diversos conjuntos de datos",
#                 col = "Transformación aplicada") +
#   ggplot2::scale_x_continuous(breaks = seq(from = 2, to = 20)) +
#   ggplot2::scale_y_continuous(breaks = seq(from = 0, to = 0.6, by = 0.1)) +
#   ggplot2::theme_bw() +
#   ggplot2::theme(
#     legend.position = 'bottom',
#     plot.title = ggplot2::element_text(hjust = 0.5),
#     plot.subtitle = ggplot2::element_text(hjust = 0.5)
#   ) + ggplot2::ggsave(filename = paste0("output/Silhouette.png"), dpi = 1000, width = 8, height = 6)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- B. Comparación de clusters ----                            
# ---------------------------------------------------------------------------------------#

# Generamos clusterizacion para k = 5 (cantidad de clusters para mejor clustering)
grupos.jerarquico <- audio.combinado.clase %>%
  dplyr::select(id, clase) %>%
  dplyr::mutate(clase_features = stats::cutree(cluster.jerarquico.features, k = 3),
                clase_analysis = stats::cutree(cluster.jerarquico.analysis, k = 5),
                clase_combinado = stats::cutree(cluster.jerarquico.combinado, k = 6))

table(grupos.jerarquico$clase_features, grupos.jerarquico$clase_analysis)
mclustcomp::mclustcomp(x = grupos.jerarquico$clase_features, 
                       y = grupos.jerarquico$clase_analysis,
                       types = c("adjrand", "vdm", "rand"))

table(grupos.jerarquico$clase_analysis, grupos.jerarquico$clase_combinado)
mclustcomp::mclustcomp(x = grupos.jerarquico$clase_analysis, 
                       y = grupos.jerarquico$clase_combinado,
                       types = c("adjrand", "vdm", "rand"))

table(grupos.jerarquico$clase_features, grupos.jerarquico$clase_combinado)
mclustcomp::mclustcomp(x = grupos.jerarquico$clase_features, 
                       y = grupos.jerarquico$clase_combinado,
                       types = c("adjrand", "vdm", "rand"))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- C. Evaluacion externa ----                            
# ---------------------------------------------------------------------------------------#

grupos.jerarquico.5.clases <- audio.combinado.clase %>%
  dplyr::select(id, clase) %>%
  dplyr::mutate(clase_features = stats::cutree(cluster.jerarquico.features, k = 5),
                clase_analysis = stats::cutree(cluster.jerarquico.analysis, k = 5),
                clase_combinado = stats::cutree(cluster.jerarquico.combinado, k = 5))

table(grupos.jerarquico.5.clases$clase, grupos.jerarquico.5.clases$clase_features)
mclustcomp::mclustcomp(x = as.character(grupos.jerarquico.5.clases$clase_features), 
                       y = as.integer(as.factor((grupos.jerarquico.5.clases$clase))),
                       types = c("adjrand", "vdm", "rand"))

table(grupos.jerarquico.5.clases$clase, grupos.jerarquico.5.clases$clase_analysis)
mclustcomp::mclustcomp(x = as.character(grupos.jerarquico.5.clases$clase_analysis), 
                       y = as.integer(as.factor((grupos.jerarquico.5.clases$clase))),
                       types = c("adjrand", "vdm", "rand"))

table(grupos.jerarquico.5.clases$clase, grupos.jerarquico.5.clases$clase_combinado)
mclustcomp::mclustcomp(x = as.character(grupos.jerarquico.5.clases$clase_combinado), 
                       y = as.integer(as.factor((grupos.jerarquico.5.clases$clase))),
                       types = c("adjrand", "vdm", "rand"))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- D. Visualizacion de clusters con UMAP ----                            
# ---------------------------------------------------------------------------------------#

# Se grafica el mejor clustering (audio analysis)
datos.grafico.clusters <- dplyr::bind_cols(
  grupos.jerarquico.5.clases,
  as.data.frame(m.audio.analysis)
) %>% dplyr::rename(x = V1, y = V2, clase_cluster = clase_analysis) %>%
  dplyr::select(x, y, clase, clase_cluster)
ggplot2::ggplot(data = datos.grafico.clusters) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y, col = as.factor(clase_cluster), shape = clase)) +
  ggplot2::labs(x = "Componente 1", y = "Componente 2", title = "Clustering en base a datos de audio_analisis",
                subtitle = "Los features fueron transformados previamente utilizando UMAP",
                col = "Cluster", shape = "Género musical") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "right",
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) + ggplot2::ggsave(filename = paste0("output/ClusterUMAP.png"), dpi = 1000, width = 8, height = 6)
# ----------------------------------------------------------------------------------------