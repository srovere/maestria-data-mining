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
require(MASS)
require(purrr)
require(readr)
require(tidyr)

# Definir si se va a normalizar y/o estandarizar
normalizar   <- FALSE
estandarizar <- FALSE

options(bitmapType = "cairo")
# ----------------------------------------------------------------------------------------

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
        valores.normalizado <- bestNormalize::bestNormalize(x = valores, standardize = estandarizar)
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
          valores.normalizado <- bestNormalize::bestNormalize(x = valores, standardize = estandarizar)
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
cluster.jerarquico.features <- cluster.jerarquico
matriz.distancia.features   <- matriz.distancia
# ----------------------------------------------------------------------------------------

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

# ---------------------------------------------------------------------------------------#
# ---- B. Evaluar agrupamientos ----                            
# ---------------------------------------------------------------------------------------#

# Generamos clusterizacion para k = 5 (dado que esto va a servir para comparar con los generos)
grupos.jerarquico <- audio.combinado.clase %>%
  dplyr::select(id, clase) %>%
  dplyr::mutate(clase_features = stats::cutree(cluster.jerarquico.features, k = 4),
                clase_analysis = stats::cutree(cluster.jerarquico.analysis, k = 4),
                clase_combinado = stats::cutree(cluster.jerarquico.combinado, k = 4))

table(grupos.jerarquico$clase_features, grupos.jerarquico$clase_analysis)
table(grupos.jerarquico$clase_analysis, grupos.jerarquico$clase_combinado)
table(grupos.jerarquico$clase_features, grupos.jerarquico$clase_combinado)

table(grupos.jerarquico$clase, grupos.jerarquico$clase_features)

# PCA
pca.audio.features           <- audio.features
rownames(pca.audio.features) <- pca.audio.features$id
pca.audio.features %<>% dplyr::select(-id)

pca            <- stats::princomp(x = pca.audio.features, cor = TRUE)
prop.varianzas <- (pca$sdev ^ 2) / sum((pca$sdev ^ 2))
ggplot2::ggplot(data = data.frame(proporcion = 100 * prop.varianzas, componente = 1:length(prop.varianzas)),
       mapping = ggplot2::aes(x = componente, y = proporcion, fill = componente)) +
  ggplot2::scale_x_continuous(breaks = 1:length(prop.varianzas)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::geom_text(mapping = ggplot2::aes(y = proporcion + 1, label = sprintf("%0.2f", proporcion))) +
  ggplot2::labs(x = "Componente", y = "Porcentaje", title = "Porcentaje de varianza explicada por cada componente") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )

ggbiplot::ggbiplot(pcobj = pca, ellipse = TRUE, alpha = 0) +
  ggplot2::scale_colour_brewer(name = 'Región', type = "qual", palette = "Set1") +
  ggplot2::labs(x = sprintf("Componente 1 (%0.2f%%)", 100*prop.varianzas[1]),
                y = sprintf("Componente 2 (%0.2f%%)", 100*prop.varianzas[2]),
                title = "Biplot de las primeras 2 componentes") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
  ggplot2::coord_fixed(ratio = 0.5)
