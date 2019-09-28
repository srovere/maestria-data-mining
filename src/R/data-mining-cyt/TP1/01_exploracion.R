# ---------------------------------------------------------------------------------------#
# ---- Script para integración de set de datos para TP1 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(Cairo)
require(dplyr)
require(ggplot2)
require(ggbiplot)
require(GGally)
require(jsonlite)
require(magrittr)
require(purrr)
require(randomForest)
require(readr)
require(tidyr)

options(bitmapType = "cairo")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- II. Lectura del set de datos  ----                            
# ---------------------------------------------------------------------------------------#

# i. Metadatos
metadatos <- readr::read_csv(file = "input/metadata.csv")

# ii. Datos
audio.features               <- readr::read_csv(file = "input/audio_features.csv") %>%
  as.data.frame()

# iii. Datos para PCA
pca.audio.features           <- audio.features
rownames(pca.audio.features) <- pca.audio.features$id
pca.audio.features %<>% dplyr::select(-id)

# iv. Datos para analisis de importancia de features para clasificar por genero
clase <- "genre"
audio.features.clase <- audio.features %>%
  dplyr::inner_join(dplyr::select(metadatos, id, !! clase), by = "id") %>%
  dplyr::select(-id) %>%
  dplyr::mutate(clase = as.factor(!! rlang::sym(clase))) %>%
  dplyr::select(- !! clase)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Exploracion de features  ----                            
# ---------------------------------------------------------------------------------------#

# Matriz de correlaciones y distribuciones
GGally::ggpairs(data = pca.audio.features, 
                columns = 1:ncol(pca.audio.features), 
                #upper = list(continuous = wrap("density", alpha = 0.5), combo = "box"),
                lower = list(continuous = wrap("points", alpha = 0.3,    size=0.1), 
                             combo = wrap("dot", alpha = 0.4,            size=0.2) )) +
  ggplot2::labs(title = "Asociación de features tomados de a pares", x = "", y = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )

# PCA
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

# Random Forest para buscar importancia de atributos
modelo.rf <- randomForest::randomForest(clase ~ ., data = audio.features.clase)
varImpPlot(modelo.rf)
# ----------------------------------------------------------------------------------------