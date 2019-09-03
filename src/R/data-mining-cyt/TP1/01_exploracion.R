# ---------------------------------------------------------------------------------------#
# ---- Script para integración de set de datos para TP1 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(dplyr)
require(ggplot2)
require(ggbiplot)
require(GGally)
require(magrittr)
require(purrr)
require(readr)
require(tidyr)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- II. Lectura del set de datos  ----                            
# ---------------------------------------------------------------------------------------#

audio.features <- readr::read_csv(file = "input/audio_features.csv") 
rownames(audio.features) <- audio.features$id
audio.features %<>% dplyr::select(-id)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Exploracion de features  ----                            
# ---------------------------------------------------------------------------------------#

GGally::ggpairs(data = audio.features, columns = 1:ncol(audio.features)) +
  ggplot2::labs(title = "Features de audio",
                x = "", y = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )

pca            <- stats::princomp(x = audio.features, cor = TRUE)
prop.varianzas <- (pca$sdev ^ 2) / sum((pca$sdev ^ 2))

ggplot2::ggplot(data = data.frame(proporcion = 100 * cumsum(prop.varianzas), componente = 1:length(prop.varianzas)),
       mapping = ggplot2::aes(x = componente, y = proporcion, fill = componente)) +
  ggplot2::scale_x_continuous(breaks = 1:length(prop.varianzas)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::geom_text(mapping = ggplot2::aes(y = proporcion + 1, label = sprintf("%0.2f", proporcion))) +
  ggplot2::labs(x = "Componente", y = "Porcentaje acumulado de varianza explicada") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'none')

ggbiplot::ggbiplot(pcobj = pca, ellipse = TRUE) +
  ggplot2::scale_colour_brewer(name = 'Región', type = "qual", palette = "Set1") +
  ggplot2::labs(x = sprintf("Componente 1 (%0.2f%%)", 100*prop.varianzas[1]),
                y = sprintf("Componente 2 (%0.2f%%)", 100*prop.varianzas[2])) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'right') +
  ggplot2::coord_fixed(ratio = 0.5)
# ----------------------------------------------------------------------------------------