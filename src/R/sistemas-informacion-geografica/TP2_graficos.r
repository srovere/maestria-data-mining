# -----------------------------------------------------------------------------#
# --- PASO 1. Inicializar script ----
# -----------------------------------------------------------------------------#
# a) Borrar entorno
rm(list = ls()); gc()

# b) Cargar paquetes
list.of.packages <- c("Cairo", "dplyr", "ggplot2", "ggrepel", "grDevices", "purrr",
                      "raster", "readr", "rpart", "rpart.plot", "sf", "sp", "utils")
                      
for (pack in list.of.packages) {
  if (!require(pack, character.only = TRUE)) {
    stop("Paquete no instalado: ", pack)
  }
}
rm(list.of.packages, pack); gc()
options(bitmapType = "cairo")

# c) Definir carpetas
working.directory <- paste0(getwd(), "/tp-teledeteccion2020")
images.directory  <- paste0(working.directory, "/images/final")

# d) Cargar metricas de cutoff
metricas.promedio <- readr::read_csv("data/metricas_cutoff.csv") %>%
  dplyr::mutate(fpr = 1 - specificity, tpr = recall)

# e) Cargar datos de muestras
load("data/datos_muestras.RData")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Graficos de ROC+Precision-Recall ----
# -----------------------------------------------------------------------------#

# a) Calculo de AUC (metodo de los trapecios)
CalcularAUC <- function(x, y) {
  puntos <- purrr::transpose(list(x = x, y = y))
  auc    <- purrr::pmap(
    .l = list(desde = puntos[1:length(x)-1], hasta = puntos[2:length(x)]),
    .f = function(desde, hasta) {
      dx   <- abs(hasta$x - desde$x)
      ymin <- min(desde$y, hasta$y)
      ymax <- max(desde$y, hasta$y)
      return (((ymin+ymax)*dx)/2)
    }
  ) %>% purrr::reduce(.x = ., .f = `+`)
  return (auc)
}

# b) ROC
metricas.roc <- metricas.promedio %>%
  dplyr::select(cutoff, tpr, fpr) %>%
  dplyr::bind_rows(., data.frame(cutoff = 0, tpr = 1, fpr = 1), data.frame(cutoff = 1, tpr = 0, fpr = 0)) %>%
  dplyr::arrange(fpr)
roc.auc      <- CalcularAUC(x = dplyr::pull(metricas.roc, fpr), y = dplyr::pull(metricas.roc, tpr))
puntos.opt   <- metricas.roc %>%
  dplyr::filter((cutoff >= 0.33) & (cutoff <= 0.62))
grafico.roc  <- ggplot2::ggplot(data = metricas.roc) +
  ggplot2::geom_rect(xmin = min(puntos.opt$fpr), xmax = max(puntos.opt$fpr), ymin = 0, ymax = 1, fill = 'grey80') +
  ggplot2::geom_rect(ymin = min(puntos.opt$tpr), ymax = max(puntos.opt$tpr), xmin = 0, xmax = 1, fill = 'grey80') +
  ggplot2::geom_rect(xmin = min(puntos.opt$fpr), xmax = max(puntos.opt$fpr), 
                     ymin = min(puntos.opt$tpr), ymax = max(puntos.opt$tpr), col = 'black', fill = 'grey70', linetype = 'dotted') +
  ggplot2::geom_line(mapping = ggplot2::aes(x = fpr, y = tpr),
                     color = "#377eb8", size = 1) + 
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotdash", color = "#e6ab02", size = 1) +
  ggplot2::geom_point(data = data.frame(x = 0.1, y = 0.9), color = '#e41a1c', size = 4,
                      mapping = ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_point(data = data.frame(x = 0.0614, y = 0.854), color = '#4daf4a', size = 4,
                      mapping = ggplot2::aes(x = x, y = y)) +
  ggrepel::geom_label_repel(data = data.frame(x = 0.2, y = 0.2, label = paste0("Clasificación aleatoria\nAUC: ", 0.5)),
                            mapping = ggplot2::aes(x = x, y = y, label = label),
                            nudge_x = 0.3, nudge_y = 0, size = 5) +
  ggrepel::geom_label_repel(data = data.frame(x = 0.8, y = 1, label = paste0("Clasificación con XGBoost\nAUC: ", round(roc.auc, 3))),
                            mapping = ggplot2::aes(x = x, y = y, label = label),
                            nudge_x = 0, nudge_y = -0.2, size = 5) +
  ggrepel::geom_label_repel(data = data.frame(x = 0.15, y = 0.87, label = paste0("Zona de puntos de corte\n variando entre 0.62 y 0.33")),
                            mapping = ggplot2::aes(x = x, y = y, label = label),
                            nudge_x = 0.1, nudge_y = -0.1, size = 5) +
  ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::labs(x = 'False positive rate', y = 'True positive rate',
                title = 'Curva ROC para conjunto de entrenamiento-validación', subtitle = 'Modelo con algoritmo XGBoost') +
  ggplot2::theme_bw(base_size = 20) +
  ggplot2::theme(
    legend.position = 'bottom',
    panel.grid.major = ggplot2::element_line(colour = 'grey50', linetype = 'dotted'),
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
ggplot2::ggsave(filename = paste0(images.directory, "/XGB-CV-ROC.png"),
                plot = grafico.roc, dpi = 120, width = 9, height = 6)

# c) RP
metricas.pr <- metricas.promedio %>%
  dplyr::select(cutoff, recall, precision) %>%
  dplyr::bind_rows(., data.frame(cutoff = 0, recall = 1, precision = 0.5), data.frame(cutoff = 1, recall = 0, precision = 1)) %>%
  dplyr::arrange(recall)
rp.auc     <- CalcularAUC(x = dplyr::pull(metricas.pr, recall), y = dplyr::pull(metricas.pr, precision))
cortes.opt <- metricas.pr %>%
  dplyr::filter((precision >= 0.85) & (precision <= 0.95) & (recall >= 0.85) & (recall <= 0.95)) %>%
  dplyr::pull(cutoff) %>%
  sort()
grafico.rp <- ggplot2::ggplot(data = metricas.pr) +
  ggplot2::geom_rect(xmin = 0.85, xmax = 0.95, ymin = 0.5, ymax = 1, fill = 'grey80') +
  ggplot2::geom_rect(ymin = 0.85, ymax = 0.95, xmin = 0, xmax = 1, fill = 'grey80') +
  ggplot2::geom_rect(ymin = 0.85, ymax = 0.95, xmin = 0.85, xmax = 0.95, col = 'black', fill = 'grey70', linetype = 'dotted') +
  ggplot2::geom_line(mapping = ggplot2::aes(x = recall, y = precision), 
                     color = "#377eb8", size = 1) + 
  ggplot2::geom_hline(yintercept = 0.5, linetype = "dotdash", color = "#e6ab02", size = 1) +
  ggplot2::geom_point(data = data.frame(x = 0.9, y = 0.9), color = '#e41a1c', size = 4,
                      mapping = ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_point(data = data.frame(x = 0.854, y = 0.933), color = '#4daf4a', size = 4,
                      mapping = ggplot2::aes(x = x, y = y)) +
  ggrepel::geom_label_repel(data = data.frame(x = 0.5, y = 0.5, label = paste0("Clasificación aleatoria\nAUC: ", 0.5)),
                            mapping = ggplot2::aes(x = x, y = y, label = label),
                            nudge_x = 0, nudge_y = 0.1, size = 5) +
  ggrepel::geom_label_repel(data = data.frame(x = 0.2, y = 1, label = paste0("Clasificación con XGBoost\nAUC: ", round(rp.auc, 3))),
                            mapping = ggplot2::aes(x = x, y = y, label = label),
                            nudge_x = 0, nudge_y = -0.3, size = 5) +
  ggrepel::geom_label_repel(data = data.frame(x = 0.87, y = 0.87, label = sprintf("Zona con precision y recall\n entre 0.85 y 0.95\nPunto de corte entre %.2f y %.2f", max(cortes.opt), min(cortes.opt))),
                            mapping = ggplot2::aes(x = x, y = y, label = label),
                            nudge_x = -0.15, nudge_y = -0.1, size = 5) +
  ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
  ggplot2::labs(x = 'Recall', y = 'Precision',
                title = 'Curva PR para conjunto de entrenamiento-validación', subtitle = 'Modelo con algoritmo XGBoost') +
  ggplot2::theme_bw(base_size = 20) +
  ggplot2::theme(
    legend.position = 'bottom',
    panel.grid.major = ggplot2::element_line(colour = 'grey50', linetype = 'dotted'),
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
ggplot2::ggsave(filename = paste0(images.directory, "/XGB-CV-PR.png"),
                plot = grafico.rp, dpi = 120, width = 9, height = 6)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Árbol de decisión ----
# -----------------------------------------------------------------------------#

# a) Crear modelo usando un arbol de decision CART
datos.nombre.clase <- datos.muestras %>%
  dplyr::rename(Blue = b2, Green = b3, Red = b4, NIR = b8, NDWI = ndwi, NDVI = ndvi) %>%
  dplyr::mutate(clase = factor(dplyr::if_else(clase == 0, "No inundación", "Inundación"), levels = c("No inundación", "Inundación")))
set.seed(0)
modelo <- rpart::rpart(clase ~ ., 
                       data = datos.nombre.clase, 
                       method = 'class', 
                       control = list(maxdepth = 10))


# b) Graficar
grDevices::png(filename = paste0(images.directory, "/CART-Decision-Tree.png"),
               quality = 150)
rpart.plot::rpart.plot(x = modelo, digits = 3, type = 4, box.palette = "BnBu", cex = 1.5,
                       main = "Modelo basado en árbol de decisión")
grDevices::dev.off()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Métricas para cutoff ----
# -----------------------------------------------------------------------------#

metricas.cutoff <- metricas.promedio %>%
  dplyr::rename(Accuracy = accuracy, F1 = f1, Kappa = kappa, Precision = precision, Recall = recall) %>%
  dplyr::select(cutoff, Accuracy, F1, Kappa, Precision, Recall) %>%
  tidyr::pivot_longer(data = ., names_to = "metrica", values_to = "valor", 
                      cols = c(-cutoff))
grafico.cutoff <- ggplot2::ggplot(data = metricas.cutoff) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = cutoff, y = valor, col = metrica), size = 1) + 
  ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1),
                              limits = c(0.01, 0.99)) +
  ggplot2::labs(x = 'Punto de corte', y = 'Valor de la métrica', col = 'Métrica',
                title = 'Métricas para XGBoost según punto de corte (entrenamiento-validación)',
                subtitle = 'Se consideran inundados los píxels con probabilidad mayor o igual al punto de corte') +
  ggplot2::theme_bw(base_size = 20) +
  ggplot2::theme(
    legend.position = 'bottom',
    panel.grid.major = ggplot2::element_line(colour = 'grey50', linetype = 'dotted'),
    panel.grid.minor = ggplot2::element_line(colour = 'grey60', linetype = 'dotted'),
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
ggplot2::ggsave(filename = paste0(images.directory, "/XGB-CV-Metricas-Cutoff.png"),
                plot = grafico.cutoff, dpi = 120, width = 12, height = 8)
# ------------------------------------------------------------------------------