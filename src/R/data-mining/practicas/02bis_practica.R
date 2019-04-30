# ----------------------------------------------------------------------------------
# --- Practica de laboratorio 02 bis
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Inicializacion del entorno ----
# ---------------------------------------------------------------------------------#
# i. Eliminacion de objetos del ambiente
rm(list = objects())

# ii. Carga de librerias
require(dplyr)
require(ggplot2)
require(GGally)
require(kernlab)
require(readr)
require(Rlof)
require(tidyr)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Lectura de registros diarios de estacion meteorológica ----
# ---------------------------------------------------------------------------------#
# i. Leer registros diarios
registros.diarios <- readr::read_delim(file = paste0("input/LAB02bis/pehuajo.csv"), delim = "\t")

# ii. Obtener temperaturas y eliminar registros que contengan algun faltante
temperaturas <- registros.diarios %>%
  dplyr::mutate(observacion = dplyr::row_number()) %>%
  dplyr::filter(! (is.na(tmax) | is.na(tmin))) %>%
  dplyr::select(fecha, observacion, tmax, tmin)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Outliers utilizando LOF a partir de tmax y tmin ----
# ---------------------------------------------------------------------------------#
# i. Buscar local outfiler factors
lof.values <- Rlof::lof(data = temperaturas[, c("tmax", "tmin")], k = 5, cores = 8)

# ii. Dados los valores de LOF, buscar los outliers de forma univariada
#     considerando aquellos valores que superen Q3 + 1.5 * IQR(lof.values)
umbral.lof       <- stats::quantile(x = lof.values, probs = 0.75, na.rm = TRUE) + 1.5 * stats::IQR(x = lof.values, na.rm = TRUE)
temperaturas.lof <- temperaturas %>%
  dplyr::mutate(es.outlier = (lof.values >= umbral.lof)) %>%
  dplyr::select(fecha, es.outlier) %>%
  dplyr::right_join(registros.diarios) %>%
  dplyr::mutate(amplitud_termica = tmax - tmin) %>%
  dplyr::mutate(tipo_dato = dplyr::if_else(is.na(es.outlier), "Faltante",
                                           dplyr::if_else(es.outlier, "Atípico", "Normal")),
                metodo = 'LOF') %>%
  dplyr::select(fecha, tmax, tmin, amplitud_termica, tipo_dato, metodo)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Outliers utilizando distancia de Mahalanobis a partir de tmax y tmin ----
# ---------------------------------------------------------------------------------#
# i. Buscar distancia de mahalanobis
dist.manalanobis <- stats::mahalanobis(x = temperaturas[, c("tmax", "tmin")],
                                       center = base::colMeans(temperaturas[, c("tmax", "tmin")]),
                                       cov = stats::cov(temperaturas[, c("tmax", "tmin")]))

# ii. Dados los valores de LOF, buscar los outliers de forma univariada
#     considerando aquellos valores que superen Q3 + 1.5 * IQR(lof.values)
umbral.maha       <- stats::quantile(x = dist.manalanobis, probs = 0.75) + 1.5 * stats::IQR(x = dist.manalanobis)
temperaturas.maha <- temperaturas %>%
  dplyr::mutate(es.outlier = (dist.manalanobis >= umbral.maha)) %>%
  dplyr::select(fecha, es.outlier) %>%
  dplyr::right_join(registros.diarios) %>% 
  dplyr::mutate(amplitud_termica = tmax - tmin) %>%
  dplyr::mutate(tipo_dato = dplyr::if_else(is.na(es.outlier), "Faltante",
                                           dplyr::if_else(es.outlier, "Atípico", "Normal")),
                metodo = 'Mahalanobis') %>%
  dplyr::select(fecha, tmax, tmin, amplitud_termica, tipo_dato, metodo)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Outliers utilizando SVM a partir de tmax y tmin ----
# ---------------------------------------------------------------------------------#
# i. Ajustar modelo SVM
svm.model <- kernlab::ksvm(x = as.matrix(temperaturas[, c("tmax", "tmin")]), 
                           nu=0.09, type="one-svc", kernel="vanilladot")

# ii. Predecir outliers
temperaturas.svm <- temperaturas %>%
  dplyr::mutate(es.outlier = kernlab::predict(svm.model)) %>%
  dplyr::select(fecha, es.outlier) %>%
  dplyr::right_join(registros.diarios) %>% 
  dplyr::mutate(amplitud_termica = tmax - tmin) %>%
  dplyr::mutate(tipo_dato = dplyr::if_else(is.na(es.outlier), "Faltante",
                                           dplyr::if_else(es.outlier, "Atípico", "Normal")),
                metodo = 'SVM') %>%
  dplyr::select(fecha, tmax, tmin, amplitud_termica, tipo_dato, metodo)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Outliers utilizando K-means a partir de tmax y tmin ----
# ---------------------------------------------------------------------------------#
# i. Determinar clusters con 2 centros
km.cluster <- stats::kmeans(x = temperaturas[, c("tmax", "tmin")], centers = 2)

# ii. Predecir outliers
temperaturas.km <- temperaturas %>%
  dplyr::mutate(grupo = km.cluster$cluster) %>%
  dplyr::mutate(tmax_centro = km.cluster$centers[km.cluster$cluster, "tmax"],
                tmin_centro = km.cluster$centers[km.cluster$cluster, "tmin"]) %>%
  dplyr::mutate(distancia_centro = sqrt((tmax - tmax_centro) ^ 2) + (tmin - tmin_centro) ^ 2)
umbral <- stats::quantile(x = temperaturas.km$distancia_centro, probs = 0.75) + 1.5 * stats::IQR(x = temperaturas.km$distancia_centro)
temperaturas.km <- temperaturas.km %>%
  dplyr::mutate(es.outlier = (distancia_centro >= umbral)) %>%
  dplyr::select(fecha, es.outlier) %>%
  dplyr::right_join(registros.diarios) %>% 
  dplyr::mutate(amplitud_termica = tmax - tmin) %>%
  dplyr::mutate(tipo_dato = dplyr::if_else(is.na(es.outlier), "Faltante",
                                           dplyr::if_else(es.outlier, "Atípico", "Normal")),
                metodo = 'K-Means') %>%
  dplyr::select(fecha, tmax, tmin, amplitud_termica, tipo_dato, metodo)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Graficar ----
# ---------------------------------------------------------------------------------#
# i. Scatterplot de temperaturas
temperaturas.outliers <- dplyr::bind_rows(
  temperaturas.lof, temperaturas.maha, temperaturas.svm, temperaturas.km
)
ggplot2::ggplot(data = dplyr::filter(temperaturas.outliers, lubridate::year(fecha) == 2018)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = tmin, y = tmax, col = tipo_dato)) +
  ggplot2::facet_wrap(~metodo, ncol = 1) +
  ggplot2::scale_color_manual(name = "Tipo de dato", 
                              values = c("Normal" = "#000000", "Atípico" = "#ff0000", "Faltante" = "#0000ff"))

# ii. Series temporales para 2018
series.temporales.outliers <- temperaturas.outliers %>%
  tidyr::gather(key = variable, value = valor, -fecha, -tipo_dato, -metodo)
ggplot2::ggplot(data = dplyr::filter(series.temporales.outliers, lubridate::year(fecha) == 2018)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = fecha, y = valor, col = tipo_dato)) +
  ggplot2::facet_grid(metodo~variable) +
  ggplot2::scale_color_manual(name = "Tipo de dato", 
                              values = c("Normal" = "#000000", "Atípico" = "#ff0000", "Faltante" = "#0000ff"))
# ----------------------------------------------------------------------------------
