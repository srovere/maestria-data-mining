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
  dplyr::select(fecha, tmax, tmin, es.outlier)

# iii. Transformar a data frame largo por variable
datos.grafico.lof <- temperaturas.lof %>%
  tidyr::gather(key = variable, value = valor, -fecha, -es.outlier) %>%
  dplyr::mutate(tipo_dato = dplyr::if_else(is.na(es.outlier), "Faltante",
                                           dplyr::if_else(es.outlier, "Atípico", "Normal")))

# iv. Generar grafico solamente para 2018
ggplot2::ggplot(data = dplyr::filter(datos.grafico.lof, lubridate::year(fecha) == 2018)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = fecha, y = valor, col = tipo_dato)) +
  ggplot2::facet_wrap(~variable, ncol = 1) +
  ggplot2::scale_color_manual(name = "Tipo de dato", 
                              values = c("Normal" = "#000000", "Atípico" = "#ff0000", "Faltante" = "#0000ff"))
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
  dplyr::select(fecha, tmax, tmin, amplitud_termica, es.outlier)

# iii. Transformar a data frame largo por variable
datos.grafico.maha <- temperaturas.maha %>%
  tidyr::gather(key = variable, value = valor, -fecha, -es.outlier) %>%
  dplyr::mutate(tipo_dato = dplyr::if_else(is.na(es.outlier), "Faltante",
                                           dplyr::if_else(es.outlier, "Atípico", "Normal")))

# iv. Generar grafico solamente para 2018
ggplot2::ggplot(data = dplyr::filter(datos.grafico.maha, lubridate::year(fecha) == 2018)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = fecha, y = valor, col = tipo_dato)) +
  ggplot2::facet_wrap(~variable, ncol = 1) +
  ggplot2::scale_color_manual(name = "Tipo de dato", 
                              values = c("Normal" = "#000000", "Atípico" = "#ff0000", "Faltante" = "#0000ff"))
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
  dplyr::select(fecha, tmax, tmin, amplitud_termica, es.outlier)

# iii. Transformar a data frame largo por variable
datos.grafico.svm <- temperaturas.svm %>%
  tidyr::gather(key = variable, value = valor, -fecha, -es.outlier) %>%
  dplyr::mutate(tipo_dato = dplyr::if_else(is.na(es.outlier), "Faltante",
                                           dplyr::if_else(es.outlier, "Atípico", "Normal")))

# iv. Generar grafico solamente para 2018
ggplot2::ggplot(data = dplyr::filter(datos.grafico.svm, lubridate::year(fecha) == 2018)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = fecha, y = valor, col = tipo_dato)) +
  ggplot2::facet_wrap(~variable, ncol = 1) +
  ggplot2::scale_color_manual(name = "Tipo de dato", 
                              values = c("Normal" = "#000000", "Atípico" = "#ff0000", "Faltante" = "#0000ff"))
# ----------------------------------------------------------------------------------
