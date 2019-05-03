# ----------------------------------------------------------------------------------
# --- Practica de laboratorio 02
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
require(kableExtra)
require(knitr)
require(readr)
require(rmarkdown)
require(tidyr)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Ejercicio 1a. Lectura de datos ----
# ---------------------------------------------------------------------------------#
# i. Lectura a data frame
ruidoso <- readr::read_csv(paste0("input/LAB02/ruidoso.txt"))
colnames(ruidoso) <- c('Observation', 'Road_55dB', 'Road_60dB', 'Railway_65dB', 'Industry_65dB')

# ii. Pasame a formato largo
ruidoso.largo <- ruidoso %>%
  tidyr::gather(key = Variable, value = Valor, -Observation)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Ejercicio 1b. Exploración ----
# ---------------------------------------------------------------------------------#
# i. Calculo de medidas de posicion
CalcularModa <- function(valores) {
  unicos <- unique(valores)
  return (unicos[which.max(tabulate(match(valores, unicos)))])
}
medidas.posicion.por.variable <- ruidoso.largo %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(Media = mean(Valor, na.rm = TRUE), 
                   Mediana = median(Valor, na.rm = TRUE), 
                   Moda = CalcularModa(Valor)) %>%
  dplyr::arrange(Variable) 

# ii. Graficos de medida de posicion
medidas.posicion.por.variable.largo <- medidas.posicion.por.variable %>%
  tidyr::gather(key = Metrica, value = Valor, -Variable)
grafico.medidas.posicion <- ggplot2::ggplot(data = medidas.posicion.por.variable.largo) +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = Metrica, y = Valor, fill = Metrica), stat = 'identity') +
  ggplot2::facet_wrap(~Variable, scale = "free", ncol = 1) +
  ggplot2::labs(title = "Dataset LAB02",
                subtitle = "Medidas de posición",
                x = "Región", y = "Valor", fill = "Región") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank()
  )

# iii. Histogramas
grafico.histogramas <- ggplot2::ggplot(data = ruidoso.largo) +
  ggplot2::geom_histogram(mapping = ggplot2::aes(x = Valor, fill = Variable)) +
  ggplot2::facet_wrap(~Variable, scales = "free", ncol = 1) +
  ggplot2::labs(title = "Dataset LAB02",
                subtitle = "Histogramas para cada variable medida",
                x = "Valor", y = "Cantidad", fill = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )

# iii. Calculo de medidas de dispersion por variable
medidas.dispersion.por.variable <- ruidoso.largo %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(Varianza = var(Valor, na.rm = TRUE), 
                   MAD = sd(Valor, na.rm = TRUE)) %>%
  dplyr::arrange(Variable)

# iv. Graficos de medida de dispersion
medidas.dispersion.por.variable.largo <- medidas.dispersion.por.variable %>%
  tidyr::gather(key = Metrica, value = Valor, -Variable)
grafico.medidas.dispersion <- ggplot2::ggplot(data = medidas.dispersion.por.variable.largo) +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = Metrica, y = Valor, fill = Metrica), stat = 'identity') +
  ggplot2::facet_wrap(~Variable, scale = "free", ncol = 1) +
  ggplot2::labs(title = "Dataset LAB02",
                subtitle = "Medidas de dispersión",
                x = "Variable", y = "Valor", fill = "Variable") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank()
  )

# iii. Histogramas
graficos.boxplot <- ggplot2::ggplot(data = datos.histogramas) +
  ggplot2::geom_boxplot(mapping = ggplot2::aes(x = W_Region, y = Valor, fill = W_Region)) +
  ggplot2::facet_grid(Variable~W_Region, scales = "free") +
  ggplot2::labs(title = "Métricas de pobreza multidimensionales",
                subtitle = "Histogramas para cada variable medida",
                x = "Valor", y = "Cantidad", fill = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Ejercicio 4. Medidas de asociación ----
# ---------------------------------------------------------------------------------#
# i. Grafico de medidas de asociación por region
graficos.asociacion <- purrr::map(
  .x = unique(datos.histogramas$W_Region),
  .f = function(region) {
    datos.region <- mpi.subnational %>%
      dplyr::filter(W_Region == region)
    grafico.region <- GGally::ggpairs(data = datos.region, columns = 5:ncol(datos.region)) +
      ggplot2::labs(title = "Métricas de pobreza multidimensionales",
                    subtitle = paste0("Medidas de asociación para ", region),
                    x = "", y = "") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.subtitle = ggplot2::element_text(hjust = 0.5)
      )
    return(grafico.region) 
  }
)
names(graficos.asociacion) <- unique(datos.histogramas$W_Region)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Generación de informe ----
# ---------------------------------------------------------------------------------#
rmarkdown::render(
  input       = "LAB01.Rmd",
  output_dir  = paste0(getwd(), "/output"),
  intermediates_dir = paste0(getwd(), "/output"),
  output_file = "LAB01.pdf",
  encoding = "UTF-8",
  clean = TRUE
)
# ----------------------------------------------------------------------------------
