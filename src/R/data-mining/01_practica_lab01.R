# ----------------------------------------------------------------------------------
# --- Practica de laboratorio 01
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
require(readr)
require(rmarkdown)
require(tidyr)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Lectura de datos ----
# ---------------------------------------------------------------------------------#
# i. MPI National
mpi.national           <- readr::read_csv(paste0("input/LAB01/MPI_national.csv"))
colnames(mpi.national) <- c(
  'ISO', 'Country', 
  'MPI_Urban', 'HR_Urban', 'IOD_Urban',
  'MPI_Rural', 'HR_Rural', 'IOD_Rural'
)

# ii. MPI Subnational
mpi.subnational           <- readr::read_csv(paste0("input/LAB01/MPI_subnational.csv"))
colnames(mpi.subnational) <- c(
  'ISO', 'Country', 'SN_Region', 'W_Region',
  'MPI_National', 'MPI_Regional',
  'HR_Regional', 'IOD_Regional'
)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Ejercicio 1. Exploración de datos ----
# ---------------------------------------------------------------------------------#
# i. Cantidad de ciudades por region (World Region)
ciudades.por.region <- mpi.subnational %>%
  dplyr::group_by(W_Region) %>%
  dplyr::summarise(Cantidad = dplyr::n()) %>%
  dplyr::arrange(W_Region)
grafico.ciudades.region <- ggplot2::ggplot(data = ciudades.por.region) +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = W_Region, y = Cantidad, fill = W_Region), stat = 'identity') +
  ggplot2::labs(title = "Métricas de pobreza multidimensionales",
                subtitle = "Cantidad de ciudades por región",
                x = "Región", y = "Cantidad de ciudades", fill = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Ejercicio 2. Medidas de posición ----
# ---------------------------------------------------------------------------------#
# i. Calculo de medidas de posicion por region
CalcularModa <- function(valores) {
  unicos <- unique(valores)
  return (unicos[which.max(tabulate(match(valores, unicos)))])
}
medidas.posicion.por.region <- mpi.subnational %>%
  dplyr::select(-ISO, -Country, -SN_Region) %>%
  tidyr::gather(key = Variable, value = Valor, -W_Region) %>%
  dplyr::group_by(W_Region, Variable) %>%
  dplyr::summarise(Media = mean(Valor, na.rm = TRUE), 
                   Mediana = median(Valor, na.rm = TRUE), 
                   Moda = CalcularModa(Valor)) %>%
  dplyr::arrange(W_Region, Variable) 

# ii. Graficos de medida de posicion
medidas.posicion.por.region.largo <- medidas.posicion.por.region %>%
  tidyr::gather(key = Medida, value = Valor, -W_Region, -Variable)
grafico.medidas.posicion <- ggplot2::ggplot(data = medidas.posicion.por.region.largo) +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = W_Region, y = Valor, fill = W_Region), stat = 'identity') +
  ggplot2::facet_grid(Variable~Medida, scale = "free") +
  ggplot2::labs(title = "Métricas de pobreza multidimensionales",
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
datos.histogramas <- mpi.subnational %>%
  dplyr::select(-ISO, -Country, -SN_Region) %>%
  tidyr::gather(key = Variable, value = Valor, -W_Region)
grafico.histogramas <- ggplot2::ggplot(data = datos.histogramas) +
  ggplot2::geom_histogram(mapping = ggplot2::aes(x = Valor, fill = W_Region)) +
  ggplot2::facet_grid(W_Region~Variable, scales = "free") +
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
# ---- Ejercicio 3. Medidas de dispersión ----
# ---------------------------------------------------------------------------------#
# i. Calculo de medidas de dispersion por region
medidas.dispersion.por.region <- mpi.subnational %>%
  dplyr::select(-ISO, -Country, -SN_Region) %>%
  tidyr::gather(key = Variable, value = Valor, -W_Region) %>%
  dplyr::group_by(W_Region, Variable) %>%
  dplyr::summarise(Varianza = var(Valor, na.rm = TRUE), 
                   DesviacionEstandar = sd(Valor, na.rm = TRUE),
                   DesviacionMedianaAbsoluta = sd(Valor, na.rm = TRUE), 
                   Minimo = min(Valor, na.rm = TRUE), 
                   Maximo = max(Valor, na.rm = TRUE), 
                   IQR = IQR(Valor, na.rm = TRUE)) %>%
  dplyr::arrange(W_Region, Variable)

# ii. Graficos de medida de posicion
medidas.dispersion.por.region.largo <- medidas.dispersion.por.region %>%
  tidyr::gather(key = Medida, value = Valor, -W_Region, -Variable)
grafico.medidas.dispersion <- ggplot2::ggplot(data = medidas.dispersion.por.region.largo) +
  ggplot2::geom_bar(mapping = ggplot2::aes(x = W_Region, y = Valor, fill = W_Region), stat = 'identity') +
  ggplot2::facet_grid(Variable~Medida, scale = "free") +
  ggplot2::labs(title = "Métricas de pobreza multidimensionales",
                subtitle = "Medidas de dispersión",
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
    GGally::ggpairs(data = datos.region, columns = 5:ncol(datos.region)) +
      ggplot2::labs(title = "Métricas de pobreza multidimensionales",
                    subtitle = paste0("Medidas de asociación para ", region),
                    x = "", y = "") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.subtitle = ggplot2::element_text(hjust = 0.5)
      ) 
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
  output_file = "LAB01.pdf"
)
# ----------------------------------------------------------------------------------
