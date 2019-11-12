# ---------------------------------------------------------------------------------------#
# ---- Script para integración de set de datos para TP2 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(Cairo)
require(caret)
require(dplyr)
require(ggplot2)
require(ggbiplot)
require(GGally)
require(magrittr)
require(MASS)
require(purrr)
require(readr)
require(stringr)
require(tibble)
require(tidyr)

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

# Analisis de un grafo
adj.mat <- datos.entrada %>%
  dplyr::filter(sujeto == 1 & estadio == "N1") %>%
  dplyr::select(-sujeto, -estadio) %>%
  as.matrix()
densidad.media <- mean(adj.mat)

grafo <- igraph::graph.adjacency(adj.mat, mode="undirected", diag=FALSE, weighted = T)

densidad.umbrales <- purrr::map_dfr(
  .x = seq(from = 0, to = 1, by = 0.01),
  .f = function(umbral) {
    adj.mat.2 <- adj.mat
    adj.mat.2[which(adj.mat.2 < umbral)] <- 0
    adj.mat.2[which(adj.mat.2 >= umbral)] <- 1
    data.frame(umbral = umbral, densidad = mean(adj.mat.2))    
  }
) %>% dplyr::mutate(abs.dif = abs(densidad - densidad.media))

ggplot2::ggplot(data = densidad.umbrales) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = umbral, y = densidad)) +
  ggplot2::geom_hline(yintercept = mean(adj.mat), col = "tomato") +
  ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1))

mejor.umbral <- densidad.umbrales %>%
  dplyr::filter(abs.dif == min(abs.dif)) %>%
  dplyr::pull(umbral)

adj.mat.3 <- adj.mat
adj.mat.3[which(adj.mat.3 < mejor.umbral)] <- 0
adj.mat.3[which(adj.mat.3 >= mejor.umbral)] <- 1
grafo <- graph.adjacency(adj.mat.3, mode="undirected", diag=FALSE)

a<-igraph::degree(grafo)
hist(a)

# download.file("http://moreno.ss.uci.edu/beach.dat", destfile = "windsurfers.dat")
# ws <- read.table("windsurfers.dat", skip = 7)
# dim(ws)
# 
# ws.obs <- as.matrix(ws[1:43, ])
# ws.per <- as.matrix(ws[44:86, ])
# # image(ws.obs)
# # image(ws.per)
# 
# ws.obs.red <- graph.adjacency(ws.obs, mode="undirected", diag=FALSE, weighted = T)
# plot(ws.obs.red)
# 
# hist(ws.per[lower.tri(ws.per)], main = "histograma de las interacciones percibidas")
# 
# umbral <- 0.5
# ws.per.2 <- ws.per
# ws.per.2[which(ws.per.2 <= umbral)] <- 0
# ws.per.red <- graph.adjacency(ws.per.2, mode="undirected", diag=FALSE, weighted = T)
# image(ws.per.2)
# plot(ws.per.red)
# 
