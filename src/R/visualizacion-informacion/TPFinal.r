rm(list = objects())

require(dplyr)
require(ggnet)
require(readr)
require(networkD3)

# Lectura de dataset
if (! file.exists("VASTDataset.RData")) {
  archivos.csv <- list.files(path = "vast", pattern = "*.csv", recursive = TRUE) %>%
    stringr::str_match(string = ., pattern = "(Person(\\d+)/Person\\d+_(\\d+)\\.csv)") %>%
    as.data.frame() %>%
    dplyr::rename(person_id = V3, photo_id = V4, file = V1) %>%
    dplyr::mutate(file = as.character(file)) %>%
    dplyr::select(file, person_id, photo_id)
  
  datos <- purrr::pmap_dfr(
    .l = archivos.csv,
    .f = function(file, person_id, photo_id) {
      contenido <- readr::read_csv(file = paste0(getwd(), "/vast/", file))
      if (("Label" %in% colnames(contenido))) {
        contenido <- contenido %>%
          dplyr::select(Label, Score) %>%
          dplyr::mutate(person_id = person_id, photo_id = photo_id)
        return (contenido)
      } else {
        warning(file)
        return (NULL)
      }
    }
  )
  save(datos, file = "VASTDataset.RData")
} else {
  load("VASTDataset.RData")
}

# Generar nodos. Hay que modificar el umbral!!
umbral <- 0.8
nodes <- rbind(
  datos %>%
    dplyr::filter(Score >= umbral) %>%
    dplyr::distinct(datos, person_id) %>%
    dplyr::mutate(Name = paste0("Person_", person_id), Group = 'P', Size = 5, Grado = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(Name, Group, Size, Grado),
  pesos <- datos %>%
    dplyr::filter(Score >= umbral) %>%
    dplyr::rename(Name = Label) %>%
    dplyr::group_by(Name) %>%
    dplyr::summarise(Grado = dplyr::n_distinct(person_id)) %>%
    dplyr::mutate(Group = dplyr::if_else(Grado >= 8, 'C', 'O'), Size = 5) %>%
    dplyr::select(Name, Group, Size, Grado)
) %>% dplyr::mutate(ID = dplyr::row_number() - 1) %>%
  as.data.frame()
  
# Generar Links
links <- datos %>%
  dplyr::filter(Score >= umbral) %>%
  dplyr::mutate(Source = paste0("Person_", person_id), Target = Label) %>%
  dplyr::select(Source, Target) %>%
  dplyr::inner_join(nodes, by = c("Source" = "Name")) %>%
  dplyr::select(-Group, -Size) %>%
  dplyr::rename(SourceID = ID) %>%
  dplyr::inner_join(nodes, by = c("Target" = "Name")) %>%
  dplyr::select(-Group, -Size) %>%
  dplyr::rename(TargetID = ID) %>%
  dplyr::mutate(Value = 1) %>%
  as.data.frame()

# Eliminar nombres de nodos con grado < 8
nodes <- nodes %>%
  dplyr::mutate(Name = dplyr::if_else(Grado >= 8, paste0(Name, " (", Grado, ")"), ""),
                Size = dplyr::if_else(Grado >= 8, 10, 5))

# Colores
colourScale <- JS('d3.scaleOrdinal()
            .domain(["C", "O", "P"])
           .range(["#e41a1c", "#377eb8", "#4daf4a"]);')

# Generar grafo
grafo <- networkD3::forceNetwork(Links = links, Nodes = nodes, colourScale = colourScale,
                                 Source = "SourceID", Target = "TargetID", fontSize = 18,
                                 Value = "Value", NodeID = "Name", opacityNoHover = 1,
                                 Group = "Group", Nodesize = "Size", charge = -50,
                                 radiusCalculation = htmlwidgets::JS("d.nodesize"),
                                 opacity = 1, width = 800, height = 800, bounded = TRUE)
grafo

# Separacion de personas y objetos
personas <- dplyr::distinct(datos, person_id)
objetos  <- dplyr::distinct(datos, Label)


# Heatmap (por maximo score)
colores      <- c('#cccccc','#ffffff','#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
limites      <- c(0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
maximo.score <- datos %>%
  dplyr::group_by(Label, person_id) %>%
  dplyr::summarise(MaxScore = max(Score, na.rm = TRUE)) %>%
  dplyr::mutate(Etiqueta = cut(x = MaxScore, breaks = limites))
etiquetas    <- c("Sin asociación", levels(maximo.score$Etiqueta))
heatmap.maximo.score <- tidyr::crossing(personas, objetos) %>%
  dplyr::left_join(maximo.score, by = c("person_id", "Label")) %>%
  dplyr::mutate(person_id = factor(person_id, levels = as.character(sort(unique(as.integer(person_id))))),
                Etiqueta = factor(dplyr::if_else(! is.na(Etiqueta), as.character(Etiqueta), "Sin asociación"),
                                  levels = etiquetas))
ggplot2::ggplot(data = heatmap.maximo.score) +
  ggplot2::geom_tile(mapping = ggplot2::aes(x = person_id, y = Label, fill = Etiqueta), colour = "black") +
  ggplot2::scale_fill_manual(values = colores, labels = c(levels(heatmap.maximo.score$Etiqueta))) +
  ggplot2::labs(x = "ID de persona", y = "Objeto", fill = "Máximo score",
                title = "Asociación de personas con objetos", 
                subtitle = "El color representa el máximo score de la asociación") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    panel.grid.major = ggplot2::element_line(colour = "white")
  )

### Datos corregidos
datos.corregidos <- readr::read_delim(file = "Ocurrencias.csv", delim = ",", col_names = TRUE) %>%
  dplyr::rename(person_id = id, frecuencia_absoluta = imgs) %>%
  tidyr::pivot_longer(names_to = "Label", values_to = "ocurrencias", cols = c(-person_id, -frecuencia_absoluta)) %>%
  dplyr::filter(! is.na(ocurrencias) & (ocurrencias > 0)) %>%
  dplyr::mutate(Score = ocurrencias / frecuencia_absoluta)

umbral <- 0.161
nodes  <- rbind(
  datos.corregidos %>%
    dplyr::filter(Score >= umbral) %>%
    dplyr::distinct(person_id) %>%
    dplyr::mutate(Name = paste0("Person_", person_id), Group = 'P', Size = 5, Grado = 1) %>%
    dplyr::select(Name, Group, Size, Grado),
  datos.corregidos %>%
    dplyr::filter(Score >= umbral) %>%
    dplyr::rename(Name = Label) %>%
    dplyr::group_by(Name) %>%
    dplyr::summarise(Grado = dplyr::n_distinct(person_id)) %>%
    dplyr::mutate(Group = dplyr::if_else(Grado >= 8, 'C', 'O'), Size = 5) %>%
    dplyr::select(Name, Group, Size, Grado)
) %>% dplyr::mutate(ID = dplyr::row_number() - 1) %>%
  as.data.frame()

# Generar Links
links <- datos.corregidos %>%
  dplyr::filter(Score >= umbral) %>%
  dplyr::mutate(Source = paste0("Person_", person_id), Target = Label) %>%
  dplyr::select(Source, Target) %>%
  dplyr::inner_join(nodes, by = c("Source" = "Name")) %>%
  dplyr::select(-Group, -Size) %>%
  dplyr::rename(SourceID = ID) %>%
  dplyr::inner_join(nodes, by = c("Target" = "Name")) %>%
  dplyr::select(-Group, -Size) %>%
  dplyr::rename(TargetID = ID) %>%
  dplyr::mutate(Value = 1) %>%
  as.data.frame()

# Eliminar nombres de nodos con grado < 8
nodes <- nodes %>%
  dplyr::mutate(Name = dplyr::if_else(Grado >= 8, paste0(Name, " (", Grado, ")"), ""),
                Size = dplyr::if_else(Grado >= 8, 10, 5))

# Colores
colourScale <- JS('d3.scaleOrdinal()
            .domain(["C", "O", "P"])
           .range(["#e41a1c", "#377eb8", "#4daf4a"]);')

# Generar grafo
grafo <- networkD3::forceNetwork(Links = links, Nodes = nodes, colourScale = colourScale,
                                 Source = "SourceID", Target = "TargetID", fontSize = 18,
                                 Value = "Value", NodeID = "Name", opacityNoHover = 1,
                                 linkColour = "#dbdbdb",
                                 Group = "Group", Nodesize = "Size", charge = -300,
                                 radiusCalculation = htmlwidgets::JS("d.nodesize"),
                                 opacity = 1, width = 800, height = 800, bounded = TRUE)
grafo
