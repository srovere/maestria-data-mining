# Borrar variables del ambiente
rm(list = objects())

# Carga de paquetes necesarios para hacer los gráficos
require(Cairo)
require(dplyr)
require(highcharter)
require(Hmisc)
require(leaflet)
require(sf)
require(shiny)
require(shinycssloaders)
require(shinydashboard)
require(shinyjs)
require(sp)

# Uso de Cairo para renderizar los gráficos
options(bitmapType = "cairo")

# Cargar dataset
load("Dataset.RData")

# Definicion de proyeccion lat-long
proj4string.latlon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

# Items de menu de exportacion
ObtenerOpcionesExportacion <- function(exportar.a.imagen = TRUE, exportar.a.texto = TRUE) {
  export.items <- list()
  if (exportar.a.imagen) {
    export.items <- append(export.items, list(
      list(text="Exportar a PNG", onclick=JS("function () { this.exportChart({ type: 'image/png' }); }")),
      list(text="Exportar a JPG", onclick=JS("function () { this.exportChart({ type: 'image/jpeg' }); }")),
      list(text="Exportar a SVG", onclick=JS("function () { this.exportChart({ type: 'image/svg+xml' }); }")),
      list(text="Exportar a PDF", onclick=JS("function () { this.exportChart({ type: 'application/pdf' }); }"))))
  }
  if (exportar.a.texto) {
    if (exportar.a.imagen) {
      export.items <- append(export.items, list(list(separator=TRUE)))
    }
    export.items <- append(export.items, list(
      list(text="Exportar a XLS", onclick=JS("function () { this.downloadXLS(); }"))))
  }
  return (export.items)
}

# Obtencion de hogares NBI por zonas de influencia
ObtenerHogaresPorZonaInfluencia <- function(solo.escuelas.publicas = TRUE) {
  # Para cada poligono de influencia, calcular la cantidad de hogares NBI por establecimiento
  escala.limites           <- c(-Inf, 50, 100, 250, 500, Inf)
  escala.colores           <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
  escala.etiquetas         <- c('Hasta 50','De 51 a 100','De 101 a 250','De 250 a 500', 'Más de 500')
  hogares.nbi.por.poligono <- sf::st_set_geometry(censo, NULL) %>%
    dplyr::select(poligono_influencia_id, HOGARES, HOGARES_NBI) %>%
    dplyr::group_by(poligono_influencia_id) %>%
    dplyr::summarise(hogares = sum(HOGARES), hogares_nbi = sum(HOGARES_NBI))
  
  if (! solo.escuelas.publicas) {
    establecimientos.base <- rbind(establecimientos, establecimientos.privados)
  } else {
    establecimientos.base <- establecimientos  
  }
  establecimientos.por.poligono <- sf::st_set_geometry(establecimientos.base, NULL) %>%
    dplyr::group_by(poligono_influencia_id, barrio_id) %>%
    dplyr::summarise(establecimientos = dplyr::n(),
                     nombres = paste0(NOMBRE_EST, collapse = "<br>"))
  
  hogares.establecimientos.poligonos <- influencia.establecimientos %>%
    dplyr::left_join(hogares.nbi.por.poligono, by = c("poligono_influencia_id")) %>%
    dplyr::left_join(establecimientos.por.poligono, by = c("poligono_influencia_id")) %>%
    dplyr::mutate(hogares = dplyr::if_else(! is.na(hogares), hogares, as.double(0)),
                  hogares_nbi = dplyr::if_else(! is.na(hogares_nbi), hogares_nbi, as.double(0)),
                  hogares_nbi_establecimiento = hogares_nbi / establecimientos,
                  escala = cut(x = hogares_nbi_establecimiento, breaks = escala.limites, labels = escala.colores)) %>%
    sf::st_transform(crs = proj4string.latlon)
  return (hogares.establecimientos.poligonos)
}

# Calcular oferta/demanda de hogares NBI por barrios
CalcularOfertaDemandaHogaresNBI <- function(hogares.establecimientos.poligonos, por.comuna = FALSE) {
  # Hogares NBI por barrio (demanda)
  hogares.nbi.por.barrio <- censo %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_by(barrio_id) %>%
    dplyr::summarise(demanda = sum(HOGARES_NBI))
  
  # Hogares NBI por poligono (oferta)
  hogares.nbi.por.barrio.establecimiento <- hogares.establecimientos.poligonos %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_by(barrio_id) %>%
    dplyr::summarise(oferta = sum(hogares_nbi))
  
  # Agrupar por barrio o comuna
  hogares.nbi.oferta.demanda <- NULL
  if (por.comuna) {
    hogares.nbi.oferta.demanda <- barrios %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::select(barrio_id, comuna) %>%
      dplyr::mutate(comuna = paste0("Comuna ", comuna)) %>%
      dplyr::left_join(hogares.nbi.por.barrio, by = c("barrio_id")) %>%
      dplyr::left_join(hogares.nbi.por.barrio.establecimiento, by = c("barrio_id")) %>%
      dplyr::mutate(oferta = dplyr::if_else(! is.na(oferta), oferta, as.double(0)),
                    demanda = dplyr::if_else(! is.na(demanda), demanda, as.double(0))) %>%
      dplyr::group_by(comuna) %>%
      dplyr::summarise(oferta = sum(oferta), demanda = sum(demanda)) %>%
      dplyr::mutate(diferencia = oferta - demanda) %>%
      dplyr::arrange(dplyr::desc(diferencia)) %>%
      dplyr::mutate(nombre = factor(comuna, levels = comuna))
  } else {
    hogares.nbi.oferta.demanda <- barrios %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::select(barrio_id, nombre) %>%
      dplyr::left_join(hogares.nbi.por.barrio, by = c("barrio_id")) %>%
      dplyr::left_join(hogares.nbi.por.barrio.establecimiento, by = c("barrio_id")) %>%
      dplyr::mutate(oferta = dplyr::if_else(! is.na(oferta), oferta, as.double(0)),
                    demanda = dplyr::if_else(! is.na(demanda), demanda, as.double(0)),
                    diferencia = oferta - demanda) %>%
      dplyr::arrange(dplyr::desc(diferencia)) %>%
      dplyr::mutate(nombre = forcats::fct_reorder(nombre, dplyr::desc(diferencia)))
  }
  return (hogares.nbi.oferta.demanda)
}

# Definicion de escalas para hogares NBI
escalas.hogares.nbi <- list(
  "porcentaje" = list(
    "limites"   = c(-Inf, 1, 2, 5, 10, 15, 20, Inf),
    "colores"   = c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026'),
    "etiquetas" = c('Hasta 1%','De 1% a 2%','De 2% a 5%','De 5% a 10%', 'De 10% a 15%', 'De 15% a 20%', 'Más de 20%')
  ),
  "cantidad" = list(
    "limites"   = c(-Inf, 250, 500, 1000, 2500, Inf),
    "colores"   = c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026'),
    "etiquetas" = c('Hasta 250', 'De 251 a 500', 'De 501 a 1000', 'De 1001 a 2500', 'Más de 2500')
  ),
  "densidad" = list(
    "limites"   = c(-Inf, 75, 150, 250, 600, Inf),
    "colores"   = c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026'),
    "etiquetas" = c('Hasta 75/km²', 'De 75/km² a 150/km²', 'De 150/km² a 250/km²', 'De 250/km² a 600/km²', 'Más de 600/km²')
  )
)

# Definicion de escalas para conectividad
escalas.conectividad <- list(
  "cantidad" = list(
    "limites"   = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
    "colores"   = rev(c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')),
    "etiquetas" = c('Sin paradas','1 parada','2 paradas','3 paradas', '4 paradas', '5 paradas', 'Más de 5 paradas')
  )
)