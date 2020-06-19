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
  