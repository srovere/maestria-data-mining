# -----------------------------------------------------------------------------#
# --- Censo CREA - Reporte para asesores (Server)
# -----------------------------------------------------------------------------#

shiny::shinyServer(function(input, output, session) {
  # Reactives
  obtenerHogaresNBIPorBarrio <- shiny::reactive({
    # Porcentaje de hogares NBI por barrio
    escala.limites         <- c(-Inf, 1, 2, 5, 10, 15, 20, Inf)
    escala.colores         <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
    hogares.nbi.por.barrio <- censo %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::group_by(barrio_id) %>%
      dplyr::summarise(hogares_nbi = sum(HOGARES_NBI), hogares = sum(HOGARES)) %>%
      dplyr::mutate(porcentaje = 100 * hogares_nbi / hogares,
                    escala = cut(x = porcentaje, breaks = escala.limites, labels = escala.colores))
    hogares.nbi.por.barrio <- barrios %>%
      dplyr::inner_join(hogares.nbi.por.barrio, by = c("barrio_id")) %>%
      sf::st_transform(crs = proj4string.latlon)
    return (hogares.nbi.por.barrio)
  })
  
  # Porcentaje de hogares NBI por barrio
  output$mapaNBIBarrio <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(map = ., urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = "SDIG-2020 TP1")
  })
  observe({
    hogares.nbi.por.barrio <- obtenerHogaresNBIPorBarrio()
    if (! is.null(hogares.nbi.por.barrio)) {
      escala.colores   <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
      escala.etiquetas <- c('Hasta 1%','De 1% a 2%','De 2% a 5%','De 5% a 10%', 'De 10% a 15%', 'De 15% a 20%', 'Más de 20%')
      extent.zona      <- sp::bbox(sf::as_Spatial(hogares.nbi.por.barrio))
      proxy            <- leaflet::leafletProxy(mapId = "mapaNBIBarrio", data = hogares.nbi.por.barrio) %>%
        leaflet::addPolygons(map = ., stroke = TRUE, opacity = 1.0, weight = 1, fillOpacity = 1, color = "#000000",
                             smoothFactor = 0.5, fillColor = ~escala, 
                             popup = ~sprintf("%s: %.2f%%", nombre, porcentaje)) %>%
        leaflet::fitBounds(map = ., lng1 = extent.zona["x", "min"], lng2 = extent.zona["x", "max"],
                           lat1 = extent.zona["y", "min"], lat2 = extent.zona["y", "max"]) %>%
        leaflet::addLegend(map = ., colors = escala.colores, labels = escala.etiquetas, position = "bottomright",
                           opacity = 1, title = "Porcentaje")
    }
  })
})