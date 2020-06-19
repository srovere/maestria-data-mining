# -----------------------------------------------------------------------------#
# --- Censo CREA - Reporte para asesores (Server)
# -----------------------------------------------------------------------------#

shiny::shinyServer(function(input, output, session) {
  # Reactives
  obtenerHogaresNBIPorBarrio <- shiny::reactive({
    if (! is.null(input$opciones_hogares_nbi_barrio)) {
      # Porcentaje de hogares NBI por barrio
      opcion                 <- input$opciones_hogares_nbi_barrio
      escala.limites         <- escalas.hogares.nbi[[opcion]]$limites
      escala.colores         <- escalas.hogares.nbi[[opcion]]$colores
      hogares.nbi.por.barrio <- censo %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::group_by(barrio_id) %>%
        dplyr::summarise(cantidad = sum(HOGARES_NBI), hogares = sum(HOGARES))
      hogares.nbi.por.barrio <- barrios %>%
        dplyr::inner_join(hogares.nbi.por.barrio, by = c("barrio_id")) %>%
        dplyr::mutate(densidad = cantidad / area,
                      porcentaje = 100 * cantidad / hogares,
                      escala = cut(x = !! rlang::sym(opcion), breaks = escala.limites, labels = escala.colores)) %>%
        sf::st_transform(crs = proj4string.latlon)
      return (hogares.nbi.por.barrio)
    }
  })
  
  # Porcentaje de hogares NBI por barrio (mapa)
  output$mapaNBIBarrio <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(map = ., urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = "SDIG-2020 TP1")
  })
  observe({
    hogares.nbi.por.barrio <- obtenerHogaresNBIPorBarrio()
    if (! is.null(hogares.nbi.por.barrio)) {
      opcion           <- input$opciones_hogares_nbi_barrio
      escala.colores   <- escalas.hogares.nbi[[opcion]]$colores
      escala.etiquetas <- escalas.hogares.nbi[[opcion]]$etiquetas
      extent.zona      <- sp::bbox(sf::as_Spatial(hogares.nbi.por.barrio))
      proxy            <- leaflet::leafletProxy(mapId = "mapaNBIBarrio", data = hogares.nbi.por.barrio) %>%
        leaflet::clearControls(map = .) %>%
        leaflet::clearShapes(map = .) %>%
        leaflet::addPolygons(map = ., stroke = TRUE, opacity = 1.0, weight = 1, fillOpacity = 1, color = "#000000",
                             smoothFactor = 0.5, fillColor = ~escala, 
                             popup = ~sprintf("<b>%s</b><br/>Cantidad: %d<br/>Porcentaje: %.2f%%<br/>Densidad: %.2f/km²", nombre, cantidad, porcentaje, densidad)) %>%
        leaflet::fitBounds(map = ., lng1 = extent.zona["x", "min"], lng2 = extent.zona["x", "max"],
                           lat1 = extent.zona["y", "min"], lat2 = extent.zona["y", "max"]) %>%
        leaflet::addLegend(map = ., colors = escala.colores, labels = escala.etiquetas, position = "bottomright",
                           opacity = 1, title = Hmisc::capitalize(opcion))
    }
  })
  # Porcentaje de hogares NBI por barrio (grafico)
  output$graficoNBIBarrio <- highcharter::renderHighchart({
    hogares.nbi.por.barrio <- obtenerHogaresNBIPorBarrio()
    if (! is.null(hogares.nbi.por.barrio)) {
      opcion                 <- input$opciones_hogares_nbi_barrio
      hogares.nbi.por.barrio <- hogares.nbi.por.barrio %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::arrange(dplyr::desc(!! rlang::sym(opcion))) %>%
        dplyr::mutate(nombre = forcats::fct_reorder(nombre, dplyr::desc(!! rlang::sym(opcion))),
                      variable = !! rlang::sym(opcion))
      grafico <- highcharter::highchart() %>%
        highcharter::hc_xAxis(categories = levels(hogares.nbi.por.barrio$nombre), style = list(color = "#212121")) %>%
        highcharter::hc_yAxis(title = list(text = Hmisc::capitalize(opcion), style = list(color = "#212121")),
                              allowDecimals = FALSE) %>%
        highcharter::hc_chart(options3d = list(enabled = FALSE, beta = 15, alpha = 15),
                              style = list(backgroundColor = "#d8d8d8")) %>%
        highcharter::hc_add_series(type = "bar", data = hogares.nbi.por.barrio,
                                   mapping = highcharter::hcaes(x = nombre, y = variable, cantidad = cantidad, color = escala),
                                   tooltip = list(pointFormat = 'Hogares NBI: <b>{point.cantidad}</b><br/>Porcentaje: <b>{point.y:.2f} %</b><br/>Densidad: <b>{point.densidad:.2f}/km²</b><br/>'),
                                   dataLabels = list(
                                     list(enabled = TRUE, crop = FALSE, allowOverlap = TRUE, format = ifelse(opcion == "cantidad", "{y}", ifelse(opcion == "densidad", "{y:.2f}/km²", "{y:.2f}%")))
                                   )) %>%
        highcharter::hc_tooltip(useHTML = TRUE, shared = TRUE) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_title(text = "Hogares con necesidades básicas insatisfechas (NBI)", style = list(color = "#212121")) %>%
        highcharter::hc_subtitle(text = "Ciudad Autónoma de Buenos Aires, Censo 2010", style = list(color = "#212121")) %>%
        highcharter::hc_exporting(enabled = TRUE, showTable = FALSE, buttons = list(
          contextButton = list(menuItems = ObtenerOpcionesExportacion(exportar.a.texto = FALSE))
        )) %>%
        highcharter::hc_plotOptions(
          bar = list(
            borderWidth = 1,
            borderColor = "#7f7f7f",
            dataLabels = list(enabled = TRUE, color = "#212121", style = list(fontSize = "14px"))
          )
        )
      return (grafico)
    }
  })
})