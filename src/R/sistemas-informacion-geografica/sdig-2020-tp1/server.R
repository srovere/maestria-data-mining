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
  obtenerHogaresPorZonaInfluencia <- shiny::reactive({
    # Para cada poligono de influencia, calcular la cantidad de hogares NBI por establecimiento
    escala.limites           <- c(-Inf, 50, 100, 250, 500, Inf)
    escala.colores           <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
    escala.etiquetas         <- c('Hasta 50','De 51 a 100','De 101 a 250','De 250 a 500', 'Más de 500')
    hogares.nbi.por.poligono <- sf::st_set_geometry(censo, NULL) %>%
      dplyr::select(poligono_influencia_id, HOGARES, HOGARES_NBI) %>%
      dplyr::group_by(poligono_influencia_id) %>%
      dplyr::summarise(hogares = sum(HOGARES), hogares_nbi = sum(HOGARES_NBI))
    establecimientos.por.poligono <- sf::st_set_geometry(establecimientos, NULL) %>%
      dplyr::group_by(poligono_influencia_id) %>%
      dplyr::summarise(establecimientos = dplyr::n())
    hogares.establecimientos.poligonos <- influencia.establecimientos %>%
      dplyr::left_join(hogares.nbi.por.poligono, by = c("poligono_influencia_id")) %>%
      dplyr::left_join(establecimientos.por.poligono, by = c("poligono_influencia_id")) %>%
      dplyr::mutate(hogares = dplyr::if_else(! is.na(hogares), hogares, as.double(0)),
                    hogares_nbi = dplyr::if_else(! is.na(hogares_nbi), hogares_nbi, as.double(0)),
                    hogares_nbi_establecimiento = hogares_nbi / establecimientos,
                    escala = cut(x = hogares_nbi_establecimiento, breaks = escala.limites, labels = escala.colores)) %>%
      sf::st_transform(crs = proj4string.latlon)
    return (hogares.establecimientos.poligonos)
  })
  
  ## Porcentaje de hogares NBI
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
        leaflet::addPolygons(map = ., stroke = TRUE, opacity = 0.75, weight = 1, fillOpacity = 0.75, color = "#000000",
                             smoothFactor = 0.5, fillColor = ~escala, 
                             popup = ~sprintf("<b>%s</b><br/>Cantidad: %d<br/>Porcentaje: %.2f%%<br/>Densidad: %.2f/km²", nombre, cantidad, porcentaje, densidad)) %>%
        leaflet::fitBounds(map = ., lng1 = extent.zona["x", "min"], lng2 = extent.zona["x", "max"],
                           lat1 = extent.zona["y", "min"], lat2 = extent.zona["y", "max"]) %>%
        leaflet::addLegend(map = ., colors = escala.colores, labels = escala.etiquetas, position = "bottomright",
                           opacity = 1, title = Hmisc::capitalize(opcion))
    }
  })
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
                                   tooltip = list(pointFormat = 'Hogares NBI: <b>{point.cantidad}</b><br/>Porcentaje: <b>{point.porcentaje:.2f} %</b><br/>Densidad: <b>{point.densidad:.2f}/km²</b><br/>'),
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
  
  ## Cobertura educativa
  # Según zona de influencia
  output$mapaCoberturaInfluencia <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(map = ., urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = "SDIG-2020 TP1")
  })
  observe({
    if (input$menu == "cobertura_educativa_influencia") {
    hogares.establecimientos.poligonos <- obtenerHogaresPorZonaInfluencia()
      if (! is.null(hogares.establecimientos.poligonos)) {
        escala.colores   <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
        escala.etiquetas <- c('Hasta 50','De 51 a 100','De 101 a 250','De 250 a 500', 'Más de 500')
        extent.zona      <- sp::bbox(sf::as_Spatial(hogares.establecimientos.poligonos))
        proxy            <- leaflet::leafletProxy(mapId = "mapaCoberturaInfluencia", data = hogares.establecimientos.poligonos) %>%
          leaflet::clearControls(map = .) %>%
          leaflet::clearShapes(map = .) %>%
          leaflet::addPolygons(map = ., stroke = TRUE, opacity = 0.75, weight = 1, fillOpacity = 0.75, color = "#000000",
                               smoothFactor = 0.5, fillColor = ~escala, 
                               popup = ~sprintf("Establecimientos: %d<br/>Hogares NBI: %d<br/>Hogares NBI/establecimiento: %.2f", establecimientos, hogares_nbi, hogares_nbi_establecimiento)) %>%
          leaflet::fitBounds(map = ., lng1 = extent.zona["x", "min"], lng2 = extent.zona["x", "max"],
                             lat1 = extent.zona["y", "min"], lat2 = extent.zona["y", "max"]) %>%
          leaflet::addLegend(map = ., colors = escala.colores, labels = escala.etiquetas, position = "bottomright",
                             opacity = 1, title = "Hogares NBI por establecimiento")
      }
    }
  })
  output$graficoCoberturaInfluencia <- highcharter::renderHighchart({
    hogares.establecimientos.poligonos <- obtenerHogaresPorZonaInfluencia()
    if (! is.null(hogares.establecimientos.poligonos)) {
      # Elaborar CDF
      valores.hogares.nbi.establecimiento  <- sort(dplyr::pull(hogares.establecimientos.poligonos, hogares_nbi_establecimiento))
      func.cdf.hogares.nbi.establecimiento <- stats::ecdf(valores.hogares.nbi.establecimiento)
      cdf.hogares.nbi.establecimiento      <- purrr::map_dfr(
        .x = seq(from = 0, to = ceiling(max(valores.hogares.nbi.establecimiento))),
        .f = function(x) {
          y <- func.cdf.hogares.nbi.establecimiento(x)
          return (data.frame(x = log10(x), hogares = x, y = y))
        }
      )
      
      # Definir bandas
      escala.limites   <- c(0, 50, 100, 250, 500, ceiling(max(valores.hogares.nbi.establecimiento)))
      escala.colores   <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
      escala.etiquetas <- c('Hasta 50','De 51 a 100','De 101 a 250','De 250 a 500', 'Más de 500')
      plotBands        <- list()
      for (i in seq(from = 1, to = length(escala.colores))) {
        desde          <- ifelse(i == 1, 0, log10(escala.limites[i]))
        plotBands[[i]] <- list(from = desde, to = log10(escala.limites[i+1]), 
                               color = escala.colores[i], thickness = 30)
      }
      
      # Grafico
      grafico <- highcharter::highchart() %>%
        highcharter::hc_xAxis(title = list(text = "Log(Hogares NBI por establecimiento)", style = list(color = "#212121")),
                              plotBands = plotBands) %>%
        highcharter::hc_yAxis(title = list(text = "Probabilidad acumulada", style = list(color = "#212121")),
                              allowDecimals = FALSE, min = 0, max = 1) %>%
        highcharter::hc_chart(options3d = list(enabled = FALSE, beta = 15, alpha = 15),
                              style = list(backgroundColor = "#d8d8d8"),
                              zoomType = 'x', panning = TRUE, panKey = 'shift') %>%
        highcharter::hc_add_series(type = "line", data = cdf.hogares.nbi.establecimiento,
                                   mapping = highcharter::hcaes(x = x, y = y, hogares = hogares),
                                   tooltip = list(
                                     headerFormat = '<span style="font-size: 12px; font-weight: bold;">Hogares NBI por establecimiento</span><br/>',
                                     pointFormat = 'Hogares NBI: <b>{point.hogares:.2f}</b><br/>Probabilidad acumulada: <b>{point.y:.3f}</b>'
                                   )) %>%
        highcharter::hc_colors("#1b9e77") %>%
        highcharter::hc_tooltip(useHTML = TRUE, shared = TRUE) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_title(text = "CDF de hogares NBI por establecimiento educativo", style = list(color = "#212121")) %>%
        highcharter::hc_subtitle(text = "Ciudad Autónoma de Buenos Aires, Censo 2010", style = list(color = "#212121")) %>%
        highcharter::hc_exporting(enabled = TRUE, showTable = FALSE, buttons = list(
          contextButton = list(menuItems = ObtenerOpcionesExportacion(exportar.a.texto = FALSE))
        )) %>%
        highcharter::hc_plotOptions(
          series = list(
            line = list(
              lineWidth = "2px"
            )
          )
        )
      return (grafico)
    }
  })
})