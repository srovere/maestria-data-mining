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
  obtenerHogaresPorZonaInfluenciaSoloPublicos <- shiny::reactive({
    ObtenerHogaresPorZonaInfluencia()
  })
  obtenerHogaresPorZonaInfluencia <- shiny::reactive({
    if (! is.null(input$agregar_establecimientos_privados)) {
      ObtenerHogaresPorZonaInfluencia(! input$agregar_establecimientos_privados)
    }
  })
  obtenerConectividadEscuelas <- shiny::reactive({
    if (! is.null(input$distancia_maxima_conectividad)) {
      withProgress({
        establecimientos.buffer <- establecimientos %>%
          sf::st_buffer(x = ., dist = input$distancia_maxima_conectividad)
        conectividad.paradas    <- sf::st_contains(x = establecimientos.buffer, y = paradas.transporte.publico) %>%
          purrr::imap_dfr(
            .x = .,
            .f = function(ids.paradas, i) {
              data.frame(parada_id = ids.paradas) %>%
                dplyr::mutate(establecimiento_id = as.integer(establecimientos.buffer[i, ]$establecimiento_id))
            }
          )
        conectividad.cantidad    <- establecimientos.buffer %>%
          dplyr::left_join(conectividad.paradas, by = c("establecimiento_id")) %>%
          dplyr::group_by(establecimiento_id) %>%
          dplyr::summarise(cantidad = sum(! is.na(parada_id))) %>%
          sf::st_set_geometry(NULL)
        
        escala.limites           <- escalas.conectividad$cantidad$limites
        escala.colores           <- escalas.conectividad$cantidad$colores
        conectividad             <- establecimientos %>%
          dplyr::inner_join(conectividad.cantidad, by = c("establecimiento_id")) %>%
          dplyr::mutate(escala = cut(x = cantidad, breaks = escala.limites, labels = escala.colores)) %>%
          sf::st_transform(crs = proj4string.latlon) %>%
          dplyr::mutate(center_x = sf::st_coordinates(geometry)[,1], center_y = sf::st_coordinates(geometry)[,2])
        return (conectividad)
      }, message = sprintf("Calculando conectividad por escuela para un radio de %dm...", input$distancia_maxima_conectividad), value = NULL)
    }
  })
  obtenerLongitudSenderoPorBarrio <- shiny::reactive({
    escala.limites            <- escalas.senderos$longitud.barrio$limites
    escala.colores            <- escalas.senderos$longitud.barrio$colores
    longitud.senderos         <- senderos.escolares %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::group_by(barrio_id) %>%
      dplyr::summarise(longitud_total = sum(longitud),
                       escala = cut(x = longitud_total/1000, breaks = escala.limites, labels = escala.colores))
    longitud.senderos.barrios <- barrios %>%
      dplyr::inner_join(longitud.senderos, by = c("barrio_id")) %>%
      sf::st_transform(crs = proj4string.latlon)
    return (longitud.senderos.barrios)
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
    if (input$menu == "cobertura_educativa") {
      hogares.establecimientos.poligonos <- obtenerHogaresPorZonaInfluencia()
      if (! is.null(hogares.establecimientos.poligonos)) {
        escala.colores   <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
        escala.etiquetas <- c('Hasta 50','De 51 a 100','De 101 a 250','De 250 a 500', 'Más de 500')
        extent.zona      <- sp::bbox(sf::as_Spatial(hogares.establecimientos.poligonos))
        proxy            <- leaflet::leafletProxy(mapId = "mapaCoberturaInfluencia", data = hogares.establecimientos.poligonos) %>%
          leaflet::clearShapes(map = .) %>%
          leaflet::clearControls(map = .) %>%
          leaflet::addPolygons(map = ., stroke = TRUE, opacity = 1, weight = 1, fillOpacity = 1, color = "#000000",
                               smoothFactor = 0.5, fillColor = ~escala,
                               popup = ~sprintf("<b>%s</b><br/>Hogares NBI: %d<br/>Hogares NBI/establecimiento: %.2f", nombres, hogares_nbi, hogares_nbi_establecimiento)) %>%
          leaflet::fitBounds(map = ., lng1 = extent.zona["x", "min"], lng2 = extent.zona["x", "max"],
                             lat1 = extent.zona["y", "min"], lat2 = extent.zona["y", "max"]) %>%
          leaflet::addLegend(map = ., colors = escala.colores, labels = escala.etiquetas, position = "bottomright",
                             opacity = 1, title = "Hogares NBI por establecimiento")
      }
    }
  })
  output$cdfCoberturaInfluencia <- highcharter::renderHighchart({
    hogares.establecimientos.poligonos.todos    <- obtenerHogaresPorZonaInfluencia()
    hogares.establecimientos.poligonos.publicos <- obtenerHogaresPorZonaInfluenciaSoloPublicos()
    if (! is.null(hogares.establecimientos.poligonos.todos) && ! is.null(hogares.establecimientos.poligonos.publicos)) {
      # Elaborar CDF
      tipos.establecimiento              <- list("Solamente escuelas públicas", "Con agregado de escuelas privadas")
      hogares.establecimientos.poligonos <- list(hogares.establecimientos.poligonos.publicos, hogares.establecimientos.poligonos.todos)
      max.valor                          <- -Inf
      cantidad.tipos                     <- ifelse(input$agregar_establecimientos_privados, 2, 1)
      cdf.hogares.nbi.establecimiento    <- purrr::map_dfr(
        .x = seq_len(cantidad.tipos),
        .f = function(i) {
          valores.hogares.nbi.establecimiento  <- sort(dplyr::pull(hogares.establecimientos.poligonos[[i]], hogares_nbi_establecimiento))
          max.valor                            <<- max(max(valores.hogares.nbi.establecimiento), max.valor)
          func.cdf.hogares.nbi.establecimiento <- stats::ecdf(valores.hogares.nbi.establecimiento)
          cdf.hogares.nbi.establecimiento      <- purrr::map_dfr(
            .x = seq(from = 0, to = ceiling(max(valores.hogares.nbi.establecimiento))),
            .f = function(x) {
              y <- func.cdf.hogares.nbi.establecimiento(x)
              return (data.frame(serie = tipos.establecimiento[[i]], x = log10(x), hogares = x, y = y))
            }
          )   
        }
      )
      
      # Definir bandas
      escala.limites   <- c(0, 50, 100, 250, 500, ceiling(max.valor))
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
                                   mapping = highcharter::hcaes(x = x, y = y, hogares = hogares, group = serie),
                                   tooltip = list(
                                     headerFormat = '<span style="font-size: 12px; font-weight: bold;">Hogares NBI por establecimiento</span><br/>',
                                     pointFormat = '<b>{series.name}</b>. Hogares NBI: <b>{point.hogares:.2f}</b>. Probabilidad acumulada: <b>{point.y:.3f}</b><br/>'
                                   )) %>%
        highcharter::hc_colors(c("#1b9e77", "#7570b3")) %>%
        highcharter::hc_tooltip(useHTML = TRUE, shared = TRUE) %>%
        highcharter::hc_legend(enabled = TRUE) %>%
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
  
  # Oferta/demanda educativa
  output$barriosOfertaDemanda <- highcharter::renderHighchart({
    hogares.establecimientos.poligonos <- obtenerHogaresPorZonaInfluenciaSoloPublicos()
    if (! is.null(hogares.establecimientos.poligonos)) {
      hogares.nbi.oferta.demanda <- CalcularOfertaDemandaHogaresNBI(hogares.establecimientos.poligonos)
      
      # Grafico
      grafico <- highcharter::highchart() %>%
        highcharter::hc_xAxis(categories = levels(hogares.nbi.oferta.demanda$nombre), style = list(color = "#212121"),
                              labels = list(rotation = 0)) %>%
        highcharter::hc_yAxis(title = list(text = "Diferencia oferta/demanda", style = list(color = "#212121")),
                              allowDecimals = FALSE) %>%
        highcharter::hc_chart(options3d = list(enabled = FALSE, beta = 15, alpha = 15), inverted = TRUE,
                              style = list(backgroundColor = "#d8d8d8")) %>%
        highcharter::hc_add_series(type = "column", data = hogares.nbi.oferta.demanda, name = "Diferencia entre hogares NBI y cantidad atendida por establecimientos del barrio",
                                   mapping = highcharter::hcaes(x = nombre, y = diferencia),
                                   tooltip = list(
                                     pointFormat = 'Oferta: <b>{point.oferta}</b><br/>Demanda: <b>{point.demanda}</b>'
                                   )) %>%
        highcharter::hc_tooltip(useHTML = TRUE) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_title(text = "Diferencia entre hogares NBI y cantidad atendida por establecimientos del barrio", style = list(color = "#212121")) %>%
        highcharter::hc_subtitle(text = "Ciudad Autónoma de Buenos Aires, Censo 2010", style = list(color = "#212121")) %>%
        highcharter::hc_exporting(enabled = TRUE, showTable = FALSE, buttons = list(
          contextButton = list(menuItems = ObtenerOpcionesExportacion(exportar.a.texto = FALSE))
        )) %>%
        highcharter::hc_plotOptions(
          bar = list(
            borderWidth = 1,
            borderColor = "#7f7f7f",
            dataLabels = list(enabled = TRUE, color = "#212121", style = list(fontSize = "14px"))
          ),
          series = list(
            negativeColor = "#d73027",
            color = "#4575b4"
          )
        )
      return (grafico)
    }
  })
  output$comunasOfertaDemanda <- highcharter::renderHighchart({
    hogares.establecimientos.poligonos <- obtenerHogaresPorZonaInfluenciaSoloPublicos()
    if (! is.null(hogares.establecimientos.poligonos)) {
      hogares.nbi.oferta.demanda <- CalcularOfertaDemandaHogaresNBI(hogares.establecimientos.poligonos, por.comuna = TRUE)
      
      # Grafico
      grafico <- highcharter::highchart() %>%
        highcharter::hc_xAxis(categories = levels(hogares.nbi.oferta.demanda$nombre), style = list(color = "#212121"),
                              labels = list(rotation = 0)) %>%
        highcharter::hc_yAxis(title = list(text = "Diferencia oferta/demanda", style = list(color = "#212121")),
                              allowDecimals = FALSE) %>%
        highcharter::hc_chart(options3d = list(enabled = FALSE, beta = 15, alpha = 15), inverted = TRUE,
                              style = list(backgroundColor = "#d8d8d8")) %>%
        highcharter::hc_add_series(type = "column", data = hogares.nbi.oferta.demanda, name = "Diferencia entre hogares NBI y cantidad atendida por establecimientos del barrio",
                                   mapping = highcharter::hcaes(x = nombre, y = diferencia),
                                   tooltip = list(
                                     pointFormat = 'Oferta: <b>{point.oferta}</b><br/>Demanda: <b>{point.demanda}</b>'
                                   )) %>%
        highcharter::hc_tooltip(useHTML = TRUE) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_title(text = "Diferencia entre hogares NBI y cantidad atendida por establecimientos del comuna", style = list(color = "#212121")) %>%
        highcharter::hc_subtitle(text = "Ciudad Autónoma de Buenos Aires, Censo 2010", style = list(color = "#212121")) %>%
        highcharter::hc_exporting(enabled = TRUE, showTable = FALSE, buttons = list(
          contextButton = list(menuItems = ObtenerOpcionesExportacion(exportar.a.texto = FALSE))
        )) %>%
        highcharter::hc_plotOptions(
          bar = list(
            borderWidth = 1,
            borderColor = "#7f7f7f",
            dataLabels = list(enabled = TRUE, color = "#212121", style = list(fontSize = "14px"))
          ),
          series = list(
            negativeColor = "#d73027",
            color = "#4575b4"
          )
        )
      return (grafico)
    }
  })
  
  # Conectividad
  output$mapaConectividad <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(map = ., urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = "SDIG-2020 TP1")
  })
  observe({
    if (input$menu == "conectividad") {
      conectividad <- obtenerConectividadEscuelas()
      if (! is.null(conectividad)) {
        escala.colores        <- escalas.conectividad$cantidad$colores
        escala.etiquetas      <- escalas.conectividad$cantidad$etiquetas
        extent.zona           <- sp::bbox(sf::as_Spatial(conectividad))
        rownames(extent.zona) <- c("x", "y")
        proxy                 <- leaflet::leafletProxy(mapId = "mapaConectividad", data = conectividad) %>%
        leaflet::clearShapes(map = .) %>%
          leaflet::clearControls(map = .) %>%
          leaflet::addCircleMarkers(map = ., stroke = TRUE, opacity = 1, weight = 1, fillOpacity = 1, color = "#000000",
                                    fillColor = ~escala, lng = ~center_x, lat = ~center_y,
                                    popup = ~sprintf("<b>%s</b><br/>Cantidad de paradas cercanas: %d", NOMBRE_EST, cantidad)) %>%
          leaflet::fitBounds(map = ., lng1 = extent.zona["x", "min"], lng2 = extent.zona["x", "max"],
                             lat1 = extent.zona["y", "min"], lat2 = extent.zona["y", "max"]) %>%
          leaflet::addLegend(map = ., colors = escala.colores, labels = escala.etiquetas, position = "bottomright",
                             opacity = 1, title = "Paradas dentro del radio")
      }
    }
  })
  output$boxplotsConectividad <- highcharter::renderHighchart({
    conectividad <- obtenerConectividadEscuelas()
    if (! is.null(conectividad)) {
      conectividad.barrios <- sf::st_set_geometry(barrios, NULL) %>%
        dplyr::inner_join(dplyr::select(sf::st_set_geometry(conectividad, NULL), barrio_id, cantidad), by = c("barrio_id"))
      medianas             <- conectividad.barrios %>%
        dplyr::group_by(nombre) %>%
        dplyr::summarise(mediana = median(cantidad)) %>%
        dplyr::arrange(dplyr::desc(mediana)) %>%
        dplyr::mutate(nombre = factor(nombre, levels = nombre))
      conectividad.barrios <- conectividad.barrios %>%
        dplyr::mutate(nombre = factor(nombre, levels = levels(medianas$nombre)))

      # Grafico
      grafico <- highcharter::hcboxplot(x = conectividad.barrios$cantidad, var = conectividad.barrios$nombre, outliers = FALSE,
                                        name = "Distribución de paradas por establecimiento educativo") %>%
        highcharter::hc_xAxis(categories = levels(conectividad.barrios$nombre), style = list(color = "#212121"),
                              offset = 10) %>%
        highcharter::hc_yAxis(title = list(text = "Cantidad de paradas", style = list(color = "#212121")),
                              allowDecimals = FALSE, floor = 0) %>%
        highcharter::hc_plotOptions(
          boxplot = list(
            color = '#3aaf9d',
            fillColor = 'rgba(58,175,157,0.5)',
            lineWidth = 1,
            medianColor = '#3aaf9d',
            medianWidth = 1,
            stemColor = '#3aaf9d',
            stemDashStyle = 'dash',
            stemWidth = 1,
            whiskerColor = '#3aaf9d',
            whiskerLength = '40%',
            whiskerWidth = 1,
            tooltip = list(
              headerFormat = '<span style="font-size: 12px; font-weight: bold;">{point.key}</span><br/>',
              pointFormat = 'Máximo: {point.high:.2f}<br/>Tercer cuartil: {point.q3:.2f}<br/>Mediana: {point.median:.2f}<br/>Primer cuartil: {point.q1:.2f}<br/>Mínimo: {point.low:.2f}<br/>'
            )
          )
        ) %>%
        highcharter::hc_tooltip(useHTML = TRUE) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_title(text = "Distribución de paradas por establecimiento educativo", style = list(color = "#212121")) %>%
        highcharter::hc_exporting(enabled = TRUE, showTable = FALSE, buttons = list(
          contextButton = list(menuItems = ObtenerOpcionesExportacion(exportar.a.texto = FALSE))
        ))
      return (grafico)
    }
  })
  
  # Senderos escolares
  output$mapaSenderosEscolares <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(map = ., urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = "SDIG-2020 TP1")
  })
  observe({
    if (input$menu == "senderos_escolares") {
      longitudSenderosPorBarrios <- obtenerLongitudSenderoPorBarrio()
      if (! is.null(longitudSenderosPorBarrios)) {
        escala.colores   <- escalas.senderos$longitud.barrio$colores
        escala.etiquetas <- escalas.senderos$longitud.barrio$etiquetas
        extent.zona      <- sp::bbox(sf::as_Spatial(longitudSenderosPorBarrios))
        proxy            <- leaflet::leafletProxy(mapId = "mapaSenderosEscolares", data = longitudSenderosPorBarrios) %>%
          leaflet::clearShapes(map = .) %>%
          leaflet::clearControls(map = .) %>%
          leaflet::addPolygons(map = ., stroke = TRUE, opacity = 1, weight = 1, fillOpacity = 1, color = "#000000",
                               smoothFactor = 0.5, fillColor = ~escala,
                               popup = ~sprintf("<b>%s</b><br/>Longitud de senderos escolares: %.2f km", nombre, longitud_total/1000)) %>%
          leaflet::fitBounds(map = ., lng1 = extent.zona["x", "min"], lng2 = extent.zona["x", "max"],
                             lat1 = extent.zona["y", "min"], lat2 = extent.zona["y", "max"]) %>%
          leaflet::addLegend(map = ., colors = escala.colores, labels = escala.etiquetas, position = "bottomright",
                             opacity = 1, title = "Longitud de senderos escolares")
      }
    }
  })
})