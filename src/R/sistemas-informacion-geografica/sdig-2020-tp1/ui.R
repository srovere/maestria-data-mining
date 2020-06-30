# -----------------------------------------------------------------------------#
# --- Censo CREA - Reporte para asesores (UI)
# -----------------------------------------------------------------------------#

shiny::shinyUI(
  dashboardPage(skin = "green", title = "SDIG 2020 - TP1",
    # Header
    dashboardHeader(
      title = "", titleWidth="300"
    ),
    
    # Menu de informes
    dashboardSidebar(width="300",
      sidebarMenu(id = "menu",
        # Menus
        menuItem("Nivel socio-económico", tabName = "hogares_nbi_barrio", selected = TRUE),
        menuItem("Cobertura educativa", 
          menuSubItem('Zonas de influencia', tabName = "cobertura_educativa"),
          menuSubItem("Atención de hogares NBI", tabName = "oferta_demanda_educativa")
        ),
        menuItem("Conectividad",
          menuSubItem("Transporte público", tabName = "conectividad"),
          menuSubItem("Senderos escolares", tabName = "senderos_escolares")
        )
      ),
      hr(),
      shiny::conditionalPanel(
        condition = 'input.menu == "hogares_nbi_barrio"',
        shiny::radioButtons(inputId = "opciones_hogares_nbi_barrio",
                            choices = c("Porcentaje hogares NBI" = "porcentaje", "Cantidad hogares NBI" = "cantidad", 
                                        "Densidad hogares NBI/km²" = "densidad"),
                            label = "Indique la métrica a mostrar",
                            selected = "porcentaje", inline = FALSE),
      ),
      shiny::conditionalPanel(
        condition = 'input.menu == "cobertura_educativa"',
        shiny::checkboxInput(inputId = "agregar_establecimientos_privados", value = FALSE,
                             label = "Agregar establecimientos privados")
      ),
      shiny::conditionalPanel(
        condition = 'input.menu == "conectividad"',
        shiny::sliderInput(inputId = "distancia_maxima_conectividad",
                           min = 100, max = 2000, value = 300, step = 50,
                           label = "Distancia máxima tolerable para buscar paradas de transporte publico")
      ),
      shiny::conditionalPanel(
        condition = 'input.menu == "senderos_escolares"',
        shiny::radioButtons(inputId = "opcion_mapa_sendero",
                            label = "Indicar el tipo de mapa a mostrar", 
                            choices = c("Longitud de senderos por barrio" = "longitud_senderos_barrio", 
                                        "Distancia de escuelas a senderos" = "distancia_escuelas_senderos"),
                            selected = "longitud_senderos_barrio"),
        shiny::conditionalPanel(
          condition = '((input.menu == "senderos_escolares") && (input.opcion_mapa_sendero == "distancia_escuelas_senderos"))',
          shiny::sliderInput(inputId = "distancia_minima_senderos",
                             min = 100, max = 1800, value = 10, step = 100,
                             label = "Distancia mínima a un sendero escolar")
        )
      )
    ), # dashboardSidebar
      
    # Cuerpo del informe
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto+Condensed"),
        tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
        tags$style(".shiny-notification {position: fixed; top: 50%; left: 50% }")
      ),
      
      tabItems(
        tabItem(tabName = "hogares_nbi_barrio",
          fluidRow(
            column(6, shinycssloaders::withSpinner(leaflet::leafletOutput("mapaNBIBarrio", height = 800), type = 5, color = "#008d4c")),
            column(6, shinycssloaders::withSpinner(highcharter::highchartOutput("graficoNBIBarrio", height = 800), type = 5, color = "#008d4c"))
          )
        ), # Porcentaje de hogares NBI por barrio
        tabItem(tabName = "cobertura_educativa",
          fluidRow(
            column(6, shinycssloaders::withSpinner(leaflet::leafletOutput("mapaCoberturaInfluencia", height = 700), type = 5, color = "#008d4c")),
            column(6, shinycssloaders::withSpinner(highcharter::highchartOutput("cdfCoberturaInfluencia", height = 700), type = 5, color = "#008d4c"))
          )
        ), # Cobertura educativa
        tabItem(tabName = "oferta_demanda_educativa",
          fluidRow(
            column(6, shinycssloaders::withSpinner(highcharter::highchartOutput("barriosOfertaDemanda", height = 700), type = 5, color = "#008d4c"), type = 5, color = "#008d4c"),
            column(6, shinycssloaders::withSpinner(highcharter::highchartOutput("comunasOfertaDemanda", height = 700), type = 5, color = "#008d4c"), type = 5, color = "#008d4c")
          )
        ), # Oferta/demanda educativa
        tabItem(tabName = "conectividad",
          fluidRow(
            column(6, shinycssloaders::withSpinner(leaflet::leafletOutput("mapaConectividad", height = 800), type = 5, color = "#008d4c")),
            column(6, shinycssloaders::withSpinner(highcharter::highchartOutput("boxplotsConectividad", height = 800), type = 5, color = "#008d4c"))
          )
        ), # Conectividad
        tabItem(tabName = "senderos_escolares",
          fluidRow(
            column(6, shinycssloaders::withSpinner(leaflet::leafletOutput("mapaSenderosEscolares", height = 800), type = 5, color = "#008d4c")),
            column(6, shinycssloaders::withSpinner(highcharter::highchartOutput("hogaresNBILongitudSenderos", height = 800), type = 5, color = "#008d4c"))
          )
        ) # Senderos escolares
      ) # tabItems
    ) # dashboardBody
  ) # shiny::dashboardPage
) # shiny::shinyUI