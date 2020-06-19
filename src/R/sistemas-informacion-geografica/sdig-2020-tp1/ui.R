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
        menuItem("Hogares NBI por barrio", tabName = "hogares_nbi_barrio", selected = TRUE),
        menuItem("Cobertura educativa", tabName = "cobertura_educativa",
          #menuSubItem("Según prioridad de inscripción", tabName = "cobertura_educativa_prioridad"),
          menuSubItem("Según zona de influencia", tabName = "cobertura_educativa_influencia")
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
        tabItem(tabName = "cobertura_educativa_prioridad",
          fluidRow(
            column(6, shinycssloaders::withSpinner(leaflet::leafletOutput("mapaCoberturaPrioridad", height = 800), type = 5, color = "#008d4c")),
            column(6, shinycssloaders::withSpinner(highcharter::highchartOutput("graficoCoberturaPrioridad", height = 800), type = 5, color = "#008d4c"))
          )
        ), # Cobertura educativa / Según prioridad de inscripción
        tabItem(tabName = "cobertura_educativa_influencia",
          fluidRow(
            column(5, shinycssloaders::withSpinner(leaflet::leafletOutput("mapaCoberturaInfluencia", height = 800), type = 5, color = "#008d4c")),
            column(7, 
              fluidRow(shinycssloaders::withSpinner(highcharter::highchartOutput("cdfCoberturaInfluencia", height = 400), type = 5, color = "#008d4c")),
              fluidRow(shinycssloaders::withSpinner(highcharter::highchartOutput("barriosCoberturaInfluencia", height = 400), type = 5, color = "#008d4c"))
            )
          )
        ) # Cobertura educativa / Según zona de influencia
      ) # tabItems
    ) # dashboardBody
  ) # shiny::dashboardPage
) # shiny::shinyUI
