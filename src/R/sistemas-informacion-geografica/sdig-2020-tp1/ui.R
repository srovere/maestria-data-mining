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
        menuItem("Porcentaje de hogares NBI por barrio", tabName = "porcentaje_nbi_barrio", selected = TRUE)
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
        tabItem(tabName = "porcentaje_nbi_barrio",
          shinycssloaders::withSpinner(leaflet::leafletOutput("mapaNBIBarrio", height = 700), type = 5, color = "#008d4c")
        ) # Porcentaje de hogares NBI por barrio
      ) # tabItems
    ) # dashboardBody
  ) # shiny::dashboardPage
) # shiny::shinyUI
