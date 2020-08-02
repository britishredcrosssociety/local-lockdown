#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(readr)
library(magrittr)
library(dplyr)
library(sf)
library(leaflet)
library(raster)

# ---- Load data ----
la_data = read_csv("data/local authority stats.csv")
lad = read_sf("data/Local_Authority_Districts__December_2019__Boundaries_UK_BGC.shp")
vi = read_sf("data/vulnerability.geojson")

lad = lad %>% st_transform(crs = 4326)


# ---- UI ----
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    leafletOutput("map", width = "100%", height = "100%"),
    
    absolutePanel(top = 0, left = 50, titlePanel("Local Lockdown")),
    
    absolutePanel(top = 10, right = 10,
                  selectInput("lad", 
                              label = "Choose a Local Authority",
                              choices = sort(la_data$Name))
    )
)


# ---- Server ----
server <- function(input, output) {
    # ---- Draw basemap ----
    # set up the static parts of the map (that don't change as user selects different options)
    output$map <- renderLeaflet({
        leaflet(lad, options = leafletOptions(minZoom = 5, maxZoom = 12, attributionControl = F)) %>% 
            setView(lat=54.00366, lng=-2.547855, zoom=7) %>%  # centre map on Whitendale Hanging Stones, the centre of GB: https://en.wikipedia.org/wiki/Centre_points_of_the_United_Kingdom
            addProviderTiles(providers$CartoDB.Positron)
    })
    
    # ---- Change map when user selects a Local Authority ----
    # Show data for and zoom to selected Local Authority
    filteredLA <- reactive({
        lad %>% filter(lad19nm == input$lad)
    })
    
    filteredVI <- reactive({
        # get code from selected LAD name
        lad_code = lad %>% filter(lad19nm == input$lad)
        
        vi %>% filter(LAD19CD == lad_code$lad19cd)
    })
    
    pal <- colorNumeric("viridis", 1:10)
    
    observe({
        curr_LA = filteredLA()
        
        leafletProxy("map", data = curr_LA) %>%
            clearShapes() %>%

            addPolygons(data = filteredVI(), 
                        fillColor = ~pal(Vulnerability.decile), fillOpacity = 0.8, color = "white", weight = 0.5, 
                        popup = ~paste("<b>", Name, "</b><br/><br/>",
                                       "Overall vulnerability (10 = worst): ", Vulnerability.decile, "<br/>",
                                       "Clinical vulnerability: ", Clinical.Vulnerability.decile, "<br/>",
                                       "Health/wellbeing vulnerability: ", Health.Wellbeing.Vulnerability.decile, "<br/>",
                                       "Socioeconomic vulnerability: ", Socioeconomic.Vulnerability.decile, "<br/>")) %>% 
            
            addPolygons(fill = FALSE) %>%  # Local Authority boundaries
            
            setView(lng = curr_LA$long, lat = curr_LA$lat, zoom = 10)
        
    })
}

# Run app ----
shinyApp(ui, server)
