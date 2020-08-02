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
    
    # absolutePanel(top = 0, left = 50, 
    #               titlePanel("Local Lockdown")),
    
    absolutePanel(id = "controls", class = "panel panel-default",
                  top = 0, left = 50, width = 250, fixed = TRUE,
                  draggable = TRUE, height = "auto",
                  
                  h2("Local Lockdown"),
                  
                  selectInput("lad", 
                              label = "Choose a Local Authority",
                              choices = sort(la_data$Name)),
                  
                  textOutput("infection_rate_latest"),
                  br(),
                  textOutput("infection_rate_mean"),
                  br(),
                  textOutput("shielded"),
                  br(),
                  textOutput("deprivation"),
                  br(),
                  textOutput("bame_uk"),
                  textOutput("bame_non_uk"),
                  br(),
                  textOutput("asylum")
    )
                  
    
    # absolutePanel(top = 10, right = 10,
    #               selectInput("lad", 
    #                           label = "Choose a Local Authority",
    #                           choices = sort(la_data$Name))
    # )
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
    
    filteredLAStats = reactive({
        la_data %>% filter(Name == input$lad)
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
    
    # ---- Reactive text outputs for Local Authority stats ----
    output$infection_rate_latest = renderText({
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`Latest infection rate`))
            paste0("Latest weekly Covid-19 cases per 100,000 people tested: ", round(curr_stats$`Latest infection rate`, 2))
        else
            ""
    })
    
    output$infection_rate_mean = renderText({
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`Mean infection rate over last 3 weeks`))
            paste0("Mean rate of Covid-19 cases over previous 3 weeks: ", round(curr_stats$`Mean infection rate over last 3 weeks`, 2))
        else
            ""
    })
    
    output$shielded = renderText({
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`Clinically extremely vulnerable`))
            paste0("No. clinically extremely vulnerable: ", curr_stats$`Clinically extremely vulnerable`)
        else
            ""
    })
    
    output$deprivation = renderText({
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`IMD 2019 - Extent`))
            paste0("Population living in highly deprived areas: ", round(curr_stats$`IMD 2019 - Extent` * 100, 1), "%")
        else
            ""
    })
    
    output$bame_uk = renderText({
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`Percentage of population who are ethnic minority UK born`))
            paste0("BAME population, UK born: ", curr_stats$`Percentage of population who are ethnic minority UK born`, "%")
        else
            ""
    })
    
    output$bame_non_uk = renderText({
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`Percentage of population who are ethnic minority not UK born`))
            paste0("BAME population, not UK born: ", curr_stats$`Percentage of population who are ethnic minority not UK born`, "%")
        else
            ""
    })
    
    output_asylum = renderText({
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`People receiving Section 95 support`))
            paste0("People receiving Section 95 support: ", curr_stats$`People receiving Section 95 support`)
        else
            ""
    })
}

# Run app ----
shinyApp(ui, server)
