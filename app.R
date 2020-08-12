#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyr)
library(stringr)
library(readr)
library(magrittr)
library(dplyr)
library(sf)
library(leaflet)
library(scales)
# library(raster)

# ---- Load data ----
source("functions.R")

la_data = read_csv("data/local authority stats.csv")
lad = read_sf("data/Local_Authority_Districts__December_2019__Boundaries_UK_BGC.shp")
vi = read_sf("data/vulnerability.geojson")
markers = read_sf("data/hospital-markers.shp") %>%
    replace_na(list(OrgnstN = "",
                    Addrss1 = "",
                    Postcod = "",
                    Website = "")) %>% 
    mutate(popup = paste0(OrgnstN, "<br/>",
                          Addrss1, "<br/>",
                          Postcod, "<br/>",
                          "<a href='", Website, "'>", Website, "</a>"))

lad = lad %>%
    filter(str_sub(lad19cd, 1, 1) == "E") %>%  # only use England's LAs for now, because that's where we have hospital data for
    st_transform(crs = 4326)  # could do this in preprocessing to speed up load times

la_data = la_data %>% filter(str_sub(LAD19CD, 1, 1) == "E")
vi = vi %>% filter(str_sub(LAD19CD, 1, 1) == "E")


# ---- UI ----
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    tags$head(includeCSS("styles.css")),
    tags$head(HTML("<title>Local Lockdown | Find potential mobile testing sites</title>")),
    
    leafletOutput("map", width = "100%", height = "100%"),
    
    absolutePanel(id = "controls", class = "panel panel-default",
                  top = 10, left = 50, bottom = "auto", width = 330, fixed = TRUE,
                  draggable = TRUE, height = "auto",
                  
                  h2("Local Lockdown"),
                  p("This tool helps you find hospitals to use for Covid-19 testing sites. Use the drop-down box below to select a Local Authority in England. The filled regions of the map show neighbourhood vulnerability (from ", a(href = "https://britishredcrosssociety.github.io/covid-19-vulnerability", target = "_blank", "British Red Cross's Vulnerability Index"), "). Markers show hospitals in or near highly vulnerable areas."),
                  
                  selectInput("lad", 
                              label = "Choose a Local Authority",
                              choices = sort(la_data$Name)),
                  
                  selectInput("vi",
                              label = "Type of vulnerability",
                              choices = c("Socioeconomic vulnerability", "Clinical vulnerability", "Overall vulnerability")),
                  
                  p("Developed by"), img(src = "brc-logo.jpg", width = 225)
    ),
    
    absolutePanel(id = "controls", class = "panel panel-default",
                  top = 10, left = "auto", right = 10, bottom = "auto", width = 330, fixed = TRUE,
                  draggable = TRUE, height = "auto",
                  
                  htmlOutput("la_stats")
                  
                  # textOutput("infection_rate_latest"),
                  # br(),
                  # textOutput("infection_rate_mean"),
                  # br(),
                  # textOutput("shielded"),
                  # br(),
                  # textOutput("deprivation"),
                  # br(),
                  # textOutput("bame_uk"),
                  # textOutput("bame_non_uk"),
                  # br(),
                  # textOutput("asylum")
    )
)


# ---- Server ----
server <- function(input, output) {
    # ---- Draw basemap ----
    # set up the static parts of the map (that don't change as user selects different options)
    output$map <- renderLeaflet({
        leaflet(lad, options = leafletOptions(minZoom = 5, maxZoom = 15, attributionControl = F)) %>% 
            setView(lat=54.00366, lng=-2.547855, zoom=7) %>%  # centre map on Whitendale Hanging Stones, the centre of GB: https://en.wikipedia.org/wiki/Centre_points_of_the_United_Kingdom
            addProviderTiles(providers$CartoDB.Positron) %>% 
            # Add button to reset zoom
            addEasyButton(easyButton(
                icon="fa-globe", title="Reset zoom level",
                onClick=JS("function(btn, map){ map.setZoom(6); }"))) %>% 
            # Add measuring tool to allow computation of distances as the crow flies
            addMeasure(position = "bottomleft",
                       primaryLengthUnit = "miles",
                       secondaryLengthUnit = "kilometers",
                       activeColor = "#21908D",
                       completedColor = "#3B1C8C") %>% 
            # Add hospital markers
            addMarkers(data = markers,
                       options = markerOptions(riseOnHover = TRUE,
                                               opacity = .8),
                       popup = markers$popup)
    })
    
    # ---- Change map when user selects a Local Authority ----
    # Show data for and zoom to selected Local Authority
    filteredLA <- reactive({
        lad %>% filter(lad19nm == input$lad)
    })
    
    filteredVI <- reactive({
        # get code from selected LAD name
        lad_code = lad %>% filter(lad19nm == input$lad)
        
        vi %>% 
            filter(LAD19CD == lad_code$lad19cd) %>% 
            mutate(Decile = case_when(
                input$vi == "Socioeconomic vulnerability" ~ Socioeconomic.Vulnerability.decile, 
                input$vi == "Clinical vulnerability" ~ Clinical.Vulnerability.decile, 
                input$vi == "Overall vulnerability" ~ Vulnerability.decile
            ))
    })
    
    pal <- colorFactor("viridis", c(1:10), reverse = TRUE)
    
    observe({
        curr_LA = filteredLA()
        
        leafletProxy("map", data = curr_LA) %>%
            clearShapes() %>%

            addPolygons(data = filteredVI(), 
                        fillColor = ~pal(Decile), fillOpacity = 0.8, color = "white", weight = 0.7, 
                        popup = ~paste("<b>", Name, "</b><br/><br/>",
                                       "Overall vulnerability (10 = worst): ", Vulnerability.decile, "<br/>",
                                       "Clinical vulnerability: ", Clinical.Vulnerability.decile, "<br/>",
                                       "Health/wellbeing vulnerability: ", Health.Wellbeing.Vulnerability.decile, "<br/>",
                                       "Socioeconomic vulnerability: ", Socioeconomic.Vulnerability.decile, "<br/>")) %>% 
            
            # addPolygons(fill = FALSE, color = "grey20", weight = 1) %>%  # Local Authority boundaries (don't really need them actually)
            
            setView(lng = curr_LA$long, lat = curr_LA$lat, zoom = 10)
        
    })
    
    # Use a separate observer to recreate the legend as needed.
    observe({
        leafletProxy("map", data = vi) %>% 
            clearControls() %>% 
            addLegend_decreasing(position = "bottomright",
                                 pal = pal,
                                 values = ~Vulnerability.decile,
                                 title = paste0(input$vi, tags$br(), " (10 = most vulnerable)"),
                                 opacity = 0.8,
                                 decreasing = TRUE)
            
    })
    
    # ---- Reactive text outputs for Local Authority stats ----
    # LA statistics to display in top-right panel
    output$la_stats = renderUI({  # render as HTML
        str_stats = c()  # the string to build in this function
        
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`Latest infection rate`))
            str_stats = c(str_stats, paste0("Latest weekly Covid-19 cases per 100,000 people tested: ", round(curr_stats$`Latest infection rate`, 2), " (England average: ", round(mean(la_data$`Latest infection rate`, na.rm = TRUE), 2), ")"))

        if (!is.na(curr_stats$`Mean infection rate over last 3 weeks`))
            str_stats = c(str_stats, paste0("Mean rate of Covid-19 cases over previous 3 weeks: ", round(curr_stats$`Mean infection rate over last 3 weeks`, 2), " (England average: ", round(mean(la_data$`Mean infection rate over last 3 weeks`, na.rm = TRUE), 2), ")"))
        
        if (!is.na(curr_stats$`Clinically extremely vulnerable`))
            pstr_stats = c(str_stats, paste0("No. clinically extremely vulnerable: ", comma(curr_stats$`Clinically extremely vulnerable`), " (England total: ", comma(sum(la_data$`Clinically extremely vulnerable`, na.rm = TRUE)), ")"))
        
        if (!is.na(curr_stats$`IMD 2019 - Extent`))
            str_stats = c(str_stats, paste0("Population living in highly deprived areas: ", round(curr_stats$`IMD 2019 - Extent` * 100, 1), "%"))
        
        if (!is.na(curr_stats$`Percentage of population who are ethnic minority UK born`))
            str_stats = c(str_stats, paste0("BAME population, UK born: ", curr_stats$`Percentage of population who are ethnic minority UK born`, "%"))
        
        if (!is.na(curr_stats$`Percentage of population who are ethnic minority not UK born`))
            str_stats = c(str_stats, paste0("BAME population, not UK born: ", curr_stats$`Percentage of population who are ethnic minority not UK born`, "%"))
        
        if (!is.na(curr_stats$`People receiving Section 95 support`))
            str_stats = c(str_stats, paste0("People receiving Section 95 support: ", comma(curr_stats$`People receiving Section 95 support`), " (UK total: ", comma(sum(la_data$`People receiving Section 95 support`, na.rm = TRUE)), ")"))
        
        HTML(paste(str_stats, collapse = "<br/><br/>"))
    })
    
    output$infection_rate_latest = renderText({
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`Latest infection rate`))
            paste0("Latest weekly Covid-19 cases per 100,000 people tested: ", round(curr_stats$`Latest infection rate`, 2), " (England average: ", round(mean(la_data$`Latest infection rate`, na.rm = TRUE), 2), ")")
        else
            ""
    })
    
    output$infection_rate_mean = renderText({
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`Mean infection rate over last 3 weeks`))
            paste0("Mean rate of Covid-19 cases over previous 3 weeks: ", round(curr_stats$`Mean infection rate over last 3 weeks`, 2), " (England average: ", round(mean(la_data$`Mean infection rate over last 3 weeks`, na.rm = TRUE), 2), ")")
        else
            ""
    })
    
    output$shielded = renderText({
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`Clinically extremely vulnerable`))
            paste0("No. clinically extremely vulnerable: ", comma(curr_stats$`Clinically extremely vulnerable`), " (England total: ", comma(sum(la_data$`Clinically extremely vulnerable`, na.rm = TRUE)), ")")
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
    
    output$asylum = renderText({
        curr_stats = la_data %>% filter(Name == input$lad)
        
        if (!is.na(curr_stats$`People receiving Section 95 support`))
            paste0("People receiving Section 95 support: ", comma(curr_stats$`People receiving Section 95 support`), " (UK total: ", comma(sum(la_data$`People receiving Section 95 support`, na.rm = TRUE)), ")")
        else
            ""
    })
}

# Run app ----
shinyApp(ui, server)
