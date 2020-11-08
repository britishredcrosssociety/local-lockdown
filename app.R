library(shiny)

# Ensure all libraries are included in the Dockerfile
library(tidyr)
library(stringr)
library(readr)
library(magrittr)
library(dplyr)
library(sf)
library(leaflet)
library(scales)
library(shinydashboard)
library(shinyWidgets)
# for plots
library(echarts4r)
# Custom error messages and loading screens
library(sever)
library(waiter)
# Custom shiny theme
library(dashboardthemes)

# ---- Load data ----
source("functions.R")

la_data <- read_csv("data/local authority stats.csv")
lad <- read_sf("data/Local_Authority_Districts__December_2019__Boundaries_UK_BGC.shp")
vi <- read_sf("data/vulnerability.geojson")
markers_hosp <- read_sf("data/hospitals-vulnerability.shp") %>%
  replace_na(list(
    Name = "",
    Address = "",
    Postcod = ""
  )) %>%
  mutate(popup = paste0(
    Name,    "<br/>",
    Address, "<br/>",
    Postcod, "<br/>"
  ))

markers_car <- read_sf("data/carparks-vulnerability.shp") %>%
  mutate(URL_text = ifelse(!is.na(URL),
                           paste0("<a href='", URL, "' target='_blank'>Click here for website</a>"),
                           ""
  )) %>%
  mutate(popup = paste0(
    "<b>", Name, "</b><br/>",
    "Stay type: ", StayTyp, "<br/><br/>",
    Address, " ", "<br/>",
    Postcod, "<br/>",
    URL_text
  ))

lad <- lad %>%
  st_transform(crs = 4326) # could do this in preprocessing to speed up load times

# Use Covid data from Colin Angus's Shiny dashboard: https://github.com/VictimOfMaths/COVID_LA_Plots | https://victimofmaths.shinyapps.io/COVID_LA_Plots/
covid_inf <- read_csv("https://github.com/VictimOfMaths/COVID_LA_Plots/raw/master/LACases.csv")
covid_inf$date <- as.Date(covid_inf$date)

covid_deaths <- read_csv("https://github.com/VictimOfMaths/COVID_LA_Plots/raw/master/LAExcess.csv")

# Load Primary Care Networks
pcn_shp <- read_sf("data/Primary_Care_Networks.shp")
pcn_shp <- pcn_shp %>%
  mutate(Name = str_to_title(str_remove(PCN_Name, " PCN")))

pcn_msoa <- read_csv("data/lookup primary care network to msoas.csv")


# # ---- UI ----
# https://community.rstudio.com/t/big-box-beside-4-small-boxes-using-shinydashboard/39489
body_colwise <- dashboardBody(
  
  # - Error and waiting functions to improve UX -
  use_sever(),
  use_waiter(),
  waiter_show_on_load(html = tagList(
    spin_5(),
    div(p("Finding mobile testing sites"), style = "padding-top:25px;")
  )),
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS("styles.css")),
  tags$head(HTML("<title>Find potential COVID-19 mobile testing sites | British Red Cross</title>")),
  
  # Load custom theme
  shinyDashboardThemes(
    theme = "grey_light"
  ),
  
  fluidRow(
    column(
      width = 6,
      box(
        width = NULL, height = "960px",
        title = "Neighbourhood Vulnerability",
        leafletOutput("map", height = "890px")
      )
    ),
    
    column(
      width = 6,
      tabBox(
        id = "covid_stats",
        width = NULL, height = "470px",
        tabPanel("Covid-19 Cases", echarts4rOutput("latest_inf", height = "400px")),
        tabPanel("Covid-19 Hospitalisations", echarts4rOutput("latest_hospitalisations", height = "400px")),
        tabPanel("Excess deaths", echarts4rOutput("latest_deaths", height = "400px"))
      ),
      tabBox(
        id = "non_covid_stats",
        width = NULL, height = "470px",
        tabPanel(
          title = "Population", align = "center",
          echarts4rOutput("pop_breakdown", height = "400px")
        ),
        tabPanel(
          title = "Furlough rate", align = "center",
          echarts4rOutput("furlough", height = "400px")
        ),
        tabPanel(
          title = "Deprived areas", align = "center",
          echarts4rOutput("IMD", height = "400px")
        ),
        tabPanel(
          title = "Extremely vulnerable", align = "center",
          echarts4rOutput("cl_vunl", height = "400px")
        ),
        tabPanel(
          width = NULL, solidHeader = TRUE, status = "danger",
          title = "Homeless",
          # Plot
          echarts4rOutput("homelessness", height = "400px")
        ),
        tabPanel(
          title = "Asylum", align = "center",
          echarts4rOutput("sec_95", height = "400px")
        )
      ) # tabBox 2
    ) # column
  ) # fluidRow
) # dashboardBody

ui <- function(request) {
  dashboardPage(
    header = dashboardHeader(
      title = "COVID-19 Testing Site Tool", titleWidth = "300px",
      # to add in bookmark button
      tags$li(class = "dropdown", bookmarkButton(), style = "padding-top: 8px; padding-bottom: 8px; padding-right: 15px")
    ),
    sidebar = dashboardSidebar(
      width = "300px",
      
      h3("Info", style = "padding-left:10px; padding-right:10px;"),
      
      p(
        style = "text-align: justify; font-size:12px; color:black; padding-left:10px; padding-right:10px;",
        "This tool helps you find sites to use for COVID-19 mobile testing.
        The shaded regions of the map show neighbourhood vulnerability (from ", a(href = "https://britishredcrosssociety.github.io/covid-19-vulnerability", target = "_blank", "British Red Cross's Vulnerability Index"), "). 
      Markers show hospitals in or near highly vulnerable areas. Parking lots are shown by clusters (circles containing a number). Click 
      a cluster to narrow in on the parking lots."
      ),
      
      h3("Instructions", style = "padding-left:10px; padding-right:10px;"),
      
      p(
        style = "text-align: justify; font-size:12px; color:black; padding-left:10px; padding-right:10px;",
        "Use the drop-down boxes below to first select a local authority or primary care network,
        and then select a domain of vulnerability to show on the map."
      ),
      
      h4("1. Select area", style = "padding-left:10px; padding-right:10px;"),
      
      sidebarMenu(
        id = "sidebar",
        menuItem("Local Authorities",
                 tabName = "la", icon = icon("building"), startExpanded = TRUE,
                 selectInput("lad",
                             label = "Choose a Local Authority",
                             choices = sort(la_data$Name),
                             selected = "Tower Hamlets"
                 )
        ),
        
        menuItem("Primary Care Networks",
                 icon = icon("stethoscope"), tabName = "pcn", # badgeLabel = "new", badgeColor = "green",
                 selectInput("pcn_name",
                             label = "Choose a Primary Care Network",
                             choices = sort(pcn_shp$Name)
                 )
        )
      ),
      
      h4("2. Select vulnerability domain", style = "padding-left:10px; padding-right:10px;"),
      
      selectInput("vi",
                  label = "Type of vulnerability",
                  choices = c("Socioeconomic vulnerability", "Clinical vulnerability", "Overall vulnerability")
      ),
      br(),
      br(),
      
      div(
          img(src = "brc-logo.jpg", width = 220),
          p(
            style = "font-size:7px; color:black;",
            a(href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target = "_blank", "Contains public sector information licensed under the Open Government Licence v3.0.")
          ),
          style = "position:fixed; bottom:0; padding:10px; text-align: center;")
    ),
    body_colwise
  )
}

# ---- Server ----
server <- function(input, output) {
  # ---- Custom markers ----
  hospital_icon <- makeIcon("www/hospital-red.png", 20, 20)
  carpark_icon <- makeIcon("www/parking.png", 20, 20)
  
  # ---- Draw basemap ----
  # set up the static parts of the map (that don't change as user selects different options)
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, attributionControl = F)) %>%
      setView(lat = 54.00366, lng = -2.547855, zoom = 7) %>% # centre map on Whitendale Hanging Stones, the centre of GB: https://en.wikipedia.org/wiki/Centre_points_of_the_United_Kingdom
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Add button to reset zoom
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset zoom level",
        onClick = JS("function(btn, map){ map.setZoom(6); }")
      )) %>%
      
      # Add measuring tool to allow computation of distances as the crow flies
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "miles",
        secondaryLengthUnit = "kilometers",
        activeColor = "#21908D",
        completedColor = "#3B1C8C"
      ) %>%
      
      # Add hospital markers
      addMarkers(
        data = markers_hosp,
        popup = ~popup,
        # icon=hospital_icon
        icon = list(iconUrl = "www/hospital-red.png", iconSize = c(20, 20))
      ) %>%
      
      # Add carpark markers to firs displayed map?
      addMarkers(
        data = markers_car,
        popup = ~popup,
        clusterOptions = markerClusterOptions(),
        # icon=hospital_icon
        icon = list(iconUrl = "www/parking.png", iconSize = c(20, 20))
      )
  })
  
  
  # ---- Change map when user selects a Local Authority ----
  # Show data for and zoom to selected Local Authority
  filteredLA <- reactive({
    lad %>% filter(lad19nm == input$lad)
  })
  
  filteredVI <- reactive({
    # get code from selected LAD name
    lad_code <- lad %>% filter(lad19nm == input$lad)
    
    vi %>%
      filter(LAD19CD == lad_code$lad19cd) %>%
      mutate(Decile = case_when(
        input$vi == "Socioeconomic vulnerability" ~ Socioeconomic.Vulnerability.decile,
        input$vi == "Clinical vulnerability" ~ Clinical.Vulnerability.decile,
        input$vi == "Overall vulnerability" ~ Vulnerability.decile
      ))
  })
  
  filteredPCN <- reactive({
    pcn_shp %>% filter(Name == input$pcn_name)
  })
  
  filteredVI_PCN <- reactive({
    # get code from selected PCN name
    pcn_code <- pcn_shp %>% filter(Name == input$pcn_name)
    
    # get MSOAs that overlap with this PCN
    curr_msoas <- pcn_msoa %>% filter(PCN_Code == pcn_code$PCN_Code)
    
    vi %>%
      filter(Code %in% curr_msoas$MSOA11CD) %>%
      # filter(PCN_Code %in% pcn_code$PCN_Code) %>%
      mutate(Decile = case_when(
        input$vi == "Socioeconomic vulnerability" ~ Socioeconomic.Vulnerability.decile,
        input$vi == "Clinical vulnerability" ~ Clinical.Vulnerability.decile,
        input$vi == "Overall vulnerability" ~ Vulnerability.decile
      ))
  })
  
  pal <- colorFactor("viridis", c(1:10), reverse = TRUE)
  
  observe({
    # get which sidebar item is currently expanded
    req(input$sidebarItemExpanded)
    
    # Variables to use for map plotting
    vi_data <- NULL
    curr_bounds_lat <- 0
    curr_bounds_lng <- 0
    curr_polygon <- NULL
    
    if (input$sidebarItemExpanded == "LocalAuthorities") {
      # User selected a LA, so get details
      curr_polygon <- filteredLA()
      
      vi_data <- filteredVI()
      
      curr_bounds_lng <- curr_polygon$long
      curr_bounds_lat <- curr_polygon$lat
    } else {
      # User selected a PCN, so get details
      curr_polygon <- filteredPCN()
      pcn_point <- curr_polygon %>%
        st_centroid() %>%
        st_geometry()
      
      vi_data <- filteredVI_PCN()
      
      curr_bounds_lng <- pcn_point[[1]][1]
      curr_bounds_lat <- pcn_point[[1]][2]
    }
    
    # Plot current LA or PCN on map
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data = vi_data,
        fillColor = ~ pal(Decile), fillOpacity = 0.8, color = "white", weight = 0.7,
        popup = ~ paste(
          "<b>", Name, "</b><br/><br/>",
          "Overall vulnerability (10 = worst): ", Vulnerability.decile, "<br/>",
          "Clinical vulnerability: ", Clinical.Vulnerability.decile, "<br/>",
          "Health/wellbeing vulnerability: ", Health.Wellbeing.Vulnerability.decile, "<br/>",
          "Socioeconomic vulnerability: ", Socioeconomic.Vulnerability.decile, "<br/>"
        )
      ) %>%
      
      # Add LA or PCN boundary
      addPolygons(
        data = curr_polygon, fill = FALSE, color = "black", weight = 2
      ) %>%
      
      # Zoom to current LA or PCN
      setView(lng = curr_bounds_lng, lat = curr_bounds_lat, zoom = 10)
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    leafletProxy("map", data = vi) %>%
      clearControls() %>%
      addLegend_decreasing(
        position = "bottomright",
        pal = pal,
        values = ~Vulnerability.decile,
        title = paste0(input$vi, tags$br(), " (10 = most vulnerable)"),
        opacity = 0.8,
        decreasing = TRUE
      )
  })
  
  
  # ---- Local authority statistics plots ----
  # Population statistics bar chart
  output$pop_breakdown <- renderEcharts4r({
    if (input$sidebarItemExpanded == "PrimaryCareNetworks") {
      scatter <- tibble(stat = 0) %>%
        e_charts(x = stat) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_title("Data unavailable for Primary Care Networks")
    } else {
      
      # select user input LA
      curr_stats <- la_data %>% filter(Name == input$lad)
      # print(curr_stats)
      
      # select bame columns
      bame_stats <- curr_stats %>% select("Percentage of population who are white UK born", "Percentage of population who are white not UK born", "Percentage of population who are ethnic minority UK born", "Percentage of population who are ethnic minority not UK born")
      bame_stats <- bame_stats %>%
        rename("White UK born" = "Percentage of population who are white UK born") %>%
        rename("White not UK born" = "Percentage of population who are white not UK born") %>%
        rename("BAME UK born" = "Percentage of population who are ethnic minority UK born") %>%
        rename("BAME not UK born" = "Percentage of population who are ethnic minority not UK born")
      
      # avg for each category
      white_uk <- la_data %>%
        select("Percentage of population who are white UK born") %>%
        drop_na("Percentage of population who are white UK born") %>%
        mutate(avg_white_uk = mean(`Percentage of population who are white UK born`))
      white_uk <- round(white_uk$avg_white_uk[1], 1)
      
      # white not uk born
      white_nuk <- la_data %>%
        select("Percentage of population who are white not UK born") %>%
        drop_na("Percentage of population who are white not UK born") %>%
        mutate(avg_white_nuk = mean(`Percentage of population who are white not UK born`))
      white_nuk <- round(white_nuk$avg_white_nuk[1], 1)
      
      # bame uk born
      bame_uk <- la_data %>%
        select("Percentage of population who are ethnic minority UK born") %>%
        drop_na("Percentage of population who are ethnic minority UK born") %>%
        mutate(avg_bame_uk = mean(`Percentage of population who are ethnic minority UK born`))
      bame_uk <- round(bame_uk$avg_bame_uk[1], 1)
      
      # bame not uk born
      bame_nuk <- la_data %>%
        select("Percentage of population who are ethnic minority not UK born") %>%
        drop_na("Percentage of population who are ethnic minority not UK born") %>%
        mutate(avg_bame_nuk = mean(`Percentage of population who are ethnic minority not UK born`))
      bame_nuk <- round(bame_nuk$avg_bame_nuk[1], 1)
      
      # Given there is data missing for some LAD I am unsure how accurate these averages are - maybe could hard code in using ONS statistics?
      all_avgs <- c(white_uk, white_nuk, bame_uk, bame_nuk)
      
      # if all elements not NA - have made assumption sums to 100
      if (all(!is.na(bame_stats)) & input$sidebarItemExpanded == "LocalAuthorities") {
        
        # lad specific legend label
        aoi <- paste0("LAD: ", input$lad)
        
        # transpose dataframe
        tbame_stats <- bame_stats %>% pivot_longer(c("White UK born", "White not UK born", "BAME UK born", "BAME not UK born"), names_to = "population", values_to = "proportion")
        
        # add ENG averages
        tbame_stats <- tbame_stats %>% mutate(eng_avg = all_avgs)
        
        # Plot population statistics
        BAME_stats <- tbame_stats %>%
          e_charts(x = population) %>%
          e_bar(proportion, name = aoi) %>%
          e_scatter(eng_avg, name = "National avg", symbolSize = 8) %>%
          e_grid(containLabel = TRUE) %>%
          e_flip_coords() %>%
          e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%"), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
          e_y_axis(axisLabel = list(interval = 0, show = T)) %>%
          e_tooltip()
      }
      
      else {
        # data doesn't exist
        if (all(is.na(bame_stats))) {
          # keep all the data even if all NAs
          tbame_stats <- bame_stats %>% pivot_longer(c("White UK born", "White not UK born", "BAME UK born", "BAME not UK born"), names_to = "population", values_to = "proportion")
          
          # convert NAs to 0 with case_when not going to plot anything
          tbame_stats <- tbame_stats %>% mutate(to_plot = case_when(
            population == "White UK born" & is.na(proportion) ~ 0,
            population == "White not UK born" & is.na(proportion) ~ 0,
            population == "BAME UK born" & is.na(proportion) ~ 0,
            population == "BAME not UK born" & is.na(proportion) ~ 0,
          ))
          
          # create title
          title <- paste0("Data Unavailable")
          subtext <- paste0("LAD: ", input$lad)
          
          # echart4R
          pop_plot <- tbame_stats %>%
            e_charts(x = population) %>%
            e_bar(to_plot, legend = F) %>%
            e_grid(containLabel = TRUE) %>%
            e_flip_coords() %>%
            # by using show=F no plot is produced.
            e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show = F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35, show = F) %>%
            e_y_axis(axisLabel = list(interval = 0, show = F), show = F) %>%
            e_tooltip() %>%
            e_title(title, subtext)
        }
        
        else {
          # transpose and remove NAs
          missing_data_or_genuine_zero <- bame_stats %>% pivot_longer(c("White UK born", "White not UK born", "BAME UK born", "BAME not UK born"), names_to = "population", values_to = "proportion", values_drop_na = TRUE)
          
          # sum all values in row to see if they == 100 - if yes
          if (sum(missing_data_or_genuine_zero$proportion) == 100) {
            # NA in whichever column represents zero
            
            # get data
            tbame_stats <- bame_stats %>% pivot_longer(c("White UK born", "White not UK born", "BAME UK born", "BAME not UK born"), names_to = "population", values_to = "proportion")
            
            # add england averages
            tbame_stats <- tbame_stats %>% mutate(eng_avg = all_avgs)
            
            # account for missing data as true zeros?
            tbame_stats <- tbame_stats %>% mutate(to_plot = case_when(
              population == "White UK born" & !is.na(proportion) ~ proportion,
              population == "White UK born" & is.na(proportion) ~ 0,
              population == "White not UK born" & !is.na(proportion) ~ proportion,
              population == "White not UK born" & is.na(proportion) ~ 0,
              population == "BAME UK born" & !is.na(proportion) ~ proportion,
              population == "BAME UK born" & is.na(proportion) ~ 0,
              population == "BAME not UK born" & !is.na(proportion) ~ proportion,
              population == "BAME not UK born" & is.na(proportion) ~ 0,
            ))
            
            
            # lad specific legend label
            aoi <- paste0("LAD: ", input$lad)
            
            # echart4R bar chart
            pop_plot <- tbame_stats %>%
              e_charts(x = population) %>%
              e_bar(to_plot, name = aoi) %>%
              e_scatter(eng_avg, name = "National avg", symbolSize = 8) %>%
              e_grid(containLabel = TRUE) %>%
              e_flip_coords() %>%
              e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%"), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_y_axis(axisLabel = list(interval = 0, show = T)) %>%
              e_tooltip()
          }
          
          else { # if values don't sum to 100 - give user warning to check figures.
            # NA in whichever column represents missing data.
            # get data
            tbame_stats <- bame_stats %>% pivot_longer(c("White UK born", "White not UK born", "BAME UK born", "BAME not UK born"), names_to = "population", values_to = "proportion")
            
            # add england averages
            tbame_stats <- tbame_stats %>% mutate(eng_avg = all_avgs)
            
            # account for missing data with NA?
            tbame_stats <- tbame_stats %>% mutate(to_plot = case_when(
              population == "White UK born" & !is.na(proportion) ~ proportion,
              population == "White UK born" & is.na(proportion) ~ NA_real_,
              population == "White not UK born" & !is.na(proportion) ~ proportion,
              population == "White not UK born" & is.na(proportion) ~ NA_real_,
              population == "BAME UK born" & !is.na(proportion) ~ proportion,
              population == "BAME UK born" & is.na(proportion) ~ NA_real_,
              population == "BAME not UK born" & !is.na(proportion) ~ proportion,
              population == "BAME not UK born" & is.na(proportion) ~ NA_real_,
            ))
            
            # remove NAs
            tbame_stats <- tbame_stats %>% drop_na("to_plot")
            
            # lad specific legend label
            aoi <- paste0("LAD: ", input$lad)
            
            # missing data note
            missing_data <- paste("Warning", "Missing data", "(n!=100)", sep = "\n")
            
            # echart4R plot chart
            pop_plot <- tbame_stats %>%
              e_charts(x = population) %>%
              e_bar(to_plot, name = aoi) %>%
              e_scatter(eng_avg, name = "National avg", symbolSize = 8) %>%
              e_grid(containLabel = TRUE) %>%
              e_flip_coords() %>%
              e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%"), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_y_axis(axisLabel = list(interval = 0, show = T)) %>%
              e_tooltip() %>%
              e_title("", missing_data, bottom = 40, left = 1)
          }
        }
      }
    }
  })
  
  # ---- Covid cases ----
  output$latest_inf <- renderEcharts4r({
    curr_stats <- covid_inf %>%
      filter(name == input$lad) %>%
      select(country, code, name, date, caserate_avg)
    
    curr_nation <- unique(curr_stats$country)
    
    lag <- 3 # discount most recent three days' figures (which are under-reported)
    
    # Get cases for the nation the selected LA is in
    country_stats <- covid_inf %>%
      filter(name == curr_nation) %>%
      select(date, national_avg = caserate_avg)
    
    # convert england data into right format
    # eng_stats <- covid_inf2 %>% filter(Name == "England")
    # eng_stats <- eng_stats %>% select(-one_of("LAD19CD", "Name"))
    # eng_stats <- eng_stats %>% pivot_longer(c(names(eng_stats)), names_to = "stat", values_to = "eng_cases")
    # # print(eng_stats)
    
    # if lad has no infection data
    any_info <- curr_stats %>% select(-one_of("country", "code", "name"))
    
    # where data is available
    if (!all(is.na(any_info)) & input$sidebarItemExpanded == "LocalAuthorities") {
      
      # transpose
      # inf_rate <- any_info %>% pivot_longer(c(names(any_info)), names_to = "stat", values_to = "value")
      
      inf_rate <- left_join(any_info, country_stats, by = "date") %>%
        na.omit() %>%
        mutate(
          caserate_avg = round(caserate_avg, 1),
          national_avg = round(national_avg, 1)
        ) %>%
        arrange(date) %>%
        filter(row_number() < n() - lag)
      
      # to highlight current infection rate
      y_point <- inf_rate %>%
        select(caserate_avg) %>%
        slice(n())
      x_point <- inf_rate %>%
        select(date) %>%
        slice(n())
      
      # add eng rates
      # inf_rate <- inf_rate %>% mutate(eng_cases = eng_stats$eng_cases)
      
      # LAD specific legend
      area <- paste0(input$lad)
      nation <- paste0(curr_nation, " (average)")
      
      # format xasix label
      label <- paste("Cases per", "100,000", sep = "\n")
      
      # plot
      scatter <- inf_rate %>%
        e_charts(x = date) %>%
        e_line(caserate_avg, name = area, symbolSize = 5) %>%
        e_line(national_avg, name = nation, symbolSize = 5) %>%
        e_x_axis(axisLabel = list(interval = 0), name = NA, nameLocation = "middle", nameGap = 25) %>%
        e_y_axis(axisLabel = list(interval = 0), name = label, nameLocation = "middle", nameGap = 25) %>%
        e_mark_point(area,
                     data = list(xAxis = x_point$date, yAxis = y_point$caserate_avg, value = y_point$caserate_avg),
                     label = list(fontSize = 8)
        ) %>%
        e_tooltip(trigger = "axis") %>%
        e_title(
          "",
          "\nRolling 7-day average of confirmed new COVID-19 cases"
        )
    } else if (all(is.na(any_info)) & input$sidebarItemExpanded == "LocalAuthorities") {
      
      # transpose
      inf_rate <- any_info %>% pivot_longer(c(names(any_info)), names_to = "stat", values_to = "value")
      
      # add eng rates
      inf_rate <- inf_rate %>% mutate(eng_cases = eng_stats$eng_cases)
      # print(inf_rate)
      
      inf_rate <- inf_rate %>% mutate(to_plot = case_when(
        is.na(value) ~ 0,
      ))
      
      # LAD specific legend
      area <- paste0("Infection rate for: ", input$lad)
      
      # create title
      title <- paste0("Infection rate data is not available for ", input$lad)
      # subtext <- paste0("Please select another local authority")
      
      # plot
      scatter <- inf_rate %>%
        e_charts(x = stat) %>%
        # e_line(to_plot, name=area, symbolSize=8) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_title(title) # , subtext)
      
      # If user is selecting Primary Care Networks, plot an error
    } else if (input$sidebarItemExpanded == "PrimaryCareNetworks") {
      scatter <- tibble(stat = 0) %>%
        e_charts(x = stat) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_title("Infection rate data is not available for Primary Care Networks")
    }
  })
  
  # ---- Hospitalisations ----
  output$latest_hospitalisations <- renderEcharts4r({
    if (input$sidebarItemExpanded == "LocalAuthorities") {
      curr_stats <- covid_inf %>%
        # filter(name == input$lad) %>%
        filter(name == "Tower Hamlets") %>%
        select(country, code, name, date, admrate_avg) %>%
        na.omit()
      
      curr_nation <- unique(curr_stats$country)
      
      lag <- 3 # discount most recent three days' figures (which are under-reported)
      
      # Get cases for the nation the selected LA is in
      country_stats <- covid_inf %>%
        filter(name == curr_nation) %>%
        select(date, national_avg = admrate_avg)
      
      # Plot only if there's something to show
      if (nrow(curr_stats) > 0) {
        curr_stats <- left_join(curr_stats, country_stats, by = "date") %>%
          na.omit() %>%
          mutate(
            admrate_avg = round(admrate_avg, 2),
            national_avg = round(national_avg, 2)
          ) %>%
          arrange(date) %>%
          filter(row_number() < n() - lag)
        
        # to highlight current infection rate
        y_point <- curr_stats %>%
          select(admrate_avg) %>%
          slice(n())
        x_point <- curr_stats %>%
          select(date) %>%
          slice(n())
        
        # LAD specific legend
        area <- paste0(input$lad)
        nation <- paste0(curr_nation, " (average)")
        
        # format xasix label
        label <- paste("Daily confirmed new hospital", "admissions per 100,000", sep = "\n")
        
        curr_stats %>%
          e_charts(x = date) %>%
          e_line(admrate_avg, name = area, symbolSize = 5) %>%
          e_line(national_avg, name = nation, symbolSize = 5) %>%
          e_x_axis(axisLabel = list(interval = 0), name = NA, nameLocation = "middle", nameGap = 25) %>%
          e_y_axis(axisLabel = list(interval = 0), name = label, nameLocation = "middle", nameGap = 25) %>%
          e_mark_point(area,
                       data = list(xAxis = x_point$date, yAxis = y_point$admrate_avg, value = y_point$admrate_avg),
                       label = list(fontSize = 8)
          ) %>%
          e_tooltip(trigger = "axis") %>%
          e_title(
            "",
            "\nRolling 7-day average of confirmed new COVID-19 admissions per 100,000 people"
          )
      } else {
        # No data available for this LA - don't show anything
        scatter <- tibble(stat = 0) %>%
          e_charts(x = stat) %>%
          e_x_axis(show = F) %>%
          e_y_axis(show = F) %>%
          e_title(paste0("Hospitalisations data is not available for ", input$lad))
      }
    } else {
      # User selected Primary Care Networks - don't show anything
      scatter <- tibble(stat = 0) %>%
        e_charts(x = stat) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_title("Hospitalisations data is not available for Primary Care Networks")
    }
  })
  
  # ---- Excess deaths ----
  output$latest_deaths <- renderEcharts4r({
    if (input$sidebarItemExpanded == "LocalAuthorities") {
      curr_stats <- covid_deaths %>%
        filter(name == input$lad) %>%
        select(code, name, week, location, allexcess) %>%
        na.omit()
      
      # Plot only if there's something to show
      if (nrow(curr_stats) > 0) {
        curr_stats %>%
          group_by(location) %>%
          e_charts(week, stack = "grp") %>%
          e_bar(allexcess) %>%
          e_x_axis(type = "category", name = "Week number", nameLocation = "middle", nameGap = 25) %>%
          e_y_axis(axisLabel = list(interval = 0), name = "Excess deaths vs. 2015-19 average", nameLocation = "middle", nameGap = 25) %>%
          e_title(
            "",
            "\nExcess deaths by occurence in 2020 vs. 2015-19 average by location.\nAround 5% of deaths are not included in this data until at least 3 months from when the death occurs."
          ) %>%
          e_tooltip(trigger = "axis")
      } else {
        # No data available for this LA - don't show anything
        scatter <- tibble(stat = 0) %>%
          e_charts(x = stat) %>%
          e_x_axis(show = F) %>%
          e_y_axis(show = F) %>%
          e_title(paste0("Excess deaths data is not available for ", input$lad))
      }
    } else {
      # User selected Primary Care Networks - don't show anything
      scatter <- tibble(stat = 0) %>%
        e_charts(x = stat) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_title("Excess deaths data is not available for Primary Care Networks")
    }
  })
  
  # ---- Clinically extremely vulnerable ----
  output$cl_vunl <- renderEcharts4r({
    # filter for just one of interest
    curr_stats <- la_data %>% filter(Name == input$lad)
    
    if (!is.na(curr_stats$`Clinically extremely vulnerable`) & input$sidebarItemExpanded == "LocalAuthorities") {
      
      # filter na's from la_data
      all_clinic_vuln <- la_data %>%
        select("Name", "Clinically extremely vulnerable") %>%
        drop_na("Clinically extremely vulnerable")
      # calculate average
      mean_clinic_vul <- all_clinic_vuln %>% summarise(avg_eng_vuln = mean(`Clinically extremely vulnerable`))
      
      # format xasix label
      label <- paste("Clinically", "extremely", "vulnerable", sep = "\n")
      
      # Extract just columns for LAD of interest and append England average and label columns
      lad_clinic_vuln <- curr_stats %>%
        select("Name", "Clinically extremely vulnerable") %>%
        mutate(avg_eng = round(mean_clinic_vul$avg_eng_vuln, 0)) %>%
        mutate(label = label)
      
      # lad specific legend label
      aoi <- paste0("LAD: ", input$lad)
      
      # plot clinical vulnerability
      clinic_vuln_bar <- lad_clinic_vuln %>%
        e_charts(x = label) %>%
        e_bar(`Clinically extremely vulnerable`, name = aoi) %>%
        e_scatter(avg_eng, name = "England avg", symbolSize = 12) %>%
        e_grid(containLabel = TRUE) %>%
        e_flip_coords() %>%
        e_x_axis(axisLabel = list(interval = 0, rotate = 45), name = "No. of people", nameLocation = "middle", nameGap = 40) %>%
        e_y_axis(axisLabel = list(interval = 0, show = T)) %>%
        e_tooltip()
    }
    
    else if (is.na(curr_stats$`Clinically extremely vulnerable`) & input$sidebarItemExpanded == "LocalAuthorities") {
      
      # plot message saying data unavailable
      lad_clinic_vuln <- curr_stats %>% select("Name", "Clinically extremely vulnerable")
      
      # convert NAs to 0 with case when not going to plot anything
      lad_clinic_vuln <- lad_clinic_vuln %>% mutate(to_plot = case_when(
        is.na(`Clinically extremely vulnerable`) ~ 0,
      ))
      
      # create title
      title <- paste0("Data Unavailable")
      subtext <- paste0("LAD: ", input$lad)
      
      # echart4R
      pop_plot <- lad_clinic_vuln %>%
        e_charts(x = Name) %>%
        e_bar(to_plot, legend = F) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_tooltip() %>%
        e_title(title, subtext)
      
      # If user is selecting Primary Care Networks, plot an error
    } else if (input$sidebarItemExpanded == "PrimaryCareNetworks") {
      scatter <- tibble(stat = 0) %>%
        e_charts(x = stat) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_title("Data unavailable for Primary Care Networks")
    }
  })
  
  # ---- Proportion of population living in highly deprived areas ----
  output$IMD <- renderEcharts4r({
    # filter for just one of interest
    curr_stats <- la_data %>% filter(Name == input$lad)
    
    if (!is.na(curr_stats$Extent) & input$sidebarItemExpanded == "LocalAuthorities") {
      
      # filter na's from la_data
      IMD_vuln <- la_data %>%
        select("Name", "Extent") %>%
        drop_na("Extent")
      
      # calculate average
      mean_IMD_vuln <- IMD_vuln %>% summarise(avg_eng_IMD = mean(Extent))
      
      # format axis label
      label <- paste("Population", "living in highly", "deprived area", sep = "\n")
      
      # Extract just columns for LAD of interest and append England average
      lad_IMD_vuln <- curr_stats %>%
        select("Name", "Extent") %>%
        mutate(avg_eng = round(mean_IMD_vuln$avg_eng_IMD * 100, 1)) %>%
        mutate(`Population living in highly deprived areas (%)` = round(Extent * 100, 1)) %>%
        mutate(label = label)
      
      # lad specific legend
      aoi <- paste0("LAD: ", input$lad)
      
      # plot proportion living in highly deprived areas
      IMD_prop_bar <- lad_IMD_vuln %>%
        e_charts(x = label) %>%
        e_bar(`Population living in highly deprived areas (%)`, name = aoi) %>%
        e_scatter(avg_eng, name = "National avg", symbolSize = 12) %>%
        e_grid(containLabel = TRUE) %>%
        e_flip_coords() %>%
        e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%"), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
        # e_format_x_axis(suffix='%') %>%
        e_y_axis(axisLabel = list(interval = 0, show = T)) %>%
        e_tooltip()
    }
    
    else if (is.na(curr_stats$Extent) & input$sidebarItemExpanded == "LocalAuthorities") {
      
      # get one with no data
      lad_IMD_vuln <- curr_stats %>% select("Name", "Extent")
      
      # convert NAs to 0 with case when not going to plot anything
      lad_IMD_vuln <- lad_IMD_vuln %>% mutate(to_plot = case_when(
        is.na(Extent) ~ 0,
      ))
      
      # create title
      title <- paste0("Data Unavailable")
      subtext <- paste0("LAD: ", input$lad)
      
      # echart4R
      pop_plot <- lad_IMD_vuln %>%
        e_charts(x = Name) %>%
        e_bar(to_plot, legend = F) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_tooltip() %>%
        e_title(title, subtext)
      
      # If user is selecting Primary Care Networks, plot an error
    } else if (input$sidebarItemExpanded == "PrimaryCareNetworks") {
      scatter <- tibble(stat = 0) %>%
        e_charts(x = stat) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_title("Data unavailable for Primary Care Networks")
    }
  })
  
  # No. of people receiving section 95 report
  output$sec_95 <- renderEcharts4r({
    # filter for just one of interest
    curr_stats <- la_data %>% filter(Name == input$lad)
    
    if (!is.na(curr_stats$`People receiving Section 95 support`) & input$sidebarItemExpanded == "LocalAuthorities") {
      
      # filter na's from la_data
      all_sec_95 <- la_data %>%
        select("Name", "People receiving Section 95 support") %>%
        drop_na("People receiving Section 95 support")
      # calculate average
      mean_sec_95 <- all_sec_95 %>% summarise(avg_eng_sec_95 = mean(`People receiving Section 95 support`))
      
      # format axis label
      label <- paste("People recieving", "Section 95", "support", sep = "\n")
      
      # Extract just columns for LAD of interest and append England average
      lad_sec_95 <- curr_stats %>%
        select("Name", "People receiving Section 95 support") %>%
        mutate(avg_eng = round(mean_sec_95$avg_eng_sec_95, 0)) %>%
        mutate(label = label)
      
      # lad specific label
      aoi <- paste0("LAD: ", input$lad)
      
      # Section 95 support
      sec_95_bar <- lad_sec_95 %>%
        e_charts(x = label) %>%
        e_bar(`People receiving Section 95 support`, name = aoi) %>%
        e_scatter(avg_eng, name = "National avg", symbolSize = 12) %>%
        e_grid(containLabel = TRUE) %>%
        e_flip_coords() %>%
        e_x_axis(axisLabel = list(interval = 0, rotate = 45), name = "No. of people", nameLocation = "middle", nameGap = 35) %>%
        e_y_axis(axisLabel = list(interval = 0, show = T)) %>%
        e_tooltip()
    }
    
    else if (is.na(curr_stats$`People receiving Section 95 support`) & input$sidebarItemExpanded == "LocalAuthorities") {
      # lad with no section 95 data
      lad_sec_95 <- curr_stats %>% select("Name", "People receiving Section 95 support")
      
      # convert NAs to 0 with case when not going to plot anything
      lad_sec_95 <- lad_sec_95 %>% mutate(to_plot = case_when(
        is.na(`People receiving Section 95 support`) ~ 0,
      ))
      
      # create title
      title <- paste0("Data Unavailable")
      subtext <- paste0("LAD: ", input$lad)
      
      # echart4R
      pop_plot <- lad_sec_95 %>%
        e_charts(x = Name) %>%
        e_bar(to_plot, legend = F) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_tooltip() %>%
        e_title(title, subtext)
      
      # If user is selecting Primary Care Networks, plot an error
    } else if (input$sidebarItemExpanded == "PrimaryCareNetworks") {
      scatter <- tibble(stat = 0) %>%
        e_charts(x = stat) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_title("Data unavailable for Primary Care Networks")
    }
  })
  
  # - Furlough -
  output$furlough <- renderEcharts4r({
    
    # filter for just one of interest
    curr_stats <- la_data %>% filter(Name == input$lad)
    
    if (!is.na(curr_stats$`Furlough count`) & input$sidebarItemExpanded == "LocalAuthorities") {
      
      # Calculate mean rate
      mean_fulough <-
        la_data %>%
        summarise(f_count = mean(`Furlough count`, na.rm = TRUE))
      
      # format axis label
      label <- paste("Employments", "furloughed", sep = "\n")
      
      # Create dataframe
      lad_furlough <- curr_stats %>%
        select("Name", `Furlough count`) %>%
        mutate(avg_eng = round(mean_fulough$f_count, 0)) %>%
        mutate(label = label)
      
      # lad specific label
      aoi <- paste0("LAD: ", input$lad)
      
      # Plot
      lad_furlough %>%
        e_charts(x = label) %>%
        e_bar(`Furlough count`, name = aoi) %>%
        e_scatter(avg_eng, name = "National avg", symbolSize = 12) %>%
        e_grid(containLabel = TRUE) %>%
        e_flip_coords() %>%
        e_x_axis(axisLabel = list(interval = 0, rotate = 45), name = "No. of people", nameLocation = "middle", nameGap = 40) %>%
        e_y_axis(axisLabel = list(interval = 0, show = T)) %>%
        e_tooltip()
    }
    
    else if (is.na(curr_stats$`Furlough count`) & input$sidebarItemExpanded == "LocalAuthorities") {
      # lad with no furlough data
      lad_furlough <- curr_stats %>% select("Name", `Furlough count`)
      
      # convert NAs to 0 with case when not going to plot anything
      lad_furlough <- lad_furlough %>% mutate(to_plot = case_when(
        is.na(`Furlough count`) ~ 0,
      ))
      
      # create title
      title <- paste0("Data Unavailable")
      subtext <- paste0("LAD: ", input$lad)
      
      # echart4R
      pop_plot <- lad_furlough %>%
        e_charts(x = Name) %>%
        e_bar(to_plot, legend = F) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_tooltip() %>%
        e_title(title, subtext)
      
      # If user is selecting Primary Care Networks, plot an error
    } else if (input$sidebarItemExpanded == "PrimaryCareNetworks") {
      scatter <- tibble(stat = 0) %>%
        e_charts(x = stat) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_title("Data unavailable for Primary Care Networks")
    }
  })
  
  # - Homeless -
  output$homelessness <- renderEcharts4r({
    
    # filter for just one of interest
    curr_stats <- la_data %>% filter(Name == input$lad)
    
    if (!is.na(curr_stats$Homelessness) & input$sidebarItemExpanded == "LocalAuthorities") {
      
      # Calculate mean rate
      mean_homelessness <-
        la_data %>%
        summarise(h_count = mean(Homelessness, na.rm = TRUE))
      
      # format axis label
      label <- paste("Homelessness", "rate per", "1000", sep = "\n")
      
      # Create dataframe
      lad_homelessness <- curr_stats %>%
        select("Name", Homelessness) %>%
        mutate(avg_eng = round(mean_homelessness$h_count, 0)) %>%
        mutate(label = label)
      
      # lad specific label
      aoi <- paste0("LAD: ", input$lad)
      
      # Plot
      lad_homelessness %>%
        e_charts(x = label) %>%
        e_bar(Homelessness, name = aoi) %>%
        e_scatter(avg_eng, name = "England avg", symbolSize = 12) %>%
        e_grid(containLabel = TRUE) %>%
        e_flip_coords() %>%
        e_x_axis(axisLabel = list(interval = 0, rotate = 45), name = "Rate per 1,000", nameLocation = "middle", nameGap = 25) %>%
        e_y_axis(axisLabel = list(interval = 0, show = T)) %>%
        e_tooltip()
    }
    
    else if (is.na(curr_stats$Homelessness) & input$sidebarItemExpanded == "LocalAuthorities") {
      # lad with no homelessness data
      lad_homelessness <- curr_stats %>% select("Name", Homelessness)
      
      # convert NAs to 0 with case when not going to plot anything
      lad_homelessness <- lad_homelessness %>% mutate(to_plot = case_when(
        is.na(Homelessness) ~ 0,
      ))
      
      # create title
      title <- paste0("Data Unavailable")
      subtext <- paste0("LAD: ", input$lad)
      
      # echart4R
      pop_plot <- lad_homelessness %>%
        e_charts(x = Name) %>%
        e_bar(to_plot, legend = F) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_tooltip() %>%
        e_title(title, subtext)
      
      # If user is selecting Primary Care Networks, plot an error
    } else if (input$sidebarItemExpanded == "PrimaryCareNetworks") {
      scatter <- tibble(stat = 0) %>%
        e_charts(x = stat) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_title("Data unavailable for Primary Care Networks")
    }
  })
  
  # - Error messages -
  sever()
  
  # - Waiter -
  waiter_hide()
}


enableBookmarking(store = "url") # saving as url results in a very long url but necessary in this deployment

# Run app ----
shinyApp(ui, server)
