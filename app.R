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

# ---- Load data ----
source("functions.R")

la_data <- read_csv("data/local authority stats.csv")
lad <- read_sf("data/Local_Authority_Districts__December_2019__Boundaries_UK_BGC.shp")
vi <- read_sf("data/vulnerability.geojson")
markers_hosp <- read_sf("data/hospital-markers.shp") %>%
  replace_na(list(
    OrgnstN = "",
    Addrss1 = "",
    Postcod = "",
    Website = ""
  )) %>%
  mutate(popup = paste0(
    OrgnstN, "<br/>",
    Addrss1, "<br/>",
    Postcod, "<br/>",
    "<a href='", Website, "'>", Website, "</a>"
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
  # filter(str_sub(lad19cd, 1, 1) == "E") %>% # only use England's LAs for now, because that's where we have hospital data for
  st_transform(crs = 4326) # could do this in preprocessing to speed up load times

# la_data <- la_data %>% filter(str_sub(LAD19CD, 1, 1) == "E")
# vi <- vi %>% filter(str_sub(LAD19CD, 1, 1) == "E")

# total lad's in England 382, total without inf rate 260 (this varies based on the week), 150 with data at some point in covid_sum

covid_inf <- read_csv("data/all_covid_infection_rate_data.csv")

# # ---- UI ----
# ui <- bootstrapPage(
#     tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#
#     tags$head(includeCSS("styles.css")),
#     tags$head(HTML("<title>Local Lockdown | Find potential mobile testing sites</title>")),
#
#     leafletOutput("map", width = "100%", height = "100%"),
#
#     absolutePanel(id = "controls", class = "panel panel-default",
#                   top = 10, left = 50, bottom = "auto", width = 330, fixed = TRUE,
#                   draggable = TRUE, height = "auto",
#
#                   h2("Local Lockdown"),
#                   p("This tool helps you find hospitals and car parks to use for Covid-19 mobile testing sites. Use the drop-down box below to select a Local Authority in England. The filled regions of the map show neighbourhood vulnerability (from ", a(href = "https://britishredcrosssociety.github.io/covid-19-vulnerability", target = "_blank", "British Red Cross's Vulnerability Index"), "). Markers show hospitals and car parks in or near highly vulnerable areas."),
#
#                   selectInput("lad",
#                               label = "Choose a Local Authority",
#                               choices = sort(la_data$Name)),
#
#                   selectInput("vi",
#                               label = "Type of vulnerability",
#                               choices = c("Socioeconomic vulnerability", "Clinical vulnerability", "Overall vulnerability")),
#
#                   p("Developed by"), img(src = "brc-logo.jpg", width = 225)
#     ),
#
#     absolutePanel(id = "controls", class = "panel panel-default",
#                   top = 10, left = "auto", right = 10, bottom = "auto", width = 330, fixed = TRUE,
#                   draggable = TRUE, height = "auto",
#
#                   htmlOutput("la_stats")
#     )
# )


# https://community.rstudio.com/t/big-box-beside-4-small-boxes-using-shinydashboard/39489
body_colwise <- dashboardBody(

  # - Error and waiting functions to improve UX -
  use_sever(),
  use_waiter(),
  waiter_show_on_load(html = tagList(
    spin_5(),
    div(p("Finding track and trace locations"), style = "padding-top:25px;")
  )),

  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS("styles.css")),
  tags$head(HTML("<title>Local Lockdown | Find potential mobile testing sites</title>")),
  
  # - Row one -
  fluidRow(
    column(
      width = 12,
      column(
        width = 6,
        box(
          width = NULL, height = "450px", solidHeader = TRUE, status = "danger",
          title = "Neighbourhood Vulnerability",
          leafletOutput("map", height = "395px")
        )
      ),
      
      column(
        width = 6,
        box(
          width = NULL, height = "450px", solidHeader = TRUE, status = "danger",
          title = "COVID-19 Infection Rate (per 100,000 people)",
          # Plot
          echarts4rOutput("latest_inf", height = "395px")
        )
      )
    ),

    # - Row two -
    fluidRow( # title='demographic data',
      column(
        width = 12,
        column(
          width = 4,
          box(
            width = NULL, height = "250px", solidHeader = TRUE, status = "danger",
            title = "Furlough rate",
            # Plot
            echarts4rOutput("furlough", height = "230px")
          )
        ),
        
        column(
          width = 4,
          box(
            width = NULL, solidHeader = TRUE, status = "danger",
            title = "Population living in highly deprived areas", height = "250px", align = "center",
            echarts4rOutput("IMD", height = "230px")
          )
        ),
        
        column(
          width = 4,
          box(
            width = NULL, height = "250px", solidHeader = TRUE, status = "danger",
            title = "Population Breakdown", align = "center",
            echarts4rOutput("pop_breakdown", height = "230px")
          )
        ),

        # - Row three -
        fluidRow(
          column(
            width = 12,
            column(
              width = 4,
              box(
                width = NULL, solidHeader = TRUE, status = "danger",
                title = "No. clinically extremely vulnerable", height = "250px", align = "center",
                echarts4rOutput("cl_vunl", height = "230px")
              )
            ),
            
            column(
              width = 4,
              box(
                width = NULL, height = "250px", solidHeader = TRUE, status = "danger",
                title = "Homeless rate",
                # Plot
                echarts4rOutput("homelessness", height = "230px")
              )
            ),

            column(
              width = 4,
              box(
                width = NULL, solidHeader = TRUE, status = "danger",
                title = "People recieving Section 95 support", height = "250px", align = "center",
                echarts4rOutput("sec_95", height = "230px")
              ),
            )
          )
        )
      )
    )
  )
)

ui <- function(request) {
  dashboardPage(
  skin = "red",
  header = dashboardHeader(title = "Local Lockdown", titleWidth = "300px",
                           #to add in bookmark button
                           tags$li(class="dropdown", bookmarkButton(), style = "padding-top: 8px; padding-bottom: 8px; padding-right: 15px")),
  sidebar = dashboardSidebar(
    width = "300px",
    # insert dropdown menu and text here
    # p(" "),
    # p(" "),
    br(),
    p("This tool helps you find hospitals to use for COVID-19 testing sites. 
      Use the drop-down box below to select a Local Authority. 
      The shaded regions of the map show neighbourhood vulnerability (from ", a(href = "https://britishredcrosssociety.github.io/covid-19-vulnerability", target = "_blank", "British Red Cross's Vulnerability Index"), "). 
      Markers show hospitals in or near highly vulnerable areas. Parking lots are shown by clusters (circles containing a number). Click 
      a cluster to narrow in on the parking lots."),
    br(),
    selectInput("lad",
      label = "Choose a Local Authority",
      choices = sort(la_data$Name),
      selected = "Tower Hamlets"
    ),
    br(),

    selectInput("vi",
      label = "Type of vulnerability",
      choices = c("Socioeconomic vulnerability", "Clinical vulnerability", "Overall vulnerability")
    ),

    br(),
    br(),
    br(),

    div(p("Developed by"), img(src = "brc-logo.jpg", width = 225), style = "text-align: center;")
  ),
  body_colwise
)
}

# ---- Server ----
server <- function(input, output) {
  # ---- Custom markers ----
  # hospital_icon = awesomeIcons(
  #     icon = 'fa-plus',
  #     iconColor = 'red',
  #     library = 'fa',
  #     markerColor = "white",
  #     squareMarker = FALSE
  # )

  hospital_icon <- makeIcon("www/hospital-red.png", 20, 20)
  carpark_icon <- makeIcon("www/parking.png", 20, 20)

  # carpark_icon = awesomeIcons(
  #     icon = 'fa-parking',
  #     iconColor = 'white',
  #     library = 'fa',
  #     markerColor = "blue",
  #     squareMarker = TRUE
  # )
  #

  # ---- Draw basemap ----
  # set up the static parts of the map (that don't change as user selects different options)
  output$map <- renderLeaflet({
    leaflet(lad, options = leafletOptions(minZoom = 5, maxZoom = 15, attributionControl = F)) %>%
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

  # IE not 100% sure this is working - when you change LAD the parking spaces from previously selected remain
  # filteredCarPark <- reactive({
  #     # get code from selected LAD name
  #     lad_code = lad %>% filter(lad19nm == input$lad)
  #
  #     markers_car %>%
  #         filter(LAD19CD == lad_code$lad19cd)
  # })

  pal <- colorFactor("viridis", c(1:10), reverse = TRUE)

  observe({
    curr_LA <- filteredLA()

    leafletProxy("map", data = curr_LA) %>%
      clearShapes() %>%
      addPolygons(
        data = filteredVI(),
        fillColor = ~ pal(Decile), fillOpacity = 0.8, color = "white", weight = 0.7,
        popup = ~ paste(
          "<b>", Name, "</b><br/><br/>",
          "Overall vulnerability (10 = worst): ", Vulnerability.decile, "<br/>",
          "Clinical vulnerability: ", Clinical.Vulnerability.decile, "<br/>",
          "Health/wellbeing vulnerability: ", Health.Wellbeing.Vulnerability.decile, "<br/>",
          "Socioeconomic vulnerability: ", Socioeconomic.Vulnerability.decile, "<br/>"
        )
      ) %>%

      # Add car park markers
      # addMarkers(data = filteredCarPark(),
      #                   icon = list(iconUrl='www/parking.png',iconSize=c(20,20)),
      #                   popup = ~popup) %>%

      # addPolygons(fill = FALSE, color = "grey20", weight = 1) %>%  # Local Authority boundaries (don't really need them actually)

      setView(lng = curr_LA$long, lat = curr_LA$lat, zoom = 10)
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

  # ---- Reactive text outputs for Local Authority stats ----
  # LA statistics to display in top-right panel
  # output$la_stats = renderUI({  # render as HTML
  #     str_stats = c()  # the string to build in this function
  #
  #     curr_stats = la_data %>% filter(Name == input$lad)
  #
  #     if (!is.na(curr_stats$`Latest infection rate`))
  #         str_stats = c(str_stats, paste0("Latest weekly Covid-19 cases per 100,000 people tested: ", round(curr_stats$`Latest infection rate`, 2), " (England average: ", round(mean(la_data$`Latest infection rate`, na.rm = TRUE), 2), ")"))
  #
  #     if (!is.na(curr_stats$`Mean infection rate over last 3 weeks`))
  #         str_stats = c(str_stats, paste0("Mean rate of Covid-19 cases over previous 3 weeks: ", round(curr_stats$`Mean infection rate over last 3 weeks`, 2), " (England average: ", round(mean(la_data$`Mean infection rate over last 3 weeks`, na.rm = TRUE), 2), ")"))
  #
  #     if (!is.na(curr_stats$`Clinically extremely vulnerable`))
  #         str_stats = c(str_stats, paste0("No. clinically extremely vulnerable: ", comma(curr_stats$`Clinically extremely vulnerable`), " (England total: ", comma(sum(la_data$`Clinically extremely vulnerable`, na.rm = TRUE)), ")"))
  #
  #     if (!is.na(curr_stats$`IMD 2019 - Extent`))
  #         str_stats = c(str_stats, paste0("Population living in highly deprived areas: ", round(curr_stats$`IMD 2019 - Extent` * 100, 1), "%"))
  #
  #     if (!is.na(curr_stats$`Percentage of population who are ethnic minority UK born`))
  #         str_stats = c(str_stats, paste0("BAME population, UK born: ", curr_stats$`Percentage of population who are ethnic minority UK born`, "%"))
  #
  #     if (!is.na(curr_stats$`Percentage of population who are ethnic minority not UK born`))
  #         str_stats = c(str_stats, paste0("BAME population, not UK born: ", curr_stats$`Percentage of population who are ethnic minority not UK born`, "%"))
  #
  #     if (!is.na(curr_stats$`People receiving Section 95 support`))
  #         str_stats = c(str_stats, paste0("People receiving Section 95 support: ", comma(curr_stats$`People receiving Section 95 support`), " (UK total: ", comma(sum(la_data$`People receiving Section 95 support`, na.rm = TRUE)), ")"))
  #
  #     HTML(paste(str_stats, collapse = "<br/><br/>"))
  # })
  #
  # output$infection_rate_latest = renderText({
  #     curr_stats = la_data %>% filter(Name == input$lad)
  #
  #     if (!is.na(curr_stats$`Latest infection rate`))
  #         paste0("Latest weekly Covid-19 cases per 100,000 people tested: ", round(curr_stats$`Latest infection rate`, 2), " (England average: ", round(mean(la_data$`Latest infection rate`, na.rm = TRUE), 2), ")")
  #     else
  #         ""
  # })
  #
  # output$infection_rate_mean = renderText({
  #     curr_stats = la_data %>% filter(Name == input$lad)
  #
  #     if (!is.na(curr_stats$`Mean infection rate over last 3 weeks`))
  #         paste0("Mean rate of Covid-19 cases over previous 3 weeks: ", round(curr_stats$`Mean infection rate over last 3 weeks`, 2), " (England average: ", round(mean(la_data$`Mean infection rate over last 3 weeks`, na.rm = TRUE), 2), ")")
  #     else
  #         ""
  # })
  #
  # output$shielded = renderText({
  #     curr_stats = la_data %>% filter(Name == input$lad)
  #
  #     if (!is.na(curr_stats$`Clinically extremely vulnerable`))
  #         paste0("No. clinically extremely vulnerable: ", comma(curr_stats$`Clinically extremely vulnerable`), " (England total: ", comma(sum(la_data$`Clinically extremely vulnerable`, na.rm = TRUE)), ")")
  #     else
  #         ""
  # })
  #
  # output$deprivation = renderText({
  #     curr_stats = la_data %>% filter(Name == input$lad)
  #
  #     if (!is.na(curr_stats$`IMD 2019 - Extent`))
  #         paste0("Population living in highly deprived areas: ", round(curr_stats$`IMD 2019 - Extent` * 100, 1), "%")
  #     else
  #         ""
  # })
  #
  # output$bame_uk = renderText({
  #     curr_stats = la_data %>% filter(Name == input$lad)
  #
  #     if (!is.na(curr_stats$`Percentage of population who are ethnic minority UK born`))
  #         paste0("BAME population, UK born: ", curr_stats$`Percentage of population who are ethnic minority UK born`, "%")
  #     else
  #         ""
  # })
  #
  # output$bame_non_uk = renderText({
  #     curr_stats = la_data %>% filter(Name == input$lad)
  #
  #     if (!is.na(curr_stats$`Percentage of population who are ethnic minority not UK born`))
  #         paste0("BAME population, not UK born: ", curr_stats$`Percentage of population who are ethnic minority not UK born`, "%")
  #     else
  #         ""
  # })
  #
  # output$asylum = renderText({
  #     curr_stats = la_data %>% filter(Name == input$lad)
  #
  #     if (!is.na(curr_stats$`People receiving Section 95 support`))
  #         paste0("People receiving Section 95 support: ", comma(curr_stats$`People receiving Section 95 support`), " (UK total: ", comma(sum(la_data$`People receiving Section 95 support`, na.rm = TRUE)), ")")
  #     else
  #         ""
  # })

  ### Local authority statistics plots ###

  # Population statistics bar chart
  output$pop_breakdown <- renderEcharts4r({
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
    if (all(!is.na(bame_stats))) {

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
  })

  # ## plotting infection rate statistics
  # output$latest_inf <- renderEcharts4r({
  #     curr_stats = la_data %>% filter(Name == input$lad)
  #     inf_stats = curr_stats %>% select('Name','Latest infection rate', 'upper', 'lower')
  #
  #     # At the moment only plots if there's a value in latest infection rate column
  #     # there are two LAD ('Redcar and Cleveland','Bracknell Forest') that have a past 3 week avg inf rate but not a current infection rate - these return no data available
  #     if (!is.na(curr_stats$`Latest infection rate`)) {
  #
  #         #transpose
  #         inf_rate <- inf_stats %>% pivot_longer(c(`Latest infection rate`), names_to = "stat", values_to = "value")
  #         inf_rate <- inf_rate %>% mutate(avg_eng=11.5) %>% mutate(value_round = round(value, 1)) %>% mutate('lower'= NA_real_) %>% mutate('upper'= NA_real_)
  #
  #         #round lad 3 week infection average
  #         round_lad3week <- round(curr_stats$`Mean infection rate over last 3 weeks`, 1)
  #
  #         #add row with eng avg over past 3 weeks and the LADs avg over previous 3 weeks- this is currently hard coded in - should this be a calculation based on the mean of each of the columns?
  #         inf_rate <- inf_rate %>% add_row(stat='avg over previous 3 weeks',value=curr_stats$`Mean infection rate over last 3 weeks`, value_round=round_lad3week, avg_eng=10.4, lower=curr_stats$lower, upper=curr_stats$upper, .before=1)
  #
  #         #LAD specific legend
  #         area = paste0('Infection rate for: ', input$lad)
  #
  #         #plot
  #         scatter <- inf_rate %>% e_charts(x=stat) %>%
  #             e_line(value_round, name=area, symbolSize=12) %>%
  #             e_line(avg_eng, name='England Avg',symbolSize=12) %>%
  #             e_error_bar(lower, upper, legend=F) %>%
  #             e_tooltip()
  #
  #
  #     }
  #
  #     else{
  #         #plot message saying data unavailable
  #         inf_rate <- inf_stats %>% pivot_longer(c(`Latest infection rate`), names_to = "stat", values_to = "value")
  #
  #         #convert NAs to 0 with case when not going to plot anything
  #         inf_rate = inf_rate %>% mutate(to_plot = case_when(
  #             is.na(value) ~ 0,
  #         ))
  #
  #         #create title
  #         title = paste0('Data Unavailable')
  #         subtext = paste0("LAD: ", input$lad)
  #
  #         #echart4R
  #         pop_plot <- inf_rate %>% e_charts(x = stat) %>%
  #             e_scatter(to_plot, legend=F) %>%
  #             e_x_axis(show=F) %>%
  #             e_y_axis(show=F) %>%
  #             e_tooltip() %>%
  #             e_title(title, subtext)
  #
  #     }
  #
  #
  # })


  ## plotting infection rate statistics
  output$latest_inf <- renderEcharts4r({
    curr_stats <- covid_inf %>% filter(Name == input$lad)

    # convert england data into right format
    eng_stats <- covid_inf %>% filter(Name == "England")
    eng_stats <- eng_stats %>% select(-one_of("LAD19CD", "Name"))
    eng_stats <- eng_stats %>% pivot_longer(c(names(eng_stats)), names_to = "stat", values_to = "eng_cases")
    # print(eng_stats)

    # if lad has no infection data
    any_info <- curr_stats %>% select(-one_of("LAD19CD", "Name"))

    # where data is available
    if (!all(is.na(any_info))) {

      # transpose
      inf_rate <- any_info %>% pivot_longer(c(names(any_info)), names_to = "stat", values_to = "value")

      # to highlight current infection rate
      y_point <- inf_rate %>%
        select(value) %>%
        slice(n())
      x_point <- inf_rate %>%
        select(stat) %>%
        slice(n())

      # add eng rates
      inf_rate <- inf_rate %>% mutate(eng_cases = eng_stats$eng_cases)


      # LAD specific legend
      area <- paste0(input$lad)

      # format xasix label
      label <- paste("Cases per", "100,000", sep = "\n")


      # plot
      scatter <- inf_rate %>%
        e_charts(x = stat) %>%
        e_line(value, name = area, symbolSize = 8) %>%
        e_line(eng_cases, name = "England (average)", symbolSize = 8) %>%
        e_x_axis(axisLabel = list(interval = 0), name = "Week", nameLocation = "middle", nameGap = 25) %>%
        e_y_axis(axisLabel = list(interval = 0), name = label, nameLocation = "middle", nameGap = 25) %>%
        e_mark_point(area,
                     data = list(xAxis = x_point$stat, yAxis = y_point$value, value = y_point$value),
                     label = list(fontSize = 8)) %>%
        e_tooltip(trigger = "axis")
    }

    else {

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
      title <- paste0("Unfortunately, infection rate data is unavailable for ", input$lad)
      # subtext <- paste0("Please select another local authority")

      # plot
      scatter <- inf_rate %>%
        e_charts(x = stat) %>%
        # e_line(to_plot, name=area, symbolSize=8) %>%
        e_x_axis(show = F) %>%
        e_y_axis(show = F) %>%
        e_title(title) # , subtext)
    }
  })



  # Clinically extremely vulnerable --> display as comparison to avg no. of clinically extremely vulnerable
  output$cl_vunl <- renderEcharts4r({
    # filter for just one of interest
    curr_stats <- la_data %>% filter(Name == input$lad)
    if (!is.na(curr_stats$`Clinically extremely vulnerable`)) {

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

    else {

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
    }
  })

  # Proportion of population living in highly deprived areas plot
  output$IMD <- renderEcharts4r({
    # filter for just one of interest
    curr_stats <- la_data %>% filter(Name == input$lad)
    if (!is.na(curr_stats$Extent)) {

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

    else {

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
    }
  })

  # No. of people receiving section 95 report
  output$sec_95 <- renderEcharts4r({
    # filter for just one of interest
    curr_stats <- la_data %>% filter(Name == input$lad)

    if (!is.na(curr_stats$`People receiving Section 95 support`)) {

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

    else {
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
    }
  })
  
  # - Furlough -
  output$furlough <- renderEcharts4r({
    
    # filter for just one of interest
    curr_stats <- la_data %>% filter(Name == input$lad)
    
    if(!is.na(curr_stats$`Furlough count`)){
      
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
    
    else {
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
    }
  })
  
  # - Homeless -
  output$homelessness <- renderEcharts4r({
    
    # filter for just one of interest
    curr_stats <- la_data %>% filter(Name == input$lad)
    
    if(!is.na(curr_stats$Homelessness)){
      
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
    
    else {
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
    }
    
  })

  # - Error messages -
  sever()

  # - Waiter -
  waiter_hide()
}


#enableBookmarking(store='url') #saving as url results in a very long url
enableBookmarking(store='server') 

# Run app ----
shinyApp(ui, server)
