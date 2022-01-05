############################ Install Libraries #########################

# install.packages('shiny')
# install.packages('httr')
# install.packages('shinydashboard')
# install.packages('jsonlite')
# install.packages('dplyr')
# install.packages('tidyr')
# install.packages('dplyr') 
# install.packages('sf') 
# install.packages('ggplot2')
# install.packages('ggmap')
# install.packages('leaflet')
# install.packages('lwgeom')
# install.packages('stringi')
# install.packages('shiny')
# install.packages('stringr')
# install.packages('htmltools' )
# install.packages('shinyWidgets' )
# install.packages('SpatialEpi')
# install.packages('shinybusy')

############################ Import Libraries #########################

library(shiny)
library(httr)
library(shinydashboard)
library(jsonlite)
library(dplyr)
library(tidyr)
library(dplyr) 
library(sf) 
library(ggplot2)
library(ggmap)
library(leaflet)
library(lwgeom)
library(stringi)
library(shiny)
library(stringr)
library(htmltools )
library(shinyWidgets )
library(SpatialEpi)
library(shinybusy)

######################### app ############################################################

# Dashboard
ui <- fluidPage(bootstrapPage(
  
  # Add a loading message, when the dashboard is busy
  add_busy_spinner(
    spin = "spring",
    color = "#808080",
    timeout = 100,
    position = "full-page",
    onstart = TRUE,
    margins = c(10, 10),
    height = "50px",
    width = "50px"
  ),
  
  ###################################### DASHBOARD ###############################
  
  leafletOutput("map",width="100%",height="600px"), # Show the map with all the data
  
  tags$head(tags$style(HTML('#controls {background-color: rgba(255,255,255,0.8);;}'))), # Design of the absolute panel (white-transparent)
  
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, 
                top = 15, left = "auto", right = 30, bottom = "auto", width = 350, height = "auto", # Configure the absolute Panel
                fluidRow(column(1," "),
                         column(9, fluidRow(h4(strong("FreshLife Map")))),
                         column(1, style = "margin-top: 7px;", HTML('<button data-toggle="collapse" data-target="#panel"><strong>-</strong></button>'))),
                tags$div(id = 'panel',  class="collapse in",
                column(1, " "), # Add margin to the absolute panel 
                
                column(10,  # Center column to add all the features
                       
                       fluidRow(
                         
                         ###################  Maps section #################################
                         
                         fluidRow(h4(strong("Maps"))),
                         
                         # Toggle Button and Slider Input to select the size of the icons of Outdoor Activities Map
                         fluidRow(column(8,materialSwitch(inputId = "outact_id", label = "Outdoor Activities", value = TRUE, right = TRUE)), column(4, style = "margin-top: -3px;", HTML('<button data-toggle="collapse" data-target="#icon-size">Settings</button>'))),
                         tags$div(id = 'icon-size',  class="collapse",
                                  fluidRow(column(1," "), column(5,style = "margin-top: 10px;",tags$i("Icon Size (px)")),column(5,sliderInput("size_icon", NULL, 2, 28, 16, step = 2,ticks = FALSE)))),
                         
                         # Toggle Button and Slider Input to select the opacity of the polygons of Health Advice Map
                         fluidRow(column(8,materialSwitch(inputId = "HA_id", label = "Health Advice", value = TRUE, right = TRUE)), column(4, style = "margin-top: -3px;", HTML('<button data-toggle="collapse" data-target="#HA-opacity">Settings</button>'))),
                         tags$div(id = 'HA-opacity',  class="collapse",
                                  fluidRow(column(1," "), column(5,style = "margin-top: 10px;",tags$i("Opacity")),column(5,sliderInput("opacity_HA", NULL, 0, 1, 0.5, step = 0.1,ticks = FALSE)))),
                         
                         # Toggle Button and Slider Input to select the opacity of the polygons of Air Quality Index Map
                         fluidRow(column(8,materialSwitch(inputId = "AQI_id", label = "Air Quality Index", right = TRUE)), column(4, style = "margin-top: -3px;", HTML('<button data-toggle="collapse" data-target="#AQI-opacity">Settings</button>'))),
                         tags$div(id = 'AQI-opacity',  class="collapse",
                                  fluidRow(column(1," "), column(5,style = "margin-top: 10px;",tags$i("Opacity")),column(5,sliderInput("opacity_AQI", NULL, 0, 1, 0.5, step = 0.1,ticks = FALSE)))), 
                         
                         
                         ################### Filters Section #################################
                         
                         fluidRow(h4(strong("Filters"))), 
                         
                         # Filter of Outdoor Activities Type
                         fluidRow(pickerInput(inputId = "outdoor_activity",
                                              choices = levels(as.factor(out_act$TYPE)), 
                                              multiple = T,
                                              width = 300,
                                              label='Type',
                                              selected = F,
                                              options = list(
                                                `live-search` = TRUE,
                                                `actions-box` = TRUE,
                                                `selected-text-format`= "count", 
                                                `count-selected-text` = "{0} out of {1} activities selected",
                                                `size` = 10))),
                         
                         # Filter of Region
                         fluidRow(pickerInput(inputId = "region",
                                              choices = levels(as.factor(AQ_data$REGION)),
                                              multiple = T,
                                              width = 300,
                                              label='Region',
                                              selected = F,
                                              options = list(
                                                `live-search` = TRUE,
                                                `actions-box` = TRUE,
                                                `selected-text-format`= "count", 
                                                `count-selected-text` = "{0} out of {1} regions selected"))),
                         
                         # Filter of Suburb
                         fluidRow(pickerInput(inputId = "suburb",
                                              choices = levels(as.factor(AQ_data$POSTCODE_SUBURB)),
                                              multiple = T,
                                              width = 300,
                                              label='Suburb',
                                              selected = F,
                                              options = list(
                                                `live-search` = TRUE,
                                                `actions-box` = TRUE,
                                                `selected-text-format`= "count", 
                                                       `count-selected-text` = "{0} out of {1} suburbs selected",
                                                `size` = 6))),
                         
                         ####################### Last Update Section ########################################
                         
                         fluidRow(h4(strong("Last Update"))),
                         
                         fluidRow(column(6, h5("Date:"), textOutput("date"), tags$head(tags$style("#date{color: black; font-size: 15px;}"))),
                                  column(6, h5("Hour:"),textOutput("last_update"), tags$head(tags$style("#last_update{color: black;font-size: 15px;}"))))),br()),
                
                column(1, " "))) # Margin
  ))


server <- function(input, output, session) {
  
  ################################# IMPORT DATA ##########################################################
  
  outdoor_data <- read.csv(file='./data.csv') # Outdoor Activities
  # Air Quality API
  AQI = as.data.frame(fromJSON(rawToChar(GET('https://gateway.api.epa.vic.gov.au/environmentMonitoring/v1/sites?environmentalSegment=air&X-API-Key=a002c28e7b9f4aa2bace96124336313a&Host=gateway.api.epa.vic.gov.au')$content))$records) %>% unnest_wider(siteHealthAdvices)
  AQI$long = as.data.frame(t(as.data.frame(AQI$geometry$coordinates)))$V2
  AQI$lat = as.data.frame(t(as.data.frame(AQI$geometry$coordinates)))$V1
  AQI = AQI[, c("siteID", "averageValue", "healthAdvice", "healthAdviceColor", "long", "lat")]
  
  # Wrangle the data not retrieved by the API (Sometimes the API does not retrieve the data expected)
  # Assign the mean value to NA values
  AQI$averageValue[is.na(AQI$averageValue)] <- mean(AQI$averageValue, na.rm = TRUE)
  
  # Assign the Health Advices based on AQI (Following the recommendations of EPA)
  AQI$healthAdvice[is.na(AQI$healthAdvice)] <- case_when(AQI$averageValue[is.na(AQI$healthAdvice)] < 25 ~ "Good",
                                                         AQI$averageValue[is.na(AQI$healthAdvice)] > 25 & AQI$averageValue[is.na(AQI$healthAdvice)] < 50 ~ "Fair",
                                                         AQI$averageValue[is.na(AQI$healthAdvice)] > 50 & AQI$averageValue[is.na(AQI$healthAdvice)] < 100 ~ "Poor",
                                                         AQI$averageValue[is.na(AQI$healthAdvice)] > 100 & AQI$averageValue[is.na(AQI$healthAdvice)] < 300 ~ "Very Poor",
                                                         AQI$averageValue[is.na(AQI$healthAdvice)] > 300 ~ "Extremely Poor")
  
  # Assigning the Color of the health Advice given by the EPA
  AQI$healthAdviceColor[is.na(AQI$healthAdviceColor)] <- case_when(AQI$averageValue[is.na(AQI$healthAdviceColor)] < 25 ~ "#42A93C",
                                                                   AQI$averageValue[is.na(AQI$healthAdviceColor)] > 25 & AQI$averageValue[is.na(AQI$healthAdviceColor)] < 50 ~ "#EEC900",
                                                                   AQI$averageValue[is.na(AQI$healthAdviceColor)] > 50 & AQI$averageValue[is.na(AQI$healthAdviceColor)] < 100 ~ "#E47400",
                                                                   AQI$averageValue[is.na(AQI$healthAdviceColor)] > 100 & AQI$averageValue[is.na(AQI$healthAdviceColor)] < 300 ~ "#BA0029", 
                                                                   AQI$averageValue[is.na(AQI$healthAdviceColor)] > 300 ~ "#590019")
  
  
  # Left Join between Outdoor Activities and AQI API
  out_act  = left_join(outdoor_data, AQI, by = c("join_siteI" = "siteID"))
  
  # Suburbs 
  suburb <- subset(st_read('./Spatial_Data/Spatial_Data.shp') %>% st_transform(4326), select=c(NAME,jon_stI, POSTCODE, POSTCODE_, VGREG, xmax, xmin, ymax, ymin,geometry))
  names(suburb) = c("SUBURB", "site_ID", "POSTCODE","POSTCODE_SUBURB","REGION", "xmax", "xmin", "ymax", "ymin","geometry")
  suburb$REGION[suburb$REGION == "BARWON SOUTH WEST"] = "BARWON SOUTHWEST"

  # Left Join between Suburbs and AQI API
  AQ_data  = left_join(suburb, AQI, by = c("site_ID" = "siteID"))
  

  
  
  # Import the Icons that will be displayed on the map
  icons_map = reactive({iconList(
    "Camping" = makeIcon("./www/camping.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "BBQ" = makeIcon("./www/bbq.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "Beach" = makeIcon("./www/beach.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "Campervan" = makeIcon("./www/campervan.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "Fishing" = makeIcon("./www/fishing.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "Fossicking" = makeIcon("./www/fossicking.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "Hang Glide" = makeIcon("./www/hang-gliding.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "Heritage" = makeIcon("./www/heritage.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "Horse Riding" = makeIcon("./www/horse-riding.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "Paddling" = makeIcon("./www/paddle-board.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "Picnic" = makeIcon("./www/picnic.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "Trail Bike" = makeIcon("./www/mountain-bike.png", iconWidth = input$size_icon, iconHeight = input$size_icon),
    "Wildlife" = makeIcon("./www/koala.png", iconWidth = input$size_icon, iconHeight = input$size_icon)
  )})
  
  
  ##################################### LAST UPDATE ################################################################
  
  output$last_update = renderText({paste(str_sub(Sys.time(), -8), "AEST")}) # Read the time
  output$date = renderText({format(Sys.Date(), "%b %d, %Y")}) # Read the Date 
  
  
  ######################################### REACTIVE DATAFRAMES ######################################################
  
  # Reactive table for outdoor activities
  reactive_out_act <- reactive({
    if (!is.null(input$outdoor_activity) & !is.null(input$region) & !is.null(input$suburb)){ # If all the filters have values
      if(length(intersect(unique(out_act$POSTCODE_SUBURB), input$suburb)) > 0){
        out_act %>% filter(
          TYPE %in% input$outdoor_activity &
            REGION %in% input$region &
              POSTCODE_SUBURB %in% intersect(unique(out_act$POSTCODE_SUBURB), input$suburb))}
        else{
          out_act %>% filter(
            TYPE %in% input$outdoor_activity &
              REGION %in% input$region)
        }
      }
      else if (is.null(input$outdoor_activity) & !is.null(input$region) & !is.null(input$suburb)){
        if(length(intersect(unique(out_act$POSTCODE_SUBURB), input$suburb)) > 0){
          out_act %>% filter(
            REGION %in% input$region &
              POSTCODE_SUBURB %in% intersect(unique(out_act$POSTCODE_SUBURB), input$suburb))}
        else{
          out_act %>% filter(
            REGION %in% input$region)
        }
      }
      else if (!is.null(input$outdoor_activity) & is.null(input$region) & !is.null(input$suburb)){
        if(length(intersect(unique(out_act$POSTCODE_SUBURB), input$suburb)) > 0){
          out_act %>% filter(
            TYPE %in% input$outdoor_activity &
              POSTCODE_SUBURB %in% intersect(unique(out_act$POSTCODE_SUBURB), input$suburb))}
        else{
          out_act %>% filter(
            TYPE %in% input$outdoor_activity)
        }
      }
      else if (!is.null(input$outdoor_activity) & !is.null(input$region) & is.null(input$suburb)){
        out_act %>% filter(
          TYPE %in% input$outdoor_activity &
            REGION %in% input$region)
      }
      else if (is.null(input$outdoor_activity) & is.null(input$region) & !is.null(input$suburb)){
        if(length(intersect(unique(out_act$POSTCODE_SUBURB), input$suburb)) > 0){
          out_act %>% filter(
            POSTCODE_SUBURB %in% intersect(unique(out_act$POSTCODE_SUBURB), input$suburb))}
        else{
          out_act 
        }
      }
      else if (is.null(input$outdoor_activity) & !is.null(input$region) & is.null(input$suburb)){
        out_act %>% filter(
          REGION %in% input$region)
      }
      else if (!is.null(input$outdoor_activity) & is.null(input$region) & is.null(input$suburb)){
        out_act %>% filter(
          TYPE %in% input$outdoor_activity)
      }
      else{out_act}
    })
  
  
  # Reactive table for Spatial_DAta
  reactive_AQ_data <-
    reactive({
      if (!is.null(input$region) & !is.null(input$suburb)){
        AQ_data %>% filter(
            REGION %in% input$region &
              POSTCODE_SUBURB %in% input$suburb)
      }
      else if (is.null(input$region) & !is.null(input$suburb)){
        AQ_data %>% filter(
          POSTCODE_SUBURB %in% input$suburb)
      }
      else if (!is.null(input$region) & is.null(input$suburb)){
        AQ_data %>% filter(
          REGION %in% input$region)
      }
      else{AQ_data}
    })
  
  
  
  ################################## Update Filters with the Reactive Dataframes #################################################
  
  # Update filters Region and Suburb based on the value of the Outdoor Activities
  observeEvent(input$outdoor_activity, { 
    if(!is.null(input$outdoor_activity)){
      choices_region = levels(as.factor(reactive_out_act()$REGION))
      choices_suburb = levels(as.factor(reactive_out_act()$POSTCODE_SUBURB))
    }
    else{
      choices_region = levels(as.factor(reactive_AQ_data()$REGION))
      choices_suburb = levels(as.factor(reactive_AQ_data()$POSTCODE_SUBURB))
    }
    
    updatePickerInput(session = session, inputId = "region", choices = choices_region)
    updatePickerInput(session = session, inputId = "suburb", choices = choices_suburb)}, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  # Update filter Suburbs based on the value of the Region
  observeEvent(input$region, {
    if(!is.null(input$outdoor_activity)){
      choices_suburb = levels(as.factor(reactive_out_act()$POSTCODE_SUBURB))
    }
    else{
      choices_suburb = levels(as.factor(reactive_AQ_data()$POSTCODE_SUBURB))
    }
    
    updatePickerInput(session = session, inputId = "suburb", choices = choices_suburb)}, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  
##################################### SET-UP OF MAPS #########################################################   
  
  # Create the legend of the Outdoor Activities
  html_legend <- "<strong>Outdoor Activities</strong><br/>
  <img src='bbq.png' width='18' height='18'> BBQ<br/>
  <img src='beach.png' width='18' height='18'> Beach<br/>
  <img src='campervan.png' width='18' height='18'> Campervan<br/>
  <img src='camping.png' width='18' height='18'> Camping<br/>
  <img src='fishing.png' width='18' height='18'> Fishing<br/>
  <img src='fossicking.png' width='18' height='18'> Fossicking<br/>
  <img src='hang-gliding.png' width='18' height='18'> Hang Glide<br/>
  <img src='heritage.png' width='18' height='18'> Heritage<br/>
  <img src='horse-riding.png' width='18' height='18'> Horse Riding<br/>
  <img src='paddle-board.png' width='18' height='18'> Paddling<br/>
  <img src='picnic.png' width='18' height='18'> Picnic<br/>
  <img src='mountain-bike.png' width='18' height='18'> Trail Bike<br/>
  <img src='koala.png' width='18' height='18'> Wildlife"
  
  # Create the palette of the Health Advice
  healthcolor <- colorFactor(palette=c("#42A93C", "#EEC900", "#E47400", "#BA0029", "#590019"), 
                             levels = c("Good", "Fair", "Poor", "Very poor", "Extremly poor"))
  
  # Create the palette of the Air Quality based on 5 different scales of Air Quality
  bins <- c(0)
  range_bin = (max(AQ_data$averageValue, na.rm = TRUE) - min(AQ_data$averageValue, na.rm = TRUE))/4
  for (i in 1:5){
    bins = c(bins, range_bin*i)
  }
  pal <- colorBin("YlOrRd", domain = AQ_data$averageValue, bins = bins)
  

################################# MAPS #########################################################################
  
  # Reactive Map
  output$map <- renderLeaflet({
    if (nrow(reactive_out_act()) > 0 | nrow(reactive_AQ_data()) > 0){ # If at least one reactive dataframe has data 
      if(input$AQI_id & input$HA_id & input$outact_id){ # If all the maps must be displayed
        leaflet() %>%
          fitBounds(lng1 = min(reactive_AQ_data()$xmin), lat1= min(reactive_AQ_data()$ymin), 
                    lng2=max(reactive_AQ_data()$xmax), lat2=max(reactive_AQ_data()$ymax)) %>%
          addTiles(options = providerTileOptions(minZoom = 7)) %>%
          
          # Outdoor Activities Map
          addMarkers(
            lng=reactive_out_act()$LONGITUDE,
            lat=reactive_out_act()$LATITUDE,
            label = reactive_out_act()$NAME,
            icon = icons_map()[reactive_out_act()$TYPE],
            popup = paste("<B>Name:</B>", reactive_out_act()$NAME,"<br>",
                          "<B>Type:</B>", reactive_out_act()$TYPE,"<br>",
                          "<B>Description:</B>", reactive_out_act()$DESCRIPTION,"<br>",
                          "<br>","<B>Health Advice:</B>", reactive_out_act()$healthAdvice,"<br>",
                          "<B>Air Quality Index:</B>", reactive_out_act()$averageValue, "ug/m3","<br>","<br>",
                          "<center><b><a href='",reactive_out_act()$EVENTS, "'target='_blank'>",reactive_out_act()$TYPE,"Events</a></b></center>")) %>%
          
          # Health Advice Map
          addPolygons(data=reactive_AQ_data()$geometry, 
                      color = reactive_AQ_data()$healthAdviceColor, 
                      weight = 0.1, 
                      smoothFactor = 0.5,
                      opacity = input$opacity_HA, 
                      fillOpacity = input$opacity_HA, 
                      label = reactive_AQ_data()$SUBURB,
                      popup = paste("<B>Suburb:</B>", reactive_AQ_data()$SUBURB,"<br>",
                                    "<B>Index:</B>", paste(reactive_AQ_data()$averageValue, "ug/m3")),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>% 
          
          # Air Quality Index Map
          addPolygons(data=reactive_AQ_data()$geometry, 
                      fillColor = pal(reactive_AQ_data()$averageValue), 
                      color = '#808080', 
                      weight = 0.1, 
                      smoothFactor = 0.5,
                      opacity = input$opacity_AQI, 
                      fillOpacity = input$opacity_AQI, 
                      label = reactive_AQ_data()$SUBURB,
                      popup = paste("<B>Suburb:</B>", reactive_AQ_data()$SUBURB,"<br>",
                                    "<B>Index:</B>", paste(reactive_AQ_data()$averageValue, "ug/m3"),"<br>",
                                    "<B>Health Advice:</B>", reactive_AQ_data()$healthAdvice),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
          
          # Add legends of each Map
          addControl(html = html_legend, 
                     position = "bottomleft") %>% 
          addLegend(pal = pal, 
                    values = round(reactive_AQ_data()$averageValue,1), 
                    opacity = 0.7, 
                    title = "Air Quality Index", 
                    position = "bottomleft") %>% 
          addLegend(pal = healthcolor, 
                    values = reactive_AQ_data()$healthAdvice, 
                    title = "Health Advice",
                    opacity = 0.7, 
                    position = "bottomleft")}
      
      else if(input$HA_id & input$outact_id){ # If only Health Advice and Outdoor Activities must be displayed
        leaflet() %>%
          fitBounds(lng1 = min(reactive_AQ_data()$xmin), lat1= min(reactive_AQ_data()$ymin), 
                    lng2=max(reactive_AQ_data()$xmax), lat2=max(reactive_AQ_data()$ymax)) %>%
          
          addTiles(options = providerTileOptions(minZoom = 7)) %>%
          
          # Outdoor Activities Map
          addMarkers(
            lng=reactive_out_act()$LONGITUDE,
            lat=reactive_out_act()$LATITUDE,
            label = reactive_out_act()$NAME,
            icon = icons_map()[reactive_out_act()$TYPE],
            popup = paste("<B>Name:</B>", reactive_out_act()$NAME,"<br>",
                          "<B>Type:</B>", reactive_out_act()$TYPE,"<br>",
                          "<B>Description:</B>", reactive_out_act()$DESCRIPTION,"<br>",
                          "<br>","<B>Health Advice:</B>", reactive_out_act()$healthAdvice,"<br>",
                          "<B>Air Quality Index:</B>", reactive_out_act()$averageValue, "ug/m3","<br>","<br>",
                          "<center><b><a href='",reactive_out_act()$EVENTS, "'target='_blank'>",reactive_out_act()$TYPE,"Events</a></b></center>")) %>%
          
          # Health Advice Map
          addPolygons(data=reactive_AQ_data()$geometry, 
                      color = reactive_AQ_data()$healthAdviceColor, 
                      weight = 0.1, 
                      smoothFactor = 0.5,
                      opacity = input$opacity_HA, 
                      fillOpacity = input$opacity_HA, 
                      label = reactive_AQ_data()$SUBURB,
                      popup = paste("<B>Suburb:</B>", reactive_AQ_data()$SUBURB,"<br>",
                                    "<B>Index:</B>", paste(reactive_AQ_data()$averageValue, "ug/m3")),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
          
          # Add legends of each Map
          addControl(html = html_legend, 
                     position = "bottomleft") %>% 
          addLegend(pal = healthcolor, 
                    values = reactive_AQ_data()$healthAdvice, 
                    title = "Health Advice",
                    opacity = 0.7, 
                    position = "bottomleft")}
      
      
      else if(input$AQI_id & input$outact_id){ # If only Air Quality and Outdoor Activities must be displayed
        leaflet() %>%
          fitBounds(lng1 = min(reactive_AQ_data()$xmin), lat1= min(reactive_AQ_data()$ymin), 
                    lng2=max(reactive_AQ_data()$xmax), lat2=max(reactive_AQ_data()$ymax)) %>%
          addTiles(options = providerTileOptions(minZoom = 7)) %>%
          
          # Outdoor Activities Map
          addMarkers(
            lng=reactive_out_act()$LONGITUDE,
            lat=reactive_out_act()$LATITUDE,
            label = reactive_out_act()$NAME,
            icon = icons_map()[reactive_out_act()$TYPE],
            popup = paste("<B>Name:</B>", reactive_out_act()$NAME,"<br>",
                          "<B>Type:</B>", reactive_out_act()$TYPE,"<br>",
                          "<B>Description:</B>", reactive_out_act()$DESCRIPTION,"<br>",
                          "<br>","<B>Health Advice:</B>", reactive_out_act()$healthAdvice,"<br>",
                          "<B>Air Quality Index:</B>", reactive_out_act()$averageValue, "ug/m3","<br>","<br>",
                          "<center><b><a href='",reactive_out_act()$EVENTS, "'target='_blank'>",reactive_out_act()$TYPE,"Events</a></b></center>")) %>%
          
          # Air Quality Index Map
          addPolygons(data=reactive_AQ_data()$geometry, 
                      fillColor = pal(reactive_AQ_data()$averageValue), 
                      color = '#808080', 
                      weight = 0.1, 
                      smoothFactor = 0.5,
                      opacity = input$opacity_AQI, 
                      fillOpacity = input$opacity_AQI, 
                      label = reactive_AQ_data()$SUBURB,
                      popup = paste("<B>Suburb:</B>", reactive_AQ_data()$SUBURB,"<br>",
                                    "<B>Index:</B>", paste(reactive_AQ_data()$averageValue, "ug/m3"),"<br>",
                                    "<B>Health Advice:</B>", reactive_AQ_data()$healthAdvice),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
          
          # Add legends of each Map
          addControl(html = html_legend, 
                     position = "bottomleft") %>% 
          addLegend(pal = pal, 
                    values = round(reactive_AQ_data()$averageValue,1), 
                    opacity = 0.7, 
                    title = "Air Quality Index", 
                    position = "bottomleft")}
      
      else if(input$AQI_id & input$HA_id){ # If only Air Quality and Health Advice must be displayed
        leaflet() %>%
          fitBounds(lng1 = min(reactive_AQ_data()$xmin), lat1= min(reactive_AQ_data()$ymin), 
                    lng2=max(reactive_AQ_data()$xmax), lat2=max(reactive_AQ_data()$ymax)) %>%
          addTiles(options = providerTileOptions(minZoom = 7)) %>%
          
          # Health Advice Map
          addPolygons(data=reactive_AQ_data()$geometry, 
                      color = reactive_AQ_data()$healthAdviceColor, 
                      weight = 0.1, 
                      smoothFactor = 0.5,
                      opacity = input$opacity_HA, 
                      fillOpacity = input$opacity_HA, 
                      label = reactive_AQ_data()$SUBURB,
                      popup = paste("<B>Suburb:</B>", reactive_AQ_data()$SUBURB,"<br>",
                                    "<B>Index:</B>", paste(reactive_AQ_data()$averageValue, "ug/m3")),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
          
          # Air Quality Index Map
          addPolygons(data=reactive_AQ_data()$geometry, 
                      fillColor = pal(reactive_AQ_data()$averageValue), 
                      color = '#808080', 
                      weight = 0.1, 
                      smoothFactor = 0.5,
                      opacity = input$opacity_AQI, 
                      fillOpacity = input$opacity_AQI, 
                      label = reactive_AQ_data()$SUBURB,
                      popup = paste("<B>Suburb:</B>", reactive_AQ_data()$SUBURB,"<br>",
                                    "<B>Index:</B>", paste(reactive_AQ_data()$averageValue, "ug/m3"),"<br>",
                                    "<B>Health Advice:</B>", reactive_AQ_data()$healthAdvice),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
          
          # Add legends of each Map
          addLegend(pal = pal, 
                    values = round(reactive_AQ_data()$averageValue,1), 
                    opacity = 0.7, 
                    title = "Air Quality Index", 
                    position = "bottomleft") %>% 
          addLegend(pal = healthcolor, 
                    values = reactive_AQ_data()$healthAdvice, 
                    title = "Health Advice",
                    opacity = 0.7, 
                    position = "bottomleft")}
      
    else if(input$AQI_id){ # If only Air Quality must be displayed
      leaflet() %>%
        fitBounds(lng1 = min(reactive_AQ_data()$xmin), lat1= min(reactive_AQ_data()$ymin), 
                  lng2=max(reactive_AQ_data()$xmax), lat2=max(reactive_AQ_data()$ymax)) %>%
        addTiles(options = providerTileOptions(minZoom = 7)) %>%
        
        # Air Quality Index Map
        addPolygons(data=reactive_AQ_data()$geometry, 
                    fillColor = pal(reactive_AQ_data()$averageValue), 
                    color = '#808080', 
                    weight = 0.1, 
                    smoothFactor = 0.5,
                    opacity = input$opacity_AQI, 
                    fillOpacity = input$opacity_AQI, 
                    label = reactive_AQ_data()$SUBURB,
                    popup = paste("<B>Suburb:</B>", reactive_AQ_data()$SUBURB,"<br>",
                                  "<B>Index:</B>", paste(reactive_AQ_data()$averageValue, "ug/m3"),"<br>",
                                  "<B>Health Advice:</B>", reactive_AQ_data()$healthAdvice),
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
        
        # Add legends of each Map
        addLegend(pal = pal, 
                  values = round(reactive_AQ_data()$averageValue,1), 
                  opacity = 0.7, 
                  title = "Air Quality Index", 
                  position = "bottomleft")}
      
    else if(input$HA_id){ # If only Health Advice must be displayed
      leaflet() %>%
        fitBounds(lng1 = min(reactive_AQ_data()$xmin), lat1= min(reactive_AQ_data()$ymin), 
                  lng2=max(reactive_AQ_data()$xmax), lat2=max(reactive_AQ_data()$ymax)) %>%
        addTiles(options = providerTileOptions(minZoom = 7)) %>%
        
        # Health Advice Map
        addPolygons(data=reactive_AQ_data()$geometry, 
                    color = reactive_AQ_data()$healthAdviceColor, 
                    weight = 0.1, 
                    smoothFactor = 0.5,
                    opacity = input$opacity_HA, 
                    fillOpacity = input$opacity_HA, 
                    label = reactive_AQ_data()$SUBURB,
                    popup = paste("<B>Suburb:</B>", reactive_AQ_data()$SUBURB,"<br>",
                                  "<B>Index:</B>", paste(reactive_AQ_data()$averageValue, "ug/m3")),
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
        
        # Add legends of each Map
        addLegend(pal = healthcolor, 
                  values = reactive_AQ_data()$healthAdvice, 
                  title = "Health Advice",
                  opacity = 0.7, 
                  position = "bottomleft")}
      
    else if(input$outact_id){ # If only Outdoor Activities must be displayed
        leaflet() %>%
        fitBounds(lng1 = min(reactive_AQ_data()$xmin), lat1= min(reactive_AQ_data()$ymin), 
                  lng2=max(reactive_AQ_data()$xmax), lat2=max(reactive_AQ_data()$ymax)) %>%
          addTiles(options = providerTileOptions(minZoom = 7)) %>%
        
        # Outdoor Activities Map
        addMarkers(
          lng=reactive_out_act()$LONGITUDE,
          lat=reactive_out_act()$LATITUDE,
          label = reactive_out_act()$NAME,
          icon = icons_map()[reactive_out_act()$TYPE],
          popup = paste("<B>Name:</B>", reactive_out_act()$NAME,"<br>",
                        "<B>Type:</B>", reactive_out_act()$TYPE,"<br>",
                        "<B>Description:</B>", reactive_out_act()$DESCRIPTION,"<br>",
                        "<br>","<B>Health Advice:</B>", reactive_out_act()$healthAdvice,"<br>",
                        "<B>Air Quality Index:</B>", reactive_out_act()$averageValue, "ug/m3","<br>","<br>",
                        "<center><b><a href='",reactive_out_act()$EVENTS, "'target='_blank'>",reactive_out_act()$TYPE,"Events</a></b></center>")) %>%
        
        # Add legends of each Map
        addControl(html = html_legend, 
                   position = "bottomleft")}
      
    else{
      leaflet() %>%
          fitBounds(lng1 = min(AQ_data$xmin), lat1= min(AQ_data$ymin), 
                    lng2=max(AQ_data$xmax), lat2=max(AQ_data$ymax)) %>%
          addTiles(options = providerTileOptions(minZoom = 7))}
    }
    else{leaflet() %>% 
        setView(lng = 145, lat = -37, zoom = 7) %>% addTiles(options = providerTileOptions(minZoom = 7))}
  })
}


shinyApp(ui, server)