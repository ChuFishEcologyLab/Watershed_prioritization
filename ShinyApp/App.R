require(shiny)
library(shinythemes)
library(tidyverse)
require(leaflet)
require(sf)
require(shinybusy)
require(viridis)
library(shinyWidgets)
require(htmltools)

############
ui <- navbarPage(
  theme = shinytheme("simplex"),
  title = "Watershed prioritization in Canada",
# 
#tags$style(type='text/css', ".selectize-input { font-size: 11px; line-height: 11px;} .selectize-dropdown { font-size: 11px; line-height: 11px; }"),

  tabPanel(
    "Map",

    #sidebar
     sidebarLayout(
      sidebarPanel(
        width = 4,
        p(strong("Prioritizing watersheds for conservation")),
        splitLayout(
        selectInput(inputId = "scale", label = "Watershed scale", 
                    choices = c("Hydrobasin level 5", "Hydrobasin level 6")),
        selectInput(inputId = "objective", label = "Weighting schema", choices = c(
          "Custom","Area-based protection", "Restoration", "SAR management", "AIS management" )),
        tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
        cellWidths = c("45%", "55%")
        ),
        splitLayout(
        actionButton(inputId = "go", label = "Generate map"),
        actionButton(inputId = "reset", label = "Reset sliders")
        ),
        
        checkboxInput("regional", label = "Watersheds ranked within ecoregions?", value = TRUE),
        p(strong("Weight watersheds according to:")),
        sliderInput(inputId = "WSI", 
                    label ="Watershed stress",
                    min = -5, max = 5, value = 0, step=0.1),
        sliderInput(inputId = "SARI", 
                    label ="Fish species at risk (SAR) richness",
                    min = -5, max = 5, value = 0, step=0.1),
        sliderInput(inputId = "Richness", 
                    label ="Fish species richness",
                    min = -5, max = 5, value = 0, step=0.1),
        sliderInput(inputId = "Q", 
                    label ="Fish species rarity",
                    min = -5, max = 5, value = 0, step=0.1),
        sliderInput(inputId = "FBCI", label="Fish biodiversity change",
                      min = -5, max = 5, value = 0, step=0.1),
        sliderInput(inputId = "CCI", 
                   label ="Climate change",
                    min = -5, max = 5, value = 0, step=0.1)
     
        ),
     
    mainPanel( # main section of Interactive map tab (i.e. the map)
        width = 8,
       leafletOutput("map", height="90vh")
           ) # end of main panel
    ) # end of sideBarLayout
  ), #end of Tab
  
  tabPanel(
    "Details",
   p("This app ranks watersheds for conservation objectives based on the methods outlined in [PAPER REFERENCE]"),
    hr(),
      p(strong("Watershed scale"),
      br(),
      "The unit of watersheds at which the prioritization is done. Options include Hydrobasin level 5 and 6 from SOURCE. Level 5 watersheds are larger than level 6. "
    ),
    
   br(),
   p(strong("Weighting schema"),
     br(),
     "Create a custom weighting scheme, or select one of the schemes meant to select watersheds for area-based protection, restoration, species at risk management or invasive species management from PAPER REFERENCE"
   ),
   hr(),
 
   p(em("Select weights for the 6 variables below to help identify priority watersheds for conservation. Positive weight vales (i.e. > 0) will select watersheds with large values of the variable,
      while negative weight values (i.e. < 0) will select watersheds with low values of the variable. Weight values of 0 indicate that the variable should be ignored when selecting watersheds.")),
   
   p(strong("Watershed stress"),
     br(),
     "Intensity of human impact in a watershed based on the Global Human Modification dataset (Theobald et al. 2017. Earth Sci Syst Data). Normalized range of 0-100"
   ),
    
   br(),
   p(strong("Fish species at risk (SAR) richness"),
     br(),
     "Number of COSEWIC listed fish species at risk with a watershed. Normalized range of 0-100"
   ),
   
   br(),
   p(strong("Fish species richness"),
     br(),
     "Number of fish species (including native, foreign and translocated species) within a watershed. Normalized range of 0-100"
   ),
   
   br(),
   p(strong("Fish biodiversity change"),
     br(),
     "Index of the relative rarity of fish species within in the amount, from Minns (1987) NZ J Zool. Normalized range of 0-100"
   ),
   
   br(),
   p(strong("Fish biodiversity change"),
     br(),
     "Index of the amount of change in a fish community between historical (ie 1960s) and current periods. Most of this change is driven by invasive species. Normalized range of 0-100"
   ),
   
   br(),
   p(strong("Climate change"),
     br(),
     "Index of the amount of climate change predicted in a watershed by 2055 under an RCP 4.5 scenario. Normalized range of 0-100"
   ),
   hr(),
  
   p(strong("Watersheds ranked within ecoregions?"),
     br(),
     "Select whether you want watersheds to be ranked within their freshwater ecoregion (i.e. high rankings = high for that ecoregion). Default (unchecked) value is that rankings are conducted at a national scale."
     )
    )
)

############
server <- function(input, output, session) {

  
 # load initial map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldTopoMap,
                       options = providerTileOptions(
                         updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                         updateWhenIdle = TRUE           # map won't load new tiles when panning
                                   ))  %>%
      fitBounds(lat1 = 43, lng1=-143, lat2=75, lng2=-50)
    
     })
  
#Reset sliders buttons
  observeEvent(input$reset, {
    
      if(input$objective == "Area-based protection"){ 
        updateSliderInput(inputId = "WSI",  value = -1)
        updateSliderInput(inputId = "SARI",  value = 1.1)
        updateSliderInput(inputId = "Richness",  value = 1)
        updateSliderInput(inputId = "Q",  value = 1.7)
        updateSliderInput(inputId = "FBCI", value = -1.1 )
        updateSliderInput(inputId = "CCI",  value = -0.5)
      } 
      if(input$objective == "Restoration"){ 
        updateSliderInput(inputId = "WSI",  value = 2.5)
        updateSliderInput(inputId = "SARI",  value = 1.4)
        updateSliderInput(inputId = "Richness",  value = 1)
        updateSliderInput(inputId = "Q",  value = 1.2)
        updateSliderInput(inputId = "FBCI", value = 0.1 )
        updateSliderInput(inputId = "CCI",  value = -1)
      }
      if(input$objective == "SAR management"){ 
        updateSliderInput(inputId = "WSI",  value = 1.5)
        updateSliderInput(inputId = "SARI",  value = 2.1)
        updateSliderInput(inputId = "Richness",  value = 0 )
        updateSliderInput(inputId = "Q",  value = 1.3)
        updateSliderInput(inputId = "FBCI", value = 1.3 )
        updateSliderInput(inputId = "CCI",  value = 1)
      }
      if(input$objective == "AIS management"){ 
        updateSliderInput(inputId = "WSI",  value = 1.4)
        updateSliderInput(inputId = "SARI",  value = 1)
        updateSliderInput(inputId = "Richness",  value =  1)
        updateSliderInput(inputId = "Q",  value = 1)
        updateSliderInput(inputId = "FBCI", value =  1.8)
        updateSliderInput(inputId = "CCI",  value = 0.5)
      }
      if(input$objective == "Custom"){ 
        updateSliderInput(inputId = "WSI",  value = 0)
        updateSliderInput(inputId = "SARI",  value = 0)
        updateSliderInput(inputId = "Richness",  value =  0)
        updateSliderInput(inputId = "Q",  value = 0)
        updateSliderInput(inputId = "FBCI", value =  0)
        updateSliderInput(inputId = "CCI",  value = 0)
      }
    
    
    }) 
  
  
#update sliders based on objective
    observeEvent(input$objective, {
    if(input$objective == "Area-based protection"){ 
      updateSliderInput(inputId = "WSI",  value = -1)
      updateSliderInput(inputId = "SARI",  value = 1.1)
      updateSliderInput(inputId = "Richness",  value = 1)
      updateSliderInput(inputId = "Q",  value = 1.7)
      updateSliderInput(inputId = "FBCI", value = -1.1 )
      updateSliderInput(inputId = "CCI",  value = -0.5)
    } 
      if(input$objective == "Restoration"){ 
        updateSliderInput(inputId = "WSI",  value = 2.5)
        updateSliderInput(inputId = "SARI",  value = 1.4)
        updateSliderInput(inputId = "Richness",  value = 1)
        updateSliderInput(inputId = "Q",  value = 1.2)
        updateSliderInput(inputId = "FBCI", value = 0.1 )
        updateSliderInput(inputId = "CCI",  value = -1)
      }
      if(input$objective == "SAR management"){ 
        updateSliderInput(inputId = "WSI",  value = 1.5)
        updateSliderInput(inputId = "SARI",  value = 2.1)
        updateSliderInput(inputId = "Richness",  value = 0 )
        updateSliderInput(inputId = "Q",  value = 1.3)
        updateSliderInput(inputId = "FBCI", value = 1.3 )
        updateSliderInput(inputId = "CCI",  value = 1)
      }
      if(input$objective == "AIS management"){ 
        updateSliderInput(inputId = "WSI",  value = 1.4)
        updateSliderInput(inputId = "SARI",  value = 1)
        updateSliderInput(inputId = "Richness",  value =  1)
        updateSliderInput(inputId = "Q",  value = 1)
        updateSliderInput(inputId = "FBCI", value =  1.8)
        updateSliderInput(inputId = "CCI",  value = 0.5)
      }
      if(input$objective == "Custom"){ 
        updateSliderInput(inputId = "WSI",  value = 0)
        updateSliderInput(inputId = "SARI",  value = 0)
        updateSliderInput(inputId = "Richness",  value =  0)
        updateSliderInput(inputId = "Q",  value = 0)
        updateSliderInput(inputId = "FBCI", value =  0)
        updateSliderInput(inputId = "CCI",  value = 0)
      }
        
      })
    
  
  
   #  data = read_csv("watershed_prioritization_level5.csv")
   #  data = data[,1:8]
   #  map = read_sf("Hybas5_map/hyC_5_join_shp_Lamb_up.shp")
   #  map <- st_transform(map, crs = 4326)
   # feow = read_sf("FEOW_CAN_Extent/FEOW__CAN_Extent.shp")
   # feow <- st_transform(feow, crs = 4326)
    
   
    
     data5 = read_csv("watershed_prioritization_level5.csv")
      data5 = data5[,1:8]
      data6 = read_csv("watershed_prioritization_level6.csv")
      data6 = data6[,1:8]
      
        map5 = read_sf("s_map5.gpkg")
      map6 = read_sf("s_map6.gpkg")
      
     feow = read_sf("s_feow.gpkg")
   
      
    

    
    
  #Generate map button
  observeEvent(input$go, {
    
    show_modal_spinner() # show the modal window

    if(input$scale == "Hydrobasin level 5"){
      map<-map5
      data<-data5}else{
        map<-map6
        data<-data6
      }
 
  #calculate the ranks based on user input weightings
  data = data %>% 
    dplyr::mutate( score =  
              WSI_n * input$WSI+
              FBCI_n * input$FBCI+
              CCI_n * input$CCI+
              SARI_n * input$SARI+
              Fish_richness_n * input$Richness+
              Priority_n * input$Q)
  
  if(input$regional == FALSE){
  
    data = data %>% dplyr::mutate(watershed_rank = rank(-score))
    } else{
    
      data =  data %>% arrange(FEOW_ID, -score) %>% 
      group_by(FEOW_ID) %>% 
      dplyr::mutate(watershed_rank=row_number()) 
      
      #convert to relative ranks
      maxrank = max(data$watershed_rank-1)
      
      data = data %>%  group_by(FEOW_ID) %>%
        dplyr::mutate(watershed_rank = ( (watershed_rank-1)* maxrank/max(watershed_rank-1) )+1)
      
     data$watershed_rank[which(is.nan(data$watershed_rank))]<-(maxrank+1)/2 #if there is only 1 watershed in feow
          }
  
  pal = viridis(max(data$watershed_rank), 
                option = "magma", 
                direction = -1, 
                end = 0.9)
  
  data$wsh_fill = pal[data$watershed_rank]
  
  #join the data with the map
   #map = map %>% select(HYBAS_ID, geometry)
   map = map %>% select(HYBAS_ID, geom)
   map = left_join(map, data, by="HYBAS_ID")
   
#popup labels for watersheds
   
   wsh_labels <- lapply(seq(nrow(map)), function(i) {
     paste0(
       "Watershed stress: ", round(map$WSI_n[i],2), "<br>",
       "SAR richness: ", round(map$SARI_n[i],2), "<br>",
       "Fish species richness: ", round(map$Fish_richness_n[i],2), "<br>",
       "Biodiversity change: ", round(map$FBCI_n[i],2), "<br>",
       "Fish rarity: ", round(map$Priority_n[i],2), "<br>",
       "Climate change: ", round(map$CCI_n[i],2), "<br>"
     )
   })
   

      #map it
   leafletProxy("map") %>%
     clearGroup(group ="watersheds") %>%
     clearShapes() %>%
     clearControls() %>%
       addPolygons(
         data = map,
         fillColor = map$wsh_fill,
         color = map$wsh_fill,
         opacity = 0.6,
         weight=2,
         group ="Watersheds",
         label = lapply(wsh_labels, htmltools::HTML),
         fillOpacity = 0.8,
         highlightOptions = highlightOptions(color = "white", weight = 2,
                                             bringToFront = TRUE)
       ) %>%
       addPolygons(
         data = feow,
                 color = "black",
         weight = 3,
         fillColor = "transparent",
         fillOpacity = 0,
         group = "Ecoregions",
         opacity = 1
       ) %>%
     hideGroup(group = "Ecoregions") %>%
       addLegend(
         colors =  viridis(9, alpha = 1, end = 0.9, direction = -1, option = "magma"),
         labels = c("high", rep("",7), "low"),
         title = "Priority", group="watersheds"
       ) %>%
        addLayersControl(
         overlayGroups = c("Watersheds", "Ecoregions"),
         position = "bottomright",
         options = layersControlOptions(collapsed = FALSE)
       )
     


   remove_modal_spinner() # remove it when done
   
    }) #end of 'go' button
  
  
  
  
 } #end of server

shinyApp(ui = ui, server = server)
