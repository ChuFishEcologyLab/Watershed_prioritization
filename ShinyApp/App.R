library(shiny)
library(shinybusy)
library(shinyjs)
library(leaflet)
library(bslib)

# Geometries
## Canada (with results)
map6 <- sf::read_sf("s_map6.gpkg")
## Lake Erie (with results)
mapl <- sf::read_sf("s_mapl.gpkg")
## FEOW
feow <- sf::read_sf("s_feow.gpkg")

############
ui <- navbarPage(
  theme = bs_theme(version = 5),
  useShinyjs(),
  title = "Watershed prioritization in Canada",
  #
  tabPanel(
    "Map",
    # sidebar
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h5(strong("Prioritizing watersheds for conservation and management")),
        div(
          class = "alert alert-primary", role = "alert",
          tagList(
            p(
              icon("circle-info"),
              "First, select the watershed scale you want to view on the map. If interested in the national scale select whether you would like the scores calculated within Freshwater Ecoregions. Next, select the conservation objective of interest (default values are median of the co-author weightings). Finally, click the button at the bottom to generate the map."
            )
          )
        ),
        splitLayout(
          selectInput(
            inputId = "scale", label = "Watershed scale",
            choices = c("National hydrobasin level 6", "Lake Erie and Lake Ontario")
          ),
          div(
            id = "within_feow",
            class = "my-3 pt-4 mx-3",
            checkboxInput("regional", label = "Watersheds ranked within ecoregions?", value = TRUE)
          ),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          cellWidths = c("40%", "60%")
        ),
        selectInput(inputId = "objective", label = "Weighting schema", choices = c(
          #"Median across co-authors", "Custom",# "Area-based protection", "Restoration", "SAR management", "AIS management"
        )),
        div(
          class = "my-4",
          h5(strong("Weight watersheds according to:"))
        ),
        sliderInput(
          inputId = "WSI",
          label = "Watershed stress",
          min = -5, max = 5, value = 0, step = 0.1
        ),
        sliderInput(
          inputId = "SARI",
          label = "Fish species at risk (SAR) richness",
          min = -5, max = 5, value = 0, step = 0.1
        ),
        sliderInput(
          inputId = "Richness",
          label = "Fish species richness",
          min = -5, max = 5, value = 0, step = 0.1
        ),
        sliderInput(
          inputId = "Q",
          label = "Fish species rarity",
          min = -5, max = 5, value = 0, step = 0.1
        ),
        sliderInput(
          inputId = "FBCI", label = "Fish biodiversity change",
          min = -5, max = 5, value = 0, step = 0.1
        ),
        sliderInput(
          inputId = "CCI",
          label = "Climate change",
          min = -5, max = 5, value = 0, step = 0.1
        ),
        div(
          class = "d-flex d-flex justify-content-between",
          actionButton(inputId = "reset", icon = icon("refresh"), label = "Reset sliders"),
          actionButton(inputId = "go", icon = icon("map"), label = "Generate map")
        )
      ),
      mainPanel( # main section of Interactive map tab (i.e. the map)
        width = 8,
        leafletOutput("map", height = "90vh")
      ) # end of main panel
    ) # end of sideBarLayout
  ), # end of Tab
  tabPanel(
    "Details",
    div(
      class = "d-flex justify-content-center",
      div(
        class = "w-50 p-3",
        includeHTML("details.html")
      )
    )
  )
)

############
server <- function(input, output, session) {
  # -------- Hide button if Lake Erie
  observe({
    if (input$scale == "Lake Erie") {
      hide("within_feow")
    } else {
      show("within_feow")
    }
  })

  # -------- Weighting schema
  cursor_ref <- reactive({
    switch(input$objective,
      # WSI, SAR, Rich, Q, FBCI, CCI
      # "Median across co-authors" = c(-1, 1.12, 1, 1.72, -1.1, -0.5), # old
      #"Median across co-authors" = c(-1, 1.63, 1.75, 2.92, -1, 0.5),
      "Area-based protection" = c(-1, 1.1, 1, 1.7, -1.1, -0.5),
      "Restoration" = c(2.5, 1.4, 1, 1.2, 0.1, -1),
      "SAR management" = c(1.5, 2.1, 0, 1.3, 1.3, 1),
      "AIS management" = c(1.4, 1, 1, 1, 1.8, 0.5),
      #"Custom" = rep(1, 6),
      stop("Unknown schema")
    )
  })

  observe({
    curs <- cursor_ref()
    updateSliderInput(inputId = "WSI", value = curs[1])
    updateSliderInput(inputId = "SARI", value = curs[2])
    updateSliderInput(inputId = "Richness", value = curs[3])
    updateSliderInput(inputId = "Q", value = curs[4])
    updateSliderInput(inputId = "FBCI", value = curs[5])
    updateSliderInput(inputId = "CCI", value = curs[6])
  })

  # Reset sliders buttons
  observeEvent(input$reset, {
    curs <- cursor_ref()
    updateSliderInput(inputId = "WSI", value = curs[1])
    updateSliderInput(inputId = "SARI", value = curs[2])
    updateSliderInput(inputId = "Richness", value = curs[3])
    updateSliderInput(inputId = "Q", value = curs[4])
    updateSliderInput(inputId = "FBCI", value = curs[5])
    updateSliderInput(inputId = "CCI", value = curs[6])
  })


  # -------- Map
  # load initial map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(providers$Esri.WorldTopoMap,
        options = providerTileOptions(
          updateWhenZooming = FALSE, # map won't update tiles until zoom is done
          updateWhenIdle = TRUE # map won't load new tiles when panning
        )
      ) |>
      leafem::addMouseCoordinates() |>
      fitBounds(lat1 = 43, lng1 = -143, lat2 = 75, lng2 = -50)
  })
  # Generate map
  observeEvent(input$go, {
    if (
      all(c(
        input$WSI, input$SARI, input$Richness, input$Q, input$FBCI, input$CCI
      ) == 0)
    ) {
      showNotification("Setting all weights to 0 will result in non-informative maps.", type = "warning", duration = 5)
    } else {
      show_modal_spinner() # show the modal window

      if (input$scale == "Hydrobasin level 6") {
        map <- map6
      } else {
        map <- mapl
      }
      bbox <- sf::st_bbox(map)

      map <- map |>
        dplyr::mutate(
          score =
            WSI_n * input$WSI +
              FBCI_n * input$FBCI +
              CCI_n * input$CCI +
              SARI_n * input$SARI +
              Fish_richness_n * input$Richness +
              Priority_n * input$Q
        )

      # popup labels for watersheds
      wsh_labels <- lapply(seq(nrow(map)), function(i) {
        paste0(
          "Watershed stress: ", round(map$WSI_n[i], 2), "<br>",
          "SAR richness: ", round(map$SARI_n[i], 2), "<br>",
          "Fish species richness: ", round(map$Fish_richness_n[i], 2), "<br>",
          "Biodiversity change: ", round(map$FBCI_n[i], 2), "<br>",
          "Fish rarity: ", round(map$Priority_n[i], 2), "<br>",
          "Climate change: ", round(map$CCI_n[i], 2), "<br>"
        )
      })

      if (input$scale == "Lake Erie") {
        map <- map |>
          dplyr::mutate(watershed_rank = rank(-score))

        pal <- viridis::viridis(max(map$watershed_rank),
          option = "magma",
          direction = -1,
          end = 0.9
        )

        map$wsh_fill <- pal[map$watershed_rank]

        leafletProxy("map") |>
          removeLayersControl() |>
          clearShapes() |>
          clearControls() |>
          leafem::addMouseCoordinates() |>
          addPolygons(
            data = map,
            fillColor = map$wsh_fill,
            color = map$wsh_fill,
            opacity = 0.6,
            weight = 2,
            label = lapply(wsh_labels, htmltools::HTML),
            fillOpacity = 0.8,
            highlightOptions = highlightOptions(
              color = "white", weight = 2,
              bringToFront = TRUE
            )
          ) |>
          addLegend(
            colors = viridis::viridis(9, alpha = 1, end = 0.9, direction = -1, option = "magma"),
            labels = c("high", rep("", 7), "low"),
            title = "Priority", group = "watersheds"
          ) |>
          leaflet::setView(
            lng = mean(bbox[c(1, 3)]),
            lat = mean(bbox[c(2, 4)]),
            zoom = 8
          )
      } else {
        if (!input$regional) {
          map <- map |>
            dplyr::mutate(watershed_rank = rank(-score))
        } else {
          map <- map |>
            dplyr::arrange(FEOW_ID, -score) |>
            dplyr::group_by(FEOW_ID) |>
            dplyr::mutate(watershed_rank = dplyr::row_number())

          # convert to relative ranks
          maxrank <- max(map$watershed_rank - 1)
          map <- map |>
            dplyr::group_by(FEOW_ID) |>
            dplyr::mutate(watershed_rank = ((watershed_rank - 1) * maxrank / max(watershed_rank - 1)) + 1)

          map$watershed_rank[which(is.nan(map$watershed_rank))] <- (maxrank + 1) / 2
          # if there is only 1 watershed in feow
        }


        pal <- viridis::viridis(max(map$watershed_rank),
          option = "magma",
          direction = -1,
          end = 0.9
        )
        map$wsh_fill <- pal[map$watershed_rank]

        # map it
        leafletProxy("map") |>
          clearShapes() |>
          clearControls() |>
          addPolygons(
            data = map,
            fillColor = map$wsh_fill,
            color = map$wsh_fill,
            opacity = 0.6,
            weight = 2,
            group = "Watersheds",
            label = lapply(wsh_labels, htmltools::HTML),
            fillOpacity = 0.8,
            highlightOptions = highlightOptions(
              color = "white", weight = 2,
              bringToFront = TRUE
            )
          ) |>
          addPolygons(
            data = feow,
            color = "black",
            weight = 3,
            fillColor = "transparent",
            fillOpacity = 0,
            group = "Ecoregions",
            opacity = 1
          ) |>
          hideGroup(group = "Ecoregions") |>
          addLegend(
            colors = viridis::viridis(9, alpha = 1, end = 0.9, direction = -1, option = "magma"),
            labels = c("high", rep("", 7), "low"),
            title = "Priority", group = "watersheds"
          ) |>
          addLayersControl(
            overlayGroups = c("Watersheds", "Ecoregions"),
            position = "bottomright",
            options = layersControlOptions(collapsed = FALSE)
          ) |>
          fitBounds(lat1 = 43, lng1 = -143, lat2 = 75, lng2 = -50)
      }

      remove_modal_spinner() # remove it when done
    }
  }) # end of 'go' button
} # end of server

shinyApp(ui = ui, server = server)
