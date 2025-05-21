library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)

mapUI <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header("Map View"),
    card_body(
      leafletOutput(ns("map"), height = "500px")%>% withSpinner(color = "#3734eb", type = 1, hide.ui = FALSE)
    )
  )
}

mapServer <- function(id, shared, fs_cache, load_map_data_memo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
    # Reactive to load the original (unfiltered) map data.
    original_data <- reactive({
      req(shared$selectedProperty)
      
      df <- load_map_data_memo(shared$selectedProperty)
      
      
      if (is.null(df)) {
        showNotification(paste("Map data file not found for", shared$selectedProperty), type = "error")
      }
      
      df
    })

    # Reactive to filter the original data based on shared$modelType and shared$selectedGroupName.
    filtered_data <- reactive({
      # Start fresh from the original data every time
      df <- original_data()[, 1:27]
      

      if (is.null(shared$modelType)) {
        color_col <- NULL
      } else {
        # Determine the column to use for coloring/filtering
        color_col <- switch(shared$modelType,
                            "order" = "taxonomic_order",
                            "lulc"  = "LULC",
                            "mlra"  = "MLRA",
                            NULL)
      }
      
      # If a valid group is selected, filter the data
      if (!is.null(color_col) && !is.null(shared$selectedGroupName) && shared$selectedGroupName != "") {

        if (shared$modelType == "order") {
          filter <- tolower(shared$selectedGroupName)
        } else {
          filter <- shared$selectedGroupName
        }
        
        # add if statement here to see if we filter for non spatial data!!!!!
        
        df <- df[df[[color_col]] == filter, ]
        # New name for coords column with .txt files
        df <- df[!is.na(df$latitude_std_decimal_degrees) &
                   !is.na(df$longitude_std_decimal_degrees), ]
      }
      
      df
    })

    output$map <- renderLeaflet({
      req(filtered_data())
      
      df <- filtered_data()
      
      if (is.null(shared$modelType)) {
        color_col <- NULL
      } else {
        # Determine the column to use for coloring/filtering
        color_col <- switch(shared$modelType,
                            "order" = "taxonomic_order",
                            "lulc"  = "LULC",
                            "mlra"  = "MLRA",
                            NULL)
      }
      
      
      # Convert the data to an sf object for leaflet mapping.
      sample_locations <- st_as_sf(df, coords = c("longitude_std_decimal_degrees", "latitude_std_decimal_degrees"), crs = 4326)
      
      if (nrow(df) == 0) {
        showNotification("No points available for this selection.", type = "warning")
      }
      
      # Prepare the popup columns (only include columns that exist in the data)
      popup_cols <- c("analyte_name", "taxon_name", "taxon_kind", "taxonomic_classification_name",
                      "taxonomic_order", "taxonomic_suborder", "taxonomic_subgroup",
                      "taxonomic_moisture_subclass", "taxonomic_temp_regime", "LULC")
      popup_cols <- popup_cols[popup_cols %in% names(df)]
      
      # Generate popup content for each point.
      popup_content <- apply(df[popup_cols], 1, function(row) {
        paste0("<b>", popup_cols, ":</b> ", row, collapse = "<br>")
      })
      
      # Build the color palette if applicable.
      pal <- NULL
      if (!is.null(color_col) && color_col %in% names(df) && nrow(df) > 0) {
        categories <- unique(df[[color_col]])
        pal <- colorFactor(brewer.pal(min(length(categories), 12), "Paired"), df[[color_col]])
      }
      
      # Create the leaflet map.
      m <- leaflet(sample_locations) %>% addTiles()
      if (!is.null(pal) && !is.null(color_col)) {
        m <- m %>%
          addCircleMarkers(
            radius = 5,
            color = ~pal(get(color_col)),
            stroke = FALSE, fillOpacity = 0.8,
            popup = popup_content
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = ~get(color_col),
            title = switch(shared$modelType,
                           "order" = "Soil Order",
                           "lulc"  = "Landuse",
                           "mlra"  = "MLRA",
                           "Categories"),
            opacity = 1
          )
      } else {
        m <- m %>%
          addCircleMarkers(
            radius = 5,
            color = "black",
            stroke = FALSE, fillOpacity = 0.8,
            popup = popup_content
          )
      }
      
      m
    }) 
  })
}
