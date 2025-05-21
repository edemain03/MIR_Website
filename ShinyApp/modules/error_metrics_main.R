# modules/error_metrics_module.R

library(shiny)
library(dplyr)

# UI function for error metrics module
errorMetricsUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      inputId = ns("sort_method"),
      label = "Sort by",
      choices = c("Stratification" = "strat", "Best R²" = "best"),
      selected = "strat",
      inline = TRUE
    ),
    # This selectInput is only visible when user chooses "Stratification"
    conditionalPanel(
      condition = "input.sort_method == 'strat'",
      ns = ns,  # ensures the condition is scoped to this module
      selectInput(
        inputId = ns("strat_select"),
        label = "Choose Stratification Type",
        choices = c("Order", "Global", "Texture", "Depth", "MLRA", "LULC"),
        selected = "Order"
      )
    ),
    tableOutput(ns("error_table"))
  )
}


# Server function for error metrics module
errorMetricsServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    
    dir_mapping <- list(
      "Order"   = "models/Orders",
      "Global"  = "models/Global",
      "Texture" = "models/Texture_classes",
      "Depth"   = "models/Depths",
      "MLRA"    = "models/MLRA",
      "LULC"    = "models/LULC"
    )
    
    # List of machine learning model subdirectories (each folder name within a given strat folder)
    ml_models <- c("Cubist", "PLS", "RF", "SVM", "CNN")
    
    # Reactive expression to search through all CSV files in all the specified directories
    compileAllErrors <- reactive({
      all_dfs <- list()
      
      for (category in names(dir_mapping)) {
        base_dir <- dir_mapping[[category]]
        for (ml_model in ml_models) {
          folder <- file.path(base_dir, ml_model)
          if (!dir.exists(folder)) next
          
          csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
          if (length(csv_files) == 0) next
          
          for (f in csv_files) {
            df <- tryCatch(read.csv(f, stringsAsFactors = FALSE),
                           error = function(e) NULL)
            if (is.null(df)) next
            
            # 1) Add metadata: record stratification type and ml model
            df$ModelType <- category
            df$ML_Model  <- ml_model
            
            # 2) Rename or add strat column
            if (category == "Global") {
              # For Global, add a 'strat' column with NA (as character)
              df$strat <- NA_character_
            } else {
              if (category %in% names(df)) {
                names(df)[names(df) == category] <- "strat"
              } else {
                df$strat <- NA_character_
              }
            }
            
            # 3) Ensure that the 'strat' column is character (in case it was numeric or factor)
            df$strat <- as.character(df$strat)
            
            # 4) Ensure required columns exist; if missing add them as NA
            req_cols <- c("filename", "R2.val", "RMSE.val", "RPIQ.val", "RPD.val")
            for (col in req_cols) {
              if (!(col %in% names(df))) df[[col]] <- NA
            }
            
            # 5) Subset only the needed columns:
            keep_cols <- c("filename", "ML_Model", "ModelType", "strat", "R2.val",
                           "RMSE.val", "RPIQ.val", "RPD.val")
            df_subset <- df[, keep_cols, drop = FALSE]
            
            # 6) Append to the list
            all_dfs[[length(all_dfs) + 1]] <- df_subset
          }
        }
      }
      
      # 7) Combine into one large data frame, or return an empty data frame if nothing was found.
      if (length(all_dfs) == 0) {
        return(data.frame())
      } else {
        big_df <- dplyr::bind_rows(all_dfs)
        return(big_df)
      }
    })
    
    
    # Reactive filtering: match the compiled data against the selected soil property.
    filteredData <- reactive({
      df <- compileAllErrors()
      if (nrow(df) == 0) return(df)
      
      prop <- shared$selectedProperty
      if (!is.null(prop) && prop != "") {
        df <- df[grepl(prop, df$filename, ignore.case = TRUE), ]
      }
      df
    })
    
    
    # Based on user sort method selection, filter and sort the data.
    displayedData <- reactive({
      df <- filteredData()
      if (nrow(df) == 0) return(df)
      
      if (is.null(input$sort_method)) input$sort_method <- "best"
      
      if (input$sort_method == "best") {
        # Order all models (for the chosen soil property) in descending order of R2.val.
        df <- arrange(df, desc(R2.val))
      } else if (input$sort_method == "strat") {
        # Filter rows based on the stratification selection.
        if (!is.null(input$strat_select) && input$strat_select != "") {
          if (input$strat_select == "Global") {
            df <- df[df$ModelType == "Global", ]
          } else {
            df <- df[df$ModelType == input$strat_select, ]
          }
        }
        # Optionally, sort these filtered rows by ML_Model or another metric.
        df <- arrange(df, ML_Model)
        # Remove the filename column for stratification display
        df <- df[, colnames(df) != "filename", drop = FALSE]
      }
      df
    })
    
    
    # Render the final table output with capitalized column names
    output$error_table <- renderTable({
      df <- displayedData()
      if (nrow(df) == 0) {
        return(data.frame(Message = "No data found"))
      }
      
      # Helper function to capitalize first letter of each column name
      capitalize <- function(x) {
        paste0(toupper(substr(x, 1, 1)), substring(x, 2))
      }
      colnames(df) <- sapply(colnames(df), capitalize, USE.NAMES = FALSE)
      
      df
    })
    
  })
}
