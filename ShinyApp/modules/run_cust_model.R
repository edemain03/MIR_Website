library(shiny)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(ggplot2)
library(keras)  # Added to support Keras CNN models

customModelUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    shinyjs::useShinyjs(),

    layout_column_wrap(
      width = 1/2,
      fill  = TRUE,         # <-- makes each wrap item the same height
      card(
        card_header("Model Input"),
        card_body(
          # Row 1: modelFile on the left, Use PCA? on the right
          fluidRow(
            column(
              width = 6,
              fileInput(
                ns("modelFile"),
                label = "Upload Your .rds or .keras/.h5 Model File",
                accept = c(".rds", ".keras", ".h5"),
                buttonLabel = "Browse"
              )
            ),
            column(
              width = 4,
              # align checkbox vertically with the fileInput
              div(style="margin-top: 2.2em; margin-left: 0.5em;",
                  checkboxInput(
                    ns("use_pca"),
                    "Use PCA? (Only if PCA was used during training)",
                    value = FALSE
                  )
              )
            )
          ),
          
          # Row 2 (only if PCA is TRUE): two inputs, half‑width each
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("use_pca")),
            fluidRow(
              column(
                width = 6,
                fileInput(
                  ns("pcaFile"),
                  label = "Upload Your PCA Model File",
                  accept = ".rds",
                  buttonLabel = "Browse"
                )
              ),
              column(
                width = 6,
                fileInput(
                  ns("dataFile"),
                  label = "Upload Your Data File",
                  accept = ".csv",
                  buttonLabel = "Browse"
                )
              )
            )
          ),
          
          # Row 2 (only if PCA is FALSE): single full‑width data upload
          conditionalPanel(
            condition = sprintf("input['%s'] == false", ns("use_pca")),
            fluidRow(
              column(
                width = 12,
                fileInput(
                  ns("dataFile"),
                  label = "Upload Your Data File",
                  accept = ".csv",
                  buttonLabel = "Browse"
                )
              )
            )
          )
        )
        
      ),
      card(
        card_header("Results Table"),
        card_body(
          DTOutput(ns("results_table"))
        )
      )
    )
    ,
    layout_column_wrap(
      downloadButton(ns("downloadData"), "Download Predictions"),
      actionButton(ns("run_cust_model"), "Run Custom Model")
    )
  )
}

customModelServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive values to store models, data, and predictions
    lv <- reactiveValues(
      model = NULL,       # Uploaded main model (either .rds or keras)
      model_type = NULL,  # "rds" or "keras"
      pca_model = NULL,   # Uploaded PCA model (if any)
      use_pca = FALSE,    # Whether to use PCA
      data = NULL,        # Uploaded data for prediction (numeric features)
      preds = NULL,       # Prediction results
      pca_scores = NULL   # PCA scores for new data
    )
    
    # Load the main model file (.rds, .zip, .keras, or .h5)
    observeEvent(input$modelFile, {
      req(input$modelFile)
      file_ext <- tools::file_ext(input$modelFile$name)
      
      if(file_ext %in% c("rds", "zip")){
        tryCatch({
          lv$model <- readRDS(input$modelFile$datapath)
          lv$model_type <- "rds"
          cat("Main model loaded successfully.\n")
          showNotification("Main model loaded successfully.", type = "message")
        }, error = function(e) {
          showNotification(paste("Error loading main model:", e$message), type = "error")
          lv$model <- NULL
        })
      } else if(file_ext %in% c("keras", "h5")) {
        tryCatch({
          lv$model <- load_model_hdf5(input$modelFile$datapath)
          lv$model_type <- "keras"
          cat("Keras model loaded successfully.\n")
          showNotification("Keras model loaded successfully.", type = "message")
        }, error = function(e) {
          showNotification(paste("Error loading Keras model:", e$message), type = "error")
          lv$model <- NULL
        })
      } else {
        showNotification("Unsupported model file type. Please upload a .rds or .keras/.h5 file.", type = "error")
        lv$model <- NULL
      }
    })
    
    # Toggle PCA usage
    observeEvent(input$use_pca, {
      lv$use_pca <- input$use_pca
    })
    
    # Load the PCA model (.rds) if PCA is to be used
    observeEvent(input$pcaFile, {
      req(input$pcaFile)
      tryCatch({
        lv$pca_model <- readRDS(input$pcaFile$datapath)
        cat("PCA model loaded successfully.\n")
        showNotification("PCA model loaded successfully.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error loading PCA model:", e$message), type = "error")
        lv$pca_model <- NULL
      })
    })
    
    # Load and validate the data file (.csv)
    observeEvent(input$dataFile, {
      req(input$dataFile)
      tryCatch({
        df <- read.csv(
          input$dataFile$datapath,
          header = TRUE,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        cat("Data file loaded successfully.\n")
        
        if (ncol(df) < 2) {
          showNotification(
            "Data file must contain at least two columns (Sample ID and features).",
            type = "error"
          )
          lv$data <- NULL
          return()
        }
        
        # Extract Sample IDs (first column) and features (remaining columns)
        lv$smp_ID <- df[, 1, drop = TRUE]
        data_features <- df[, -1, drop = FALSE]
        
        # Ensure column names are character type
        colnames(data_features) <- as.character(colnames(data_features))
        
        # Convert feature columns to numeric if possible and check for non-numeric entries
        for(col in colnames(data_features)) {
          if(!is.numeric(data_features[[col]])) {
            converted <- suppressWarnings(as.numeric(data_features[[col]]))
            if(any(is.na(converted))) {
              showNotification(
                paste("Feature column", col, "contains non-numeric values. Please ensure all feature columns are numeric."),
                type = "error"
              )
              lv$data <- NULL
              return()
            } else {
              data_features[[col]] <- converted
            }
          }
        }
        
        lv$data <- data_features
        cat("Data preprocessing completed.\n")
        showNotification("Data file loaded and preprocessed successfully.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error loading data file:", e$message), type = "error")
        lv$data <- NULL
      })
    })
    
    # -- Render PCA Plot (training vs new data) --
    output$pca_plot <- renderPlot({
      req(lv$use_pca == TRUE)      # only if user wants PCA
      req(lv$pca_model)            # PCA model is loaded
      req(lv$pca_model$x)          # prcomp object with training scores
      req(lv$pca_scores)           # new data's PCA scores must exist
      
      # Prepare data frames for plotting
      train_pca_scores <- data.frame(
        PC1 = lv$pca_model$x[, 1],
        PC2 = lv$pca_model$x[, 2]
      )
      new_pca_scores <- data.frame(
        PC1 = lv$pca_scores[, 1],
        PC2 = lv$pca_scores[, 2]
      )
      
      ggplot(train_pca_scores, aes(x = PC1, y = PC2)) +
        geom_point(alpha = 0.5, size = 3, color = "gold") +
        geom_point(
          data = new_pca_scores,
          aes(x = PC1, y = PC2),
          size = 3, color = "red"
        ) +
        theme_light() +
        theme(legend.position = "bottom") +
        labs(subtitle = "Red dots represent your sample points in the PCA space")
    })
    
    # Run Custom Model Predictions
    observeEvent(input$run_cust_model, {
      # Validate required inputs
      if (is.null(lv$model)) {
        showNotification("Please upload a valid main model file (.rds or .keras/.h5).", type = "error")
        return()
      }
      if (is.null(lv$data)) {
        showNotification("Please upload a valid data file (.csv).", type = "error")
        return()
      }
      if (lv$use_pca && is.null(lv$pca_model)) {
        showNotification("Please upload a valid PCA model file (.rds) when using PCA.", type = "error")
        return()
      }
      
      # Proceed with predictions
      withProgress(message = "Running predictions...", value = 0, {
        incProgress(0.2, detail = "Preparing data for prediction")
        
        processed_data <- lv$data
        
        # Step 1: If PCA is used, transform the new data
        if (lv$use_pca) {
          tryCatch({
            # Determine number of components (for .rds models, assumes attribute exists; otherwise, use all columns)
            ncomp <- if (lv$model_type == "rds") lv$model$num_components else ncol(processed_data)
            pca_scores <- predict(lv$pca_model, newdata = processed_data)[, 1:ncomp, drop = FALSE]
            lv$pca_scores <- pca_scores  # Store for plotting
            processed_data <- lv$pca_scores
          }, error = function(e) {
            showNotification(paste("Error during PCA transformation:", e$message), type = "error")
            return()
          })
        } else {
          # Clear any old PCA scores
          lv$pca_scores <- NULL
        }
        
        # Step 2: Making Predictions
        incProgress(0.6, detail = "Making predictions with the model")
        predictions <- NULL
        tryCatch({
          if(lv$model_type == "rds"){
            if(lv$use_pca){
              # For rds models with PCA, assume the underlying model is stored in lv$model$model
              predictions <- round(predict(lv$model$model, newdata = processed_data), 2)
            } else {
              # For rds models without PCA, use the model directly
              predictions <- round(predict(lv$model, newdata = processed_data), 2)
            }
          } else if(lv$model_type == "keras"){
            # For keras models, convert the data to a matrix before prediction
            predictions <- round(predict(lv$model, as.matrix(processed_data)), 2)
          } else {
            stop("Unknown model type.")
          }
          lv$preds <- predictions
          cat("Predictions made successfully.\n")
          incProgress(0.15, detail = "Finalizing predictions")
        }, error = function(e) {
          showNotification(paste("Error during prediction:", e$message), type = "error")
          return()
        })
        
        Sys.sleep(0.5)  # Optional pause for progress bar
      })
      
      # Step 3: Display/Download results if predictions were made
      Sys.sleep(1)
      if (!is.null(lv$preds)) {
        # Combine sample IDs with predictions
        output_df <- data.frame(
          Sample_ID = lv$smp_ID,
          Prediction = lv$preds
        )
        
        # Store predictions for download
        shared$predictions <- output_df
        
        # Render the predictions table
        output$results_table <- renderDT({
          datatable(output_df, options = list(pageLength = 10, scrollX = TRUE))
        })
        
        showNotification("Predictions completed successfully.", type = "message")
      }
    })
    
    # Download handler for predictions
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("predictions-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(lv$preds)
        write.csv(shared$predictions, file, row.names = FALSE)
      }
    )
    
  })
}
