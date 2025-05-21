library(shiny)
library(dplyr)
library(knitr)
library(prospectr)  
library(ggplot2)

# dataInputUI remains the same as before
dataInputUI <- function(id) {
  
  ns <- NS(id)
  card(
    card_header("Data Input"),
    card_body(
      fileInput(
        ns("file1"),
        "Upload CSV File",
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      actionButton(ns("predict"), "Make Prediction"),
      br(),
      downloadButton(ns("downloadData"), "Download Predictions & Metadata")
    )
  )
  
}

dataInputServer <- function(id, shared, load_spectral_data_memo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
    # Reactive expression to read uploaded CSV data
    dataInput <- reactive({
      req(input$file1)
      inFile <- input$file1
      df <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
      
      if (ncol(df) < 342 || ncol(df) > 342) {
        showNotification("CSV File has unexpected format, please refer to data preprocessing for instructions on formatting.", type = "error")
        return(NULL)
      } else if (!all(names(df)[2:342] == as.character(seq(4000, 600, by = -10)))) {
        showNotification("CSV File has unexpected format, please refer to data preprocessing for instructions on formatting.", type = "error")
        return(NULL)
      }
      
      # We assume the first column is a sample ID, 
      # and columns 2..end are the MIR intensities from 4000..600 by -10
      smp_ID <- df[, 1, drop = FALSE]
      MIR    <- df[, 2:ncol(df)]
      
      # If your user data is at 10 cm^-1 intervals from 4000..600:
      colnames(MIR) <- seq(4000, 600, by = -10)
      
      list(MIR = MIR, smp_ID = smp_ID)
    })
    
    observeEvent(input$file1, {
      shared$df <- dataInput()$MIR
    })
    
    doPLSPCA <- function(propertyName, userMIR) {
      # load csv from memoise function
      dat <- load_spectral_data_memo(propertyName)
      if (is.null(dat) || nrow(dat) == 0) {
        showNotification(
          paste("No calibration data found for property:", propertyName),
          type = "error"
        )
        return(NULL)
      }
      
      soil <- dat[, c(1:27)]
      MIR  <- dat[, c(38:1792)]
      colnames(MIR) <- seq(4000, 600, by = -1.927)
      MIR <- as.matrix(MIR)
      
      # Preprocessing steps: Savitzky–Golay smoothing, resampling, SNV, etc.
      MIR.sg <- savitzkyGolay(MIR, m = 0, w = 13, p = 2)
      wav <- as.numeric(colnames(MIR.sg))
      new.wav <- seq(4000, 600, by = -10)
      MIR.res <- resample(MIR.sg, wav, new.wav)
      MIR.snv <- standardNormalVariate(MIR.res)
      
      # Create calibration (training) set
      nd <- nrow(soil)
      set.seed(123)
      ic <- sample(1:nd, round(nd * 0.7))  # 70% for calibration
      Spec_Calib <- MIR.snv[ic, ]
      
      # Process the user data in the same way:
      userMIR <- as.matrix(userMIR)
      colnames(userMIR) <- seq(4000, 600, by = -10)
      user.sg <- savitzkyGolay(userMIR, m = 0, w = 13, p = 2)
      user.wav <- as.numeric(colnames(user.sg))
      user.res <- resample(user.sg, user.wav, new.wav)
      user.snv <- standardNormalVariate(user.res)
      
      # Combine the calibration set with user data for visualization
      spec <- rbind(Spec_Calib, user.snv)
      
      # Run PCA on the combined data
      pca_res <- prcomp(spec, scale. = TRUE)
      pca_df <- as.data.frame(pca_res$x)
      
      # Split the PCA scores into calibration and input sets:
      calib_scores <- pca_df[1:nrow(Spec_Calib), 1:2]
      input_scores <- pca_df[(nrow(Spec_Calib) + 1):nrow(pca_df), 1:2]
      
      # Build the base PCA plot using the same style as non-PLS plots:
      p <- ggplot() +
        theme_dsh() +
        scale_color_dsh() +
        geom_point(data = calib_scores, aes(x = PC1, y = PC2),
                   color = "gold", size = 3, alpha = 0.5) +
        geom_point(data = input_scores, aes(x = PC1, y = PC2),
                   color = "red", size = 3) +
        theme(legend.position = "bottom") +
        labs(x = paste0("PC1 (", round(100 * (pca_res$sdev[1]^2 / sum(pca_res$sdev^2)), 2), "%)"),
             y = paste0("PC2 (", round(100 * (pca_res$sdev[2]^2 / sum(pca_res$sdev^2)), 2), "%)"),
             subtitle = "Gold: calibration data | Red: input data")
      
      return(p)
    }
    
    #===============================================================================
    
    # Observe event to make predictions when 'predict' button is clicked
    observeEvent(input$predict, {
      req(dataInput())
      
      print(shared$modelType)
      
      shared$tablemlModel <- shared$mlModel
      shared$tablemodelType <- shared$modelType
      shared$tablestrata <- shared$selectedGroupName
      
      # Let modelSelectionServer know the user clicked 'predict'
      shared$predictClicked <- TRUE
      
      if (is.null(shared$usedModel)) {
        showNotification("Please select a model before making predictions", type = "error")
        return()
      }
      if (is.null(shared$mlModel)) {
        showNotification("Please select a machine learning model before making predictions", type = "error")
        return()
      }
      
      userData <- dataInput()
      df       <- userData$MIR
      smp_ID   <- userData$smp_ID
      model_choice <- shared$usedModel
      
      #-------------------------------------
      # If PLS, do the *exact* PCA code
      #-------------------------------------
      if (shared$mlModel == "PLS") {
        # Make predictions
        predictions <- round(predict(model_choice, newdata = df), 2)
        
        if (is.null(predictions)) {
          showNotification("Error making predictions", type = "error")
          return()
        } else {
          showNotification("Predictions made Successfully", type = "message")
        }
        
        # Build the PCA plot using the exact snippet approach
        if (is.null(shared$selectedProperty)) {
          # fallback if no property chosen
          shared$pcaPlot <- ggplot() + theme_void() +
            annotate("text", x=0.5, y=0.5, label="No property selected")
        } else {
          pca_plot <- doPLSPCA(shared$selectedProperty, df)
          if (is.null(pca_plot)) {
            shared$pcaPlot <- ggplot() + theme_void() +
              annotate("text", x=0.5, y=0.5, label="Error producing PCA.")
          } else {
            shared$pcaPlot <- pca_plot
          }
        }
        
      } else if (shared$mlModel == "CNN") {  # **Added for CNN models**
        # Make predictions using the CNN model
        predictions <- tryCatch({
          round(predict(shared$usedModel, as.matrix(df)), 2)
        }, error = function(e) {
          showNotification(paste("Error making predictions with CNN model:", e$message), type = "error")
          NULL
        })
        
        if (!is.null(predictions)) {
          predictions_vector <- as.vector(predictions)
          outputFile <- data.frame("Sample ID" = smp_ID, "Predictions" = predictions_vector)
          shared$predictions <- outputFile
          showNotification("Predictions made successfully with CNN model", type = "message")
        } else {
          shared$predictions <- NULL
        }
        
        # Do not assign shared$pcaPlot for CNN models
      } else {  # Existing logic for non-PLS, non-CNN models
        # All other models use the previously loaded PCA (shared$usedPCA)
        pca_model <- shared$usedPCA
        if (is.null(pca_model)) {
          showNotification("PCA model not loaded", type = "error")
          return()
        }
        
        # Determine the number of PCs to keep (to explain 99% of variance)
        cum_var <- cumsum(pca_model$sdev^2) / sum(pca_model$sdev^2)
        num_components <- which(cum_var >= 0.99)[1]
        num_components <- min(num_components, ncol(pca_model$rotation))
        
        # Project the user data (df) into the PCA space of the calibration data
        Spec_Valid_PCA <- predict(pca_model, newdata = df)[, 1:num_components, drop = FALSE]
        
        # Build data frames for plotting using the first two principal components
        train_pca_scores <- data.frame(PC1 = pca_model$x[, 1],
                                       PC2 = pca_model$x[, 2])
        new_pca_scores   <- data.frame(PC1 = Spec_Valid_PCA[, 1],
                                       PC2 = Spec_Valid_PCA[, 2])
        
        # Create the base PCA plot:
        pca_plot <- ggplot(train_pca_scores, aes(x = PC1, y = PC2)) +
          theme_dsh() +
          scale_color_dsh() +
          geom_point(alpha = 0.5, size = 3, color = "gold") +
          geom_point(data = new_pca_scores, aes(x = PC1, y = PC2),
                     size = 3, color = "red") +
          theme(legend.position = "bottom") +
          labs(subtitle = "Gold: calibration data | Red: input data")
        
        shared$pcaPlot <- pca_plot
        
        # Make predictions using the selected model and the user data projected into the PCA space.
        predictions <- round(predict(model_choice, newdata = Spec_Valid_PCA), 2)
        if (is.null(predictions)) {
          showNotification("Error making predictions", type = "error")
          return()
        } else {
        showNotification("Predictions made Successfully", type = "message")
        }
      }
      
      # Collect predictions
      predictions_vector <- if (is.matrix(predictions) || is.array(predictions)) {
        as.vector(predictions)
      } else {
        predictions
      }
      
      outputFile <- data.frame("Sample ID" = smp_ID, "Predictions" = predictions_vector)
      shared$predictions <- outputFile
    })
    
    getErrorMetricsForModel <- function(shared) {
      # Define model directories and available machine learning models
      modelTypeDirs <- c(
        Orders = "models/Orders",
        Texture_classes = "models/Texture_classes",
        Depths = "models/Depths",
        LULC = "models/LULC",
        MLRA = "models/MLRA",
        Global = "models/Global"
      )
      mlModels <- c("Cubist", "RF", "SVM", "PLS")  # No CNN here
      
      all_dfs <- list()
      
      for (typeDir in modelTypeDirs) {
        for (mlm in mlModels) {
          folder <- file.path(typeDir, mlm)
          if (!dir.exists(folder)) next
          
          csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
          if (length(csv_files) == 0) next
          
          for (f in csv_files) {
            df <- read.csv(f, stringsAsFactors = FALSE)
            # Tag with folder/model information
            df$ModelType <- basename(typeDir)
            df$ML_Model  <- mlm
            all_dfs[[length(all_dfs) + 1]] <- df
          }
        }
      }
      
      if (length(all_dfs) == 0) {
        return(NULL)
      }
      
      big_df <- dplyr::bind_rows(all_dfs)
      
      # Filter by the selected soil property
      if (!is.null(shared$selectedProperty) && shared$selectedProperty != "") {
        big_df <- dplyr::filter(big_df, grepl(shared$selectedProperty, big_df$filename, ignore.case = FALSE))
      }
      
      # Filter for just the user’s chosen model
      if (!is.null(shared$mlModel) && shared$mlModel != "") {
        big_df <- dplyr::filter(big_df, ML_Model == shared$mlModel)
        if (shared$modelType != "global") {
          pattern <- paste0("^", shared$selectedProperty, "_", shared$selectedGroupName, "$")
          keep <- grepl(pattern, big_df$filename, ignore.case = TRUE)
      
          big_df <- big_df[keep, ]
        } else {
          keep <- grepl(paste0(shared$selectedProperty, "_Final"), big_df$filename, ignore.case = TRUE)
   
          big_df <- big_df[keep, ]
        }
      }
      
      # Remove duplicate rows if needed
      big_df <- dplyr::distinct(big_df, filename, ModelType, ML_Model, .keep_all = TRUE)
      
      return(big_df)
    }
    
    
    
    
    # Download handler: Bundle predictions and metadata into a zip file
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("predictions_", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        tmpdir <- tempdir()
        
        # Write predictions CSV file
        predictions_file <- file.path(tmpdir, "predictions.csv")
        write.csv(shared$predictions, predictions_file, row.names = FALSE)
        
        # Retrieve general metadata information
        mlModel         <- if (!is.null(shared$mlModel)) shared$mlModel else "Not specified"
        modeltype       <- if (!is.null(shared$modelType)) shared$modelType else "Not specified"
        stratification  <- if (!is.null(shared$selectedGroupName)) shared$selectedGroupName else "Not specified"
        systime         <- as.character(Sys.time())
        sysdate         <- as.character(Sys.Date())
        
        # Retrieve error metrics for the chosen soil property and model
        error_metrics_df <- getErrorMetricsForModel(shared)
        print(error_metrics_df)
        if (!is.null(error_metrics_df) && nrow(error_metrics_df) > 0) {
          # Build a text block listing error metrics row by row.
          error_metrics_text <- paste(
            apply(error_metrics_df, 1, function(row) {
              paste(
                "Dataset:", row["filename"],
                "R2:", row["R2.val"],
                "RMSE:", row["RMSE.val"],
                "RPIQ:", row["RPIQ.val"],
                "RPD:", row["RPD.val"]
              )
            }),
            collapse = "\n"
          )
        } else {
          error_metrics_text <- "No error metrics available for the chosen model"
        }
        
        # Build metadata content with error metrics appended
        metadata_content <- paste(
          "Project Title: Soil Property Prediction",
          "\nVersion 1.0",
          "\nsystime:", systime,
          "\nsysdate:", sysdate,
          "\nData:",
          "\n      Source: KSSL Reference Dataset",
          "\n      Strata: ", shared$modelType, shared$selectedGroupName,
          "\n      Soil Property:", shared$selectedProperty,
          "\n      Format: CSV",
          "\nModel Info:",
          "\n      mlModel:", mlModel,
          "\n      modeltype:", modeltype,
          "\n      stratification:", stratification,
          "\n      Data Splitting: 70/30 (training/testing)",
          "\n\nError Metrics:\n", error_metrics_text,
          "\nPreprocessing:",
          "\n      Spectral Range: 4000-600 cm^-1",
          "\n      Resolution: 10 cm^-1",
          "\n      Noise Reduction: Savitzky-Golay (m=0, w=13, p=2)",
          "\n      Normalization: Standard Normal Variate",
          "\nSoftware: R 4.2.0 (caret, pls, tidymodels, prospectr, keras, tensorflow, randomForest"
        )
        
        # Write the metadata file
        metadata_file <- file.path(tmpdir, "metadata.txt")
        writeLines(metadata_content, metadata_file)
        
        # Create a zip file including both predictions and metadata files
        old_wd <- getwd()
        setwd(tmpdir)
        zip(zipfile = file, files = c("predictions.csv", "metadata.txt"))
        setwd(old_wd)
      },
      contentType = "application/zip"
    )
    
    
    
  })
}
