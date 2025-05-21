library(dplyr)
library(randomForest)
library(caret)
library(caTools)
library(shiny)
library(shinyjs)
library(bslib)
library(prospectr)
library(FNN)
library(ggplot2)
library(DT)

# knn_models.R

knnUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
  column(
         width = 4,
    verticalLayout(
        card(
          card_header("Instructions"),
          card_body(
            h2("Instructions:"),
            tags$li("Upload a CSV file containing spectral data, downloaded from Data Preprocessing"),
            tags$li("Select the soil property you want to predict"),
            tags$li("Train based on distance or # of neighbors. Use distance if you require larger training datasets, or neighbors if you prefer smaller datasets"),
            tags$li("Select the model type you want to use. Choose between Cubist, RF, SVM, PLS, or CNN"),
            tags$li("For more information, see ",
                    tags$a(
                      "User Guide",
                      href = "#",
                      style = "color:#0000EE;",
                      onclick = sprintf(
                        "Shiny.setInputValue('%s', Math.random()); return false;", 
                        ns("goto_user_guide")
                      )
                    ))
          )
        ),
        card(    
          height = "250px",
          card_header("Data Input"),
          card_body(
            fileInput(
              ns("file1"),
              "Upload CSV File",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
            ),
          )
        ),
        card(
          card_header("Train Settings"),
            verticalLayout(
              selectizeInput(
                ns("soilProperty"),
                "Select Soil Property to Predict",
                choices = c(  "Sand        " = "Sand",
                              "Silt         " = "Silt",
                              "Clay         " = "Clay",
                              "Aggregate Stability" = "AS",
                              "Bulk Density" = "BD",
                              "pH        " = "pH",
                              "Electrical Conductivity" = "EC",
                              "Total Carbon" = "TC",
                              "Organic Carbon" = "SOC",
                              "Carbon (pom)" = "C_pom",
                              "Carbon (hpom)" = "C_hpom",
                              "Carbon (pom mineral)" = "C_pom_mineral",
                              "Total Nitrogen" = "TN",
                              "Phosphorus (Olsen)" = "P_Olsen",
                              "Phosphorus (Bray)" = "P_Bray",
                              "Phosphorus (Mehlich)" = "P_Mehlich3",
                              "Total Sulfur" = "TS",
                              "CEC         " = "CEC",
                              "Potassium       " = "K",
                              "Carbonate      " = "Carbonate",
                              "Gypsum" = "Gypsum"),
                selected = "Sand"
              ),
              
              verticalLayout(
                radioButtons(ns("knnType"),
                             "Train based on distance or # of neighbors?",
                             choices = c("Distance", "Neighbors"),
                             selected = "Distance",
                             inline = TRUE),
                uiOutput(ns("knnSliderUI"))%>% withSpinner(type = 1, color = "#3734eb", hide.ui = FALSE)
              ), 
              
              selectizeInput(
                ns("modelType"),
                "Select Model Type",
                choices = c("Cubist" = "cb", 
                            "Random Forest" = "rf", 
                            "Support Vector Machine" = "svm", 
                            "Partial Least Squares" = "pls",
                            "CNN" = "cnn"),
                selected = "Cubist"
              ),
              
              actionButton(ns("predict"), "Make Prediction"),
              downloadButton(ns("downloadData"), "Download Predictions & Metadata")
            )
        ),


        )),
  column(width = 8,
      verticalLayout(
        card(
          card_header("PCA Plot"),
          plotOutput(ns("knn_pcaPlot"))
          
        ),

        card(
          card_header("Internal Calibration Metrics"),
          card_body(
            div(
              style = "overflow-x:auto;",          # enable horizontal scroll on phones
              tableOutput(ns("calib_metrics"))
            )
          )
        ),
        layout_column_wrap(
          ncol = 2,          # ← two equal columns
          gap  = "16px",     # ← optional gutter
          
          # left card ─ predictions table
          card(
            card_header("Predictions"),
            DT::dataTableOutput(ns("pred_table"))
          ),
          
          # right card ─ 1:1 plot
          card(
            card_header("Calibration: Observed vs Predicted"),
            plotOutput(ns("calib_scatter"))
          )
        )
      )
  )
    )
  )
}

knnServer <- function(id, shared, load_spectral_data_memo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## keep any tidbits we want to echo into metadata.txt
    metaRV <- reactiveValues(
      metrics        = NULL,   # data‑frame from calibMetrics()
      n_neighbors    = NA,     # how many rows used to train
      knn_type       = NA,     # "Distance" / "Neighbors"
      knn_value      = NA      # threshold or k
    )
    
    trainPlotData <- reactiveVal(NULL)
    
    observeEvent(input$goto_user_guide, {
      # just flip a shared flag
      shared$goto_user_guide <- Sys.time()
      
    })
    
    #reactive to store calib metrics
    calibMetrics <- reactiveVal(data.frame(
      Metric = character(0),
      Value = numeric(0),
      stringsAsFactors = FALSE
    ))
    
    #Output said error metrics
    output$calib_metrics <- renderTable({
      df <- calibMetrics()
      validate(need(nrow(df) > 0, "No metrics available"))
      
      ## transpose so metrics run left‑to‑right
      wide <- as.data.frame(t(df$Value),       # make the values the single row
                            stringsAsFactors = FALSE)
      colnames(wide) <- df$Metric              # column names = metric labels
      wide                                           # one row, many columns
    }, rownames = FALSE)
    
    
    # Reactive to compute calibration distances for the slider input.
    calibDistances <- reactive({
      req(input$file1, input$soilProperty)
      tryCatch({
        res_var <- if (is.null(input$res_var) || input$res_var == "") "calc_value" else input$res_var
        
        # Load calibration data (using memoised function)
        cal_data <- load_spectral_data_memo(input$soilProperty)
        if (is.null(cal_data) || nrow(cal_data) == 0) {
          return(list(min = 0, max = 10, default = 4))
        }
        
        # Process calibration data.
        soil <- cal_data[, 1:27]
        MIR_new <- cal_data[, 28:1792]
        colnames(MIR_new) <- seq(4000, 600, by = -1.927)
        wav_orig <- colnames(MIR_new)
        MIR_new <- as.matrix(MIR_new)
        MIR_new <- matrix(as.numeric(MIR_new), nrow = nrow(MIR_new))
        colnames(MIR_new) <- wav_orig
        
        MIR.sg <- savitzkyGolay(MIR_new, m = 0, w = 13, p = 2)
        wav <- as.numeric(colnames(MIR.sg))
        new.wav <- seq(4000, 600, by = -10)
        MIR.res <- resample(MIR.sg, wav, new.wav)
        MIR.snv <- standardNormalVariate(MIR.res)
        
        if ("calc_value" %in% colnames(soil)) {
          calib_response <- soil[["calc_value"]]
        } else {
          calib_response <- NULL
        }
        
        calib_df <- as.data.frame(MIR.snv)
        if (!is.null(calib_response)) {
          calib_df[[res_var]] <- calib_response
        }
        calib_df$source <- "calibration"
        
        # Remove response and extra columns for PCA.
        calib_predictors <- calib_df[, setdiff(names(calib_df), c(res_var, "source")), drop = FALSE]
        
        # Read user-uploaded data.
        user_df <- tryCatch({
          read.csv(input$file1$datapath, check.names = FALSE)
        }, error = function(e) {
          showNotification(paste("Error reading user CSV file:", e$message), type = "error")
          return(NULL)
        })
        if (is.null(user_df)) return(list(min = 0, max = 10, default = 4))
        
        user_predictors <- user_df
        commonCols <- intersect(names(calib_predictors), names(user_predictors))
        calib_predictors <- calib_predictors[, commonCols, drop = FALSE]
        user_predictors  <- user_predictors[, commonCols, drop = FALSE]
        
        combined <- rbind(calib_predictors, user_predictors)
        pca_res <- prcomp(combined, center = TRUE, scale. = TRUE)
        pca_scores <- as.data.frame(pca_res$x)
        
        n_calib <- nrow(calib_predictors)
        if (n_calib == 0 || n_calib >= nrow(pca_scores)) {
          return(list(min = 0, max = 10, default = 4))
        }
        calib_scores <- pca_scores[1:n_calib, 1:2, drop = FALSE]
        user_scores  <- pca_scores[(n_calib + 1):nrow(pca_scores), 1:2, drop = FALSE]
        
        neighbors <- get.knnx(data = as.matrix(calib_scores), query = as.matrix(user_scores), k = nrow(calib_scores))
        min_dist <- round(min(neighbors$nn.dist, na.rm = TRUE), 2)
        max_dist <- round(max(neighbors$nn.dist, na.rm = TRUE), 2)
        default_val <- round((min_dist + max_dist) / 2, 2)
        
        list(min = min_dist, max = max_dist, default = default_val)
      }, error = function(e) {
        showNotification(paste("Error computing calibration distances:", e$message), type = "error")
        list(min = 0, max = 10, default = 4)
      })
    })
    
    output$knnSliderUI <- renderUI({
      req(input$knnType)
      if (input$knnType == "Distance") {
        distances <- calibDistances()
        sliderInput(ns("knnSlider"), "Distance Threshold", 
                    min = distances$min, 
                    max = distances$max, 
                    value = distances$default, 
                    step = 0.01)
      } else {
        sliderInput(ns("knnSlider"), "Number of Neighbors", min = 1, max = 10, value = 5)
      }
    })
    
    # Reactive to read user-uploaded CSV data with error handling.
    user_data <- reactive({
      req(input$file1)
      tryCatch({
        read.csv(input$file1$datapath, check.names = FALSE)
      }, error = function(e) {
        showNotification(paste("Error reading user data:", e$message), type = "error")
        return(NULL)
      })
    })
    
    output$downloadData <- downloadHandler(
      filename = function() sprintf("predictions_%s.zip", Sys.Date()),
      
      content = function(file) {
        
        ## ---------------------------------------------
        ## 1.  Write the predictions CSV
        ## ---------------------------------------------
        tmpdir <- tempdir()
        pred_file <- file.path(tmpdir, "predictions.csv")
        write.csv(shared$preds, pred_file, row.names = FALSE)
        
        ## ---------------------------------------------
        ## 2.  Assemble metadata
        ## ---------------------------------------------
        m <- metaRV$metrics %||% data.frame()           # safe NULL‑to‑empty
        metrics_str <- if (nrow(m)) {
          paste(
            "\nInternal calibration metrics:",
            paste(apply(m, 1, function(r)
              sprintf("\n      • %s : %s", r["Metric"], r["Value"])), collapse = "")
          )
        } else ""
        
        knn_str <- sprintf(
          "\nKNN selection : %s = %s (neighbors used: %d)",
          metaRV$knn_type, metaRV$knn_value, metaRV$n_neighbors
        )
        
        meta <- paste(
          "Project : MIR KNN predictions",
          "\nDate    :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          "\nSoil property predicted :", input$soilProperty,
          "\nModel type              :", input$modelType,
          knn_str,
          metrics_str,
          "\nSoftware : R", getRversion(), "(caret, randomForest, etc.)"
        )
        
        meta_file <- file.path(tmpdir, "metadata.txt")
        writeLines(meta, meta_file)
        
        ## ---------------------------------------------
        ## 3.  Zip and stream
        ## ---------------------------------------------
        old <- setwd(tmpdir); on.exit(setwd(old), add = TRUE)
        zip(zipfile = file, files = c("predictions.csv", "metadata.txt"))
      },
      
      contentType = "application/zip"
    )
    
    
    ##############################################################
    # Training Functions with Error Handling
    ##############################################################
    
    train_pls <- function(trainData, res_var = "calc_value") {
      tryCatch({
        X <- trainData[, !(names(trainData) %in% res_var)]
        Y <- trainData[[res_var]]
        fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
        pls_model <- train(
          x = X,
          y = Y,
          na.action = na.omit,
          trControl = fitControl,
          method = "pls",
          tuneLength = 30,
          metric = "RMSE"
        )
        list(model = pls_model, pca = NULL, num_components = NA)
      }, error = function(e) {
        showNotification(paste("Error training PLS model:", e$message, " try increasing distance or number of neighbors."), type = "error")
        return(NULL)
      })
    }
    
    train_rf <- function(trainData, res_var = "calc_value") {
      tryCatch({
        X <- trainData[, !(names(trainData) %in% res_var)]
        Y <- trainData[[res_var]]
        pca <- prcomp(X, center = TRUE, scale. = TRUE)
        cum_var <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
        num_components <- which(cum_var >= 0.99)[1]
        X_pca <- as.data.frame(pca$x[, 1:num_components, drop = FALSE])
        fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
        model <- train(x = X_pca, y = Y,
                       method = "rf",
                       trControl = fitControl,
                       metric = "RMSE")
        list(model = model, pca = pca, num_components = num_components)
      }, error = function(e) {
        showNotification(paste("Error training Random Forest model:", e$message, " try increasing distance or number of neighbors."), type = "error")
        return(NULL)
      })
    }
    
    train_svm <- function(trainData, res_var = "calc_value") {
      tryCatch({
        X <- trainData[, !(names(trainData) %in% res_var)]
        Y <- trainData[[res_var]]
        pca <- prcomp(X, center = TRUE, scale. = TRUE)
        cum_var <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
        num_components <- which(cum_var >= 0.99)[1]
        X_pca <- as.data.frame(pca$x[, 1:num_components, drop = FALSE])
        fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
        model <- train(x = X_pca, y = Y,
                       method = "svmRadial",
                       trControl = fitControl,
                       metric = "RMSE",
                       tuneLength = 10)
        list(model = model, pca = pca, num_components = num_components)
      }, error = function(e) {
        showNotification(paste("Error training SVM model:", e$message, " try increasing distance or number of neighbors."), type = "error")
        return(NULL)
      })
    }
    
    train_cb <- function(trainData, res_var = "calc_value") {
      tryCatch({
        X <- trainData[, !(names(trainData) %in% res_var)]
        Y <- trainData[[res_var]]
        pca <- prcomp(X, center = TRUE, scale. = TRUE)
        cum_var <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
        num_components <- which(cum_var >= 0.99)[1]
        X_pca <- as.data.frame(pca$x[, 1:num_components, drop = FALSE])
        fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
        model <- train(x = X_pca, y = Y,
                       method = "cubist",
                       trControl = fitControl,
                       metric = "RMSE")
        list(model = model, pca = pca, num_components = num_components)
      }, error = function(e) {
        showNotification(paste("Error training Cubist model:", e$message, " try increasing distance or number of neighbors."), type = "error")
        return(NULL)
      })
    }
    
    train_cnn <- function(trainData, res_var = "calc_value") {
      tryCatch({
        data <- trainData[, !(names(trainData) %in% res_var)]
        Y <- trainData[[res_var]]
        n_features <- ncol(data)
        model <- keras_model_sequential() %>% 
          layer_conv_1d(filters = 32, kernel_size = 3, activation = 'relu', 
                        input_shape = c(n_features, 1)) %>%
          layer_batch_normalization() %>%
          layer_max_pooling_1d(pool_size = 2) %>%
          layer_conv_1d(filters = 64, kernel_size = 5, activation = 'relu') %>%
          layer_batch_normalization() %>%
          layer_max_pooling_1d(pool_size = 2) %>%
          layer_flatten() %>%
          layer_dense(units = 128, activation = 'relu') %>%
          layer_dropout(rate = 0.2) %>%
          layer_dense(units = 1, activation = "linear")
        
        model %>% compile(
          loss = "mse",
          optimizer = "adam",
          metrics = c("mean_absolute_error")
        )
        
        x <- as.matrix(data)
        y <- Y
        x_array <- array_reshape(x, c(nrow(x), n_features, 1))
        
        history <- model %>% fit(
          x_array, y,
          epochs = 50,
          batch_size = 32,
          callbacks = list(
            callback_early_stopping(monitor = "loss", patience = 15, min_delta = 0.001, restore_best_weights = TRUE)
          ),
          verbose = 0
        )
        list(model = model, pca = NULL, num_components = NA)
      }, error = function(e) {
        showNotification(paste("Error training CNN model:", e$message, " try increasing distance or number of neighbors."), type = "error")
        return(NULL)
      })
    }
    
    
    # 1) Reactive: load & preprocess calibration + user data, run PCA, project user
    pcaData <- reactive({
      req(input$file1, input$soilProperty)
      
      # — load calibration spectra via your memoised function
      cal_data <- load_spectral_data_memo(input$soilProperty)
      validate(need(!is.null(cal_data) && nrow(cal_data) > 0,
                    "No calibration data available"))
      
      # — split soil vs spectra
      soil    <- cal_data[,1:27]
      MIR_new <- as.matrix(cal_data[,28:1792])
      colnames(MIR_new) <- seq(4000, 600, by = -1.927)
      
      # — SG smoothing, resample & SNV
      MIR.sg  <- savitzkyGolay(MIR_new, m = 0, w = 13, p = 2)
      wav     <- as.numeric(colnames(MIR.sg))
      new.wav <- seq(4000, 600, by = -10)
      MIR.res <- resample(MIR.sg, wav, new.wav)
      MIR.snv <- standardNormalVariate(MIR.res)
      
      # — PCA on calibration
      pca_res      <- prcomp(MIR.snv, center = TRUE, scale. = TRUE)
      calib_scores <- as.data.frame(pca_res$x)[,1:2, drop = FALSE]
      
      # — read & coerce user-upload
      user_df   <- read.csv(input$file1$datapath, check.names = FALSE)
      user_mat  <- as.matrix(user_df[, intersect(colnames(user_df), colnames(MIR.snv))])
      user_mat  <- matrix(as.numeric(user_mat),
                          nrow = nrow(user_mat),
                          dimnames = list(NULL, colnames(user_mat)))
      user_proj <- predict(pca_res, newdata = user_mat)[,1:2, drop = FALSE]
      user_scores <- as.data.frame(user_proj)
      
      list(pca_res      = pca_res,
           calib_scores = calib_scores,
           user_scores  = user_scores)
    })
    
    # 2) Reactive: pick neighbor indices based on type + slider
    neighborIdx <- reactive({
      pd <- pcaData()
      validate(need(nrow(pd$calib_scores) > 0, "No calibration scores"))
      
      if (input$knnType == "Neighbors") {
        k  <- input$knnSlider
        nn <- get.knnx(data  = pd$calib_scores,
                       query = pd$user_scores,
                       k     = k)
        unique(as.vector(nn$nn.index))
        
      } else {
        thresh <- input$knnSlider
        nn     <- get.knnx(data  = pd$calib_scores,
                           query = pd$user_scores,
                           k     = nrow(pd$calib_scores))
        # collect all calib rows within threshold distance
        hits <- which(nn$nn.dist <= thresh, arr.ind = TRUE)[,2]
        validate(need(length(hits) > 0,
                      "No neighbors within that distance"))
        unique(hits)
      }
    })
    
    # 3) Render the live PCA + neighbor overlay
    output$knn_pcaPlot <- renderPlot({
      pd  <- pcaData()
      idx <- neighborIdx()
      
      dfCal  <- pd$calib_scores %>% mutate(type = "Calibration")
      dfNbr  <- dfCal[idx, ]    %>% mutate(type = "Selected Neighbors")
      dfUser <- pd$user_scores  %>% mutate(type = "User Data")
      
      ggplot() +
        geom_point(data = dfCal,  aes(PC1, PC2, color = type), alpha = .6) +
        geom_point(data = dfNbr,  aes(PC1, PC2, color = type), size = 3) +
        geom_point(data = dfUser, aes(PC1, PC2, color = type), size = 3) +
        scale_color_manual(name = "Legend",
                           values = c(
                             "Calibration"        = "black",
                             "Selected Neighbors" = "gold",
                             "User Data"          = "red"
                           )) +
        guides(color = guide_legend(override.aes = list(size = 5))) +  # ← bigger keys
        theme(
          legend.title = element_text(size = 16),  # ← larger title
          legend.text  = element_text(size = 14)   # ← larger labels
        ) +
        labs(title = "PCA: Calibration / Neighbors / User",
             x = "PC1", y = "PC2")
      
    })

    ##############################################################
    # Main event to train the model and then predict on user data.
    ##############################################################
    observeEvent(input$predict, {
      req(input$soilProperty, user_data())
      withProgress(message = "Making Predictions...", value = 0, {
        tryCatch({
          
          res_var <- "calc_value"
          
          incProgress(0.1, detail = "Loading calibration data...")
          # Load calibration data
          cal_data <- tryCatch({
            load_spectral_data_memo(input$soilProperty)
          }, error = function(e) {
            showNotification(paste("Error loading calibration data:", e$message, " try increasing distance or number of neighbors."), type = "error")
            return(NULL)
          })
          validate(
            need(!is.null(cal_data) && nrow(cal_data) > 0,
                 "No calibration data found or it is empty.")
          )
          
          incProgress(0.1, detail = "Processing calibration data...")
          # Separate soil info and spectra.
          
          soil <- cal_data[, 1:27]
          MIR_new <- cal_data[, 28:1792]
          colnames(MIR_new) <- seq(4000, 600, by = -1.927)
          MIR_new <- as.matrix(MIR_new)
          MIR_new <- matrix(as.numeric(MIR_new), nrow = nrow(MIR_new),
                            dimnames = list(NULL, colnames(MIR_new)))
          MIR.sg  <- savitzkyGolay(MIR_new, m = 0, w = 13, p = 2)
          wav     <- as.numeric(colnames(MIR.sg))
          new.wav <- seq(4000, 600, by = -10)
          MIR.res <- resample(MIR.sg, wav, new.wav)
          MIR.snv <- standardNormalVariate(MIR.res)
          
          calib_df <- as.data.frame(MIR.snv)
          if ("calc_value" %in% colnames(soil)) {
            calib_df$calc_value <- soil[["calc_value"]]
          }
          
          incProgress(0.2, detail = "Performing PCA on calibration data...")
          pca_res <- tryCatch({
            prcomp(calib_df[, 1:ncol(MIR.snv)], center = TRUE, scale. = TRUE)
          }, error = function(e) {
            showNotification(paste("Error during PCA on calibration data:", e$message), type = "error")
            return(NULL)
          })
          
          incProgress(0.1, detail = "Projecting user data into PCA space...")
          # Process user data.
          user_matrix <- tryCatch({
            as.matrix(user_data())
          }, error = function(e) {
            showNotification(paste("Error processing user data:", e$message), type = "error")
            return(NULL)
          })
          user_matrix <- matrix(as.numeric(user_matrix),
                                nrow = nrow(user_matrix),
                                dimnames = list(NULL, colnames(user_data())))
          user_pca_scores <- tryCatch({
            predict(pca_res, newdata = user_matrix)
          }, error = function(e) {
            showNotification(paste("Error projecting user data into PCA space:", e$message), type = "error")
            return(NULL)
          })
          calib_pca_scores <- pca_res$x
          
          incProgress(0.1, detail = "Selecting neighbors...")
          # Use KNN to select calibration rows.
          if (input$knnType == "Neighbors") {
            k <- input$knnSlider
            neighbors <- tryCatch({
              get.knnx(data = calib_pca_scores, query = user_pca_scores, k = k)
            }, error = function(e) {
              showNotification(paste("Error finding neighbors:", e$message), type = "error")
              return(NULL)
            })
            # Use all neighbor indices (duplicates allowed if same calibration point is selected for multiple user points)
            all_neighbor_indices <- as.vector(neighbors$nn.index)
            print(paste("Number of neighbors per user point:", k))
          } else {
            neighbors <- tryCatch({
              get.knnx(data = calib_pca_scores, query = user_pca_scores, k = nrow(calib_pca_scores))
            }, error = function(e) {
              showNotification(paste("Error computing distances:", e$message), type = "error")
              return(NULL)
            })
            threshold <- input$knnSlider
            all_neighbor_indices <- tryCatch({
              valid_indices <- c()
              for (user_i in seq_len(nrow(user_pca_scores))) {
                row_dists <- neighbors$nn.dist[user_i, ]
                row_idxs  <- neighbors$nn.index[user_i, ]
                good_cols <- which(row_dists <= threshold)
                if (length(good_cols) > 0) {
                  valid_indices <- c(valid_indices, row_idxs[good_cols])
                }
              }
              neighbor_indices <- unique(valid_indices)
              if (length(neighbor_indices) == 0) {
                stop("No neighbors selected with the given distance threshold.")
              }
              neighbor_indices
            }, error = function(e) {
              showNotification(e$message, type = "error")
              return(NULL)
            })
            print(paste("Number of selected neighbors:", length(all_neighbor_indices)))
          }
          
          incProgress(0.1, detail = "Building training data...")
          # Subset calibration data.
          if ("calc_value" %in% colnames(calib_df)) {
            sub_soil <- soil[all_neighbor_indices, ]
            sub_spec <- MIR.snv[all_neighbor_indices, ]
            complete_rows <- complete.cases(sub_soil[["calc_value"]])
            sub_soil <- sub_soil[complete_rows, ]
            sub_spec <- sub_spec[complete_rows, ]
          } else {
            showNotification("Response variable not found in calibration data", type = "error")
            return()
          }
          
          trainData <- tryCatch({
            cbind(as.data.frame(sub_spec), calc_value = sub_soil[["calc_value"]])
          }, error = function(e) {
            showNotification(paste("Error building training data:", e$message), type = "error")
            return(NULL)
          })
          
          incProgress(0.1, detail = "Training model and making predictions...")
          # Train the model based on the selected model type.
          modelResult <- switch(input$modelType,
                                "rf"  = train_rf(trainData, res_var),
                                "svm" = train_svm(trainData, res_var),
                                "cb"  = train_cb(trainData, res_var),
                                "pls" = train_pls(trainData, res_var),
                                "cnn" = train_cnn(trainData, res_var))
          if (is.null(modelResult)) return()
          showNotification("Model training complete!", type = "message")
          
          
          #compute predictions on training data
          obs_cal <- trainData$calc_value
          if (!is.null(modelResult$pca)) {
            pca_scores <- as.data.frame(modelResult$pca$x)[, 1:modelResult$num_components, drop=FALSE]
            pred_cal   <- predict(modelResult$model, newdata = pca_scores)
          } else if (input$modelType == "cnn") {
            pred_cols  <- setdiff(names(trainData), "calc_value")
            xmat       <- as.matrix(trainData[, pred_cols, drop=FALSE])
            arr_train  <- array_reshape(xmat, c(nrow(xmat), ncol(xmat), 1))
            pred_cal   <- as.numeric(modelResult$model %>% predict(arr_train))
          } else {
            pred_cols  <- setdiff(names(trainData), "calc_value")
            pred_cal   <- predict(modelResult$model, newdata = trainData[, pred_cols, drop=FALSE])
          }
          pred_cal <- as.numeric(pred_cal)
          
          # --- calculate metrics ---
          r_val   <- cor(obs_cal, pred_cal, use = "complete.obs")
          r2      <- r_val^2
          me <- mean(pred_cal - obs_cal, na.rm = TRUE)      
          rmse    <- sqrt(mean((pred_cal - obs_cal)^2, na.rm = TRUE))
          sd_obs  <- sd(obs_cal, na.rm = TRUE)
          rpd     <- sd_obs / rmse
          rpiq    <- IQR(obs_cal, na.rm = TRUE) / rmse
          ccc     <- (2 * r_val * sd_obs * sd(pred_cal, na.rm = TRUE)) /
            (sd_obs^2 + var(pred_cal, na.rm = TRUE) + (mean(obs_cal) - mean(pred_cal))^2)
          
          metrics_df <- data.frame(
            Metric = c("R2", "RMSE", "ME", "RPD", "RPIQ", "CCC"),
            Value  = round(c(r2, rmse, me, rpd, rpiq, ccc), 4),
            stringsAsFactors = FALSE
          )
          
          calibMetrics(metrics_df)
          
          metaRV$metrics     <- metrics_df
          metaRV$n_neighbors <- length(all_neighbor_indices)
          metaRV$knn_type    <- input$knnType
          metaRV$knn_value   <- input$knnSlider
          
          # Predict on the user data.
          if (!is.null(modelResult$pca)) {
            user_pca <- tryCatch({
              predict(modelResult$pca, newdata = user_matrix)[, 1:modelResult$num_components, drop = FALSE]
            }, error = function(e) {
              showNotification(paste("Error projecting user data for prediction:", e$message), type = "error")
              return(NULL)
            })
            preds <- tryCatch({
              round(predict(modelResult$model, newdata = as.data.frame(user_pca)), 2)
            }, error = function(e) {
              showNotification(paste("Error predicting with PCA-based model:", e$message), type = "error")
              return(NULL)
            })
          } else if (input$modelType == "pls") {
            preds <- tryCatch({
              round(predict(modelResult$model, newdata = as.data.frame(user_matrix)), 2)
            }, error = function(e) {
              showNotification(paste("Error predicting with PLS model:", e$message), type = "error")
              return(NULL)
            })
          } else if (input$modelType == "cnn") {
            predictor_cols <- setdiff(names(user_data()), "scan_path_name")
            user_matrix <- tryCatch({
              as.matrix(user_data()[, predictor_cols, drop = FALSE])
            }, error = function(e) {
              showNotification(paste("Error subsetting user predictors:", e$message), type = "error")
              return(NULL)
            })
            n_features <- ncol(user_matrix)
            x_array <- tryCatch({
              array_reshape(user_matrix, c(nrow(user_matrix), n_features, 1))
            }, error = function(e) {
              showNotification(paste("Error reshaping user data for CNN:", e$message), type = "error")
              return(NULL)
            })
            preds <- tryCatch({
              round(predict(modelResult$model, x_array), 2)
            }, error = function(e) {
              showNotification(paste("Error predicting with CNN model:", e$message), type = "error")
              return(NULL)
            })
          }
          if (is.null(preds)) return()
          
          incProgress(0.1, detail = "Finishing up...")
          pred_df <- tryCatch({
            data.frame("Sample Name" = user_data()[["scan_path_name"]], Prediction = preds)
          }, error = function(e) {
            showNotification(paste("Error building predictions dataframe:", e$message), type = "error")
            return(NULL)
          })
          
          shared$preds <- pred_df
          
          output$pred_table <- DT::renderDataTable({
            DT::datatable(pred_df)
          })

        }, error = function(e) {
          showNotification(paste("Error during model training and prediction:", e$message), type = "error")
        })
        trainPlotData( data.frame(obs = obs_cal, pred = pred_cal) )
      })
    })
    
    output$calib_scatter <- renderPlot({
      df <- trainPlotData()
      req(df)
      
      ggplot(df, aes(obs, pred)) +
        geom_point(color = "red", alpha = 0.65, size = 2) +   # ← red points
        geom_abline(slope = 1, intercept = 0,
                    linetype = "dashed", linewidth = 1) +
        coord_equal() +
        labs(x = "Observed", y = "Predicted") +
        theme_minimal(base_size = 14)
    })
    
  })
}
