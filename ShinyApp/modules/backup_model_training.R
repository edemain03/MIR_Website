#backup_model_training.R

# model_training.R

library(dplyr)
library(randomForest)
library(caret)
library(caTools)
library(shiny)

modelTrainUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    layout_column_wrap(
      card(
        title = "Model Training UI",
        layout_column_wrap(
          selectizeInput(
            ns("trainType"),
            "Select Model Type",
            choices = c(
              "Random Forest",
              "Partial Least Squares",
              "Support Vector Machine",
              "Cubist"
            ),
            selected = "Random Forest"
          ),
          
        ),
        fileInput(
          ns("data"),
          "Upload Data",
          accept = c(".csv")
        ),
        textInput(ns("res_var"), "Response Variable Column Name", value = "calc_value")
      ),
      card(
        title = "PCA Training",
        header = "PCA Training",
        helpText("PCA Training is a method used to reduce dimensionality of the data. We recommend training a PCA model for large datasets, especially if you are using RF, Cubist, or SVM Models"),
        checkboxInput(
          ns("trainPCA"),
          "Train with PCA",
          value = FALSE
        ),
        
        conditionalPanel(
          condition = "input.trainPCA == true",  # notice we compare to 'true', not 'TRUE'
          ns = ns,
          sliderInput(ns("variance"), "Variance Explained", min = 0.80, max = 0.99, value = 0.99, step = 0.01)
        )
        
      )
    ),
    layout_column_wrap(
      downloadButton(ns("downloadModel"), "Download Model"),
      actionButton(ns("useModel"), "Use Model")
    ),
    card(
      title = "Model Plot",
      plotOutput(ns("trainedModelPlot"), height = "400px")
    )
  )
}

modelTrainingServer <- function(id, shared) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$trainType, {
        if (input$trainType == "Partial Least Squares") {
          shinyjs::disable("trainPCA")
          shinyjs::disable("variance")
        } else {
          shinyjs::enable("trainPCA")
          shinyjs::enable("variance")
        }
      })
      
      # Reactive variable for uploaded data
      data_reactive <- reactiveVal(NULL)
      
      # Observe data upload
      observeEvent(input$data, {
        req(input$data)
        df <- read.csv(input$data$datapath, header = TRUE, stringsAsFactors = FALSE)
        data_reactive(df)
      })
      
      # Keep track of model type
      observeEvent(input$trainType, {
        req(input$trainType)
        shared$trainType <- input$trainType
      })
      
      # Train PCA
      train_pca <- function(vars) {
        pca <- prcomp(spec_calib, center = TRUE, scale = TRUE)
      }
      
      # TRAINING FUNCTIONS
      train_rf <- function(data, ntree, trainPercent, res_var) {
        # Drop FIRST column
        data <- data[, -1]
        
        split <- sample.split(data[[res_var]], SplitRatio = trainPercent)
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        
        # Tune mtry
        auto_mtry <- tuneRF(
          x = train_data[, !(names(train_data) %in% res_var)],
          y = train_data[[res_var]],
          stepFactor = 0.5,
          improve = 0.01,
          ntree = ntree,
          trace = TRUE,
          plot = FALSE,
          doBest = FALSE
        )
        best_mtry <- auto_mtry[which.min(auto_mtry[, "OOBError"]), "mtry"]
        
        # Train RF
        rf_model <- randomForest(
          x = train_data[, !(names(train_data) %in% res_var)],
          y = train_data[[res_var]],
          ntree = ntree,
          mtry = best_mtry
        )
        
        # Evaluate
        rf_pred <- predict(rf_model, newdata = test_data)
        if (is.factor(train_data[[res_var]])) {
          print(confusionMatrix(rf_pred, test_data[[res_var]]))
        } else {
          print(postResample(rf_pred, test_data[[res_var]]))
        }
        
        rf_model
      }
      
      train_pls <- function(data, ncomp, trainPercent, res_var) {
        data <- data[, -1]
        split <- sample.split(data[[res_var]], SplitRatio = trainPercent)
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        
        ctrl <- trainControl(method = "none")
        pls_model <- caret::train(
          as.formula(paste(res_var, "~ .")),
          data = train_data,
          method = "pls",
          tuneGrid = data.frame(ncomp = ncomp),
          trControl = ctrl
        )
        
        pls_pred <- predict(pls_model, newdata = test_data)
        if (is.factor(train_data[[res_var]])) {
          print(confusionMatrix(pls_pred, test_data[[res_var]]))
        } else {
          print(postResample(pls_pred, test_data[[res_var]]))
        }
        
        pls_model
      }
      
      train_svm <- function(data, cost, gamma, trainPercent, res_var) {
        data <- data[, -1]
        split <- sample.split(data[[res_var]], SplitRatio = trainPercent)
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        
        ctrl <- trainControl(method = "none")
        svm_model <- caret::train(
          as.formula(paste(res_var, "~ .")),
          data = train_data,
          method = "svmRadial",
          tuneGrid = data.frame(C = cost, sigma = gamma),
          trControl = ctrl
        )
        
        svm_pred <- predict(svm_model, newdata = test_data)
        if (is.factor(train_data[[res_var]])) {
          print(confusionMatrix(svm_pred, test_data[[res_var]]))
        } else {
          print(postResample(svm_pred, test_data[[res_var]]))
        }
        
        svm_model
      }
      
      train_cb <- function(data, committees, neighbors, trainPercent, res_var) {
        data <- data[, -1]
        split <- sample.split(data[[res_var]], SplitRatio = trainPercent)
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        
        ctrl <- trainControl(method = "none")
        cb_model <- caret::train(
          as.formula(paste(res_var, "~ .")),
          data = train_data,
          method = "cubist",
          tuneGrid = data.frame(committees = committees, neighbors = neighbors),
          trControl = ctrl
        )
        
        cb_pred <- predict(cb_model, newdata = test_data)
        if (is.factor(train_data[[res_var]])) {
          print(confusionMatrix(cb_pred, test_data[[res_var]]))
        } else {
          print(postResample(cb_pred, test_data[[res_var]]))
        }
        
        cb_model
      }
      
      # Reactive: store trained model
      trainedModel <- reactiveVal(NULL)
      
      # Plot output
      output$trainedModelPlot <- renderPlot({
        req(trainedModel())
        # If caret model, varImp; if randomForest, varImpPlot
        if (inherits(trainedModel(), "train")) {
          imp <- varImp(trainedModel())
          plot(imp, top = 10)
        } else if (inherits(trainedModel(), "randomForest")) {
          randomForest::varImpPlot(trainedModel())
        } else {
          plot(1, 1, main = "No varImp method for this model object")
        }
      })
      
      # Main training observe
      observe({
        if (shared$trainModel == 1) {
          shared$trainModel <- 0
          req(data_reactive())
          
          withProgress(
            message = paste0("Training ", shared$trainType, " model"),
            value = 0,
            {
              incProgress(0.3, detail = "Preparing data...")
              Sys.sleep(0.5)  # demonstration
              
              model_obj <- NULL
              incProgress(0.6, detail = "Running training...")
              
              # Nested loops here, if (trainpca) { pca function + train function } else { train function }
              
              if (shared$trainType == "Random Forest") {
                model_obj <- train_rf(data_reactive(), 100, 0.7, input$res_var)
              } else if (shared$trainType == "Partial Least Squares") {
                model_obj <- train_pls(data_reactive(), 5, 0.7, input$res_var)
              } else if (shared$trainType == "Support Vector Machine") {
                model_obj <- train_svm(data_reactive(), 1, 0.1, 0.7, input$res_var)
              } else if (shared$trainType == "Cubist") {
                model_obj <- train_cb(data_reactive(), 50, 5, 0.7, input$res_var)
              }
              
              incProgress(1, detail = "Finalizing...")
              trainedModel(model_obj)
            }
          )
          
          # Once training is done, show a pop-up notification
          showNotification(
            ui = paste(shared$trainType, "model training complete!"),
            duration = 5,
            type = "message"
          )
        }
      })
      
      # Download
      output$downloadModel <- downloadHandler(
        filename = function() {
          paste0(shared$trainType, "_model_", Sys.Date(), ".rds")
        },
        content = function(file) {
          req(trainedModel())
          saveRDS(trainedModel(), file)
        }
      )
    }
  )
}
