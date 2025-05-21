library(shiny)
library(shinyjs)
library(bslib)

modelTrainUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    
    # ---- HEAD: slider CSS + tab‑fix JS ----
    tags$head(
      # slider colours
      tags$style(HTML(sprintf("
        /* slider track & filled handle */
        #%1$s .irs-bar,
        #%1$s .irs-bar-edge {
          background: #0713f5 !important;
          border-color: #080808 !important;
        }
        /* bubble background & border */
        #%1$s .irs-single {
          background: #0713f5 !important;
          border-color: #080808 !important;
        }
        /* from/to labels */
        #%1$s .irs-from,
        #%1$s .irs-to {
          color: #ffffff !important;
        }
      ", ns("variance")))),
      
      # ARIA‑correct the tabs
      tags$script(HTML("
        function fixTabs(){
          $('ul.nav.nav-tabs').children('li')
            .attr('role','presentation')
            .removeAttr('tabindex');
          $('ul.nav.nav-tabs').find('a')
            .attr('role','tab')
            .each(function(){
              var isActive = $(this).parent().hasClass('active');
              $(this)
                .attr('tabindex',     isActive ? '0' : '-1')
                .attr('aria-selected', isActive ? 'true' : 'false');
            });
        }
        $(document).ready(fixTabs);
        $(document).on('shown.bs.tab', fixTabs);
      "))
    ),
    
    # ---- Top Card: training controls ----
    layout_column_wrap(
      width = 1/2,
      card(
        title = "Model Training UI",
        card_header("Train a Model", icon = icon("cogs")),
        
          layout_column_wrap(
          verticalLayout(
          fileInput(ns("data"), "Upload Data", accept = ".csv"),
          textInput(ns("res_var"), "Response Variable Column Name", value = "calc_value"),
          
          br(),
          
          # model type + PCA switch
          
          selectizeInput(
            ns("trainType"), "Select Model Type",
            choices = c(
              "Random Forest",
              "Partial Least Squares",
              "Support Vector Machine",
              "Cubist",
              "CNN"
            ),
            selected = "Random Forest"
          )),
          verticalLayout(
          sliderInput(ns("trainPercent"), "Training Data Percentage",
                      min = 50, max = 90, value = 70, step = 1,
                      post = "%", width = "100%"
          ),
          radioButtons(ns("cvtype"), "Calibration model tuning - Cross Validation Type",
                       inline = TRUE,
                       choices = c("K-fold" = "cv", "Leave-One-Out" = "loo"),
                       selected = "cv"
          ),
          uiOutput(ns("cvk")),
          checkboxInput(ns("trainPCA"), "Train with PCA", value = FALSE),
          conditionalPanel(
            condition = "input.trainPCA == true", ns = ns,
            sliderInput(
              ns("variance"), "Variance Explained Threshold",
              min = 0.80, max = 0.99, value = 0.99, step = 0.01
            )
          )
        )
      ) 
      ),
      # ---- Results Tabs ----
      navset_card_tab(
        id   = ns("page_nav"),
        
        nav_panel("1-1 Plot",    plotOutput(ns("trainedModelPlot"), height = "400px")),
        nav_panel("PCA Plot",      plotOutput(ns("trainedModelPCA"),    height = "400px")),
        nav_panel("Error Metrics", uiOutput(ns("model_error_ui")))
      )
    ),
    
    # ---- Buttons ----
    layout_column_wrap(
      width = 1/2,
      downloadButton(ns("downloadModel"), "Download Model"),
      actionButton(ns("trainModel"),    "Train Model")
    )

  )
}



modelTrainingServer <- function(id, shared, load_spectral_data_memo) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$kfold, {
        req(input$kfold)
        if (input$kfold < 2) {
          updateNumericInput(session, "kfold", value = 2)
        } else if (input$kfold > 20) {
          updateNumericInput(session, "kfold", value = 20)
        }
      })
      
      observeEvent(input$kfold, {
        if (is.na(input$kfold) || input$kfold == "") {
          shinyjs::disable("trainModel")
          showNotification("Please enter a valid number of folds", type = "error")
        } else if (!is.na(input$kfold) || input$kfold > 1) {
          if (!is.na(input$shuffle)) {
          shinyjs::enable("trainModel")
          }
        }
      })
      
      
      observeEvent(input$shuffle, {
        req(input$shuffle)
        if (input$shuffle < 1) {
          updateNumericInput(session, "shuffle", value = 1)
        } else if (input$shuffle > 20) {
          updateNumericInput(session, "shuffle", value = 20)
        }
      })
      
      observeEvent(input$shuffle, {
        if (is.na(input$shuffle) || input$shuffle == "") {
          shinyjs::disable("trainModel")
          showNotification("Please enter a valid number of repetitions", type = "error")
        } else if (!is.na(input$shuffle) || input$shuffle > 0) {
          if (!is.na(input$kfold)) {
           shinyjs::enable("trainModel")
          }
        }
      })
      
      metaRV <- reactiveValues(
        model_type      = NA,        # “Random Forest” …
        used_pca        = FALSE,
        n_components    = NA,        # if PCA
        var_threshold   = NA,        # if PCA
        split_percent   = NA,
        n_train         = NA,
        n_test          = NA,
        err_metrics     = NULL       # list returned by computeErrorMetrics(_CNN)
      )
      
      output$cvk <- renderUI({
        req(input$cvtype)
        if (input$cvtype == "cv") {
          layout_column_wrap(       
            numericInput(ns("kfold"), "Number of Folds",
                        min = 2, max = 20, value = 10),
            numericInput(ns("shuffle"), "Repetitions",
                        min = 1, max = 20, value = 10)
          )
        } else {
          NULL
        }
      })
      
      
      splitrat <- reactive({
        input$trainPercent / 100
      })
      
      observeEvent(input$trainModel, {
        shared$trainModel <- TRUE
      })
      
      observeEvent(input$trainType, {
        shared$trainType <- input$trainType
      })
   
      # COmputing error metrics (non CNN)   
      computeErrorMetrics <- function(model, train_data, test_data, res_var) {
        if (!is.null(model$bestTune)) {
          best_tune <- model$bestTune
          row_match <- rep(TRUE, nrow(model$results))
          for (param_name in names(best_tune)) {
            row_match <- row_match & (round(model$results[[param_name]], 6) == round(best_tune[[param_name]], 6))
          }
          
          RMSE.cal <- model$results[row_match, "RMSE"]
          R2.cal   <- model$results[row_match, "Rsquared"]
        } else {
          pred_cal <- predict(model, train_data)
          obs_cal  <- train_data[[res_var]]
          RMSE.cal <- sqrt(mean((pred_cal - obs_cal)^2))
          R2.cal   <- cor(pred_cal, obs_cal)^2
        }
        obs_cal <- train_data[[res_var]]
        RPD.cal  <- sd(obs_cal) / RMSE.cal
        RPIQ.cal <- (quantile(obs_cal, 0.75) - quantile(obs_cal, 0.25)) / RMSE.cal
        pred_val <- predict(model, test_data[, setdiff(names(test_data), res_var), drop = FALSE])
        
        
        # print(pred_val)
        obs_val  <- test_data[[res_var]]
        RMSE.val <- sqrt(mean((pred_val - obs_val)^2))
        R2.val   <- cor(pred_val, obs_val)^2
        RPD.val  <- sd(obs_val) / RMSE.val
        RPIQ.val <- (quantile(obs_val, 0.75) - quantile(obs_val, 0.25)) / RMSE.val
        ME.val  <- mean(pred_val - obs_val)
        MEC.val <- 1 - sum((obs_val - pred_val)^2) / sum((obs_val - mean(obs_val))^2)
        CCC.f <- function(predicted, observed) {
          cov_xy <- cov(predicted, observed)
          var_x  <- var(predicted)
          var_y  <- var(observed)
          mean_x <- mean(predicted)
          mean_y <- mean(observed)
          2 * cov_xy / (var_x + var_y + (mean_x - mean_y)^2)
        }
        CCC.val <- CCC.f(pred_val, obs_val)
        list(
          R2.cal    = R2.cal,
          RMSE.cal  = RMSE.cal,
          RPD.cal   = RPD.cal,
          RPIQ.cal  = RPIQ.cal,
          R2.val    = R2.val,
          RMSE.val  = RMSE.val,
          RPD.val   = RPD.val,
          RPIQ.val  = RPIQ.val,
          ME.val    = ME.val,
          MEC.val   = MEC.val,
          CCC.val   = CCC.val
        )
      }
      
      ###### 1. Disable PCA if PLS selected ######
      observeEvent(input$trainType, {
        if (input$trainType == "Partial Least Squares" || input$trainType == "CNN") {
          shinyjs::disable("trainPCA")
          shinyjs::disable("variance")
          shinyjs::disable("useKNN")
        } else {
          shinyjs::enable("trainPCA")
          shinyjs::enable("variance")
          shinyjs::enable("useKNN")
        }
      })
      
      observeEvent(input$trainPCA, {
        if (input$trainPCA) {
          shinyjs::enable("useKNN")
        } else {
          shinyjs::disable("useKNN")
        }
      })
      
      observeEvent(input$useModel, {
        shared$cust_pca_model <- trainedPCA()
        shared$cust_main_model <- trainedModel()
        nav_select(id = "page_navbar",
                   selected = "custom_model",
                   session = session)
      })
      
      ###### 2. Reactive variable for uploaded data ######
      data_reactive <- reactiveVal(NULL)
      
      observeEvent(input$data, {
        req(input$data)
        df <- read.csv(input$data$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
        data_reactive(df)
        if (input$res_var %in% names(df)) {
          shinyjs::enable("trainModel")
        } else {
          showNotification("Response variable not found in uploaded data", type = "error")
          shinyjs::disable("trainModel")
          return(NULL)
        }
      })
      
      observeEvent(input$trainType, {
        req(input$trainType)
        shared$trainType <- input$trainType
      })
      
      ###### 3. PCA Helper Function ######
      train_pca <- function(trainDF, testDF, varianceThreshold) {
        
        pca <- prcomp(trainDF, center = TRUE, scale. = TRUE)
        cum_var <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
        num_components <- which(cum_var >= varianceThreshold)[1]
        train_pca_data <- pca$x[, 1:num_components]
        test_pca_data  <- predict(pca, newdata = testDF)[, 1:num_components]
        list(
          pca = pca,
          num_components = num_components,
          trainPC = train_pca_data,
          testPC  = test_pca_data
        )
      }
      
      ###### 4. Training Functions (No PCA) ######
      train_rf <- function(data, ntree, trainPercent, res_var) {
        data <- data[, -1]
        
        # Check for NA Values
        data <- data[!is.na(data[[res_var]]), ]
        
        split <- sample.split(data[[res_var]], SplitRatio = (trainPercent))
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        
        fitControl = if (input$cvtype == "cv") {
          trainControl(
          method = "repeatedcv", 
          number = input$kfold,
          repeats = input$shuffle)
        } else {
            trainControl(method = "LOOCV")
          }
        
        rf_model <- train(x = train_data[, !(names(train_data) %in% res_var)],
                          y = train_data[[res_var]],
                          method = "rf",
                          trControl = fitControl,
                          metric = "RMSE")
        
        rf_pred <- predict(rf_model, newdata = test_data)
        obs_val <- test_data[[res_var]]
        
        rf_model$pred_obs <- data.frame(
          observed  = obs_val,
          predicted = rf_pred
        )
        
        if (is.factor(train_data[[res_var]])) {
          print(confusionMatrix(rf_pred, test_data[[res_var]]))
        } else {
          print(postResample(rf_pred, test_data[[res_var]]))
        }
        
        rf_model$errorMetrics <- computeErrorMetrics(rf_model, train_data, test_data, res_var)
        rf_model
      }
      
      train_pls <- function(data, ncomp, trainPercent, res_var) {
        data <- data[, -1]
        
        # Check for NA Values
        data <- data[!is.na(data[[res_var]]), ]
        
        split <- sample.split(data[[res_var]], SplitRatio = trainPercent)
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        
        fitControl = if (input$cvtype == "cv") {
          trainControl(
            method = "repeatedcv", 
            number = input$kfold,
            repeats = input$shuffle)
        } else {
          trainControl(method = "LOOCV")
        }
        
        pls_model <- train(
          x = train_data[, !(names(train_data) %in% res_var)],
          y = train_data[[res_var]],
          na.action = na.omit,
          trControl = fitControl,
          method = "pls",
          tuneLength = 30,
          metric = "RMSE")
        
        pls_pred <- predict(pls_model, newdata = test_data)
        obs_val <- test_data[[res_var]]
        
        pls_model$pred_obs <- data.frame(
          observed  = obs_val,
          predicted = pls_pred
        )
        
        if (is.factor(train_data[[res_var]])) {
          print(confusionMatrix(pls_pred, test_data[[res_var]]))
        } else {
          print(postResample(pls_pred, test_data[[res_var]]))
        }
        
        pls_model$errorMetrics <- computeErrorMetrics(pls_model, train_data, test_data, res_var)
        pls_model
      }
      
      reduce_lr <- callback_reduce_lr_on_plateau(
        monitor = "val_loss",
        factor = 0.5, 
        patience = 5,
        min_lr = 1e-6
      )
      
      # Computing CNN Error Metrics
      computeErrorMetrics_CNN <- function(pred_train, y_train, pred_test, y_test) {
        RMSE_train <- sqrt(mean((pred_train - y_train)^2))
        R2_train   <- cor(pred_train, y_train)^2
        RMSE_test  <- sqrt(mean((pred_test - y_test)^2))
        R2_test    <- cor(pred_test, y_test)^2
        RPD_train  <- sd(y_train) / RMSE_train
        RPD_test   <- sd(y_test) / RMSE_test
        RPIQ_train <- (quantile(y_train, 0.75) - quantile(y_train, 0.25)) / RMSE_train
        RPIQ_test  <- (quantile(y_test, 0.75) - quantile(y_test, 0.25)) / RMSE_test
        ME_test    <- mean(pred_test - y_test)
        MEC_test   <- 1 - sum((y_test - pred_test)^2) / sum((y_test - mean(y_test))^2)
        
        # You can also define a helper for CCC if needed
        CCC_f <- function(predicted, observed) {
          cov_xy <- cov(predicted, observed)
          var_x  <- var(predicted)
          var_y  <- var(observed)
          mean_x <- mean(predicted)
          mean_y <- mean(observed)
          2 * cov_xy / (var_x + var_y + (mean_x - mean_y)^2)
        }
        CCC_test <- CCC_f(pred_test, y_test)
        
        list(
          R2.cal    = R2_train,
          RMSE.cal  = RMSE_train,
          RPD.cal   = RPD_train,
          RPIQ.cal  = RPIQ_train,
          R2.val    = R2_test,
          RMSE.val  = RMSE_test,
          RPD.val   = RPD_test,
          RPIQ.val  = RPIQ_test,
          ME.val    = ME_test,
          MEC.val   = MEC_test,
          CCC.val   = CCC_test
        )
      }
      
      
      #CNN Model training, should probably be 1d but do more research
      
      build_cnn <- function(n_features) {
        
        model <- keras_model_sequential()
        model %>% layer_conv_1d(filters = 32, kernel_size = 3,
                                activation = "relu",
                                input_shape = c(n_features, 1))
        model %>% layer_batch_normalization()
        model %>% layer_max_pooling_1d(pool_size = 2)
        model %>% layer_conv_1d(filters = 64, kernel_size = 5,
                                activation = "relu")
        model %>% layer_batch_normalization()
        model %>% layer_max_pooling_1d(pool_size = 2)
        model %>% layer_flatten()
        model %>% layer_dense(units = 128, activation = "relu")
        model %>% layer_dropout(rate = 0.2)
        model %>% layer_dense(units = 1, activation = "linear")
        
        compile(model,
                loss      = "mse",
                optimizer = "adam",
                metrics   = "mean_absolute_error")
        
        model
      }
      
      
      
      train_cnn <- function(data, trainPercent, res_var,
                               cv_type = "cv", kfold, repeats) {
        
        ## ---- 0.  hold-out test split (unchanged) ----
        data <- data[, -1]
        data <- data[!is.na(data[[res_var]]), ]
        split <- sample.split(data[[res_var]], SplitRatio = trainPercent)
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        
        n_features <- ncol(train_data) - 1       # predictors only
        x_test <- as.matrix(test_data[, !(names(test_data) %in% res_var)])
        y_test <- test_data[[res_var]]
        x_test_arr <- array_reshape(x_test, c(nrow(x_test), n_features, 1))
        
        ## ---- 1.  generate resampling indices ----
        if (cv_type == "loo") {
          fold_indices <- caret::createFolds(train_data[[res_var]],
                                             k = nrow(train_data),
                                             returnTrain = TRUE)
        } else {
          fold_indices <- caret::createMultiFolds(train_data[[res_var]],
                                                  k = kfold, times = repeats)
        }
        
        rmse_vec <- numeric(length(fold_indices))
        r2_vec   <- numeric(length(fold_indices))
        
        ## ---- 2.  run the folds ----
        pb <- txtProgressBar(max = length(fold_indices), style = 3)
        for (i in seq_along(fold_indices)) {
          idx <- fold_indices[[i]]
          tr  <- train_data[idx,  ]
          va  <- train_data[-idx, ]
          
          ## 2a. build + fit a fresh model on *this* fold
          model <- build_cnn(n_features)
          
          x_tr <- as.matrix(tr[,  !(names(tr) %in% res_var)])
          y_tr <- tr[[res_var]]
          x_va <- as.matrix(va[,  !(names(va) %in% res_var)])
          y_va <- va[[res_var]]
          
          x_tr_arr <- array_reshape(x_tr, c(nrow(x_tr), n_features, 1))
          x_va_arr <- array_reshape(x_va, c(nrow(x_va), n_features, 1))
          
          history <- model %>% fit(
            x_tr_arr, y_tr,
            epochs = 100,
            batch_size = 32,
            callbacks = list(
              callback_early_stopping(patience = 15, min_delta = 0.001,
                                      restore_best_weights = TRUE)
            ),
            validation_data = list(x_va_arr, y_va),
            verbose = 0
          )
          
          ## 2b. predict the validation fold
          pred_va <- as.numeric(model %>% predict(x_va_arr))
          rmse_vec[i] <- sqrt(mean((pred_va - y_va)^2))
          r2_vec[i]   <- cor(pred_va, y_va)^2
          
          setTxtProgressBar(pb, i)
        }
        close(pb)
        
        cv_rmse <- mean(rmse_vec)
        cv_r2   <- mean(r2_vec)
        
        ## ---- 3.  refit on full training data ----
        final_model <- build_cnn(n_features)
        
        x_train_full <- as.matrix(train_data[, !(names(train_data) %in% res_var)])
        y_train_full <- train_data[[res_var]]
        x_train_full_arr <- array_reshape(x_train_full,
                                          c(nrow(x_train_full), n_features, 1))
        
        final_history <- final_model %>% fit(
          x_train_full_arr, y_train_full,
          validation_data = list(x_test_arr, y_test),
          epochs = 100,
          batch_size = 32,
          callbacks = list(
            callback_early_stopping(patience = 15, min_delta = 0.001,
                                    restore_best_weights = TRUE)
          ),
          verbose = 0
        )
        
        ## ---- 4.  usual hold-out metrics ----
        pred_tr <- as.numeric(final_model %>% predict(x_train_full_arr))
        pred_ts <- as.numeric(final_model %>% predict(x_test_arr))
        err <- computeErrorMetrics_CNN(pred_tr, y_train_full, pred_ts, y_test)
        
        ## ---- 5.  add CV summary to the metric list ----
        err$RMSE.cv <- cv_rmse
        err$R2.cv   <- cv_r2
        
        list(
          keras_model      = final_model,
          training_history = final_history,
          errorMetrics     = err,
          model_type       = "CNN"
        )
      }
      
      

      train_svm <- function(data, cost, gamma, trainPercent, res_var) {
        data <- data[, -1]
        
        data <- data[!is.na(data[[res_var]]), ]
        
        split <- sample.split(data[[res_var]], SplitRatio = trainPercent)
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        
        fitControl = if (input$cvtype == "cv") {
          trainControl(
            method = "repeatedcv", 
            number = input$kfold,
            repeats = input$shuffle)
        } else {
          trainControl(method = "LOOCV")
        }
        
        svm_model = train(x = train_data[, !(names(train_data) %in% res_var)],
                          y = train_data[[res_var]],
                          method = "svmRadial",
                          metric = "RMSE",
                          trControl = fitControl,
                          tuneLength = 10,
                          preProcess = c("center", "scale"))
        
        svm_pred <- predict(svm_model, newdata = test_data)
        obs_val <- test_data[[res_var]]
        
        svm_model$pred_obs <- data.frame(
          observed  = obs_val,
          predicted = svm_pred
        )
        if (is.factor(train_data[[res_var]])) {
          print(confusionMatrix(svm_pred, test_data[[res_var]]))
        } else {
          print(postResample(svm_pred, test_data[[res_var]]))
        }
        
        svm_model$errorMetrics <- computeErrorMetrics(svm_model, train_data, test_data, res_var)
        svm_model
      }
      
      train_cb <- function(data, committees, neighbors, trainPercent, res_var) {
        data <- data[, -1]
        
        data <- data[!is.na(data[[res_var]]), ]
        
        split <- sample.split(data[[res_var]], SplitRatio = trainPercent)
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        data[[res_var]] <- as.numeric(data[[res_var]])
        
        fitControl = if (input$cvtype == "cv") {
          trainControl(
            method = "repeatedcv", 
            number = input$kfold,
            repeats = input$shuffle)
        } else {
          trainControl(method = "LOOCV")
        }
        
        cb_model <- train(
          x = train_data[, !(names(train_data) %in% res_var)],
          y = train_data[[res_var]],
          method = "cubist",
          tuneGrid = data.frame(committees = c(10, 20, 50),
                                neighbors = c(0, 5, 9)),
          trControl = fitControl,
          metric = "RMSE")
        cb_pred <- predict(cb_model, newdata = test_data)
        obs_val <- test_data[[res_var]]
        cb_model$pred_obs <- data.frame(
          observed  = obs_val,
          predicted = cb_pred
        )
        cb_model$errorMetrics <- computeErrorMetrics(cb_model, train_data, test_data, res_var)
        cb_model
      }
      
      ###### 5. Training Functions (With PCA) ######
      train_rf_pca <- function(data, ntree, trainPercent, res_var, varianceThreshold) {
       
        data <- data[, -1]
   
        # Check for NA Values
        data <- data[!is.na(data[[res_var]]), ]
        
        split <- sample.split(data[[res_var]], SplitRatio = trainPercent)
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        
        X_train <- train_data[, !(names(train_data) %in% res_var)]
        Y_train <- train_data[[res_var]]
        X_test  <- test_data[, !(names(test_data) %in% res_var)]
        Y_test  <- test_data[[res_var]]
        
        pca_res <- train_pca(X_train, X_test, varianceThreshold)
        X_train_pca <- pca_res$trainPC
        X_test_pca  <- pca_res$testPC
        
        fitControl = if (input$cvtype == "cv") {
          trainControl(
            method = "repeatedcv", 
            number = input$kfold,
            repeats = input$shuffle)
        } else {
          trainControl(method = "LOOCV")
        }
        
        rf_model <- train(
          x = X_train_pca,
          y = Y_train,
          method = "rf",
          trControl = fitControl,
          metric = "RMSE")
        
        train_df_pca <- as.data.frame(X_train_pca)
        train_df_pca[[res_var]] <- Y_train
        test_df_pca <- as.data.frame(X_test_pca)
        test_df_pca[[res_var]] <- Y_test
        
        rf_pred <- predict(rf_model, newdata = test_df_pca)
        obs_val <- test_df_pca[[res_var]]
        rf_model$pred_obs <- data.frame(
          observed  = obs_val,
          predicted = rf_pred
        )
        
        rf_model$errorMetrics <- computeErrorMetrics(rf_model, train_df_pca, test_df_pca, res_var)
        list(
          model = rf_model,
          pca   = pca_res$pca,
          num_components = pca_res$num_components
        )
      }
      
      train_svm_pca <- function(data, cost, gamma, trainPercent, res_var, varianceThreshold) {
        data <- data[, -1]
        
        # Check for NA Values
        data <- data[!is.na(data[[res_var]]), ]
        
        split <- sample.split(data[[res_var]], SplitRatio = trainPercent)
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        
        X_train <- train_data[, !(names(train_data) %in% res_var)]
        Y_train <- train_data[[res_var]]
        X_test  <- test_data[, !(names(test_data) %in% res_var)]
        Y_test  <- test_data[[res_var]]
        
        pca_res <- train_pca(X_train, X_test, varianceThreshold)
        X_train_pca <- pca_res$trainPC
        X_test_pca  <- pca_res$testPC
        
        fitControl = if (input$cvtype == "cv") {
          trainControl(
            method = "repeatedcv", 
            number = input$kfold,
            repeats = input$shuffle)
        } else {
          trainControl(method = "LOOCV")
        }
        
        svm_model <- caret::train(
          x = X_train_pca,
          y = Y_train,
          method = "svmRadial",
          metric = "RMSE",
          trControl = fitControl,
          tuneLength = 10,
          preProcess = c("center", "scale"))
        
        train_df_pca <- as.data.frame(X_train_pca)
        train_df_pca[[res_var]] <- Y_train
        test_df_pca <- as.data.frame(X_test_pca)
        test_df_pca[[res_var]] <- Y_test
        
        svm_pred <- predict(svm_model, newdata = test_df_pca)
        obs_val <- test_df_pca[[res_var]]
        svm_model$pred_obs <- data.frame(
          observed  = obs_val,
          predicted = svm_pred
        )
        
        svm_model$errorMetrics <- computeErrorMetrics(svm_model, train_df_pca, test_df_pca, res_var)

        list(
          model = svm_model,
          pca   = pca_res$pca,
          num_components = pca_res$num_components
        )
      }
      
      train_cb_pca <- function(data, committees, neighbors, trainPercent, res_var, varianceThreshold) {
        data <- data[, -1]
        # Check for NA Values
        data <- data[!is.na(data[[res_var]]), ]
        
        split <- sample.split(data[[res_var]], SplitRatio = trainPercent)
        train_data <- subset(data, split == TRUE)
        test_data  <- subset(data, split == FALSE)
        
        X_train <- train_data[, !(names(train_data) %in% res_var)]
        Y_train <- train_data[[res_var]]
        X_test  <- test_data[, !(names(test_data) %in% res_var)]
        Y_test  <- test_data[[res_var]]
        
        pca_res <- train_pca(X_train, X_test, varianceThreshold)
        X_train_pca <- pca_res$trainPC
        X_test_pca  <- pca_res$testPC
        
        fitControl = if (input$cvtype == "cv") {
          trainControl(
            method = "repeatedcv", 
            number = input$kfold,
            repeats = input$shuffle)
        } else {
          trainControl(method = "LOOCV")
        }
        
        cb_model <- caret::train(
          x = X_train_pca,
          y = Y_train,
          method = "cubist",
          na.action = na.omit,
          metric = "RMSE",
          trControl = fitControl)
        
        train_df_pca <- as.data.frame(X_train_pca)
        train_df_pca[[res_var]] <- Y_train
        test_df_pca <- as.data.frame(X_test_pca)
        test_df_pca[[res_var]] <- Y_test
        
        cb_pred <- predict(cb_model, newdata = test_df_pca)
        obs_val <- test_df_pca[[res_var]]
        cb_model$pred_obs <- data.frame(
          observed  = obs_val,
          predicted = cb_pred
        )
        
        cb_model$errorMetrics <- computeErrorMetrics(cb_model, train_df_pca, test_df_pca, res_var)
        list(
          model = cb_model,
          pca   = pca_res$pca,
          num_components = pca_res$num_components
        )
      }
      
      ###### 6. Reactives: store trained model & PCA ######
      trainedModel <- reactiveVal(NULL)
      trainedPCA   <- reactiveVal(NULL)
      
      ###### 7. Plot Outputs ######
      observeEvent(shared$trainModel == 1, {
        doPCA <- if(shared$trainType == "Partial Least Squares") {
          FALSE 
        } else if (shared$trainType == "CNN") {
          FALSE
        } else {
          input$trainPCA
        }
        output$trainedModelPlot <- renderPlot({
          mod <- trainedModel()
          df  <- if (is.data.frame(mod$pred_obs)) {
            mod$pred_obs                     # no-PCA case
          } else if (is.list(mod$model) &&
                     is.data.frame(mod$model$pred_obs)) {
            mod$model$pred_obs               # PCA case
          } else {
            return(NULL)                     # nothing to plot
          }
          
          ggplot(df, aes(observed, predicted)) +
            geom_point(color = "red", alpha = 0.7, size = 2) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
            coord_equal() +
            labs(
              title = "Predicted vs Observed",
              x     = "Observed",
              y     = "Predicted"
            ) +
            theme_minimal(base_size = 14) +
            theme(
              panel.border = element_rect(        # ← the border itself
                colour = "black",                 #   any hex/colour name
                fill   = NA,                      #   keep panel transparent
                linewidth = 0.8                   #   adjust thickness as desired
              ),
              panel.background = element_blank(), # keep it clean
              panel.grid.major = element_line(colour = "#eaeaea"), # optional grid
              panel.grid.minor = element_blank()
            )
        })
        
        
      })
      
      output$trainedModelPCA <- renderPlot({
        req(trainedModel())
        mod <- trainedModel()
        if (!is.list(mod) || is.null(mod$pca)) {
          return(NULL)
        }
        
          req(trainedPCA())
          pca_obj <- trainedPCA()
          pca_df <- as.data.frame(pca_obj$x)
          if (ncol(pca_df) < 2) return(NULL)
          cum_var <- cumsum(pca_obj$sdev^2) / sum(pca_obj$sdev^2)
          xlab_str <- paste0("PC1 (", round(cum_var[1]*100, 2), "%)")
          ylab_str <- paste0("PC2 (", round(cum_var[2]*100, 2), "%)")
          ggplot(pca_df, aes(x = PC1, y = PC2)) +
            theme_dsh() + 
            scale_color_dsh() +
            geom_point(alpha = 1, color = "red", size = 4) +
            theme_minimal() +
            labs(x = xlab_str, y = ylab_str, title = "PCA Plot of Training Data")
        
      })
      
      ###### 8. Main training observe ######
      observe({
        if (shared$trainModel == 1) {
          shared$trainModel <- 0
          req(data_reactive())
          
          print(splitrat())
          
          withProgress(
            message = paste0("Training ", shared$trainType, " model"),
            value = 0,
            {
              incProgress(0.3, detail = "Preparing data...")
              Sys.sleep(0.5)
              
              model_obj <- NULL
              pca_obj   <- NULL
              
              incProgress(0.6, detail = "Running training...")
              
              doPCA <- if(shared$trainType == "Partial Least Squares") {
                FALSE 
              } else if (shared$trainType == "CNN") {
                FALSE
              } else {
                input$trainPCA
              }
              
              
              
              if (!doPCA) {
                if (shared$trainType == "Random Forest") {
                  model_obj <- train_rf(data_reactive(), shared$ntree, splitrat(), input$res_var)
                } else if (shared$trainType == "Partial Least Squares") {
                  model_obj <- train_pls(data_reactive(), shared$components, splitrat(), input$res_var)
                } else if (shared$trainType == "Support Vector Machine") {
                  model_obj <- train_svm(data_reactive(), shared$cost, shared$gamma, splitrat(), input$res_var)
                } else if (shared$trainType == "Cubist") {
                  model_obj <- train_cb(data_reactive(), shared$committees, shared$neighbors, splitrat(), input$res_var)
                } else if (shared$trainType == "CNN") {
                  model_obj <- train_cnn(data_reactive(), splitrat(), input$res_var, cv_type = input$cvtype, kfold = input$kfold, repeats = input$shuffle)
                }
              } else {
                varThresh <- input$variance
                  if (shared$trainType == "Random Forest") {
                    pcaRes <- train_rf_pca(data_reactive(), shared$ntree, splitrat(), input$res_var, varThresh)
                  } else if (shared$trainType == "Support Vector Machine") {
                    pcaRes <- train_svm_pca(data_reactive(), shared$cost, shared$gamma, splitrat(), input$res_var, varThresh)
                  } else if (shared$trainType == "Cubist") {
                    pcaRes <- train_cb_pca(data_reactive(), shared$committees, shared$neighbors, splitrat(), input$res_var, varThresh)
                  }
                
                model_obj <- pcaRes
                pca_obj   <- pcaRes$pca
              }

              incProgress(1, detail = "Finalizing...")
              trainedModel(model_obj)
              trainedPCA(pca_obj)
            }
          )
          
          ## --------------------------------------------------------------
          ## Save facts for the metadata file
          ## --------------------------------------------------------------
          metaRV$model_type    <- shared$trainType
          metaRV$used_pca      <- doPCA
          metaRV$n_components  <- if (doPCA) trainedModel()$num_components else NA
          metaRV$var_threshold <- if (doPCA) input$variance else NA
          metaRV$split_percent <- input$trainPercent
          metaRV$n_train       <- sum(sample.split(data_reactive()[[input$res_var]],
                                                   SplitRatio = splitrat()))
          metaRV$n_test        <- nrow(data_reactive()) - metaRV$n_train
          if (identical(shared$trainType, "CNN")) {
            metaRV$err_metrics <- trainedModel()$errorMetrics
          } else if (is.list(trainedModel()) && !is.null(trainedModel()$model)) {
            metaRV$err_metrics <- trainedModel()$model$errorMetrics
          } else {
            metaRV$err_metrics <- trainedModel()$errorMetrics
          }
          
          
          showNotification(
            ui = paste(shared$trainType, "model training complete!"),
            duration = 5,
            type = "message"
          )
        }
      })
      
      ###### 9. Download Handler ######
      output$downloadModel <- downloadHandler(
        filename = function() sprintf("%s_%s.zip",
                                      gsub(' ', '_', shared$trainType),
                                      Sys.Date()),
        
        content = function(file) {
          req(trainedModel())
          
          tmpdir <- tempdir()
          
          ## ---- 1.  save model(s) ------------------------------------------------
          if (shared$trainType == "CNN") {
            save_model_tf(trainedModel()$keras_model, file.path(tmpdir, "cnn_model.keras"))
            model_files <- "cnn_model.keras"
          } else if (!metaRV$used_pca || shared$trainType == "Partial Least Squares") {
            saveRDS(trainedModel(), file.path(tmpdir, "model.rds"))
            model_files <- "model.rds"
          } else {                              # main + PCA
            saveRDS(trainedModel(), file.path(tmpdir, "main_model.rds"))
            saveRDS(trainedPCA(),   file.path(tmpdir, "pca_model.rds"))
            model_files <- c("main_model.rds", "pca_model.rds")
          }
          
          ## ---- 2.  write metadata.txt ------------------------------------------
          em <- metaRV$err_metrics
          mtxt <- paste(
            "Project      : Custom MIR model training",
            "\nDate         :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            "\nModel type   :", metaRV$model_type,
            if (metaRV$used_pca)
              sprintf("\n  ├─ PCA components : %d (≥ %.2f variance)",
                      metaRV$n_components, metaRV$var_threshold),
            sprintf("\nData split   : %d%% train / %d%% test  (%d / %d samples)",
                    metaRV$split_percent,
                    100 - metaRV$split_percent,
                    metaRV$n_train, metaRV$n_test),
            "\n\nError metrics:",
            sprintf("\n  • R² (cal / val)        : %.3f / %.3f", em$R2.cal,   em$R2.val),
            sprintf("\n  • RMSE (cal / val)      : %.3f / %.3f", em$RMSE.cal, em$RMSE.val),
            sprintf("\n  • RPD  (cal / val)      : %.3f / %.3f", em$RPD.cal,  em$RPD.val),
            sprintf("\n  • RPIQ (cal / val)      : %.3f / %.3f", em$RPIQ.cal, em$RPIQ.val),
            sprintf("\n  • ME / MEC / CCC (val)  : %.3f / %.3f / %.3f",
                    em$ME.val, em$MEC.val, em$CCC.val),
            "\n\nSoftware     : R", getRversion(), "(caret, randomForest, keras, etc.)"
          )
          writeLines(mtxt, file.path(tmpdir, "metadata.txt"))
          
          ## ---- 3.  zip & stream -------------------------------------------------
          old <- setwd(tmpdir); on.exit(setwd(old), add = TRUE)
          zip(zipfile = file,
              files   = c(model_files, "metadata.txt"))
        },
        
        contentType = "application/zip"
      )
      
      
      output$model_error_ui <- renderUI({
        req(trainedModel())
        if (is.list(trainedModel()) &&
            "model" %in% names(trainedModel()) && 
            !inherits(trainedModel(), "train")) {
          em <- trainedModel()$model$errorMetrics
        } else {
          em <- trainedModel()$errorMetrics
        }
        
        validate(need(!is.null(em), "No error metrics found."))
        
        tags$div(
          class = "card",
          style = "margin: 15px; box-shadow: 2px 2px 6px #ccc;",
          tags$div(
            class = "card-header",
            tags$h4("Model Error Metrics", style = "margin: 0;")
          ),
          tags$div(
            class = "card-body",
            tags$table(
              class = "table table-striped table-bordered",
              tags$thead(
                tags$tr(
                  tags$th("Metric"),
                  tags$th("Calibration"),
                  tags$th("Validation")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("R2"),
                  tags$td(round(em$R2.cal, 2)),
                  tags$td(round(em$R2.val, 2))
                ),
                tags$tr(
                  tags$td("RMSE"),
                  tags$td(round(em$RMSE.cal, 2)),
                  tags$td(round(em$RMSE.val, 2))
                ),
                tags$tr(
                  tags$td("RPD"),
                  tags$td(round(em$RPD.cal, 2)),
                  tags$td(round(em$RPD.val, 2))
                ),
                tags$tr(
                  tags$td("RPIQ"),
                  tags$td(round(em$RPIQ.cal, 2)),
                  tags$td(round(em$RPIQ.val, 2))
                ),
                tags$tr(
                  tags$td("ME"),
                  tags$td("-"),
                  tags$td(round(em$ME.val, 2))
                ),
                tags$tr(
                  tags$td("MEC"),
                  tags$td("-"),
                  tags$td(round(em$MEC.val, 2))
                ),
                tags$tr(
                  tags$td("CCC"),
                  tags$td("-"),
                  tags$td(round(em$CCC.val, 2))
                )
              )
            )
          )
        )
      })
    }
  )
}
