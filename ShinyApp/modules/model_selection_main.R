library(shiny)

# updated for new model naming conventions, significantly reduces working space

MLRA_list <- c(
  "_W1_" = "W1",
  "_X1_" = "X1",
  "_X2_" = "X2",
  "_A_" = "A",
  "_B_" = "B",
  "_C_" = "C",
  "_D_" = "D",
  "_E_" = "E",
  "_F_" = "F",
  "_G_" = "G",
  "_H_" = "H",
  "_I_" = "I",
  "_J_" = "J",
  "_K_" = "K",
  "_L_" = "L",
  "_M_" = "M",
  "_N_" = "N",
  "_O_" = "O",
  "_P_" = "P",
  "_Q_" = "Q",
  "_R_" = "R",
  "_S_" = "S",
  "_T_" = "T",
  "_U_" = "U",
  "_V_" = "V",
  "_W_" = "W",
  "_X_" = "X",
  "_Y_" = "Y",
  "_Z_" = "Z"
)

depth_list <- c(
  "15" = "15cm",
  "30" = "30cm",
  "60" = "60cm",
  "100" = "100cm",
  "200" = "200cm"
)

order_list <- c(
  "alfisols" = "Alfisols",
  "entisols" = "Entisols",
  "ultisols"  = "Ultisols",
  "mollisols" = "Mollisols",
  "inceptisols" = "Inceptisols",
  "spodosols" = "Spodosols",
  "andisols" = "Andisols",
  "histosols" = "Histosols",
  "vertisols" = "Vertisols",
  "gelisols" = "Gelisols",
  "aridisols" = "Aridisols"
)

texture_list <- c(
  "_Cl_" = "Clay",
  "_ClLo_" = "Clay Loam",
  "_Lo_" = "Loam",
  "_LoSa_" = "Loam Sandy",
  "_Sa_" = "Sandy",
  "_SaCl_" = "Sandy Clay",
  "_SaClLo_" = "Sandy Clay Loam",
  "_SaLo_" = "Sandy Loam",
  "_Si_" = "Silt",
  "_SiCl_" = "Silt Clay",
  "_SiClLo_" = "Silt Clay Loam",
  "_SiLo_" = "Silt Loam"
)
LULC_list <- c(
  "Cultivated Crops" = "Cultivated crops",
  "Decidous trees" = "Decidous trees",
  "Developed" = "Developed",
  "Evergreen forest_" = "Evergreen forest",
  "Herbaceous_wetland_" = "Herbaceous wetland",
  "Herbaceous_" = "Herbaceous",
  "Mixed forest" = "Mixed forest",
  "pasture" = "Pasture",
  "Shrub" = "Shrub",
  "Woody wetland" = "Woody wetland",
  "Croplands" = "Croplands",
  "Barren" = "Barren",
  "Deciduous forests" = "Deciduous forests",
  "Evergreen forests" = "Evergreen forests",
  "Grasslands" = "Grasslands",
  "Herbaceous wetlands" = "Herbaceous wetlands",
  "Open water" = "Open water"
)

# model_selection_main.R
modelSelectionUI <- function(id) {
  tags$style(HTML(".bslib-mb-spacing {
                  margin-bottom: 0 !important;}"))
  ns <- NS(id)
  fluidPage(
    card(
      min_height = "300px",
      max_height = "300px",
      card_header("Model Selection"),
      card_body(
        layout_column_wrap(
          width = 1/2,
          verticalLayout(
            selectizeInput(
              ns("mlModel"),
              "Select a Machine Learning Model",
              choices = c(
                "Select a model" = "",
                "Random Forest" = "RF",
                "Support Vector Machine" = "SVM",
                "Cubist" = "Cubist",
                "Convolutional Neural Network" = "CNN",
                "Partial Least Squares" = "PLS"
              ),
              selected = ""
            ),
            
            # Conditional panels for different model types
            conditionalPanel(
              condition = sprintf("input['%s'] != 'global'", ns("modelType")),
              uiOutput(ns("dynamic_select"))
            )
          ),
          radioButtons(
            ns("modelType"),
            "Stratification Method",
            choices = c("Global" = "global",
                        "Soil Order" = "order",
                        "Texture Class" = "texture",
                        "MLRA" = "mlra",
                        "LULC" = "lulc",
                        "Sample Depth" = "depth"),
            selected = character(0)
          )
        )
      )
    )
  )
}

modelSelectionServer <- function(id, shared, soilPropertyName) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Observe changes to mlModel and update shared$mlModel
    observeEvent(input$mlModel, {
      shared$mlModel <- input$mlModel
    }, ignoreNULL = FALSE)
    
    # Observe changes to modelType and update shared$modelType
    observeEvent(input$modelType, {
      shared$modelType <- input$modelType
    }, ignoreNULL = FALSE)
    
    
    # inside modelSelectionServer
    find_model <- reactive({
      req(shared$modelType, shared$mlModel)
      if (shared$modelType == "global") {
        switch(shared$mlModel,
               RF = "rf",
               SVM = "svm",
               PLS = "pls",
               CNN = "CNN",
               Cubist = "cubist")
      } else {
        switch(shared$mlModel,
               RF = "rf",
               SVM = "svmRadial",
               PLS = "pls",
               CNN = "CNN",
               Cubist = "cubist")
      }
    })
    
    # Reactive expressions for model paths based on selected mlModel
    model_paths <- reactive({
      req(shared$modelType, shared$mlModel)
      base_path <- switch(shared$modelType,
                          "global" = "models/Global",
                          "texture" = "models/Texture_classes",
                          "order" = "models/Orders",
                          "depth" = "models/Depths",
                          "lulc" = "models/LULC",
                          "mlra" = "models/MLRA")
      model_folder <- shared$mlModel
      file.path(base_path, model_folder)
    })
    
    # Reactive expressions for model choices
    model_choices <- reactive({
      req(shared$mlModel, shared$modelType)
      path <- model_paths()
      
      if (!is.null(path) && dir.exists(path)) {
        
        depth_order <- depth_list                          # "15cm" … "200cm" (already ascending)
        
        if (shared$mlModel != "CNN") {                     # ---------- non-CNN ----------
          files <- list.files(path, pattern = "\\.rds$", full.names = FALSE)
          files <- files[!grepl("_pca\\.rds$", files)]
          files <- files[grepl(id, files)]
          
          if (length(files) == 0) {
            showNotification("No models found", type = "warning")
            return(NULL)
          }
          
          pattern_list <- switch(shared$modelType,
                                 "global"  = NULL,
                                 "texture" = texture_list,
                                 "order"   = order_list,
                                 "depth"   = depth_list,
                                 "lulc"    = LULC_list,
                                 "mlra"    = MLRA_list)
          
          adjust_name <- function(file) {
            if (!is.null(pattern_list)) {
              for (pat in names(pattern_list)) {
                if (grepl(pat, file, ignore.case = TRUE))
                  return(pattern_list[[pat]])
              }
            }
            tools::file_path_sans_ext(file)     # fall-back
          }
          
          adjusted <- sapply(files, adjust_name)
          choices  <- setNames(files, adjusted)
          
          ## ---------- depth specific tidy-up ----------
          if (shared$modelType == "depth") {
            # 1. drop anything that was **not** mapped by depth_list  (e.g., 250 cm)
            choices <- choices[names(choices) %in% depth_order]
            
            # 2. reorder to 15 → 30 → 60 → 100 → 200
            choices <- choices[match(depth_order, names(choices))]
          }
          return(choices)
          
        } else {                                           # ----------  CNN ----------
          files <- list.files(path, pattern = "\\.keras$", full.names = FALSE)
          files <- files[grepl(id, files)]
          
          if (length(files) == 0) {
            showNotification("No models found", type = "warning")
            return(NULL)
          }
          
          pattern_list <- switch(shared$modelType,
                                 "global"  = NULL,
                                 "texture" = texture_list,
                                 "order"   = order_list,
                                 "depth"   = depth_list,
                                 "lulc"    = LULC_list,
                                 "mlra"    = MLRA_list)
          
          adjust_name <- function(file) {
            if (!is.null(pattern_list)) {
              for (pat in names(pattern_list)) {
                if (grepl(pat, file, ignore.case = TRUE))
                  return(pattern_list[[pat]])
              }
            }
            tools::file_path_sans_ext(file)
          }
          
          adjusted <- sapply(files, adjust_name)
          choices  <- setNames(files, adjusted)
          
          ## depth clean-up (same idea as above)
          if (shared$modelType == "depth") {
            choices <- choices[names(choices) %in% depth_order]
            choices <- choices[match(depth_order, names(choices))]
          }
          return(choices)
        }
      } else {
        showNotification("Model path does not exist", type = "warning")
        NULL
      }
    })
    
    
    
    
    # Output UI for dynamic model selection
    output$dynamic_select <- renderUI({
      req(shared$modelType != "global")
      inputID <- ns(paste0(shared$modelType, "_model"))
      label <- switch(shared$modelType,
                      "texture" = "Select Texture Class Model",
                      "order" = "Select Order Model",
                      "depth" = "Select Depth Model",
                      "lulc" = "Select LULC Model",
                      "mlra" = "Select MLRA Model",
                      "Select Model")
      placeholder <- label
      choices <- model_choices()
      selectizeInput(
        inputId = inputID,
        label = label,
        choices = c("Select your model" = "", choices),
        selected = NULL,
        multiple = FALSE,
        options = list(placeholder = placeholder, plugins = "clear_button")
      )
    })
    
    # Observe changes and load models accordingly
    observeEvent({
      input[[paste0(shared$modelType, "_model")]]
      input$modelType
      input$mlModel
    }, {
      req(shared$mlModel, shared$modelType)
      if (shared$modelType == "global") {
        # --- Handling global PLS and CNN ---
        
        if (shared$mlModel == "PLS") {
          model_file <- file.path(model_paths(), paste0(id, "_pls_model.rds"))
          if (file.exists(model_file)) {
            showNotification("Loaded PLS model file", type = "message")
            shared$usedModel <- readRDS(model_file)
          } else {
            showNotification("PLS model file not found", type = "warning")
            shared$usedModel <- NULL
          }
        } else if (shared$mlModel == "CNN") {
          # Loading CNN model using Keras
          model_file <- file.path(model_paths(), paste0(id, "_CNN_model.keras"))
          if (file.exists(model_file)) {
            showNotification("Loaded CNN model file", type = "message")
            shared$usedModel <- tryCatch({
              load_model_tf(model_file)
            }, error = function(e) {
              print(e$message)
              showNotification(paste("Error loading CNN model:", e$message), type = "error")
              NULL
            })
          } else {
            showNotification("CNN model file not found", type = "warning")
            shared$usedModel <- NULL
          }
        } else {
          # Load other models
          model_file <- file.path(model_paths(), paste0(id, "_", find_model(), "_model.rds"))
          print(model_file)
          if (file.exists(model_file)) {
            showNotification("Loaded model file", type = "message")
            shared$usedModel <- readRDS(model_file)
          } else {
            showNotification("Model file not found", type = "warning")
            shared$usedModel <- NULL
          }
        }
        
        # Disable PCA plot if the model is CNN or PLS
        if (shared$mlModel != "PLS" && shared$mlModel != "CNN") {
          pca_model_file <- file.path(model_paths(), paste0(id, "_pca", ".rds"))
          if (file.exists(pca_model_file)) {
            showNotification("PCA model found and loaded", type = "message")
            shared$usedPCA <- readRDS(pca_model_file)
          } else {
            shared$usedPCA <- NULL
            showNotification("PCA model file not found", type = "warning")
          }
        } else {
          shared$usedPCA <- NULL
        }
        
      } else {
        # --- Handling stratified models ---
        model_input_id <- paste0(shared$modelType, "_model")
        model_input_value <- input[[model_input_id]]
        print(model_input_value)
        req(model_input_value)
        print(model_input_value)
        
        if (shared$mlModel == "CNN") {
          # Load CNN model using Keras
          model_file <- file.path(model_paths(), model_input_value)
          print(model_file)
          if (file.exists(model_file)) {
            showNotification("Loaded CNN model file", type = "message")
            shared$usedModel <- tryCatch({
              load_model_tf(model_file)
            }, error = function(e) {
              showNotification(paste("Error loading CNN model:", e$message), type = "error")
              NULL
            })
          } else {
            showNotification("CNN model file not found", type = "warning")
            shared$usedModel <- NULL
          }
        } else {
          # Load the selected RDS model
          model_file <- file.path(model_paths(), model_input_value)
          if (file.exists(model_file)) {
            shared$usedModel <- readRDS(model_file)
            print(shared$usedModel)
          } else {
            shared$usedModel <- NULL
          }
        }
        
        
        # Determine selected group name from the chosen model's display name
        model_input_id <- paste0(shared$modelType, "_model")
        model_input_value <- input[[model_input_id]]
        
        # If the dynamic select is at its default (empty), clear the group name
        if (is.null(model_input_value) || model_input_value == "") {
          shared$selectedGroupName <- NULL
          print("Default selection; setting shared$selectedGroupName to NULL")
        } else {
          # Use the current model choices (no isolate so that changes to modelType are reflected)
          current_choices <- model_choices()
          label_index <- which(current_choices == model_input_value)
          
          if (length(label_index) == 1) {
            display_name <- names(current_choices)[label_index]
            shared$selectedGroupName <- display_name
          } else {
            print("Conditions not met")
            shared$selectedGroupName <- NULL
          }
        }
        oldmodelType <- shared$modelType
        print(paste0("Group Name:", shared$selectedGroupName))
        
        observeEvent(shared$modelType, {
          
          if(shared$modelType != oldmodelType) {
            shared$selectedGroupName <- NULL
          }
          
        })
        
        
        # Disable PCA plot if the model is CNN or PLS
        if (shared$mlModel != "PLS" && shared$mlModel != "CNN") {
          # Load PCA model if applicable
         
          pca_model_file <- file.path(model_paths(), paste0(gsub(paste0(find_model(), "_model"), "pca", model_input_value)))
          if (file.exists(pca_model_file)) {
            shared$usedPCA <- readRDS(pca_model_file)
            showNotification("PCA model found and loaded", type = "message")
            print(pca_model_file)
          } else {
            shared$usedPCA <- NULL
            showNotification("PCA model file not found", type = "warning")
          }
        } else {
          shared$usedPCA <- NULL
        }
      }
    })
  })
}

