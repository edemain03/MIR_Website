# main.R

# In Desc Stats, adjust so when using strats "Show All" shows all in the strat category, and Pin the navbar to the top so we see it when scrolling

options(warn=-1)

options(max.print = 10000)

options(error = function() {
  err <- geterrmessage()
  cat(sprintf("%s - ERROR: %s\n", Sys.time(), err),
      file = "error.log", append = TRUE)
})

library(shiny)
library(shinythemes)
library(DT)
library(bslib)
library(bsicons)
library(readxl)
library(sass)
library(ggplot2)
library(shinyWidgets)
library(memoise)
library(reticulate)
library(shinycssloaders)
library(keras)
library(FNN)
library(factoextra)
library(thematic)
library(readr)
library(shiny.commons)
library(tidyr)
library(bcrypt)

spectral_cache <- new.env(parent = emptyenv())

# Persistant cache
cache_dir <- "cache_data"
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir)
}

# Create a cache using the memoise filesystem cache
fs_cache <- cache_filesystem(cache_dir)

# Create a memoized function for loading and processing map data
load_map_data_memo <- memoise(function(property) {
  infile <- switch(property,
                   "AS" = "Full_DFs/AS.txt",
                   "BD" = "Full_DFs/BD.txt",
                   "C_pom_mineral" = "Full_DFs/C_pom_mineral.txt",
                   "C_hpom" = "Full_DFs/C_hpom.txt",
                   "C_pom" = "Full_DFs/C_pom.txt",
                   "Carbonate" = "Full_DFs/Carbonate.txt",
                   "CEC" = "Full_DFs/CEC.txt",
                   "Gypsum" = "Full_DFs/Gypsum.txt",
                   "K" = "Full_DFs/K.txt",
                   "P_Bray" = "Full_DFs/P_Bray.txt",
                   "P_Mehlich3" = "Full_DFs/P_Mehlich.txt",
                   "P_Olsen" = "Full_DFs/P_Olsen.txt",
                   "pH" = "Full_DFs/pH.txt",
                   "TC" = "Full_DFs/TC.txt",
                   "TN" = "Full_DFs/TN.txt",
                   "TS" = "Full_DFs/TS.txt",
                   "Clay" = "Full_DFs/Clay.txt",
                   "Sand" = "Full_DFs/Sand.txt",
                   "Silt" = "Full_DFs/Silt.txt",
                   "EC" = "Full_DFs/EC.txt",
                   "SOC" = "Full_DFs/SOC.txt",
                   NULL)
  
  if (is.null(infile) || !file.exists(infile)) {
    # Return NULL if no file or file not found
    return(NULL)
  }
  
  df <- read_delim(infile, delim =",")
  
  # Filter rows with complete coordinates
  df <- df[complete.cases(df[, c('latitude_std_decimal_degrees', 'longitude_std_decimal_degrees')]), ]
  
  # Get unique site IDs to reduce clutter
  unique_df <- df %>%
    distinct(lims_site_id, .keep_all = TRUE)
  
  unique_df
}, cache = fs_cache)

# Source module files
source_files <- list.files('./modules', pattern = '\\.R$', full.names = TRUE)
sapply(source_files, source)

# Define soil properties
soilProperties <- list(
  "Sand        " = "Sand",
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
  "Gypsum" = "Gypsum"
)

load_spectral_data_memo <- memoise(function(property) {
  infile <- switch(property, 
                   "AS" = "spectral_data/AS.txt",
                   "BD" = "spectral_data/BD.txt",
                   "C_pom_mineral" = "spectral_data/C_pom_mineral.txt",
                   "C_hpom" = "spectral_data/C_hpom.txt",
                   "C_pom" = "spectral_data/C_pom.txt",
                   "Carbonate" = "spectral_data/Carbonate.txt",
                   "CEC" = "spectral_data/CEC.txt",
                   "Gypsum" = "spectral_data/Gypsum.txt",
                   "K" = "spectral_data/K.txt",
                   "P_Bray" = "spectral_data/P_Bray.txt",
                   "P_Mehlich3" = "spectral_data/P_Mehlich3.txt",
                   "P_Olsen" = "spectral_data/P_Olsen.txt",
                   "pH" = "spectral_data/pH.txt",
                   "TC" = "spectral_data/TC.txt",
                   "TN" = "spectral_data/TN.txt",
                   "TS" = "spectral_data/TS.txt",
                   "Clay" = "spectral_data/Clay.txt",
                   "Sand" = "spectral_data/Sand.txt",
                   "Silt" = "spectral_data/Silt.txt",
                   "EC" = "spectral_data/EC.txt",
                   "SOC" = "spectral_data/SOC.txt",
                   NULL)
  if (!is.null(infile) && file.exists(infile)) {
    data <- read_delim(infile, delim = ",")
    return(data)
  } else {
    print(paste0("no data found for", shared$selectedProperty))
    return(NULL)
  }
}, cache = fs_cache)

ui <- fluidPage(
  useShinyjs(),
  loginUI("auth"), 

  # These are just default Theme settings, many are overwritten in 'www/styles.css'
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    primary = "#f5eded",       
    secondary = "#030303",    
    success = "#4B9B3D",       
    info = "#5BC0DE",          
    warning = "#FFD700",     
    danger = "#FF4500",        
    base_font = font_google("Arimo"),
    heading_font = font_google("Arimo"),
    bg = "#fcfafa",           
    fg = "#030303",
    "dropdown-bg"            = "#bfbebd",  # same as navbar bg
    "dropdown-border-color"  = "#f5eded",
    "dropdown-link-color"    = "#030303",  # text colour
    "dropdown-link-hover-color" = "#030303",
    "dropdown-link-hover-bg" = "#e4e4e4",   # subtle hover
    "dropdown-min-width" = "14rem",
    "dropdown-item-padding-y" = ".55rem",
    "dropdown-font-size" = "1.3rem",
    "navbar-nav-link-padding-y" = ".65rem",
    "navbar-nav-link-padding-x" = "1.0rem",
    "navbar-nav-link-font-size" = "1.0rem"  
  ),
  
  tags$head(
    # This script sets the lang attribute on the <html> element to English ("en")
    tags$script(HTML("
    // Set the language of the document to English
    document.documentElement.setAttribute('lang', 'en');
    
        // home item reordering script, must keep specific order in definitions or certain pages won't load
    document.addEventListener('DOMContentLoaded', function() {
      // Find the <a> that has data-value='home'
      const homeLink = document.querySelector('.navbar-nav .nav-item a[data-value=\"home\"]');
      if (homeLink) {
        // Grab the parent <li>
        const homeLi = homeLink.closest('li.nav-item');
        // Apply inline style to reorder
        homeLi.style.order = '-1';
      }
    });
    
     // handler for zoom messages
  Shiny.addCustomMessageHandler('setZoom', function(z) {
    document.body.style.zoom = z;
  });
    
        // Give every nav-tabs container the required ARIA role
    function fixTabRoles() {
      document.querySelectorAll('ul.nav.nav-tabs:not([role=\"tablist\"])')
        .forEach(el => el.setAttribute('role', 'tablist'));
    }
    // run now, and whenever Shiny swaps DOM
    document.addEventListener('DOMContentLoaded', fixTabRoles);
    $(document).on('shiny:recalculated', fixTabRoles);
  ")),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
 
  # UI for zoom functionality
  absolutePanel(
    id = "fontSizer", top = 70, right = 20, width = "auto",
    style = "background: rgba(255,255,255,0.8); padding: 4px; border-radius:4px; z-index:1000;",
    actionLink("small",  HTML("<span style='font-size:0.8em; color:#542b2b; cursor:pointer;'>A</span>")),
    actionLink("medium", HTML("<span style='font-size:1em; color:#542b2b; cursor:pointer; margin:0 6px;'>A</span>")),
    actionLink("large",  HTML("<span style='font-size:1.2em; color:#542b2b; cursor:pointer;'>A</span>"))
  ),
  
  # Navigation UI
  page_navbar(
    id = "page_navbar",
    position = c("fixed-top"),
    collapsible = FALSE,
    title = tags$div(
      class = "navbar-brand d-flex align-items-center flex-nowrap logo-strip",
      
      # each logo keeps its own margin so the spacing looks
      # the same whenever it’s on screen
      tags$img(src = "logo.png",         class = "logo-brand logo-mir me-4", alt = "MIR Logo"),
      tags$img(src = "beavs_logo.png",   class = "logo-brand logo-osu me-4", alt = "Oregon State University Logo", height = 40),
      tags$img(src = "uw_madison_logo.png",
               class = "logo-brand logo-uw  me-4", height = 40, alt = "University of Wisconsin-Madison Logo"),
      
      # USDA mini-block can stay exactly as you had it
      tags$div(class = "d-inline-flex align-items-center me-4 logo-usda",
               tags$img(src = "USDA.png",  class = "logo-brand me-2", height = 40,
                        alt = "USDA Logo"),
               tags$div(
                 style = "color: #0a0a0a; font-size: 16px",
                 tags$div(class = "site-name", "Natural Resources Conservation Service"),
                 tags$div("U.S. Department of Agriculture")
               )
      )
    )
    ,
    

    
    header = tags$div(
      class = "top-right-btn",
      
      uiOutput("conditional_error_button"),
      uiOutput('instructions_button')
    ),
    
    nav_spacer(),
    # Use nav_menu to save space
    nav_menu(
      title = "Tools",
      icon = bs_icon("list"),
      align = "right",
      nav_panel(
        title = tagList(bs_icon("filter"), "Data Preprocessing"),
        value = "data_preprocessing",
        id = "data_preprocessing",
        dataAggregationUI("data_aggregation")
    ),
    
      # ---- Static Models ----
      # This is the main tab for static models, which contains all the soil properties
      # and their respective models
    nav_panel(
      
      title = tagList(bs_icon("card-list"), "Static Models"),
      value = "static_models",
      id = "static_models",
      layout_sidebar(
        sidebar = sidebar(
          # wrap the entire navset_card_tab in a focusable div
          tags$div(
            class = "sidebar",
            role       = "region",
            `aria-label` = "Soil property navigation",
            tabindex   = "0",
            style      = "outline: none;",  # so you don’t get an ugly default focus ring
            do.call(
              navset_card_tab,
              c(
                list(id = "soilProperty"),
                lapply(names(soilProperties), function(name) {
                  nav_panel(title = name, value = soilProperties[[name]])
                })
              )
            )
          )
        ),
        # Main content area
        fluidRow(
          layout_column_wrap(
            height = "300",
            heights_equal = c("all", "row"),
            fillable = TRUE,
            width = 1/2,
            uiOutput("model_content"),
            uiOutput("data_input")
          )),
        uiOutput("extraction_methods"),
        verticalLayout(
          uiOutput("descriptive_stats"),
          uiOutput("box_plots"),
          
          layout_column_wrap(
            heights_equal = "all",
            fillable = TRUE,
            width = 1/2,
            card(
              card_header("PCA Plot"),
              full_screen = TRUE,
              plotOutput("pca_plot")%>% withSpinner(color = "#3734eb", type = 1, hide.ui = FALSE)
            ),
            uiOutput("map_module")
          ),
          uiOutput("prediction_results"),
          uiOutput("spec_plots")
        )
      )
    ),
    

    
        
      nav_panel(
        value = "knn_model",
        id = "knn_model",
        title = tagList(bs_icon("hdd-rack"), "Customized Model"),
        knnUI("knn")
      ),
      nav_panel(
        value = "train_model",
        id = "train_model",
        title = tagList(bs_icon("pc"), "Build Your Own Model"),
        modelTrainUI("model_training"),
        customModelUI("custom_model")
      )),
    
    nav_panel(
      title = tagList(bs_icon("house"), "Home"),
      value = "home",
      id = "home",
      homePageUI("home")
    ),
    
   
    # Tools tab doesn't indicate which page is selected so use this
    # In the future, potentially set the name of tools nav menu to be whatever page is selected ("tools" if home)
    nav_item( uiOutput("page_indicator")),
    
    # Login menu, only shows relevant options (i.e., won't show login if user is already logged in)
    nav_menu(
      title  = tagList(bs_icon("person-circle"), "Account"),
      value     = "account_menu",
      align  = "right",                    # keeps it flush with the icon’s edge
      
      # ---- menu items are just nav_item() wrappers around actionLink() ----
      nav_item(actionLink("account_login",  "Log in")),
      nav_item(actionLink("account_logout", "Log out")),
      nav_item(actionLink("account_create", "Create account"))
    ),
    
    selected = "home"
    
  ),
  dsh_footer()
)


server <- function(input, output, session) {
  options(shiny.maxRequestSize = 500 * 1024^2)
  
  shinyjs::addClass(id = "page_navbar", class = "navbar-right")
  
  thematic_shiny()
  
  # load hashed usernames / passwords
  credentials <- readRDS("www/credentials.rds")
  
  credentials[] <- lapply(credentials, function(col) {
    if (is.factor(col)) as.character(col) else col
  })
  
  credentials_path <- "www/credentials.rds"
  logged <- loginServer("auth", credentials_path)
  
  # ----  ObserveEvents for zoom change, default 100% --------
  observeEvent(input$small,  session$sendCustomMessage("setZoom","100%"))
  observeEvent(input$medium, session$sendCustomMessage("setZoom","120%"))
  observeEvent(input$large,  session$sendCustomMessage("setZoom","150%"))
  
  # ---- page‑indicator ----------------------------------------------------------
  output$page_indicator <- renderUI({
    req(input$page_navbar)               # only run once the navbar exists
    
    page <- switch(
      input$page_navbar,
      "data_preprocessing" = "Data Preprocessing",
      "static_models"      = "Static Models",
      "knn_model"          = "Customized Model",
      "train_model"        = "Build Your Own Model",
      NULL                 # ← for "home" show nothing
    )
    
    if (is.null(page)) return(NULL)      # hide on Home
    
    tags$span(
      style  = "color:#000000 !important;",  
      class  = "navbar-text ms-2 fw-semibold",  #bold, black, easy to read
      page
    
    )
  })
  
  # ---- show only the link that's relevant for login -------------------
  observe({
    if (logged()) {
      shinyjs::hide("account_login",  asis = TRUE)
      shinyjs::show("account_logout", asis = TRUE)
      shinyjs::hide("account_create", asis = TRUE)
    } else {
      shinyjs::show("account_login",  asis = TRUE)
      shinyjs::hide("account_logout", asis = TRUE)
    }
  })
  

  # ------------------------------------------------------------------
  #  1. show the login panel the first time the user tries to leave Home
  #  2. bounce them back to Home until they succeed
  # ------------------------------------------------------------------
  observeEvent(input$page_navbar, {
    if (input$page_navbar != "home" && !logged()) {
      session$userData$showLogin()        # <── same here
      showNotification("Please log-in first", type = "message")
      updateNavbarPage(session, "page_navbar", selected = "home")
    }
  })
  
  observeEvent(input$account_login,  {
    session$userData$showLogin()          # <── opens the login modal
  })
  
  # Upon logout refresh app, ensures user logs out and also
  observeEvent(input$account_logout, {
    session$reload()    
  })
  
  # Create account modal
  observeEvent(input$account_create, {
    showModal(
      modalDialog(
        title = "Create a new account",
        textInput("new_user", "Username"),
        passwordInput("new_pw", "Password"),
        passwordInput("new_pw2", "Confirm password"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_new_user", "Create account")
        )
      )
    )
  })
  
  # ----  "Create account" handler  ---------------------------------------------
  observeEvent(input$submit_new_user, {
    # 1. Make sure all three boxes are filled
    req(input$new_user, input$new_pw, input$new_pw2)
    
    # 2. Trim white‑space and pull the existing table off disk
    uname      <- trimws(input$new_user)
    creds_path <- credentials_path      # defined near the top of server()
    creds_df   <- if (file.exists(creds_path)) readRDS(creds_path) else
      data.frame(username = character(),
                 hash     = character(),
                 stringsAsFactors = FALSE)
    
    # 3. Quick safety checks -----------------------------------------------------
    if (uname == "") {
      showNotification("Username can’t be blank.", type = "error");      return()
    }
    if (input$new_pw != input$new_pw2) {
      showNotification("Passwords don’t match.",  type = "error");       return()
    }
    if (uname %in% creds_df$user) {
      showNotification("That username is already taken.", type = "error");return()
    }
    if (nchar(input$new_pw) < 8) {           # optional—strength check
      showNotification("Password must be at least 8 characters.",
                       type = "error");                                   return()
    }
    
    # 4. Hash the password (bcrypt automatically salts)
    pw_hash <- bcrypt::hashpw(input$new_pw)
    
    # 5. Append the new row and write it back to disk ---------------------------
    new_row <- data.frame(
      user     = uname,
      pwd_hash = pw_hash,
      stringsAsFactors = FALSE
    )
    
    creds_df <- dplyr::bind_rows(creds_df, new_row)
    
    # Use a simple file‑lock so two users can’t register at the exact same time. (edge case issues)
    if (!requireNamespace("filelock", quietly = TRUE)) install.packages("filelock")
    lock <- filelock::lock(paste0(creds_path, ".lock"))
    on.exit(filelock::unlock(lock), add = TRUE)
    saveRDS(creds_df, creds_path)
    
    # Update the in‑memory copy so this session can log in immediately
    credentials <<- creds_df      # relies on the global `credentials`
    
    removeModal()
    showNotification("Account created – you can now log in!", type = "message")
  })
  
  # Long list of shared reactive values among all scripts
  # Gets passed into each server function
  shared <- reactiveValues(
    mlModel = NULL,
    modelType = NULL,
    usedModel = NULL,
    usedPCA = NULL,
    pcaPlot = NULL,
    predictions = NULL,
    selectedProperty = NULL,
    df = NULL,
    selectedGroupName = NULL,
    modelChoices = NULL,
    ntree = NULL,
    trainType = NULL,
    ncomp = NULL,
    cost = NULL,
    gamma = NULL,
    committees = NULL,
    neighbors = NULL,
    trainPercent = NULL,
    modelFile = NULL,
    runModel = FALSE,
    trainModel = FALSE,
    custom_params = NULL,
    variance = NULL,
    cust_pca_model = NULL,
    cust_main_model = NULL,
    knnTrainDataPlot = NULL,
    find_model = NULL,
    tablestrata = NULL,
    tablemodelType = NULL,
    tablemlModel = NULL,
    change_guide = FALSE,
    click_data_preproc = FALSE,
    click_static_models = FALSE,
    click_custom_models = FALSE,
    click_byo_models = FALSE,
    ncomp       = 2,
    ntree       = 100,
    cost        = 1,
    gamma       = 0.1,
    committees  = 100,
    neighbors   = 0,
    page_navbar = "home"
  )
  
  # Show error metrics button if static models page is selected
  output$conditional_error_button <- renderUI({
    req(input$page_navbar)
    if (input$page_navbar == "static_models") {
      tags$div(
        class = "d-flex align-items-center gap-2",   
        actionButton("show_errors", "View Error Metrics"),
        actionButton("show_static_instructions", "Instructions")
      )
    }
  })
  
  # Update shared$page_navbar when the user clicks on a different page
  observeEvent(input$page_navbar, {
    shared$page_navbar <- input$page_navbar
  })
  
  # Observe changes in each of the model training parameters, update them to share
  observeEvent(input$trainPercent, {
    if (shared$custom_params == TRUE) {
      shared$trainPercent <- input$trainPercent
    } else {
      shared$trainPercent <- 0.7
    }
  })

  # For when user uploads a custom model
  observeEvent(input$modelFile, {
    shared$modelFile <- input$modelFile
  })
  
  observeEvent(input$run_cust_model, {
    #create a boolean shared$runModel in run_cust_model.R, set to true, and then in the custom model server, check if this is true, and if it is, run the model. Then set back to false in server after run
    shared$runModel <- TRUE
  })
  
  # SHow instuctions when custom model page is selected
  output$instructions_button <- renderUI({
    req(input$page_navbar) # Ensure input is available
    if (input$page_navbar == "train_model") {
      actionButton("train_instructions", "View Instructions")
    } else {
      NULL
    }
  })
  
  # Update selected soil Property
  observeEvent(input$soilProperty, {
    shared$selectedProperty <- input$soilProperty
  }, ignoreNULL = FALSE)
  
  
  # Update the model content based on the selected soil property
  output$model_content <- renderUI({
    req(shared$selectedProperty)
    # Call the UI of the selected module
    modelSelectionUI(shared$selectedProperty)
  })
  outputOptions(output, "model_content", suspendWhenHidden = FALSE)
  
  # Update the PCA plot based on the selected soil property
  output$pca_plot <- renderPlot({
    req(shared$pcaPlot)
    shared$pcaPlot
  })
  
  # UI options for static models
  output$data_input <- renderUI({
    dataInputUI("data_input")
  })
  
  # UI options for static models
  output$extraction_methods <- renderUI({
    extraction_methodsUI("extraction_methods")
  })
  # UI options for static models
  output$spec_plots <- renderUI({
    specPlotsUI("spec_plots")
  })
  # UI options for static models
  output$descriptive_stats <- renderUI({
    descriptiveStatsUI("descriptive_stats")
  })
  # UI for KNN models page
  output$custom_model <- renderUI({
    customModelUI("custom_model")
  })
  # UI for BYO models page
  output$model_train <- renderUI({
    modelTrainUI("model_training")
  })
  # prediction results UI
  output$prediction_results <- renderUI({
    predictionResultsUI("prediction_results")
  })
  # UI for map module
  output$map_module <- renderUI({
    mapUI("map_module")
  })
  # UI for box plots
  output$box_plots <- renderUI({
    boxPlotsUI("boxplots")
  })
  
  # Initialize server logic for each soil property
  lapply(names(soilProperties), function(name) {
    prop <- soilProperties[[name]]
    modelSelectionServer(
      id = prop,
      shared,
      soilPropertyName = name
    )
  })

  # Initialize Server logic for each file
  dataAggregationServer("data_aggregation", shared)
  dataInputServer("data_input", shared, load_spectral_data_memo)
  predictionResultsServer("prediction_results", shared)
  specPlotsServer("spec_plots", shared, fs_cache = fs_cache, load_spectral_data_memo)
  mapServer("map_module", shared, fs_cache = fs_cache, load_map_data_memo)
  boxPlotsServer("boxplots", shared, fs_cache = fs_cache, load_spectral_data_memo)
  descriptiveStatsServer("descriptive_stats", shared, load_spectral_data_memo)
  errorMetricsServer("error_metrics", shared)
  instructionsServer("instructions", shared)
  modelTrainingServer("model_training", shared, load_spectral_data_memo)
  customModelServer("custom_model", shared)
  knnServer("knn", shared, load_spectral_data_memo)
  extraction_methodsServer("extraction_methods", shared)
  homepageServer("home", shared)
  static_instructionsServer("static_instructions", shared)
  
  # When user clicks on any "User Guide" button, send them home (where user guide is)
  observeEvent(shared$goto_user_guide, {
    # 1) Switch the top-level navbar:
    updateNavbarPage(session, "page_navbar", selected = "home")
    removeModal()
  }, ignoreInit = TRUE)
  
  # Change page to data preprocessing
  observeEvent(shared$click_data_preproc, {
    if (shared$click_data_preproc == TRUE) {
      updateNavbarPage(session, "page_navbar", "data_preprocessing")
      shared$click_data_preproc <- FALSE
    }
  })
  
  # Change page to static models
  observeEvent(shared$click_static_models, {
    if (shared$click_static_models == TRUE) {
      updateNavbarPage(session, "page_navbar", "static_models")
      shared$click_static_models <- FALSE
    }
  })
  
  # Change page to custom models
  observeEvent(shared$click_custom_models, {
    if (shared$click_custom_models == TRUE) {
      updateNavbarPage(session, "page_navbar", "knn_model")
      shared$click_custom_models <- FALSE
    }
  })
  
  # Change page to BYO models
  observeEvent(shared$click_byo_models, {
    if (shared$click_byo_models == TRUE) {
      updateNavbarPage(session, "page_navbar", "train_model")
      shared$click_byo_models <- FALSE
    }
  })
  
  # Doesn't work for now. Will hopefully fix late
  observeEvent(input$page_navbar, {
    if (input$page_navbar == "home" && !is.null(shared$goto_user_guide)) {
      updateTabsetPanel(session, "helpNav",   selected = "user_guide")
      updateTabsetPanel(session, "guideTabs", selected = "data_preprocessing")
      shared$goto_user_guide <- NULL
    }
  }, ignoreInit = TRUE)
  
  
  # Show error metrics modal when button is clicked
  observeEvent(input$show_errors, {
    showModal(
      modalDialog(
        title = "Error Metrics",
        errorMetricsUI("error_metrics"),
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  # Show static model instructions when button is clicked
  observeEvent(input$show_static_instructions, {
    showModal(
      modalDialog(
        title = "Static Model Instructions",
        static_instructionsUI("static_instructions"),
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  # Show instructions when button is clicked
  observeEvent(input$train_instructions, {
    showModal(
      modalDialog(
        title = "Model Training Instructions",
        instructionsUI("instructions"),
        easyClose = TRUE,
        size = "l",
        tags$style(HTML("
                        .modal-dialog {
                        width: 46% !important;
                        max-width: none;"))
      )
    )
  })
}

shinyApp(ui = ui, server = server)
