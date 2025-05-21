library(shinyWidgets)
library(DT)
library(e1071)
library(dplyr)

strat_names_list <- list(
  depth = c("15" = "15cm", "30" = "30cm", "60" = "60cm", "100" = "100cm", "200" = "200cm"),
  order = c("alfisols" = "Alfisols", "entisols" = "Entisols", "ultisols" = "Ultisols", 
            "mollisols" = "Mollisols", "inceptisols" = "Inceptisols", "spodosols" = "Spodosols", 
            "andisols" = "Andisols", "histosols" = "Histosols", "vertisols" = "Vertisols", 
            "gelisols" = "Gelisols", "aridisols" = "Aridisols"),
  texture = c("Cl" = "Clay", "ClLo" = "Clay Loam", "Lo" = "Loam", "LoSa" = "Loam Sandy", 
              "Sa" = "Sandy", "SaCl" = "Sandy Clay", "SaClLo" = "Sandy Clay Loam",
              "SaLo" = "Sandy Loam", "Si" = "Silt", "SiCl" = "Silt Clay", 
              "SiClLo" = "Silt Clay Loam", "SiLo" = "Silt Loam"),
  lulc = c("Cultivated crops" = "Cultivated crops", "Decidous trees" = "Decidous trees",
           "Developed" = "Developed", "Evergreen forest" = "Evergreen forest",
           "Herbaceous_wetland" = "Herbaceous wetland", "Herbaceous" = "Herbaceous",
           "Mixed forest" = "Mixed forest", "pasture" = "Pasture", 
           "Shrub" = "Shrub", "Woody wetlands" = "Woody wetlands"),
  mlra = c("W1" = "W1","X1" = "X1","A" = "A","B" = "B","C" = "C","D" = "D","E" = "E",
           "F" = "F","G" = "G","H" = "H","I" = "I","J" = "J","K" = "K","L" = "L",
           "M" = "M","N" = "N","O" = "O","P" = "P","Q" = "Q","R" = "R","S" = "S",
           "T" = "T","U" = "U","V" = "V","W" = "W","X" = "X","Y" = "Y","Z" = "Z")
)

descriptiveStatsUI <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    height = "300px",
    card_header("Descriptive Statistics"),
    card_body(
      fluidRow(
        # Native label with a 'for' attribute
        tags$label(
          "See all data?",
          `for` = ns("show_all_data"),
          class = "control-label" # some styling class
        ),
        
        # prettySwitch with no label
        prettySwitch(
          inputId = ns("show_all_data"),
          label   = NULL,   # remove label here
          value   = FALSE,
          status  = "primary",
          fill    = TRUE
        ),
        br(),
        DTOutput(ns("desc_table")) %>% withSpinner(color = "#3734eb", type = 1, hide.ui = FALSE)
      )
      
    )
  )
}

descriptiveStatsServer <- function(id, shared, load_spectral_data_memo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    invert_named_vector <- function(x) {
      setNames(names(x), x)
    }
    
    # Unpack the lists for stratification
    depth_list_   <- strat_names_list$depth
    order_list_   <- strat_names_list$order
    texture_list_ <- strat_names_list$texture
    LULC_list_    <- strat_names_list$lulc
    MLRA_list_    <- strat_names_list$mlra
    
    get_strat_col <- function(modelType) {
      switch(modelType,
             "order"   = "taxonomic_order",
             "texture" = "Soil_Class",
             "depth"   = "Depths",
             "lulc"    = "Landuse",
             "mlra"    = "MLRA",
             NULL)
    }
    
    get_possible_categories <- function(modelType) {
      switch(modelType,
             "order"   = order_list_,
             "texture" = texture_list_,
             "depth"   = depth_list_,
             "lulc"    = LULC_list_,
             "mlra"    = MLRA_list_,
             NULL)
    }
    
    compute_stats <- function(df) {
      prop_clean <- df[complete.cases(df[, "calc_value"]), ]
      if (nrow(prop_clean) == 0) {
        return(data.frame(
          Count = 0,
          Min = NA,
          Max = NA,
          Mean = NA,
          SD = NA,
          Median = NA,
          f_Quartile = NA,
          T_quartile = NA,
          Skewness = NA
        ))
      }
      values <- as.double(prop_clean$calc_value)
      data.frame(
        Count      = length(values),
        Min        = round(min(values), 1),
        Max        = round(max(values), 1),
        Mean       = round(mean(values), 1),
        SD         = round(sd(values), 1),
        Median     = round(median(values), 1),
        f_Quartile = round(quantile(values, probs = 0.25), 1),
        T_quartile = round(quantile(values, probs = 0.75), 1),
        Skewness   = round(skewness(values), 1)
      )
    }
    
    # Build a stats table for all stratification levels
    # ------------------------------------------------------------------
    # replace your existing cat_stats() with this version
    cat_stats <- function(df, prefix, colname, cat_list) {
      # helper to grab UOM (and collapse in case there are more than one)
      get_uom <- function(d) {
        u <- unique(d$uom_abbrev[!is.na(d$uom_abbrev) & nzchar(d$uom_abbrev)])
        if (length(u) == 0) return(NA_character_)
        if (length(u) == 1) return(u)
        paste(u, collapse = "; ")
      }
      
      # global case (no stratification)
      if (is.null(colname)) {
        uom <- get_uom(df)
        s   <- compute_stats(df)
        return(cbind(
          Category = "Global",
          UOM      = uom,
          s
        ))
      }
      
      # stratified case
      results <- lapply(names(cat_list), function(code) {
        df_cat <- df[df[[colname]] == code, , drop = FALSE]
        uom    <- get_uom(df_cat)
        s      <- compute_stats(df_cat)
        cbind(
          Category = paste0(prefix, ": ", cat_list[code]),
          UOM      = uom,
          s
        )
      })
      do.call(rbind, results)
    }
    # ------------------------------------------------------------------
    
    # no other changes needed to compute_stats(), big_table_stats(), etc.
    # in your displayed_stats() you already rename f_Quartile/T_quartile to 25%/75%
    # and then renderDT() will pick up the new UOM column automatically.
    
    
    big_table_stats <- function(df) {
      global_stats   <- cat_stats(df, "",  NULL,        NULL)
      order_stats    <- cat_stats(df, "Order",   "taxonomic_order", order_list_)
      texture_stats  <- cat_stats(df, "Texture", "Soil_Class",      texture_list_)
      depth_stats    <- cat_stats(df, "Depth",   "Depths",          depth_list_)
      lulc_stats     <- cat_stats(df, "LULC",    "Landuse",         LULC_list_)
      mlra_stats     <- cat_stats(df, "MLRA",    "MLRA",            MLRA_list_)
      rbind(global_stats, order_stats, texture_stats, depth_stats, lulc_stats, mlra_stats)
    }
    
    base_data <- reactive({
      req(shared$selectedProperty)
      df <- load_spectral_data_memo(shared$selectedProperty)
      if (is.null(df) || nrow(df) == 0) return(data.frame())
      if (!"calc_value" %in% names(df)) return(data.frame())
      
      # Derive Depths if available
      if ("lay_depth_to_bottom" %in% names(df)) {
        df <- df %>%
          mutate(
            Depths = case_when(
              lay_depth_to_bottom <= 15  ~ "15",
              lay_depth_to_bottom <= 30  ~ "30",
              lay_depth_to_bottom <= 60  ~ "60",
              lay_depth_to_bottom <= 100 ~ "100",
              lay_depth_to_bottom <= 200 ~ "200",
              TRUE ~ NA_character_
            )
          )
      } else {
        df$Depths <- NA_character_
      }
      
      # Ensure needed columns exist
      if (!"taxonomic_order" %in% names(df)) df$taxonomic_order <- NA_character_
      if (!"Soil_Class" %in% names(df))       df$Soil_Class <- NA_character_
      if (!"Landuse" %in% names(df))          df$Landuse <- NA_character_
      if (!"MLRA" %in% names(df))             df$MLRA <- NA_character_
      
      df[complete.cases(df$calc_value), ]
    })
    
    # Render the category select UI only when not showing all data and for non-global stratification.
    output$category_ui <- renderUI({
      if (!is.null(input$show_all_data) && !input$show_all_data) {
        if (shared$modelType != "global") {
          choices <- get_possible_categories(shared$modelType)
          if (is.null(choices)) return(NULL)
          
          # Invert the vector so that the user sees the friendly name while the value is the code.
          inverted <- invert_named_vector(choices)
          
          return(selectInput(
            ns("strat_category"),
            label = "Select Category",
            choices = inverted,
            selected = names(inverted)[1]
          ))
        }
      }
      NULL
    })
    
    displayed_stats <- reactive({
      df <- base_data()
      if (nrow(df) == 0) {
        return(data.frame(
          Category = character(),
          UOM      = character(),
          Count    = numeric(),
          Min      = numeric(),
          Max      = numeric(),
          Mean     = numeric(),
          SD       = numeric(),
          Median   = numeric(),
          `25%`    = numeric(),
          `75%`    = numeric(),
          Skewness = numeric(),
          stringsAsFactors = FALSE
        ))
      }
      
      # ← Define these before using them
      show_all  <- isTRUE(input$show_all_data)
      modelType <- shared$modelType
      strat_col <- get_strat_col(modelType)
      cat_list  <- get_possible_categories(modelType)
      prefix    <- switch(modelType,
                          order   = "Order",
                          texture = "Texture",
                          depth   = "Depth",
                          lulc    = "LULC",
                          mlra    = "MLRA",
                          "")
      
      if (show_all) {
        if (modelType == "global") {
          df_stats <- big_table_stats(df)
        } else {
          df_stats <- cat_stats(df, prefix, strat_col, cat_list)
        }
      } else {
        if (modelType == "global" ||
            is.null(shared$selectedGroupName) ||
            shared$selectedGroupName == "") {
          df_stats <- cat_stats(df, "", NULL, NULL)
        } else {
          # normalize a few naming quirks the same as before…
          sel <- shared$selectedGroupName
          if (sel == "Woody wetland") sel <- "Woody wetlands"
          if (sel == "Shrubland")     sel <- "Shrub"
          if (sel == "Pasture")       sel <- "pasture"
          if (sel == "Herbaceous wetland") sel <- "Herbaceous_wetland"
          
          code <- names(which(strat_names_list[[modelType]] == sel))
          if (length(code) == 0) {
            df_stats <- cat_stats(df, "", NULL, NULL)
          } else {
            # build a one‐entry list so cat_stats still works
            one_list  <- setNames(cat_list[code], code)
            df_stats  <- cat_stats(df, prefix, strat_col, one_list)
          }
        }
      }
      
      df_stats %>%
        rename(`25%` = f_Quartile, `75%` = T_quartile)
    })
    
    
    output$desc_table <- renderDT({
      
      validate(
        need(!is.null(shared$modelType) && nzchar(shared$modelType),
             "Please select a stratification method to see descriptive statistics")
      )
      
      datatable(displayed_stats(), options = list(dom = 't', pageLength = -1), rownames = FALSE)
    })
  })
}
