# modules/boxPlots.R
library(shiny)
library(ggplot2)
library(dplyr)

boxPlotsUI <- function(id) {
  ns <- NS(id)
  card(
    card_header("Box Plots"),
    card_body(
      plotOutput(ns("boxplot"), height = "500px")%>% withSpinner(type = 1, color = "#3734eb", hide.ui = FALSE)
    )
  )
}

boxPlotsServer <- function(id, shared, fs_cache, load_spectral_data_memo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    strat_names_list <- list(
      depth = c("15" = "15cm", "30" = "30cm", "60" = "60cm", "100" = "100cm", "200" = "200cm"),
      order = c("alfisols" = "Alfisols", "entisols" = "Entisols", "ultisols" = "Ultisols", 
                "mollisols" = "Mollisols", "mollisol" = "Mollisols", "inceptisols" = "Inceptisols", 
                "spodosols" = "Spodosols", "andisols" = "Andisols", "histosols" = "Histosols", 
                "vertisols" = "Vertisols", "gelisols" = "Gelisols", "aridisols" = "Aridisols", 
                "oxisols" = "Oxisols"),
      texture = c("Cl" = "Clay", "ClLo" = "Clay Loam", "Lo" = "Loam", "LoSa" = "Loam Sandy", 
                  "Sa" = "Sandy", "SaCl" = "Sandy Clay", "SaClLo" = "Sandy Clay Loam",
                  "SaLo" = "Sandy Loam", "Si" = "Silt", "SiCl" = "Silt Clay", 
                  "SiClLo" = "Silt Clay Loam", "SiLo" = "Silt Loam"),
      lulc = c("Cultivated crops" = "Cultivated crops", "Decidous trees" = "Decidous trees", 
               "Developed" = "Developed", "Evergreen forest" = "Evergreen forest", 
               "Herbaceous_wetland" = "Herbaceous wetland", "Herbaceous" = "Herbaceous",
               "Mixed forest" = "Mixed forest", "pasture" = "Pasture", 
               "Shrub" = "Shrub", "Woody wetlands" = "Woody wetlands")
    )
    
    # A small helper to show soil property with units or a friendlier label.
    get_property_label <- function(prop) {
      switch(prop,
             "Sand"      = "Sand Content (% wt)",
             "Silt"      = "Silt Content (% wt)",
             "Clay"      = "Clay Content (% wt)",
             "AS"        = "Aggregate Stability (% wt)",
             "BD"        = "Bulk Density (g/cm³)",
             "pH"        = "pH (unitless)",
             "CEC"       = "CEC (cmol/kg)",
             "Carbonate" = "Carbonate (% wt)",
             "C_hpom"    = "Carbon (hpom) (% wt)",
             "C_pom"    = "Carbon (pom) (g/g)",
             "C_pom_mineral" = "Carbon (pom mineral) (g/g)",
             "EC"       = "Electrical Conductivity (dS/m)",
             "Gypsum" = "Gypsum (% wt)",
             "K"        = "Potassium (cmol/kg)",
             "P_Bray"   = "Phosphorus (Bray) (mg/kg)",
             "P_Mehlich" = "Phosphorus (Mehlich) (mg/kg)",
             "P_Olsen" = "Phosphorus (Olsen) (mg/kg)",
             "SOC"      = "Soil Organic Carbon (%)",
             "TC" = "Total Carbon (% wt)",
             "TN" = "Total Nitrogen (% wt)",
             "TS" = "Total Sulfur (% wt)",
             prop
      )
    }
    
    data_reactive <- reactive({
      req(shared$selectedProperty)
      
      df <- load_spectral_data_memo(shared$selectedProperty)
      if (!"calc_value" %in% colnames(df)) {
        showNotification("No 'calc_value' column in loaded data.", type = "error")
        return(NULL)
      }
      df <- df[complete.cases(df$calc_value), ]
      
      # Determine the stratification column
      strat_col <- switch(
        shared$modelType,
        "order"   = "taxonomic_order",
        "texture" = "Soil_Class",
        "depth"   = "Depths",  # We'll define it if needed
        "lulc"    = "LULC",
        "mlra"    = "MLRA",
        "global"  = NULL
      )
      
      # If global, we’ll just have one “Global” group
      if (shared$modelType == "global") {
        df$Strat <- "Global"
      } else if (shared$modelType == "depth") {
        if (!"lay_depth_to_bottom" %in% colnames(df)) {
          showNotification("No 'lay_depth_to_bottom' column in loaded data.", type = "error")
          return(NULL)
        }
        
        # Convert numeric depths to categories
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
        df$Strat <- df$Depths
        
      } else {
        # For other stratifications: order, texture, lulc, mlra
        if (is.null(strat_col) || !strat_col %in% names(df)) {
          showNotification(
            paste("No stratification column found for modelType:", shared$modelType),
            type = "error"
          )
          return(NULL)
        }
        df$Strat <- df[[strat_col]]
        df$Strat[df$Strat == ""] <- "NA"
      }
      
      # If our modelType is one that has a named vector for display names,
      # recode the values to friendlier text
      if (shared$modelType %in% names(strat_names_list)) {
        df <- df %>%
          mutate(Strat = dplyr::recode(Strat, !!!strat_names_list[[shared$modelType]]))
      }
      
      df
    })
    
    
    output$boxplot <- renderPlot({
      
      validate(
        need(!is.null(shared$modelType) && nzchar(shared$modelType),
             "Please select a stratification method to see boxplots")
      )
      
      df <- data_reactive()
      req(df)
      
      # Figure out what group is "selected." If the user chose a sub-model 
      # (MLRA A, etc.), shared$selectedGroupName will be that label. 
      # If it's empty or global, there's nothing to highlight.
      selected_group <- if (!is.null(shared$selectedGroupName) && nchar(shared$selectedGroupName) > 0) {
        shared$selectedGroupName
      } else {
        NA_character_
      }
      
      # Create a new column that indicates whether each row is in the "selected" group 
      # or "other."
      df <- df %>%
        mutate(
          highlight_group = if_else(Strat == selected_group, "Highlighted", "Other")
        )
      
      # Get a nice label (with units) for the Y‐axis
      ylab_text <- get_property_label(shared$selectedProperty)
      
      # Build the boxplot
      # We map fill to highlight_group, not to Strat. 
      # Then we can color "Highlighted" vs "Other" differently.
      p <- ggplot(df, aes(x = Strat, y = calc_value, fill = highlight_group)) +
        scale_color_dsh() +
        scale_fill_dsh() + 
        theme_dsh() +
        geom_boxplot(
          width = 0.5,
          outlier.shape = 16,
          outlier.size = 1.5,
          outlier.stroke = 0.4,
          outlier.color = "grey20",
          notch = FALSE
        ) +
        xlab(NULL) +
        ylab(ylab_text) +
        theme(
          legend.position  = "none",   # Hide legend if you like
          axis.text.x      = element_text(color="black", size=12, angle=45, hjust=1),
          axis.text.y      = element_text(color="black", size=12),
          axis.title.x     = element_text(size=14),
          axis.title.y     = element_text(size=14),
          panel.border     = element_blank(),
          panel.grid       = element_blank(),
          plot.margin      = unit(c(10, 10, 10, 10), "pt")
        ) +
        # Use a manual fill scale: "Highlighted" gets a color, "Other" is gray
        scale_fill_manual(
          values = c("Highlighted" = "#E69F00",   # e.g. orange
                     "Other"       = "grey80")
        )
      
      # If "global," we can flip for a single horizontal box
      if (shared$modelType == "global") {
        p <- p + coord_flip()
      }
      
      p
    }) 
  })
}
