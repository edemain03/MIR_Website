#extraction_methods.R

extraction_methodsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    card(
      title = "Extraction Methods",
      uiOutput(ns("extraction_methods")),
    )
  )
}

extraction_methodsServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    dat <- read.csv("www/Analyte_Methods.csv", stringsAsFactors = FALSE)
    
    # --- (1) code → long analyte name ----------------------------------
    property_lookup <- c(
      AS        = "Aggregate Stability, 0.5-2mm Aggregates",
      BD        = "Bulk Density, <2mm Fraction, Ovendry",
      Carbonate = "Carbonate, <2mm Fraction",
      TC        = "Carbon, Total",
      TN        = "Nitrogen, Total",
      pH        = "pH, 1:1 Soil-Water Suspension",
      P_Bray    = "Phosphorus, Bray-1 Extractable",
      P_Olsen   = "Phosphorus, Olsen Extractable",
      Clay      = "Clay",
      EC        = "Electrical Conductivity, Predict, 1:2 (w/w)",
      TS        = "Sulfur, Total",
      P_Mehlich = "Phosphorus, Mehlich-3 Extractable",
      Gypsum    = "Corrected Gypsum, <2mm",
      CEC       = "CEC, NH4OAc, pH 7.0, 2M KCl displacement",
      K         = "Potassium, NH4OAc Extractable, 2M KCl displacement",
      C_pom     = "Carbon, pom",
      C_hpom    = "Carbon, hpom",
      C_pom_mineral = "Carbon, pom mineral",
      Sand      = "Sand, Total",
      Silt      = "Silt, Total",
      SOC       = "Estimated Organic Carbon, CO2"
    )
    
    # --- (2) code → PDF file ------------------------------------------
    pdf_lookup <- c(
      AS        = "Kellogg_Lab_Manual.pdf",
      BD        = "Kellogg_Lab_Manual.pdf",
      Carbonate = "Kellogg_Lab_Manual.pdf",
      TC        = "Kellogg_Lab_Manual.pdf",
      TN        = "Kellogg_Lab_Manual.pdf",
      pH        = "Kellogg_Lab_Manual.pdf",
      P_Bray    = "Kellogg_Lab_Manual.pdf",
      P_Olsen   = "Kellogg_Lab_Manual.pdf",
      Clay      = "Kellogg_Lab_Manual.pdf",
      EC        = "Kellogg_Lab_Manual.pdf",
      TS        = "Kellogg_Lab_Manual.pdf",
      P_Mehlich = "Kellogg_Lab_Manual.pdf",
      Gypsum    = "Kellogg_Lab_Manual.pdf",
      CEC       = "Kellogg_Lab_Manual.pdf",
      K         = "Kellogg_Lab_Manual.pdf",
      C_pom     = "Kellogg_Lab_Manual.pdf",
      C_hpom    = "Kellogg_Lab_Manual.pdf",
      C_pom_mineral = "Kellogg_Lab_Manual.pdf",
      Sand      = "Kellogg_Lab_Manual.pdf",
      Silt      = "Kellogg_Lab_Manual.pdf",
      SOC       = "Kellogg_Lab_Manual.pdf"
    )
    
    
    # --- (3) translate code to full name + pdf ------------------------
    analyte_name <- reactive({
      code <- req(shared$selectedProperty)
      req(property_lookup[[code]])
    })
    pdf_file <- reactive({
      code <- req(shared$selectedProperty)
      pdf_lookup[[code]]               # will be NULL if not mapped
    })
    
    # --- (4) extract the method(s) ------------------------------------
    extraction_methods <- reactive({
      hit <- which(dat$analyte_name == analyte_name())
      if (length(hit))
        dat[hit, 3, drop = TRUE]
      else
        character(0)
    })
    
    # --- (5) render UI -------------------------------------------------
    output$extraction_methods <- renderUI({
      methods <- extraction_methods()
      link    <- pdf_file()
      
      tagList(
        if (length(methods)) tags$ul(lapply(methods, tags$li))
        else                 tags$p("No extraction methods found."),
        if (!is.null(link))
          tags$p(
            tags$a(
              "Download full manual (PDF)",
              href   = link,          # relative to www/
              target = "_blank",      # open in new tab
              download = NA, # also triggers download attribute
              style = "color: #0000EE;"
            )
          )
      )
    })
  })
}

