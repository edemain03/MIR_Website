# static_instructions.R
static_instructionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Instructions"),
    p("To use this page, first visit Data Preprocessing. Once you have processed data using the default settings, you can proceed to this page."),
    p("To begin, select the desired soil property in the sidebar, and your Machine learning model from the dropdown menu, as well as stratification method, and stratification parameter if applicable. To aid in this, model error metrics are included to help choose the best model for your data."),
    p("Once you have selected your model, upload your data and click make predictions."),
    p("Once run, you will be able to view model predictions, a PCA plot, a map of data points used in training, and a spectral plot."),
    tags$p("for more information on this page, see the ",
           tags$a("User Guide",
                  href    = "#",
                  style = "color: #0000EE",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', Math.random()); return false;", 
                    ns("goto_user_guide")
                  )
           ))
  )
}

static_instructionsServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$goto_user_guide, {
      # just flip a shared flag
      shared$goto_user_guide <- Sys.time()
    })
    
  })
}