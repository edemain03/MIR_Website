#prediction_results_main.R

#displays prediction results. Apparently in TC website predictions were handled in another file so we just gonna handle it here now for obvious reasons

library(stringr)

predictionResultsUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Results Table"),
      div(
        h4(textOutput(ns("table_title"))),  # ← dynamic title
        DTOutput(ns("predictions"))
      )
    )
  )
}


predictionResultsServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$predictions <- renderDT({
      req(shared$predictions)
      datatable(shared$predictions, options = list(
        scrollX = TRUE,
        pageLength = 5,
        lengthMenu = c(5, 10, 25, 50, 100)
      ))
    })
    
    output$table_title <- renderText({
      req(shared$selectedProperty)
      if (is.null(shared$tablestrata)) {
        paste("Predictions for ", str_to_title(shared$tablemodelType), shared$tablemlModel, "model") 
      } else {
        paste("Predictions for ", shared$tablemlModel, shared$tablestrata, "model")
      }
    })
  })
}
