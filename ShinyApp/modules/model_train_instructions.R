#model_train_instructions.R

instructionsUI <- function(id) {
  ns <- NS(id)
    card(
      title = "instructions",
      card_header = "Instructions",
      mainPanel(
      imageOutput(ns("csv_example"),
                  width = "50%",
                  height = "auto"),
      strong("CSV Format:"),
      p("Above is an image describing the format of the csv file that should be uploaded."),
      p("The first column should be the sample ID, the second column should be the response variable (calc_value), and the rest of the columns should be spectral data."),
      p("The response variable should be a numeric value with no units."),
      p("Ensure that you accurately input your wavelength start and end point, as well as step size. Your data should NOT include a row of column names."),
      strong("Training the model:"),
      p("Once you upload your data, first verify the reponse variable column name. Then, select your model type; Random Forest, Cubist, CNN, PLS, or SVM. The default is Random Forest."),
      p("Next choose your training data split, and cross validation method. The default is 70% training data and 10-fold cross validation."),
      p("Finally, choose your PCA variability explained. The default is 99%."),
      p("When you click train, the model will be trained on your uploaded data and will automatically find the best tuning parameters for your dataset."),
      strong("Results and Running your model:"),
      p("Once the model is trained, you can see results, including a 1-1 Observed vs Predicted plot, Calibration + Validation Error metrics, and a PCA plot."),
      p("You can also run your model on new data. Click the Download Model buton to download your model and any relevant training data, then upload this model to the Run Model tab."),
      p("Finally, upload your new data in the same format as the training data (without a response variable column), and click Run Model to see our new predictions"),
      tags$p(
        "For more information, please see the ",
        tags$a(
          "User Guide",
          href = "#",
          style = "color:#0000EE;",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Math.random()); return false;", 
            ns("goto_user_guide")
          )
        )
      )
      
      )
    )
}

instructionsServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    output$csv_example <- renderImage({
      
      
      list(src = "model_train/csv_example.png",
           contentType = "image/png",
           alt = "CSV Example")
    }, deleteFile = FALSE)
    
    observeEvent(input$goto_user_guide, {
      # just flip a shared flag
      removeModal()
      shared$goto_user_guide <- Sys.time()
      
    })
    
  })
}