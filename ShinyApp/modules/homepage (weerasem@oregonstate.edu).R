# homepage.R

library(leaflet)
library(bslib)
library(DiagrammeR)

homePageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$figure(
      style = "margin: 0; padding: 0; text-align: center;",
      class = "centerFigure",
      tags$img(src = "background.jpg",
               style = "
                          max-width: 100%;
                          display: block;
                          margin: 0 auto;
                          border-radius: 8px;
                        ",
               alt = "Soils Background Image"
               )
    ),
    
    br(),
    
    tags$head(
      tags$style(HTML("
      .card-header-tabs.nav-tabs .nav-link.active {
        font-weight: bold !important;
      }
    "))
    ),
    
    h2("Help & Documentation"),
    
    navset_card_tab(
      id = ns("helpNav"),
      nav_panel(
        title = "About",
        value = "about",
        HTML("
             <h1>Welcome to the MIR Soil Property Platform</h1>
             
             <p>The MIR Soil Property Platform is a user-friendly, fast, web-based platform for soil property estimation from MIR spectra.  After uploading MIR spectra, some preprocessing, and choosing and executing a prediction model, this platform will provide soil property estimates with a prediction accuracy. 
             <br>
             <p>This web tool has four components :
             <ul 'font-size: 22px;'>
             <li><strong>Data Preprocessing</strong> 
             <li><strong>Static models</strong>
             <li><strong>Customized models</strong>
             <li><strong>Build your own machine learning models</strong>
             </ul>
             <p>To access each component, use the dropdown menu under the <strong>Tools</strong> tab in the top-right corner of the page. 
             <br>
             <p>Let MIR Soil-Analytics handle the modelling so that you can focus on results. 
             <br>
             <p>Need help choosing the right tool for your dataset? Use our decision tree to navigate the web tool efficiently. Visit <strong>Which Tool is Right for Me? </strong> to get started!
             
             ")
        ),

#      
#      nav_panel(
#        title = "Custom Models",
#        HTML("
#              <h2> Custom models are a helpful way to build customized models that are fine tuned to be used for your specific data. </h2>
#              <ul style = 'font-family: Verdana; font-size: 22px;'>
#              <li>There are 2 different model training options, training a model to use/save for later, and using a KNN algorithm to fine tune a model based on existing KSSL Data.
#              
#              <li>To do this, we recommend using the 'Model Training' tool. Compress all of your scanned samples (.csv, .0) as a zip file, and upload it here.
#              
#              <li>If you wish to train models to download and use later based on your own data (must include a response variable), use the 'Build Your Own Model' page. You may also use this page to upload and use models trained on the page and stored locally.
#              
#              <li>If you wish to train a model for one time use based on data without a response variable, use the 'KNN Model' page, which creates a customized model based on similar data points.
#             </ul>")
#      ),
      
      nav_panel(
        title = "Which Tool is Right for Me?",
        value = "which_tool",
        HTML("
             <h2>Which Tool is Right for Me?</h2>
             <p>Use the flowchart below to determine which tool is best suited for your data.</p>
             "),
        div(style ="width: 1200px; margin: 0 auto;",

        grVizOutput(ns("whichToolFlow")),
        

        
      )),

nav_panel(
  title = "User Guide",
  value = "user_guide",
  fluidRow(
    column(width = 12,
           tabsetPanel(
             id = ns("guideTabs"),
             tabPanel(
               "Data preprocessing",
               value = "data_preprocessing",
               HTML("<p>You can upload either an OPUS (.0) file or a .CSV file</p>
               <br>
               <p>If you already have aggregated data, meaning a .csv file where all spectral scans are combined, with sample IDs and absorbance values
               organized by wavenumber, then go to the Aggregated CSV File tab to upload your file and begin preprocessing
               <br>
               <p>Detailed instructions for file formatting and uploading are provided in the Preprocessing tab.
               <p>If you are unsure about the quality of your spectral data, we recommend enabling Outlier Detection by checking the corresponding box. Outliers are identified based on spectral similarity in PCA space, and any spectra falling outside the 96% similarity threshold are automatically excluded before preprocessing.
                     <ul style = 'font-family: Arimo; font-size: 18px;'>
                            <li>
                              <strong>Aggregated Data</strong><br>
                              All OPUS (.0) or .CSV files are combined into a single dataset.<br>
                              You can download this aggregated data for your own use.
                            </li>
                            <li>
                              <strong>Filtered Data</strong><br>
                              The aggregated file is automatically processed.<br>
                              Any scans containing negative values are removed in this step.
                            </li>
                            <li>
                              <strong>Averaged Data</strong><br>
                              Since each soil sample is scanned multiple times to create replicates, this step averages the scans to produce a single representative spectrum.<br>
                              <em>Ensure your sample IDs follow the required format (e.g. sampleID_rep, where rep is a numeric replicate number)</em>
                            </li>
                            <li>
                              <strong>Savitzky Golay smoothing</strong>
                            </li>
                            <li>
                              <strong>Resampling</strong>
                            </li>
                            <li>
                              <strong>Baseline correction</strong>
                            </li>
                            </ul>
                          </ol>
                          <p style='font-family: Times New Roman; font-size: 18px;'>
                            We recommend processing up to the baseline correction step for optimal results.<br>
                            To use our built-in static models, you must complete all preprocessing steps and resample the spectra at every 10 cm<sup>-1</sup>.
                          </p>
                          
                          <p style='font-family: Times New Roman; font-size: 18px;'>
                            If you plan to use our built-in models, you don’t need to follow each preprocessing step manually.
                            Simply upload your OPUS or .CSV files and click \"Download Processed Data\" (located just below the data upload button on the left).
                          </p>")
             ),
             tabPanel(
               "Static Models",
               value = "static_models",
               HTML("
                            <h3>Static Models Overview</h3>
                            <p>
                              Our static models were developed using the NRCS-KSSL spectral dataset, a legacy database containing over 80,000 soil samples and their corresponding spectra. These models allow you to predict 19 soil properties that include physical, chemical, and biological properties.
                            </p>
                            <br>
                            <h4>Machine Learning Models Used</h4>
                            <p>
                              The static models were developed using five advanced machine-learning techniques:
                            </p>
                            <ul>
                              <li><strong>Partial Least Squares (PLS)</strong> – A robust regression method commonly used for spectral data analysis.</li>
                              <li><strong>Random Forest (RF)</strong> – A tree-based ensemble model that improves accuracy by averaging multiple decision trees.</li>
                              <li><strong>Cubist</strong> – A rule-based predictive modeling approach that enhances decision tree methods.</li>
                              <li><strong>Support Vector Machine (SVM)</strong> – A powerful algorithm for classification and regression tasks.</li>
                              <li><strong>Convolutional Neural Network (CNN)</strong> – A deep learning model designed for complex pattern recognition in spectral data.</li>
                            </ul>
                            
                            <p>
                              Our models are tailored to various soil and environmental conditions, ensuring adaptability across different landscapes. You can select models based on: <strong>Soil Order, Texture Classes, MLRA (Major Land Resource Area), Land Use &amp; Land Cover (LULC), Sample Depths</strong>.<br>
                              In addition to condition-specific models, we also provide <strong>Global Models</strong>—trained using the entire NRCS-KSSL dataset—to offer generalized predictions.
                            </p>
                            <br>
                            <h4>Data Input</h4>
                            <p>
                              To use the static models, upload a .CSV file containing:
                            </p>
                            <ul>
                              <li>A Sample ID column</li>
                              <li>Preprocessed spectral data resampled at every 10 cm<sup>-1</sup></li>
                            </ul>
                            <p>
                              Below is an example of the required CSV format:
                            </p>
                            <table style='width: 100%; border-collapse: collapse; font-family: Times New Roman; font-size: 18px;'>
                              <thead>
                                <tr>
                                  <th style='border: 1px solid #ddd; padding: 8px;'>Column</th>
                                  <th style='border: 1px solid #ddd; padding: 8px;'>Description</th>
                                </tr>
                              </thead>
                              <tbody>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Sample ID</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Unique identifier for each soil sample</td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>4000</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Spectral data at 4000 cm<sup>-1</sup></td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>3980</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Spectral data at 3980 cm<sup>-1</sup></td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>…</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>…</td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>600</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Spectral data at 600 cm<sup>-1</sup></td>
                                </tr>
                              </tbody>
                            </table>
                            <p>
                              If your spectral data is not yet preprocessed, use the <strong>Data Preprocessing</strong> tab in the web tool to generate a compatible preprocessed .CSV file. Compress your OPUS (.0) or .CSV files into a .zip, and upload it in the Data Preprocessing tab, click \"Download Processed Data\" (located just below the data upload button on the left), and then use the downloaded file for static model predictions.
                            </p>
                            <br>
                            <h4>Generate Soil Property Predictions</h4>
                            <p>
                              In the left sidebar, choose the soil property you want to predict. From the Machine Learning drop-down menu, select your preferred model.
                            </p>
                            <p>
                              <strong>Choose a Model Type</strong> – Use the radio buttons to select:
                              <br>&bull; Global Models (trained on all available data)
                              <br>&bull; Stratified Models (Soil Order, Texture Class, MLRA, LULC, Sample Depth)
                            </p>
                            <p>
                              If you choose a stratified model, select the appropriate category from the drop-down menu based on the stratification. (If a specific stratification is not available, it means the legacy dataset did not have enough data to develop a machine learning model for that category.)
                            </p>
                            <br>
                            <h4>View and Download Predictions</h4>
                            <p>
                              Predictions will be displayed at the bottom of the page. Click the Download button to save the predictions and metadata.
                            </p>
                            <br>
                            <h4>How to Choose the Right Model for Your Data</h4>
                            <p>
                              To help you select the most suitable model, we offer several tools for evaluating model performance and data compatibility:
                            </p>
                            <ul>
                              <li>
                                <strong>Review Model Performance</strong><br>
                                <em>Error Matrices</em>: Located at the top left of the page, these matrices display the validation results using an independent dataset to help you assess model accuracy. (The static models were trained using 70% of the data and tested on the remaining 30%. The accuracy matrix reflects the model's performance on the validation set.)
                              </li>
                              <li>
                                <strong>Check Spectral Compatibility</strong><br>
                                <em>Spectral Plots</em>: These plots overlay your spectral data with the spectral data from the selected model, allowing you to visually assess compatibility.<br>
                                <em>PCA Plots</em>: Principal Component Analysis (PCA) plots compare your data with the dataset used to train the model. If your data points are far from the KSSL data in the PCA plot, consider selecting a different model for better predictions.
                              </li>
                              <li>
                                <strong>Explore Dataset Descriptive Statistics</strong><br>
                                Navigate to the Descriptive Statistics tab to view summary statistics of the dataset used to build the model. By default, this tab displays statistics for the selected model. To compare multiple models, click \"See All Data\".
                              </li>
                              <li>
                                <strong>Check Spatial Information</strong><br>
                                The map shows the spatial location of the training data. Clicking on any point displays detailed information, including taxonomic data.
                              </li>
                            </ul>
                          ")
             ),
             tabPanel(
               "Customized Models",
               HTML("
                            <h3>Customized Models Overview</h3>
                            <p>
                              If you have spectral data and want to develop machine learning models using legacy data with spectra similar to your local data, the Customized Model tab is the ideal option for you. This feature allows you to refine and personalize the built-in models by leveraging the similarity of your spectral data to legacy data.
                            </p>
                            <br>
                            <h4>How It Works</h4>
                            <p>
                              The Customized Model tab uses the K-Nearest Neighbor (KNN) algorithm in the Principal Component Analysis (PCA) space. This method identifies spectra from the legacy dataset that are similar to your local spectral data. These “neighboring” spectra are then used to adjust and customize the machine learning models, improving the predictions for your specific dataset.
                            </p>
                            <br>
                            <h4>Available Machine Learning Models for Customization</h4>
                            <p>
                              Once similar spectra are identified, you can customize the following models:
                            </p>
                            <ul>
                              <li>Partial Least Squares (PLS)</li>
                              <li>Random Forest (RF)</li>
                              <li>Cubist</li>
                              <li>Support Vector Machine (SVM)</li>
                              <li>Convolutional Neural Network (CNN)</li>
                            </ul>
                            
                            <h4>Customization Options</h4>
                            <p>
                              You can choose how to select similar spectra based on the following parameters:
                            </p>
                            <ul>
                              <li><strong>Distance-based Selection:</strong> Neighbors are selected based on their proximity to your data in the PCA space.</li>
                              <li><strong>Number of Neighbors:</strong> You can specify the exact number of similar spectra (neighbors) to use in the customization process.</li>
                            </ul>
                            <p>
                              By adjusting these settings, you can fine-tune the model to better suit your local data, enhancing prediction accuracy. For datasets with few samples, we recommend using the Distance-based Selection. Due to the nature of the data, we are unable to provide validation metrics for this feature.
                            </p>
                            <br>
                            <h4>Data Input</h4>
                            <p>
                              The data input process is identical to that of the static models. Upload a .CSV file containing:
                            </p>
                            <ul>
                              <li>A Sample ID column</li>
                              <li>Preprocessed spectral data resampled at every 10 cm<sup>-1</sup></li>
                            </ul>
                            <p>Below is an example of the required CSV format:</p>
                            <table style='width:100%; border-collapse: collapse; font-family: Times New Roman; font-size: 18px;'>
                              <thead>
                                <tr>
                                  <th style='border: 1px solid #ddd; padding: 8px;'>Column</th>
                                  <th style='border: 1px solid #ddd; padding: 8px;'>Description</th>
                                </tr>
                              </thead>
                              <tbody>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Sample ID</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Unique identifier for each sample</td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>4000</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Spectral data at 4000 cm<sup>-1</sup></td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>3980</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Spectral data at 3980 cm<sup>-1</sup></td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>…</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>…</td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>400</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Spectral data at 400 cm<sup>-1</sup></td>
                                </tr>
                              </tbody>
                            </table>
                            <p>
                              If your spectral data is not yet preprocessed, use the <strong>Data Preprocessing</strong> tab in the web tool to generate a compatible preprocessed .CSV file. Compress your OPUS (.0) or .CSV files into a .zip file, and upload it in the Data Preprocessing tab. Then click \"Download Processed Data\" (located just below the data upload button on the left), and use the downloaded file for predictions.
                            </p>
                            <br>
                            <h4>Generate Your Soil Property Predictions</h4>
                            <p>
                              In the card on the top right, select your soil property from the “Select Soil Property …” dropdown. Choose a Model Type – use the dropdown to select which machine learning model you would like to train for your data. Then, choose how to select your Neighbors – use the radio buttons and the slider input to select by distance or by number of neighbors per PCA point. The more neighbors you select, the longer the model will take to train and predict.
                            </p>
                            <p>
                              Once you click “Make Prediction”, the model will begin training and, once finished, will display predictions in the bottom left. You may download the predictions by clicking “Download Predictions”.
                            </p>
                          ")
             )
             ,
             tabPanel(
               "Build your own machine learning model",
               value = "byo_model",
               HTML("
                            <h3>Build Your Own Machine Learning Model</h3>
                            <p>
                              The Build Your Own Model tab allows you to develop machine learning models using your own local spectral library.
                            </p>
                            <br>
                            <h4>How It Works</h4>
                            <p>
                              You can upload your own spectral data, define the response variable (measured soil property values), and build a machine-learning model that suits your needs. The preprocessing steps—including resampling—can be customized to your preference. You can perform resampling of your spectral data at every 10 cm<sup>-1</sup> or choose another interval that fits your needs. We recommend following all the spectral preprocessing steps for best results.
                            </p>
                            <br>
                            <h4>Input Data Requirements</h4>
                            <p>
                              To build a machine learning model, your input data should include the following:
                            </p>
                            <ul>
                              <li><strong>Sample ID:</strong> A unique identifier for each sample.</li>
                              <li><strong>Measured Soil Property Value (Response Variable):</strong> This is the target variable you want to predict and must be included in your dataset.</li>
                              <li><strong>Preprocessed Spectral Data:</strong> Ensure that your spectral data has been preprocessed.</li>
                            </ul>
                            <p>
                              We expect to implement functionality to combine preprocessed data from the data aggregation page with a separate calc_value file soon, but for now this process must be done manually (see flow chart in “Which Tool is Right for Me?”).
                            </p>
                            <br>
                            <h4>Training the Model</h4>
                            <p>
                              The Build Your Own Model tab allows you to train machine learning models using your own spectral data and measured soil property values. Follow the steps below to build and optimize your model:
                            </p>
                            <ol>
                              <li>
                                <strong>Upload Your Data</strong><br>
                                - Ensure your spectral data is preprocessed before uploading.<br>
                                - Upload a dataset that includes both spectral data and measured soil property values (response variable).
                              </li>
                              <li>
                                <strong>Specify the Response Variable</strong><br>
                                - Identify the response variable (the soil property you want to predict) by typing its column name into the web tool.
                              </li>
                              <li>
                                <strong>Select a Machine Learning Algorithm</strong><br>
                                - Choose a model from the <em>Select Model Type</em> dropdown menu:
                                <ul>
                                  <li>Partial Least Squares (PLS)</li>
                                  <li>Random Forest (RF)</li>
                                  <li>Cubist</li>
                                  <li>Support Vector Machine (SVM)</li>
                                  <li>Convolutional Neural Network (CNN)</li>
                                </ul>
                              </li>
                              <li>
                                <strong>Dimensionality Reduction (Optional)</strong><br>
                                - If you choose RF, SVM, or Cubist, you may reduce data dimensions using PCA (Principal Component Analysis).<br>
                                - <em>Why Use PCA?</em> It enhances training speed and efficiency.<br>
                                - <em>When NOT to Use PCA:</em> PCA is not recommended for CNN and PLS models, as it negatively impacts their performance.<br>
                                - <em>How to Apply PCA:</em> If using RF, SVM, or Cubist, check the \"Train with PCA\" box before training.
                              </li>
                              <li>
                                <strong>Train the Model</strong><br>
                                - Click <em>Train Model</em> to start the training process.<br>
                                - A progress bar will indicate the training status.
                              </li>
                              <li>
                                <strong>Review Model Performance</strong><br>
                                Once training is complete, you can analyze:
                                <ul>
                                  <li>Model Plot: A visual representation of training performance.</li>
                                  <li>PCA Plot: Shows how your data aligns with the trained model.</li>
                                  <li>Calibration/Validation Error Metrics: Assess model accuracy.</li>
                                </ul>
                              </li>
                              <li>
                                <strong>Download the Model</strong><br>
                                - Click <em>Download Model</em> to save your trained model.<br>
                                - If you trained with PCA, a ZIP file will be downloaded containing both the trained model and the PCA transformation file.
                              </li>
                            </ol>
                            
                            <h4>Using the Model</h4>
                            <p>
                              Below the model training section, there is a box that allows you to upload a .rds, or .keras/.h5 model (for CNN). If PCA was used during training, click the <em>Use PCA</em> box and upload the .rds PCA model as well. Finally, upload your data file. This should be in a similar format to the files uploaded in the static models—ensure your data file matches the expected format of your model (for example, if your model was trained using data resampled at 50 cm<sup>-1</sup>, ensure your spectral data matches that). Also, ensure you have a Sample ID column at the beginning.
                            </p>
                            <p>
                              Once this is done, click <em>Run Custom Model</em> and your predictions will appear in the results table to the right. You may download the predictions by clicking the <em>Download Predictions</em> button.
                            </p>
                          ")
             )
             
             
           )
    ),
    # column(width = 6,
    #        tags$figure(
    #          style = "float: right; margin: 10px;",
    #          tags$img(src = "spec.png",
    #                   style = "max-width: 100%; border-radius: 8px;")
    #        )
    # )
  )
),


# ---- Contact tab ------------------------------------------------------------
      nav_panel(
        "Contact",
        value = "contact",
        tags$head(
          tags$style(HTML("
    /* make email links look like the surrounding text */
    .contact-card a {
      color: #1a0dab !important;          /* black */
      text-decoration: none;           /* optional: removes underline */
    }
    .contact-card a:hover,
    .contact-card a:focus {
      text-decoration: underline;      /* keep a cue on hover/focus */
    }
  "))
        ),
        
        title = "Contact",
        
        # 1.  One flex-box container that can wrap
        tags$div(
          class = "d-flex flex-wrap gap-4",     # flex row + wrap + 1 rem gaps
          
          # 2.  A small card for every person ─ copy-paste / loop as needed
          tags$div(
            class = "contact-card",             # optional hook for extra CSS
            h5("Elliott Demain"),
            h6("Undergraduate Research Assistant – Oregon State University"),
            p( tags$a(href = "mailto:demaine@oregonstate.edu",
                      "demaine@oregonstate.edu") )
          ),
          
          tags$div(
            class = "contact-card",
            h5("Malithi Weerasekara"),
            h6("PhD Student & Research Assistant – Oregon State University"),
            p( tags$a(href = "mailto:weerasem@oregonstate.edu",
                      "weerasem@oregonstate.edu") )
          ),
          
          tags$div(
            class = "contact-card",
            h5("Dr. Yakun Zhang"),
            h6("Assistant Professor – Oregon State University"),
            p( tags$a(href = "mailto:yakun.zhang@oregonstate.edu",
                      "yakun.zhang@oregonstate.edu") )
          ),
          
          tags$div(
            class = "contact-card",
            h5("Dr. Jonathan Maynard"),
            h6("Research Soil Scientist – USDA NRCS"),
            p( tags$a(href = "mailto:Jonathan.Maynard@usda.gov",
                      "Jonathan.Maynard@usda.gov") )
          ),
          
          tags$div(
            class = "contact-card",
            h5("Dr. Alfred Hartemink"),
            h6("Professor – University of Wisconsin-Madison"),
            p( tags$a(href = "mailto:hartemink@wisc.edu",
                      "hartemink@wisc.edu") )
          )
        )
      ),
      nav_spacer(),
      navbarMenu(title = "Links",
                nav_item(
                  tags$a(
                    "Kellogg Soil Survey Laboratory",
                    href = "https://www.nrcs.usda.gov/conservation-basics/natural-resource-concerns/soil/kellogg-soil-survey-laboratory-kssl",
                    target = "_blank"
                  )
                )
               
               )
               
    )
  )
}

homepageServer <- function(id, shared) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$whichToolFlow <- renderGrViz({
        DiagrammeR::grViz("
                digraph flowchart {
                  # Set left-to-right layout
                  
                  
                  graph [bgcolor = 'transparent', rankdir = TB, splines = line]
                  
                  # Node defaults
                  node [shape = rectangle,
                        style = filled,
                        fillcolor = lightcyan,
                        color = black,
                        fontsize = 24,
                        width = 16,
                        height = 4];
                        
                  #force 'raw' and 'soil' to be in same row
                  { rank = same; raw; soil }
                  
                          { rank = same;  preproc; combine }
                  
                          {rank = same; builtIn; custom; buildOwn}
                          {rank = same; decisionBuiltIn; decisionCustom}
                  
                  # Define nodes
                  raw       [label = 'Raw Opus or .CSV files\\nfrom the MIR spectrometer', fillcolor = '#bae3c5'];
                  prep      [id = 'prep', label = 'Data preprocessing', fillcolor = '#e0bcad', tooltip='Click me'];
                  preproc   [label = 'Preprocessed MIR spectra', fillcolor = 'transparent', color = '#547838'];
                  resample  [label = 'Preprocessed MIR spectra data\\nresampled by every 10 cm^-1', fillcolor = 'transparent', color = '#547838'];
                  soil      [label = 'Soil property measurements', fillcolor = '#bae3c5'];
                  combine   [label = 'Combine preprocessed spectral data\\nwith local soil property measurement', 
                  shape = diamond, 
                  width = 16, 
                  height = 4, 
                  fixedsize = TRUE, 
                  fillcolor = '#e3d888'];
                  
                  builtIn   [id = 'builtIn', label = 'Static models', fillcolor = '#e0bcad', tooltip='Click me'];
                  custom    [id = 'custom', label = 'Customized models', fillcolor = '#e0bcad', tooltip='Click me'];
                  buildOwn  [id = 'buildOwn', label = 'Build your own machine learning model\\nwith local spectral libraries', fillcolor = '#e0bcad', tooltip='Click me'];
                  
                  decisionBuiltIn [
                    label = 'Want to use built in models\nto predict soil properties?',
                    shape = diamond, 
                    style = filled,
                    fillcolor = '#e3d888',
                    width = 16,
                    height = 4,
                    fixedsize = TRUE
                  ]
                  
                  decisionCustom [
                    label = 'Customize models to your local data\nusing legacy data and predict soil properties?',
                    shape = diamond, 
                    style = filled,
                    fillcolor = '#e3d888',
                    width = 16,
                    height = 4,
                    fixedsize = TRUE
                  ]
                  
                  # Define edges
                  raw -> prep -> preproc -> resample[minlen = 10];
                  preproc -> combine [minlen = 80];
                  soil -> combine;
                  resample:s -> decisionBuiltIn[arrowhead = none, minlen = 10];
                  decisionBuiltIn:sw -> builtIn [minlen = 10];
                  resample:s -> decisionCustom[arrowhead = none, minlen = 10];
                  decisionCustom:se -> custom [minlen = 10];
                  combine -> buildOwn;
                  
                  #INVISIBLE EDGE to add space between builtin and custom
                  builtIn -> custom [style = invis, arrowhead = none, minlen = 24];
                  
                  decisionBuiltIn -> decisionCustom [style = invis, arrowhead = none, minlen = 6];
                }
                ",
                          height = "600px",
                          width  = "1200px")
      })
      
      
      observeEvent(input$whichToolFlow_click, {
        click <- input$whichToolFlow_click
        
        req(click$id, length(click$id) >= 1)
        
        node_id <- click$id[[1]]
        
        if (node_id == "prep") {
          shared$click_data_preproc <- TRUE
        } else if (node_id == "builtIn") {
          shared$click_static_models <- TRUE
        } else if (node_id == "custom") {
          shared$click_custom_models <- TRUE
        } else if (node_id == "buildOwn") {
          shared$click_byo_models <- TRUE
        }
      })

      
      # observeEvent(shared$change_guide, {
      #   req(isTRUE(shared$change_guide))
      #   session$onFlushed(function() {
      #     showNotification("🛠️ jumping to User Guide tabs…", type = "message")
      #     updateTabsetPanel(session, "helpNav",   selected = "user_guide")
      #     updateTabsetPanel(session, "guideTabs", selected = "data_preprocessing")
      #   }, once = TRUE)
      #   shared$change_guide <- FALSE
      # })
    })
}



