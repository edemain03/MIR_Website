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
             <p>Let MIR Soil-Analytics handle the modelling so that you can focus on results. 
             <br>
             <p>Need help choosing the right tool for your dataset? Use our decision tree to navigate the web tool efficiently. Visit <strong>Which Tool is Right for Me? </strong> to get started!
             ")
        ),

nav_panel(
  title  = "Which Tool is Right for Me?",
  value  = "which_tool",
  HTML("<h2>Which Tool is Right for Me?</h2>
       <p>Use the flowchart below to determine which tool is best suited for your data.</p>"),
  
  # NEW:  a flex-box that centres the chart and adds scrollbars if needed
  div(
    style = "
      max-width: 100%;
      overflow-x: auto;     /* horizontal scroll if the chart gets wider */
      overflow-y: auto;     /* vertical scroll for tall screens           */
    ",
    grVizOutput(ns('whichToolFlow'), height = '700px')   # a tad taller
  )
),


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
               HTML("<p>You can upload either an OPUS (.0) file or a .CSV file. Once uploaded, you can select the preprocessing steps you need.</p>
               <p> If you already have aggregated data, meaning a .csv file where all spectral scans are combined, with sample IDs and absorbance values
               organized by wavenumber, then go to the Aggregated CSV File tab to upload your file and begin preprocessing 
               <p> Detailed instructions for file formatting and uploading are provided in the Preprocessing tab.
               <p>If you are unsure about the quality of your spectral data, we recommend enabling Outlier Detection by checking the corresponding box. 
               Outliers are identified based on spectral similarity in PCA space, and any spectra falling outside the 96% similarity threshold are automatically excluded before preprocessing.
               <br>
               <p> To use the Static Models, select the <strong>Default</strong> Preprocessing option. 
               This option initiates a set of built-in steps designed to meet the requirements of the static prediction models. The following preprocessing steps are automatically applied:
               <li> Data aggregation, Filtering, and Spectral averaging </li>
               <li> Savitzky-Golay Smoothing – Applied using:<br>
                 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Window size (w) = 13 <br>
                 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Polynomial order (p) = 2 
               </li>
               <li>Resampling – Spectra are resampled at 10 cm⁻¹ intervals.</li>
               <li>Baseline correction/ Standard Normal Variate (SNV) transformation </li>
               (See details about these functions in the section below.)
               <p>If you would like preprocessing spectral data tailored to your needs, click on the <strong>custom</strong> preprocessing. 


                     <ul style = 'font-family: Arimo;'>
                            <li>
                              <span style='font-size: 18px;'><strong>Aggregated Data</strong></span><br>
                              All OPUS (.0) or .CSV files are combined into a single dataset. You can download this aggregated data for your own use.
                            </li>
                            <li>
                              <span style='font-size: 18px;'><strong>Filtered Data</strong></span><br>
                              The aggregated file is automatically processed. Any scans containing negative values are removed.
                            </li>
                            <li>
                              <span style='font-size: 18px;'><strong>Averaged Data</strong></span><br>
                              Since each soil sample is scanned multiple times, this step averages the scans to produce a single spectrum.<br>
                              <em>Ensure your sample IDs follow the required format (e.g. sampleID_rep, where rep is a numeric replicate number)</em>
                            </li>
                            <li>
                              <span style='font-size: 18px;'><strong>Savitzky Golay smoothing</strong> </span><br>
                              <u>Understanding the Savitzky-Golay Filter</u><br>
                              The Savitzky-Golay (SG) filter is a widely used smoothing technique for spectral data. It reduces noise while preserving important features like peaks and valleys, which are often distorted by simpler smoothing methods <br>
                              <u>How It Works</u> <br>
                              The SG filter moves a small window across the data and fits a polynomial curve to the values within that window. 
                              The center point is then replaced by the predicted value from the fitted curve. 
                              Repeating this process across the entire dataset produces a smoothed version of the signal.<br>
                              The filter depends on three parameters:
                              
                              
                              <table style='width: 50%; border-collapse: collapse;'>
                              <thead>
                                <tr>
                                  <th style='border: 1px solid #ddd; padding: 8px;font-size: 18px;'>Parameter</th>
                                  <th style='border: 1px solid #ddd; padding: 8px;font-size: 18px;'>Meaning</th>
                                </tr>
                              </thead>
                              <tbody>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Window size (w)</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Number of data points used in each local fit (must be an odd number). 
                                  A smaller window retains fine detail but smooths less. A larger window provides stronger smoothing but may flatten sharp features</td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Polynomial degree (p)</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Degree of the polynomial fitted within each window. It should be lower than w and typically 2 or 3.</td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Derivative order (m)</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Order of the derivative to compute. Use m = 0 for smoothing only.m = 1 for first derivative, and m = 2 for second derivative.</td>
                                </tr>
                                
                              </tbody>
                            </table>
                            <br>
                            <u>Best Practices</u> <br>
                            Start with the defaults and check if the result looks clean but not over-smoothed
                            Avoid using a high-degree polynomial with a small window, as this can overfit noise<br>
                            Use visual inspection to see whether fine-tuning is needed for modeling or interpretation<br>
                            The SG filter is an essential tool for improving signal quality while preserving meaningful structures of the spectral data.
                            By choosing the parameters, you can improve data for further analysis and modeling.</li> <br>
                            <li>
                              <span style='font-size: 18px;'><strong>Resampling</span></strong><br>
                              <u>Why Resample MIR Spectra?</u><br>
                              MIR spectra are often recorded at very high resolution, capturing absorbance values at hundreds or thousands of wavenumbers.
                              While this level of detail can be valuable, it also introduces noise, redundant information, and large file sizes. 
                              These could complicate data processing and slow down modeling workflows. Resampling addresses this by reducing the number of data points, and it selects absorbance values at regular intervals (e.g., every 10 cm⁻¹), while preserving the essential shape and features of the spectrum.
                              <br>
                              Keep in mind : If you resample at large intervals, you lose important spectral features that are critical for modeling or interpretation.
                            </li>
                            <li>
                              <span style='font-size: 18px;'><strong>Baseline correction</span></strong><br>
                              Standard Normal Variate (SNV) / Baseline Correction is a normalization technique used to correct unwanted variation in spectral data.
                              SNV transforms each spectrum so that it has a mean of 0 and a standard deviation of 1, making spectra more consistent and comparable.<br>
                              <u>Why Use SNV?</u><br>
                              When MIR spectra are collected, soil samples can scatter light differently because of surface texture, particle size, or packing. This often results in vertical shifts (offsets in absorbance) and slope differences across spectra. These variations are unrelated to the actual chemical composition and that is why SNV becomes useful <br>
                              SNV helps to reduce scatter effects, remove baseline offsets and slope variations, and make the spectra more comparable and interpretable.<br>
                              Improving signal clarity and consistency, SNV/baseline correction enhances the reliability of downstream analysis, and it is recommended preprocessing the MIR data up to the baseline correction step.
                            </li>
                            </ul>
                          </ol>
                          ")
             ),
             tabPanel(
               "Static Models",
               value = "static_models",
               HTML("
                            <h3>Static Models Overview</h3>
                            <p>
                             The static models were developed using the NRCS-KSSL spectral dataset, a database containing analytical data of over 80,000 soil samples and their corresponding MIR spectra.<br>
                             These models allow you to predict 21 soil properties:
                             <div style='display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; font-family: Arimo;'>
                               <ul style='list-style-type: disc; padding-left: 10px;'>
                               <li>Sand</li>
                               <li>Silt</li>
                               <li>Clay</li>
                               <li>Aggregate stability</li>
                               <li>Bulk Density</li>
                               <li>pH</li>
                               <li>Electrical conductivity</li>
                               </ul>
                               
                               <ul style='list-style-type: disc; padding-left: 10px;'>
                               <li>Total Carbon</li>
                               <li>Organic Carbon</li>
                               <li>Carbon (pom)</li>
                               <li>Carbon (hpom)</li>
                               <li>Carbon (pom mineral)</li>
                               <li>Total Nitrogen</li>
                               <li>Phosphorus (Olsen)</li>
                               </ul>
                               
                               <ul style='list-style-type: disc; padding-left: 10px;'>
                               <li>Phosphorus (Bray)</li>
                               <li>Phosphorus (Mehlich)</li>
                               <li>Total Sulfur</li>
                               <li>CEC</li>
                               <li>Potassium</li>
                               <li>Carbonate</li>
                               <li>Gypsum</li>
                               </ul>
                               </div>
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
                            Background information and examples using these machine learning methods can be found in the <em>References.</em> <br>
                            Our models are tailored to various soil and environmental conditions, and you can select one of the stratified models: 

                            Our models are tailored to various soil and environmental conditions, and you can select one of the stratified models:
                            <ul>
                            <li><strong>Soil Order </li>
                            <li>Texture Classes</li>
                            <li>MLRA (Major Land Resource Areas)</li>
                            <li>Land Use &amp; Land Cover (LULC)</li>
                            <li>Sample Depths</li></strong><br>
                              In addition to these models, we provide <strong>Global Models</strong> trained using the entire NRCS-KSSL dataset, which offers generalized predictions.
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
                            <table style='width: 50%; border-collapse: collapse;'>
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
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Absorbance at 4000 cm<sup>-1</sup></td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>3980</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Absorbance at  3980 cm<sup>-1</sup></td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>…</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>…</td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>600</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Absorbance at 600 cm<sup>-1</sup></td>
                                </tr>
                              </tbody>
                            </table>
                            <p>
                              If your spectral data is not preprocessed, use the <strong>Data Preprocessing</strong> tab in the web tool to generate a compatible preprocessed .CSV file. Compress your OPUS (.0) or .CSV files into a .zip, and upload it in the Data Preprocessing tab, click \"Download Processed Data\" (located just below the data upload button on the left), and then use the downloaded file for static model predictions.
                            </p>
                            <br>
                            <h4>Generate Soil Property Predictions</h4>
                            <p>
                              From the left sidebar, choose the soil property you want to predict. From the Machine Learning drop-down menu, select your preferred model.
                            </p>
                            <p>
                              <strong>Choose a Model Type</strong> – Use the radio buttons to select:
                              <br>&bull; Global Models (trained on all available data)
                              <br>&bull; Stratified Models (Soil Order, Texture Class, MLRA, LULC, Sample Depth)
                            </p>
                            <p>
                              If a stratified method is chosen, select the appropriate stratified model from the drop-down menu based on the stratification. 
                              If a specific stratification model is not available, it means the legacy dataset did not have enough data to develop a machine learning model for that category.
                            </p>
                            <br>
                            <h4>View and Download Predictions</h4>
                            <p>
                              Predictions will be displayed at the bottom of the page. Click the Download button to save the predictions and its metadata.
                            </p>
                            <br>
                            <h4>How to Choose the Right Model</h4>
                            <p>
                              To help you select the most suitable model, we offer several tools for evaluating model performance and data compatibility:
                            </p>
                            <ul>
                              <li>
                                <strong>Review Model Performance</strong><br>
                                <em>Error Matrices</em>: Located at the top left of the page, these matrices display the validation results using an independent dataset to help you assess model accuracy. The static models were trained using 70% of the data and tested on the remaining 30%. The accuracy matrix reflects the model's performance on the validation set.
                              </li>
                              <li>
                                <strong>Check Spectral Compatibility</strong><br>
                                <em>Spectral Plots</em>: These plots overlay your spectral data with the spectral data from the selected model, allowing visual comparison. <br>
                                <em>PCA Plots</em>: Principal Component Analysis (PCA) Plots compare your data with the dataset used to train the model. If your data points are far from the KSSL data in the PCA plot, consider selecting a different model for better predictions.
                              </li>
                              <li>
                                <strong>Explore Dataset Descriptive Statistics</strong><br>
                                Navigate to the Descriptive Statistics tab to view summary statistics of the dataset used to build the model. By default, this tab displays statistics for the selected model. To compare multiple models, click \"See All Data\".
                              </li>
                              <li>
                                <strong>Check Spatial Information</strong><br>
                                The map shows the location of the training data. Clicking on any point displays detailed information, including taxonomic data.
                              </li>
                            </ul>
                          ")
             ),
             tabPanel(
               "Customized Models",
               HTML("
                            <h3>Customized Models Overview</h3>
                            <p>
                              The Customized Model tab allows to develop machine learning models using legacy data with spectra similar to your local data. It refines and personalizes the built-in models by leveraging the similarity in your spectral data and legacy data.
                            </p>
                            <br>
                            <p>
                            <u>How It Works</u><br>
                            The Customized Model tab uses the K-Nearest Neighbor (KNN) algorithm in the Principal Component Analysis (PCA) space. It identifies spectra from the legacy dataset similar to your local spectral data. The neighboring spectra are then used to adjust and customize the machine learning models, improving the predictions.
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
                            <p>
                              You can choose how to select similar spectra based on the following parameters:
                            </p>
                            <ul>
                              <li><strong>Distance-based Selection: </strong> Neighbors are selected based on their proximity to your data in the PCA space.</li>
                              <li><strong>Number of Neighbors: </strong> You can specify the exact number of similar spectra (neighbors) to use in the customization process.</li>
                            </ul>
                            <p>
                              You can fine-tune the model to better suit your local data and improve the prediction accuracy. For datasets with few samples, we recommend using the Distance-based Selection, but the platform is unable to provide validation metrics for this feature. You can check the model internal calibration metric in the Calibration metric tab. 
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
                            <table style='width:50%; border-collapse: collapse;'>
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
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Absorbance at 4000 cm<sup>-1</sup></td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>3980</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Absorbance at  3980 cm<sup>-1</sup></td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>…</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>…</td>
                                </tr>
                                <tr>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>400</td>
                                  <td style='border: 1px solid #ddd; padding: 8px;'>Absorbance at  400 cm<sup>-1</sup></td>
                                </tr>
                              </tbody>
                            </table>
                            <p>If your spectral data is not yet preprocessed follow these steps: 
                            <ul>
                            <li> Use the <strong>Data Preprocessing</strong> tab – Default preprocessing step in the web tool to generate a compatible preprocessed .CSV file. </li>
                            <li> Compress your OPUS (.0) or .CSV files into a .zip file</li>
                            <li> Upload it in the Data Preprocessing tab </li>
                            <li> Click \"Download Processed Data\" (located just below the data upload button on the left), and use the downloaded file for predictions. </li>
                            </ul>

                            <h4>Generate Your Soil Property Predictions</h4>
                            <p>In the Train settings panel (bottom left, just below Data Input), start by selecting the soil property you're interested in from the dropdown menu.
                            Then, configure the neighbor selection by using the radio buttons and slider to choose either distance-based or choosing a fixed number of neighbors per PCA point<br>
                            
                            The PCA plot displays the legacy data, the input data you upload (overlaid on top of the legacy points),
                            and the subset of legacy points selected by the algorithm based on your neighbor selection. The more neighbors you select, the longer it will take for the model to train and predict<br>
                            Once the neighbors are selected, choose a Model Type using the corresponding dropdown to specify which machine learning model you'd like to train. 
                            Then click make prediction.<br>
                            Once you click “Make Prediction”, the model will begin training and once finished, will display predictions in the bottom left. You may download the predictions by clicking “Download Predictions”.</p>
                          ")
             )
             ,
             tabPanel(
               "Build your own machine learning model",
               value = "byo_model",
               HTML("
                            <h3>Build Your Own Machine Learning Model</h3>
                            <p>
                              The Build Your Model tab allows you to develop machine learning models using your local spectral library.
                            <br>
                            <br>
                            <u>How It Works</u> <br>
                            You can upload your spectral data, specify the response variable (e.g., measured soil property values), and build a machine learning model tailored to your needs. You also have the option to preprocess your spectral data using customizable settings. Visit the Data Preprocessing tab to get started
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
                           
                            <br>
                            <h4>Training the Model</h4>
                            <p>
                              The Build Your Model tab allows you to train machine learning models using your spectral data and measured soil property values. Follow the steps below to build and optimize your model:
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
                              <strong>Choose the data splitting method for calibration and validation.</strong><br>
                              -First, use the slider to select the percentage of data to be used for training the model.<br> 
                              -Then, choose the cross-validation method for internal model tuning.<br> 
                              &nbsp;&nbsp;You can choose between K-fold cross-validation and leave-one-out cross-validation. We recommend using K-fold cross-validation with 10 folds and 10 repetitions for a good balance between computational efficiency and accuracy.<br>
                              &nbsp;&nbsp;The best model is selected based on the lowest RMSE (Root Mean Square Error). 
                              
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
                                - <em>When NOT to Use PCA:</em> PCA is not recommended for CNN and PLS models. PCA is a built in function in PLS models and PCA can negatively performance of CNN models.<br>
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
                              Below the model training section, there is a box that allows you to upload a .rds, or .keras/.h5 model (for CNN). 
                              If PCA was used during training, click the Use PCA box and upload the .rds PCA model as well. Finally, upload your data file. 
                              This should be in a similar format to the files uploaded in the static models. 
                              Ensure your data file matches the expected format of your model (for example, if your model was trained using data resampled at 50 cm-1, ensure your spectral data matches that). 
                              Also, ensure you have a Sample ID column at the beginning. Once this is done, click Run Custom Model , and your predictions will appear in the results table to the right. You may download the predictions by clicking the Download Predictions button.
                              </p>
                            <p>
                              Once this is done, click <em>Run Custom Model</em> and your predictions will appear in the results table to the right. You may download the predictions by clicking the <em>Download Predictions</em> button.
                            </p>
                          ")
             )
             
             
           )
    )
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
            class = "contact-card",             
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
                ),
                nav_item(
                  tags$a(
                    "USDA Agriculture Handbook (PDF)",
                    href     = "AgHandbook296_text_low-res.pdf",   # www/static_model_sop.pdf
                    target   = "_blank",                 # open in a new tab
                    download = NA                        # …and offer Save-as
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
        DiagrammeR::grViz(sprintf('
    digraph flowchart {
      # --------------------------  GLOBAL GRAPH  --------------------------
      graph [
        bgcolor  = "transparent",
        rankdir  = TB,
        splines  = line,
        fontname = "Arimo",         # <- matches bs_theme()
        fontsize = 32               # <- base ==> node labels inherit this
      ]

      # --------------------------  NODE DEFAULTS --------------------------
      node [
        shape     = rectangle,
        style     = filled,
        fillcolor = lightcyan,
        color     = black,
        fontname  = "Arimo",
        fontsize  = 32,             # 32 pt everywhere
        width     = 7.8,            # 30 %% wider (was 6)
        height    = 1.3,            # 30 %% taller (was 1)
        fixedsize = true
      ]

      # keep original rank groups so spacing ratios stay unchanged ----------
      { rank = same; raw; soil }
      { rank = same; preproc; combine }
      { rank = same; builtIn; custom; buildOwn }
      { rank = same; decisionBuiltIn; decisionCustom }

      # --------------------------  NODES -----------------------------------
      raw     [label = "Raw Opus or .CSV files\\nfrom the MIR spectrometer",
               fillcolor = "#bae3c5"];
      prep    [id = "prep", label = "Data preprocessing",
               fillcolor = "#e0bcad", tooltip = "Click me" ];
      preproc [label = "Preprocessed MIR spectra",
               fillcolor = "transparent", color = "#547838"];
      resample[label = "Preprocessed MIR spectra data\\nresampled by every 10 cm^-1",
               fillcolor = "transparent", color = "#547838"];
      soil    [label = "Soil property measurements", fillcolor = "#bae3c5"];

      combine [label = "Combine preprocessed spectral data\\nwith local soil property measurement",
               shape = diamond, width = 12, height = 3,   # 30 %% bump
               fillcolor = "#e3d888"];

      builtIn  [id = "builtIn",  label = "Static models",
                fillcolor = "#e0bcad", tooltip = "Click me"];
      custom   [id = "custom",   label = "Customized models",
                fillcolor = "#e0bcad", tooltip = "Click me"];
      buildOwn [id = "buildOwn", label = "Build your own machine learning model\\nwith local spectral libraries",
                fillcolor = "#e0bcad", tooltip = "Click me"];

      decisionBuiltIn [
        label = "Want to use built-in models\\nto predict soil properties?",
        shape = diamond,
        width = 12, height = 3,
        fillcolor = "#e3d888"
      ]

      decisionCustom [
        label = "Customize models to your local data\\nusing legacy data and predict soil properties?",
        shape = diamond,
        width = 12, height = 3,
        fillcolor = "#e3d888"
      ]

      # --------------------------  EDGES -----------------------------------
      raw -> prep -> preproc -> resample;
      preproc -> combine   [minlen = 40];
      soil    -> combine;

      resample:s -> decisionBuiltIn [arrowhead = none, minlen = 4];
      decisionBuiltIn:sw -> builtIn [minlen = 4];

      resample:s -> decisionCustom [arrowhead = none, minlen = 4];
      decisionCustom:se -> custom  [minlen = 4];

      combine -> buildOwn;

      builtIn  -> custom        [style = invis, arrowhead = none, minlen = 30];
      decisionBuiltIn -> decisionCustom [style = invis, arrowhead = none, minlen = 6];
    }
  '))
      })
      
      # Logic to handle clicks on flowchart nodes, updates relevant shared value
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

    })
}



