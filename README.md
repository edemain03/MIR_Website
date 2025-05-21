# MIR Website

This repository holds **only the source code** for the MIR (Mid-Infrared) Soil Analytics Shiny application.  
The models and raw spectral datasets are **not** included because they exceed GitHub’s standard file-size limits.

## What’s here
* `/ShinyApp/` – modular R Shiny code for the web interface  
* `/modules/`   – server / UI modules for data input, aggregation, model selection, etc.  
* `main.R` – app entry point 
* Misc. scripts & helper functions

## What’s **not** here
* Pre-trained model objects (≈ hundreds of MB each)  
* Large MIR spectral training sets (multi-GB)  

These files are stored off-repo (institutional cloud storage).
