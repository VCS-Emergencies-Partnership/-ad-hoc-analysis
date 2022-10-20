# Ad Hoc 

## Overview
This repo is a space for the VSCEP/RBC insight team to run ad hoc analyses. Each ad hoc analysis should be a self contained folder in the `analysis/` directory. All old files have been moved to depreciated, keeping the old folder structure intact.


## Analysis

### Data quality

This folder contains code to run [pandas profiling](https://pandas-profiling.ydata.ai/docs/master/index.html) code on a single file (or a set of files within a folder). It is currently for csv files but can be amended to also cover other types of files. It saves the pandas profiling reports for the datasets into a older. The `instructions` file contains instructions of how to run the code. 

If you wish to create a pandas profiling report on a csv file not using code Mike Page has created a Shiny app where can upload the file and the report outputs directly [here](https://github.com/MikeJohnPage/shiny-panda). 

### Local capability mapping mockup 

This contains code for a Shiny app created to demo what the local capability mapping project output could look like. The data used is mock data. 

### NFVI & SFRI testing
Code for data checks on data from [ClimateJust site](https://www.climatejust.org.uk/map) relating to the Social Flood Vulnerability Index & Neighbourhood Flood Vulnerability Index.  

### VCSEP regions boundaries

The VCSEP regional boundaries changed as Thames Valley area moved from the South East to the South West. Output is a Local Authority to VCSEP region lookup csv and spatial boundary files of VCSEP regions. 

### Other

#### EPT prototype images

Code to create images used to create an interactive protoype for the Emergency Planning Tool using [JustInMind](https://www.justinmind.com/). 

#### Scraping powercuts

Starter code for scraping data for powercut locations. Policies on scraping network websites need to be confirmed before use.  

#### Testing lookup methods

Ahead of new search functionality for geographical areas in future Shiny Apps testing time taken for different approaches of lookup methods (dplyr::filter, data.table, hashtables). 