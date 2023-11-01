# This script was used for processing the My Little Moves app data (as described in section 2.8.1.1 of the article)
#The MLM app data was structured in the following steps:
## 1) .json file of the app data, milestoned into .csv files: each row represents an acitivity entry and the follow-up questions
## 2) Link the Castor ID to the MLM app data using the unique research codes
## 3) Calculate time spent in activity categories and label the movement behavior. 
#     Then calculate time spent and frequency in all activity categories per day for all participants

rm(list = ls())
gc()
# User input
date <- "20231031"  #Date of today
date.app <- "20230918" # Date app export
date.castor <- "20231031" # Date castor export
datadir.app <- paste0("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/App-data/", date.app)
datadir.castor <- paste0("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Castor export/", date.castor, "/Study")
filename.castor <- paste0("My_Little_Moves_export_", date.castor, ".csv")

# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/AxivityMLMapp/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

## Unnest the .json files and save as .csv 
unnest.json(datadir.app, date)

## Link Castor ID to the MLM app data
link.app.castor(datadir.app, datadir.castor, filename.castor, date)

## Restructure data per participant per day
categorize.structure.activities(datadir.app, date)
  

