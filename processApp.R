# This script was used for processing the My Little Moves app data (as described in section 2.7.1 of the article)

# User input
date <- "20230918"
datadir.app <- paste0("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/App-data/", date)
datadir.castor <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Castor export/20231005/Study" 
filename.castor <- "My_Little_Moves_export_20231005.csv"

# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/AxivityMLMapp/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

### Structure MLM app data in the following steps:
## 1) .json file of the app data, milestoned into .csv files: each row represents an acitivity entry and the follow-up questions
## 2) Link the Castor ID to the MLM app data using the unique research codes
## 3) Calculate time spent in activity categories and label the movement behavior. 
#     Then calculate time spent and frequency in all activity categories per day for all participants

## Unnest the .json files and save as .csv 
unnest.json(datadir.app, date)

## Link Castor ID to the MLM app data
link.app.castor(datadir.app, datadir.castor, filename.castor, date)

## Restructure data per participant per day
categorize.structure.activities(datadir.app, date)
  

