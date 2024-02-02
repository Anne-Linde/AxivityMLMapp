# This script was used for processing the Axivity .cwa files (as described in section 2.8.1.2 of the article)

## 1) Load raw data and convert into 5-sec epoch data;
## 2) Remove periods of non-wear;

rm(list=ls())
gc()

# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/AxivityMLMapp/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

## User input  
# Path to the original data files of measurement period 1
filepath.axivity <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1"
#outputdir <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/5sec"
outputdir <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/rerun"

filepath.castor <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Castor export"
filename.castor <- "/20231007/Study/My_Little_Moves_export_20231007.csv"
data.castor <- load.castor(filepath.castor, filename.castor, cohort = c(3))
# Path to the directory where the epochdata was saved
#epochdir <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/5sec/epochdata"
epochdir <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/rerun/epochdata"
epochdir.hip <- paste0(epochdir, "/hip")  
epochdir.wrist <- paste0(epochdir, "/wrist")  
savefolder <- "/nonwear_removed/"

# Structure raw files into 5-sec epochdata
load.structure.axivity(filepath = paste0(filepath.axivity, "/wave1"), outputdir = outputdir, validhours = 12, processeddir = paste0(outputdir, "/output_wave1")) 
load.structure.axivity(filepath = paste0(filepath.axivity, "/wave2"), outputdir = outputdir, validhours = 12, processeddir = paste0(outputdir, "/output_wave2")) 

### Remove data detected as non-wear from the 5-sec epoch data
remove.nonwear(epochdir.hip, epochdir.wrist, savefolder, data.castor)
