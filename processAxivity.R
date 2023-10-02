# This script was used for processing the Axivity .cwa files (as described in section 2.7.2 of the article)
# User input
# Path to the original data files of measurement period 1
filepath.axivity <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1"

### Structure the Axivity files into 5-sec epoch data including:
## 1) data quality check;
## 2) the metrics roll_med_acc for x, y and z axis, ENMO and MAD;
## 3) indicator for non-wear detection.
load.structure.axivity(filepath = paste0(filepath.axivity, "/wave1"), outputdir = outputdir, processeddir = paste0(outputdir, "/output_wave1")) 
load.structure.axivity(filepath = paste0(filepath.axivity, "/wave2"), outputdir = outputdir, processeddir = paste0(outputdir, "/output_wave2")) 

### Remove data detected as non-wear from the 5-sec epoch data

# To discuss: exclude non-wear data only, or additional non-wear too?
