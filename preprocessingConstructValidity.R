## This script was used to preprocess the data for construct validity analyses (as described in section 2.4.3.1 of the article):
# 1) Synchronize the Axivity data to the app entries 
# 2) Report the duration, frequency and acceleration per activity category over all reported categories and 24-h movement behaviors

rm(list = ls())
gc()

## User input 
date <- "20231031" # Date of last data update
tz = "Europe/Amsterdam"
# Use preprocessed Axivity data (data without non-wear)
filepath.hip <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/rerun/epochdata/hip/nonwear_removed"
filepath.wrist <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/rerun/epochdata/wrist/nonwear_removed"
# Use app data activities
filepath.app <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/App-data/20230918"
filename.app <- paste0("/", date, "_activities_castor_linked_duration.csv")
filename.app.day <- paste0("/", date, "_MLMapp_pp_duration_frequency_day.csv")

# Path to directory to save the data list
savedir <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output"

# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/AxivityMLMapp/R"
for (function_file in dir(my_functions_folder, full.names = TRUE)) source(function_file) #load functions

## Load app data
data.app <- load.app(filepath.app, filename.app, cohort = c(3), measurementperiod = 1, sep = ",") # Load app data
data.app.day <- read.csv(paste0(filepath.app, filename.app.day))[which(data.app$castorID %in% unique(data.app$castorID)),-1]
data.app.day <- data.app.day[which(data.app.day$measurement == 1),]

#### STEP 1: Match the axivity data to the app entries
#run L22 once: then load in data from saved file
#data.app.axivity <- match.app.axivity(data.app, filepath.hip, filepath.wrist, tz, date)
load("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output/app_ax_entry_20231031.RData")

## Plot the synchronized data in overlay plots per day
for(pp in 1:length(unique(data.pp$castorID))){
  create.overlayplot(data.app, filepath.hip, filepath.wrist, unique(data.pp$castorID)[pp], savedir)
}

#### STEP 2: Descriptives duration, frequency and acceleration per reported activity category and movement behavior
### Descriptives for the activity categories
data.pp$activity <- as.factor(data.pp$activity)
library(dplyr)
desc_activity <- data.pp %>% group_by(activity) %>% 
  summarise(dur = median(total_duration, na.rm = TRUE),
            freq = n(),
            per =  n() / nrow(data.pp) * 100,
            ENMO.hip = median(ENMO.hip, na.rm = TRUE),
            MAD.hip = median(MAD.hip, na.rm = TRUE),
            ENMO.wrist = median(ENMO.wrist, na.rm = TRUE),
            MAD.wrist = median(MAD.wrist, na.rm = TRUE)
  )

## IQR per activity category 
# Sleeping
quantile(data.pp$total_duration[which(data.pp$activity == "sleeping")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sleeping")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sleeping")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sleeping")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sleeping")], na.rm = TRUE)
# Sitting/lying
quantile(data.pp$total_duration[which(data.pp$activity == "sittinglying")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sittinglying")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sittinglying")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sittinglying")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sittinglying")], na.rm = TRUE)
# Personal care
quantile(data.pp$total_duration[which(data.pp$activity == "personalcare")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "personalcare")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "personalcare")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "personalcare")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "personalcare")], na.rm = TRUE)
# Eating/drinking
quantile(data.pp$total_duration[which(data.pp$activity == "eatingdrinking")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "eatingdrinking")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "eatingdrinking")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "eatingdrinking")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "eatingdrinking")], na.rm = TRUE)
# Passive screen use
quantile(data.pp$total_duration[which(data.pp$activity == "passivescreen")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "passivescreen")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "passivescreen")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "passivescreen")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "passivescreen")], na.rm = TRUE)
# Active screen use
quantile(data.pp$total_duration[which(data.pp$activity == "activescreen")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activescreen")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activescreen")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activescreen")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activescreen")], na.rm = TRUE)
# Passive transport
quantile(data.pp$total_duration[which(data.pp$activity == "passivetransport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "passivetransport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "passivetransport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "passivetransport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "passivetransport")], na.rm = TRUE)
# Active transport
quantile(data.pp$total_duration[which(data.pp$activity == "activetransport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activetransport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activetransport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activetransport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activetransport")], na.rm = TRUE)
# Calm play
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay")], na.rm = TRUE)
# Active play
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay")], na.rm = TRUE)
# Unknown play
quantile(data.pp$total_duration[which(data.pp$activity == "dontknowplay")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "dontknowplay")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "dontknowplay")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "dontknowplay")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "dontknowplay")], na.rm = TRUE)
# Other activity
quantile(data.pp$total_duration[which(data.pp$activity == "otheractivity")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "otheractivity")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "otheractivity")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "otheractivity")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "otheractivity")], na.rm = TRUE)
# Child was with someone else
quantile(data.pp$total_duration[which(data.pp$activity == "someoneelse")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "someoneelse")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "someoneelse")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "someoneelse")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "someoneelse")], na.rm = TRUE)
# I don't know
quantile(data.pp$total_duration[which(data.pp$activity == "dontknow")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "dontknow")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "dontknow")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "dontknow")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "dontknow")], na.rm = TRUE)

### Descriptives for different postures 
## Sitting/lying
desc_sittinglying <- data.pp[data.pp$activity == "sittinglying",] %>% group_by(posture) %>% 
  summarise(dur = median(total_duration, na.rm = TRUE),
            freq = n(),
            per =  n() / sum(desc_sittinglying$freq) * 100,
            ENMO.hip = median(ENMO.hip, na.rm = TRUE),
            MAD.hip = median(MAD.hip, na.rm = TRUE),
            ENMO.wrist = median(ENMO.wrist, na.rm = TRUE),
            MAD.wrist = median(MAD.wrist, na.rm = TRUE)
  )
# Being carried
quantile(data.pp$total_duration[which(data.pp$activity == "sittinglying" & data.pp$posture == "carried")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "carried")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "carried")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "carried")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "carried")], na.rm = TRUE)
# Lying on tummy
quantile(data.pp$total_duration[which(data.pp$activity == "sittinglying" & data.pp$posture == "tummy")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "tummy")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "tummy")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "tummy")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "tummy")], na.rm = TRUE)
# Lying on back
quantile(data.pp$total_duration[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying_back")], na.rm = TRUE)
# Lying on side
quantile(data.pp$total_duration[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying_side")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying_side")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying_side")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying_side")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying_side")], na.rm = TRUE)
# Sitting with support
quantile(data.pp$total_duration[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
# Sitting without support
quantile(data.pp$total_duration[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
# Lying
quantile(data.pp$total_duration[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "lying")], na.rm = TRUE)
# Sitting
quantile(data.pp$total_duration[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "sitting")], na.rm = TRUE)
# Changing posture
quantile(data.pp$total_duration[which(data.pp$activity == "sittinglying" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "alternating")], na.rm = TRUE)
# I don't know
quantile(data.pp$total_duration[which(data.pp$activity == "sittinglying" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "sittinglying" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "sittinglying" & data.pp$posture == "dontknow")], na.rm = TRUE)

## Calm play
desc_calmplay <- data.pp[data.pp$activity == "quietplay",] %>% group_by(posture) %>% 
  summarise(dur = median(total_duration, na.rm = TRUE),
            freq = n(),
            per =  n() / sum(desc_calmplay$freq) * 100,
            ENMO.hip = median(ENMO.hip, na.rm = TRUE),
            MAD.hip = median(MAD.hip, na.rm = TRUE),
            ENMO.wrist = median(ENMO.wrist, na.rm = TRUE),
            MAD.wrist = median(MAD.wrist, na.rm = TRUE)
  )
# Being carried
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "carried")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "carried")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "carried")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "carried")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "carried")], na.rm = TRUE)
# Lying on back
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "lying_back")], na.rm = TRUE)
# Lying on side
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "lying_side")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "lying_side")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "lying_side")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "lying_side")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "lying_side")], na.rm = TRUE)
# Sitting with support
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
# Sitting without support
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
# Standing with support
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "standing_withsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "standing_withsupport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "standing_withsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "standing_withsupport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "standing_withsupport")], na.rm = TRUE)
# Standing without support
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "standing_withoutsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "standing_withoutsupport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "standing_withoutsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "standing_withoutsupport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "standing_withoutsupport")], na.rm = TRUE)
# Lying
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "lying")], na.rm = TRUE)
# Sitting
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "sitting")], na.rm = TRUE)
# Standing
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "standing")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "standing")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "standing")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "standing")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "standing")], na.rm = TRUE)
# Changing posture
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "alternating")], na.rm = TRUE)
# I don't know
quantile(data.pp$total_duration[which(data.pp$activity == "quietplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "quietplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "quietplay" & data.pp$posture == "dontknow")], na.rm = TRUE)

## Active play
desc_activeplay <- data.pp[data.pp$activity == "activeplay",] %>% group_by(posture) %>% 
  summarise(dur = median(total_duration, na.rm = TRUE),
            freq = n(),
            per =  n() / sum(desc_activeplay$freq) * 100,
            ENMO.hip = median(ENMO.hip, na.rm = TRUE),
            MAD.hip = median(MAD.hip, na.rm = TRUE),
            ENMO.wrist = median(ENMO.wrist, na.rm = TRUE),
            MAD.wrist = median(MAD.wrist, na.rm = TRUE)
  )
# Lying on tummy
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay" & data.pp$posture == "tummy")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "tummy")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "tummy")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "tummy")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "tummy")], na.rm = TRUE)
# Lying on back
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "lying_back")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "lying_back")], na.rm = TRUE)
# Sitting with support
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting_withsupport")], na.rm = TRUE)
# Sitting without support
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting_withoutsupport")], na.rm = TRUE)
# Standing with support
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay" & data.pp$posture == "standing_withsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "standing_withsupport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "standing_withsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "standing_withsupport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "standing_withsupport")], na.rm = TRUE)
# Standing without support
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay" & data.pp$posture == "standing_withoutsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "standing_withoutsupport")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "standing_withoutsupport")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "standing_withoutsupport")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "standing_withoutsupport")], na.rm = TRUE)
# Lying
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "lying")], na.rm = TRUE)
# Sitting
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "sitting")], na.rm = TRUE)
# Standing
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay" & data.pp$posture == "standing")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "standing")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "standing")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "standing")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "standing")], na.rm = TRUE)
# Changing posture
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "alternating")], na.rm = TRUE)
# I don't know
quantile(data.pp$total_duration[which(data.pp$activity == "activeplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "activeplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "activeplay" & data.pp$posture == "dontknow")], na.rm = TRUE)

## Unknown play
desc_unknownplay <- data.pp[data.pp$activity == "dontknowplay",] %>% group_by(posture) %>% 
  summarise(dur = median(total_duration, na.rm = TRUE),
            freq = n(),
            per =  n() / sum(desc_unknownplay$freq) * 100,
            ENMO.hip = median(ENMO.hip, na.rm = TRUE),
            MAD.hip = median(MAD.hip, na.rm = TRUE),
            ENMO.wrist = median(ENMO.wrist, na.rm = TRUE),
            MAD.wrist = median(MAD.wrist, na.rm = TRUE)
  )
# Lying
quantile(data.pp$total_duration[which(data.pp$activity == "dontknowplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "dontknowplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "dontknowplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "dontknowplay" & data.pp$posture == "lying")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "dontknowplay" & data.pp$posture == "lying")], na.rm = TRUE)
# Sitting
quantile(data.pp$total_duration[which(data.pp$activity == "dontknowplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "dontknowplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "dontknowplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "dontknowplay" & data.pp$posture == "sitting")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "dontknowplay" & data.pp$posture == "sitting")], na.rm = TRUE)
# Changing posture
quantile(data.pp$total_duration[which(data.pp$activity == "dontknowplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "dontknowplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "dontknowplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "dontknowplay" & data.pp$posture == "alternating")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "dontknowplay" & data.pp$posture == "alternating")], na.rm = TRUE)
# I don't know
quantile(data.pp$total_duration[which(data.pp$activity == "dontknowplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$activity == "dontknowplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$activity == "dontknowplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$activity == "dontknowplay" & data.pp$posture == "dontknow")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$activity == "dontknowplay" & data.pp$posture == "dontknow")], na.rm = TRUE)

### Descriptives for the movement behaviors
data.pp$behavior <- as.factor(data.pp$behavior)
library(dplyr)
desc_behavior <- data.pp %>% group_by(behavior) %>% 
  summarise(dur = median(total_duration, na.rm = TRUE),
            freq = n(),
            per =  n() / nrow(data.pp) * 100,
            ENMO.hip = median(ENMO.hip, na.rm = TRUE),
            MAD.hip = median(MAD.hip, na.rm = TRUE),
            ENMO.wrist = median(ENMO.wrist, na.rm = TRUE),
            MAD.wrist = median(MAD.wrist, na.rm = TRUE)
  )

## IQR per movement behavior 
# Sleep
quantile(data.pp$total_duration[which(data.pp$behavior == "sleep")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$behavior == "sleep")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$behavior == "sleep")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$behavior == "sleep")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$behavior == "sleep")], na.rm = TRUE)
# SB
quantile(data.pp$total_duration[which(data.pp$behavior == "SB")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$behavior == "SB")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$behavior == "SB")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$behavior == "SB")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$behavior == "SB")], na.rm = TRUE)
# PA
quantile(data.pp$total_duration[which(data.pp$behavior == "PA")], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(data.pp$behavior == "PA")], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(data.pp$behavior == "PA")], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(data.pp$behavior == "PA")], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(data.pp$behavior == "PA")], na.rm = TRUE)
# NA
quantile(data.pp$total_duration[which(is.na(data.pp$behavior))], na.rm = TRUE)
quantile(data.pp$ENMO.hip[which(is.na(data.pp$behavior))], na.rm = TRUE)
quantile(data.pp$MAD.hip[which(is.na(data.pp$behavior))], na.rm = TRUE)
quantile(data.pp$ENMO.wrist[which(is.na(data.pp$behavior))], na.rm = TRUE)
quantile(data.pp$MAD.wrist[which(is.na(data.pp$behavior))], na.rm = TRUE)