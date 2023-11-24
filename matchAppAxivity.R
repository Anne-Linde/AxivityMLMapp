## This script was used to:
# 1) Synchronize the axivity data to the app entries (as described in section 2.8.3.1 of the article)
# 2) Report the duration, frequency and acceleration per activity category over all reported categories and 24-h movement behaviors
# 3) Report descriptives overlap accelerometers and app

rm(list = ls())
gc()

## User input 
date <- "20231031" # Date of last data update
tz = "Europe/Amsterdam"
# Use preprocessed Axivity data (data without non-wear)
filepath.hip <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/5sec/epochdata/hip/nonwear_removed"
filepath.wrist <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/5sec/epochdata/wrist/nonwear_removed"
# Use app data activities
filepath.app <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/App-data/20230918"
filename.app <- paste0("/", date, "_activities_castor_linked_duration.csv")
filename.app.day <- paste0("/", date, "_MLMapp_pp_duration_frequency_day.csv")

# Path to directory to save the data list
savedir <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output"

# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/AxivityMLMapp/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

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
            per =  n() / nrow(data.pp) * 100,
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
            per =  n() / nrow(data.pp) * 100,
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
            per =  n() / nrow(data.pp) * 100,
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
            per =  n() / nrow(data.pp) * 100,
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




desc %>% 
  dplyr::filter(IQR > quantile(IQR, 0.25), 
                IQR < quantile(IQR, 0.75))

## Descriptives overlap app - accelerometer
load("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output/app_missing_ax_day_20231031.RData")

missing_app_min <- c()
data_hip_min <- c()
data_wrist_min <- c()
NA_category_min <- c()
NA_hip_min <- c()
NA_wrist_min <- c()

for(pp in 1:nrow(missing.app)){
  missing_app_min <- c(missing_app_min, mean(as.numeric(missing.app[pp,c(2:9)]), na.rm = T))
  data_hip_min <- c(data_hip_min, mean(as.numeric(missing.app[pp,c(10:17)]), na.rm = T))
  data_wrist_min <- c(data_wrist_min, mean(as.numeric(missing.app[pp,c(18:25)]), na.rm = T))
  
  tmp.act <- data.pp[data.pp$castorID == missing.app$castorID[pp],]
  tmp.act.na <- tmp.act[which(tmp.act$activity %in% c("someoneelse", "otheractivity", "dontknow")),]
  NA_category_min <- c(NA_category_min, mean(tmp.act.na$total_duration))
  NA_hip_min <- c(NA_hip_min, mean(tmp.act.na$acc_min_hip))
  NA_wrist_min <- c(NA_wrist_min, mean(tmp.act.na$acc_min_wrist))
}

mean(NA_category_min, na.rm = TRUE)
mean(NA_hip_min, na.rm = TRUE)
mean(NA_wrist_min, na.rm = TRUE)


#### Rewrite this script from here


# Combine data for both placements
#data.app.axivity$ENMO.both = data.app.axivity$ENMO.hip * data.app.axivity$ENMO.wrist 
#data.app.axivity$MAD.both = data.app.axivity$MAD.hip * data.app.axivity$MAD.wrist 

## SCATTERPLOTS
df2 <- dplyr::select(data.app.axivity, -activity)

scatter.ENMOhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = ENMO.hip, color = activity)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = activity)) + 
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ggtitle("Hip") + ggplot2::ylim(c(0, 500)) 
  
scatter.ENMOwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = ENMO.wrist, color = activity)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = activity)) + 
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ylim(c(0, 500)) + ggplot2::ggtitle("Wrist")

gridExtra::grid.arrange(scatter.ENMOhip, scatter.ENMOwrist, ncol=2) #arranges plots within grid
scatter_ENMO <- gridExtra::arrangeGrob(scatter.ENMOhip + ggplot2::theme(axis.title.x = ggplot2::element_blank()),
                                   scatter.ENMOwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                   ncol=2, 
                                   bottom = "Duration (min)") # generates plot
plot(scatter_ENMO) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/categories/scatter_categories_ENMO.png"), scatter_ENMO, width = 10, height = 8, dpi = 600) #saves g

scatter.MADhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = MAD.hip, color = activity)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = activity)) + 
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ggtitle("Hip") + ggplot2::ylim(c(0, 500)) 

scatter.MADwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = MAD.wrist, color = activity)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = activity)) + 
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ylim(c(0, 500)) + ggplot2::ggtitle("Wrist")

gridExtra::grid.arrange(scatter.MADhip, scatter.MADwrist, ncol=2) #arranges plots within grid
scatter_MAD <- gridExtra::arrangeGrob(scatter.MADhip + ggplot2::theme(axis.title.x = ggplot2::element_blank()),
                                       scatter.MADwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       ncol=2, 
                                       bottom = "Duration (min)") # generates plot
plot(scatter_MAD) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/categories/scatter_categories_MAD.png"), scatter_MAD, width = 10, height = 8, dpi = 600) #saves g

## DENSITY PLOTS
# Per activity
density.ENMOwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.wrist, color = activity)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.ENMOhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.hip, color = activity)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") 

gridExtra::grid.arrange(density.ENMOwrist, density.ENMOhip, nrow=2) #arranges plots within grid
density_ENMO <- gridExtra::arrangeGrob(density.ENMOwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       density.ENMOhip + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                       nrow=2, left = "Density") # generates plot
plot(density_ENMO) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/categories/density_categories_ENMO.png"), density_ENMO, width = 10, height = 15, dpi = 600) #saves g

density.MADwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = MAD.wrist, color = activity)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.MADhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = MAD.hip, color = activity)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") 

gridExtra::grid.arrange(density.MADwrist, density.MADhip, nrow=2) #arranges plots within grid
density_MAD <- gridExtra::arrangeGrob(density.MADwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       density.MADhip + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                       nrow=2, left = "Density") # generates plot
plot(density_MAD) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/categories/density_categories_MAD.png"), density_MAD, width = 10, height = 15, dpi = 600) #saves g

# Postures for activities: active play, quiet play, i don't know play, and sitting/lying
data.app.axivity.posture <- data.app.axivity[data.app.axivity$activity %in% c("actiefspelen", "rustigspelen", "weetnietspelen", "zittenliggen"),]
density.ENMOwrist.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = ENMO.wrist, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") + ggplot2::ylim(c(0, 0.4)) 

density.ENMOhip.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = ENMO.hip, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="bottom") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") + ggplot2::xlim(c(0, 500)) 

gridExtra::grid.arrange(density.ENMOwrist.posture, density.ENMOhip.posture, nrow=2) #arranges plots within grid
density_ENMO.posture <- gridExtra::arrangeGrob(density.ENMOwrist.posture + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       density.ENMOhip.posture + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                       nrow=2, left = "Density") # generates plot
plot(density_ENMO.posture) #print the plot
ggplot2:::ggsave(file=paste0(savedir, "/plots/distributions/categories/density_categories_posture_ENMO.png"), density_ENMO.posture, width = 210, height = 297, units = "mm")

density.MADwrist.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = MAD.wrist, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist")  + ggplot2::ylim(c(0, 0.4)) 

density.MADhip.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = MAD.hip, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="bottom") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") + ggplot2::xlim(c(0, 500)) 

gridExtra::grid.arrange(density.MADwrist.posture, density.MADhip.posture, nrow=2) #arranges plots within grid
density_MAD.posture <- gridExtra::arrangeGrob(density.MADwrist.posture + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                      density.MADhip.posture + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                      nrow=2, left = "Density") # generates plot
plot(density_MAD.posture) #print the plot
ggplot2:::ggsave(file=paste0(savedir, "/plots/distributions/categories/density_categories_posture_MAD.png"), density_MAD.posture, width = 210, height = 297, units = "mm")

### Behavior distributions
## BOXPLOT
level_order <- c("sleep", "SB", "PA")
bxp_ENMO <- ggplot2::ggplot(df_long.ENMO, ggplot2::aes(y = value, x = as.factor(behavior), fill =metric)) + 
  ggplot2::geom_boxplot(outlier.shape = 19, outlier.alpha = 0.1) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = level_order) + ggplot2::xlab("") + 
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) +
  ggplot2::scale_fill_discrete(name = "Accelerometer placement", labels = c("Hip", "Wrist"))

bxp_MAD <- ggplot2::ggplot(df_long.MAD, ggplot2::aes(y = value, x = as.factor(behavior), fill =metric)) + 
  ggplot2::geom_boxplot(outlier.shape = 19, outlier.alpha = 0.1) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = level_order) + ggplot2::xlab("App categories") + 
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) +
  ggplot2::scale_fill_discrete(name = "Accelerometer placement", labels = c("Hip", "Wrist"))

gridExtra::grid.arrange(bxp_ENMO, bxp_MAD, nrow=2) #arranges plots within grid

boxplots <- gridExtra::arrangeGrob(bxp_ENMO + ggplot2::theme(legend.position="top"),
                                   bxp_MAD + ggplot2::theme(legend.position="none"),
                                   nrow=2) # generates plot
plot(boxplots) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/boxplot_behaviors.png"), boxplots, width = 10, height = 8, dpi = 600) #saves g

## SCATTERPLOTS
df2 <- dplyr::select(data.app.axivity, -behavior)

scatter.ENMOhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = ENMO.hip, color = behavior)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = behavior)) + 
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ggtitle("Hip") + ggplot2::ylim(c(0, 500)) 

scatter.ENMOwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = ENMO.wrist, color = behavior)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = behavior)) + 
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ylim(c(0, 500)) + ggplot2::ggtitle("Wrist")

gridExtra::grid.arrange(scatter.ENMOhip, scatter.ENMOwrist, ncol=2) #arranges plots within grid
scatter_ENMO <- gridExtra::arrangeGrob(scatter.ENMOhip + ggplot2::theme(axis.title.x = ggplot2::element_blank()),
                                       scatter.ENMOwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       ncol=2, 
                                       bottom = "Duration (min)") # generates plot
plot(scatter_ENMO) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/scatter_behavior_ENMO.png"), scatter_ENMO, width = 10, height = 8, dpi = 600) #saves g

scatter.MADhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = MAD.hip, color = behavior)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = behavior)) + 
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ggtitle("Hip") + ggplot2::ylim(c(0, 500)) 

scatter.MADwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = MAD.wrist, color = behavior)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = behavior)) + 
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ylim(c(0, 500)) + ggplot2::ggtitle("Wrist")

gridExtra::grid.arrange(scatter.MADhip, scatter.MADwrist, ncol=2) #arranges plots within grid
scatter_MAD <- gridExtra::arrangeGrob(scatter.MADhip + ggplot2::theme(axis.title.x = ggplot2::element_blank()),
                                      scatter.MADwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                      ncol=2, 
                                      bottom = "Duration (min)") # generates plot
plot(scatter_MAD) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/scatter_behavior_MAD.png"), scatter_MAD, width = 10, height = 8, dpi = 600) #saves g

## DENSITY PLOTS
# Per behavior
density.ENMOwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.wrist, color = behavior)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.ENMOhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.hip, color = behavior)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") ggplot2::xlim(c(0, 500))

gridExtra::grid.arrange(density.ENMOwrist, density.ENMOhip, nrow=2) #arranges plots within grid
density_ENMO <- gridExtra::arrangeGrob(density.ENMOwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       density.ENMOhip + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                       nrow=2, left = "Density") # generates plot
plot(density_ENMO) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/density_behaviors_ENMO.png"), density_ENMO, width = 10, height = 15, dpi = 600) #saves g

density.MADwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = MAD.wrist, color = behavior)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.MADhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = MAD.hip, color = behavior)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") + ggplot2::xlim(0, 500)

gridExtra::grid.arrange(density.MADwrist, density.MADhip, nrow=2) #arranges plots within grid
density_MAD <- gridExtra::arrangeGrob(density.MADwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                      density.MADhip + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                      nrow=2, left = "Density") # generates plot
plot(density_MAD) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/density_behaviors_MAD.png"), density_MAD, width = 10, height = 15, dpi = 600) #saves g

# Postures for activities: active play, quiet play, i don't know play, and sitting/lying
data.app.axivity.posture <- data.app.axivity[data.app.axivity$behavior %in% c("SB", "PA"),]
density.ENMOwrist.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = ENMO.wrist, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.ENMOhip.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = ENMO.hip, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="bottom") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") 

gridExtra::grid.arrange(density.ENMOwrist.posture, density.ENMOhip.posture, nrow=2) #arranges plots within grid
density_ENMO.posture <- gridExtra::arrangeGrob(density.ENMOwrist.posture + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                               density.ENMOhip.posture + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                               nrow=2, left = "Density") # generates plot
plot(density_ENMO.posture) #print the plot
ggplot2:::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/density_behaviors_posture_ENMO.png"), density_ENMO.posture, width = 210, height = 297, units = "mm")

density.MADwrist.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = MAD.wrist, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.MADhip.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = MAD.hip, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="bottom") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") 

gridExtra::grid.arrange(density.MADwrist.posture, density.MADhip.posture, nrow=2) #arranges plots within grid
density_MAD.posture <- gridExtra::arrangeGrob(density.MADwrist.posture + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                              density.MADhip.posture + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                              nrow=2, left = "Density") # generates plot
plot(density_MAD.posture) #print the plot
ggplot2:::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/density_behaviors_posture_MAD.png"), density_MAD.posture, width = 210, height = 297, units = "mm")



