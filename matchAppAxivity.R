## User input 
date <- "20230918" # Date 
tz = "Europe/Amsterdam"
# Use preprocessed Axivity data (data without non-wear)
filepath.hip <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/5sec/epochdata/hip/nonwear_removed"
filepath.wrist <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/5sec/epochdata/wrist/nonwear_removed"
# Use app data activities
filepath.app <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/App-data/20230918"
filename.app <- "/20230918_activities_castor_linked_duration.csv"
# Path to directory to save the data list
savedir <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output"

# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/AxivityMLMapp/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

## Load data
data.app <- load.app(filepath.app, filename.app, cohort = c(3), measurementperiod = 1, sep = ",") # Load app data

## Match axivity data to app entries
#run L22 once: then load in data from saved file
data.app.axivity <- match.app.axivity(data.app, filepath.hip, filepath.wrist, tz, date)
load("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output/app_ax_entry_20230925.RData")
paste0("n parents filled in at least one activity in the app: ", length(unique(data.app$castorID)))
#TO DO:
# - paste0("n children only wore the hip accelerometer: ", length(unique(data.app$castorID)))
# - paste0("n children only wore the wrist accelerometer: ", length(unique(data.app$castorID)))
# - paste0("n children wore in both accelerometers: ", length(unique(data.app$castorID)))

# From gravitational units in g to mg
data.app.axivity$ENMO.hip <- data.app.axivity$ENMO.hip * 1000
data.app.axivity$ENMO.wrist <- data.app.axivity$ENMO.wrist * 1000
data.app.axivity$MAD.hip <- data.app.axivity$MAD.hip * 1000
data.app.axivity$MAD.wrist <- data.app.axivity$MAD.wrist * 1000

# Combine data for both placements
#data.app.axivity$ENMO.both = data.app.axivity$ENMO.hip * data.app.axivity$ENMO.wrist 
#data.app.axivity$MAD.both = data.app.axivity$MAD.hip * data.app.axivity$MAD.wrist 

# Reshape data to long format
df_long <- tidyr::gather(data.app.axivity, metric, value, ENMO.hip:MAD.wrist)
#df_long <- tidyr::gather(data.app.axivity, metric, value, ENMO.hip:MAD.both)
df_long.ENMO <- df_long[which(startsWith(df_long$metric, "ENMO")),]
df_long.MAD <- df_long[which(startsWith(df_long$metric, "MAD")),]

## BOXPLOT
ggplot2::ggplot(df_long.ENMO, ggplot2::aes(y = value, x = as.factor(activity), fill =metric)) +
  ggplot2::geom_boxplot()



# Example scatterplot
ggplot2::ggplot(verzorging, ggplot2::aes(x = total_duration, y = ENMO.hip)) + ggplot2::geom_point() 
  
ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = ENMO.hip, color = activity)) + 
  ggplot2::geom_line(alpha = .5) 

#density plot of acceleration
ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.hip, color = activity)) + 
  ggplot2::geom_density(alpha = 0.25) 
ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.wrist, color = activity)) + 
  ggplot2::geom_density(alpha = 0.25) 

ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, color = behavior)) + 
  ggplot2::geom_density(alpha = 0.25) 


hist(data.app.axivity$total_duration)



ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.hip, color = behavior)) + 
  ggplot2::geom_density(alpha = 0.25) 

ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.hip*ENMO.wrist, color = behavior)) + 
  ggplot2::geom_density(alpha = 0.25) 



ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.hip, y = as.factor(activity))) +
  ggplot2::geom_boxplot()

ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.wrist, y = as.factor(activity))) +
  ggplot2::geom_boxplot()

ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = c(MAD.hip, MAD.wrist), y = as.factor(activity))) +
  ggplot2::geom_boxplot()

ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = MAD.wrist, y = as.factor(activity))) +
  ggplot2::geom_boxplot()



ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.both, y = as.factor(activity))) +
  ggplot2::geom_boxplot()

data.app.axivity$MAD.both = data.app.axivity$MAD.hip * data.app.axivity$MAD.wrist
ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = MAD.both, y = as.factor(activity))) +
  ggplot2::geom_boxplot()


#Combine outcomes into one plot





## Reshape from long to wide to gather information per day for each participant... then gather per participant???


## Plot distributions of acceleration metrics per category

categories <- unique(data.app.axivity$activity)
verzorging <- data.app.axivity[which(data.app.axivity$activity == "verzorging"),]
plot(verzorging$total_duration, verzorging$MAD.hip)


files <- filelist[which(startsWith(filelist.hip, participant[id]))]  # Associated filenames

list.files(axivityDir)

# data.app <- data.app[order(data.app$castorID),]
filelist <- list.files(axivityDir)
