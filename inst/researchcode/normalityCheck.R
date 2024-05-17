### This script (as described in section 2.4.2 of the article)
## 1) Creates normality and Q-Q plots for a) the My Little Moves app: the total duration (separate for activities and 24-h movement behaviors);
## and b) the accelerometer hip and wrist data (separate plots for each participant)
## 2) Transforms accelererometer data (log + small jitter)
## 3) Creates normality and Q-Q plots for the log transformed data and saves the log transformed data

## User settings
filepath.app <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/App-data/20230918/"
filename.app <- "20230918_activities_castor_linked_duration.csv"
filename.app.day <- "/20230918_MLMapp_pp_duration_frequency_day.csv"
savedir <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output"

filepath.axivity <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/5sec/epochdata/"
filepath.hip <- paste0(filepath.axivity, "hip/nonwear_removed")
filepath.wrist <- paste0(filepath.axivity, "wrist/nonwear_removed")

# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/AxivityMLMapp/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

## Load data
data.app <- load.app(filepath.app, filename.app, cohort = c(1,2,3), measurementperiod = 1, sep = ",") # Load app data
data.app.3 <- load.app(filepath.app, filename.app, cohort = c(3), measurementperiod = 1, sep = ",") # Load app data
#data.app.day <- read.csv(paste0(filepath.app, filename.app.day))[-1] # Load app data
load("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output/app_ax_entry_20231010.RData")

### STEP 1 My Little Moves data
## For sub-cohort 3
# Total duration 
h <- ggplot2::ggplot(data = data.app.3, mapping = ggplot2::aes(x = total_duration)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::xlab("Duration (min)") + ggplot2::ylab("Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/App data/cohort3/total_duration_activities.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

q <- ggplot2::ggplot(data = data.app.3, mapping = ggplot2::aes(sample = total_duration)) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line() 
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/App data/cohort3/qq_total_duration_activities.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

# Duration per activity category
for (category in 1:length(unique(data.app.3$activity))){
  h <- ggplot2::ggplot(data = data.app.3[data.app.3$activity == unique(data.app.3$activity)[category],], mapping = ggplot2::aes(x = total_duration)) + ggplot2::geom_histogram() +
    ggplot2::theme_classic() + ggplot2::labs(title = unique(data.app.3$activity)[category], x = "Duration (min)", y = "Frequency")
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/App data/cohort3/", unique(data.app.3$activity)[category], ".jpeg"), h, width = 10, height = 8, dpi = 600) #saves g
}
q <- ggplot2::ggplot(data = data.app.3, mapping = ggplot2::aes(sample = total_duration, colour = as.factor(activity))) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/App data/cohort3/qq_total_duration_activities_separate.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

# Duration per behavior
for (category in 1:length(unique(data.app.3$behavior))){
  h <- ggplot2::ggplot(data = data.app.3[data.app.3$behavior == unique(data.app.3$behavior)[category],], mapping = ggplot2::aes(x = total_duration)) + ggplot2::geom_histogram() +
    ggplot2::theme_classic() + ggplot2::labs(title = unique(data.app.3$behavior)[category], x = "Duration (min)", y = "Frequency")
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/App data/cohort3/", unique(data.app.3$behavior)[category], ".jpeg"), h, width = 5, height = 5, dpi = 200) #saves g
}
q <- ggplot2::ggplot(data = data.app.3, mapping = ggplot2::aes(sample = total_duration, colour = as.factor(behavior))) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/App data/cohort3/qq_total_duration_behavior_separate.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

## For all data
# Total duration 
h <- ggplot2::ggplot(data = data.app, mapping = ggplot2::aes(x = total_duration)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::xlab("Duration (min)") + ggplot2::ylab("Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/App data/total_duration_activities.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

q <- ggplot2::ggplot(data = data.app, mapping = ggplot2::aes(sample = total_duration)) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/App data/qq_total_duration_activities.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

# Duration per activity category
for (category in 1:length(unique(data.app$activity))){
  h <- ggplot2::ggplot(data = data.app[data.app$activity == unique(data.app$activity)[category],], mapping = ggplot2::aes(x = total_duration)) + ggplot2::geom_histogram() +
    ggplot2::theme_classic() + ggplot2::labs(title = unique(data.app$activity)[category], x = "Duration (min)", y = "Frequency")
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/App data/", unique(data.app$activity)[category], ".jpeg"), h, width = 5, height = 5, dpi = 200) #saves g
}
q <- ggplot2::ggplot(data = data.app, mapping = ggplot2::aes(sample = total_duration, colour = as.factor(activity))) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/App data/qq_total_duration_activities_separate.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

# Duration per behavior
for (category in 1:length(unique(data.app$behavior))){
  h <- ggplot2::ggplot(data = data.app[data.app$behavior == unique(data.app$behavior)[category],], mapping = ggplot2::aes(x = total_duration)) + ggplot2::geom_histogram() +
    ggplot2::theme_classic() + ggplot2::labs(title = unique(data.app$behavior)[category], x = "Duration (min)", y = "Frequency")
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/App data/", unique(data.app$behavior)[category], ".jpeg"), h, width = 5, height = 5, dpi = 200) #saves g
}
q <- ggplot2::ggplot(data = data.app, mapping = ggplot2::aes(sample = total_duration, colour = as.factor(behavior))) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/App data/qq_total_duration_behavior_separate.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

### Accelerometer data
## Hip
# ENMO
h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = ENMO.hip)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' median accelerations for all categories", 
                                           x = expression(paste("ENMO (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/ENMO/hip/all_median_acc.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

q <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(sample = ENMO.hip)) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/ENMO/hip/all_median_acc.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = ENMO.hip, colour = activity)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' median accelerations per category", 
                                           x = expression(paste("ENMO (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/ENMO/hip/all_median_acc_per_category.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

# Log transform ENMO values
data.pp$logENMO.hip <- log(data.pp$ENMO.hip + 0.001)
h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = logENMO.hip)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' log(x+0,001) transformed accelerations for all categories", 
                                           x = expression(paste("ENMO (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/ENMO/hip/log_transformed_acc.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

q <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(sample = logENMO.hip)) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/ENMO/hip/log_transformed_acc.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

# #square root transformation
# data.pp$sqrtENMO.hip <- sqrt(data.pp$ENMO.hip)
# h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = sqrtENMO.hip)) + ggplot2::geom_histogram() +
#   ggplot2::theme_classic() + ggplot2::labs(title = "All participants' sqrt(x) transformed accelerations for all categories", 
#                                            x = expression(paste("ENMO (m", italic("g"), ")")), y = "Frequency")
# ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/ENMO/hip/srt_transformed_acc.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g
# 
# q <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(sample = sqrtENMO.hip)) + ggplot2::geom_qq() +
#   ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
# ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/ENMO/hip/srt_transformed_acc.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

# MAD
h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = MAD.hip)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' median accelerations for all categories", 
                                           x = expression(paste("MAD (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/MAD/hip/all_median_acc.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

q <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(sample = MAD.hip)) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/MAD/hip/all_median_acc.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = MAD.hip, colour = activity)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' median accelerations per category", 
                                           x = expression(paste("MAD (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/MAD/hip/all_median_acc_per_category.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

# Log transform MAD values
data.pp$logMAD.hip <- log(data.pp$MAD.hip + 0.001)
h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = logMAD.hip)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' log(x+0,001) transformed accelerations for all categories", 
                                           x = expression(paste("MAD (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/MAD/hip/log_transformed_acc.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

q <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(sample = logMAD.hip)) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/MAD/hip/log_transformed_acc.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g


## Wrist
# ENMO
h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = ENMO.wrist)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' median accelerations for all categories", 
                                           x = expression(paste("ENMO (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/ENMO/wrist/all_median_acc.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

q <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(sample = ENMO.wrist)) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/ENMO/wrist/all_median_acc.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = ENMO.wrist, colour = activity)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' median accelerations per category", 
                                           x = expression(paste("ENMO (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/ENMO/wrist/all_median_acc_per_category.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

# Log transform ENMO values
data.pp$logENMO.wrist <- log(data.pp$ENMO.wrist + 0.001)
h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = logENMO.wrist)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' log(x+0,001) transformed accelerations for all categories", 
                                           x = expression(paste("ENMO (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/ENMO/wrist/log_transformed_acc.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

q <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(sample = logENMO.wrist)) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/ENMO/wrist/log_transformed_acc.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

# MAD
h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = MAD.wrist)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' median accelerations for all categories", 
                                           x = expression(paste("MAD (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/MAD/wrist/all_median_acc.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

q <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(sample = MAD.wrist)) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/MAD/wrist/all_median_acc.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = MAD.wrist, colour = activity)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' median accelerations per category", 
                                           x = expression(paste("MAD (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/MAD/wrist/all_median_acc_per_category.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

# Log transform MAD values
data.pp$logMAD.wrist <- log(data.pp$MAD.wrist + 0.001)
h <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(x = logMAD.wrist)) + ggplot2::geom_histogram() +
  ggplot2::theme_classic() + ggplot2::labs(title = "All participants' log(x+0,001) transformed accelerations for all categories", 
                                           x = expression(paste("MAD (m", italic("g"), ")")), y = "Frequency")
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/MAD/wrist/log_transformed_acc.jpeg"), h, width = 5, height = 5, dpi = 200) #saves g

q <- ggplot2::ggplot(data = data.pp, mapping = ggplot2::aes(sample = logMAD.wrist)) + ggplot2::geom_qq() +
  ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/MAD/wrist/log_transformed_acc.jpeg"), q, width = 5, height = 5, dpi = 200) #saves g

## Save dataset including log transformed data
save(data.pp, file = paste0(savedir, "/logtransformed_app_ax_entry_20231025.RData"))

### Separate for all participants
## Hip
filelist <- list.files(filepath.hip, pattern = ".RData")
for (file in 1:length(filelist)){
  load(paste0(filepath.hip, "/", filelist)[file])
  
  h <- ggplot2::ggplot(data = epochdata$agg.epoch, mapping = ggplot2::aes(x = ENMO)) + ggplot2::geom_histogram() +
    ggplot2::theme_classic() + ggplot2::labs(title = filelist[file], 
                                             x = expression(paste("ENMO (m", italic("g"), ")")), y = "Frequency")
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/ENMO/hip/", filelist[file], ".jpeg"), h, width = 5, height = 5, dpi = 200) #saves g
  
  q <- ggplot2::ggplot(data = epochdata$agg.epoch, mapping = ggplot2::aes(sample = ENMO)) + ggplot2::geom_qq() +
    ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/ENMO/hip/", filelist[file], ".jpeg"), q, width = 5, height = 5, dpi = 200) #saves g
                  
  i <- ggplot2::ggplot(data = epochdata$agg.epoch, mapping = ggplot2::aes(x = MAD)) + ggplot2::geom_histogram() +
    ggplot2::theme_classic() + ggplot2::labs(title = filelist[file], 
                                             x = expression(paste("MAD (m", italic("g"), ")")), y = "Frequency")
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/MAD/hip/", filelist[file], ".jpeg"), i, width = 5, height = 5, dpi = 200) #saves g
                  
  q <- ggplot2::ggplot(data = epochdata$agg.epoch, mapping = ggplot2::aes(sample = MAD)) + ggplot2::geom_qq() +
    ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/MAD/hip/", filelist[file], ".jpeg"), q, width = 5, height = 5, dpi = 200) #saves g
}

## Wrist
filelist <- list.files(filepath.wrist, pattern = ".RData")
for (file in 1:length(filelist)){
  load(paste0(filepath.wrist, "/", filelist)[file])
  
  h <- ggplot2::ggplot(data = epochdata$agg.epoch, mapping = ggplot2::aes(x = ENMO)) + ggplot2::geom_histogram() +
    ggplot2::theme_classic() + ggplot2::labs(title = filelist[file], 
                                             x = expression(paste("ENMO (m", italic("g"), ")")), y = "Frequency")
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/ENMO/wrist/", filelist[file], ".jpeg"), h, width = 5, height = 5, dpi = 200) #saves g
  
  q <- ggplot2::ggplot(data = epochdata$agg.epoch, mapping = ggplot2::aes(sample = ENMO)) + ggplot2::geom_qq() +
    ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/ENMO/wrist/", filelist[file], ".jpeg"), q, width = 5, height = 5, dpi = 200) #saves g
  
  i <- ggplot2::ggplot(data = epochdata$agg.epoch, mapping = ggplot2::aes(x = MAD)) + ggplot2::geom_histogram() +
    ggplot2::theme_classic() + ggplot2::labs(title = filelist[file], 
                                             x = expression(paste("MAD (m", italic("g"), ")")), y = "Frequency")
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/histograms/Accelerometer data/MAD/wrist/", filelist[file], ".jpeg"), i, width = 5, height = 5, dpi = 200) #saves g
  
  q <- ggplot2::ggplot(data = epochdata$agg.epoch, mapping = ggplot2::aes(sample = MAD)) + ggplot2::geom_qq() +
    ggplot2::theme_classic() + ggplot2::stat_qq() + ggplot2::stat_qq_line()
  ggplot2::ggsave(file=paste0(savedir, "/plots/normality/qq-plot/Accelerometer data/MAD/wrist/", filelist[file], ".jpeg"), q, width = 5, height = 5, dpi = 200) #saves g
}

