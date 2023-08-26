# User settings
# filepath.app <- "M:/projecten/2019/My Little Moves/Dataverzameling/Data/My Little Moves app/cleanMLMappdata/"
# filename.app <- "20230209_activities_castor_linked.csv" # contains all filled in activities
# filepath.castor <- "M:/projecten/2019/My Little Moves/Dataverzameling/Data/Castor/20230209/"
# filename.castor <- "Study/My_Little_Moves_export_20230209.csv"
# filepath.axivity <- "M:/projecten/2019/My Little Moves/Dataverzameling/Data/Accelerometer/Measurement period 1/"

filepath.app <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/App-data/13dec2022"
filename.app <- "/20230206_activities_castor_linked.csv"
filepath.castor <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Castor export"
filename.castor <- "/20230206/Study/My_Little_Moves_export_20230206.csv"
filepath.axivity <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1"

# TO DO: load all .R scripts

## Load data
data.castor <- load.castor(filepath.castor, filename.castor, cohort = 3)

## App data
data.app <- load.app(filepath.app, filename.app, cohort = 3) # TO DO: app data per dag

## Axivity
# Load and structure acceleration data into 5-sec epochs
outputdir <- paste0(filepath.axivity, "/5sec")
filepath.processedData <- paste0(outputdir, "/output_Measurement1")

# start <- as.POSIXct(data.castor$Date_measurement_period_1, format="%d-%m-%Y") # Initial accelerometer start date
# comments.axivity <- data.castor$Comments_Acc_1 # Comments
# #start[7] <- as.POSIXct("21-10-2022", format="%d-%m-%Y") # Change start date of [7] to 21-10-2022
# start.axivity <- list(id = id, date = start)
load.structure.axivity(filepath = filepath.axivity, outputdir = outputdir, processeddir = filepath.processedData) 

## Step 1: Determine the length of one day of data
epochdir.hip <- paste0(filepath.processedData, "/epochdata/hip")
epochdir.wrist <- paste0(filepath.processedData, "/epochdata/wrist")

# Number of accelerometer files at start
filelist.hip <- list.files(epochdir.hip, pattern = ".RData")
filelist.wrist <- list.files(epochdir.hip, pattern = ".RData")
paste0("accelerometer files hip: ", length(filelist.hip))
paste0("accelerometer files wrist: ", length(filelist.wrist))

# Select files that provide > 8 hours on one day
savedir.hip <- paste0(epochdir.hip, "/8h1d")
preprocess.axivity(epochdir.hip, epochlength = 5, minhours = 8, mindays = 1, savedir = savedir.hip, castordata = data.castor) 

savedir.wrist <- paste0(epochdir.wrist, "/8h1d")
preprocess.axivity(epochdir.wrist, epochlength = 5, minhours = 8, mindays = 1, savedir = savedir.wrist, castordata = data.castor)

paste0("hip: accelerometer files that provide > 8 h on one day: ", length(list.files(savedir.hip, pattern = ".RData")))
paste0("wrist: accelerometer files that provide > 8 h on one day: ", length(list.files(savedir.wrist, pattern = ".RData")))

# Apply the 70/80 rule
weartime.hip <- apply.7080rule(savedir.hip)
paste0("minimum required wear time hip: ", weartime.hip, " min (", weartime.hip/60, " h)")
weartime.wrist <- apply.7080rule(savedir.wrist)
paste0("minimum required wear time wrist: ", weartime.wrist, " min (", weartime.wrist/60, " h)")

## Step 2: Preparing data for analyses - Process the data for files with 7 complete days
savedir.hip <- paste0(epochdir.hip, "/17h7d")
preprocess.axivity(epochdir = epochdir.hip, epochlength = 5, minhours = 17, mindays = 7, savedir = savedir.hip, castordata = data.castor)
paste0("hip: accelerometer files that provide 7 days of at least 17 h: ", length(list.files(savedir.hip, pattern = ".RData")))
axivity.outcomes.hip <- outcomes.axivity(savedir.hip) # Generate the outcomes per accelerometer recording (per day)

savedir.wrist <- paste0(epochdir.wrist, "/17h7d")
preprocess.axivity(epochdir = epochdir.wrist, epochlength = 5, minhours = 17, mindays = 7, savedir = savedir.wrist, castordata = data.castor) 
paste0("wrist: accelerometer files that provide 7 days of at least 17 h: ", length(list.files(savedir.wrist, pattern = ".RData")))
axivity.outcomes.wrist <- outcomes.axivity(savedir.wrist) # Generate the outcomes per accelerometer recording (per day)

# Step 3: Weekend day inclusion?
library(dplyr)
# Hip
group_by(axivity.outcomes.hip, weekday) %>% 
  summarise_at(c("ENMO_25", "avg_ENMO"), # c("acc_x_25", "acc_y_25", "acc_z_25", "ENMO_25", 
               #                  "avg_acc_x", "avg_acc_y", "avg_acc_z", "avg_ENMO")
               list(median = median, IQR = quantile))
# res_hip_acc_x_25 <- wilcox.test(acc_x_25 ~ weekday, data = axivity.outcomes.hip, paired = FALSE)
# res_hip_acc_y_25 <- wilcox.test(acc_y_25 ~ weekday, data = axivity.outcomes.hip, paired = FALSE)
# res_hip_acc_z_25 <- wilcox.test(acc_z_25 ~ weekday, data = axivity.outcomes.hip, paired = FALSE)
res_hip_ENMO_25 <- wilcox.test(ENMO_25 ~ weekday, data = axivity.outcomes.hip, paired = FALSE)
z_ENMO_25 <- qnorm(res_hip_ENMO_25$p.value/2)
# res_hip_avg_acc_x <- wilcox.test(avg_acc_x ~ weekday, data = axivity.outcomes.hip, paired = FALSE)
# res_hip_avg_acc_y <- wilcox.test(avg_acc_y ~ weekday, data = axivity.outcomes.hip, paired = FALSE)
# res_hip_avg_acc_z <- wilcox.test(avg_acc_z ~ weekday, data = axivity.outcomes.hip, paired = FALSE)
res_hip_avg_ENMO <- wilcox.test(avg_ENMO ~ weekday, data = axivity.outcomes.hip, paired = FALSE)
z_avg_ENMO <- qnorm(res_hip_avg_ENMO$p.value/2)

# Wrist
group_by(axivity.outcomes.wrist, weekday) %>% 
  summarise_at(c("ENMO_25", "avg_ENMO"), 
               list(median = median, IQR = quantile))
# res_wrist_acc_x_25 <- wilcox.test(acc_x_25 ~ weekday, data = axivity.outcomes.wrist, paired = FALSE)
# res_wrist_acc_y_25 <- wilcox.test(acc_y_25 ~ weekday, data = axivity.outcomes.wrist, paired = FALSE)
# res_wrist_acc_z_25 <- wilcox.test(acc_z_25 ~ weekday, data = axivity.outcomes.wrist, paired = FALSE)
res_wrist_ENMO_25 <- wilcox.test(ENMO_25 ~ weekday, data = axivity.outcomes.wrist, paired = FALSE)
z_ENMO_25 <- qnorm(res_wrist_ENMO_25$p.value/2)
# res_wrist_avg_acc_x <- wilcox.test(avg_acc_x ~ weekday, data = axivity.outcomes.wrist, paired = FALSE)
# res_wrist_avg_acc_y <- wilcox.test(avg_acc_y ~ weekday, data = axivity.outcomes.wrist, paired = FALSE)
# res_wrist_avg_acc_z <- wilcox.test(avg_acc_z ~ weekday, data = axivity.outcomes.wrist, paired = FALSE)
res_wrist_avg_ENMO <- wilcox.test(avg_ENMO ~ weekday, data = axivity.outcomes.wrist, paired = FALSE) 
z_avg_ENMO <- qnorm(res_wrist_avg_ENMO$p.value/2)

#% Step 4: Day-to-day variability
# Hip
day_day_ENMO_25.hip <- friedman.test(y = axivity.outcomes.hip$ENMO_25, groups = axivity.outcomes.hip$days, blocks = axivity.outcomes.hip$id)
day_day_ENMO.hip <- friedman.test(y = axivity.outcomes.hip$avg_ENMO, groups = axivity.outcomes.hip$days, blocks = axivity.outcomes.hip$id)

# Wrist
day_day_ENMO_25.wrist <- friedman.test(y = axivity.outcomes.wrist$ENMO_25, groups = axivity.outcomes.wrist$days, blocks = axivity.outcomes.wrist$id)
day_day_ENMO.wrist <- friedman.test(y = axivity.outcomes.wrist$avg_ENMO, groups = axivity.outcomes.wrist$days, blocks = axivity.outcomes.wrist$id)

# No differences
# Differences: explore why / days due to illnes??

## Step 5: Determine single day intra-class correlations
# Run intra-class correlations (ICC). Random consistency models, using single measure, ICC(2,1). Single measure is ICC for single day.
# Hip
ratings_avgENMO.hip_long <- as.data.frame(cbind(axivity.outcomes.hip$id, axivity.outcomes.hip$days, axivity.outcomes.hip$avg_ENMO))
names(ratings_avgENMO.hip_long) <- c("id", "days", "avgENMO")
ratings_avgENMO.hip_long$avgENMO <- as.numeric(ratings_avgENMO.hip_long$avgENMO)
ratings_avgENMO.hip  <- reshape(ratings_avgENMO.hip_long, idvar = "days", timevar = "id", direction = "wide")
ICC.avgENMO.hip <- irr::icc(ratings_avgENMO.hip[,-1], model = "twoway", type = "agreement")


ratings_25ENMO.hip_long <- as.data.frame(cbind(axivity.outcomes.hip$id, axivity.outcomes.hip$days, axivity.outcomes.hip$ENMO_25))
names(ratings_25ENMO.hip_long) <- c("id", "days", "ENMO25")
ratings_25ENMO.hip_long$ENMO25 <- as.numeric(ratings_25ENMO.hip_long$ENMO25)
ratings_25ENMO.hip  <- reshape(ratings_25ENMO.hip_long, idvar = "days", timevar = "id", direction = "wide")
ICC.25ENMO.hip <- irr::icc(ratings_25ENMO.hip[,-1], model = "twoway", type = "agreement")

# Wrist
ratings_avgENMO.wrist_long <- as.data.frame(cbind(axivity.outcomes.wrist$id, axivity.outcomes.wrist$days, axivity.outcomes.wrist$avg_ENMO))
names(ratings_avgENMO.wrist_long) <- c("id", "days", "avgENMO")
ratings_avgENMO.wrist_long$avgENMO <- as.numeric(ratings_avgENMO.wrist_long$avgENMO)
ratings_avgENMO.wrist  <- reshape(ratings_avgENMO.wrist_long, idvar = "days", timevar = "id", direction = "wide")
ICC.avgENMO.wrist <- irr::icc(ratings_avgENMO.wrist[,-1], model = "twoway", type = "agreement")

ratings_25ENMO.wrist_long <- as.data.frame(cbind(axivity.outcomes.wrist$id, axivity.outcomes.wrist$days, axivity.outcomes.wrist$ENMO_25))
names(ratings_25ENMO.wrist_long) <- c("id", "days", "ENMO25")
ratings_25ENMO.wrist_long$ENMO25 <- as.numeric(ratings_25ENMO.wrist_long$ENMO25)
ratings_25ENMO.wrist  <- reshape(ratings_25ENMO.wrist_long, idvar = "days", timevar = "id", direction = "wide")
ICC.25ENMO.wrist <- irr::icc(ratings_25ENMO.wrist[,-1], model = "twoway", type = "agreement")

## Step 6: Determine the number of days to reach reliability
# Run the Spearman-Brown prophecy formula inputting single day ICC’s for each outcome and defining
# Hip
spearman.brown(singleICC = ICC.avgENMO.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.25ENMO.hip$value, desiredR = 0.7)

# Wrist
spearman.brown(singleICC = ICC.avgENMO.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.25ENMO.wrist$value, desiredR = 0.7)
