## User settings
# Directory path and name of castor export file
filepath.castor <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Castor export"
filename.castor <- "/20230918/Study/My_Little_Moves_export_20230918.csv"
# Directory path and name of app data (frequency and duration per day)
filepath.app <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/App-data/20230918"
filename.app <- "/20230918_MLMapp_activiteit_gedrag_duur_per_dag.csv"
# Directory path for epochdata hip and wrist
epochdir.hip <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/5sec/epochdata/hip/nonwear_removed"
epochdir.wrist <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/5sec/epochdata/wrist/nonwear_removed"

# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/AxivityMLMapp/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

# TO DO: 
# - check normality of continuous data using Shaphiro-Wilks test (accelerometry)
# - check normality of app data
# - split analyses from step 3 into age groups/milestone groups?

# Load in Castor data (this file includes participant characteristics)
data.castor <- load.castor(filepath.castor, filename.castor, cohort = 3)

### Axivity ###
## Step 1: Determine the length of one day of data
# Number of accelerometer files at start
filelist.hip <- list.files(epochdir.hip, pattern = ".RData")
filelist.wrist <- list.files(epochdir.hip, pattern = ".RData")
paste0("accelerometer files hip: ", length(filelist.hip))
paste0("accelerometer files wrist: ", length(filelist.wrist))

# Select files that provide => 8 hours on one day
savedir.hip <- paste0(epochdir.hip, "/1d8h")
validday.axivity(epochdir.hip, epochlength = 5, minhours = 8, mindays = 1, savedir = savedir.hip, data.castor) 
savedir.wrist <- paste0(epochdir.wrist, "/1d8h")
validday.axivity(epochdir.wrist, epochlength = 5, minhours = 8, mindays = 1, savedir = savedir.wrist, data.castor)
paste0("hip: accelerometer files that provide > 8 h on one day: ", length(list.files(savedir.hip, pattern = ".RData")))
paste0("wrist: accelerometer files that provide > 8 h on one day: ", length(list.files(savedir.wrist, pattern = ".RData")))

# Apply the 70/80 rule
weartime.hip <- apply.7080rule(savedir.hip, method = "axivity")
paste0("minimum required wear time hip: ", weartime.hip, " min (", weartime.hip/60, " h)")
weartime.wrist <- apply.7080rule(savedir.wrist, method = "axivity")
paste0("minimum required wear time wrist: ", weartime.wrist, " min (", weartime.wrist/60, " h)")

## Step 2: Preparing data for analyses - Process the data for files with 7 complete days
savedir.hip <- paste0(epochdir.hip, "/7d16h")
validday.axivity(epochdir = epochdir.hip, epochlength = 5, minhours = 16, mindays = 7, savedir = savedir.hip, data.castor, analysis = "reliability")
paste0("hip: accelerometer files that provide 7 days of at least 16 h: ", length(list.files(savedir.hip, pattern = ".RData")))
savedir.wrist <- paste0(epochdir.wrist, "/7d16h")
validday.axivity(epochdir = epochdir.wrist, epochlength = 5, minhours = 16, mindays = 7, savedir = savedir.wrist, data.castor, analysis = "reliability") 
paste0("wrist: accelerometer files that provide 7 days of at least 16 h: ", length(list.files(savedir.wrist, pattern = ".RData")))

# Generate the outcomes per accelerometer recording (per day)
axivity.estimates.hip <- estimates.axivity(savedir.hip) 
axivity.estimates.wrist <- estimates.axivity(savedir.wrist)

## Step 3: Weekend day inclusion?
library(dplyr)
# Hip
desc.weekend.hip <- group_by(axivity.estimates.hip, weekday) %>% 
  summarise_at(c("ENMO", "ENMO_25", "ig_ENMO", "M60_ENMO", "M30_ENMO", "M10_ENMO", "L5_ENMO",
                 "MAD", "MAD_25", "ig_MAD", "M60_MAD", "M30_MAD", "M10_MAD", "L5_MAD"),
               list(median = median, IQR = quantile), na.rm = TRUE)
View(desc.weekend.hip[c(2,4,7,9),])

res_ENMO.hip <- wilcox.test(ENMO ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_ENMO.hip <- qnorm(res_ENMO.hip$p.value/2)
res_ENMO_25.hip <- wilcox.test(ENMO_25 ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_ENMO_25.hip <- qnorm(res_ENMO_25.hip$p.value/2)
res_ig_ENMO.hip <- wilcox.test(ig_ENMO ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_ig_ENMO.hip <- qnorm(res_ig_ENMO.hip$p.value/2)
res_M60_ENMO.hip <- wilcox.test(M60_ENMO ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_M60_ENMO.hip <- qnorm(res_M60_ENMO.hip$p.value/2)
res_M30_ENMO.hip <- wilcox.test(M30_ENMO ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_M30_ENMO.hip <- qnorm(res_M30_ENMO.hip$p.value/2)
res_M10_ENMO.hip <- wilcox.test(M10_ENMO ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_M10_ENMO.hip <- qnorm(res_M10_ENMO.hip$p.value/2)
res_L5_ENMO.hip <- wilcox.test(L5_ENMO ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_L5_ENMO.hip <- qnorm(res_L5_ENMO.hip$p.value/2)

res_MAD.hip <- wilcox.test(MAD ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_MAD.hip <- qnorm(res_MAD.hip$p.value/2)
res_MAD_25.hip <- wilcox.test(MAD_25 ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_MAD_25.hip <- qnorm(res_MAD_25.hip$p.value/2)
res_ig_MAD.hip <- wilcox.test(ig_MAD ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_ig_MAD.hip <- qnorm(res_ig_MAD.hip$p.value/2)
res_M60_MAD.hip <- wilcox.test(M60_MAD ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_M60_MAD.hip <- qnorm(res_M60_MAD.hip$p.value/2)
res_M30_MAD.hip <- wilcox.test(M30_MAD ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_M30_MAD.hip <- qnorm(res_M30_MAD.hip$p.value/2)
res_M10_MAD.hip <- wilcox.test(M10_MAD ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_M10_MAD.hip <- qnorm(res_M10_MAD.hip$p.value/2)
res_L5_MAD.hip <- wilcox.test(L5_MAD ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_L5_MAD.hip <- qnorm(res_L5_MAD.hip$p.value/2)

# Wrist
desc.weekend.wrist <- group_by(axivity.estimates.wrist, weekday) %>% 
  summarise_at(c("ENMO", "ENMO_25", "ig_ENMO", "M60_ENMO", "M30_ENMO", "M10_ENMO", "L5_ENMO",
                 "MAD", "MAD_25", "ig_MAD", "M60_MAD", "M30_MAD", "M10_MAD", "L5_MAD"),
               list(median = median, IQR = quantile), na.rm = TRUE)
View(desc.weekend.wrist[c(2,4,7,9),])

res_ENMO.wrist <- wilcox.test(ENMO ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_ENMO.wrist <- qnorm(res_ENMO.wrist$p.value/2)
res_ENMO_25.wrist <- wilcox.test(ENMO_25 ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_ENMO_25.wrist <- qnorm(res_ENMO_25.wrist$p.value/2)
res_ig_ENMO.wrist <- wilcox.test(ig_ENMO ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_ig_ENMO.wrist <- qnorm(res_ig_ENMO.wrist$p.value/2)
res_M60_ENMO.wrist <- wilcox.test(M60_ENMO ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_M60_ENMO.wrist <- qnorm(res_M60_ENMO.wrist$p.value/2)
res_M30_ENMO.wrist <- wilcox.test(M30_ENMO ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_M30_ENMO.wrist <- qnorm(res_M30_ENMO.wrist$p.value/2)
res_M10_ENMO.wrist <- wilcox.test(M10_ENMO ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_M10_ENMO.wrist <- qnorm(res_M10_ENMO.wrist$p.value/2)
res_L5_ENMO.wrist <- wilcox.test(L5_ENMO ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_L5_ENMO.wrist <- qnorm(res_L5_ENMO.wrist$p.value/2)

res_MAD.wrist <- wilcox.test(MAD ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_MAD.wrist <- qnorm(res_MAD.wrist$p.value/2)
res_MAD_25.wrist <- wilcox.test(MAD_25 ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_MAD_25.wrist <- qnorm(res_MAD_25.wrist$p.value/2)
res_ig_MAD.wrist <- wilcox.test(ig_MAD ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_ig_MAD.wrist <- qnorm(res_ig_MAD.wrist$p.value/2)
res_M60_MAD.wrist <- wilcox.test(M60_MAD ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_M60_MAD.wrist <- qnorm(res_M60_MAD.wrist$p.value/2)
res_M30_MAD.wrist <- wilcox.test(M30_MAD ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_M30_MAD.wrist <- qnorm(res_M30_MAD.wrist$p.value/2)
res_M10_MAD.wrist <- wilcox.test(M10_MAD ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_M10_MAD.wrist <- qnorm(res_M10_MAD.wrist$p.value/2)
res_L5_MAD.wrist <- wilcox.test(L5_MAD ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_L5_MAD.wrist <- qnorm(res_L5_MAD.wrist$p.value/2)

## Step 4: Day-to-day variability
# Hip
day_day_ENMO.hip <- friedman.test(y = axivity.estimates.hip$ENMO, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_ENMO_25.hip <- friedman.test(y = axivity.estimates.hip$ENMO_25, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_ig_ENMO.hip <- friedman.test(y = axivity.estimates.hip$ig_ENMO, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_M60_ENMO.hip <- friedman.test(y = axivity.estimates.hip$M60_ENMO, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_M30_ENMO.hip <- friedman.test(y = axivity.estimates.hip$M30_ENMO, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_M10_ENMO.hip <- friedman.test(y = axivity.estimates.hip$M10_ENMO, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_L5_ENMO.hip <- friedman.test(y = axivity.estimates.hip$L5_ENMO, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)

day_day_MAD.hip <- friedman.test(y = axivity.estimates.hip$MAD, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_MAD_25.hip <- friedman.test(y = axivity.estimates.hip$MAD_25, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_ig_MAD.hip <- friedman.test(y = axivity.estimates.hip$ig_MAD, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_M60_MAD.hip <- friedman.test(y = axivity.estimates.hip$M60_MAD, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_M30_MAD.hip <- friedman.test(y = axivity.estimates.hip$M30_MAD, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_M10_MAD.hip <- friedman.test(y = axivity.estimates.hip$M10_MAD, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_L5_MAD.hip <- friedman.test(y = axivity.estimates.hip$L5_MAD, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)

# Wrist
day_day_ENMO.wrist <- friedman.test(y = axivity.estimates.wrist$ENMO, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_ENMO_25.wrist <- friedman.test(y = axivity.estimates.wrist$ENMO_25, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_ig_ENMO.wrist <- friedman.test(y = axivity.estimates.wrist$ig_ENMO, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_M60_ENMO.wrist <- friedman.test(y = axivity.estimates.wrist$M60_ENMO, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_M30_ENMO.wrist <- friedman.test(y = axivity.estimates.wrist$M30_ENMO, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_M10_ENMO.wrist <- friedman.test(y = axivity.estimates.wrist$M10_ENMO, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_L5_ENMO.wrist <- friedman.test(y = axivity.estimates.wrist$L5_ENMO, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)

day_day_MAD.wrist <- friedman.test(y = axivity.estimates.wrist$MAD, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_MAD_25.wrist <- friedman.test(y = axivity.estimates.wrist$MAD_25, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_ig_MAD.wrist <- friedman.test(y = axivity.estimates.wrist$ig_MAD, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_M60_MAD.wrist <- friedman.test(y = axivity.estimates.wrist$M60_MAD, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_M30_MAD.wrist <- friedman.test(y = axivity.estimates.wrist$M30_MAD, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_M10_MAD.wrist <- friedman.test(y = axivity.estimates.wrist$M10_MAD, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_L5_MAD.wrist <- friedman.test(y = axivity.estimates.wrist$L5_MAD, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)

# No differences
# Differences: explore why / days due to illness??

## Step 5: Determine single day intra-class correlations
# Run intra-class correlations (ICC). Random consistency models, using single measure, ICC(2,1). Single measure is ICC for single day.
# Hip
ratings.hip  <- reshape(axivity.estimates.hip, idvar = "days", timevar = "id", direction = "wide")

ratings_ENMO.hip_long <- as.data.frame(cbind(axivity.estimates.hip$id, axivity.estimates.hip$days, axivity.estimates.hip$ENMO))
names(ratings_ENMO.hip_long) <- c("id", "days", "ENMO")
ratings_ENMO.hip_long$ENMO <- as.numeric(ratings_ENMO.hip_long$ENMO)
ratings_ENMO.hip  <- reshape(ratings_ENMO.hip_long, idvar = "days", timevar = "id", direction = "wide")
ICC.ENMO.hip <- irr::icc(ratings_ENMO.hip[,-1], model = "twoway", type = "consistency")

ratings_ENMO_25.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "ENMO_25.")]
ICC.ENMO_25.hip <- irr::icc(ratings_ENMO_25.hip_long[,-1], model = "twoway", type = "consistency")
ratings_ig_ENMO.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "ig_ENMO.")]
ratings_ig_ENMO.hip_long <- ratings_ig_ENMO.hip_long[ , colSums(is.na(ratings_ig_ENMO.hip_long))==0] #omit NA columns
ICC.ig_ENMO.hip <- irr::icc(ratings_ig_ENMO.hip_long[,-1], model = "twoway", type = "consistency")
ratings_M60_ENMO.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "M60_ENMO.")]
ratings_M60_ENMO.hip_long <- ratings_M60_ENMO.hip_long[ , colSums(is.na(ratings_M60_ENMO.hip_long))==0] #omit NA columns
ICC.M60_ENMO.hip <- irr::icc(ratings_M60_ENMO.hip_long[,-1], model = "twoway", type = "consistency")
ratings_M30_ENMO.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "M30_ENMO.")]
ratings_M30_ENMO.hip_long <- ratings_M30_ENMO.hip_long[ , colSums(is.na(ratings_M30_ENMO.hip_long))==0] #omit NA columns
ICC.M30_ENMO.hip <- irr::icc(ratings_M30_ENMO.hip_long[,-1], model = "twoway", type = "consistency")
ratings_M10_ENMO.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "M10_ENMO.")]
ratings_M10_ENMO.hip_long <- ratings_M10_ENMO.hip_long[ , colSums(is.na(ratings_M10_ENMO.hip_long))==0] #omit NA columns
ICC.M10_ENMO.hip <- irr::icc(ratings_M10_ENMO.hip_long[,-1], model = "twoway", type = "consistency")
ratings_L5_ENMO.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "L5_ENMO.")]
ratings_L5_ENMO.hip_long <- ratings_L5_ENMO.hip_long[ , colSums(is.na(ratings_L5_ENMO.hip_long))==0] #omit NA columns
ICC.L5_ENMO.hip <- irr::icc(ratings_L5_ENMO.hip_long[,-1], model = "twoway", type = "consistency")

ratings_MAD.hip_long <- as.data.frame(cbind(axivity.estimates.hip$id, axivity.estimates.hip$days, axivity.estimates.hip$MAD))
names(ratings_MAD.hip_long) <- c("id", "days", "MAD")
ratings_MAD.hip_long$MAD <- as.numeric(ratings_MAD.hip_long$MAD)
ratings_MAD.hip  <- reshape(ratings_MAD.hip_long, idvar = "days", timevar = "id", direction = "wide")
ICC.MAD.hip <- irr::icc(ratings_MAD.hip[,-1], model = "twoway", type = "consistency")

ratings_MAD_25.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "MAD_25.")]
ICC.MAD_25.hip <- irr::icc(ratings_MAD_25.hip_long[,-1], model = "twoway", type = "consistency")
ratings_ig_MAD.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "ig_MAD.")]
ratings_ig_MAD.hip_long <- ratings_ig_MAD.hip_long[ , colSums(is.na(ratings_ig_MAD.hip_long))==0] #omit NA columns
ICC.ig_MAD.hip <- irr::icc(ratings_ig_MAD.hip_long[,-1], model = "twoway", type = "consistency")
ratings_M60_MAD.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "M60_MAD.")]
ratings_M60_MAD.hip_long <- ratings_M60_MAD.hip_long[ , colSums(is.na(ratings_M60_MAD.hip_long))==0] #omit NA columns
ICC.M60_MAD.hip <- irr::icc(ratings_M60_MAD.hip_long[,-1], model = "twoway", type = "consistency")
ratings_M30_MAD.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "M30_MAD.")]
ratings_M30_MAD.hip_long <- ratings_M30_MAD.hip_long[ , colSums(is.na(ratings_M30_MAD.hip_long))==0] #omit NA columns
ICC.M30_MAD.hip <- irr::icc(ratings_M30_MAD.hip_long[,-1], model = "twoway", type = "consistency")
ratings_M10_MAD.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "M10_MAD.")]
ratings_M10_MAD.hip_long <- ratings_M10_MAD.hip_long[ , colSums(is.na(ratings_M10_MAD.hip_long))==0] #omit NA columns
ICC.M10_MAD.hip <- irr::icc(ratings_M10_MAD.hip_long[,-1], model = "twoway", type = "consistency")
ratings_L5_MAD.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "L5_MAD.")]
ratings_L5_MAD.hip_long <- ratings_L5_MAD.hip_long[ , colSums(is.na(ratings_L5_MAD.hip_long))==0] #omit NA columns
ICC.L5_MAD.hip <- irr::icc(ratings_L5_MAD.hip_long[,-1], model = "twoway", type = "consistency")

# Wrist
ratings.wrist  <- reshape(axivity.estimates.wrist, idvar = "days", timevar = "id", direction = "wide")
#TO DO change ratings for all metrics similar to 228t/m232
ratings_ENMO.wrist_long <- as.data.frame(cbind(axivity.estimates.wrist$id, axivity.estimates.wrist$days, axivity.estimates.wrist$ENMO))
names(ratings_ENMO.wrist_long) <- c("id", "days", "ENMO")
ratings_ENMO.wrist_long$ENMO <- as.numeric(ratings_ENMO.wrist_long$ENMO)
ratings_ENMO.wrist  <- reshape(ratings_ENMO.wrist_long, idvar = "days", timevar = "id", direction = "wide")
ICC.ENMO.wrist <- irr::icc(ratings_ENMO.wrist[,-1], model = "twoway", type = "consistency")

ratings_ENMO_25.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "ENMO_25.")]
ICC.ENMO_25.wrist <- irr::icc(ratings_ENMO_25.wrist_long[,-1], model = "twoway", type = "consistency")
ratings_ig_ENMO.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "ig_ENMO.")]
ratings_ig_ENMO.wrist_long <- ratings_ig_ENMO.wrist_long[ , colSums(is.na(ratings_ig_ENMO.wrist_long))==0] #omit NA columns
ICC.ig_ENMO.wrist <- irr::icc(ratings_ig_ENMO.wrist_long[,-1], model = "twoway", type = "consistency")
ratings_M60_ENMO.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "M60_ENMO.")]
ratings_M60_ENMO.wrist_long <- ratings_M60_ENMO.wrist_long[ , colSums(is.na(ratings_M60_ENMO.wrist_long))==0] #omit NA columns
ICC.M60_ENMO.wrist <- irr::icc(ratings_M60_ENMO.wrist_long[,-1], model = "twoway", type = "consistency")
ratings_M30_ENMO.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "M30_ENMO.")]
ratings_M30_ENMO.wrist_long <- ratings_M30_ENMO.wrist_long[ , colSums(is.na(ratings_M30_ENMO.wrist_long))==0] #omit NA columns
ICC.M30_ENMO.wrist <- irr::icc(ratings_M30_ENMO.wrist_long[,-1], model = "twoway", type = "consistency")
ratings_M10_ENMO.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "M10_ENMO.")]
ratings_M10_ENMO.wrist_long <- ratings_M10_ENMO.wrist_long[ , colSums(is.na(ratings_M10_ENMO.wrist_long))==0] #omit NA columns
ICC.M10_ENMO.wrist <- irr::icc(ratings_M10_ENMO.wrist_long[,-1], model = "twoway", type = "consistency")
ratings_L5_ENMO.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "L5_ENMO.")]
ratings_L5_ENMO.wrist_long <- ratings_L5_ENMO.wrist_long[ , colSums(is.na(ratings_L5_ENMO.wrist_long))==0] #omit NA columns
ICC.L5_ENMO.wrist <- irr::icc(ratings_L5_ENMO.wrist_long[,-1], model = "twoway", type = "consistency")

ratings_MAD.wrist_long <- as.data.frame(cbind(axivity.estimates.wrist$id, axivity.estimates.wrist$days, axivity.estimates.wrist$MAD))
names(ratings_MAD.wrist_long) <- c("id", "days", "MAD")
ratings_MAD.wrist_long$MAD <- as.numeric(ratings_MAD.wrist_long$MAD)
ratings_MAD.wrist  <- reshape(ratings_MAD.wrist_long, idvar = "days", timevar = "id", direction = "wide")
ICC.MAD.wrist <- irr::icc(ratings_MAD.wrist[,-1], model = "twoway", type = "consistency")

ratings_MAD_25.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "MAD_25.")]
ICC.MAD_25.wrist <- irr::icc(ratings_MAD_25.wrist_long[,-1], model = "twoway", type = "consistency")
ratings_ig_MAD.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "ig_MAD.")]
ratings_ig_MAD.wrist_long <- ratings_ig_MAD.wrist_long[ , colSums(is.na(ratings_ig_MAD.wrist_long))==0] #omit NA columns
ICC.ig_MAD.wrist <- irr::icc(ratings_ig_MAD.wrist_long[,-1], model = "twoway", type = "consistency")
ratings_M60_MAD.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "M60_MAD.")]
ratings_M60_MAD.wrist_long <- ratings_M60_MAD.wrist_long[ , colSums(is.na(ratings_M60_MAD.wrist_long))==0] #omit NA columns
ICC.M60_MAD.wrist <- irr::icc(ratings_M60_MAD.wrist_long[,-1], model = "twoway", type = "consistency")
ratings_M30_MAD.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "M30_MAD.")]
ratings_M30_MAD.wrist_long <- ratings_M30_MAD.wrist_long[ , colSums(is.na(ratings_M30_MAD.wrist_long))==0] #omit NA columns
ICC.M30_MAD.wrist <- irr::icc(ratings_M30_MAD.wrist_long[,-1], model = "twoway", type = "consistency")
ratings_M10_MAD.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "M10_MAD.")]
ratings_M10_MAD.wrist_long <- ratings_M10_MAD.wrist_long[ , colSums(is.na(ratings_M10_MAD.wrist_long))==0] #omit NA columns
ICC.M10_MAD.wrist <- irr::icc(ratings_M10_MAD.wrist_long[,-1], model = "twoway", type = "consistency")
ratings_L5_MAD.wrist_long <- ratings.wrist[,stringr::str_detect(names(ratings.wrist), "L5_MAD.")]
ratings_L5_MAD.wrist_long <- ratings_L5_MAD.wrist_long[ , colSums(is.na(ratings_L5_MAD.wrist_long))==0] #omit NA columns
ICC.L5_MAD.wrist <- irr::icc(ratings_L5_MAD.wrist_long[,-1], model = "twoway", type = "consistency")

## Step 6: Determine the number of days to reach reliability
# Run the Spearman-Brown prophecy formula inputting single day ICC’s for each outcome and defining
# Hip
spearman.brown(singleICC = ICC.ENMO.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.ENMO_25.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.ig_ENMO.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M60_ENMO.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M30_ENMO.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M10_ENMO.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.L5_ENMO.hip$value, desiredR = 0.7)

spearman.brown(singleICC = ICC.MAD.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.MAD_25.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.ig_MAD.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M60_MAD.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M30_MAD.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M10_MAD.hip$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.L5_MAD.hip$value, desiredR = 0.7)

# Wrist
spearman.brown(singleICC = ICC.ENMO.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.ENMO_25.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.ig_ENMO.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M60_ENMO.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M30_ENMO.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M10_ENMO.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.L5_ENMO.wrist$value, desiredR = 0.7)

spearman.brown(singleICC = ICC.MAD.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.MAD_25.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.ig_MAD.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M60_MAD.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M30_MAD.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.M10_MAD.wrist$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.L5_MAD.wrist$value, desiredR = 0.7)


### My Little Moves app ###
# Load app data for all cohorts
data.app <- load.app(filepath.app, filename.app, cohort = c(1,2,3), measurementperiod = 1, sep = ";")

## Step 1: Determine the length of one day of data
# Number of participants at start
paste0("number of participants: ", length(unique(data.app$castorID)))

# Select files that provide => 8 hours on one day
valid_data_8h1d <- validday.app(data.app, minhours = 8, mindays = 1, savedir = filepath.app) 
paste0("participants provide > 8 h on one day: ", length(unique(valid_data_8h1d$castorID)))

# Apply the 70/80 rule
minimumtime <- apply.7080rule(method = "app", data.app = valid_data_8h1d)
paste0("minimum time app: ", minimumtime, " min (", minimumtime/60, " h)")

## Step 2: Preparing data for analyses - Process the data for files with 7 complete days
valid_data_16h7d <- validday.app(data.app, minhours = 16, mindays = 7, savedir = filepath.app) 
paste0("participants provide > 16 h on 7 day: ", length(unique(valid_data_16h7d$castorID)))

## Step 3: Weekend day inclusion?
# Descriptives
library(dplyr)
desc.weekend.app <- group_by(valid_data_16h7d, weekenddag) %>% 
  summarise_at(c("PA", "SB", "sleep"),
               list(median = median, IQR = quantile), na.rm = TRUE)
View(desc.weekend.app[c(2,4,7,9),])

res_PA <- wilcox.test(PA ~ weekenddag, data = valid_data_16h7d, paired = FALSE)
z_PA <- qnorm(res_PA$p.value/2)
res_SB <- wilcox.test(SB ~ weekenddag, data = valid_data_16h7d, paired = FALSE)
z_SB <- qnorm(res_SB$p.value/2)
res_sleep <- wilcox.test(sleep ~ weekenddag, data = valid_data_16h7d, paired = FALSE)
z_sleep <- qnorm(res_sleep$p.value/2)

## Step 4: Day-to-day variability
day_day_PA <- friedman.test(y = valid_data_16h7d$PA, groups = valid_data_16h7d$dag, blocks = valid_data_16h7d$castorID)
day_day_SB <- friedman.test(y = valid_data_16h7d$SB, groups = valid_data_16h7d$dag, blocks = valid_data_16h7d$castorID)
day_day_sleep <- friedman.test(y = valid_data_16h7d$sleep, groups = valid_data_16h7d$dag, blocks = valid_data_16h7d$castorID)

# No differences

## Step 5: Determine single day intra-class correlations
# Run intra-class correlations (ICC). Random consistency models, using single measure, ICC(2,1). Single measure is ICC for single day.
ratings_PA_long <- as.data.frame(cbind(valid_data_16h7d$castorID, valid_data_16h7d$dag, valid_data_16h7d$PA))
names(ratings_PA_long) <- c("id", "days", "PA")
ratings_PA_long$PA <- as.numeric(ratings_PA_long$PA)
ratings_PA_long$days <- as.factor(ratings_PA_long$days)
ratings_PA_long$id <- as.factor(ratings_PA_long$id)
ratings_PA  <- reshape(ratings_PA_long, idvar = "id", timevar = "days", direction = "wide")

ratings_SB_long <- as.data.frame(cbind(valid_data_16h7d$castorID, valid_data_16h7d$dag, valid_data_16h7d$SB))
names(ratings_SB_long) <- c("id", "days", "SB")
ratings_SB_long$SB <- as.numeric(ratings_SB_long$SB)
ratings_SB_long$days <- as.factor(ratings_SB_long$days)
ratings_SB_long$id <- as.factor(ratings_SB_long$id)
ratings_SB  <- reshape(ratings_SB_long, idvar = "id", timevar = "days", direction = "wide")

ratings_sleep_long <- as.data.frame(cbind(valid_data_16h7d$castorID, valid_data_16h7d$dag, valid_data_16h7d$sleep))
names(ratings_sleep_long) <- c("id", "days", "sleep")
ratings_sleep_long$sleep <- as.numeric(ratings_sleep_long$sleep)
ratings_sleep_long$days <- as.factor(ratings_sleep_long$days)
ratings_sleep_long$id <- as.factor(ratings_sleep_long$id)
ratings_sleep  <- reshape(ratings_sleep_long, idvar = "id", timevar = "days", direction = "wide")

ICC.PA <- irr::icc(ratings_PA[,-1], model = "twoway", type = "consistency")
ICC.SB <- irr::icc(ratings_SB[,-1], model = "twoway", type = "consistency")
ICC.sleep <- irr::icc(ratings_sleep[,-1], model = "twoway", type = "consistency")

## Step 6: Determine the number of days to reach reliability
# Run the Spearman-Brown prophecy formula inputting single day ICC’s for each outcome and defining
spearman.brown(singleICC = ICC.PA$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.SB$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.sleep$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.PA$value, desiredR = 0.75)
spearman.brown(singleICC = ICC.SB$value, desiredR = 0.75)
spearman.brown(singleICC = ICC.sleep$value, desiredR = 0.75)
spearman.brown(singleICC = ICC.PA$value, desiredR = 0.8)
spearman.brown(singleICC = ICC.SB$value, desiredR = 0.8)
spearman.brown(singleICC = ICC.sleep$value, desiredR = 0.8)