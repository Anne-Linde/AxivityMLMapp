## User settings
# Directory path and name of castor export file
filepath.castor <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Castor export"
filename.castor <- "/20231007/Study/My_Little_Moves_export_20231007.csv"
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
# - split analyses from step 3 into age groups/milestone groups?



### Axivity ###
## Step 1: Determine the length of one day of data
# Number of accelerometer files at start
filelist.hip <- list.files(epochdir.hip, pattern = ".RData")
filelist.wrist <- list.files(epochdir.wrist, pattern = ".RData")
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
savedir.hip <- paste0(epochdir.hip, "/7d18h")
validday.axivity(epochdir = epochdir.hip, epochlength = 5, minhours = 18, mindays = 7, savedir = savedir.hip, data.castor, analysis = "reliability")
paste0("hip: accelerometer files that provide 7 days of at least 18 h: ", length(list.files(savedir.hip, pattern = ".RData")))
savedir.wrist <- paste0(epochdir.wrist, "/7d18h")
validday.axivity(epochdir = epochdir.wrist, epochlength = 5, minhours = 18, mindays = 7, savedir = savedir.wrist, data.castor, analysis = "reliability") 
paste0("wrist: accelerometer files that provide 7 days of at least 18 h: ", length(list.files(savedir.wrist, pattern = ".RData")))

## Generate the outcomes per accelerometer recording (per day)
axivity.estimates.hip <- estimates.axivity(savedir.hip) 
axivity.estimates.wrist <- estimates.axivity(savedir.wrist)

axivity.estimates.both <- merge(axivity.estimates.hip, axivity.estimates.wrist, by = c("id", "date"))
axivity.estimates.both$ENMO.both <- axivity.estimates.both$ENMO.x * axivity.estimates.both$ENMO.y
axivity.estimates.both$MAD.both <- axivity.estimates.both$MAD.x * axivity.estimates.both$MAD.y
axivity.estimates.both$ENMOig.both <- axivity.estimates.both$ig_ENMO.x * axivity.estimates.both$ig_ENMO.y
axivity.estimates.both$MADig.both <- axivity.estimates.both$ig_MAD.x * axivity.estimates.both$ig_MAD.y

# Generate per milestone group (no crawl, crawl and walk) as it is expected that the accelerometers record different accelerations 
milestones <- load.app(filepath.app, "/20230918_motormilestones_castor_linked.csv", cohort = c(3), measurementperiod = 1, sep = ",")
pp <- unique(axivity.estimates.hip$id)
crawl <- c()
walk <- c()
which(milestones$castorID %in% pp)
for (subj in 1:length(pp)) {
  tmp <- milestones[milestones$castorID==pp[subj],]
  if(tmp$age.group == 5 || tmp$age.group== 4){
    crawl <- c(crawl, 1)
    walk <- c(walk, 1)
  } else if(tmp$age.group == 1) {
    walk <- c(walk, 0)
    if(tmp$crawl == 1){
      crawl <- c(crawl, 1)
    } else {crawl <- c(crawl, 0)}  } 
  else {
    if(tmp$crawl == 1){
      crawl <- c(crawl, 1)
    } else {crawl <- c(crawl, 0)}
    if(tmp$walk == 1){
      walk <- c(walk, 1)
    } else {walk <- c(walk, 0)}
  }
}
tmp.hip <- as.data.frame(cbind(pp, crawl, walk))
R.nocrawl.hip <- tmp.hip$pp[which(tmp.hip$crawl == 0 & tmp.hip$walk == 0)]
R.crawl.hip <- tmp.hip$pp[which(tmp.hip$crawl == 1 & tmp.hip$walk == 0)]
R.walk.hip <- tmp.hip$pp[which(tmp.hip$crawl == 1 & tmp.hip$walk == 1)]
estimates.nocrawl.hip <- subset(axivity.estimates.hip, subset=axivity.estimates.hip$id %in% R.nocrawl.hip) 
estimates.crawl.hip <- subset(axivity.estimates.hip, subset=axivity.estimates.hip$id %in% R.crawl.hip) 
estimates.walk.hip <- subset(axivity.estimates.hip, subset=axivity.estimates.hip$id %in% R.walk.hip) 

pp <- unique(axivity.estimates.wrist$id)
crawl <- c()
walk <- c()
for (subj in 1:length(pp)) {
  tmp <- milestones[milestones$castorID==pp[subj],]
  if(tmp$age.group == 5 || tmp$age.group== 4){
    crawl <- c(crawl, 1)
    walk <- c(walk, 1)
  } else if(tmp$age.group == 1) {
    walk <- c(walk, 0)
    if(tmp$crawl == 1){
      crawl <- c(crawl, 1)
    } else {crawl <- c(crawl, 0)}  } 
  else {
    if(tmp$crawl == 1){
      crawl <- c(crawl, 1)
    } else {crawl <- c(crawl, 0)}
    if(tmp$walk == 1){
      walk <- c(walk, 1)
    } else {walk <- c(walk, 0)}
  }
}
tmp.wrist <- as.data.frame(cbind(pp, crawl, walk))
R.nocrawl.wrist <- tmp.wrist$pp[which(tmp.wrist$crawl == 0 & tmp.wrist$walk == 0)]
R.crawl.wrist <- tmp.wrist$pp[which(tmp.wrist$crawl == 1 & tmp.wrist$walk == 0)]
R.walk.wrist <- tmp.wrist$pp[which(tmp.wrist$crawl == 1 & tmp.wrist$walk == 1)]
estimates.nocrawl.wrist <- subset(axivity.estimates.wrist, subset=axivity.estimates.wrist$id %in% R.nocrawl.wrist) 
estimates.crawl.wrist <- subset(axivity.estimates.wrist, subset=axivity.estimates.wrist$id %in% R.crawl.wrist) 
estimates.walk.wrist <- subset(axivity.estimates.wrist, subset=axivity.estimates.wrist$id %in% R.walk.wrist) 


## Step 3: Weekend day inclusion?
library(dplyr)
# Hip
desc.weekend.hip <- group_by(axivity.estimates.hip, weekday) %>% 
  summarise_at(c("ENMO", "ig_ENMO", 
                 "MAD", "ig_MAD"),
               list(median = median, IQR = quantile), na.rm = TRUE)
View(desc.weekend.hip[c(2,4,7,9),])

res_ENMO.hip <- wilcox.test(ENMO ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_ENMO.hip <- qnorm(res_ENMO.hip$p.value/2)
res_ig_ENMO.hip <- wilcox.test(ig_ENMO ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_ig_ENMO.hip <- qnorm(res_ig_ENMO.hip$p.value/2)
res_MAD.hip <- wilcox.test(MAD ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_MAD.hip <- qnorm(res_MAD.hip$p.value/2)
res_ig_MAD.hip <- wilcox.test(ig_MAD ~ weekday, data = axivity.estimates.hip, paired = FALSE)
z_ig_MAD.hip <- qnorm(res_ig_MAD.hip$p.value/2)

# Split analyses for the milestone groups
desc.weekend.hip.nocrawl <- group_by(estimates.nocrawl.hip, weekday) %>% 
  summarise_at(c("ENMO", "ig_ENMO", 
                 "MAD", "ig_MAD"),
               list(median = median, IQR = quantile), na.rm = TRUE)
View(desc.weekend.hip.nocrawl[c(2,4,7,9),])

res_ENMO.hip.nocrawl <- wilcox.test(ENMO ~ weekday, data = estimates.nocrawl.hip, paired = FALSE)
z_ENMO.hip.nocrawl <- qnorm(res_ENMO.hip.nocrawl$p.value/2)
res_ig_ENMO.hip.nocrawl <- wilcox.test(ig_ENMO ~ weekday, data = estimates.nocrawl.hip, paired = FALSE)
z_ig_ENMO.hip.nocrawl <- qnorm(res_ig_ENMO.hip.nocrawl$p.value/2)
res_MAD.hip.nocrawl <- wilcox.test(MAD ~ weekday, data = estimates.nocrawl.hip, paired = FALSE)
z_MAD.hip.nocrawl <- qnorm(res_MAD.hip.nocrawl$p.value/2)
res_ig_MAD.hip.nocrawl <- wilcox.test(ig_MAD ~ weekday, data = estimates.nocrawl.hip, paired = FALSE)
z_ig_MAD.hip.nocrawl <- qnorm(res_ig_MAD.hip.nocrawl$p.value/2)

desc.weekend.hip.crawl <- group_by(estimates.crawl.hip, weekday) %>% 
  summarise_at(c("ENMO", "ig_ENMO", 
                 "MAD", "ig_MAD"),
               list(median = median, IQR = quantile), na.rm = TRUE)
View(desc.weekend.hip.crawl[c(2,4,7,9),])

res_ENMO.hip.crawl <- wilcox.test(ENMO ~ weekday, data = estimates.crawl.hip, paired = FALSE)
z_ENMO.hip.crawl <- qnorm(res_ENMO.hip.nocrawl$p.value/2)
res_ig_ENMO.hip.crawl <- wilcox.test(ig_ENMO ~ weekday, data = estimates.crawl.hip, paired = FALSE)
z_ig_ENMO.hip.crawl <- qnorm(res_ig_ENMO.hip.nocrawl$p.value/2)
res_MAD.hip.crawl <- wilcox.test(MAD ~ weekday, data = estimates.crawl.hip, paired = FALSE)
z_MAD.hip.crawl <- qnorm(res_MAD.hip.crawl$p.value/2)
res_ig_MAD.hip.crawl <- wilcox.test(ig_MAD ~ weekday, data = estimates.crawl.hip, paired = FALSE)
z_ig_MAD.hip.crawl <- qnorm(res_ig_MAD.hip.crawl$p.value/2)

desc.weekend.hip.walk <- group_by(estimates.walk.hip, weekday) %>% 
  summarise_at(c("ENMO", "ig_ENMO", 
                 "MAD", "ig_MAD"),
               list(median = median, IQR = quantile), na.rm = TRUE)
View(desc.weekend.hip.walk[c(2,4,7,9),])
res_ENMO.hip.walk <- wilcox.test(ENMO ~ weekday, data = estimates.walk.hip, paired = FALSE)
z_ENMO.hip.walk <- qnorm(res_ENMO.hip.nocrawl$p.value/2)
res_ig_ENMO.hip.walk <- wilcox.test(ig_ENMO ~ weekday, data = estimates.walk.hip, paired = FALSE)
z_ig_ENMO.hip.walk <- qnorm(res_ig_ENMO.hip.nocrawl$p.value/2)
res_MAD.hip.walk <- wilcox.test(MAD ~ weekday, data = estimates.walk.hip, paired = FALSE)
z_MAD.hip.walk <- qnorm(res_MAD.hip.walk$p.value/2)
res_ig_MAD.hip.walk <- wilcox.test(ig_MAD ~ weekday, data = estimates.walk.hip, paired = FALSE)
z_ig_MAD.hip.walk <- qnorm(res_ig_MAD.hip.walk$p.value/2)

# Wrist
desc.weekend.wrist <- group_by(axivity.estimates.wrist, weekday) %>% 
  summarise_at(c("ENMO", "ig_ENMO", 
                 "MAD", "ig_MAD"),
               list(median = median, IQR = quantile), na.rm = TRUE)
View(desc.weekend.wrist[c(2,4,7,9),])

res_ENMO.wrist <- wilcox.test(ENMO ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_ENMO.wrist <- qnorm(res_ENMO.wrist$p.value/2)
res_ig_ENMO.wrist <- wilcox.test(ig_ENMO ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_ig_ENMO.wrist <- qnorm(res_ig_ENMO.wrist$p.value/2)
res_MAD.wrist <- wilcox.test(MAD ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_MAD.wrist <- qnorm(res_MAD.wrist$p.value/2)
res_ig_MAD.wrist <- wilcox.test(ig_MAD ~ weekday, data = axivity.estimates.wrist, paired = FALSE)
z_ig_MAD.wrist <- qnorm(res_ig_MAD.wrist$p.value/2)

# Both
desc.weekend.both <- group_by(axivity.estimates.both, weekday.x) %>% 
  summarise_at(c("ENMO.both", "ENMOig.both", 
                 "MAD.both", "MADig.both"),
               list(median = median, IQR = quantile), na.rm = TRUE)
View(desc.weekend.both[c(2,4,7,9),])

res_ENMO.both <- wilcox.test(ENMO.both ~ weekday.x, data = axivity.estimates.both, paired = FALSE)
z_ENMO.both <- qnorm(res_ENMO.both$p.value/2)
res_ig_ENMO.both <- wilcox.test(ENMOig.both ~ weekday.x, data = axivity.estimates.both, paired = FALSE)
z_ig_ENMO.both <- qnorm(res_ig_ENMO.both$p.value/2)
res_MAD.both <- wilcox.test(MAD.both ~ weekday.x, data = axivity.estimates.both, paired = FALSE)
z_MAD.both <- qnorm(res_MAD.both$p.value/2)
res_ig_MAD.both <- wilcox.test(MADig.both ~ weekday.x, data = axivity.estimates.both, paired = FALSE)
z_ig_MAD.both <- qnorm(res_ig_MAD.both$p.value/2)

## Step 4: Day-to-day variability
# Hip
day_day_ENMO.hip <- friedman.test(y = axivity.estimates.hip$ENMO, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_ig_ENMO.hip <- friedman.test(y = axivity.estimates.hip$ig_ENMO, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_MAD.hip <- friedman.test(y = axivity.estimates.hip$MAD, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)
day_day_ig_MAD.hip <- friedman.test(y = axivity.estimates.hip$ig_MAD, groups = axivity.estimates.hip$days, blocks = axivity.estimates.hip$id)

# Wrist
day_day_ENMO.wrist <- friedman.test(y = axivity.estimates.wrist$ENMO, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_ig_ENMO.wrist <- friedman.test(y = axivity.estimates.wrist$ig_ENMO, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_MAD.wrist <- friedman.test(y = axivity.estimates.wrist$MAD, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)
day_day_ig_MAD.wrist <- friedman.test(y = axivity.estimates.wrist$ig_MAD, groups = axivity.estimates.wrist$days, blocks = axivity.estimates.wrist$id)

# Both
day_day_ENMO.both <- friedman.test(y = axivity.estimates.both$ENMO.both, groups = axivity.estimates.both$days.x, blocks = axivity.estimates.both$id)
day_day_ig_ENMO.both <- friedman.test(y = axivity.estimates.both$ENMOig.both, groups = axivity.estimates.both$days.x, blocks = axivity.estimates.both$id)
day_day_MAD.both <- friedman.test(y = axivity.estimates.both$MAD.both, groups = axivity.estimates.both$days.x, blocks = axivity.estimates.both$id)
day_day_ig_MAD.both <- friedman.test(y = axivity.estimates.both$MADig.both, groups = axivity.estimates.both$days.x, blocks = axivity.estimates.both$id)

# No differences

## Step 5: Determine single day intra-class correlations
# Run intra-class correlations (ICC). Random consistency models, using single measure, ICC(2,1). Single measure is ICC for single day.
# Hip
ratings.hip  <- reshape(axivity.estimates.hip, idvar = "days", timevar = "id", direction = "wide")
ratings_ENMO.hip_long <- as.data.frame(cbind(axivity.estimates.hip$id, axivity.estimates.hip$days, axivity.estimates.hip$ENMO))
names(ratings_ENMO.hip_long) <- c("id", "days", "ENMO")
ratings_ENMO.hip_long$ENMO <- as.numeric(ratings_ENMO.hip_long$ENMO)
ratings_ENMO.hip  <- reshape(ratings_ENMO.hip_long, idvar = "days", timevar = "id", direction = "wide")
ICC.ENMO.hip <- irr::icc(ratings_ENMO.hip[,-1], model = "twoway", type = "consistency")

ratings_ig_ENMO.hip_long <- as.data.frame(cbind(axivity.estimates.hip$id, axivity.estimates.hip$days, axivity.estimates.hip$ig_ENMO))
names(ratings_ig_ENMO.hip_long) <- c("id", "days", "ENMO")
ratings_ig_ENMO.hip_long$ENMO <- as.numeric(ratings_ig_ENMO.hip_long$ENMO)
ratings_igENMO.hip  <- reshape(ratings_ig_ENMO.hip_long, idvar = "days", timevar = "id", direction = "wide")
ICC.ig_ENMO.hip <- irr::icc(ratings_igENMO.hip[,-1], model = "twoway", type = "consistency")

ratings_ig_ENMO.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "ig_ENMO.")]
ratings_ig_ENMO.hip_long <- ratings_ig_ENMO.hip_long[ , colSums(is.na(ratings_ig_ENMO.hip_long))==0] #omit NA columns
ICC.ig_ENMO.hip <- irr::icc(ratings_ig_ENMO.hip_long[,-1], model = "twoway", type = "consistency")

ratings_MAD.hip_long <- as.data.frame(cbind(axivity.estimates.hip$id, axivity.estimates.hip$days, axivity.estimates.hip$MAD))
names(ratings_MAD.hip_long) <- c("id", "days", "MAD")
ratings_MAD.hip_long$MAD <- as.numeric(ratings_MAD.hip_long$MAD)
ratings_MAD.hip  <- reshape(ratings_MAD.hip_long, idvar = "days", timevar = "id", direction = "wide")
ICC.MAD.hip <- irr::icc(ratings_MAD.hip[,-1], model = "twoway", type = "consistency")

ratings_ig_MAD.hip_long <- ratings.hip[,stringr::str_detect(names(ratings.hip), "ig_MAD.")]
ratings_ig_MAD.hip_long <- ratings_ig_MAD.hip_long[ , colSums(is.na(ratings_ig_MAD.hip_long))==0] #omit NA columns
ICC.ig_MAD.hip <- irr::icc(ratings_ig_MAD.hip_long[,-1], model = "twoway", type = "consistency")


estimates.nocrawl.hip

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

# Both
ratings.both  <- reshape(axivity.estimates.both, idvar = "days.x", timevar = "id", direction = "wide")
ratings_ENMO.both_long <- as.data.frame(cbind(axivity.estimates.both$id, axivity.estimates.both$days.x, axivity.estimates.both$ENMO.both))
names(ratings_ENMO.both_long) <- c("id", "days", "ENMO")
ratings_ENMO.both_long$ENMO <- as.numeric(ratings_ENMO.both_long$ENMO)
ratings_ENMO.both  <- reshape(ratings_ENMO.both_long, idvar = "days", timevar = "id", direction = "wide")
ICC.ENMO.both <- irr::icc(ratings_ENMO.both[,-1], model = "twoway", type = "consistency")
ratings_ENMOig.both_long <- as.data.frame(cbind(axivity.estimates.both$id, axivity.estimates.both$days.x, axivity.estimates.both$ENMOig.both))
names(ratings_ENMOig.both_long) <- c("id", "days", "ENMO")
ratings_ENMOig.both_long$ENMO <- as.numeric(ratings_ENMOig.both_long$ENMO)
ratings_ENMOig.both  <- reshape(ratings_ENMOig.both_long, idvar = "days", timevar = "id", direction = "wide")
ICC.ENMOig.both <- irr::icc(ratings_ENMOig.both[,-1], model = "twoway", type = "consistency")
ratings_MAD.both_long <- as.data.frame(cbind(axivity.estimates.both$id, axivity.estimates.both$days.x, axivity.estimates.both$MAD.both))
names(ratings_MAD.both_long) <- c("id", "days", "MAD")
ratings_MAD.both_long$MAD <- as.numeric(ratings_MAD.both_long$MAD)
ratings_MAD.both  <- reshape(ratings_MAD.both_long, idvar = "days", timevar = "id", direction = "wide")
ICC.MAD.both <- irr::icc(ratings_MAD.both[,-1], model = "twoway", type = "consistency")
ratings_MADig.both_long <- as.data.frame(cbind(axivity.estimates.both$id, axivity.estimates.both$days.x, axivity.estimates.both$MADig.both))
names(ratings_MADig.both_long) <- c("id", "days", "MAD")
ratings_MADig.both_long$MAD <- as.numeric(ratings_MADig.both_long$MAD)
ratings_MADig.both  <- reshape(ratings_MADig.both_long, idvar = "days", timevar = "id", direction = "wide")
ICC.MADig.both <- irr::icc(ratings_MADig.both[,-1], model = "twoway", type = "consistency")


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

# Both
spearman.brown(singleICC = ICC.ENMO.both$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.ENMO.both$value, desiredR = 0.75)
spearman.brown(singleICC = ICC.ENMO.both$value, desiredR = 0.8)
spearman.brown(singleICC = ICC.ENMOig.both$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.ENMOig.both$value, desiredR = 0.75)
spearman.brown(singleICC = ICC.ENMOig.both$value, desiredR = 0.8)
spearman.brown(singleICC = ICC.MAD.both$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.MAD.both$value, desiredR = 0.75)
spearman.brown(singleICC = ICC.MAD.both$value, desiredR = 0.8)
spearman.brown(singleICC = ICC.MADig.both$value, desiredR = 0.7)
spearman.brown(singleICC = ICC.MADig.both$value, desiredR = 0.75)
spearman.brown(singleICC = ICC.MADig.both$value, desiredR = 0.8)


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