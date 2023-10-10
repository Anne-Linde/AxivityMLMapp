#Function to match Axivity with App data, for each app entry the mean ENMO and MAD are calculated for both hip and wrist data
match.app.axivity <- function(data.app, filepath.hip, filepath.wrist, tz, date){
  
  #PARALLEL LOAd
  closeAllConnections() # in case there is still something running from last time, kill it
  cores = parallel::detectCores()
  Ncores = cores[1]
  Ncores = Ncores - 2
  if(Ncores < 1) Ncores = 1
  cl <- parallel::makeCluster(Ncores) # not to overload your computer
  doParallel::registerDoParallel(cl)
  participants = NULL
  
  `%myinfix%` = foreach::`%dopar%`
  
  participants <- unique(data.app$castorID) #participants with app data
  filelist.hip <- list.files(filepath.hip, pattern = ".RData")
  filelist.wrist <- list.files(filepath.wrist, pattern = ".RData")
  
  data.pp <- data.frame()
  
  # Average metrics for each activity
  #foreach::foreach(subj = 1:length(participants)) %myinfix% {#, .packages = "GGIRread") %myinfix% {
  for(subj in 1:length(participants)){
    ENMO.hip <- c()
    ENMO.wrist <- c()
    MAD.hip <- c()
    MAD.wrist <- c()
    cat(paste0("participant: ", subj, " "))
    
    app <- data.app[which(data.app$castorID == participants[subj]),] # Select app data of this parent
    #Load the accelerometer data of the child
    indexfile.hip <- which(startsWith(filelist.hip, participants[subj]))
    indexfile.wrist <- which(startsWith(filelist.wrist, participants[subj]))
    if(length(indexfile.hip) > 0) {
      load(paste(filepath.hip, filelist.hip[indexfile.hip], sep = "/")) # Load hip data
      hip <- epochdata$agg.epoch
      rm(epochdata)
    }
    if(length(indexfile.wrist) > 0){
      load(paste(filepath.wrist, filelist.wrist[indexfile.wrist], sep = "/")) # Load wrist data
      wrist <- epochdata$agg.epoch
      rm(epochdata)
    }
    
    for(entry in 1:nrow(app)){ # Select axivity data for the time slots an activity is filled in
      cat(paste0(entry, "/", nrow(app), " "))
      start <- as.POSIXlt(paste(app$date[entry], app$startTime[entry]), format = "%Y-%m-%d %H:%M:%S", tz = tz)
      end <- as.POSIXlt(paste(app$date[entry], app$endTime[entry]), format = "%Y-%m-%d %H:%M:%S", tz = tz)
      
      if(exists("hip")){
        tmp.hip <- hip[which(hip$timestampPOSIX >= start & hip$timestampPOSIX <= end),]
        ENMO <- median(tmp.hip$ENMO, na.rm = TRUE) # Save ENMO
        ENMO.hip <- c(ENMO.hip, ENMO)
        MAD <- median(tmp.hip$MAD, na.rm = TRUE) # Save MAD
        MAD.hip <- c(MAD.hip, ENMO)
      } else {
        ENMO.hip <- c(ENMO.hip, NA)
        MAD.hip <- c(MAD.hip, NA)
      }
      if(exists("wrist")){
        tmp.wrist <- wrist[which(wrist$timestampPOSIX >= start & wrist$timestampPOSIX <= end),]
        ENMO <- median(tmp.wrist$ENMO, na.rm = TRUE) # Save ENMO
        ENMO.wrist <- c(ENMO.wrist, ENMO)
        MAD <- median(tmp.wrist$MAD, na.rm = TRUE) # Save MAD
        MAD.wrist <- c(MAD.wrist, ENMO)
      } else {
        ENMO.wrist <- c(ENMO.wrist, NA)
        MAD.wrist <- c(MAD.wrist, NA)
      }
    }
    app <- cbind(app, ENMO.hip, ENMO.wrist, MAD.hip, MAD.wrist)
    data.pp <- rbind(data.pp, app)
  }
  save(data.pp, file = paste0(savedir, "/app_ax_entry_", date, ".RData"))
  parallel::stopCluster(cl)
  return(data.pp)
}
