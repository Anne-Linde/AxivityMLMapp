#Function to match Axivity with App data, for each app entry the median ENMO and MAD are calculated for both hip and wrist data
# output in acceleration values in mg 
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

  missing.app <- data.frame(matrix(0, nrow = length(participants), 
                                     ncol = 1 + 3*8))
  colnames(missing.app) <- c("castorID",
                               paste0("missing_app_day", 1:8),
                               paste0("hip_axivity_data_on_day", 1:8),
                               paste0("wrist_axivity_data_on_day", 1:8))
  
  # Average metrics for each activity
  #foreach::foreach(subj = 1:length(participants)) %myinfix% {#, .packages = "GGIRread") %myinfix% {
  for(subj in 1:length(participants)){
    acc_min_hip <- c()
    acc_min_wrist <- c()
    ENMO.hip <- c()
    ENMO.wrist <- c()
    MAD.hip <- c()
    MAD.wrist <- c()
    app.missing <- c()
    cat(paste0("participant: ", subj, " "))
    
    app <- data.app[which(data.app$castorID == participants[subj]),] # Select app data of this parent
    app <- app[order(app$date, app$startTime),] # sort the app data based on date and start time of the activity
    
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
    missing.app[subj, 1] <- unique(app$castorID)
    col <- c()
    if(length(unique(app$date)) < 8){
      if(length(unique(app$date)) <= 7){
        col = c(col, 9)
      } 
      if (length(unique(app$date)) <= 6){
        col = c(col, 8)
      } 
      if (length(unique(app$date)) <= 5){
        col = c(col, 7)
      } 
      if (length(unique(app$date)) <= 4){
        col = c(col, 6)
      } 
      if (length(unique(app$date)) <= 3){
        col = c(col, 5)
      } 
      if (length(unique(app$date)) <= 2){
        col = 4
      } 
      if (length(unique(app$date)) == 1){
        col = c(col, 3)
      } 
      if (length(unique(app$date)) < 1){
        col = c(col, 2)
      }
      missing.app[subj, col] <- NA
      missing.app[subj, col+8] <- NA
      missing.app[subj, col+6] <- NA
    }
    
    for(entry in 1:nrow(app)){ # Select axivity data for the time slots an activity is filled in
      cat(paste0(entry, "/", nrow(app), " "))
      start <- as.POSIXlt(paste(app$date[entry], app$startTime[entry]), format = "%Y-%m-%d %H:%M:%S", tz = tz)
      end <- as.POSIXlt(paste(app$date[entry], app$endTime[entry]), format = "%Y-%m-%d %H:%M:%S", tz = tz)
      duration <- as.numeric(difftime(end, start, units = "mins"))
      if(exists("hip")){
        tmp.hip <- hip[which(hip$timestampPOSIX >= start & hip$timestampPOSIX <= end),]
        ENMO <- median(tmp.hip$ENMO, na.rm = TRUE)*1000 # Save ENMO
        ENMO.hip <- c(ENMO.hip, ENMO)
        MAD <- median(tmp.hip$MAD, na.rm = TRUE)*1000 # Save MAD
        MAD.hip <- c(MAD.hip, MAD)
        # Calculate minutes available acceleration data during this activity
        if(nrow(tmp.hip) > 0){
          if(round(nrow(tmp.hip)/(60/5)) == duration){
            acc_min_hip <- c(acc_min_hip, duration)
          } else {
            acc_min_hip <- c(acc_min_hip, nrow(tmp.hip)/(60/5))
          }
        } else{
            acc_min_hip <- c(acc_min_hip, NA)
        }
      } else {
        ENMO.hip <- c(ENMO.hip, NA)
        MAD.hip <- c(MAD.hip, NA)
        acc_min_hip <- c(acc_min_hip, NA)
      }
      if(exists("wrist")){
        tmp.wrist <- wrist[which(wrist$timestampPOSIX >= start & wrist$timestampPOSIX <= end),]
        ENMO <- median(tmp.wrist$ENMO, na.rm = TRUE)*1000 # Save ENMO
        ENMO.wrist <- c(ENMO.wrist, ENMO)
        MAD <- median(tmp.wrist$MAD, na.rm = TRUE)*1000 # Save MAD
        MAD.wrist <- c(MAD.wrist, MAD)
        # Calculate minutes available acceleration data during this activity
        if(nrow(tmp.wrist) > 0){
          if(round(nrow(tmp.wrist)/(60/5)) == duration){
            acc_min_wrist <- c(acc_min_wrist, duration)
          } else {
            acc_min_wrist <- c(acc_min_wrist, nrow(tmp.wrist)/(60/5))
          }
        } else{
          acc_min_wrist <- c(acc_min_wrist, NA)
        }
      } else {
        ENMO.wrist <- c(ENMO.wrist, NA)
        MAD.wrist <- c(MAD.wrist, NA)
        acc_min_wrist <- c(acc_min_wrist, NA)
      }
      
      # Check if end/start time of activities are connected 
      end_prev <- as.POSIXct(paste(app$date[entry-1], app$endTime[entry-1]), format = "%Y-%m-%d %H:%M:%S", tz = tz)
      
      if(entry == 1){
        end_prev <- start
      }
      if((entry > 1) & (start > end_prev)){ 
        # Calculate the missing time in the app
        missing_time <- as.numeric(difftime(start, end_prev, units = "mins"))
        if((as.Date(start) > as.Date(end_prev)) & (toString(data.table::as.ITime(start)) != "00:00:00")){ 
          column <- which(unique(app$date) == as.Date(end_prev)) 
          next_day <- as.numeric(difftime(start, as.POSIXct(paste0(as.Date(start), " 00:00:00"), format = "%Y-%m-%d %H:%M:%S"), units = "mins"))
          missing.app[subj , column + 1] <- missing.app[subj , column + 1] + (missing_time - next_day)
          missing.app[subj , column + 2] <- missing.app[subj , column + 2] + next_day
          
          if(exists("hip")){
            tmp.hip <- hip[which(hip$timestampPOSIX >= end_prev & as.Date(hip$timestampPOSIX) < as.Date(start)),] 
            if(nrow(tmp.hip) > 0){
              if(round(nrow(tmp.hip)/(60/5)) == (missing_time - next_day)){
                missing.app[subj, column + 8] <- missing.app[subj, column + 8] + missing_time
              } else {
                missing.app[subj, column + 8] <- missing.app[subj, column + 8] + nrow(tmp.hip)/(60/5)
              } 
            }
            tmp.hip <- hip[which(as.Date(hip$timestampPOSIX) >= as.Date(start) & hip$timestampPOSIX <= end),] 
            if(nrow(tmp.hip) > 0){
              if(round(nrow(tmp.hip)/(60/5)) == next_day){
                missing.app[subj, column + 9] <- missing.app[subj, column + 9] + missing_time
              } else {
                missing.app[subj, column + 9] <- missing.app[subj, column + 9] + nrow(tmp.hip)/(60/5)
              }
            }
          }
          if(exists("wrist")){
            tmp.wrist <- wrist[which(wrist$timestampPOSIX >= end_prev & as.Date(wrist$timestampPOSIX) < as.Date(start)),] 
            if(nrow(tmp.wrist) > 0){
              if(round(nrow(tmp.wrist)/(60/5)) == (missing_time - next_day)){
                missing.app[subj, column + 16] <- missing.app[subj, column + 16] + missing_time
              } else {
                missing.app[subj, column + 16] <- missing.app[subj, column + 16] + nrow(tmp.wrist)/(60/5)
              }
            }
            tmp.wrist <- wrist[which(as.Date(wrist$timestampPOSIX) >= as.Date(start) & wrist$timestampPOSIX <= end),] 
            if(nrow(tmp.wrist) > 0){
              if(round(nrow(tmp.wrist)/(60/5)) == next_day){
                missing.app[subj, column + 17] <- missing.app[subj, column + 17] + missing_time
              } else {
                missing.app[subj, column + 17] <- missing.app[subj, column + 17] + nrow(tmp.wrist)/(60/5)
              }
            }
          }
        } else {
          column <- which(unique(app$date) == as.Date(end_prev)) + 1
          missing.app[subj, column] <- missing.app[subj, column] + missing_time
          if(exists("hip")){
            tmp.hip <- hip[which(hip$timestampPOSIX >= end_prev & hip$timestampPOSIX <= start),] 
            if(nrow(tmp.hip) > 0){
              if(round(nrow(tmp.hip)/(60/5)) == missing_time){
                missing.app[subj, column + 8] <- missing.app[subj, column + 8] + missing_time
              } else {
                missing.app[subj, column + 8] <- missing.app[subj, column + 8] + nrow(tmp.hip)/(60/5)
              }
            }
          }
          if(exists("wrist")){
            tmp.wrist <- wrist[which(wrist$timestampPOSIX >= end_prev & wrist$timestampPOSIX <= start),] 
            if(nrow(tmp.wrist) > 0){
              if(round(nrow(tmp.wrist)/(60/5)) == missing_time){
                missing.app[subj, column + 16] <- missing.app[subj , column + 16] + missing_time
              } else {
                missing.app[subj, column + 16] <- missing.app[subj , column + 16] + nrow(tmp.wrist)/(60/5)
              }
            }
          }
        }
        
      }
      
    
    }
    app <- cbind(app, ENMO.hip, ENMO.wrist, MAD.hip, MAD.wrist, acc_min_hip, acc_min_wrist)
    data.pp <- rbind(data.pp, app)
  }

  save(data.pp, file = paste0(savedir, "/app_ax_entry_", date, ".RData"))
  save(missing.app, file = paste0(savedir, "/app_missing_ax_day_", date, ".RData"))
  
  parallel::stopCluster(cl)
  return(list(data = data.pp, missing = missing.app))
}
