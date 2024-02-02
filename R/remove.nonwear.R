#' remove.nonwear
#'
#' @description 'remove.nonwear' removes timestamp data indicated as non-wear from accelerometer data.
#'
#' @param epochdir.hip Path to the root directory containing hip accelerometer data aggregated in epochs.
#' @param epochdir.wrist Path to the root directory containing wrist accelerometer data aggregated in epochs.
#' @param savefolder Subdirectory where the preprocessed valid data will be stored.
#' @param data.castor Data frame containing information on accelerometer attachment date and time and wear protocol deviations
#' 
#' @export

remove.nonwear <- function(epochdir.hip, epochdir.wrist, savefolder, data.castor){
  
  filelist.hip <- list.files(epochdir.hip, pattern = ".RData")
  filelist.wrist <- list.files(epochdir.wrist, pattern = ".RData")
  epochdir <- epochdir.hip
  filelist <- c(filelist.hip, filelist.wrist)
  for(file in 1:length(filelist)){
    cat(file)
    load(paste(epochdir, filelist[file], sep = "/")) # Load .RData file
    
    #Remove deviations from wear protocol: right worn instead of left
    rightwrist <- NA
    if(filelist[file] == "05MLM001_00_wrist.cwa.RData"){
      startDeviation <- strptime(paste("20-10-2022 ", "18:00:00"), format = "%d-%m-%Y %H:%M", tz = "Europe/Amsterdam")
      rightwrist <- which(epochdata$agg.epoch$timestampPOSIX >= startDeviation)
    } else if(filelist[file] == "98MLM050_00_wrist.cwa.RData"){
     startDeviation <- strptime(paste("11-09-2023 ", "16:00"), format = "%d-%m-%Y %H:%M")
     endDeviation <- strptime("15-09-2023", format = "%d-%m-%Y")
     rightwrist <- which(epochdata$agg.epoch$timestampPOSIX > startDeviation & epochdata$agg.epoch$timestampPOSIX < endDeviation)
    }
    if(!is.na(rightwrist) & length(rightwrist) > 1){ # Remove deviations from wear protocol
      epochdata$agg.epoch <- epochdata$agg.epoch[-rightwrist,] 
    }
    
    if(nrow(epochdata$agg.epoch) > 0){
      #index_nw <- which(epochdata$agg.epoch$nonwear == 1) 
      index_nw <- c(which(epochdata$agg.epoch$nonwear == 1), which(epochdata$agg.epoch$additonal_nonwear == 1))
      if(length(index_nw) > 1){ # Remove non-wear data
        epochdata$agg.epoch <- epochdata$agg.epoch[-index_nw,] 
      } 
    }
    if(nrow(epochdata$agg.epoch) > 0){
      pp <- strsplit(filelist[file], "_")[[1]][1]
      pp_characteristics <- data.castor[which(data.castor$Participant.Id == pp),]
      attachment_axivity <- as.Date(pp_characteristics$Time_Acc_start_1, format = "%d-%m-%Y")
      heuristic_nw <- which(epochdata$agg.epoch$timestampPOSIX < strptime(attachment_axivity, format = "%Y-%m-%d"))
      
      # Remove data after wear protocol was finished (i.e. 8 days wear time)
      if(pp_characteristics$Date_measurement_period_1 == ""){
        # App was not filled in, use attachment day + 7 days for measurement protocol
        endDate <- as.Date(attachment_axivity, format = "%d-%m-%Y") + 7
      } else if(as.Date(pp_characteristics$Date_measurement_period_1, format = "%d-%m-%Y") == attachment_axivity){
        # If app was activated at attachment date
        endDate <- as.Date(pp_characteristics$Date_measurement_period_1, format = "%d-%m-%Y") + 7
      } else {
        endDate <- as.Date(pp_characteristics$Date_measurement_period_1, format = "%d-%m-%Y") + 6
      } 
      if(sum(unique(as.Date(epochdata$agg.epoch$timestampPOSIX)) > endDate) > 0) {
        index <- which(as.Date(epochdata$agg.epoch$timestampPOSIX) > endDate)
        heuristic_nw <- c(heuristic_nw, index)
      }
      if(length(heuristic_nw) > 1){ # Remove non-wear data
        epochdata$agg.epoch <- epochdata$agg.epoch[-heuristic_nw,] 
      }
    }
    if(nrow(epochdata$agg.epoch) > 0){
      # Select day metrics for remaining days and save data
      date_index <- which(as.Date(epochdata$day.metrics$calendar_date) %in% unique(as.Date(epochdata$agg.epoch$timestampPOSIX) ))  
      epochdata$day.metrics <- epochdata$day.metrics[date_index,] 
      save(epochdata, file = paste0(epochdir, savefolder, filelist[file])) # Save data that meets the valid day criterion
    }
    if(file == length(filelist.hip)){
      epochdir <- epochdir.wrist
    }
  }
}