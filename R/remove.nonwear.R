# Function to remove non-wear from data
# input data is a data.frame object


#' remove.nonwear
#'
#' @description 'remove.nonwear' Removes the timestamp data indicated as non-wear
#'
#' @param epochdir Path to the root of the accelerometer data aggregated in epochs.
#' @param epochlength Integer that indicates epoch length in seconds, default = 5.
#' @param minhours Integer that indicates the minimal recorded hours/day for a day to be considered a valid day, default = 8.
#' @param mindays Integer that indicates the minimal number of days to be considered valid data, default = 7.
#' @param savedir Path to the directory where the preprocessed valid data will be stored.

#' @export

remove.nonwear <- function(epochdir.hip, epochdir.wrist, savefolder, data.castor){
  
  filelist.hip <- list.files(epochdir.hip, pattern = ".RData")
  filelist.wrist <- list.files(epochdir.wrist, pattern = ".RData")
  epochdir <- epochdir.hip
  filelist <- c(filelist.hip, filelist.wrist)
  for(file in 1:length(filelist)){
    cat(file)
    load(paste(epochdir, filelist[file], sep = "/")) # Load .RData file
    
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
      attachment_axivity <- pp_characteristics$Time_Acc_start_1 
      heuristic_nw <- which(epochdata$agg.epoch$timestampPOSIX < strptime(attachment_axivity, format = "%d-%m-%Y %H:%M"))
      
      # Remove data after wear protocol was finished (i.e. 8 days wear time)
      if(pp_characteristics$Date_measurement_period_1 == ""){
        # App was not filled in, use attachment day + 7 days for measurement protocol
        endDate <- as.Date(attachment_axivity, format = "%d-%M-%Y") + 7
      } else if(as.Date(pp_characteristics$Date_measurement_period_1, format = "%d-%M-%Y") == as.Date(attachment_axivity, format = "%d-%M-%Y")){
        # If app was activated at attachment date
        endDate <- as.Date(pp_characteristics$Date_measurement_period_1, format = "%d-%M-%Y") + 7
      } else {
        endDate <- as.Date(pp_characteristics$Date_measurement_period_1, format = "%d-%M-%Y") + 6
      } 
      if(sum(unique(as.Date(epochdata$agg.epoch$timestampPOSIX)) > endDate) > 1) {
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