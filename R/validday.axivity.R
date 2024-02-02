#' validday.axivity
#'
#' @description 'validday.axivity' preprocesses epoch-level accelerometer data by removing days that do not meet validity criteria.
#'
#' @param epochdir Path to the root directory containing epoch-level accelerometer data in .RData files.
#' @param epochlength Integer indicating the epoch length in seconds, default is 5.
#' @param minhours Integer indicating the minimum recorded hours per day for a day to be considered valid, default is 12.
#' @param mindays Integer indicating the minimum number of valid days required, default is 2.
#' @param savedir Path to the directory where the preprocessed valid data will be stored.
#' @param castordata Data frame containing information about measurement periods and participant IDs.
#
#' @export

validday.axivity <- function(epochdir, epochlength = 5, minhours = 12, mindays = 2, savedir, castordata){
  filelist <- list.files(epochdir, pattern = ".RData")
  for(file in 1:length(filelist)){
    cat(file)
    load(paste(epochdir, filelist[file], sep = "/")) # Load .RData file
    
    data_day <- split(epochdata$agg.epoch, as.Date(epochdata$agg.epoch$timestampPOSIX)) # Split data per day
    
    if(length(data_day) > 1){
      valid_days <- c()
      for (day in 1:length(data_day)) {
        epochsDay <- length(data_day[[day]]) # Number of epochs in this day
        nepoch = 60/epochlength # Number of epochs in one minute
        valid_days <- c(valid_days, (nrow(data_day[[day]]) >= (nepoch * 60 * minhours))) # TRUE if the number of epochs in this day is equal to or bigger than criterion
      }
      data_day[which(valid_days == FALSE)] <- NULL # Remove the invalid days
    }
    if(length(data_day) >= mindays){
      # analysis = "" # Previous reliability analyses required a balanced design of exactly 7 days of data.
      # if(analysis == "reliability"){ # Exact 7 different days are required
      #   if(length(data_day) > 7){ # Select the data from measurement period 1
      #     pp_id <- strsplit(filelist[file], "_")[[1]][1]
      #     startdate <- castordata$Date_measurement_period_1[which(castordata$Participant.Id == pp_id)]
      #     period <- as.Date(startdate, format="%d-%m-%Y") + 1:7
      #     
      #     index_days <- c()
      #     for (day in 1:length(period)) {
      #       index_days <- c(index_days, which(names(data_day) == period[day]))
      #     }
      #     data_day <- data_day[index_days] # Select only days within measurement period
      #   }
      #   if(length(unique(weekdays(as.Date(names(data_day))))) == 7){ # Save data if there are 7 different days
      #     valid_days <- list(data_day = data_day, metrics_day = epochdata$day.metrics)
      #     save(valid_days, file = paste(savedir, filelist[file], sep = "/")) # Save data that meets the valid day criterion
      #   }
      # }
      # if(analysis == ""){
        valid_days <- list(data_day = data_day, metrics_day = epochdata$day.metrics)
        save(valid_days, file = paste(savedir, filelist[file], sep = "/")) # Save data that meets the valid day criterion
      # }
  }
  }
}