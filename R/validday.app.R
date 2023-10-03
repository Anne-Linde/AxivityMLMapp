#' validday.app
#'
#' @description 'validday.app' Preprocesses the app data with the activity duration per category per day by removing invalid days
#'
#' @param data data.frame object that contains the app data
#' @param minhours Integer that indicates the minimal recorded hours/day for a day to be considered a valid day, default = 8.
#' @param mindays Integer that indicates the minimal number of days to be considered valid data, default = 7.
#' @param savedir Path to the directory where the preprocessed valid data will be stored.

#' @export
validday.app <- function(data, minhours = 8, mindays = 7, savedir){
  
  participants <- unique(data$castorID)
  valid_data <- data.frame()
  for(pp in 1:length(participants)){
    if(!is.na(participants[[pp]])){
      tmp <- data[data$castorID == participants[[pp]],]
      tmp_valid <- data.frame()
      for(day in 1:nrow(tmp)){
        tmp.day <- tmp[day,]
        if(sum(tmp.day$PA,tmp.day$SB,tmp.day$sleep, na.rm = TRUE) >= minhours * 60){
          tmp_valid <- rbind(tmp_valid, tmp.day)
        }
      }
      if(nrow(tmp_valid) >= mindays){
        valid_data <- rbind(valid_data, tmp_valid)
      }
    }
  }
  filename = paste0("valid_data_", minhours, "h", mindays, "d.RData")
  save(valid_data, file = paste(savedir, filename, sep = "/")) # Save data that meets the valid day criterion
  
  return(valid_data)
}