#' validday.app
#'
#' @description 'validday.app' preprocesses the app data with the activity duration per category per day by removing invalid days
#'
#' @param data A data.frame object that contains the app data
#' @param minhours An integer indicating the minimal recorded hours/day for a day to be considered a valid day (default = 8).
#' @param mindays An integer that indicates the minimal number of days to be considered valid data (default = 7).
#' @param savedir Path to the directory where the preprocessed valid data will be stored.
#' @param CoDa A boolean indicating if the compositional scores (isometric log ratios) need to be calculated (default = TRUE)
#' 
#' @return A data.frame containing the valid preprocessed data based on the specified criteria.
#' 
#' @export
#' 
#' @examples
#' # Example usage:
#' # validday.app(data = my_data, minhours = 6, mindays = 5, savedir = "path/to/save")
#'
#' @details
#' This function preprocesses app data, removing days with insufficient activity duration per day.
#' It filters data based on the given minimum recorded hours/day and minimum number of valid days.
#' The resulting valid data will be saved in the specified directory.
#'
validday.app <- function(data, minhours = 8, mindays = 7, savedir, CoDa = TRUE){
  
  participants <- unique(data$castorID)
  valid_data <- data.frame()
  for(pp in 1:length(participants)){
    if(!is.na(participants[[pp]])){
      tmp <- data[data$castorID == participants[[pp]],]
      tmp_valid <- data.frame()
      for(day in 1:nrow(tmp)){
        tmp.day <- tmp[day,]
        if(sum(tmp.day$PA,tmp.day$SB,tmp.day$sleep, 5, na.rm = TRUE) >= ((minhours * 60)-5)){
          tmp_valid <- rbind(tmp_valid, tmp.day)
        }
      }
      if(nrow(tmp_valid) >= mindays){
        valid_data <- rbind(valid_data, tmp_valid)
      }
    }
  }
  valid_data$PA[which(is.na(valid_data$PA))] <- 0
  valid_data$SB[which(is.na(valid_data$SB))] <- 0
  valid_data$sleep[which(is.na(valid_data$sleep))] <- 0
  
  if(CoDa == TRUE){
    ilr_transformed <- compositions::ilr(valid_data[,22:24] + 0.001) # Handling zero values in the data before transformation
    colnames(ilr_transformed) <- c("ilr1", "ilr2")
    valid_data <- cbind(valid_data, ilr_transformed)
  }
  filename = paste0("valid_data_", minhours, "h", mindays, "d.RData")
  save(valid_data, file = paste(savedir, filename, sep = "/")) # Save data that meets the valid day criterion
  
  return(valid_data)
}