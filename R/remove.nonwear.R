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

remove.nonwear <- function(epochdir.hip, epochdir.wrist, savefolder){
  
  filelist.hip <- list.files(epochdir.hip, pattern = ".RData")
  filelist.wrist <- list.files(epochdir.wrist, pattern = ".RData")
  epochdir <- epochdir.hip
  filelist <- c(filelist.hip, filelist.wrist)
  for(file in 1:(length(filelist.hip)+length(filelist.wrist))){
    cat(file)
    if(file == length(filelist.hip) + 1){
      epochdir <- epochdir.wrist
    }
    
    load(paste(epochdir, filelist[file], sep = "/")) # Load .RData file
    
    if(nrow(epochdata) > 0){
      index_nw <- which(epochdata$nonwear == 1) 
      #index_nw <- c(which(data_day[[day]]$additonal_nonwear == 1), which(data_day[[day]]$nonwear == 1))
      if(length(index_nw) > 1){
        epochdata <- epochdata[-index_nw,] # Remove non-wear data
      }
    }
    save(epochdata, file = paste0(epochdir, savefolder, filelist[file])) # Save data that meets the valid day criterion
    } 
}