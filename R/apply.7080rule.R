#' apply.7080rule
#'
#' @description 'apply.7080rule' Determine the length of one day of data based on the 70/80rule.
#'
#' @param datadir Path to the data of the participants with at least 8 hours on at least 1 day
#' @param epochlength Integer that indicates epoch length in seconds, default = 5.
#' @return Minimum required wear time in minutes

#' @export

apply.7080rule <- function(datadir, method, epochlength = 5, filename.app = "valid_data_8h1d.RData"){
  
  weartime <- c()

  if(method == "axivity"){
    filelist <- list.files(datadir, pattern = ".RData")
    for(file in 1:length(filelist)){
      load(paste(datadir, filelist[file], sep = "/")) # Load .RData file
      weartimepp <- c()
      for (day in 1:length(data_day)){
        nepochs <- nrow(data_day[[day]])
        epochsmin = 60/epochlength # Number of epochs in one minute
        weartimepp <- c(weartimepp, nepochs/epochsmin) # Calculate the minutes of wear time for each day
      }
      weartime <- c(weartime, mean(weartimepp))
    }
  }
  if(method == "app"){
    data.app <- load(paste(datadir, filename.app, sep = "/"))
    participants <- unique(valid_data$castorID)
    weartimepp <- c()
  
    for(pp in 1:length(participants)){
      if(!is.na(participants[[pp]])){
        tmp <- valid_data[valid_data$castorID == participants[[pp]],]
        for(day in 1:nrow(tmp)){
          weartimepp <- c(weartimepp, sum(tmp[day,18:22], na.rm = TRUE))
        }
        weartime <- c(weartime, mean(weartimepp))
      }
    }  
  }
  
  population70 <- quantile(weartime, 0.70) # the number of minutes 70
  
  return(population70[[1]]/100*80)
}