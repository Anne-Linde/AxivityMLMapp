#' outcomes.axivity
#'
#' @description 'outcomes.axivity' Returns the outcomes (average acceleration and 25th percentile) for all data files
#' 
#' @param validdatadir Path to the directory where the preprocessed valid data is stored.
#' @importFrom stats quantile
#' @return A data.frame object with the 
#' @export

outcomes.axivity <- function(validdatadir, analysis = "reliability"){
  acc_x_25 <- c()
  acc_y_25 <- c()
  acc_z_25 <- c()
  ENMO_25 <- c()
  avg_acc_x <- c()
  avg_acc_y <- c()
  avg_acc_z <- c()
  avg_ENMO <- c()
  id <- c()
  date <- c()
  days <- c()
  weekday <- c()
  
  filelist <- list.files(validdatadir, pattern = ".RData")
  for (file in 1:length(filelist)){
    cat(file)
    load(paste(validdatadir, filelist[file], sep = "/")) # Load .RData file
    name <- strsplit(filelist[file], "_00_")[[1]]
    for(day in 1:length(data_day)){
      if(day == 1 & analysis == "reliability" & length(data_day) > 7){
        next # Most participants with > 7 days, confirmed to old wear protocol of one day longer, skip the first measurement day
      }
      acc_x_25 <- c(acc_x_25, quantile(data_day[[day]]$roll_med_acc_x)[2])
      acc_y_25 <- c(acc_y_25, quantile(data_day[[day]]$roll_med_acc_y)[2])
      acc_z_25 <- c(acc_z_25, quantile(data_day[[day]]$roll_med_acc_z)[2])
      ENMO_25 <- c(ENMO_25, quantile(data_day[[day]]$ENMO)[2])
      avg_acc_x <- c(avg_acc_x, mean(data_day[[day]]$roll_med_acc_x))
      avg_acc_y <- c(avg_acc_y, mean(data_day[[day]]$roll_med_acc_y))
      avg_acc_z <- c(avg_acc_z, mean(data_day[[day]]$roll_med_acc_z))
      avg_ENMO <- c(avg_ENMO, mean(data_day[[day]]$ENMO))
      id <- c(id, name[1])
      date <- c(date, names(data_day)[day])
    }
  }
  days <- weekdays(as.Date(date))
  weekday <- ifelse(days %in% c("Saturday", "Sunday"), 0, 1)
  
  axivity.outcomes <- cbind(id, date, days, weekday, acc_x_25, acc_y_25, acc_z_25, ENMO_25, avg_acc_x,avg_acc_y, avg_acc_z, avg_ENMO)
  axivity.outcomes <- as.data.frame(axivity.outcomes)
  axivity.outcomes$weekday <- as.factor(axivity.outcomes$weekday)
  axivity.outcomes$acc_x_25 <- as.numeric(axivity.outcomes$acc_x_25)
  axivity.outcomes$acc_y_25 <- as.numeric(axivity.outcomes$acc_y_25)
  axivity.outcomes$acc_z_25 <- as.numeric(axivity.outcomes$acc_z_25)
  axivity.outcomes$ENMO_25 <- as.numeric(axivity.outcomes$ENMO_25)
  axivity.outcomes$avg_acc_x <- as.numeric(axivity.outcomes$avg_acc_x)
  axivity.outcomes$avg_acc_y <- as.numeric(axivity.outcomes$avg_acc_y)
  axivity.outcomes$avg_acc_z <- as.numeric(axivity.outcomes$avg_acc_z)
  axivity.outcomes$avg_ENMO <- as.numeric(axivity.outcomes$avg_ENMO)
  
  save(axivity.outcomes, file = paste(validdatadir, "axivity.outcomes.RData", sep = "/"))     # Save data that meets the valid day criterion
  
  return(axivity.outcomes)
}