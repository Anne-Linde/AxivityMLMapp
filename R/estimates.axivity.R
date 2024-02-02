#' estimates.axivity
#'
#' @description
#' 'estimates.axivity' calculates various metrics from the valid days' data and returns the outcomes, including
#' Median acceleration, 25th percentile, and additional calculated metrics for all data files.
#'
#' @param validdatadir Path to the directory where the preprocessed valid data is stored.
#' @importFrom stats quantile
#' @return A data.frame object with calculated metrics.
#'
#' @details
#' This function calculates metrics such as ENMO, MAD, intensity gradient (ig), various percentiles, and
#' additional information like participant ID, date, day of the week, etc. The results are returned in a
#' structured data.frame.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' data_dir <- "/path/to/valid_data"
#' result <- estimates.axivity(data_dir)

estimates.axivity <- function(validdatadir){
  # Metrics to be calculated for each valid day
  ENMO <- c()
  ENMO_25 <- c()
  ENMO_75 <- c()
  ENMO_25_duration <- c()
  ENMO_50_duration <- c()
  ENMO_75_duration <- c()
  ig_ENMO <- c()
  M60_ENMO <- c()
  M30_ENMO <- c()
  M10_ENMO <- c()
  L5_ENMO <- c()
  MAD <- c()
  MAD_25 <- c()
  MAD_75 <- c()
  MAD_25_duration <- c()
  MAD_50_duration <- c()
  MAD_75_duration <- c()
  ig_MAD <- c()
  M60_MAD <- c()
  M30_MAD <- c()
  M10_MAD <- c()
  L5_MAD <- c()
  # Additional information
  id <- c()
  date <- c()
  days <- c()
  weekday <- c()
  
  filelist <- list.files(validdatadir, pattern = ".RData")

  for (file in 1:length(filelist)){
    cat(file)
    load(paste(validdatadir, filelist[file], sep = "/")) # Load epochdata
    name <- strsplit(filelist[file], "_00_")[[1]]
    
    for(day in 1:length(valid_days$data_day)){
      a <- valid_days$metrics_day[which(as.Date(names(valid_days$data_day[day])) == as.Date(valid_days$metrics_day$calendar_date)),]
      # Select metrics for day already calculated
      ig_ENMO <- c(ig_ENMO, as.double(a$`ig_gradient_ENMO_0-24hr`))
      M60_ENMO <- c(M60_ENMO, as.double(a$`p95.83333_ENMO_mg_0-24hr`))
      M30_ENMO <- c(M30_ENMO, as.double(a$`p97.91667_ENMO_mg_0-24hr`))
      M10_ENMO <- c(M10_ENMO, as.double(a$`p99.30556_ENMO_mg_0-24hr`))
      L5_ENMO <- c(L5_ENMO, as.double(a$`L5_ENMO_mg_0-24hr`))
      ig_MAD <- c(ig_MAD, as.double(a$`ig_gradient_MAD_0-24hr`))
      M60_MAD <- c(M60_MAD, as.double(a$`p95.83333_MAD_mg_0-24hr`))
      M30_MAD <- c(M30_MAD, as.double(a$`p97.91667_MAD_mg_0-24hr`))
      M10_MAD <- c(M10_MAD, as.double(a$`p99.30556_MAD_mg_0-24hr`))
      L5_MAD <- c(L5_MAD, as.double(a$`L5_MAD_mg_0-24hr`))
      
      # Calculate metrics per day
      ENMO <- c(ENMO, median(valid_days$data_day[[day]]$ENMO)*1000)
      ENMO_25 <- c(ENMO_25, as.numeric(quantile(valid_days$data_day[[day]]$ENMO)[2])*1000)
      ENMO_75 <- c(ENMO_75, as.numeric(quantile(valid_days$data_day[[day]]$ENMO)[4])*1000)
      MAD <- c(MAD, median(valid_days$data_day[[day]]$MAD)*1000)
      MAD_25 <- c(MAD_25, as.numeric(quantile(valid_days$data_day[[day]]$MAD)[2])*1000)
      MAD_75 <- c(MAD_75, as.numeric(quantile(valid_days$data_day[[day]]$MAD)[4])*1000)
      
      # Save additional information
      id <- c(id, name[1])
      date <- c(date, names(valid_days$data_day)[day])
      days <- c(days, a$weekday)
    }
  }
  weekday <- ifelse(days %in% c("Saturday", "Sunday"), 0, 1)
  
  axivity.estimates <- cbind(id, date, days, weekday, 
                             ENMO, ENMO_25, ENMO_75, ig_ENMO, M60_ENMO, M30_ENMO, M10_ENMO, L5_ENMO,
                             MAD, MAD_25, MAD_75, ig_MAD, M60_MAD, M30_MAD, M10_MAD, L5_MAD)
  
  axivity.estimates <- as.data.frame(axivity.estimates)
  #Ensure variable types
  axivity.estimates$weekday <- as.factor(axivity.estimates$weekday)
  axivity.estimates$ENMO <- as.numeric(axivity.estimates$ENMO)
  axivity.estimates$ENMO_25 <- as.numeric(axivity.estimates$ENMO_25)
  axivity.estimates$ENMO_75 <- as.numeric(axivity.estimates$ENMO_75)
  axivity.estimates$ig_ENMO <- as.numeric(axivity.estimates$ig_ENMO)
  axivity.estimates$M60_ENMO <- as.numeric(axivity.estimates$M60_ENMO)
  axivity.estimates$M30_ENMO <- as.numeric(axivity.estimates$M30_ENMO)
  axivity.estimates$M10_ENMO <- as.numeric(axivity.estimates$M10_ENMO)
  axivity.estimates$L5_ENMO <- as.numeric(axivity.estimates$L5_ENMO)
  axivity.estimates$MAD <- as.numeric(axivity.estimates$MAD)
  axivity.estimates$MAD_25 <- as.numeric(axivity.estimates$MAD_25)
  axivity.estimates$MAD_75 <- as.numeric(axivity.estimates$MAD_75)
  axivity.estimates$ig_MAD <- as.numeric(axivity.estimates$ig_MAD)
  axivity.estimates$M60_MAD <- as.numeric(axivity.estimates$M60_MAD)
  axivity.estimates$M30_MAD <- as.numeric(axivity.estimates$M30_MAD)
  axivity.estimates$M10_MAD <- as.numeric(axivity.estimates$M10_MAD)
  axivity.estimates$L5_MAD <- as.numeric(axivity.estimates$L5_MAD)
  
  # Calculate percentiles for ENMO and MAD
  Q1_ENMO <- quantile(axivity.estimates$ENMO, na.rm = TRUE)[2]
  Q3_ENMO <- quantile(axivity.estimates$ENMO, na.rm = TRUE)[4]
  Q1_MAD <- quantile(axivity.estimates$MAD, na.rm = TRUE)[2]
  Q3_MAD <- quantile(axivity.estimates$MAD, na.rm = TRUE)[4]
  
  for (file in 1:length(filelist)){
    cat(file)
    load(paste(validdatadir, filelist[file], sep = "/")) # Load epochdata
    data <- valid_days$data_day
    for (d in 1:length(data)){
      ENMO_25_duration <- c(ENMO_25_duration, length(which(valid_days$data_day[[d]]$ENMO*1000 <= Q1_ENMO))/(60/5))
      ENMO_50_duration <- c(ENMO_50_duration, length(which(valid_days$data_day[[d]]$ENMO*1000 > Q1_ENMO & 
                                                             valid_days$data_day[[d]]$ENMO*1000 < Q3_ENMO))/(60/5))
      ENMO_75_duration <- c(ENMO_75_duration, length(which(valid_days$data_day[[d]]$ENMO*1000 >= Q3_ENMO))/(60/5))
      
      MAD_25_duration <- c(MAD_25_duration, length(which(valid_days$data_day[[d]]$MAD*1000 <= Q1_MAD))/(60/5))
      MAD_50_duration <- c(MAD_50_duration, length(which(valid_days$data_day[[d]]$MAD*1000 > Q1_MAD & 
                                                           valid_days$data_day[[d]]$MAD*1000 < Q3_MAD))/(60/5))
      MAD_75_duration <- c(MAD_75_duration, length(which(valid_days$data_day[[d]]$MAD*1000 >= Q3_MAD))/(60/5))
    }
  } 
  axivity.estimates <- cbind(axivity.estimates, ENMO_25_duration, ENMO_50_duration, ENMO_75_duration,
                             MAD_25_duration, MAD_50_duration, MAD_75_duration)
  axivity.estimates$ENMO_25_duration <- as.numeric(axivity.estimates$ENMO_25_duration)
  axivity.estimates$ENMO_50_duration <- as.numeric(axivity.estimates$ENMO_50_duration)
  axivity.estimates$ENMO_75_duration <- as.numeric(axivity.estimates$ENMO_75_duration)
  axivity.estimates$MAD_25_duration <- as.numeric(axivity.estimates$MAD_25_duration)
  axivity.estimates$MAD_50_duration <- as.numeric(axivity.estimates$MAD_50_duration)
  axivity.estimates$MAD_75_duration <- as.numeric(axivity.estimates$MAD_75_duration)
  
  if (!file.exists(paste0(validdatadir, "/results"))){
    dir.create(file.path(paste0(validdatadir, "/results")))
  }
  save(axivity.estimates, file = paste0(validdatadir, "/results/axivity.estimates.RData"))     # Save data that meets the valid day criterion
  
  return(axivity.estimates)
}