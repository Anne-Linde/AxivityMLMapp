#' estimates.axivity
#'
#' @description 'estimates.axivity' Returns the outcomes (average acceleration and 25th percentile) for all data files
#' 
#' @param validdatadir Path to the directory where the preprocessed valid data is stored.
#' @importFrom stats quantile
#' @return A data.frame object with the 
#' @export

estimates.axivity <- function(validdatadir, analysis = "reliability"){
  # Metrics to be calculated for each valid day
  acc_x <- c()
  acc_y <- c()
  acc_z <- c()
  acc_x_25 <- c()
  acc_y_25 <- c()
  acc_z_25 <- c()
  ENMO <- c()
  ENMO_25 <- c()
  ig_ENMO <- c()
  M60_ENMO <- c()
  M30_ENMO <- c()
  M10_ENMO <- c()
  L5_ENMO <- c()
  MAD <- c()
  MAD_25 <- c()
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
      #if(day == 1 & analysis == "reliability" & length(data_day) > 7){
      #  next # Most participants with > 7 days, confirmed to old wear protocol of one day longer, skip the first measurement day
      #}
      
      # Select metrics for day already calculated
      a <- valid_days$metrics_day[which(as.Date(names(valid_days$data_day[day])) == as.Date(valid_days$metrics_day$calendar_date)),]
      ENMO <- c(ENMO, mean(valid_days$data_day[[day]]$ENMO)*1000)
      ig_ENMO <- c(ig_ENMO, as.double(a$`ig_gradient_ENMO_0-24hr`))
      M60_ENMO <- c(M60_ENMO, as.double(a$`p95.83333_ENMO_mg_0-24hr`))
      M30_ENMO <- c(M30_ENMO, as.double(a$`p97.91667_ENMO_mg_0-24hr`))
      M10_ENMO <- c(M10_ENMO, as.double(a$`p99.30556_ENMO_mg_0-24hr`))
      L5_ENMO <- c(L5_ENMO, as.double(a$`L5_ENMO_mg_0-24hr`))
      MAD <- c(MAD, mean(valid_days$data_day[[day]]$MAD)*1000)
      ig_MAD <- c(ig_MAD, as.double(a$`ig_gradient_MAD_0-24hr`))
      M60_MAD <- c(M60_MAD, as.double(a$`p95.83333_MAD_mg_0-24hr`))
      M30_MAD <- c(M30_MAD, as.double(a$`p97.91667_MAD_mg_0-24hr`))
      M10_MAD <- c(M10_MAD, as.double(a$`p99.30556_MAD_mg_0-24hr`))
      L5_MAD <- c(L5_MAD, as.double(a$`L5_MAD_mg_0-24hr`))
      
      # Calculate metrics per day
      acc_x <- c(acc_x, mean(valid_days$data_day[[day]]$roll_med_acc_x)*1000)
      acc_y <- c(acc_y, mean(valid_days$data_day[[day]]$roll_med_acc_y)*1000)
      acc_z <- c(acc_z, mean(valid_days$data_day[[day]]$roll_med_acc_z)*1000)
      acc_x_25 <- c(acc_x_25, quantile(valid_days$data_day[[day]]$roll_med_acc_x)[2]*1000)
      acc_y_25 <- c(acc_y_25, quantile(valid_days$data_day[[day]]$roll_med_acc_y)[2]*1000)
      acc_z_25 <- c(acc_z_25, quantile(valid_days$data_day[[day]]$roll_med_acc_z)[2]*1000)
      ENMO_25 <- c(ENMO_25, quantile(as.double(mean(valid_days$data_day[[day]]$ENMO)), na.rm = TRUE)[2]*1000)
      MAD_25 <- c(MAD_25, quantile(as.double(mean(valid_days$data_day[[day]]$MAD)), na.rm = TRUE)[2]*1000)
      
      # Save additional information
      id <- c(id, name[1])
      date <- c(date, names(valid_days$data_day)[day])
      days <- c(days, a$weekday)
    }
  }
  weekday <- ifelse(days %in% c("Saturday", "Sunday"), 0, 1)
  
  axivity.estimates <- cbind(id, date, days, weekday, 
                             acc_x, acc_y, acc_z, acc_x_25, acc_y_25, acc_z_25, 
                             ENMO, ENMO_25, ig_ENMO, M60_ENMO, M30_ENMO, M10_ENMO, L5_ENMO,
                             MAD, MAD_25, ig_MAD, M60_MAD, M30_MAD, M10_MAD, L5_MAD)

  axivity.estimates <- as.data.frame(axivity.estimates)
  #Ensure variable types
  axivity.estimates$weekday <- as.factor(axivity.estimates$weekday)
  axivity.estimates$acc_x <- as.numeric(axivity.estimates$acc_x)
  axivity.estimates$acc_y <- as.numeric(axivity.estimates$acc_y)
  axivity.estimates$acc_z <- as.numeric(axivity.estimates$acc_z)
  axivity.estimates$acc_x_25 <- as.numeric(axivity.estimates$acc_x_25)
  axivity.estimates$acc_y_25 <- as.numeric(axivity.estimates$acc_y_25)
  axivity.estimates$acc_z_25 <- as.numeric(axivity.estimates$acc_z_25)
  axivity.estimates$ENMO <- as.numeric(axivity.estimates$ENMO)
  axivity.estimates$ENMO_25 <- as.numeric(axivity.estimates$ENMO_25)
  axivity.estimates$ig_ENMO <- as.numeric(axivity.estimates$ig_ENMO)
  axivity.estimates$M60_ENMO <- as.numeric(axivity.estimates$M60_ENMO)
  axivity.estimates$M30_ENMO <- as.numeric(axivity.estimates$M30_ENMO)
  axivity.estimates$M10_ENMO <- as.numeric(axivity.estimates$M10_ENMO)
  axivity.estimates$L5_ENMO <- as.numeric(axivity.estimates$L5_ENMO)
  axivity.estimates$MAD <- as.numeric(axivity.estimates$MAD)
  axivity.estimates$MAD_25 <- as.numeric(axivity.estimates$MAD_25)
  axivity.estimates$ig_MAD <- as.numeric(axivity.estimates$ig_MAD)
  axivity.estimates$M60_MAD <- as.numeric(axivity.estimates$M60_MAD)
  axivity.estimates$M30_MAD <- as.numeric(axivity.estimates$M30_MAD)
  axivity.estimates$M10_MAD <- as.numeric(axivity.estimates$M10_MAD)
  axivity.estimates$L5_MAD <- as.numeric(axivity.estimates$L5_MAD)
  
  if (!file.exists(paste0(validdatadir, "/results"))){
    dir.create(file.path(paste0(validdatadir, "/results")))
  }
  save(axivity.estimates, file = paste0(validdatadir, "/results/axivity.estimates.RData"))     # Save data that meets the valid day criterion
  
  return(axivity.estimates)
}