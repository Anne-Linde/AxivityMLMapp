#' create.overlayplot
#'
#' @description 'create.overlayplot' creates overlay plots of activity data recorded via the MLM app and accelerometer data (hip and wrist worn). The plots are saved as JPEG files.
#'
#' @param data.app A data frame containing the app data, which includes columns for `castorID`, `date`, `startTime`, `endTime`, and `activity`.
#' @param filepath.hip The file path to the directory containing the cleaned hip accelerometer `.cwa.RData` files.
#' @param filepath.wrist The file path to the directory containing the wrist accelerometer `.cwa.RData` files.
#' @param pp The participant identifier (castorID) to be processed.
#' @param savedir The directory where the plots will be saved.
#'
#' @details The function first filters the app data for the specified participant. It then loads the corresponding accelerometer data for the hip and wrist, if available. For each unique day of app data, it generates overlay plots showing the app-recorded activities and the accelerometer-derived ENMO values. The plots are saved to the specified directory.
#'
#' @return This function does not return a value. It saves plots as JPEG files.
#' 
#' @examples
#' \dontrun{
#' create.overlayplot(data.app = app_data, filepath.hip = "path/to/hip/data", 
#'                    filepath.wrist = "path/to/wrist/data", pp = "participant_01", 
#'                    savedir = "path/to/save/plots")
#' }
#' 
#' @import ggplot2
#' @import anytime
#' @import viridis
#'
#' @export

create.overlayplot <- function(data.app, filepath.hip, filepath.wrist, pp, savedir) {
  tz = "Europe/Amsterdam"
  
  app <- data.app[which(data.app$castorID == pp),] # Select app data of this parent
  
  if(nrow(app) > 0){
    # Load accelerometer data for hip and wrist
    indexfile.hip <- which(startsWith(list.files(filepath.hip, pattern = ".cwa.RData"), pp))
    indexfile.wrist <- which(startsWith(list.files(filepath.wrist, pattern = ".cwa.RData"), pp))
    
    if(length(indexfile.hip) > 0) {
      load(paste(filepath.hip, list.files(filepath.hip, pattern = ".cwa.RData")[indexfile.hip], sep = "/")) # Load hip data
      hip <- epochdata$agg.epoch
      rm(epochdata)
    }
    if(length(indexfile.wrist) > 0){
      load(paste(filepath.wrist, list.files(filepath.wrist, pattern = ".cwa.RData")[indexfile.wrist], sep = "/")) # Load wrist data
      wrist <- epochdata$agg.epoch
      rm(epochdata)
    } 
    
    days <- unique(app$date)
    for(day in 1:length(days)){
      app_day <- app[app$date == days[day],]
      app_day <- app_day[order(app_day$startTime),]
      xmin <- c()
      xmax <- c()
      col <- c()
      if(exists("hip")){
        data.hip_day <- hip[which(as.Date(hip$timestampPOSIX) == days[day]),]
        df_hip <- data.frame(xmin = rep(NA, nrow(app_day)), xmax = rep(NA, nrow(app_day)),
                             ymin = rep(0, nrow(app_day)), ymax = rep(max(data.hip_day$ENMO), nrow(app_day)), 
                             col = rep(NA, nrow(app_day)), alpha = rep(0.2, nrow(app_day)))
        data.hip_day$timestamp <- as.POSIXct(data.hip_day$timestampPOSIX) # fix for plotting
        
      }
      if(exists("wrist")){
        data.wrist_day <- wrist[which(as.Date(wrist$timestampPOSIX) == days[day]),]
        df_wrist <- data.frame(xmin = rep(NA, nrow(app_day)), xmax = rep(NA, nrow(app_day)),
                               ymin = rep(0, nrow(app_day)), ymax = rep(max(data.wrist_day$ENMO), nrow(app_day)), 
                               col = rep(NA, nrow(app_day)), alpha = rep(0.2, nrow(app_day)))
        data.wrist_day$timestamp <- as.POSIXct(data.wrist_day$timestampPOSIX) # fix for plotting
        
      }
      categories <- unique(app_day$activity)
      palette <- viridis(length(categories), option = "D")

      
      for(entry in 1:nrow(app_day)){ # Select axivity data for the timeslots an activity is filled in
        
        xmin <- c(xmin,  as.POSIXct(paste(app_day$date[entry], app_day$startTime[entry]), format = "%Y-%m-%d %H:%M:%S", tz = tz))
        xmax <- c(xmax, as.POSIXct(paste(app_day$date[entry], app_day$endTime[entry]), format = "%Y-%m-%d %H:%M:%S", tz = tz))
        col <- c(col, palette[which(app_day$activity[entry] == categories)])
        
        if(entry == nrow(app_day)){
          if(exists("hip")){
            df_hip$xmin <- as.POSIXct(xmin, origin = "1970-01-01")
            df_hip$xmax <- as.POSIXct(xmax, origin = "1970-01-01")
            df_hip$col <- col
            figure_labels = c("Personal care", "Active play", "Sitting/lying", "Sleeping", "Eating/drinking", "Calm Play", "Passive transport")
            
            g_hip <- ggplot() + 
              geom_rect(data = df_hip, mapping= aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col)) + 
              geom_line(data = data.hip_day, aes(data.hip_day$timestamp, data.hip_day$ENMO, group = data.hip_day$activity)) + 
              theme_classic() + 
              scale_fill_manual(name = "App categories", values = palette, labels = categories[sort(palette, index.return = TRUE)$ix]) +
              #scale_fill_manual(name = "App categories", values = palette, labels = figure_labels) +
              scale_y_continuous(name="Hip") +
              xlab("Time (hh:mm:ss)") #, date_labels = function(x) strftime(as.POSIXct(x, origin = "1970-01-01"), format = "%H:%M:%S")) +
            #ggtitle(paste0("Participant: ", pp, ", Date: ", unique(as.Date(as.POSIXct(df_wrist$xmax, origin = "1970-01-01"))))) +
            #scale_x_datetime(name="Time (hh:mm)", date_labels = "%H:%M", date_breaks = "1 hour")
            ggsave(file = paste0(savedir, "/plots/overlay/", pp, "_hip_", "day_", days[day], ".jpeg"), g_hip, device = "jpeg", width = 15, height = 10, dpi = 600)
            
          }
          if(exists("wrist")){
            df_wrist$xmin <- as.POSIXct(xmin, origin = "1970-01-01")
            df_wrist$xmax <- as.POSIXct(xmax, origin = "1970-01-01")
            df_wrist$col <- col
            
            figure_labels = c("Personal care", "Active play", "Sitting/lying", "Sleeping", "Eating/drinking", "Calm Play", "Passive transport")
            g_wrist <- ggplot() + 
              geom_rect(data = df_wrist, mapping=aes(xmin = anytime(xmin), xmax = anytime(xmax), ymin = ymin, ymax = ymax, fill = col)) + 
              geom_line(data = data.wrist_day, aes(data.wrist_day$timestamp, data.wrist_day$ENMO, group = data.wrist_day$activity)) + 
              theme_classic() + 
              #scale_fill_manual(name = "App categories", values = palette, labels = figure_labels) +
              scale_fill_manual(name = "App categories", values = palette, labels = categories[sort(palette, index.return = TRUE)$ix]) +
              scale_y_continuous(name="Wrist") +
              xlab("Time (hh:mm:ss)") #, labels = function(x) strftime(as.POSIXct(x, origin = "1970-01-01"), format = "%H:%M:%S")) +
            #ggtitle(paste0("Participant: ", pp, ", Date: ", unique(as.Date(as.POSIXct(df_wrist$xmax, origin = "1970-01-01"))))) +
            #scale_x_datetime(name="Time (hh:mm)", date_labels = "%H:%M", date_breaks = "1 hour")
            
            ggsave(file = paste0(savedir, "/plots/overlay/", pp, "_wrist_", "day_", days[day], ".jpeg"), g_wrist, device = "jpeg", width = 15, height = 10, dpi = 600)
          }
        }
      }
      if(exists("g_hip") & exists("g_wrist")){
        combine.save.plots(g_hip,g_wrist,  "overlay", savedir, filename = paste0("/plots/overlay/hipwrist_combined/", pp, "_combined", "day_", days[day], ".jpeg"))
      }
    }
  }
}