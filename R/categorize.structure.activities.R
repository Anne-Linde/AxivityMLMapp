## 3) Calculate time spent in activity categories and label the movement behavior. 
#     Then calculate time spent and frequency in all activity categories per day for all participants

categorize.structure.activities <- function(datadir, date){
  
  # Load in app data
  data <- read.csv(paste0(datadir, "/", date, "_activities_castor_linked.csv"))[,-1]

  # Calculate time spent in activity categories
  data$total_duration <- as.integer(as.POSIXct(data$endTime, format ="%H:%M:%S") - as.POSIXct(data$startTime, format ="%H:%M:%S"))
  
  # Label 24-hour movement behavior and add posture
  data$behavior <- rep(NA, nrow(data))
  data$posture <- rep(NA, nrow(data))

  for(row in 1:nrow(data)){ # For each entry
    if(data$activity[row] == "personalcare" | data$activity[row] == "eatingdrinking" | data$activity[row] == "passivetransport"){
      data$behavior[row] <- "SB"
    } else if(data$activity[row] == "activetransport") {
      data$behavior[row] <- "PA"
    } else if(data$activity[row] == "sittinglying"){
      if(data$sit_lying_posture[row] == "1" | data$sit_lying_posture[row] == "5"){ # 1 = Liggend op buik, 5 = Zittend zonder steun
        data$behavior[row] <- "PA"
        if(data$sit_lying_posture[row] == "1"){ data$posture[row] <- "tummy"
        } else { data$posture[row] <- "sitting_withoutsupport" }
      } else{ # 2 = Liggend op rug, 3 = Liggend op zij, 4 = Zittend met steun, 6 = Gedragen worden, # 7 = Liggend, 8 = Zittend, 9 = Afwisselend 2 of meer houdingen, 10 = Weet ik niet
        data$behavior[row] <- "SB"
        if(data$sit_lying_posture[row] == "2"){ data$posture[row] <- "lying_back"
        } else if(data$sit_lying_posture[row] == "3"){ data$posture[row] <- "lying_side"
        } else if(data$sit_lying_posture[row] == "4"){ data$posture[row] <- "sitting_withsupport"
        } else if (data$sit_lying_posture[row] == "6") { data$posture[row] <- "carried"
        } else if (data$sit_lying_posture[row] == "7") { data$posture[row] <- "lying"
        } else if (data$sit_lying_posture[row] == "8") { data$posture[row] <- "sitting"
        } else if (data$sit_lying_posture[row] == "9") { data$posture[row] <- "alternating"
        } else { data$posture[row] <- "dontknow" }
      }
    } else if(data$activity[row] == "playing"){
      # Actief spelen
      if(data$play_intensity[row] == "1" | data$play_intensity[row] == "3" | data$play_intensity[row] == "5" | data$play_intensity[row] == "7"){
        data$activity[row] <- "activeplay"
        data$behavior[row] <- "PA"
      } else {  # Rustig spelen / Weet niet
        if(data$play_intensity[row] == "9"){
          data$activity[row] <- "dontknowplay"
        } else{
          data$activity[row] <- "quietplay"
        }
        if(data$play_posture[row] == "1" | data$play_posture[row] == "5" | data$play_posture[row] == "9"){# 1 = Liggend op buik, 5 = Zittend zonder steun, 9 = Staand zonder steun
          data$behavior[row] <- "PA"
          if(data$play_posture[row] == "1"){ data$posture[row] <- "tummy"
          } else if(data$play_posture[row] == "5"){ data$posture[row] <- "sitting_withoutsupport"
          } else { data$posture[row] <- "standing_withoutsupport" }
          
        } else{
          data$behavior[row] <- "SB"
        }
      } 
      if(data$play_posture[row] == "1"){ data$posture[row] <- "tummy"
      } else if(data$play_posture[row] == "2"){ data$posture[row] <- "lying_back"
      } else if(data$play_posture[row] == "3"){ data$posture[row] <- "lying_side"
      } else if(data$play_posture[row] == "4"){ data$posture[row] <- "sitting_withsupport"
      } else if(data$play_posture[row] == "5"){ data$posture[row] <- "sitting_withoutsupport"
      } else if(data$play_posture[row] == "6"){ data$posture[row] <- "lying"
      } else if(data$play_posture[row] == "7"){ data$posture[row] <- "sitting"
      } else if(data$play_posture[row] == "8"){ data$posture[row] <- "standing_withsupport"
      } else if(data$play_posture[row] == "9"){ data$posture[row] <- "standing_withoutsupport"
      } else if(data$play_posture[row] == "10"){ data$posture[row] <- "carried"
      } else if(data$play_posture[row] == "11"){ data$posture[row] <- "standing"
      } else if(data$play_posture[row] == "12"){ data$posture[row] <- "alternating"
      } else { data$posture[row] <- "dontknow" }
      
    } else if(data$activity[row] == "screen"){
      if(data$screen_activity[row] == "3"){
        data$activity[row] <- "activescreen"
        data$behavior[row] <- "PA"
      } else if(data$screen_activity[row] == "4"){
        data$activity[row] <- "dontknowscreen"
        data$behavior[row] <- "SB"
      } else {
        data$activity[row] <- "passivescreen"
        data$behavior[row] <- "SB"
      }
    } else if(data$activity[row] == "sleeping"){
      data$behavior[row] <- "sleep"
    } 
  }
  # Save the dataset per activity entry
  write.csv(data, paste0(datadir, "/", date, "_activities_castor_linked_duration.csv"))
  
  # List data per measurement period
  list_data <- list()
  for(measurement in 1:length(unique(data$measurement))){
    tmp <- data[data$measurement == unique(data$measurement)[measurement], ]
    list_data[[measurement]] <- tmp
  }
  
  ## From long activity entry format to wide day format
  # Create wide data.frame object
  data_category <- data.frame(matrix(0, nrow = nrow(data), 
                                     ncol = 4 + 2*length(unique(data$activity)) + 3))
  colnames(data_category) <- c("castorID", "cohort", "date", "measurement",
                               paste0("time_", unique(data$activity)),
                               paste0("freq_", unique(data$activity)),
                               "PA", "SB", "sleep")
  # data_category <- data.frame(matrix(NA, nrow = length(unique(data$castorID))*7, 
  #                                    ncol = 4 + 4*length(unique(data$activity)) + 3))
  # colnames(data_category) <- c("castorID", "cohort", "date", "measurement",
  #                              paste0("time_", unique(data$activity)),
  #                              paste0("freq_", unique(data$activity)),
  #                              paste0("time_posture_", unique(data$posture)),
  #                              paste0("freq_posture_", unique(data$posture)),
  #                              "PA", "SB", "sleep")
  
  # Structure data per day
  counter = 1
  for(measurement in 1:length(list_data)){ # For each measurement
    data <- list_data[[measurement]]
    for(pp in 1:length(unique(data$castorID))){ # For each participant
      tmp <- data[data$castorID == data$castorID[pp], ] # Select activity entries
      #days <- unique(tmp$date)
      if(length(unique(tmp$date)) > 0){ # If there is at least one day for this participant
        for(day in 1:length(unique(tmp$date))){ # For each day
          data_category$castorID[counter] <- unique(data$castorID)[pp] # Save castorID
          data_category$cohort[counter] <- unique(tmp$cohort) # Save cohort
          data_category$date[counter] <- unique(tmp$date)[day] #Save date
          data_category$measurement[counter] <- unique(tmp$measurement) #Save measurement period
          
          data_day <- tmp[tmp$date == unique(tmp$date)[day],] #Select data for this day
          
          for(category in 1:length(unique(data_day$activity))){ # For each category entered by the parent
            activity_sum_day <- sum(data_day[data_day$activity == unique(data_day$activity)[category], ]$total_duration, na.rm = TRUE) # Calculate total activity duration
            column_index_duration <- which(colnames(data_category) == paste0("time_", unique(data_day$activity)[category]))
            data_category[counter, column_index_duration] <- activity_sum_day #Save total duration entered for that category
            activity_frequency_day <- sum(data_day$activity == unique(data_day$activity)[category], na.rm = TRUE) # Calculate activity frequency
            column_index_frequency <- which(colnames(data_category) == paste0("freq_", unique(data_day$activity)[category]))
            data_category[counter, column_index_frequency] <- activity_frequency_day #Save frequency of category entries
          }
          # for(posture in 1:length(unique(data_day$posture))){ # For each posture entered
          #   if(!is.na(data_day$posture[posture])){
          #     posture_sum_day <- sum(data_day[data_day$posture == unique(data_day$posture)[posture], ]$total_duration, na.rm = TRUE) # Calculate total activity duration
          #     column_index_duration <- which(colnames(data_category) == paste0("time_posture_", unique(data_day$posture)[posture]))
          #     data_category[counter, column_index_duration] <- posture_sum_day #Save total duration entered for that category
          #     posture_frequency_day <- sum(data_day$posture == unique(data_day$posture)[posture], na.rm = TRUE) # Calculate activity frequency
          #     column_index_frequency <- which(colnames(data_category) == paste0("freq_posture_", unique(data_day$posture)[posture]))
          #     data_category[counter, column_index_frequency] <- posture_frequency_day #Save frequency of category entries 
          #   }
          # }
          for(b in 1:length(unique(data_day$behavior))){ #For each behavior related to the categories entered
            if(!is.na(unique(data_day$behavior)[b])){
              activity_sum_day <- sum(data_day[data_day$behavior == unique(data_day$behavior)[b], ]$total_duration, na.rm = TRUE) #Calculate total behavior duration
              column_index <- which(colnames(data_category) == unique(data_day$behavior)[b])
              data_category[counter, column_index] <- activity_sum_day #Save total duration entered for that behavior
            }
          }
          counter = counter + 1 #Next row for a new day
        }
      }
    }
  }
  data_category <- as.data.frame(data_category)
  data_category <- data_category[-which(data_category$castorID == 0), ]
  
  # Add day and if weekendday
  data_category$day <- weekdays(as.Date(data_category$date))
  for (day in 1:length(data_category$day)) {
    if(data_category$day[day] == "Saturday" || data_category$day[day] == "Sunday"){
      data_category$weekendday[day] <- 1
    } else {data_category$weekendday[day] <- 0}
  }
  library(dplyr)
  data_category <- data_category %>% relocate(c(day, weekendday), .before = time_passivescreen)
  
  # Save as.csv
  write.csv(data_category, paste0(datadir, "/", date, "_MLMapp_pp_duration_frequency_day.csv"))
}  