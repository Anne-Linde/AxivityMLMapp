#' link.app.castor
#'
#' @description 'link.app.castor' links Castor IDs and cohort numbers to motor milestones and activities data collected from the MLM app. The linked data is then saved as CSV files.
#'
#' @param datadir.app A character string specifying the directory where the MLM app data files are located.
#' @param datadir.castor A character string specifying the directory where the Castor data file is located.
#' @param filename.castor A character string specifying the name of the Castor data file.
#' @param date A character string specifying the date used for naming the input and output CSV files.
#'
#' @return This function does not return a value. It saves two CSV files with the linked data in the specified directory.
#'
#' @export

link.app.castor <- function(datadir.app, datadir.castor, filename.castor, date) {
  
  # Load in MLM app data, motor milestones and Castor data 
  data.motormilestones <- read.csv(paste(datadir.app, paste0(date, "_motormilestones.csv"), sep = "/"))[,-1]
  data.activities <- read.csv(paste(datadir.app, paste0(date, "_activities.csv"), sep = "/"))[,-1]
  data.castor <- read.csv(paste(datadir.castor, filename.castor, sep = "/"), sep = ";")
  
  # Link Castor id and cohort number
  castorID <- rep(NA, nrow(data.motormilestones))
  cohort <- rep(NA, nrow(data.motormilestones))
  measurement <- rep(NA, nrow(data.motormilestones))
  data.motormilestones <- cbind(castorID, cohort, measurement, data.motormilestones)
  castorID <- rep(NA, nrow(data.activities))
  cohort <- rep(NA, nrow(data.activities))
  measurement <- rep(NA, nrow(data.activities))
  
  data.activities <- cbind(castorID, cohort, measurement, data.activities)
  rm(castorID, cohort, measurement)
  
  for(row in 1:nrow(data.castor)){ # For each participant in the Castor data
    for(measurementperiod in 1:3){ # For all three measurementperiods
      # Select all unique research codes for this participant
      if(measurementperiod == 1){
        researchcodes <- c(data.castor$X5.1.Research.code.particpant[row], data.castor$Research_code_new_1[row])
      } else if(measurementperiod == 2){
        researchcodes <- c(data.castor$X12.1.Research.code.particpant[row], data.castor$Research_code_new_2[row])
      } else {
        researchcodes <- c(data.castor$X18.1.Research.code.particpant[row], data.castor$Research_code_new_3[row])
      }
      
      ## Add Castor ID, cohort and measurement period
      # To motor milestones data
      intake_index <- which(data.motormilestones$accessCode %in% researchcodes)
      if(!length(intake_index) == 0){
        for(intake_row in 1:length(intake_index)){
          data.motormilestones[intake_index[intake_row],1] <- data.castor$Participant.Id[row]
          data.motormilestones[intake_index[intake_row],2] <- data.castor$Cohort[row]
          data.motormilestones[intake_index[intake_row],3] <- measurementperiod
        }
      }
      # To MLM app activity entries
      activities_index <- which(data.activities$accessCode %in% researchcodes)
      if(!length(activities_index) == 0){
        for(activity_row in 1:length(activities_index)){
          data.activities[activities_index[activity_row],1] <- data.castor$Participant.Id[row]
          data.activities[activities_index[activity_row],2] <- data.castor$Cohort[row]
          data.activities[activities_index[activity_row],3] <- measurementperiod
          }
      }
    }
  }
  # Remove test data (for which no Castor ID is available)
  data.motormilestones <- data.motormilestones[-which(is.na(data.motormilestones$castorID)), ]
  data.activities <- data.activities[-which(is.na(data.activities$castorID)), ]
  
  # Save as .csv
  write.csv(data.motormilestones, paste0(datadir.app, "/", date, "_motormilestones_castor_linked.csv"))
  write.csv(data.activities, paste0(datadir.app, "/", date, "_activities_castor_linked.csv"))
}