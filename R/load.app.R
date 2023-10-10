#' load.app
#'
#' @description 'load.app' loads the cleaned app data

#' @param filepath A string that contains the path to the app data
#' @param filename A string that contains the name of the Excel app data file
#' @param cohort Vector indicating the cohort for the data selection. c(1, 2, 3)
#' @param measurementperiod Integer indicating the measurement period for the data selection. One of c(1, 2, 3)

#' @return DESCRIPTION HERE
#' @importFrom utils read.csv
#' @export

load.app <- function(filepath, filename, cohort, measurementperiod, sep) {
  data.app <- read.csv(paste0(filepath, filename), sep = sep)[, -1] # Load in app data, skip first row numbering
  data.app <- data.app[data.app$measurement == measurementperiod, ] #Select data for measurement period
  tmp.data <- data.frame()
  for(coh in 1:length(cohort)){
    tmp <- data.app[data.app$cohort == cohort[coh], ] #Select data for cohort
    tmp.data <- rbind(tmp.data, tmp)
    if(cohort[coh] == 2 | cohort[coh] == 3){ #99MLM002 cohort was not assigned, as included in multiple cohorts
      if(!("99MLM002" %in% tmp.data$castorID)){ # Include only if data was not yet selected
        tmp <- data.app[data.app$castorID == "99MLM002", ] #Select data for cohort
        tmp.data <- rbind(tmp.data, tmp)
      }
    }
  }
  index <- which(is.na(tmp.data$castorID))
  tmp.data <- tmp.data[-index,] # Remove empty rows
  return(tmp.data)
}
