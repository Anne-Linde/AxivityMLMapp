#' load.app
#'
#' @description 'load.app' loads the cleaned app data

#' @param filepath A string that contains the path to the app data
#' @param filename A string that contains the name of the Excel app data file
#' @param cohort Integer indicating the cohort for the data selection. One of c(1, 2, 3)
#' @param measurementperiod Integer indicating the measurement period for the data selection. One of c(1, 2, 3)

#' @return DESCRIPTION HERE
#' @importFrom utils read.csv
#' @export

load.app <- function(filepath, filename, cohort, measurementperiod) {
  data.app <- read.csv(paste0(filepath, filename))[, -1] # Load in app data, skip first row numbering
  data.app <- data.app[data.app$cohort == cohort, ] #Select data for cohort
  data.app <- data.app[data.app$measurement == measurementperiod, ] #Select data for measurement period
  index <- which(is.na(data.app$castorID))
  data.app <- data.app[-index,] # Remove empty rows
  return(data.app)
}