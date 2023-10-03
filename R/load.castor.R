#' load.castor
#' 
#' @description 'load.castor' loads the study data retrieved from Castor EDS
#' @param filepath A string that contains the path to the data
#' @param filename A string that contains the name of the Excel app data file
#' @param cohort Integer indicating the cohort for the data selection. One of c(1, 2, 3)
#' @return DESCRIPTION HERE
#' @importFrom utils read.csv
#' @export

load.castor <- function(filepath, filename, cohort) {
  data.castor <- read.csv(paste0(filepath, filename), sep = ";") # Load in castor export
  data.castor <- data.castor[data.castor$Cohort == cohort, ] # Select data for cohort
  data.castor <- data.castor[!is.na(data.castor$Cohort), ]
  return(data.castor)
}