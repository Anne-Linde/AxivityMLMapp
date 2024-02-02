#' load.castor
#' 
#' @description 'load.castor' loads the study data retrieved from Castor EDS
#' @param filepath A string that contains the path to the data
#' @param filename A string that contains the name of the Excel app data file
#' @param cohort Vector indicating the cohort for the data selection c(1, 2, 3)
#' @return DESCRIPTION HERE
#' @importFrom utils read.csv
#' @export

load.castor <- function(filepath, filename, cohort) {
  
  data.castor <- read.csv(paste0(filepath, filename), sep = ";") # Load in castor export
  
  tmp.data <- data.frame()
  for(coh in 1:length(cohort)){
    tmp <- data.castor[data.castor$Cohort == cohort[coh], ] #Select data for cohort
    if(cohort[coh] == 3 | cohort[coh] == 2){
      pp <- data.castor[data.castor$Participant.Id == "99MLM002", ] #Select data for pp, multiple cohorts
      tmp <- rbind(tmp, pp)
    }
    tmp.data <- rbind(tmp.data, tmp)
  }
  index <- which(is.na(tmp.data$castorID))
  index <- c(index, which(tmp.data$castorID) == "04MLM004") # This participant needs to be excluded (premature <32 weeks)
  
  if(length(index) > 0){
    tmp.data <- tmp.data[-index,] # Remove empty rows
  }
  return(tmp.data)
}