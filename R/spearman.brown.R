#' spearman.brown
#'
#' @description 'spearman.brown' Preprocesses the epoch level data by removing non-wear, and invalid data
#'
#' @param singleICC Path to the root of the accelerometer data aggregated in epochs.
#' @param desiredR Integer that indicates epoch length in seconds, default = 5.
#' @return 

#' @export

spearman.brown <- function(singleICC, desiredR){
  return((desiredR*(1 - singleICC)/(singleICC*(1 - desiredR))))
}