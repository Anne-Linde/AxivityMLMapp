#' load.structure.axivity
#'
#' @description 'load.structure.axivity' Loads the files using GGIR and calculate metrics, structures the milestone data in epoch level data
#'
#' @param tz Time zone specification to be used, default "Europe/Amsterdam".
#' @param filepath Path to the root of the accelerometer data.
#' @param outputdir Path to the root directory in which all milestone data will be saved.
#' @param validhours Integer that indicates the number of hours for a day to be valid (midnight-midnight), default = 16.
#' @param epochlength Integer that indicates epoch length in seconds, default = 5.
#' @param processeddir Path to the directory in which the generated milestone was saved.
#' @param overwrit Boolean to indicate if existing data needs to be overwritten, default = FALSE
#' @import GGIR
#' @export

load.structure.axivity <- function(tz = "Europe/Amsterdam", filepath, outputdir, validhours = 16, epochlength = 5, processeddir, overwrit = FALSE){

  # Load accelerometer files and calculate metrics
  GGIR::GGIR(
    # general settings
    desiredtz = tz,
    datadir = filepath,
    outputdir = outputdir,
    overwrite = FALSE, #FALSE = do not overwrite data if milestone data already exists -> N.B. if includedaycrit changes, data needs to be overwritten?
    do.parallel = FALSE,
    mode = c(1, 2), 
    #study protocol
    strategy = 1, #analyze all available data
    dayborder = 0, #consider data per day from midnight to midnight, equal to the app
    # part 1 raw data processing
    do.roll_med_acc_x = TRUE,
    do.roll_med_acc_y = TRUE, 
    do.roll_med_acc_z = TRUE, 
    do.enmo = TRUE,
    do.MAD = TRUE,
    # do.dev_roll_med_acc_x = TRUE, 
    # do.dev_roll_med_acc_y = TRUE,
    # do.dev_roll_med_acc_z = TRUE,
    
    # part 2 data quality and descriptives
    includedaycrit = validhours, #how many hours of valid data per day (midnight-midnight) is acceptable? dit moet opnieuw
    windowsizes = c(epochlength, 900, 3600), # 5-sec epoch, 15-sec rolling window, 60-sec non-wear, do we need to change this?
    do.imp = FALSE #skips automatic imputation of missing values
  )
  
  # Structure all resulting milestone data into epoch format
  milestonedir <- paste0(processeddir, "/meta/ms2.out")
  filelist <- list.files(milestonedir, pattern = ".RData")
  
  for(file in 1:length(filelist)){
    cat(file)
    name <- strsplit(filelist[file], "_00_")[[1]]
    location <- strsplit(name[2], ".cwa")[[1]][1] 
    filename = paste(paste0(processeddir, "/epochdata/", location), filelist[file], sep = "/") # Save data separately for hip and wrist
    if(file.exists(filename) & overwrit == FALSE){
      print("epochdata already saved")
    } else {
        quality_check <- read.csv(paste0(processeddir, "/results/QC/data_quality_report.csv"), sep = ",") # Load in castor export
        
        if(!is.na(quality_check$cal.error.end[file]) & quality_check$cal.error.end[file] < 0.01) { #Only load file if the post-calibration error is < 0.01 g
          # Convert timestamp to POSIX for convenience
          load(paste(milestonedir, filelist[file], sep = "/")) # Load RData file
          tsFormat = "%Y-%m-%dT%H:%M:%S%z"
          tz = ""
          IMP$metashort$timestampPOSIX = as.POSIXlt(IMP$metashort$timestamp, format = tsFormat, tz = tz)
          
          # Interpolate scores as they are in a different resolution
          shortEpochLength = IMP$windowsizes[1]
          longEpochLength = IMP$windowsizes[2]
          NlongInsideShort = longEpochLength / shortEpochLength
          scores = IMP$rout[rep(1:nrow(IMP$rout), each = NlongInsideShort),]
          colnames(scores) = c("nonwear", "clipping", "additonal_nonwear", "studyprotocol", "all") # add column names
          IMP$metashort = cbind(IMP$metashort, scores) # combine scores with the epoch level time series
          epochdata <- IMP$metashort
          
          save(epochdata, file = filename)
        }
      }
  }  
}