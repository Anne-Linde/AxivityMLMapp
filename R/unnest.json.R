# Required libraries
#install.packages("rjson", "haven")
library("rjson", "haven") # for reading in json files,. for saving data.frame as .sav

unnest.json <- function(datadir, date){
  ### STEP 1: Load .json data files ###
  # This file includes the research codes (accessCode) and the corresponding participant ids (_id)
  participants <- rjson::fromJSON(file = paste(datadir, "participants.json", sep = "/")) #same information in participants.json
  # This file includes the answers to the intake questions for each participant (participantId)
  intake <- rjson::fromJSON(file = paste(datadir, "measurementperiods.json", sep = "/"))
  # This file includes all registered activities of all participants (measurementPeriod)
  result <- rjson::fromJSON(file = paste(datadir, "measuringmoments.json", sep = "/"))
  # This file includes the questions and the corresponding answer labels: 
  ## [[1]] = intake, including the intake questions ($questions) with answer labels ($answers)
  ## Then app categories, including follow-up questions ($questions), and their corresponding answer labels ($answers)
  ## [[2]]$category$_type = verzorging, [[3]]$category$_type = etendrinken, 
  ## [[4]]$category$_type = zittenliggen, [[5]]$category$_type = spelen, 
  ## [[6]]$category$_type = passiefverplaatsen, [[7]]$category$_type = actiefverplaatsen, 
  ## [[8]]$category$_type = beeldscherm, [[9]]$category$_type = slapen, 
  ## [[10]]$category$_type = other, [[11]]$category$_type = weetniet, 
  question_labels <- rjson::fromJSON(file = paste(datadir, "questionnaires.json", sep = "/"))
  
  ### STEP 2: Convert JSON files to unnested data list
  ## Participant file
  participants_untangled <- data.frame()
  for (json_element in 1:length(participants)) {
    participants_untangled[json_element, 1] <- participants[[json_element]]$'_id'
    participants_untangled[json_element, 2] <- participants[[json_element]]$accessCode
    participants_untangled[json_element, 3] <- participants[[json_element]]$'_createdAt'
    participants_untangled[json_element, 4] <- participants[[json_element]]$'_updatedAt'
  }
  colnames(participants_untangled) <- c("id", "accessCode", "created", "updated")
  
  ## Intake file
  intake_untangled <- data.frame()
  for (json_element in 1:length(intake)) {
    if(intake[[json_element]]$intakeVersion == 1 | intake[[json_element]]$'_version' == 1){ # skip the first version of the intake
      next
    } 
    intake_untangled[json_element, 1] <- intake[[json_element]]$'_id' #check if this pp id is still relevant?
    intake_untangled[json_element, 2] <- intake[[json_element]]$participantId
    intake_untangled[json_element, 3] <- intake[[json_element]]$startDate
    
    for(question in 1:length(intake[[json_element]]$intakeLabels)){
      # Age child
      if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 0){
        intake_untangled[json_element, 4] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
        # Based on the age of the child, some milestone questions were skipped
        if (intake_untangled[json_element, 4] == 3){
          intake_untangled[json_element, 5:10] <- NA
        } else if (intake_untangled[json_element, 4] == 4 | intake_untangled[json_element, 4] == 5) {
          intake_untangled[json_element, 5:17] <- NA
        }
      }
      
      ## Motor milestones
      # Roll from back to tummy
      else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 1){
        intake_untangled[json_element, 5] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber
        if (intake[[json_element]]$intakeLabels[[question]]$answerNumber == 2){
          intake_untangled[json_element, 6] <- NA # If child is not able to do this, the follow-up question (age) will not be asked
        }
       # Age to reach motor milestone
      } else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 11){
        intake_untangled[json_element, 6] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
      }
      # Roll from tummy to back
      else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 2){
        intake_untangled[json_element, 7] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
        if (intake[[json_element]]$intakeLabels[[question]]$answerNumber == 2){
          intake_untangled[json_element, 8] <- NA # If child is not able to do this, the follow-up question (age) will not be asked
        } 
       # Age to reach motor milestone
      } else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 21){
        intake_untangled[json_element, 8] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
      }
      # Sit without support
      else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 3){
        intake_untangled[json_element, 9] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
        if (intake[[json_element]]$intakeLabels[[question]]$answerNumber == 2){
          intake_untangled[json_element, 10] <- NA # If child is not able to do this, the follow-up question (age) will not be asked
        } 
        # Age to reach motor milestone
      } else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 31){
        intake_untangled[json_element, 10] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
      }
      # Crawling
      else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 4){
        intake_untangled[json_element, 11] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
        if (intake[[json_element]]$intakeLabels[[question]]$answerNumber == 1){
          intake_untangled[json_element, 12] <- NA # If child is able to do this, don't ask if the child can pre-crawl
        } 
        # Pre-crawling
      } else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 41){
        intake_untangled[json_element, 12] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
        if (intake[[json_element]]$intakeLabels[[question]]$answerNumber == 2){
          intake_untangled[json_element, 13] <- NA # If child is not able to do this, don't ask the follow-up (age) question
        } 
        # Age to reach motor milestone
      } else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 42){
        intake_untangled[json_element, 13] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
      }
      # Standing
      else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 5){
        intake_untangled[json_element, 14] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
        if (intake[[json_element]]$intakeLabels[[question]]$answerNumber == 2){
          intake_untangled[json_element, 15] <- NA # If child is able to do this, don't ask the follow-up (age) question
        } 
        # Age to reach motor milestone
      } else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 51){
        intake_untangled[json_element, 15] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
      }   
      # Walking without support
      else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 6){
        intake_untangled[json_element, 16] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
        if (intake[[json_element]]$intakeLabels[[question]]$answerNumber == 2){
          intake_untangled[json_element, 17] <- NA # If child is able to do this, don't ask the follow-up (age) question
        } 
        # Age to reach motor milestone
      } else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 61){
        intake_untangled[json_element, 17] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
      }    
      # First time to fill in questionnaire
      else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 7){
        intake_untangled[json_element, 18] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
        if (intake[[json_element]]$intakeLabels[[question]]$answerNumber == 2){
          intake_untangled[json_element, 5:16] <- NA 
        }
      } else if (intake[[json_element]]$intakeLabels[[question]]$questionNumber == 71){
        intake_untangled[json_element, 19] <- intake[[json_element]]$intakeLabels[[question]]$answerNumber 
      }           
      else
        next
    }
  }
  colnames(intake_untangled) <- c("id", "pp", "date", "age_group", 
                                  "roll_back_to_tummy", "roll_back_to_tummy_age", 
                                  "roll_tummy_to_back", "roll_tummy_to_back_age", 
                                  "sit", "sit_age",
                                  "crawl", "precrawl", "crawl_age",
                                  "stand", "stand_age",
                                  "walk", "walk_age", 
                                  "first_intake", "walk_age2")
  intake_untangled <- intake_untangled[-which(is.na(intake_untangled$pp)), ]
  
  ## Results file
  results_untangled <- data.frame()
  pp <- na.exclude(intake_untangled$id)
  for (json_element in 1:length(result)) {
    #extract pp id
    pp_id <- result[[json_element]]$measurementPeriodId #this id links with "_id" from the intake
    if(!pp_id %in% pp){ # Only include activities of pp id's with app version 2
      next
    }
    else{
      results_untangled[json_element, 1] <- pp_id
      results_untangled[json_element, 2] <- strsplit(result[[json_element]]$startTime, split = "T")[[1]][1] #date
      results_untangled[json_element, 3] <- strsplit(result[[json_element]]$startTime, split = "T")[[1]][2] #start time
      datecheck <- strsplit(result[[json_element]]$endTime, split = "T")[[1]][1] # check if it is still the same day
      if(datecheck == results_untangled[json_element, 2]) {
        results_untangled[json_element, 4] <- strsplit(result[[json_element]]$endTime, split = "T")[[1]][2] #end time
      } else {
        print(paste0("filled in activity extends to the next day, row: ", json_element))
        results_untangled[json_element, 4] <- result[[json_element]]$endTime #end time and date
      }
      # Activity
      results_untangled[json_element, 5] <- result[[json_element]]$labels[[1]]$category$'_type'
      if(results_untangled[json_element, 5] == "verzorging" |
         results_untangled[json_element, 5] == "etendrinken" |
         results_untangled[json_element, 5] == "passiefverplaatsen" |
         results_untangled[json_element, 5] == "actiefverplaatsen" |
         results_untangled[json_element, 5] == "other" |
         results_untangled[json_element, 5] == "iemandanders" |
         results_untangled[json_element, 5] == "weetniet") { 
        next # there are no follow-up questions
      }
      # Follow-up questions
      else if(results_untangled[json_element, 5] == "zittenliggen") {
        results_untangled[json_element, 6] <- result[[json_element]]$labels[[2]]$answerNumber #with whom
        results_untangled[json_element, 7] <- result[[json_element]]$labels[[3]]$answerNumber #where
        results_untangled[json_element, 8] <- result[[json_element]]$labels[[1]]$answerNumber #posture sit/lying
      } else if(results_untangled[json_element, 5] == "spelen") {
        results_untangled[json_element, 6] <- result[[json_element]]$labels[[3]]$answerNumber #with whom
        results_untangled[json_element, 7] <- result[[json_element]]$labels[[4]]$answerNumber #where
        results_untangled[json_element, 8] <- NA
        results_untangled[json_element, 9] <- result[[json_element]]$labels[[2]]$answerNumber #posture
        results_untangled[json_element, 10] <- result[[json_element]]$labels[[1]]$answerNumber #intensity
      } else if(results_untangled[json_element, 5] == "beeldscherm") {
        results_untangled[json_element, 6] <- result[[json_element]]$labels[[3]]$answerNumber #with whom
        results_untangled[json_element, 7:10] <- NA
        results_untangled[json_element, 11] <- result[[json_element]]$labels[[1]]$answerNumber #activity type/intensity
        results_untangled[json_element, 12] <- result[[json_element]]$labels[[2]]$answerNumber #screen type
      } else if(results_untangled[json_element, 5] == "slapen") {
        for (question in 1:length(result[[json_element]]$labels)) {
          if(result[[json_element]]$labels[[question]]$questionNumber == 1){
            results_untangled[json_element, 7:12] <- NA
            results_untangled[json_element, 13] <- result[[json_element]]$labels[[question]]$answerNumber #posture
          } else if (result[[json_element]]$labels[[question]]$questionNumber == 2){
            results_untangled[json_element, 6] <- result[[json_element]]$labels[[question]]$answerNumber #with whom
          }
        }
      } 
    }
  }
  colnames(results_untangled) <- c("pp", "date", "startTime", "endTime", "activity", 
                                   "with_whom", "where", 
                                   "sit_lying_posture", 
                                   "play_posture", "play_intensity", 
                                   "screen_activity", "screen_type",
                                   "sleep_posture")
  
  results_untangled <- results_untangled[-which(is.na(results_untangled$pp)), ]
  results_untangled$activity <- as.factor(results_untangled$activity)
  results_untangled$activity <- dplyr::recode(results_untangled$activity, weetniet = "dontknow", iemandanders = "someoneelse",
                                              zittenliggen = "sittinglying", beeldscherm = "screen", verzorging = "personalcare",
                                              spelen = "playing", slapen = "sleeping", other = "otheractivity", etendrinken = "eatingdrinking",
                                              actiefverplaatsen = "activetransport", passiefverplaatsen = "passivetransport", .default = NA_character_)
  
  
  ### STEP 3: Link the research codes
  code_intake <- c()
  for(pp in 1:nrow(intake_untangled)) {
    if(!is.na(intake_untangled$pp[[pp]])){
      intake_untangled$pp[pp]
      index_link <- which(participants_untangled$id %in% intake_untangled$pp[pp])
      code_intake <- c(code_intake, participants_untangled$accessCode[index_link])
    } else {
      code_intake <- c(code_intake, NA)
    } 
  }
  motormilestones <- cbind(code_intake, intake_untangled)
  colnames(motormilestones) <- c("accessCode", "id", "pp", "date", "age.group", 
                                 "roll_back_to_tummy", "roll_back_to_tummy_age", 
                                 "roll_tummy_to_back", "roll_tummy_to_back_age", 
                                 "sit", "sit_age",
                                 "crawl", "precrawl", "crawl_age",
                                 "stand", "stand_age",
                                 "walk", "walk_age", 
                                 "first_intake", "walk_age2")
  rm(code_intake, index_link, pp)
  
  code_results <- c()
  for(pp in 1:nrow(results_untangled)) {
    if(!is.na(results_untangled$pp[pp])){
      index_link <- which(motormilestones$id %in% results_untangled$pp[pp])
      code_results <- c(code_results, motormilestones$accessCode[index_link])
    }
    else {
      code_results <- c(code_results, NA)
    }
  }
  activities <- cbind(code_results, results_untangled)
  colnames(activities) <- c("accessCode", "pp", "date", "startTime", "endTime", "activity", 
                            "with_whom", "where", 
                            "sit_lying_posture", 
                            "play_posture", "play_intensity", 
                            "screen_activity", "screen_type",
                            "sleep_posture")
  rm(code_results, index_link, pp)
  
  ### STEP 4: Save the data
  # Save as .csv
  write.csv(motormilestones, paste0(datadir, "/", date, "_motormilestones.csv"))
  write.csv(activities, paste0(datadir, "/", date, "_activities.csv"))
}

