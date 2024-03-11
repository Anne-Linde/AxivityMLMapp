# Participant characteristics
date <- "20231031" # Date of last data update

# Load demographic data
path.demo <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Demographics/"
file.demo <- "demografische_gegevens_minimal_with_zbmi.csv"
demographics <- read.csv(paste0(path.demo, file.demo), sep = ",")[,-1]

###### MLM app reliability
#valid_data_12h2days --> minimal set
path.app <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/App-data/20230918/"
file.minimal.app <- "valid_data_12h2d.RData"
load(paste0(path.app, file.minimal.app))
pp.app.rel <- unique(valid_data$castorID)
length(pp.app.rel)
demo.rel.app <- demographics[which(demographics$id %in% pp.app.rel,),]

# 6 Parents participated with two children
# 110316 (same as 110315)
demo.rel.app[demo.rel.app$id == "110316", c(8, 9, 10, 11, 12, 13, 14, 15, 16)] <- NA
# 110416 (same as 110415)
demo.rel.app[demo.rel.app$id == "110416", c(8, 9, 10, 11, 12, 13, 14, 15, 16)] <- NA
#11043 (same as 110224) -> respondent is missing, we know that this is a male
demo.rel.app[demo.rel.app$id == "110143", c(8)] <- 1
# 98MLM003 (same as 98MLM002), 98MLM030 (same as 98MLM022), 98MLM051 (same as 98MLM049)

# 103 missings, TO DO: check who we need to (re)send the questionnaire!!!
#calculate age, gender distribution, education distribution. #missing data?
age_parent <- c()
education <- c()
for(subj in 1:nrow(demo.rel.app)){
  if(demo.rel.app$id[subj] == "02MLM001"){
    age_parent <- c(age_parent, NA)
  } else if(!is.na(demo.rel.app$respondent[subj]) & demo.rel.app$respondent[subj] == "0"){
    age_parent <- c(age_parent, demo.rel.app$leeftijd_jaren_bio_moeder[subj])
    education <- c(education, demo.rel.app$opleiding_bio_moeder[subj])
    
  } else if(!is.na(demo.rel.app$respondent[subj]) & demo.rel.app$respondent[subj] == "1"){
    age_parent <- c(age_parent, demo.rel.app$leeftijd_jaren_bio_vader[subj])
    education <- c(education, demo.rel.app$opleiding_bio_vader[subj])
  } else{
    age_parent <- c(age_parent, NA)
    education <- c(education, NA)
  }
}
sum(is.na(age_parent)) #114 missende waarden
mean(age_parent, na.rm = TRUE)
sd(age_parent, na.rm = TRUE)

# Calulate gender distribution
table(as.factor(demo.rel.app$respondent))
prop.table(table(as.factor(demo.rel.app$respondent))) * 100 #86.7 female
# Calulate birth country mother and father
table(demo.rel.app$geboorteland_bio_moeder)
sum(table(demo.rel.app$geboorteland_bio_moeder)) # total 220 values, 220-1794
table(demo.rel.app$geboorteland_bio_vader)
sum(table(demo.rel.app$geboorteland_bio_vader)) # total 218 values, 218-181
#Education
table(education)

## Child
# age
mean(demo.rel.app$age_in_months_meting_1)
sd(demo.rel.app$age_in_months_meting_1)
# gender 2 = girl
table(as.factor(demo.rel.app$geslacht_kind))
prop.table(table(as.factor(demo.rel.app$geslacht_kind))) * 100 #48.8. girl



###### Accelerometer reliability
#12h2days --> minimal set
load("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/rerun/epochdata/hip/nonwear_removed/2d12h/results/axivity.estimates.RData")
hip <- axivity.estimates
load("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/rerun/epochdata/wrist/nonwear_removed/2d12h/results/axivity.estimates.RData")
wrist <- axivity.estimates
rm(axivity.estimates)
pp <- unique(c(hip$id, wrist$id))
length(pp)
demo.rel.acc <- demographics[which(demographics$id %in% pp,),]

# 3 Parents participated with two children
# 98MLM003 (same as 98MLM002), 98MLM030 (same as 98MLM022), 98MLM051 (same as 98MLM049)

#calculate age, gender distribution, education distribution. #missing data?
age_parent <- c()
education <- c()
for(subj in 1:nrow(demo.rel.acc)){
  if(demo.rel.acc$id[subj] == "02MLM001"){
    age_parent <- c(age_parent, NA)
  } else if(!is.na(demo.rel.acc$respondent[subj]) & demo.rel.acc$respondent[subj] == "0"){
    age_parent <- c(age_parent, demo.rel.acc$leeftijd_jaren_bio_moeder[subj])
    education <- c(education, demo.rel.acc$opleiding_bio_moeder[subj])
    
  } else if(!is.na(demo.rel.acc$respondent[subj]) & demo.rel.acc$respondent[subj] == "1"){
    age_parent <- c(age_parent, demo.rel.acc$leeftijd_jaren_bio_vader[subj])
    education <- c(education, demo.rel.acc$opleiding_bio_vader[subj])
  } else{
    age_parent <- c(age_parent, NA)
    education <- c(education, NA)
  }
}
sum(is.na(age_parent)) #14 missende waarden
mean(age_parent, na.rm = TRUE)
sd(age_parent, na.rm = TRUE)

# Calulate gender distribution
table(as.factor(demo.rel.acc$respondent))
prop.table(table(as.factor(demo.rel.acc$respondent))) * 100 #85.7 female
# Calulate birth country mother and father
table(demo.rel.acc$geboorteland_bio_moeder)
sum(table(demo.rel.acc$geboorteland_bio_moeder)) # total 63 values, 63-50
table(demo.rel.acc$geboorteland_bio_vader)
sum(table(demo.rel.acc$geboorteland_bio_vader)) # total 63 values, 63-181
#Education
table(education)

## Child
# age
mean(demo.rel.acc$age_in_months_meting_1)
sd(demo.rel.acc$age_in_months_meting_1)
# gender 2 = girl
table(as.factor(demo.rel.acc$geslacht_kind))
prop.table(table(as.factor(demo.rel.acc$geslacht_kind))) * 100 #45.3. girl
#zbmi
mean(demo.rel.acc$zbmi, na.rm = T)
sd(demo.rel.acc$zbmi, na.rm = T)


###### MLM app - accelerometers overlap (cohort 3 only)
load(paste0("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output/app_ax_entry_" , date, ".RData"))
# Delete rows for which no acceleration data is there
index_remove <- which(is.na(data.pp$ENMO.hip & data.pp$ENMO.wrist&data.pp$MAD.hip & data.pp$MAD.wrist))
data.pp <- data.pp[-index_remove,]
pp <- unique(data.pp$castorID)
length(pp)

#72 hip, 70 wrist
sum(wrist.enmo$castorID %in% hip.enmo$castorID == FALSE) # = 0, all wrist acc also have hip
sum(hip.enmo$castorID %in% wrist.enmo$castorID == FALSE) # 2 only hip and no wrist
demo.consistency <- demographics[which(demographics$id %in% pp),]
# 98MLM003 (same as 98MLM002), 98MLM026 (same as 98MLM025), 98MLM030 (same as 98MLM022), 98MLM051 (same as 98MLM049)
#remove_id <- c("98MLM003", "98MLM026")

## Parent
# Calculate age
# birthdate mother 02MLM001: strange 1077-05-21 -> removed
age_parent <- c()
education <- c()
for(subj in 1:nrow(demo.consistency)){
  if(demo.consistency$id[subj] == "02MLM001"){
    age_parent <- c(age_parent, NA)
  } else if(!is.na(demo.consistency$respondent[subj]) & demo.consistency$respondent[subj] == "0"){
    age_parent <- c(age_parent, demo.consistency$leeftijd_jaren_bio_moeder[subj])
    education <- c(education, demo.consistency$opleiding_bio_moeder[subj])
    
  } else if(!is.na(demo.consistency$respondent[subj]) & demo.consistency$respondent[subj] == "1"){
    age_parent <- c(age_parent, demo.consistency$leeftijd_jaren_bio_vader[subj])
    education <- c(education, demo.consistency$opleiding_bio_vader[subj])
  } else{
    age_parent <- c(age_parent, NA)
    education <- c(education, NA)
  }
}
sum(is.na(age_parent)) #14 missende waarden
mean(age_parent, na.rm = TRUE)
sd(age_parent, na.rm = TRUE)

# Calulate gender distribution
table(as.factor(demo.consistency$respondent))
prop.table(table(as.factor(demo.consistency$respondent))) * 100 #86.7 female

# Calulate birth country mother and father
sum(table(demo.consistency$geboorteland_bio_moeder))
table(demo.consistency$geboorteland_bio_moeder)
sum(table(demo.consistency$geboorteland_bio_vader))

table(demo.consistency$geboorteland_bio_vader)

#Education
table(education)

## Child
# age
mean(demo.consistency$age_in_months_meting_1) 
sd(demo.consistency$age_in_months_meting_1) 
# gender 2 = girl
table(as.factor(demo.consistency$geslacht_kind))
prop.table(table(as.factor(demo.consistency$geslacht_kind))) * 100 #44.4 girl
#zbmi
mean(demo.consistency$zbmi, na.rm = T)
sd(demo.consistency$zbmi, na.rm = T)



##### Flowchart
file.app.activities <- "20230918_MLMapp_activiteit_gedrag_duur_per_dag.csv"


load(paste0(path.app, file.app.activities))




