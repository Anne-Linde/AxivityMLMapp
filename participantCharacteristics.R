# Participant characteristics
date <- "20240423" # Date of last data update

# Load demographic data
path.demo <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Demographics/"
file.demo <- paste0(date, "_demografische_gegevens_minimal_with_zbmi.csv")
demographics <- read.csv(paste0(path.demo, file.demo), sep = ",")[,-1]

###### MLM app reliability
#valid_data_12h2days --> minimal set
path.app <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/App-data/20230918/"
file.minimal.app <- "valid_data_12h2d.RData"
load(paste0(path.app, file.minimal.app))
pp.app.rel <- unique(valid_data$castorID)
length(pp.app.rel)
demo.rel.app <- demographics[which(demographics$castorID %in% pp.app.rel,),]

# 7 Parents participated with two children
# 110069 (same as 110108)
# 110224 (same as 110143)
# 110315 (same as 110316)
# 110416 (same as 110415)
# 98MLM003 (same as 98MLM002)
# 98MLM030 (same as 98MLM022)
# 98MLM051 (same as 98MLM049)
demo.rel.app[demo.rel.app$castorID == "110069", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                  "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA
demo.rel.app[demo.rel.app$castorID == "110224", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                  "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA
demo.rel.app[demo.rel.app$castorID == "110315", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                  "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA
demo.rel.app[demo.rel.app$castorID == "110416", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                  "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA
demo.rel.app[demo.rel.app$castorID == "98MLM003", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                  "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA
demo.rel.app[demo.rel.app$castorID == "98MLM030", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                    "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA
demo.rel.app[demo.rel.app$castorID == "98MLM051", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                    "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA

# Age parent
sum(is.na(demo.rel.app$age_years_respondent_parent1)) #51 - 7 = 44 missing values
mean(demo.rel.app$age_years_respondent_parent1, na.rm = TRUE)
sd(demo.rel.app$age_years_respondent_parent1, na.rm = TRUE)

# Calulate gender distribution
sum(is.na(demo.rel.app$respondent)) #8 - 7 = 1 missing values
table(as.factor(demo.rel.app$respondent))
prop.table(table(as.factor(demo.rel.app$respondent))) * 100 #91.1 female

# Calulate birth country mother and father
sum(is.na(demo.rel.app$birth_country_bio_mother)) #34 - 7 = 27 missing values
table(demo.rel.app$birth_country_bio_mother)
sum(table(demo.rel.app$birth_country_bio_mother)) # 290-227 = 63 niet in NL
sum(is.na(demo.rel.app$birth_country_bio_father)) #43 - 7 = 36 missing values
table(demo.rel.app$birth_country_bio_father)
sum(table(demo.rel.app$birth_country_bio_father)) # 281-227 = 54 niet in NL

#Education
sum(is.na(demo.rel.app$education_respondent_parent1)) #44+13 - 7 = 50 missing values
table(demo.rel.app$education_respondent_parent1)
sum(table(demo.rel.app$education_respondent_parent1)) #280+12-6

## Child
# age
mean(demo.rel.app$age_in_months_measurement_period_1, na.rm = T)
sd(demo.rel.app$age_in_months_measurement_period_1, na.rm = T)
# gender 2 = girl
table(as.factor(demo.rel.app$gender_child))
prop.table(table(as.factor(demo.rel.app$gender_child))) * 100 #48.8. girl

###### Accelerometer reliability
#12h2days --> minimal set
load("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/rerun/epochdata/hip/nonwear_removed/2d12h/results/axivity.estimates.RData")
hip <- axivity.estimates
load("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/rerun/epochdata/wrist/nonwear_removed/2d12h/results/axivity.estimates.RData")
wrist <- axivity.estimates
rm(axivity.estimates)
pp <- unique(c(hip$id, wrist$id))
length(pp)
demo.rel.acc <- demographics[which(demographics$castorID %in% pp,),]

# 3 Parents participated with two children
# 98MLM003 (same as 98MLM002)
# 98MLM030 (same as 98MLM022)
# 98MLM051 (same as 98MLM049)
demo.rel.acc[demo.rel.acc$castorID == "98MLM003", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                    "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA
demo.rel.acc[demo.rel.acc$castorID == "98MLM030", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                    "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA
demo.rel.acc[demo.rel.acc$castorID == "98MLM051", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                    "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA


# Age parent
sum(is.na(demo.rel.acc$age_years_respondent_parent1)) #10 - 3 = 7 missing values
mean(demo.rel.acc$age_years_respondent_parent1, na.rm = TRUE)
sd(demo.rel.acc$age_years_respondent_parent1, na.rm = TRUE)

# Calulate gender distribution
sum(is.na(demo.rel.acc$respondent)) #3 - 3 = 0 missing values
table(as.factor(demo.rel.acc$respondent))
prop.table(table(as.factor(demo.rel.acc$respondent))) * 100 #87.5 female

# Calulate birth country mother and father
sum(is.na(demo.rel.acc$birth_country_bio_mother)) #9 - 3 = 6 missing values
table(demo.rel.acc$birth_country_bio_mother)
sum(table(demo.rel.acc$birth_country_bio_mother)) # 66-53 = 13 niet in NL
sum(is.na(demo.rel.acc$birth_country_bio_father)) #9 - 3 = 6 missing values
table(demo.rel.acc$birth_country_bio_father)
sum(table(demo.rel.acc$birth_country_bio_father)) # 66-53 = 13 niet in NL

#Education
sum(is.na(demo.rel.acc$education_respondent_parent1)) #9 - 3 = 6 missing values
table(demo.rel.acc$education_respondent_parent1)
sum(table(demo.rel.acc$education_respondent_parent1)) #280+12-6

## Child
# age
mean(demo.rel.acc$age_in_months_measurement_period_1, na.rm = T)
sd(demo.rel.acc$age_in_months_measurement_period_1, na.rm = T)
# gender 2 = girl
table(as.factor(demo.rel.acc$gender_child))
prop.table(table(as.factor(demo.rel.acc$gender_child))) * 100 #45.3. girl

#zbmi
mean(demo.rel.acc$zbmi, na.rm = T)
sd(demo.rel.acc$zbmi, na.rm = T)


###### MLM app - accelerometers overlap (cohort 3 only)
date <- "20231031" # App data until measurement period 1 is sufficient
load(paste0("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output/app_ax_entry_" , date, ".RData"))
# Delete rows for which no acceleration data is there
index_remove <- which(is.na(data.pp$ENMO.hip & data.pp$ENMO.wrist&data.pp$MAD.hip & data.pp$MAD.wrist))
data.pp <- data.pp[-index_remove,]
pp <- unique(data.pp$castorID)
length(pp)

# #72 hip, 70 wrist
# sum(wrist.enmo$castorID %in% hip.enmo$castorID == FALSE) # = 0, all wrist acc also have hip
# sum(hip.enmo$castorID %in% wrist.enmo$castorID == FALSE) # 2 only hip and no wrist
# demo.consistency <- demographics[which(demographics$id %in% pp),]

demo.rel.acc.app <- demographics[which(demographics$castorID %in% pp,),]

# 3 Parents participated with two children
# 98MLM003 (same as 98MLM002)
# 98MLM030 (same as 98MLM022)
# 98MLM051 (same as 98MLM049)
demo.rel.acc.app[demo.rel.acc.app$castorID == "98MLM003", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                    "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA
demo.rel.acc.app[demo.rel.acc.app$castorID == "98MLM030", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                    "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA
demo.rel.acc.app[demo.rel.acc.app$castorID == "98MLM051", c("respondent", "gender_respondent", "birth_country_bio_mother", "birth_country_bio_father", 
                                                    "dob_respondent", "age_years_respondent_parent1", "education_respondent_parent1", "education_partner")] <- NA

# Age parent
sum(is.na(demo.rel.acc.app$age_years_respondent_parent1)) #10 - 3 = 7 missing values
mean(demo.rel.acc.app$age_years_respondent_parent1, na.rm = TRUE)
sd(demo.rel.acc.app$age_years_respondent_parent1, na.rm = TRUE)

# Calulate gender distribution
sum(is.na(demo.rel.acc.app$respondent)) #3 - 3 = 0 missing values
table(as.factor(demo.rel.acc.app$respondent))
prop.table(table(as.factor(demo.rel.acc.app$respondent))) * 100 #88.4 female

# Calulate birth country mother and father
sum(is.na(demo.rel.acc.app$birth_country_bio_mother)) #9 - 3 = 6 missing values
table(demo.rel.acc.app$birth_country_bio_mother)
sum(table(demo.rel.acc.app$birth_country_bio_mother)) # 63-47 = 16 niet in NL
sum(is.na(demo.rel.acc.app$birth_country_bio_father)) #9 - 3 = 6 missing values
table(demo.rel.acc.app$birth_country_bio_father)
sum(table(demo.rel.acc.app$birth_country_bio_father)) # 63-50 = 13 niet in NL

#Education
sum(is.na(demo.rel.acc.app$education_respondent_parent1)) #9 - 3 = 6 missing values
table(demo.rel.acc.app$education_respondent_parent1)
sum(table(demo.rel.acc.app$education_respondent_parent1)) #63-8=55

## Child
# age
mean(demo.rel.acc.app$age_in_months_measurement_period_1, na.rm = T)
sd(demo.rel.acc.app$age_in_months_measurement_period_1, na.rm = T)
# gender 2 = girl
table(as.factor(demo.rel.acc.app$gender_child))
prop.table(table(as.factor(demo.rel.acc.app$gender_child))) * 100 #45.8. girl

#zbmi
mean(demo.rel.acc.app$zbmi, na.rm = T)
sd(demo.rel.acc.app$zbmi, na.rm = T)

