### This script examines the differences between acceleration values of the app categories by:
## 1) Fitting general linear mixed models with seperate random intercepts for activity over participants
## 2) Post hov

rm(list = ls())
gc()
#### User settings
## User input 
date <- "20231031" # Date of last data update
#tz = "Europe/Amsterdam"
datadir <- paste0("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output/app_ax_entry_" , date, ".RData")
savedir <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output/agreement"

# Load acceleration values for activity entries
load(datadir)
load("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output/logtransformed_app_ax_entry_20231025.RData")

# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/AxivityMLMapp/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

# Long data
df_long <- tidyr::gather(data.pp, metric, value, logENMO.hip:logMAD.wrist)
placement <- c()
metric <- c()
for(row in 1:nrow(df_long)){
  variable <- strsplit(df_long$metric[row], split = '[.]')
  placement <- c(placement, variable[[1]][2])
  metric <- c(metric, variable[[1]][1])
}
df_long$metric <-metric
df_long$placement <- placement


#### STEP 1: Plot the acceleration distributions in the different app categories
# Reshape data to long format
df_long <- tidyr::gather(data.pp, metric, value, ENMO.hip:MAD.wrist)
df_long.ENMO <- df_long[which(startsWith(df_long$metric, "ENMO")),]
df_long.MAD <- df_long[which(startsWith(df_long$metric, "MAD")),]

### Category distributions
## BOXPLOT
level_order <- c("sleeping", "sittinglying", "personalcare", "eatingdrinking", "passivescreen", 
                 "activescreen", "passivetransport", "activetransport", "quietplay", 
                 "activeplay", "dontknowplay")
# level_order <- c("sleeping", "sittinglying", "personalcare", "eatingdrinking", "passivescreen", 
#                  "activescreen", "passivetransport", "activetransport", "quietplay", 
#                  "activeplay", "dontknowplay", "someoneelse", "otheractivity", "dontknow")
level_labels <- c("Sleeping", "Sitting/lying", "Personal care", "Eating/drinking", "Passive screen use", 
                 "Active screen use", "Passive transport", "Active transport", "Calm play", 
                 "Active Play", "Don't know play")

# Nice to have: number of entries per category
# n_fun <- function(x){
#   return(data.frame(y = 0.95*70,
#                     label = length(x)))
# }

bxp_ENMO <- ggplot2::ggplot(df_long.ENMO, ggplot2::aes(y = value, x = as.factor(activity), fill =metric)) + 
  ggplot2::geom_boxplot(outlier.shape = 19, outlier.alpha = 0.1) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = level_order, labels = level_labels) + ggplot2::xlab("") + 
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) +
  ggplot2::scale_fill_discrete(name = "Accelerometer placement", labels = c("Hip", "Wrist")) #+
 # ggplot2::stat_summary(fun.data = n_fun, geom = "text", vjust = -20) 

bxp_MAD <- ggplot2::ggplot(df_long.MAD, ggplot2::aes(y = value, x = as.factor(activity), fill =metric)) + 
  ggplot2::geom_boxplot(outlier.shape = 19, outlier.alpha = 0.1) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = level_order, labels = level_labels) + ggplot2::xlab("App categories") + 
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) +
  ggplot2::scale_fill_discrete(name = "Accelerometer placement", labels = c("Hip", "Wrist"))

gridExtra::grid.arrange(bxp_ENMO, bxp_MAD, nrow=2) #arranges plots within grid

boxplots <- gridExtra::arrangeGrob(bxp_ENMO + ggplot2::theme(legend.position="top"),
                                   bxp_MAD + ggplot2::theme(legend.position="none"),
                                   nrow=2) # generates plot
plot(boxplots) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/categories/boxplot_categories.png"), boxplots, width = 10, height = 8, dpi = 600) #saves g

#### STEP 2: Test for differences in acceleration (use g-units instead of mg)

# Transform acceleration data
data.pp$logENMO.hip <- log((data.pp$ENMO.hip/1000) + 0.001) 
data.pp$logENMO.wrist <- log((data.pp$ENMO.wrist/1000) + 0.001)
data.pp$logMAD.hip <- log((data.pp$MAD.hip/1000) + 0.001)
data.pp$logMAD.wrist <- log((data.pp$MAD.wrist/1000) + 0.001)
save(data.pp, file = paste0("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output/app_ax_entry_log_", date, ".RData"))

### Hip placement
## ENMO
# Un-transformed data
acc_act.hipENMO <- glm(ENMO.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping"), data = data.pp)
acc_act_id.hipENMO <- lme4::lmer(ENMO.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.hipENMO, acc_act_id.hipENMO, test="Chisq")
rm(acc_act.hipENMO)
acc_act_act_id.hipENMO <- lme4::lmer(ENMO.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.hipENMO, acc_act_act_id.hipENMO, test="Chisq")
rm(acc_act_id.hipENMO)

#Plot normality of residuals
plot(acc_act_act_id.hipENMO)
qqnorm(residuals(acc_act_act_id.hipENMO))

# Transformed data
acc_act.loghipENMO <- glm(logENMO.hip ~ forcats::fct_relevel(activity, ref = "sleeping"), data = data.pp)
acc_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.loghipENMO, acc_act_id.loghipENMO, test="Chisq")
rm(acc_act.loghipENMO)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.loghipENMO, acc_act_act_id.loghipENMO, test="Chisq")
rm(acc_act_id.loghipENMO)

#Plot normality of residuals
plot(acc_act_act_id.loghipENMO)
qqnorm(residuals(acc_act_act_id.loghipENMO))

# Report fitted model based on transformed data
rm(acc_act_act_id.hipENMO)
#summary(acc_act_act_id.hipENMO)
car::Anova(acc_act_act_id.loghipENMO)
jtools::summ(acc_act_act_id.loghipENMO)
report::report_table(acc_act_act_id.loghipENMO)
# Use following lines to relevel the models for the different contrasts
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "sittinglying")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "eatingdrinking")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "passivescreen")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "activescreen")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "quietplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "dontknowplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "activeplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "activetransport")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
report::report_table(acc_act_act_id.loghipENMO)
jtools::summ(acc_act_act_id.loghipENMO)

## MAD
# Un-transformed data
acc_act.hipMAD <- glm(MAD.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping"), data = data.pp)
acc_act_id.hipMAD <- lme4::lmer(MAD.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.hipMAD, acc_act_id.hipMAD, test="Chisq")
rm(acc_act.hipMAD)
acc_act_act_id.hipMAD <- lme4::lmer(MAD.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.hipMAD, acc_act_act_id.hipMAD, test="Chisq")
rm(acc_act_id.hipMAD)

#Plot normality of residuals
plot(acc_act_act_id.hipMAD)
qqnorm(residuals(acc_act_act_id.hipMAD))

# Transformed data
acc_act.loghipMAD <- glm(logMAD.hip ~ forcats::fct_relevel(activity, ref = "sleeping"), data = data.pp)
acc_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.loghipMAD, acc_act_id.loghipMAD, test="Chisq")
rm(acc_act.loghipMAD)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.loghipMAD, acc_act_act_id.loghipMAD, test="Chisq")
rm(acc_act_id.loghipMAD)

#Plot normality of residuals
plot(acc_act_act_id.loghipMAD)
qqnorm(residuals(acc_act_act_id.loghipMAD))

# Report fitted model based on transformed data
rm(acc_act_act_id.hipMAD)
#summary(acc_act_act_id.hipMAD)
car::Anova(acc_act_act_id.loghipMAD)
jtools::summ(acc_act_act_id.loghipMAD)
report::report_table(acc_act_act_id.loghipMAD)
# Use following lines to relevel the models for the different contrasts
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "sittinglying")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "eatingdrinking")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "passivescreen")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "activescreen")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "quietplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "dontknowplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "activeplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "activetransport")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
report::report_table(acc_act_act_id.loghipMAD)
jtools::summ(acc_act_act_id.loghipMAD)

### Wrist placement 
## ENMO
# Un-transformed data
acc_act.wristENMO <- glm(ENMO.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping"), data = data.pp)
acc_act_id.wristENMO <- lme4::lmer(ENMO.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.wristENMO, acc_act_id.wristENMO, test="Chisq")
rm(acc_act.wristENMO)
acc_act_act_id.wristENMO <- lme4::lmer(ENMO.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.wristENMO, acc_act_act_id.wristENMO, test="Chisq")
rm(acc_act_id.wristENMO)

#Plot normality of residuals
plot(acc_act_act_id.wristENMO)
qqnorm(residuals(acc_act_act_id.wristENMO))

# Transformed data
acc_act.logwristENMO <- glm(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "sleeping"), data = data.pp)
acc_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.logwristENMO, acc_act_id.logwristENMO, test="Chisq")
rm(acc_act.logwristENMO)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.logwristENMO, acc_act_act_id.logwristENMO, test="Chisq")
rm(acc_act_id.logwristENMO)

#Plot normality of residuals
plot(acc_act_act_id.logwristENMO)
qqnorm(residuals(acc_act_act_id.logwristENMO))

# Report fitted model based on transformed data
rm(acc_act_act_id.wristENMO)
#summary(acc_act_act_id.wristENMO)
car::Anova(acc_act_act_id.logwristENMO)
jtools::summ(acc_act_act_id.logwristENMO)
report::report_table(acc_act_act_id.logwristENMO)
# Use following lines to relevel the models for the different contrasts
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "sittinglying")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "eatingdrinking")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "passivescreen")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "activescreen")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "quietplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "dontknowplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "activeplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "activetransport")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
report::report_table(acc_act_act_id.logwristENMO)
jtools::summ(acc_act_act_id.logwristENMO)

## MAD
# Un-transformed data
acc_act.wristMAD <- glm(MAD.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping"), data = data.pp)
acc_act_id.wristMAD <- lme4::lmer(MAD.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.wristMAD, acc_act_id.wristMAD, test="Chisq")
rm(acc_act.wristMAD)
acc_act_act_id.wristMAD <- lme4::lmer(MAD.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.wristMAD, acc_act_act_id.wristMAD, test="Chisq")
rm(acc_act_id.wristMAD)

#Plot normality of residuals
plot(acc_act_act_id.wristMAD)
qqnorm(residuals(acc_act_act_id.wristMAD))

# Transformed data
acc_act.logwristMAD <- glm(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "sleeping"), data = data.pp)
acc_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.logwristMAD, acc_act_id.logwristMAD, test="Chisq")
rm(acc_act.logwristMAD)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "sleeping")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.logwristMAD, acc_act_act_id.logwristMAD, test="Chisq")
rm(acc_act_id.logwristMAD)

#Plot normality of residuals
plot(acc_act_act_id.logwristMAD)
qqnorm(residuals(acc_act_act_id.logwristMAD))

# Report fitted model based on transformed data
rm(acc_act_act_id.wristMAD)
#summary(acc_act_act_id.wristMAD)
car::Anova(acc_act_act_id.logwristMAD)
jtools::summ(acc_act_act_id.logwristMAD)
report::report_table(acc_act_act_id.logwristMAD)
# Use following lines to relevel the models for the different contrasts
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "sittinglying")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "eatingdrinking")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "passivescreen")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "activescreen")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "quietplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "dontknowplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "activeplay")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "activetransport")  + (1 | castorID/activity), data = data.pp, REML = FALSE)
report::report_table(acc_act_act_id.logwristMAD)
jtools::summ(acc_act_act_id.logwristMAD)