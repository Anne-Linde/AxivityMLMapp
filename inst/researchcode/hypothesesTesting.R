### This script evaluates the abilitiy of the MLM app to assess movement behaviors by hypotheses testing (as described in section 2.5.2 of article):
## 1) Plotting the acceleration distributions for the My Little Moves app categories
## 2) Fitting general linear mixed models with separate random intercepts for activity over participants
## 3) Fitting general linear mixed models with separate random intercepts for behavior over participants
## 4) Fitting general linear mixed models for 24-hour movement behaviors with adjusted labeling based on posture/category


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

# data.pp$ENMO.hip<- data.pp$ENMO.hip*1000
# data.pp$ENMO.wrist<- data.pp$ENMO.wrist*1000
# data.pp$MAD.hip<- data.pp$MAD.hip*1000
# data.pp$MAD.wrist<- data.pp$MAD.wrist*1000

# source functions directly from file, to be replaced by package installation:
#install.packages("ReliabilityValidityStudyAxivityMLMapp")
#library(ReliabilityValidityStudyAxivityMLMapp)
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/AxivityMLMapp/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

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
                  "Active play", "Play of unknown intensity")

# Nice to have: number of entries per category
# n_fun <- function(x){
#   return(data.frame(y = 0.95*70,
#                     label = length(x)))
# }

bxp_ENMO_activities <- ggplot2::ggplot(df_long.ENMO, ggplot2::aes(y = value, x = as.factor(activity), fill =metric), alpha = 0.35) + 
  ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.55) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = level_order, labels = level_labels) + ggplot2::xlab("") + 
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) +
  ggplot2::scale_fill_manual(values = c("#440154FF", "#FDE725FF"), name = "Accelerometer placement", labels = c("Hip", "Wrist"))+
  ggplot2::ylim(0,150) 

bxp_MAD_activities <- ggplot2::ggplot(df_long.MAD, ggplot2::aes(y = value, x = as.factor(activity), fill =metric)) + 
  ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.55) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = level_order, labels = level_labels) + ggplot2::xlab("App categories") + 
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) +
  ggplot2::scale_fill_manual(values = c("#440154FF", "#FDE725FF"), name = "Accelerometer placement", labels = c("Hip", "Wrist"))+
  ggplot2::ylim(0,150) 

gridExtra::grid.arrange(bxp_ENMO_activities, bxp_MAD_activities, nrow=2) #arranges plots within grid

boxplots_activities <- gridExtra::arrangeGrob(bxp_ENMO_activities + ggplot2::theme(legend.position="top"),
                                              bxp_MAD_activities + ggplot2::theme(legend.position="none"),
                                              nrow=2, 
                                              heights = c(4.5, 3)) # generates plot
plot(boxplots_activities) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/categories/boxplot_categories.png"), boxplots_activities, width = 10, height = 8, dpi = 600) #saves g

### 24-hour movement distributions
bxp_ENMO_behaviors <- ggplot2::ggplot(df_long.ENMO, ggplot2::aes(y = value, x = as.factor(behavior), fill =metric), alpha = 0.35) + 
  ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.55) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = c("PA", "SB", "sleep")) + ggplot2::xlab("") + 
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) +
  ggplot2::scale_fill_manual(values = c("#440154FF", "#FDE725FF"), name = "Accelerometer placement", labels = c("Hip", "Wrist"))+
  ggplot2::ylim(0,150) 

bxp_MAD_behaviors <- ggplot2::ggplot(df_long.MAD, ggplot2::aes(y = value, x = as.factor(behavior), fill =metric)) + 
  ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.55) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = c("PA", "SB", "sleep")) + ggplot2::xlab("24-h movement behaviors") + 
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) +
  ggplot2::scale_fill_manual(values = c("#440154FF", "#FDE725FF"), name = "Accelerometer placement", labels = c("Hip", "Wrist"))+
  ggplot2::ylim(0,150) 

gridExtra::grid.arrange(bxp_ENMO_behaviors, bxp_MAD_behaviors, nrow=2) #arranges plots within grid

boxplots_behaviors <- gridExtra::arrangeGrob(bxp_ENMO_behaviors + ggplot2::theme(legend.position="top"),
                                             bxp_MAD_behaviors + ggplot2::theme(legend.position="none"),
                                             nrow=2, 
                                             heights = c(4.5, 3)) # generates plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/boxplot_behaviors.png"), boxplots_behaviors, width = 10, height = 8, dpi = 600) #saves g

### Combine plots
bxp_MAD_act <- ggplot2::ggplot(df_long.MAD, ggplot2::aes(y = value, x = as.factor(activity), fill =metric)) + 
  ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.55) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = level_order, labels = level_labels) + ggplot2::xlab("") + 
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) +
  ggplot2::scale_fill_manual(values = c("#440154FF", "#FDE725FF"), name = "Accelerometer placement", labels = c("Hip", "Wrist"))+
  ggplot2::ylim(0,150) 

boxplots_act <- gridExtra::arrangeGrob(bxp_ENMO_activities + ggplot2::theme(legend.position="none"),
                                       bxp_MAD_act + ggplot2::theme(legend.position="none"),
                                       ncol=2) # generates plot

xlabel <- grid::textGrob("App activities", gp = grid::gpar(fontsize = 12))

boxplots <- gridExtra::grid.arrange(boxplots_behaviors, boxplots_act, xlabel, nrow=3, heights = c(4, 4, 0.5)) #arranges plots within grid

ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/boxplot_combined.png"), boxplots, width = 10, height = 8, dpi = 600) #saves g

#combine.save.plots(bxp_ENMO, bxp_MAD, format = "boxplots", savedir = paste0(savedir, "/plots/distributions/behaviors/"), filename = "boxplot_behaviors.png")

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
acc_act.hipENMO <- glm(ENMO.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months, data = data.pp)
acc_act_id.hipENMO <- lme4::lmer(ENMO.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.hipENMO, acc_act_id.hipENMO, test="Chisq")
rm(acc_act.hipENMO)
acc_act_act_id.hipENMO <- lme4::lmer(ENMO.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.hipENMO, acc_act_act_id.hipENMO, test="Chisq")
rm(acc_act_id.hipENMO)

#Plot normality of residuals
plot(acc_act_act_id.hipENMO)
qqnorm(residuals(acc_act_act_id.hipENMO))

# Transformed data
acc_act.loghipENMO <- glm(logENMO.hip ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months, data = data.pp)
acc_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.loghipENMO, acc_act_id.loghipENMO, test="Chisq")
rm(acc_act.loghipENMO)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months  + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.loghipENMO, acc_act_act_id.loghipENMO, test="Chisq")
rm(acc_act_id.loghipENMO)

#Plot normality of residuals
plot(acc_act_act_id.loghipENMO)
qqnorm(residuals(acc_act_act_id.loghipENMO))

# Report fitted model based on transformed data
rm(acc_act_act_id.hipENMO)
#summary(acc_act_act_id.loghipENMO)
car::Anova(acc_act_act_id.loghipENMO)
jtools::summ(acc_act_act_id.loghipENMO)
report::report_table(acc_act_act_id.loghipENMO)
# Use following lines to relevel the models for the different contrasts
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "sittinglying") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "personalcare") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "eatingdrinking") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "passivescreen") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "activescreen") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "passivetransport") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "activetransport") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "quietplay") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipENMO <- lme4::lmer(logENMO.hip ~ forcats::fct_relevel(activity, ref = "activeplay") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
car::Anova(acc_act_act_id.loghipENMO)
report::report_table(acc_act_act_id.loghipENMO)
jtools::summ(acc_act_act_id.loghipENMO)

## MAD
# Un-transformed data
acc_act.hipMAD <- glm(MAD.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping")+ gender + age_in_months, data = data.pp)
acc_act_id.hipMAD <- lme4::lmer(MAD.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.hipMAD, acc_act_id.hipMAD, test="Chisq")
rm(acc_act.hipMAD)
acc_act_act_id.hipMAD <- lme4::lmer(MAD.hip/1000 ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.hipMAD, acc_act_act_id.hipMAD, test="Chisq")
rm(acc_act_id.hipMAD)

#Plot normality of residuals
plot(acc_act_act_id.hipMAD)
qqnorm(residuals(acc_act_act_id.hipMAD))

# Transformed data
acc_act.loghipMAD <- glm(logMAD.hip ~ forcats::fct_relevel(activity, ref = "sleeping")+ gender + age_in_months, data = data.pp)
acc_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.loghipMAD, acc_act_id.loghipMAD, test="Chisq")
rm(acc_act.loghipMAD)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.loghipMAD, acc_act_act_id.loghipMAD, test="Chisq")
rm(acc_act_id.loghipMAD)

#Plot normality of residuals
plot(acc_act_act_id.loghipMAD)
qqnorm(residuals(acc_act_act_id.loghipMAD))

# Report fitted model based on transformed data
rm(acc_act_act_id.hipMAD)
#summary(acc_act_act_id.loghipMAD)
car::Anova(acc_act_act_id.loghipMAD)
jtools::summ(acc_act_act_id.loghipMAD)
report::report_table(acc_act_act_id.loghipMAD)
# Use following lines to relevel the models for the different contrasts
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "sittinglying") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "personalcare") + gender + age_in_months  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "eatingdrinking") + gender + age_in_months  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "passivescreen")+ gender + age_in_months  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "activescreen")+ gender + age_in_months  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "passivetransport") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "activetransport") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "quietplay") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.loghipMAD <- lme4::lmer(logMAD.hip ~ forcats::fct_relevel(activity, ref = "activeplay") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
report::report_table(acc_act_act_id.loghipMAD)
jtools::summ(acc_act_act_id.loghipMAD)

### Wrist placement
## ENMO
# Un-transformed data
acc_act.wristENMO <- glm(ENMO.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months, data = data.pp)
acc_act_id.wristENMO <- lme4::lmer(ENMO.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.wristENMO, acc_act_id.wristENMO, test="Chisq")
rm(acc_act.wristENMO)
acc_act_act_id.wristENMO <- lme4::lmer(ENMO.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping")+ gender + age_in_months  + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.wristENMO, acc_act_act_id.wristENMO, test="Chisq")
rm(acc_act_id.wristENMO)

#Plot normality of residuals
plot(acc_act_act_id.wristENMO)
qqnorm(residuals(acc_act_act_id.wristENMO))

# Transformed data
acc_act.logwristENMO <- glm(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "sleeping")+ gender + age_in_months, data = data.pp)
acc_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.logwristENMO, acc_act_id.logwristENMO, test="Chisq")
rm(acc_act.logwristENMO)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.logwristENMO, acc_act_act_id.logwristENMO, test="Chisq")
rm(acc_act_id.logwristENMO)

#Plot normality of residuals
plot(acc_act_act_id.logwristENMO)
qqnorm(residuals(acc_act_act_id.logwristENMO))

# Report fitted model based on transformed data
rm(acc_act_act_id.wristENMO)
#summary(acc_act_act_id.logwristENMO)
car::Anova(acc_act_act_id.logwristENMO)
jtools::summ(acc_act_act_id.logwristENMO)
report::report_table(acc_act_act_id.logwristENMO)
# Use following lines to relevel the models for the different contrasts
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "sittinglying")+ gender + age_in_months  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "personalcare") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "eatingdrinking") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "passivescreen") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "activescreen") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "passivetransport") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "activetransport") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "quietplay") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristENMO <- lme4::lmer(logENMO.wrist ~ forcats::fct_relevel(activity, ref = "activeplay") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
report::report_table(acc_act_act_id.logwristENMO)
jtools::summ(acc_act_act_id.logwristENMO)

## MAD
# Un-transformed data
acc_act.wristMAD <- glm(MAD.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping")+ gender + age_in_months, data = data.pp)
acc_act_id.wristMAD <- lme4::lmer(MAD.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.wristMAD, acc_act_id.wristMAD, test="Chisq")
rm(acc_act.wristMAD)
acc_act_act_id.wristMAD <- lme4::lmer(MAD.wrist/1000 ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.wristMAD, acc_act_act_id.wristMAD, test="Chisq")
rm(acc_act_id.wristMAD)

#Plot normality of residuals
plot(acc_act_act_id.wristMAD)
qqnorm(residuals(acc_act_act_id.wristMAD))

# Transformed data
acc_act.logwristMAD <- glm(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "sleeping")+ gender + age_in_months, data = data.pp)
acc_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_act.logwristMAD, acc_act_id.logwristMAD, test="Chisq")
rm(acc_act.logwristMAD)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "sleeping") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
anova(acc_act_id.logwristMAD, acc_act_act_id.logwristMAD, test="Chisq")
rm(acc_act_id.logwristMAD)

#Plot normality of residuals
plot(acc_act_act_id.logwristMAD)
qqnorm(residuals(acc_act_act_id.logwristMAD))

# Report fitted model based on transformed data
rm(acc_act_act_id.wristMAD)
#summary(acc_act_act_id.logwristMAD)
car::Anova(acc_act_act_id.logwristMAD)
jtools::summ(acc_act_act_id.logwristMAD)
report::report_table(acc_act_act_id.logwristMAD)
# Use following lines to relevel the models for the different contrasts
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "sittinglying") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "personalcare") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "eatingdrinking") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "passivescreen")+ gender + age_in_months  + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "activescreen") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "passivetransport") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "activetransport") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "quietplay") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
acc_act_act_id.logwristMAD <- lme4::lmer(logMAD.wrist ~ forcats::fct_relevel(activity, ref = "activeplay") + gender + age_in_months + (1 | castorID/activity), data = data.pp, REML = FALSE)
report::report_table(acc_act_act_id.logwristMAD)
jtools::summ(acc_act_act_id.logwristMAD)


#### STEP 3: Test for differences in acceleration between movement behaviors
### Hip placement
## ENMO
# Un-transformed data
acc_beh.hipENMO <- glm(ENMO.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp)
acc_beh_id.hipENMO <- lme4::lmer(ENMO.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_beh.hipENMO, acc_beh_id.hipENMO, test="Chisq")
rm(acc_beh.hipENMO)
acc_beh_id_beh.hipENMO <- lme4::lmer(ENMO.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp, REML = FALSE)
anova(acc_beh_id.hipENMO, acc_beh_id_beh.hipENMO, test="Chisq")
rm(acc_beh_id.hipENMO)

#Plot normality of residuals
plot(acc_beh_id_beh.hipENMO)
qqnorm(residuals(acc_beh_id_beh.hipENMO))

# Transformed data
acc_beh.loghipENMO <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp)
acc_beh_id.loghipENMO <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_beh.loghipENMO, acc_beh_id.loghipENMO, test="Chisq")
rm(acc_beh.loghipENMO)
acc_beh_id_beh.loghipENMO <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp, REML = TRUE)
anova(acc_beh_id.loghipENMO, acc_beh_id_beh.loghipENMO, test="Chisq")
rm(acc_beh_id.loghipENMO)

#Plot normality of residuals
plot(acc_beh_id_beh.loghipENMO)
qqnorm(residuals(acc_beh_id_beh.loghipENMO))

# Report fitted model based on transformed data
rm(acc_beh_id_beh.hipENMO)
#summary(acc_beh_id_beh.loghipENMO)
car::Anova(acc_beh_id_beh.loghipENMO)
jtools::summ(acc_beh_id_beh.loghipENMO)
logLik(acc_beh_id_beh.loghipENMO)
coef(summary(acc_beh_id_beh.loghipENMO))

# Use following lines to relevel the models for the different contrasts
acc_beh_id_beh.loghipENMO <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior, ref = "SB")  + (1 | castorID/behavior), data = data.pp, REML = TRUE)

## MAD
# Un-transformed data
acc_beh.hipMAD <- glm(MAD.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp)
acc_beh_id.hipMAD <- lme4::lmer(MAD.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_beh.hipMAD, acc_beh_id.hipMAD, test="Chisq")
rm(acc_beh.hipMAD)
acc_beh_id_beh.hipMAD <- lme4::lmer(MAD.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp, REML = FALSE)
anova(acc_beh_id.hipMAD, acc_beh_id_beh.hipMAD, test="Chisq")
rm(acc_beh_id.hipMAD)

#Plot normality of residuals
plot(acc_beh_id_beh.hipMAD)
qqnorm(residuals(acc_beh_id_beh.hipMAD))

# Transformed data
acc_beh.loghipMAD <- glm(logMAD.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp)
acc_beh_id.loghipMAD <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_beh.loghipMAD, acc_beh_id.loghipMAD, test="Chisq")
rm(acc_beh.loghipMAD)
acc_beh_id_beh.loghipMAD <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp, REML = TRUE)
anova(acc_beh_id.loghipMAD, acc_beh_id_beh.loghipMAD, test="Chisq")
rm(acc_beh_id.loghipMAD)

#Plot normality of residuals
plot(acc_beh_id_beh.loghipMAD)
qqnorm(residuals(acc_beh_id_beh.loghipMAD))

# Report fitted model based on transformed data
rm(acc_beh_id_beh.hipMAD)
#summary(acc_beh_id_beh.loghipMAD)
car::Anova(acc_beh_id_beh.loghipMAD)
jtools::summ(acc_beh_id_beh.loghipMAD)
logLik(acc_beh_id_beh.loghipMAD)
coef(summary(acc_beh_id_beh.loghipMAD))

# Use following lines to relevel the models for the different contrasts
acc_beh_id_beh.loghipMAD <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior, ref = "SB")  + (1 | castorID/behavior), data = data.pp, REML = TRUE)

### Wrist placement
## ENMO
# Un-transformed data
acc_beh.wristENMO <- glm(ENMO.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp)
acc_beh_id.wristENMO <- lme4::lmer(ENMO.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_beh.wristENMO, acc_beh_id.wristENMO, test="Chisq")
rm(acc_beh.wristENMO)
acc_beh_id_beh.wristENMO <- lme4::lmer(ENMO.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp, REML = FALSE)
anova(acc_beh_id.wristENMO, acc_beh_id_beh.wristENMO, test="Chisq")
rm(acc_beh_id.wristENMO)

#Plot normality of residuals
plot(acc_beh_id_beh.wristENMO)
qqnorm(residuals(acc_beh_id_beh.wristENMO))

# Transformed data
acc_beh.logwristENMO <- glm(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp)
acc_beh_id.logwristENMO <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_beh.logwristENMO, acc_beh_id.logwristENMO, test="Chisq")
rm(acc_beh.logwristENMO)
acc_beh_id_beh.logwristENMO <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp, REML = TRUE)
anova(acc_beh_id.logwristENMO, acc_beh_id_beh.logwristENMO, test="Chisq")
rm(acc_beh_id.logwristENMO)

#Plot normality of residuals
plot(acc_beh_id_beh.logwristENMO)
qqnorm(residuals(acc_beh_id_beh.logwristENMO))

# Report fitted model based on transformed data
rm(acc_beh_id_beh.wristENMO)
#summary(acc_beh_id_beh.logwristENMO)
car::Anova(acc_beh_id_beh.logwristENMO)
jtools::summ(acc_beh_id_beh.logwristENMO)
logLik(acc_beh_id_beh.logwristENMO)
coef(summary(acc_beh_id_beh.logwristENMO))

# Use following lines to relevel the models for the different contrasts
acc_beh_id_beh.logwristENMO <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "SB")  + (1 | castorID/behavior), data = data.pp, REML = TRUE)

## MAD
# Un-transformed data
acc_beh.wristMAD <- glm(MAD.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp)
acc_beh_id.wristMAD <- lme4::lmer(MAD.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_beh.wristMAD, acc_beh_id.wristMAD, test="Chisq")
rm(acc_beh.wristMAD)
acc_beh_id_beh.wristMAD <- lme4::lmer(MAD.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp, REML = FALSE)
anova(acc_beh_id.wristMAD, acc_beh_id_beh.wristMAD, test="Chisq")
rm(acc_beh_id.wristMAD)

#Plot normality of residuals
plot(acc_beh_id_beh.wristMAD)
qqnorm(residuals(acc_beh_id_beh.wristMAD))

# Transformed data
acc_beh.logwristMAD <- glm(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp)
acc_beh_id.logwristMAD <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(acc_beh.logwristMAD, acc_beh_id.logwristMAD, test="Chisq")
rm(acc_beh.logwristMAD)
acc_beh_id_beh.logwristMAD <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp, REML = TRUE)
anova(acc_beh_id.logwristMAD, acc_beh_id_beh.logwristMAD, test="Chisq")
rm(acc_beh_id.logwristMAD)

#Plot normality of residuals
plot(acc_beh_id_beh.logwristMAD)
qqnorm(residuals(acc_beh_id_beh.logwristMAD))

# Report fitted model based on transformed data
rm(acc_beh_id_beh.wristMAD)
#summary(acc_beh_id_beh.logwristMAD)
car::Anova(acc_beh_id_beh.logwristMAD)
jtools::summ(acc_beh_id_beh.logwristMAD)
logLik(acc_beh_id_beh.logwristMAD)
coef(summary(acc_beh_id_beh.logwristMAD))

# Use following lines to relevel the models for the different contrasts
acc_beh_id_beh.logwristMAD <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "SB")  + (1 | castorID/behavior), data = data.pp, REML = TRUE)

#### STEP 4: Test for differences in acceleration between movement behaviors when labeling of behavior changes
# Remove passive transport
data.pp.withoutpt <- data.pp[-which(data.pp$activity == "passivetransport"),]
# Using log transformed data
### Standard model without passive transport

loghipENMO.model1 <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp.withoutpt)
loghipENMO.model1.id <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp.withoutpt, REML = FALSE)
anova(loghipENMO.model1, loghipENMO.model1.id, test="Chisq")
rm(loghipENMO.model1)
loghipENMO.model1.id.beh <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp.withoutpt, REML = TRUE)
anova(loghipENMO.model1.id, loghipENMO.model1.id.beh, test="Chisq")
rm(loghipENMO.model1.id)

jtools::summ(loghipENMO.model1.id.beh)

## Hip MAD
loghipMAD.model1 <- glm(logMAD.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp.withoutpt)
loghipMAD.model1.id <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp.withoutpt, REML = FALSE)
anova(loghipMAD.model1, loghipMAD.model1.id, test="Chisq")
rm(loghipMAD.model1)
loghipMAD.model1.id.beh <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp.withoutpt, REML = TRUE)
anova(loghipMAD.model1.id, loghipMAD.model1.id.beh, test="Chisq")
rm(loghipMAD.model1.id)

jtools::summ(loghipMAD.model1.id.beh)

anova(acc_beh_id_beh.loghipENMO, loghipMAD.model1.id.beh, test="Chisq")


## Wrist ENMO
logwristENMO.model1 <- glm(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp.withoutpt)
logwristENMO.model1.id <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp.withoutpt, REML = FALSE)
anova(logwristENMO.model1, logwristENMO.model1.id, test="Chisq")
rm(logwristENMO.model1)
logwristENMO.model1.id.beh <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp.withoutpt, REML = TRUE)
anova(logwristENMO.model1.id, logwristENMO.model1.id.beh, test="Chisq")
rm(logwristENMO.model1.id)

jtools::summ(logwristENMO.model1.id.beh)

## Wrist MAD
logwristMAD.model1 <- glm(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep"), data = data.pp.withoutpt)
logwristMAD.model1.id <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID), data = data.pp.withoutpt, REML = FALSE)
anova(logwristMAD.model1, logwristMAD.model1.id, test="Chisq")
rm(logwristMAD.model1)
logwristMAD.model1.id.beh <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior, ref = "sleep")  + (1 | castorID/behavior), data = data.pp.withoutpt, REML = TRUE)
anova(logwristMAD.model1.id, logwristMAD.model1.id.beh, test="Chisq")
rm(logwristMAD.model1.id)

jtools::summ(logwristMAD.model1.id.beh)

### 2. Sitting without support: PA (model 1) -> SB
# index_sitting <- which(data.pp$posture == "sitting_withoutsupport")
# length(index_sitting)
# data.pp$behavior2 <- data.pp$behavior
# data.pp$behavior2[index_sitting] <- "SB"
index_sitting <- which(data.pp.withoutpt$posture == "sitting_withoutsupport")
length(index_sitting)
data.pp.withoutpt$behavior2 <- data.pp.withoutpt$behavior
data.pp.withoutpt$behavior2[index_sitting] <- "SB"

## Hip ENMO
# loghipENMO.model2 <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep"), data = data.pp)
# loghipENMO.model2.id <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
# anova(loghipENMO.model2, loghipENMO.model2.id, test="Chisq")
# rm(loghipENMO.model2)
# loghipENMO.model2.id.beh <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep")  + (1 | castorID/behavior2), data = data.pp, REML = TRUE)
# anova(loghipENMO.model2.id, loghipENMO.model2.id.beh, test="Chisq")
# rm(loghipENMO.model2.id)
# 
# jtools::summ(loghipENMO.model2.id.beh)

loghipENMO.model2 <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep"), data = data.pp.withoutpt)
loghipENMO.model2.id <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep")  + (1 | castorID), data = data.pp.withoutpt, REML = FALSE)
anova(loghipENMO.model2, loghipENMO.model2.id, test="Chisq")
rm(loghipENMO.model2)
loghipENMO.model2.id.beh <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep")  + (1 | castorID/behavior2), data = data.pp.withoutpt, REML = TRUE)
anova(loghipENMO.model2.id, loghipENMO.model2.id.beh, test="Chisq")
rm(loghipENMO.model2.id)

jtools::summ(loghipENMO.model2.id.beh)

## Hip MAD
loghipMAD.model2 <- glm(logMAD.hip/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep"), data = data.pp)
loghipMAD.model2.id <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipMAD.model2, loghipMAD.model2.id, test="Chisq")
rm(loghipMAD.model2)
loghipMAD.model2.id.beh <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep")  + (1 | castorID/behavior2), data = data.pp, REML = TRUE)
anova(loghipMAD.model2.id, loghipMAD.model2.id.beh, test="Chisq")
rm(loghipMAD.model2.id)

jtools::summ(loghipMAD.model2.id.beh)

## Wrist ENMO
logwristENMO.model2 <- glm(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep"), data = data.pp)
logwristENMO.model2.id <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristENMO.model2, logwristENMO.model2.id, test="Chisq")
rm(logwristENMO.model2)
logwristENMO.model2.id.beh <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep")  + (1 | castorID/behavior2), data = data.pp, REML = TRUE)
anova(logwristENMO.model2.id, logwristENMO.model2.id.beh, test="Chisq")
rm(logwristENMO.model2.id)

jtools::summ(logwristENMO.model2.id.beh)

## Wrist MAD
logwristMAD.model2 <- glm(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep"), data = data.pp)
logwristMAD.model2.id <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristMAD.model2, logwristMAD.model2.id, test="Chisq")
rm(logwristMAD.model2)
logwristMAD.model2.id.beh <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior2, ref = "sleep")  + (1 | castorID/behavior2), data = data.pp, REML = TRUE)
anova(logwristMAD.model2.id, logwristMAD.model2.id.beh, test="Chisq")
rm(logwristMAD.model2.id)

jtools::summ(logwristMAD.model2.id.beh)

### 3. Standing without support: PA (model 1) -> SB
index_standing_withoutsupport <- which(data.pp$posture == "standing_withoutsupport")
length(index_standing_withoutsupport)
data.pp$behavior3 <- data.pp$behavior
data.pp$behavior3[index_standing_withoutsupport] <- "SB"

## Hip ENMO
loghipENMO.model3 <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep"), data = data.pp)
loghipENMO.model3.id <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipENMO.model3, loghipENMO.model3.id, test="Chisq")
rm(loghipENMO.model3)
loghipENMO.model3.id.beh <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep")  + (1 | castorID/behavior3), data = data.pp, REML = TRUE)
anova(loghipENMO.model3.id, loghipENMO.model3.id.beh, test="Chisq")
rm(loghipENMO.model3.id)

jtools::summ(loghipENMO.model3.id.beh)

## Hip MAD
loghipMAD.model3 <- glm(logMAD.hip/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep"), data = data.pp)
loghipMAD.model3.id <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipMAD.model3, loghipMAD.model3.id, test="Chisq")
rm(loghipMAD.model3)
loghipMAD.model3.id.beh <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep")  + (1 | castorID/behavior3), data = data.pp, REML = TRUE)
anova(loghipMAD.model3.id, loghipMAD.model3.id.beh, test="Chisq")
rm(loghipMAD.model3.id)

jtools::summ(loghipMAD.model3.id.beh)

## Wrist ENMO
logwristENMO.model3 <- glm(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep"), data = data.pp)
logwristENMO.model3.id <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristENMO.model3, logwristENMO.model3.id, test="Chisq")
rm(logwristENMO.model3)
logwristENMO.model3.id.beh <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep")  + (1 | castorID/behavior3), data = data.pp, REML = TRUE)
anova(logwristENMO.model3.id, logwristENMO.model3.id.beh, test="Chisq")
rm(logwristENMO.model3.id)

jtools::summ(logwristENMO.model3.id.beh)

## Wrist MAD
logwristMAD.model3 <- glm(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep"), data = data.pp)
logwristMAD.model3.id <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristMAD.model3, logwristMAD.model3.id, test="Chisq")
rm(logwristMAD.model3)
logwristMAD.model3.id.beh <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior3, ref = "sleep")  + (1 | castorID/behavior3), data = data.pp, REML = TRUE)
anova(logwristMAD.model3.id, logwristMAD.model3.id.beh, test="Chisq")
rm(logwristMAD.model3.id)

jtools::summ(logwristMAD.model3.id.beh)

### 4. Playing active - carried does not exist in the data

### 5. Playing active - lying: PA (model 1) -> SB
index_activeplay <- which(data.pp$activity == "activeplay")
index_lying <- which(data.pp$posture == "lying")
index_activeplay_lying <- intersect(index_activeplay, index_lying)
length(index_activeplay_lying)
data.pp$behavior5 <- data.pp$behavior
data.pp$behavior5[index_activeplay_lying] <- "SB"

## Hip ENMO
loghipENMO.model5 <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep"), data = data.pp)
loghipENMO.model5.id <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipENMO.model5, loghipENMO.model5.id, test="Chisq")
rm(loghipENMO.model5)
loghipENMO.model5.id.beh <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep")  + (1 | castorID/behavior5), data = data.pp, REML = TRUE)
anova(loghipENMO.model5.id, loghipENMO.model5.id.beh, test="Chisq")
rm(loghipENMO.model5.id)

jtools::summ(loghipENMO.model5.id.beh)

## Hip MAD
loghipMAD.model5 <- glm(logMAD.hip/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep"), data = data.pp)
loghipMAD.model5.id <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipMAD.model5, loghipMAD.model5.id, test="Chisq")
rm(loghipMAD.model5)
loghipMAD.model5.id.beh <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep")  + (1 | castorID/behavior5), data = data.pp, REML = TRUE)
anova(loghipMAD.model5.id, loghipMAD.model5.id.beh, test="Chisq")
rm(loghipMAD.model5.id)

jtools::summ(loghipMAD.model5.id.beh)

## Wrist ENMO
logwristENMO.model5 <- glm(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep"), data = data.pp)
logwristENMO.model5.id <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristENMO.model5, logwristENMO.model5.id, test="Chisq")
rm(logwristENMO.model5)
logwristENMO.model5.id.beh <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep")  + (1 | castorID/behavior5), data = data.pp, REML = TRUE)
anova(logwristENMO.model5.id, logwristENMO.model5.id.beh, test="Chisq")
rm(logwristENMO.model5.id)

jtools::summ(logwristENMO.model5.id.beh)

## Wrist MAD
logwristMAD.model5 <- glm(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep"), data = data.pp)
logwristMAD.model5.id <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristMAD.model5, logwristMAD.model5.id, test="Chisq")
rm(logwristMAD.model5)
logwristMAD.model5.id.beh <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior5, ref = "sleep")  + (1 | castorID/behavior5), data = data.pp, REML = TRUE)
anova(logwristMAD.model5.id, logwristMAD.model5.id.beh, test="Chisq")
rm(logwristMAD.model5.id)

jtools::summ(logwristMAD.model5.id.beh)

### 6. Playing active - sitting: PA (model 1) -> SB
index_sitting <- which(data.pp$posture == "sitting")
index_activeplay_sitting <- intersect(index_activeplay, index_sitting)
length(index_activeplay_sitting)
data.pp$behavior6 <- data.pp$behavior
data.pp$behavior6[index_activeplay_sitting] <- "SB"

## Hip ENMO
loghipENMO.model6 <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep"), data = data.pp)
loghipENMO.model6.id <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipENMO.model6, loghipENMO.model6.id, test="Chisq")
rm(loghipENMO.model6)
loghipENMO.model6.id.beh <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep")  + (1 | castorID/behavior6), data = data.pp, REML = TRUE)
anova(loghipENMO.model6.id, loghipENMO.model6.id.beh, test="Chisq")
rm(loghipENMO.model6.id)

jtools::summ(loghipENMO.model6.id.beh)

## Hip MAD
loghipMAD.model6 <- glm(logMAD.hip/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep"), data = data.pp)
loghipMAD.model6.id <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipMAD.model6, loghipMAD.model6.id, test="Chisq")
rm(loghipMAD.model6)
loghipMAD.model6.id.beh <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep")  + (1 | castorID/behavior6), data = data.pp, REML = TRUE)
anova(loghipMAD.model6.id, loghipMAD.model6.id.beh, test="Chisq")
rm(loghipMAD.model6.id)

jtools::summ(loghipMAD.model6.id.beh)

## Wrist ENMO
logwristENMO.model6 <- glm(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep"), data = data.pp)
logwristENMO.model6.id <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristENMO.model6, logwristENMO.model6.id, test="Chisq")
rm(logwristENMO.model6)
logwristENMO.model6.id.beh <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep")  + (1 | castorID/behavior6), data = data.pp, REML = TRUE)
anova(logwristENMO.model6.id, logwristENMO.model6.id.beh, test="Chisq")
rm(logwristENMO.model6.id)

jtools::summ(logwristENMO.model6.id.beh)

## Wrist MAD
logwristMAD.model6 <- glm(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep"), data = data.pp)
logwristMAD.model6.id <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristMAD.model6, logwristMAD.model6.id, test="Chisq")
rm(logwristMAD.model6)
logwristMAD.model6.id.beh <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior6, ref = "sleep")  + (1 | castorID/behavior6), data = data.pp, REML = TRUE)
anova(logwristMAD.model6.id, logwristMAD.model6.id.beh, test="Chisq")
rm(logwristMAD.model6.id)

jtools::summ(logwristMAD.model6.id.beh)

### 7. Playing calm/don't know - standing: SB (model 1) -> PA
index_calmplay <- which(data.pp$activity == "quietplay")
index_dontknowplay <- which(data.pp$activity == "dontknowplay")
index <- union(index_calmplay, index_dontknowplay)
index_calmdontknowplay_standing <- intersect(index, index_sitting)
length(index_calmdontknowplay_standing)
data.pp$behavior7 <- data.pp$behavior
data.pp$behavior7[index_calmdontknowplay_standing] <- "PA"

## Hip ENMO
loghipENMO.model7 <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep"), data = data.pp)
loghipENMO.model7.id <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipENMO.model7, loghipENMO.model7.id, test="Chisq")
rm(loghipENMO.model7)
loghipENMO.model7.id.beh <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep")  + (1 | castorID/behavior7), data = data.pp, REML = TRUE)
anova(loghipENMO.model7.id, loghipENMO.model7.id.beh, test="Chisq")
rm(loghipENMO.model7.id)

jtools::summ(loghipENMO.model7.id.beh)

## Hip MAD
loghipMAD.model7 <- glm(logMAD.hip/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep"), data = data.pp)
loghipMAD.model7.id <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipMAD.model7, loghipMAD.model7.id, test="Chisq")
rm(loghipMAD.model7)
loghipMAD.model7.id.beh <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep")  + (1 | castorID/behavior7), data = data.pp, REML = TRUE)
anova(loghipMAD.model7.id, loghipMAD.model7.id.beh, test="Chisq")
rm(loghipMAD.model7.id)

jtools::summ(loghipMAD.model7.id.beh)

## Wrist ENMO
logwristENMO.model7 <- glm(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep"), data = data.pp)
logwristENMO.model7.id <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristENMO.model7, logwristENMO.model7.id, test="Chisq")
rm(logwristENMO.model7)
logwristENMO.model7.id.beh <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep")  + (1 | castorID/behavior7), data = data.pp, REML = TRUE)
anova(logwristENMO.model7.id, logwristENMO.model7.id.beh, test="Chisq")
rm(logwristENMO.model7.id)

jtools::summ(logwristENMO.model7.id.beh)

## Wrist MAD
logwristMAD.model7 <- glm(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep"), data = data.pp)
logwristMAD.model7.id <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristMAD.model7, logwristMAD.model7.id, test="Chisq")
rm(logwristMAD.model7)
logwristMAD.model7.id.beh <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior7, ref = "sleep")  + (1 | castorID/behavior7), data = data.pp, REML = TRUE)
anova(logwristMAD.model7.id, logwristMAD.model7.id.beh, test="Chisq")
rm(logwristMAD.model7.id)

jtools::summ(logwristMAD.model7.id.beh)

### 8. Playing calm/don't know - changing: SB (model 1) -> PA
index_changing <- which(data.pp$posture == "alternating")
index_calmdontknowplay_changing <- intersect(index, index_changing)
length(index_calmdontknowplay_changing)
data.pp$behavior8 <- data.pp$behavior
data.pp$behavior8[index_calmdontknowplay_changing] <- "PA"

## Hip ENMO
loghipENMO.model8 <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep"), data = data.pp)
loghipENMO.model8.id <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipENMO.model8, loghipENMO.model8.id, test="Chisq")
rm(loghipENMO.model8)
loghipENMO.model8.id.beh <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep")  + (1 | castorID/behavior8), data = data.pp, REML = TRUE)
anova(loghipENMO.model8.id, loghipENMO.model8.id.beh, test="Chisq")
rm(loghipENMO.model8.id)

jtools::summ(loghipENMO.model8.id.beh)

## Hip MAD
loghipMAD.model8 <- glm(logMAD.hip/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep"), data = data.pp)
loghipMAD.model8.id <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipMAD.model8, loghipMAD.model8.id, test="Chisq")
rm(loghipMAD.model8)
loghipMAD.model8.id.beh <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep")  + (1 | castorID/behavior8), data = data.pp, REML = TRUE)
anova(loghipMAD.model8.id, loghipMAD.model8.id.beh, test="Chisq")
rm(loghipMAD.model8.id)

jtools::summ(loghipMAD.model8.id.beh)

## Wrist ENMO
logwristENMO.model8 <- glm(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep"), data = data.pp)
logwristENMO.model8.id <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristENMO.model8, logwristENMO.model8.id, test="Chisq")
rm(logwristENMO.model8)
logwristENMO.model8.id.beh <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep")  + (1 | castorID/behavior8), data = data.pp, REML = TRUE)
anova(logwristENMO.model8.id, logwristENMO.model8.id.beh, test="Chisq")
rm(logwristENMO.model8.id)

jtools::summ(logwristENMO.model8.id.beh)

## Wrist MAD
logwristMAD.model8 <- glm(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep"), data = data.pp)
logwristMAD.model8.id <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristMAD.model8, logwristMAD.model8.id, test="Chisq")
rm(logwristMAD.model8)
logwristMAD.model8.id.beh <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior8, ref = "sleep")  + (1 | castorID/behavior8), data = data.pp, REML = TRUE)
anova(logwristMAD.model8.id, logwristMAD.model8.id.beh, test="Chisq")
rm(logwristMAD.model8.id)

jtools::summ(logwristMAD.model8.id.beh)

### 9. Playing calm: SB (model 1) -> PA

length(index_calmplay)
data.pp.withoutpt$behavior9 <- data.pp.withoutpt$behavior
data.pp.withoutpt$behavior9[index_calmplay] <- "PA"
# length(index_calmplay)
# data.pp$behavior9 <- data.pp$behavior
# data.pp$behavior9[index_calmplay] <- "PA"

## Hip ENMO
# loghipENMO.model9 <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep"), data = data.pp)
# loghipENMO.model9.id <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
# anova(loghipENMO.model9, loghipENMO.model9.id, test="Chisq")
# rm(loghipENMO.model9)
# loghipENMO.model9.id.beh <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep")  + (1 | castorID/behavior9), data = data.pp, REML = TRUE)
# anova(loghipENMO.model9.id, loghipENMO.model9.id.beh, test="Chisq")
# rm(loghipENMO.model9.id)
# 
# jtools::summ(loghipENMO.model9.id.beh)

loghipENMO.model9 <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep"), data = data.pp.withoutpt)
loghipENMO.model9.id <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep")  + (1 | castorID), data = data.pp.withoutpt, REML = FALSE)
anova(loghipENMO.model9, loghipENMO.model9.id, test="Chisq")
rm(loghipENMO.model9)
loghipENMO.model9.id.beh <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep")  + (1 | castorID/behavior9), data = data.pp.withoutpt, REML = TRUE)
anova(loghipENMO.model9.id, loghipENMO.model9.id.beh, test="Chisq")
rm(loghipENMO.model9.id)

jtools::summ(loghipENMO.model9.id.beh)

## Hip MAD
loghipMAD.model9 <- glm(logMAD.hip/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep"), data = data.pp)
loghipMAD.model9.id <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipMAD.model9, loghipMAD.model9.id, test="Chisq")
rm(loghipMAD.model9)
loghipMAD.model9.id.beh <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep")  + (1 | castorID/behavior9), data = data.pp, REML = TRUE)
anova(loghipMAD.model9.id, loghipMAD.model9.id.beh, test="Chisq")
rm(loghipMAD.model9.id)

jtools::summ(loghipMAD.model9.id.beh)

## Wrist ENMO
logwristENMO.model9 <- glm(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep"), data = data.pp)
logwristENMO.model9.id <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristENMO.model9, logwristENMO.model9.id, test="Chisq")
rm(logwristENMO.model9)
logwristENMO.model9.id.beh <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep")  + (1 | castorID/behavior9), data = data.pp, REML = TRUE)
anova(logwristENMO.model9.id, logwristENMO.model9.id.beh, test="Chisq")
rm(logwristENMO.model9.id)

jtools::summ(logwristENMO.model9.id.beh)

## Wrist MAD
logwristMAD.model9 <- glm(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep"), data = data.pp)
logwristMAD.model9.id <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristMAD.model9, logwristMAD.model9.id, test="Chisq")
rm(logwristMAD.model9)
logwristMAD.model9.id.beh <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior9, ref = "sleep")  + (1 | castorID/behavior9), data = data.pp, REML = TRUE)
anova(logwristMAD.model9.id, logwristMAD.model9.id.beh, test="Chisq")
rm(logwristMAD.model9.id)

jtools::summ(logwristMAD.model9.id.beh)


### 10. Playing dont know: SB (model 1) -> PA
length(index_dontknowplay)
data.pp$behavior10 <- data.pp$behavior
data.pp$behavior10[index_dontknowplay] <- "PA"

## Hip ENMO
loghipENMO.model10 <- glm(logENMO.hip/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep"), data = data.pp)
loghipENMO.model10.id <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipENMO.model10, loghipENMO.model10.id, test="Chisq")
rm(loghipENMO.model10)
loghipENMO.model10.id.beh <- lme4::lmer(logENMO.hip/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep")  + (1 | castorID/behavior10), data = data.pp, REML = TRUE)
anova(loghipENMO.model10.id, loghipENMO.model10.id.beh, test="Chisq")
rm(loghipENMO.model10.id)

jtools::summ(loghipENMO.model10.id.beh)

## Hip MAD
loghipMAD.model10 <- glm(logMAD.hip/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep"), data = data.pp)
loghipMAD.model10.id <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(loghipMAD.model10, loghipMAD.model10.id, test="Chisq")
rm(loghipMAD.model10)
loghipMAD.model10.id.beh <- lme4::lmer(logMAD.hip/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep")  + (1 | castorID/behavior10), data = data.pp, REML = TRUE)
anova(loghipMAD.model10.id, loghipMAD.model10.id.beh, test="Chisq")
rm(loghipMAD.model10.id)

jtools::summ(loghipMAD.model10.id.beh)

## Wrist ENMO
logwristENMO.model10 <- glm(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep"), data = data.pp)
logwristENMO.model10.id <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristENMO.model10, logwristENMO.model10.id, test="Chisq")
rm(logwristENMO.model10)
logwristENMO.model10.id.beh <- lme4::lmer(logENMO.wrist/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep")  + (1 | castorID/behavior10), data = data.pp, REML = TRUE)
anova(logwristENMO.model10.id, logwristENMO.model10.id.beh, test="Chisq")
rm(logwristENMO.model10.id)

jtools::summ(logwristENMO.model10.id.beh)

## Wrist MAD
logwristMAD.model10 <- glm(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep"), data = data.pp)
logwristMAD.model10.id <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep")  + (1 | castorID), data = data.pp, REML = FALSE)
anova(logwristMAD.model10, logwristMAD.model10.id, test="Chisq")
rm(logwristMAD.model10)
logwristMAD.model10.id.beh <- lme4::lmer(logMAD.wrist/1000 ~ forcats::fct_relevel(behavior10, ref = "sleep")  + (1 | castorID/behavior10), data = data.pp, REML = TRUE)
anova(logwristMAD.model10.id, logwristMAD.model10.id.beh, test="Chisq")
rm(logwristMAD.model10.id)

jtools::summ(logwristMAD.model10.id.beh)
