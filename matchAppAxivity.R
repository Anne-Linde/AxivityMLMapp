## This script was used to synchronize the axivity data to the app entries (as described in section 2.8.3.1 of the article)

rm(list = ls())
gc()

## User input 
date <- "20231031" # Date of last data update
tz = "Europe/Amsterdam"
# Use preprocessed Axivity data (data without non-wear)
filepath.hip <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/5sec/epochdata/hip/nonwear_removed"
filepath.wrist <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Accelerometer data/Measurement1/5sec/epochdata/wrist/nonwear_removed"
# Use app data activities
filepath.app <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/App-data/20230918"
filename.app <- paste0("/", date, "_activities_castor_linked_duration.csv")
# Path to directory to save the data list
savedir <- "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output"

# source functions directly from file, to be replaced by package installation:
my_functions_folder =   "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/AxivityMLMapp/R"
for (function_file in dir(my_functions_folder, full.names = T)) source(function_file) #load functions

## Load data
data.app <- load.app(filepath.app, filename.app, cohort = c(3), measurementperiod = 1, sep = ",") # Load app data

## Match axivity data to app entries
#run L22 once: then load in data from saved file
#data.app.axivity <- match.app.axivity(data.app, filepath.hip, filepath.wrist, tz, date)
load("/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/My Little Moves (MLM)/Comparison MLM-app and accelerometer data/Analyses/output/app_ax_entry_20231031.RData")

# Combine data for both placements
#data.app.axivity$ENMO.both = data.app.axivity$ENMO.hip * data.app.axivity$ENMO.wrist 
#data.app.axivity$MAD.both = data.app.axivity$MAD.hip * data.app.axivity$MAD.wrist 

# Reshape data to long format
df_long <- tidyr::gather(data.app.axivity, metric, value, ENMO.hip:MAD.wrist)
#df_long <- tidyr::gather(data.app.axivity, metric, value, ENMO.hip:MAD.both)
df_long.ENMO <- df_long[which(startsWith(df_long$metric, "ENMO")),]
df_long.MAD <- df_long[which(startsWith(df_long$metric, "MAD")),]

### Category distributions
## BOXPLOT
level_order <- c("actiefverplaatsen", "passiefverplaatsen", "actiefspelen", "rustigspelen", "weetnietspelen", 
                 "actiefbeeldscherm", "passiefbeeldscherm", "zittenliggen", "etendrinken", 
                 "verzorging", "slapen", "iemandanders", "other", "weetniet")
bxp_ENMO <- ggplot2::ggplot(df_long.ENMO, ggplot2::aes(y = value, x = as.factor(activity), fill =metric)) + 
  ggplot2::geom_boxplot(outlier.shape = 19, outlier.alpha = 0.1) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = level_order) + ggplot2::xlab("") + 
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) +
  ggplot2::scale_fill_discrete(name = "Accelerometer placement", labels = c("Hip", "Wrist"))
  
bxp_MAD <- ggplot2::ggplot(df_long.MAD, ggplot2::aes(y = value, x = as.factor(activity), fill =metric)) + 
  ggplot2::geom_boxplot(outlier.shape = 19, outlier.alpha = 0.1) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = level_order) + ggplot2::xlab("App categories") + 
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) +
  ggplot2::scale_fill_discrete(name = "Accelerometer placement", labels = c("Hip", "Wrist"))

gridExtra::grid.arrange(bxp_ENMO, bxp_MAD, nrow=2) #arranges plots within grid

boxplots <- gridExtra::arrangeGrob(bxp_ENMO + ggplot2::theme(legend.position="top"),
                                   bxp_MAD + ggplot2::theme(legend.position="none"),
                                      nrow=2) # generates plot
plot(boxplots) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/categories/boxplot_categories.png"), boxplots, width = 10, height = 8, dpi = 600) #saves g

## SCATTERPLOTS
df2 <- dplyr::select(data.app.axivity, -activity)

scatter.ENMOhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = ENMO.hip, color = activity)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = activity)) + 
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ggtitle("Hip") + ggplot2::ylim(c(0, 500)) 
  
scatter.ENMOwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = ENMO.wrist, color = activity)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = activity)) + 
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ylim(c(0, 500)) + ggplot2::ggtitle("Wrist")

gridExtra::grid.arrange(scatter.ENMOhip, scatter.ENMOwrist, ncol=2) #arranges plots within grid
scatter_ENMO <- gridExtra::arrangeGrob(scatter.ENMOhip + ggplot2::theme(axis.title.x = ggplot2::element_blank()),
                                   scatter.ENMOwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                   ncol=2, 
                                   bottom = "Duration (min)") # generates plot
plot(scatter_ENMO) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/categories/scatter_categories_ENMO.png"), scatter_ENMO, width = 10, height = 8, dpi = 600) #saves g

scatter.MADhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = MAD.hip, color = activity)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = activity)) + 
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ggtitle("Hip") + ggplot2::ylim(c(0, 500)) 

scatter.MADwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = MAD.wrist, color = activity)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = activity)) + 
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ylim(c(0, 500)) + ggplot2::ggtitle("Wrist")

gridExtra::grid.arrange(scatter.MADhip, scatter.MADwrist, ncol=2) #arranges plots within grid
scatter_MAD <- gridExtra::arrangeGrob(scatter.MADhip + ggplot2::theme(axis.title.x = ggplot2::element_blank()),
                                       scatter.MADwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       ncol=2, 
                                       bottom = "Duration (min)") # generates plot
plot(scatter_MAD) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/categories/scatter_categories_MAD.png"), scatter_MAD, width = 10, height = 8, dpi = 600) #saves g

## DENSITY PLOTS
# Per activity
density.ENMOwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.wrist, color = activity)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.ENMOhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.hip, color = activity)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") 

gridExtra::grid.arrange(density.ENMOwrist, density.ENMOhip, nrow=2) #arranges plots within grid
density_ENMO <- gridExtra::arrangeGrob(density.ENMOwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       density.ENMOhip + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                       nrow=2, left = "Density") # generates plot
plot(density_ENMO) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/categories/density_categories_ENMO.png"), density_ENMO, width = 10, height = 15, dpi = 600) #saves g

density.MADwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = MAD.wrist, color = activity)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.MADhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = MAD.hip, color = activity)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") 

gridExtra::grid.arrange(density.MADwrist, density.MADhip, nrow=2) #arranges plots within grid
density_MAD <- gridExtra::arrangeGrob(density.MADwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       density.MADhip + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                       nrow=2, left = "Density") # generates plot
plot(density_MAD) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/categories/density_categories_MAD.png"), density_MAD, width = 10, height = 15, dpi = 600) #saves g

# Postures for activities: active play, quiet play, i don't know play, and sitting/lying
data.app.axivity.posture <- data.app.axivity[data.app.axivity$activity %in% c("actiefspelen", "rustigspelen", "weetnietspelen", "zittenliggen"),]
density.ENMOwrist.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = ENMO.wrist, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") + ggplot2::ylim(c(0, 0.4)) 

density.ENMOhip.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = ENMO.hip, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="bottom") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") + ggplot2::xlim(c(0, 500)) 

gridExtra::grid.arrange(density.ENMOwrist.posture, density.ENMOhip.posture, nrow=2) #arranges plots within grid
density_ENMO.posture <- gridExtra::arrangeGrob(density.ENMOwrist.posture + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       density.ENMOhip.posture + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                       nrow=2, left = "Density") # generates plot
plot(density_ENMO.posture) #print the plot
ggplot2:::ggsave(file=paste0(savedir, "/plots/distributions/categories/density_categories_posture_ENMO.png"), density_ENMO.posture, width = 210, height = 297, units = "mm")

density.MADwrist.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = MAD.wrist, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist")  + ggplot2::ylim(c(0, 0.4)) 

density.MADhip.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = MAD.hip, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(activity, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="bottom") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") + ggplot2::xlim(c(0, 500)) 

gridExtra::grid.arrange(density.MADwrist.posture, density.MADhip.posture, nrow=2) #arranges plots within grid
density_MAD.posture <- gridExtra::arrangeGrob(density.MADwrist.posture + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                      density.MADhip.posture + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                      nrow=2, left = "Density") # generates plot
plot(density_MAD.posture) #print the plot
ggplot2:::ggsave(file=paste0(savedir, "/plots/distributions/categories/density_categories_posture_MAD.png"), density_MAD.posture, width = 210, height = 297, units = "mm")

### Behavior distributions
## BOXPLOT
level_order <- c("sleep", "SB", "PA")
bxp_ENMO <- ggplot2::ggplot(df_long.ENMO, ggplot2::aes(y = value, x = as.factor(behavior), fill =metric)) + 
  ggplot2::geom_boxplot(outlier.shape = 19, outlier.alpha = 0.1) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = level_order) + ggplot2::xlab("") + 
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) +
  ggplot2::scale_fill_discrete(name = "Accelerometer placement", labels = c("Hip", "Wrist"))

bxp_MAD <- ggplot2::ggplot(df_long.MAD, ggplot2::aes(y = value, x = as.factor(behavior), fill =metric)) + 
  ggplot2::geom_boxplot(outlier.shape = 19, outlier.alpha = 0.1) + ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
  ggplot2::scale_x_discrete(limits = level_order) + ggplot2::xlab("App categories") + 
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) +
  ggplot2::scale_fill_discrete(name = "Accelerometer placement", labels = c("Hip", "Wrist"))

gridExtra::grid.arrange(bxp_ENMO, bxp_MAD, nrow=2) #arranges plots within grid

boxplots <- gridExtra::arrangeGrob(bxp_ENMO + ggplot2::theme(legend.position="top"),
                                   bxp_MAD + ggplot2::theme(legend.position="none"),
                                   nrow=2) # generates plot
plot(boxplots) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/boxplot_behaviors.png"), boxplots, width = 10, height = 8, dpi = 600) #saves g

## SCATTERPLOTS
df2 <- dplyr::select(data.app.axivity, -behavior)

scatter.ENMOhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = ENMO.hip, color = behavior)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = behavior)) + 
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ggtitle("Hip") + ggplot2::ylim(c(0, 500)) 

scatter.ENMOwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = ENMO.wrist, color = behavior)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = behavior)) + 
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ylim(c(0, 500)) + ggplot2::ggtitle("Wrist")

gridExtra::grid.arrange(scatter.ENMOhip, scatter.ENMOwrist, ncol=2) #arranges plots within grid
scatter_ENMO <- gridExtra::arrangeGrob(scatter.ENMOhip + ggplot2::theme(axis.title.x = ggplot2::element_blank()),
                                       scatter.ENMOwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       ncol=2, 
                                       bottom = "Duration (min)") # generates plot
plot(scatter_ENMO) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/scatter_behavior_ENMO.png"), scatter_ENMO, width = 10, height = 8, dpi = 600) #saves g

scatter.MADhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = MAD.hip, color = behavior)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = behavior)) + 
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ggtitle("Hip") + ggplot2::ylim(c(0, 500)) 

scatter.MADwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = total_duration, y = MAD.wrist, color = behavior)) + 
  ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
  ggplot2::geom_point(ggplot2::aes(color = behavior)) + 
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 4) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::ylab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::xlab("Total duration (min)") +
  ggplot2::ylim(c(0, 500)) + ggplot2::ggtitle("Wrist")

gridExtra::grid.arrange(scatter.MADhip, scatter.MADwrist, ncol=2) #arranges plots within grid
scatter_MAD <- gridExtra::arrangeGrob(scatter.MADhip + ggplot2::theme(axis.title.x = ggplot2::element_blank()),
                                      scatter.MADwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                      ncol=2, 
                                      bottom = "Duration (min)") # generates plot
plot(scatter_MAD) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/scatter_behavior_MAD.png"), scatter_MAD, width = 10, height = 8, dpi = 600) #saves g

## DENSITY PLOTS
# Per behavior
density.ENMOwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.wrist, color = behavior)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.ENMOhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = ENMO.hip, color = behavior)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") ggplot2::xlim(c(0, 500))

gridExtra::grid.arrange(density.ENMOwrist, density.ENMOhip, nrow=2) #arranges plots within grid
density_ENMO <- gridExtra::arrangeGrob(density.ENMOwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                       density.ENMOhip + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                       nrow=2, left = "Density") # generates plot
plot(density_ENMO) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/density_behaviors_ENMO.png"), density_ENMO, width = 10, height = 15, dpi = 600) #saves g

density.MADwrist <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = MAD.wrist, color = behavior)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.MADhip <- ggplot2::ggplot(data.app.axivity, ggplot2::aes(x = MAD.hip, color = behavior)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 5) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") + ggplot2::xlim(0, 500)

gridExtra::grid.arrange(density.MADwrist, density.MADhip, nrow=2) #arranges plots within grid
density_MAD <- gridExtra::arrangeGrob(density.MADwrist + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                      density.MADhip + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                      nrow=2, left = "Density") # generates plot
plot(density_MAD) #print the plot
ggplot2::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/density_behaviors_MAD.png"), density_MAD, width = 10, height = 15, dpi = 600) #saves g

# Postures for activities: active play, quiet play, i don't know play, and sitting/lying
data.app.axivity.posture <- data.app.axivity[data.app.axivity$behavior %in% c("SB", "PA"),]
density.ENMOwrist.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = ENMO.wrist, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.ENMOhip.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = ENMO.hip, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="bottom") +
  ggplot2::xlab(expression(paste("ENMO (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") 

gridExtra::grid.arrange(density.ENMOwrist.posture, density.ENMOhip.posture, nrow=2) #arranges plots within grid
density_ENMO.posture <- gridExtra::arrangeGrob(density.ENMOwrist.posture + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                               density.ENMOhip.posture + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                               nrow=2, left = "Density") # generates plot
plot(density_ENMO.posture) #print the plot
ggplot2:::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/density_behaviors_posture_ENMO.png"), density_ENMO.posture, width = 210, height = 297, units = "mm")

density.MADwrist.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = MAD.wrist, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Wrist") 

density.MADhip.posture <- ggplot2::ggplot(data.app.axivity.posture, ggplot2::aes(x = MAD.hip, color = posture)) + 
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_wrap(~factor(behavior, levels = level_order), ncol = 2) + 
  ggplot2::theme_classic() + ggplot2::theme(legend.position="bottom") +
  ggplot2::xlab(expression(paste("MAD (m", italic("g"), ")"))) + ggplot2::ylab("Density") +
  ggplot2::ggtitle("Hip") 

gridExtra::grid.arrange(density.MADwrist.posture, density.MADhip.posture, nrow=2) #arranges plots within grid
density_MAD.posture <- gridExtra::arrangeGrob(density.MADwrist.posture + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                              density.MADhip.posture + ggplot2::theme(axis.title.y = ggplot2::element_blank()),
                                              nrow=2, left = "Density") # generates plot
plot(density_MAD.posture) #print the plot
ggplot2:::ggsave(file=paste0(savedir, "/plots/distributions/behaviors/density_behaviors_posture_MAD.png"), density_MAD.posture, width = 210, height = 297, units = "mm")



