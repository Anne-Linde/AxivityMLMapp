# metric one of c("ENMO", "MAD")
# per one of c("activity", "posture', "behavior")
create.boxplot <- function(data_long, metric, per, order_categories){
  
  if(per == "activity"){
    grouping <- data_long$activity
  } else if (per == "posture"){
    grouping <- data_long$posture
  } else
    grouping <- data_long$behavior

  if(metric == "ENMO"){
    ylabel = expression(paste("ENMO (m", italic("g"), ")"))
  } else
    ylabel = expression(paste("MAD (m", italic("g"), ")"))
  
  bxp <- ggplot2::ggplot(data_long, ggplot2::aes(y = value, x = as.factor(grouping), fill =metric)) + 
    ggplot2::geom_boxplot(outlier.shape = 19, outlier.alpha = 0.1) + ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
    ggplot2::scale_x_discrete(limits = order_categories) + ggplot2::xlab("") + 
    ggplot2::ylab(ylabel) +
    ggplot2::scale_fill_discrete(name = "Accelerometer placement", labels = c("Hip", "Wrist"))
  
  return(bxp)
}