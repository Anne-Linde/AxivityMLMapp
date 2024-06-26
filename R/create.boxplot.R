#' create.boxplot
#'
#' @description 'create.boxplot' function description

#' @param data_long Data description here
#' @param metric A string that contains the name of the metric. One of c("ENMO", "MAD")
#' @param per A string that indicating the plot windows. One of c("activity", "behavior, "posture")
#' @param order_categories Vector with the label names in the desired order

#' @return DESCRIPTION HERE
#' @import ggplot2
#' @export
#' 
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
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.55) + ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1), legend.position="top") +
    ggplot2::scale_x_discrete(limits = order_categories) + ggplot2::xlab("") + 
    ggplot2::ylab(ylabel) +
    ggplot2::scale_fill_manual(values = c("#440154FF", "#FDE725FF"), name = "Accelerometer placement", labels = c("Hip", "Wrist"))+
    ggplot2::ylim(0,150) 
  
  return(bxp)
}