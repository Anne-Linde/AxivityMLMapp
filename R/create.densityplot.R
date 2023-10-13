#' create.densityplot
#'
#' @description 'create.densityplot' loads the cleaned app data

#' @param data Data description here
#' @param metric A string that contains the name of the metric. One of c("ENMO", "MAD")
#' @param placement A string that contains the placement of the acceleromter. One of c("hip", "wrist")
#' @param per A string that indicating the plot windows. One of c("activity", "behavior, "posture")
#' @param order_categories Vector with the label names in the desired order

#' @return DESCRIPTION HERE
#' @import ggplot2
#' @export

create.densityplot <- function(data, metric, placement, per, order_categories){
  if (metric == "ENMO")
    label = expression(paste("ENMO (m", italic("g"), ")"))
  else
    label = expression(paste("MAD (m", italic("g"), ")"))
  
  if (placement == "hip")
    title = "Hip"
  else
    title = "Wrist"
  value = paste(metric, placement, sep = ".")
  density <- ggplot2::ggplot(data, ggplot2::aes(x = eval(as.symbol(value)), color = eval(as.symbol(per)))) + 
    ggplot2::geom_density(alpha = 0.25) +
    ggplot2::facet_wrap(~factor(eval(as.symbol(per)), levels = order_categories), ncol = 5) + 
    ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
    ggplot2::xlab(label) + ggplot2::ylab("Density") +
    ggplot2::ggtitle(title) 
  return(density)
}