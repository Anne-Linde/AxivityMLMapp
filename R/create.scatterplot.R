#' create.scatterplot
#'
#' @description 'create.scatterplot' loads the cleaned app data

#' @param data Data description here
#' @param metric A string that contains the name of the metric. One of c("ENMO", "MAD")
#' @param placement A string that contains the placement of the acceleromter. One of c("hip", "wrist")
#' @param per A string that indicating the plot windows. One of c("activity", "behavior, "posture")
#' @param order_categories Vector with the label names in the desired order

#' @return DESCRIPTION HERE
#' @import ggplot2
#' @export

create.scatterplot <- function(data, metric, placement, per, order_categories){
  if (metric == "ENMO")
    ylabel = expression(paste("ENMO (m", italic("g"), ")"))
  else
    ylabel = expression(paste("MAD (m", italic("g"), ")"))
  if (placement == "hip")
    title = "Hip"
  else
    title = "Wrist"
  value = paste(metric, placement, sep = ".")
  df2 <- dplyr::select(data, -per)
  scatter <- ggplot2::ggplot(data, ggplot2::aes(x = total_duration, y = eval(as.symbol(value)), color = eval(as.symbol(per)))) + 
    ggplot2::geom_point(data = df2, color = "grey70", alpha = .5) + 
    ggplot2::geom_point(ggplot2::aes(color = eval(as.symbol(per)))) + 
    ggplot2::facet_wrap(~factor(eval(as.symbol(per)), levels = order_categories), ncol = 4) + 
    ggplot2::theme_classic() + ggplot2::theme(legend.position="none") +
    ggplot2::ylab(ylabel) + ggplot2::xlab("Total duration (min)") +
    ggplot2::ggtitle(title) + ggplot2::ylim(c(0, 500)) 
  return(scatter)
}