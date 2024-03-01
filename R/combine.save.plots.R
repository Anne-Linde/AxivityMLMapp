#format = one of ("boxplots", "scatterplots") 


#' combine.save.plots
#'
#' @description 'combine.save.plots' loads the cleaned app data

#' @param plot1 Plot object 1
#' @param plot2 Plot object 2
#' @param format A string that contains the placement of the acceleromter. One of c("boxplots", "overlay", "scatterplots", "densityplots")
#' @param savedir A string that indicates the path to which the plots will be saved
#' @param filename A string that indicates the name of the file and ends with the extension (e.g. .pgn, .jpeg) 

#' @return DESCRIPTION HERE
#' @import ggplot2
#' @export


combine.save.plots <- function(plot1, plot2, format, savedir, filename){
  
  if(format == "boxplots" | format == "overlay"){
    
    
    gridExtra::grid.arrange(plot1, plot2, nrow = 2, heights = c(1.2, 1)) #arranges plots within grid
    if(format = "overlay"){
      plot1_build <- ggplot2::ggplot_build(plot1)
      plot2_build <- ggplot2::ggplot_build(plot2)
      ylim <- max(round(plot1_build$layout$panel_scales_y[[1]]$range$range[2], 1), round(plot2_build$layout$panel_scales_y[[1]]$range$range[2], 1))
      plot1 <- plot1 + ggplot2::scale_y_continuous(name="Wrist", limits = c(0, ylim)) 
      plot2 <- plot2 + ggplot2::scale_y_continuous(name="Hip", limits = c(0, ylim))
      
      shared_y_label <- grid::textGrob(expression(paste("ENMO (", italic("g"), ")")), rot = 90)
    }
    plots <- gridExtra::arrangeGrob(
                                    plot1 + ggplot2::theme(legend.position="top"),
                                    plot2 + ggplot2::theme(legend.position="none"),
                                    left = shared_y_label,
                                    nrow=2,
                                    heights = c(4, 3.5)) # generates plot
    
  }
  
  if(format == "scatterplots" | format == "densityplots"){
    if(format == "scatterplots"){
      label = "Total duration (min)"
    } else {
      label = grid::grid.text(plot1$labels$x)
    }
    gridExtra::grid.arrange(plot1, plot2, ncol=2) #arranges plots within grid
    plots <- gridExtra::arrangeGrob(plot1 + ggplot2::theme(axis.title.x = ggplot2::element_blank()),
                                          plot2 + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()),
                                          ncol=2, 
                                          bottom = label) # generates plot
  }
  #ADD y-axis label for overlay plots: ENMO(g)
  ggplot2::ggsave(file=paste0(savedir, filename), plots, width = 10, height = 8, dpi = 600) #saves g
  #ggplot2:::ggsave(file=paste0(savedir, filename), plots, width = 210, height = 297, units = "mm")
  
}