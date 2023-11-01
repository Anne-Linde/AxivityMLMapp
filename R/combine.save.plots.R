#format = one of ("boxplots", "scatterplots") 


#' combine.save.plots
#'
#' @description 'combine.save.plots' loads the cleaned app data

#' @param plot1 Plot object 1
#' @param plot2 Plot object 2
#' @param format A string that contains the placement of the acceleromter. One of c("boxplots", "scatterplots", "densityplots")
#' @param savedir A string that indicates the path to which the plots will be saved
#' @param filename A string that indicates the name of the file and ends with the extension (e.g. .pgn, .jpeg) 

#' @return DESCRIPTION HERE
#' @import ggplot2
#' @export


combine.save.plots <- function(plot1, plot2, format, savedir, filename){
  
  if(format == "boxplots"){
    gridExtra::grid.arrange(plot1, plot2, nrow = 2) #arranges plots within grid
    plots <- gridExtra::arrangeGrob(plot1 + ggplot2::theme(legend.position="top"),
                                    plot2 + ggplot2::theme(legend.position="none"),
                                    nrow=2) # generates plot
    
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
  
  ggplot2::ggsave(file=paste0(savedir, filename), plots, width = 10, height = 8, dpi = 600) #saves g
  #ggplot2:::ggsave(file=paste0(savedir, filename), plots, width = 210, height = 297, units = "mm")
  
}