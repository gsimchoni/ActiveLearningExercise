#' Function to plot experiment's results
#' 
#' @author gsimchoni
#' 
#' @param data The data frame with first column n, then accuracy columns to plot
#' @param yrange Vector of minimum and maximum y-axis scale
#' @param title Title plot
#' @param legendLabels The labels to put for each accuracy column
#' @param legendTitle Title of legend
plot.res <- function (data, yrange, title, legendLabels, legendTitle) {
  # get the range for the x and y axis 
  xrange <- range(data$n) 
  yrange <- yrange
  
  # set up the plot 
  plot(xrange, yrange, type = "n", xlab = "Sample N",
       ylab = "Accuracy" ) 
  colors <- rainbow(ncol(data[,-1])) 
  linetype <- c(1:ncol(data[,-1])) 
  plotchar <- seq(18,18+ncol(data[,-1]),1)
  
  # add lines 
  for (i in 1:ncol(data[,-1])) { 
    d <- data[, c(1, i + 1)]
    lines(d, type="b", lwd=1.5,
          lty = linetype[i], col = colors[i], pch = plotchar[i]) 
  } 
  
  # add a title and subtitle 
  title(title)
  
  # add a legend 
  legend(xrange[1], yrange[2], legendLabels, cex = 0.8, col = colors,
         pch = plotchar, lty = linetype, title = legendTitle)
}