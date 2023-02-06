plotTraces <- function(data, xMin=NULL, xMax=NULL, yMin=NULL, yMax=NULL, colrs) {
  
  #' Generates a line plot of electrophysiology trace data. 
  #' @description Generates a single plot containing all traces in provided data.
  #' Typical use is for a plot comparing two traces, one control/baseline and
  #' one treatment/after stimulus or drug exposure. 
  #' 
  #' @param data .atf file from pClamp with XY data. The first column should
  #' be the x data in the form of time is ms. Additional columns should be the
  #' y data in the form of trace amplitudes.
  #' @param xMin Minimum x value for the plot.
  #' @param xMax Maximum x value for the plot.
  #' @param yMin Minimum y value for the plot.
  #' @param yMax Maximum y value for the plot.
  #' @param colrs Character vector of colors. Can be accepted color names or hexadecimal.
  #' 
  #' @examples provide example here when code is finished.
  #' 
  
  # load in libraries
  library(ggplot2)
  
  # read in data
  traceData <- as.data.frame(read.delim(data, header = T, skip = 2))
  
  # set axis limits to min and max in data if none are provided
  if (is.null(xMin)) {
    xMin = min(traceData[,1])
  }
  
  if (is.null(xMax)) {
    xMax = max(traceData[,1])
  }
  
  if (is.null(yMin)) {
    yMin = min(traceData[,-1])
  }
  
  if (is.null(yMax)) {
    yMax = max(traceData[,-1])
  }
  
  # get column names 
  cNames <- colnames(traceData)
  
  # generate plot using ggplot2
  tracePlot <- ggplot(data = traceData, aes(x = traceData[,1], y = traceData[,2])) +
    geom_line() +
    geom_line(aes(y=traceData[,3])) +
    scale_x_continuous(limits = c(xMin, xMax)) +
    scale_y_continuous(limits = c(yMin, yMax))
  
  return(tracePlot)
}