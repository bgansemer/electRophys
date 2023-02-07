plotTraces <- function(data, xMin=190, xMax=240, yMin=NULL, yMax=50, 
                       lineSize = 1, colrs = c("black", "red")) {
  
  #' Generates a line plot of electrophysiology trace data. 
  #' @description Generates a single plot containing all traces in provided data.
  #' Current use is for a plot comparing two traces, one control/baseline and
  #' one treatment/after stimulus or drug exposure. Future developments may 
  #' support more than 2 traces. 
  #' 
  #' @param data .atf file from pClamp with XY data. The first column should
  #' be the x data in the form of time is ms. Additional columns should be the
  #' y data in the form of trace amplitudes.
  #' @param xMin Minimum x value for the plot.
  #' @param xMax Maximum x value for the plot.
  #' @param yMin Minimum y value for the plot.
  #' @param yMax Maximum y value for the plot.
  #' @param lineSize Size of line.
  #' @param colrs Character vector of colors. Can be accepted color names or hexadecimal.
  #' 
  #' @examples provide example here when code is finished.
  #' 
  
  # load in libraries
  library(ggplot2)
  library(plyr)
  
  # read in data
  inputData <- as.data.frame(read.delim(data, header = T, skip = 2))
  
  # reorganize data so any number of multiple traces can be plotted
  # get column names 
  cNames <- colnames(inputData)
  
  # separate time and trace data into separate vectors
  timeMS <- inputData[[cNames[1]]]
  traces <- numeric()
  traceName <- c()
  for (t in 2:length(cNames)) {
    #append trace data to one column for traces
    traces <- append(traces, inputData[[cNames[t]]])
    
    #create column with repeated trace names (column names of trace data)
    for (i in 1:length(inputData[[cNames[t]]])) {
      traceName <- append(traceName, cNames[t])
    }
    
    #extend time column so time is repeated for each trace
    if (t<length(cNames)) {
      timeMS <- append(timeMS, timeMS)
    }
    
  }
  
  # organize into dataframe
  traceData <- data.frame(timeMS, traces, traceName)

  
  # set axis limits to min and max in data if none are provided
  if (is.null(xMin)) {
    xMin <- min(traceData$timeMS)
  }

  if (is.null(xMax)) {
    xMax <- max(traceData$timeMS)
  }

  if (is.null(yMin)) {
    #yMin <- min(traceData[,-1])
    # begin <- min(which(round(traceData$traces)==196))
    # end <- max(which(round(traceData$traces)==220))
    # yMin <- round_any(min(traceData$traces[begin:end, -1]), 25, floor)
    yMin <- -150
  }

  if (is.null(yMax)) {
    #yMax = max(traceData[,-1])
    yMax <- 50
  }

  # generate plot using ggplot2
  tracePlot <- ggplot(data = traceData, aes(x = timeMS, y = traces, col = traceName)) +
    geom_line(size = lineSize) +
    coord_cartesian(xlim = c(xMin, xMax), ylim = c(yMin, yMax),
                    expand = FALSE) +
    scale_x_continuous(breaks = seq(xMin, xMax, 5)) +
    scale_y_continuous(breaks = seq(yMin, yMax, 25)) +
    scale_color_manual(values = colrs) +
    labs(x = "Time (ms)", y = "pA") +
    theme(
      plot.title = element_text(size = rel(1), face = "bold",
                                margin = margin(0,0,5,0),
                                hjust = 0.5),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text = element_text(size = rel(0.70)),
      axis.title = element_text(size = rel(0.85), face = "bold"),
      legend.title = element_text(size = rel(0.7)),
      legend.text = element_text(size = rel(0.6)),
      panel.grid = element_blank(),
      panel.background = element_blank()
    )


  #return(tracePlot)
  tracePlot
}