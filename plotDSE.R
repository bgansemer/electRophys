plotDSE <- function(data, plotTitle, preAvg, ptSize = 2, errorBars = "None") {
  
  #' Generate plots showing baseline amplitude and uncorrected ePSC amplitude.
  #' @description Generates separate plots for pre- and post-depolarization.
  #' May add functionality to generate plots for correct ePSC amplitude.
  #' More description here.
  #' 
  #' @param data dataframe of data for plotting.
  #' @param plotTitle string, title of plot
  #' @param preAvg integer, number of sweeps to show prior to depolarization
  #' @param ptSize integer, size of points for geom_point
  #' @param stdError boolean, specify whether to plot standard error bars
  #' 
  #' @examples
  #' plotDSE(data, size = 3, plotTitle = "Title", preAvg = 15)
  #' 
  #' 
  
  totalSweeps <- 1:(length(data$Sweep)+preAvg)
  totalTime <- (totalSweeps-1)*1.99998
  
  percAmp <- tail(data$percBaselinePre, preAvg)
  percAmp <- append(percAmp, data$percBaselinePost)
  
  if (errorBars == "std_error") {
    err <- tail(data$preSE, preAvg)
    err <- append(err, data$postSE)
    plotPercData <- data.frame(Time = totalTime, percAmplitude = percAmp, 
                               errBar <- err)
  } else if (errorBars == "std_dev") {
    err <- tail(data$preSD, preAvg)
    err <- append(err, data$postSD)
    plotPercData <- data.frame(Time = totalTime, percAmplitude = percAmp,
                               errBar <- err)
  } else if (errorBars == "None") {
    plotPercData <- data.frame(Time = totalTime, percAmplitude = percAmp)
  }
  
  if (errorBars == "None") {
    
    percAmpPlot <- ggplot(data = plotPercData,
                          aes(x = Time, y = percAmplitude)) +
      geom_point(size = ptSize) +
      scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 30),
                         expand = expansion(mult = c(0,0), add = c(1,2))) +
      scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 25), 
                         expand = c(0, 0)) +
      labs(x = "Time (s)", y = "ePSC amplitude (%)", title = plotTitle) +
      theme_BG()
    
  } else {
    
    percAmpPlot <- ggplot(data = plotPercData,
                          aes(x = Time, y = percAmplitude)) +
      geom_point(size = ptSize) +
      geom_errorbar(aes(ymin = percAmplitude-errBar, 
                        ymax = percAmplitude+errBar),
                    width = 2, position = position_dodge(0.05)) +
      scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 30),
                         expand = expansion(mult = c(0,0), add = c(1,2))) +
      scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 25), 
                         expand = c(0, 0)) +
      labs(x = "Time (s)", y = "ePSC amplitude (%)", title = plotTitle) +
      theme_BG()
    
  }
  
  return(percAmpPlot)
  
  
}