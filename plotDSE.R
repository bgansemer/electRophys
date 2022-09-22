plotDSE <- function(data, plotTitle, preAvg) {
  
  #' Generate plots showing baseline amplitude and uncorrected ePSC amplitude.
  #' @description Generates separate plots for pre- and post-depolarization.
  #' May add functionality to generate plots for correct ePSC amplitude.
  #' More description here.
  #' 
  #' Add params info here
  #' 
  #' @examples
  #' plotDSE(data, size = 3, plotTitle = "Title", preAvg = 15)
  #' 
  #' 
  
  totalSweeps <- 1:(length(data$Sweep)+preAvg)
  totalTime <- (totalSweeps)*1.99998
  
  percAmp <- tail(data$percBaselinePre, preAvg)
  percAmp <- append(percAmp, data$percBaselinePost)
  
  plotPercData <- data.frame(Time = totalTime, percAmplitude = percAmp)
  
  percAmpPlot <- ggplot(data = plotPercData,
                        aes(x = Time, y = percAmplitude)) +
    geom_point() +
    scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 30),
                       expand = expansion(mult = c(0,0), add = c(1,2))) +
    scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 25), 
                       expand = c(0, 0)) +
    labs(x = "Time (s)", y = "ePSC amplitude (%)", title = plotTitle) +
    theme_BG()
  
  return(percAmpPlot)
  
  
}