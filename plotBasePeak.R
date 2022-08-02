plotBasePeak <- function(data, size = 3 , cols = c("black", "magenta")) {
  
  lgnd <- c("Baseline" = cols[1], "Peak" = cols[2])
  prePlot <- ggplot(data, aes(x = Time)) +
    geom_point(aes(y = PreBaselineAmp, color = "Baseline"), size = size) +
    geom_point(aes(y = UncorPrePeak, color = "Peak"), shape = 17, size = size) +
    labs(title = "pre-depolarization", x = "Time (s)", y = "amplitude (pA)", 
         color = "Legend") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = lgnd) +
    scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 15),
                       expand = c(0,0))
  
  postPlot <- ggplot(data, aes(x = Time)) +
    geom_point(aes(y = PostBaselineAmp, color = "Baseline"), size = size) +
    geom_point(aes(y = UncorPostPeak, color = "Peak"), shape = 17, size = size) +
    labs(title = "post-depolarization", x = "Time (s)", y = NULL, 
         color = "Legend") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = lgnd) +
    scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 15),
                       expand = c(0,0))
  
  # combine plots into one row
  tempPlot <- plot_grid(prePlot + theme(legend.position = "none"),
                        postPlot + theme(legend.position = "none"),
                        align = "vh", hjust = -1, nrow = 1)
  
  # add legends
  leg <- get_legend(prePlot + 
                      guides(color = guide_legend(nrow = 1)) +
                      theme(legend.position = "bottom"))
  
  BPplot <- plot_grid(tempPlot, 
                      leg, ncol = 1, rel_heights = c(1, .1))
  
  return(BPplot)
}