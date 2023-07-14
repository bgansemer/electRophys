plotBasePeak <- function(data, plotTitle, size = 2 , cols = c("black", "magenta")) {
  
  #' Generate plots showing baseline amplitude and uncorrected ePSC amplitude.
  #' @description Generates separate plots for pre- and post-depolarization.
  #' May add functionality to generate plots for correct ePSC amplitude.
  #' More description here.
  #' 
  #' Add params info here
  #' 
  #' @examples
  #' plotBasePeak(data, size = 3, cols = c("black", "magenta"))
  
  # specify legend
  lgnd <- c("Prestim Baseline" = cols[1], "ePSC Peak" = cols[2])
  
  # get min and max amplitudes for specifying y-axis limits so both plots have 
  # same limits
  minY <- round(min(data$PreBaselineAmp, data$PostBaselineAmp, 
              data$UncorPrePeak, data$UncorPostPeak), digits = -1) - 5
  maxY <- round(max(data$PreBaselineAmp, data$PostBaselineAmp, 
              data$UncorPrePeak, data$UncorPostPeak), digits = -1) + 5
  
  # if maxY is less than 0, set it to 0
  if (maxY < 0){
    maxY <- 0
  }
  
  # generate plot title
  title <- ggdraw() +
    draw_label(plotTitle, fontface = "bold", hjust = 0)
  
  
  # generate pre-depolarization plot
  prePlot <- ggplot(data, aes(x = Time)) +
    geom_point(aes(y = PreBaselineAmp, color = "Prestim Baseline"), size = size) +
    geom_point(aes(y = UncorPrePeak, color = "ePSC Peak"), shape = 17, size = size) +
    labs(title = "pre-depolarization", x = NULL, y = "\namplitude (pA)",
         color = "Legend") +
    geom_line(aes(y = 0), linetype = "dashed", linewidth = 1) +
    theme_BG() +
    scale_color_manual(values = lgnd) +
    scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 15),
                       expand = expansion(mult = c(0,0), add = c(1,2))) +
    scale_y_continuous(limits = c(minY, maxY),
                       breaks = seq(0, minY, -50),
                       expand = c(0, 0))

  
  # generate post-depolarization plot
  postPlot <- ggplot(data, aes(x = Time)) +
    geom_point(aes(y = PostBaselineAmp, color = "Prestim Baseline"), size = size) +
    geom_point(aes(y = UncorPostPeak, color = "ePSC Peak"), shape = 17, size = size) +
    labs(title = "post-depolarization", x = NULL, y = NULL,
         color = "Legend") +
    geom_line(aes(y = 0), linetype = "dashed", linewidth = 1 ) +
    theme_BG() +
    scale_color_manual(values = lgnd) +
    scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 15),
                       expand = expansion(mult = c(0,0), add = c(1,2))) +
    scale_y_continuous(limits = c(minY, maxY),
                       breaks = seq(0, minY, -50),
                       expand = c(0,0))
  
  
  # plot baseline corrected ePSC amplitude
  preCorPlot <- ggplot(data, aes(x = Time, y = PrePeakAmp, 
                                 color = "ePSC Peak")) +
    geom_point(shape = 17, size = size) +
    labs(x = "Time (s)", y = "corrected\nePSC amplitude (pA)", 
            color = "Legend") +
    scale_color_manual(values = lgnd) +
    theme_BG() +
    scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 15),
                       expand = expansion(mult = c(0,0), add = c(1,2))) +
    scale_y_continuous(limits = c(minY, 0), 
                       breaks = seq(0, minY, -50),
                       expand = c(0, 0))
  
  # could potentially add baseline ePSC amplitude as a line
  postCorPlot <- ggplot(data, aes(x = Time, y = PostPeakAmp, 
                                 color = "ePSC Peak")) +
    geom_point(shape = 17, size = size) +
    labs(x = "Time (s)", y = NULL, color = "Legend") +
    theme_BG() +
    scale_color_manual(values = lgnd) +
    scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 15),
                       expand = expansion(mult = c(0,0), add = c(1,2))) +
    scale_y_continuous(limits = c(minY, 0), 
                       breaks = seq(0, minY, -50),
                       expand = c(0, 0))
  
  # uses cowplot. may try patchwork instead to improve spacing
  # combine plots into one grid
  tempPlot <- plot_grid(prePlot + theme(legend.position = "none"),
                        postPlot + theme(legend.position = "none"),
                        preCorPlot + theme(legend.position = "none"),
                        postCorPlot + theme(legend.position = "none"),
                        align = "vh", hjust = -1,
                        nrow = 2, ncol = 2)
  
  row1 <- plot_grid(prePlot + theme(legend.position = "none"),
                    postPlot + theme(legend.position = "none"),
                    align = "vh", hjust = -1,
                    ncol = 2)
  
  row2 <- plot_grid(preCorPlot + theme(legend.position = "none"),
                    postCorPlot + theme(legend.position = "none"),
                    align = "vh", hjust = -1,
                    ncol = 2)
  
                       
  
  # add legends
  leg <- get_legend(prePlot + 
                      theme_BG() +
                      guides(color = guide_legend(nrow = 1)) +
                      theme(legend.position = "bottom"))
  
  BPplot <- plot_grid(title, row1, row2, leg,
                      ncol = 1, align = "v", axis = "l",
                      rel_heights = c(0.1, 1, 1, 0.1))
  
  BPplot
}