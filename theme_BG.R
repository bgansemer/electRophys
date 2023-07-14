# generate new ggplot theme for plots
theme_BG <- function(base_size = 14){
  theme_bw(base_size = base_size) %+replace%
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
    )
  
}