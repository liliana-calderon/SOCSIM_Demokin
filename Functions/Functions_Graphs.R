#---------------------------------------------------------------------------------------------------------------
# Theme for the graphs 
# Last modified on 27-06-2023
#---------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(extrafont)

## Creation of a theme for the graphs

theme_graphs <- function() {
  theme(plot.title = element_text(family="serif", size = 24, face = "bold", 
                                  hjust=0.5, margin = margin(0, 0, 10, 0)),
        plot.subtitle = element_text(family="serif", colour = "#000000", size = 14, 
                                     hjust=0.5, margin = margin(0, 0, 10, 0)),
        plot.caption = element_text(family="serif", colour = "#000000", size = 14, 
                                    hjust=1, margin = margin(10, 0, 10, 0)),
        plot.background = element_rect(fill = "#FFFFFF"),
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm"),
        panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#000000", linetype = "solid"), 
        panel.grid.major.x = element_line(colour = "#C9C9C9", linetype = "dotted"),
        panel.grid.major.y = element_line(colour = "#C9C9C9", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(family="serif", face="bold", size=26),
        strip.background = element_rect(fill="#FFFFFF", colour="#000000", linewidth = 0.5),
        axis.title.x = element_text(family="serif", size = 22, colour = "#000000", 
                                    hjust=0.5, face = "bold", margin = margin(10, 0, 0, 0)), 
        axis.title.y = element_text(family="serif", size = 22, colour = "#000000", 
                                    face = "bold", margin = margin(0, 10, 0, 0)), 
        axis.text = element_text(family="serif", size = 20, colour = "#000000"),
        axis.line.y = element_line(colour = "#000000"),
        axis.line.x = element_line(colour = "#000000"),
        axis.ticks = element_line(colour = "#000000", linewidth = 1),    
        legend.title = element_text(family="serif", size = 20, colour = "#000000", face = "bold"),
        legend.text = element_text(family="serif", size = 20, colour = "#000000"),
        legend.background = element_rect(fill = "#FFFFFF", colour = "#000000", linewidth = 0.3, linetype = "solid"), 
        legend.key = element_rect(fill = NA, color = NA), 
        legend.position = "bottom", 
        legend.direction = "horizontal"
  )
}

## Creation of a theme for the comparison graphs

theme_graphs2 <- function() {
  theme(panel.grid.major.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        panel.grid.major.x = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        panel.grid.minor.x = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        panel.background = element_rect(fill = NA), 
        panel.ontop = TRUE,
        text = element_text(family = "serif", size = 12),
        legend.key = element_blank(),
        #legend.key = element_rect(fill = NA, color = NA), 
        strip.background = element_rect(color = "#000000", fill = "#FFFFFF"),
        strip.text = element_text(family="serif", face="bold", size=26),
        axis.title.x = element_text(family="serif", size = 20, colour = "#000000", 
                                    hjust=0.5, face = "bold", margin = margin(10, 0, 0, 0)), 
        axis.title.y = element_text(family="serif", size = 20, colour = "#000000", face = "bold"), 
        axis.text = element_text(family="serif", size = 16, colour = "#000000"),
        legend.text = element_text(family="serif", size = 18, colour = "#000000")) }

# Convert SOCSIM months to calendar years. 
asYr <- function(month, last_month, final_sim_year) {
  return(final_sim_year - trunc((last_month - month)/12))
}