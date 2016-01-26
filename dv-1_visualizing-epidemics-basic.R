# Author: Matthew Phelps
# Desc: Comparing the 3 cities' outbreals
# Output datasets: Rdata in local directory


## Intro
rm(list = ls())
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1")

setwd(data.path)


library(ggplot2)
library(ggmap)
library(dplyr)
library(grid)
library(tidyr)



# LOAD & PREP--------------------------------------------------------------------

load("Rdata\\Data-2_cities-daily.Rdata")
all_cases <- rbind(aalborg_day, cph_day, korsoer_day)



# INCIDENCE PER 10K ----------------------------------------------------

incidence_10k_plot <- ggplot(data = all_cases, aes(x = day_index, y = cases_norm, group = city, color = city)) +
  geom_line(size = 1.2) +
  xlab("Day index") +
  ylab("Incidence per 10,000") +
  xlim(0, 80) +
  ggtitle ("Daily incidence \nper 10,000 people") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.7, 0.5),
        legend.text = element_text(size = 19),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold", vjust = -0.1),
        axis.title.y = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 28, face="bold"),
        plot.margin = unit(c(0,0,0.5,0), 'lines')) +
  coord_cartesian(xlim = c(0, 80), ylim = c(5,max(all_cases$cases_norm)+5)) +
    scale_color_discrete(breaks = c('aalborg', 'cph', 'korsoer'),
                         labels = c('Aalborg', 'Copenhagen', 'Korsoer'))
incidence_10k_plot


# ABSOLUTE CASES ----------------------------------------------------------

number_cases_plot <- ggplot(data = all_cases, aes(x = day_index, y = cases, group = city, color = city)) +
  geom_line(size = 1.2) +
  xlab("Day index") +
  ylab("Incidence per 10,000") +
  xlim(0, 80) +
  ggtitle ("Number of cases in 3 Danish cities\n") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.9, 0.5),
        legend.text = element_text(size = 19),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold", vjust = -0.1),
        axis.title.y = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 28, face="bold"),
        plot.margin = unit(c(0,0,0.5,0), 'lines')) +
  coord_cartesian(xlim = c(0, 80), ylim = c(5,max(all_cases$cases)+5)) +
  scale_color_discrete(breaks = c('aalborg', 'cph', 'korsoer'),
                       labels = c('Aalborg', 'Copenhagen', 'Korsoer'))
number_cases_plot
