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
devtools::install_github('matthew-phelps/CholeraDataDK', force = T)

library(ggplot2)
library(ggmap)
library(dplyr)
library(grid)
library(tidyr)
library(CholeraDataDK)



# LOAD & PREP--------------------------------------------------------------------

all_cases_temp <- cholera_daily_data
cases <- cholera_daily_data_towns



# RATES FOR SMALL TOWNS
pop <- dk_population
pop <- pop[!is.na(pop$year),]
pop <- pop[pop$year=="1853" | pop$year == "1857" | pop$city == "nykoebing", ]
colnames(cases) <- c("date", "cases", "day_index", "city")

cases$cases_norm[cases$city=="nykoebing"] <- cases$cases[cases$city=="nykoebing"] / pop$pop[pop$city == "nykoebing"]
cases$cases_norm[cases$city=="frederikshavn"] <- cases$cases[cases$city=="frederikshavn"] / pop$pop[pop$city == "frederikshavn"]


all_cases_temp <- merge(all_cases_temp, cases, all = T)
all_cases_temp <- all_cases_temp[order(all_cases_temp$city, all_cases_temp$date), ]

# To plot all epidemics on same calendar year - put them on a "dummy" year
all_cases_temp$season <- paste("100",
                               all_cases_temp$month,
                               all_cases_temp$day,
                               sep = "-")
all_cases_temp$season <- as.Date(all_cases_temp$season)
all_cases <- all_cases_temp[all_cases_temp$city != "brandholm", ]



# ABSOLUTE CASES ----------------------------------------------------------
number_cases_plot <- ggplot(data = all_cases, aes(x = day_index, y = cases, group = city, color = city)) +
  geom_line(size = 1.2) +
  xlab("Day index") +
  ylab("Number of cases") +
  xlim(0, 80) +
  ggtitle ("Number of daily cases") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.9, 0.5),
        legend.text = element_text(size = 19),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold", vjust = -0.1),
        axis.title.y = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 20, face="bold"),
        plot.margin = unit(c(0,0,0.5,0), 'lines')) +
  coord_cartesian(xlim = c(0, 80), ylim = c(5,max(all_cases$cases)-5)) +
  scale_color_discrete(breaks = c('aalborg', 'cph', 'korsoer'),
                       labels = c('Aalborg', 'Copenhagen', 'Korsør'))
number_cases_plot

ggsave(filename = 'Output\\F2-daily-incidence-absolute.jpg',
       plot = number_cases_plot,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)


# INCIDENCE OVER NORMALIZED DAY_INDEX -------------------------------------

norm_cases_plot <- ggplot(data = all_cases, aes(x = day_norm, y = cases_norm, group = city, color = city)) +
  geom_line(size = 1.2) +
  xlab("Normalized day index") +
  ylab("Incidence per 10,000") +
  xlim(0, 1) +
  ggtitle ("Incidence per 10k people with\nnormalized time index\n") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.9, 0.5),
        legend.text = element_text(size = 19),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold", vjust = -0.1),
        axis.title.y = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 20, face="bold"),
        plot.margin = unit(c(0,0,0.5,0), 'lines')) +
  coord_cartesian(xlim = c(0, 1), ylim = c(5,max(all_cases$cases_norm)-5)) +
  scale_color_discrete(breaks = c('aalborg', 'cph', 'korsoer'),
                       labels = c('Aalborg', 'Copenhagen', 'Korsør'))
norm_cases_plot

ggsave(filename = 'Output\\F3-incidence-norm-time.jpg',
       plot = norm_cases_plot,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)



# BRANDHOLM ---------------------------------------------------------------
ggplot(data = all_cases_temp[all_cases_temp$city == "brandholm", ],
       aes(x = date, y = cases)) +
  geom_line(size = 1.2) +
  xlab("Date") +
  ylab("Number of cases")+
  ggtitle ("Daily cases in Brandholm 1850") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold", vjust = -0.1),
        axis.title.y = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 20, face="bold"),
        plot.margin = unit(c(0,0,0.5,0), 'lines'))


# TIME OF YEAR ------------------------------------------------------------


plot_season <- ggplot(data = all_cases,
       aes(x = season, y = cases_norm, group = city, color = city)) +
  geom_line(size= 1.2) +
  xlab("Date") +
  ylab("Incidence per 10,000") +
  ggtitle ("Seasonality of cholera") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.5),
        legend.text = element_text(size = 17),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold", vjust = -0.1),
        axis.title.y = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 20, face="bold"),
        plot.margin = unit(c(0,0,0.5,0), 'lines')) +
  coord_cartesian(ylim = c(5,max(all_cases$cases_norm))) +
  scale_color_discrete(breaks = c('aalborg', 'copenhagen', 'korsoer'),
                       labels = c('Aalborg (1853)', 'Copenhagen (1853)', 'Korsør (1857)'))
  
plot_season


ggsave(filename = 'Output\\F4-seasonality.jpg',
       plot = plot_season,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)
