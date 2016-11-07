# Author: Matthew Phelps
# Desc: Comparing the 3 cities' outbreals
# Output datasets: Rdata in local directory

# INTRO -------------------------------------------------------------------
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       plot.path <- "C:/Users/wrz741/Google Drive/Copenhagen/Conferences/ASTMH Novermber 2016",
       plot.path <-"/Users/Matthew/Google Drive/Copenhagen/Conferences/ASTMH Novermber 2016")


ifelse(grepl("wrz741", getwd()),
       fn.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1",
       fn.path <-"/Users/Matthew/GitClones/Cholera-DK-paper1")


ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/data",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/data")

graphics.off()
devtools::install_github('matthew-phelps/CholeraDataDK', force = F)

library(ggplot2)
library(dplyr)
library(grid)
library(tidyr)
library(CholeraDataDK)
library(epitools)
library(scales) # required for some ggplot functions
library(gridExtra) # required to remove empty plots in facet_wrap()

setwd(fn.path)
source("functions.R") # required for rotating text in ggplot 
setwd(data.path)

# LOAD --------------------------------------------------------------------

load("data-viz-prep.Rdata")
all_cases_temp <- cholera_daily_data
mort <- cph_mort_rates_10yr
pop <- cph_pop1853_10yr
attack <- cph_age_attack_rate
cases <- cholera_daily_data_towns
cen <- census

rownames(cen) <- NULL

# Re-order levels of factor so that plotting works: http://goo.gl/CD2fEC
age_char <- as.character(mort$age_range)
mort$age_range <- factor(age_char, levels = c(age_char))

# To long format data
mort_gender <- gather(mort[, c("age_range", "male_mort2", "female_mort2")],
                      key = gender,
                      value = mort_rate,
                      c(male_mort2, female_mort2))

# 1 - FACET PLOT MONTHLY ALL_CAUSE MORTALITY  ---------------------------------

# To shade 1853, need a rectangle that covers only this year for all y
yrng <- range(all_monthly_mort$mortality[all_monthly_mort$age != "total"])
start_53 <- as.Date("1853-06-01") # start of shading
end_53 <- as.Date("1853-10-01") # End of shading

allcause_plot <- ggplot(data = all_monthly_mort[all_monthly_mort$age != "total", ]) +
  geom_line(size = 1.5,
            aes(x = date, y = mortality,
                group = age, color = age, linetype = age)) +
  # Color code the years - annotate is easier for some reasons:
  # http://goo.gl/7snZ8T
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = start_53, xmax = end_53,
           ymin = yrng[1], ymax = yrng[2])+
  facet_wrap(~ area, switch = "x") +
  xlab("") +
  ylab("All-cause mortality counts") +
  scale_x_date(date_breaks = "6 month", date_labels = "%b %Y")+
  scale_y_continuous(breaks = seq(0, 1250, 250)) +
  
  scale_color_manual(name = "Age group",
                     breaks = c("<10", "10-25", "26-50", "50+"),
                     values = c("red2", "black", "blue2", "green3" ))+
  scale_linetype_manual(name = "Age group",
                        breaks = c("<10", "10-25", "26-50", "50+"),
                        values = c("longdash",  "dotted", "twodash", "solid"  ))+
  #ggtitle ("Monthly all-cause mortality") +
  theme_classic() +
  theme(legend.title = element_text(size = 24, face = "bold"),
        legend.position = c(0.5, 0.6),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size = 24, angle = 35, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 24),
        axis.title.x = element_text(size = 24, margin = margin(0,0,0,0)),
        axis.title.y = element_text(size = 28, face = "bold",
                                    margin = margin(0,10,0,0)),
        plot.margin = unit(c(0,0.3,-0.3,0.5), 'lines'),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 26, face = "bold"))+
  # increase size of line symbol on legand. 
  guides(color = guide_legend(keywidth = 2.0, keyheight = 1.8,
                              override.aes = list(size = 1.8))) 

allcause_plot

# 1 - SAVE ----------------------------------------------------------------



setwd(plot.path)
ggsave(filename = "1-monthly all-cause mort.tiff",
       plot = allcause_plot,
       width = 32,
       height = 20,
       units = 'cm',
       dpi = 300)


# 2 - DAILY INCIDENCE PER 10K ----------------------------------------------------

all_cases_temp <- all_cases_temp[order(all_cases_temp$city, all_cases_temp$date), ]

# To plot all epidemics on same calendar year - put them on a "dummy" year
all_cases_temp$season <- paste("100",
                               format(all_cases_temp$date, "%m"),
                               format(all_cases_temp$date, "%d"),
                               sep = "-")
all_cases_temp$season <- as.Date(all_cases_temp$season)
all_cases <- all_cases_temp[all_cases_temp$city != "brandholm", ]
lab_x <- min(all_cases$season)
lab_size <- 10

plot_season <- ggplot(data = all_cases,
                      aes(x = season, y = cases_norm,
                          group = city, color = city)) +
  geom_line(size= 1.5) +
  annotate("text", x = lab_x + 15, y = 14, ymax = 10,
           color="#006DDB", label="Copenhagen", size = lab_size) + 
  annotate("text", x = lab_x + 55, y = 35, ymax = 10,
           color="#E69F00", label="Aalborg", size = lab_size) +
  annotate("text", x = lab_x + 76, y = 100, ymax = 10,
           color="green4", label="Korsør (1857)", size = lab_size) +
  xlab("Date") +
  ylab("Incidence per 10,000 people") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        axis.title.x = element_text(size = 28, face = "bold",
                                    margin = margin(15,0,0,0)),
        axis.title.y = element_text(size = 28, face = "bold",
                                    margin = margin(0,10,0,0)),
        plot.margin = unit(c(0.8,0.3,0.2,1.0), 'lines')) +
  coord_cartesian(ylim = c(5,max(all_cases$cases_norm))) +
  scale_color_manual(breaks = c('copenhagen', 'aalborg', 'korsoer'),
                     values = c("#E69F00","#006DDB", "green4"))
plot_season

setwd(plot.path)
ggsave(filename = "2-seasonality-temp.tiff",
       plot = plot_season,
       width = 32,
       height = 20,
       units = 'cm',
       dpi = 300)




# 3 - Total mortality & total attack rate due to cholera -----------------------------------------
# DF with cholera mort as % of total mort & cholera mort rates in 1 df
limits <- aes(ymax = upper95, ymin = lower95,
              x= age_range, color = plot_var2)
dodge <- position_dodge(width=- 0.3)
dodge2 <- position_dodge(width=- 0.5)


# In order to makes space between cities in legend, need to plot empty "dummy" variables that will be white space. Creat empty factors:
chol_burden$plot_var2 <- chol_burden$plot_var
chol_burden$plot_var2 <- factor(chol_burden$plot_var2,
                                levels = c("korsoer.total_attack", 
                                           "korsoer.total_mort_rate",
                                           "a",
                                           "aalborg.total_attack",
                                           "aalborg.total_mort_rate",
                                           "b",
                                           "cph.total_attack",
                                           "cph.total_mort_rate"))


plot_chol_mort <- ggplot() +
  # Style the age 0 - 70 +. Will style the "total" group separately
  geom_line(data = chol_burden[chol_burden$age_range != "Total", ],
            position = dodge,
            aes(x = age_range, y = pe, group = plot_var2, color = plot_var2),
            size = 1.4, alpha = 0.9) +
  geom_point(data = chol_burden[chol_burden$age_range != "Total", ], 
             position = dodge,
             aes(x = age_range, y = pe, group = plot_var2, color = plot_var2,
                 shape = plot_var2),  size = 4.8) +
  geom_errorbar(data = chol_burden[chol_burden$age_range != "Total", ],
                limits, position = dodge, width = 0.4, size = 1) +
  
  # Same but only for the "Total" group
  geom_point(data = chol_burden[chol_burden$age_range == "Total", ], 
             position = dodge2,
             aes(x = age_range, y = pe, group = plot_var2, color = plot_var2,
                 shape = plot_var2),  size = 5.5) +
  geom_errorbar(data = chol_burden[chol_burden$age_range == "Total", ],
                limits, position = dodge2, size = 1.0, width = 0.4) +
  
  # facet wrap and put labels below x-axis (swtich="x"). Use city2 var to plot
  # empty plot so that bottom row is filled
  #facet_wrap(~ city2, ncol = 2, switch = "x", drop = FALSE) +
  
  # Create symbology
  scale_color_manual(drop = FALSE, # stops R gropping empty "dummy" factors
                     values = c("green3",  "green4", "grey100",
                                "#f4a700", "#af7a07", "grey100",
                                "steelblue1" , "royalblue4"),
                     breaks = c('korsoer.total_attack', 'korsoer.total_mort_rate', 
                                "a",
                                'aalborg.total_attack', 'aalborg.total_mort_rate',
                                "b",
                                'cph.total_attack', 'cph.total_mort_rate'),
                     labels = c('Korsør attack rate', 'Korsør mortality rate', " ",
                                'Aalborg attack rate', 'Aalborg mortality rate', "",
                                'Copenhagen attack rate', 'Copenhagen mortality rate'))+
  
  scale_shape_manual(drop = FALSE, # stops R gropping empty "dummy" factors
                     values = c(16, 17, 33,
                                16, 17, 32,
                                16, 17),
                     breaks = c('korsoer.total_attack', 'korsoer.total_mort_rate', "a",
                                'aalborg.total_attack', 'aalborg.total_mort_rate', "b",
                                'cph.total_attack', 'cph.total_mort_rate'),
                     labels = c('Korsør attack rate', 'Korsør mortality rate', " ",
                                'Aalborg attack rate', 'Aalborg mortality rate', "",
                                'Copenhagen attack rate', 'Copenhagen mortality rate'))+
  
  # guides(shape = guide_legend(keywidth = 2.5,
  #                             keyheight = 3)) + # Removes color part of legend
  
  xlab("Age group") +
  ylab("Rate per 100 people") +
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(x = 0.25, y = .74),
        legend.text = element_text(size = 22),
        legend.key.height = unit(0.6, "cm"),
        axis.text.x = element_text(size = 22,
                                   angle = 45, vjust = 1.5, hjust = 1.0,
                                   margin = margin(0,0,-10,0)),
        axis.text.y = element_text(size = 24),
        axis.title.x = element_text(size = 26, face = "bold",
                                    margin = margin(0, 0, 4, 0)),
        axis.title.y = element_text(size = 26, face = "bold",
                                    margin = margin(0,10,0,0)),
        plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines')) +
  guides(color = guide_legend(keywidth = 2.0, keyheight = 1.8)) 

plot_chol_mort

setwd(plot.path)
ggsave(filename = "3-cholera-mort-rate.jpg",
       plot = plot_chol_mort,
       width = 32,
       height = 20,
       units = 'cm',
       dpi = 300)

# ## Use grob to remove empty panels from plot. If you work with another set of
# ## plots, look at the output of names(g$grobs) and g$layout$name to figure out,
# ## which elements have to be removed. https://goo.gl/4AK5Ey
# g <- ggplotGrob(plot_chol_mort)
# 
# # remove empty panels
# g$grobs[names(g$grobs) %in% c("panel2", "strip_t2")] <- NULL
# 
# # remove panel from layout
# g$layout <- g$layout[!(g$layout$name %in% c('panel-2', 'strip_t-2')), ]
# 
# # move axis closer to panel
# g$layout[g$layout$name == "axis_b-9", c("t", "b")] = c(9,9)
# 
# # plot output
# 
# grid.newpage()
# grid.draw(g)
# ggsave(filename = "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Output/cholera-mort-rate.jpg",
#        plot = g,
#        width = 26,
#        height = 20,
#        units = 'cm',
#        dpi = 600)





# 4 - Cholera Deaths -------------------------------------------------
limits = aes(ymax = hi95, ymin = low95, x = age_range)

plot_chol_pct <- ggplot() +
  geom_line(data = cho_pct[1:8, ], aes(x = age_range, y = mortality, group = 1),
            size = 1.1) +
  geom_point(data = cho_pct[1:8, ], aes(x = age_range, y = mortality),
             size = 3.1) +
  geom_errorbar(data = cho_pct[cho_pct$age_range !="Total", ],
                limits,
                width = 0.3,
                size = 0.8) +
  geom_point(data = cho_pct[9, ], aes(x = age_range, y = mortality),
             size = 3.5, color = "red3") +
  geom_errorbar(data = cho_pct[cho_pct$age_range =="Total", ],
                limits,
                width = 0.3,
                size = 0.8, color = "red3") +
  xlab("Age group") +
  ylab("Proportion of all annual deaths\nattributed to cholera") +
  scale_y_continuous(breaks = seq(0, 0.8, 0.2),
                     limits = c(0, 0.8)) + 
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 0.9),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15,
                                    margin = margin(0,20,0,0)),
        plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines'))
plot_chol_pct

setwd(main.path)
ggsave(filename = "Output/4-cholera-of-total.jpg",
       plot = plot_chol_pct,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)


# 5 - ATTACK RELATIVE RISK DATA ---------------------------------------
limits = aes(ymax = up95, ymin = low95, color = city, x = age_range)
title <- "Relative risk of cholera morbidity by age & gender \n"
notes <- "*male is reference group"
pd <- position_dodge(0.4)
pd2 <- position_dodge(0.6)

# To specify that CPH is plotted first changed factors
rr_sic$city <- factor(rr_sic$city, levels = c("cph", "aalborg", "korsoer"))
levels(rr_sic$city) <- c("Copenhagen", "Aalborg", "Korsør (1857)")

# 5 ATTACK RR PLOT --------------------------------------------------------

plot_attack <- ggplot() +
  geom_point(data = rr_sic[rr_sic$age_range !="Total", ],
             aes(x = age_range,
                 y = rr,
                 color = city,
                 shape = city),
             position = pd,
             size = 2.3) +
  
  geom_errorbar(data = rr_sic[rr_sic$age_range !="Total", ],
                limits,
                width = 0.3,
                size = 0.8,
                position = pd) +
  # Separate series for the "Total" RR so that it can have different styling
  geom_point(data = rr_sic[rr_sic$age_range =="Total", ],
             aes(x = age_range,
                 y = rr,
                 color = city,
                 shape = city),
             position = pd2,
             size = 3.0) +
  geom_errorbar(data = rr_sic[rr_sic$age_range =="Total", ],
                limits,
                width = 0.4,
                size = 1.3,
                position = pd2) +
  coord_cartesian(ylim = c(0, 4)) +
  scale_color_manual(values = c("#E69F00","#006DDB", "green4"),
                     breaks = c('Copenhagen', 'Aalborg', 'Korsør (1857)'),
                     labels = c('Copenhagen', 'Aalborg', "Korsør (1857)")) +
  scale_shape_manual(values = c(8, 17, 15),
                     breaks = c('Copenhagen', 'Aalborg', 'Korsør (1857)'),
                     labels = c('Copenhagen', 'Aalborg', "Korsør (1857)"))  +
  geom_hline(yintercept = 1) +
  xlab("") +
  ylab("Relative risk of cholera infection") +
  # annotate("text", x = 8.5, y = 4.0, label = notes) +
  # ggtitle (title)  +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(x = 0.88, y = .8),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1.4, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15,
                                    vjust = -0.1),
        axis.title.y = element_text(size = 15,
                                    margin = margin(0,20,0,0)),
        plot.title = element_text(size = 16),
        plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines'))
plot_attack
setwd(main.path)
# ggsave(filename = "Output/6-RR-attack.jpg",
#        plot = plot_attack,
#        width = 26,
#        height = 20,
#        units = 'cm',
#        dpi = 600)

# 6 - DEATH RELATIVE RISK DATA--------------------------------------------------
# set limits for error bars: http://goo.gl/4QE74U
limits = aes(ymax = up95, ymin = low95, x = age_range, color = city)
notes <- "*male is reference group"
title <- "Relative risk of cholera mortality by age & gender \n"
pd <- position_dodge(0.4)
pd2 <- position_dodge(0.6)

# To specify that CPH is plotted first changed factors
rr_mrt$city <- factor(rr_mrt$city, levels = c("cph", "aalborg", "korsoer"))
levels(rr_mrt$city) <- c("Copenhagen", "Aalborg", "Korsør (1857)")

# 6 - DEATH RR PLOT -------------------------------------------------------

plot_mort <- ggplot() +
  geom_point(data = rr_mrt[rr_mrt$age_range != "Total", ],
             aes(x = age_range,
                 y = rr,
                 color = city,
                 shape = city),
             position = pd, size = 2.5) +
  geom_errorbar(data = rr_mrt[rr_mrt$age_range != "Total", ],
                limits,
                width = 0.3,
                size = 0.8,
                position = pd) +
  # Separate series for the "Total" RR so that it can have different styling
  geom_point(data = rr_mrt[rr_mrt$age_range == "Total", ],
             aes(x = age_range,
                 y = rr,
                 color = city,
                 shape = city),
             position = pd2, size = 3.0) +
  geom_errorbar(data = rr_mrt[rr_mrt$age_range == "Total", ],
                limits,
                width = 0.4,
                size = 1.3,
                position = pd2) +
  coord_cartesian(ylim = c(0, 4)) +
  scale_color_manual(values = c("#E69F00","#006DDB", "green4"),
                     breaks = c('Copenhagen', 'Aalborg', 'Korsør (1857)'),
                     labels = c('Copenhagen', 'Aalborg', "Korsør (1857)")) +
  scale_shape_manual(values = c(8, 17, 15),
                     breaks = c('Copenhagen', 'Aalborg', 'Korsør (1857)'),
                     labels = c('Copenhagen', 'Aalborg', "Korsør (1857)")) +
  geom_hline(yintercept = 1) +
  xlab("Age") +
  ylab("Relative risk of cholera death") +
  # annotate("text", x = 8.5, y = 4.0, label = notes) +
  #ggtitle (title)  +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(x = 0.88, y = .8),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1.4, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15,
                                    vjust = -0.1),
        axis.title.y = element_text(size = 15,
                                    margin = margin(0,20,0,0)),
        plot.title = element_text(size = 16),
        plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines'))
plot_mort

# ggsave(filename = "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Output/5-RR-mortality.jpg",
#        plot = plot_mort,
#        width = 26,
#        height = 20,
#        units = 'cm',
#        dpi = 600)



#        

# MULTIPLOT ---------------------------------------------------------------

setwd(main.path)
jpeg(filename = "Output/5-RR-gender.jpg",
     width = 20,
     height = 26,
     units = "cm",
     quality = 100,
     res = 600)
multiplot(plot_attack, plot_mort, cols = 1)
dev.off()










