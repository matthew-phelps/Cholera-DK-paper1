deathsBasePlot <- function(x){
  ggplot(data = x) +
    # Color code the years - annotate is easier for some reasons:
    # http://goo.gl/7snZ8T
    # annotate("rect", fill = "grey", alpha = 0.3,
    #          xmin = start_53, xmax = end_53,
    #          ymin = yrng[1], ymax = yrng[2]) +
    geom_line(size = 1.2,
              aes(x = date, y = dead,
                  group = cause, color = cause, linetype = cause))
  
  # 
}

deathPoints <- function(death_base, p_size){
  death_base +
    geom_point(size = p_size + 2.5,
               color = "white",
               aes(x = date, y = dead,
                   group = cause)) +
    geom_point(size = p_size,
               aes(x = date, y = dead,
                   group = cause, color = cause))
  
  # Overplot white points for effect
}

deathsStyles <- function(deaths_base){
  deaths_base + 
    xlab("Copenhagen") +
    ylab("Mortality counts") +
    scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")+
    scale_y_continuous(breaks = seq(0, 4000, 1000)) +
    
    scale_color_manual(name = "Cause of death",
                       labels = c("All", "Cholera", "Diarrhea"),
                       values = c("orange3", "dodgerblue4", "green4"))+
    scale_linetype_manual(name = "Cause of death",
                          labels = c("All", "Cholera", "Diarrhea"),
                          values = c("twodash", "solid", "solid"))+
    #ggtitle ("Monthly all-cause mortality") +
    theme_classic() +
    theme(legend.title = element_text(size = 16, face = "bold"),
          legend.position = c(0.82, 0.6),
          legend.text = element_text(size = 16),
          axis.text.x = element_text(size = 16, angle = 35, hjust = 1, vjust = 1),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 16,
                                      margin = margin(5,0,0,0)),
          axis.title.y = element_text(size = 16,
                                      margin = margin(0,10,0,0)),
          plot.margin = unit(c(0,0.3,0.5,0.5), 'lines'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 14, face = "bold"))+
    # increase size of line symbol on legand. 
    guides(color = guide_legend(keywidth = 2.8, keyheight = 1.8,
                                override.aes = list(size = 1.5))) 
}




all_cause_style <- function(allcause_base){
  allcause_base + 
    # Color code the years - annotate is easier for some reasons:
    # http://goo.gl/7snZ8T
    annotate("rect", fill = "grey", alpha = 0.3,
             xmin = start_53, xmax = end_53,
             ymin = yrng[1], ymax = yrng[2]) +
    xlab("Copenhagen") +
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
    theme(legend.title = element_text(size = 16, face = "bold"),
          legend.position = c(0.82, 0.6),
          legend.text = element_text(size = 16),
          axis.text.x = element_text(size = 16, angle = 35, hjust = 1, vjust = 1),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 16,
                                      margin = margin(5,0,0,0)),
          axis.title.y = element_text(size = 16,
                                      margin = margin(0,10,0,0)),
          plot.margin = unit(c(0,0.3,0.5,0.5), 'lines'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 14, face = "bold"))+
    # increase size of line symbol on legand. 
    guides(color = guide_legend(keywidth = 2.8, keyheight = 1.8,
                                override.aes = list(size = 1.8))) 
}


plotSeason <- function(cases_all, lab_size = 6, lab_x) {
  ggplot() +
    geom_line(data = cases_all,
              aes(x = season, y = cases_norm,
                  group = city, color = city),
              size= 1.1) +
    annotate("text", x = lab_x + 15, y = 14,
             color="#E69F00", label="Copenhagen", size = lab_size) + 
    annotate("text", x = lab_x + 56, y = 28,
             color="#006DDB", label="Aalborg", size = lab_size) +
    annotate("text", x = lab_x + 105, y = 125,
             color="green4", label="Korsør 1857", size = lab_size) +
    xlab("Date") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 15, vjust = -0.1),
          axis.title.y = element_text(size = 15,
                                      margin = margin(0,20,0,0)),
          plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines')) +
    coord_cartesian(ylim = c(5,max(all_cases$cases_norm))) +
    scale_y_continuous("Incidence per 10,000 people",
                       sec.axis = sec_axis(~. * 1, name = "Mortality counts (London)")) +
    scale_color_manual(breaks = c('copenhagen', 'aalborg', 'korsoer'),
                       values = c("#006DDB", "#E69F00", "green4"))
}
addBroadSt <- function(old_plot, lab_size = 6, lab_x){
  old_plot + geom_line(data = broad_st,
                       aes(x = season, y = deaths),
                       group = 1,
                       size = 1.1,
                       color = "grey") +
    annotate("text", x = lab_x + 67, y = 110,
             color="gray", label="London 1854 \n (mortality counts)", size = lab_size) 
}



cholMortPlot <- function(p_size){
  ggplot() +
    # Style the age 0 - 70 +. Will style the "total" group separately
    geom_line(data = chol_burden[chol_burden$age_range != "Total", ],
              position = dodge,
              aes(x = age_range, y = pe, group = plot_var2, color = plot_var2),
              size = 0.8, alpha = 0.9) +
    
    geom_point(data = chol_burden[chol_burden$age_range != "Total", ], 
               position = dodge,
               color = "white",
               aes(x = age_range, y = pe, group = plot_var2,
                   shape = plot_var2),  size = p_size) +
    
    geom_point(data = chol_burden[chol_burden$age_range != "Total", ], 
               position = dodge,
               aes(x = age_range, y = pe, group = plot_var2, color = plot_var2,
                   shape = plot_var2),  size = 2.8) +
    geom_errorbar(data = chol_burden[chol_burden$age_range != "Total", ],
                  limits, position = dodge, width = 0.2) +
    
    # Same but only for the "Total" group
    geom_point(data = chol_burden[chol_burden$age_range == "Total", ], 
               position = dodge2,
               aes(x = age_range, y = pe, group = plot_var2, color = plot_var2,
                   shape = plot_var2),  size = 3.5) +
    geom_errorbar(data = chol_burden[chol_burden$age_range == "Total", ],
                  limits, position = dodge2, size = 1.0, width = 0.4)
}
mortPlotStyle <- function(base_chol_mort){
  # Create symbology
  base_chol_mort + 
    scale_color_manual(drop = FALSE, # stops R gropping empty "dummy" factors
                       values = c("green3",  "green4", "grey100",
                                  "steelblue1", "royalblue4", "grey100",
                                  "#f4a700" , "#af7a07"),
                       breaks = c('korsoer.total_attack', 'korsoer.total_mort_rate', 
                                  "a",
                                  'aalborg.total_attack', 'aalborg.total_mort_rate',
                                  "b",
                                  'cph.total_attack', 'cph.total_mort_rate'),
                       labels = c('Korsør morbidity rate', 'Korsør mortality rate', " ",
                                  'Aalborg morbidity rate', 'Aalborg mortality rate', "",
                                  'Copenhagen morbidity rate', 'Copenhagen mortality rate'))+
    
    scale_shape_manual(drop = FALSE, # stops R gropping empty "dummy" factors
                       values = c(16, 17, 33,
                                  16, 17, 32,
                                  16, 17),
                       breaks = c('korsoer.total_attack', 'korsoer.total_mort_rate', "a",
                                  'aalborg.total_attack', 'aalborg.total_mort_rate', "b",
                                  'cph.total_attack', 'cph.total_mort_rate'),
                       labels = c('Korsør morbidity rate', 'Korsør mortality rate', " ",
                                  'Aalborg morbidity rate', 'Aalborg mortality rate', "",
                                  'Copenhagen morbidity rate', 'Copenhagen mortality rate'))+
    
    # guides(shape = guide_legend(keywidth = 2.5,
    #                             keyheight = 3)) + # Removes color part of legend
    
    xlab("Age group") +
    ylab("Rate per 100 people\n") +
    coord_cartesian(ylim = c(0, 50)) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(x = 0.25, y = .74),
          legend.text = element_text(size = 14),
          legend.key.height = unit(0.6, "cm"),
          axis.text.x = element_text(size = 14,
                                     angle = 45, vjust = .8, hjust = 1.0,
                                     margin = margin(0,0,0,0)),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14,
                                      margin = margin(10, 0, 4, 0)),
          axis.title.y = element_text(size = 14,
                                      margin = margin(0,0,0,0)),
          plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines'))
}


cholProportionDeaths <- function() {
  ggplot() +
    geom_line(data = cho_pct[1:8, ], aes(x = age_range, y = mortality, group = 1),
              size = 1.1) +
    geom_point(data = cho_pct[1:8, ],
               color = "white",
               aes(x = age_range, y = mortality),
               size = 5.9) +
    geom_point(data = cho_pct[1:8, ], aes(x = age_range, y = mortality),
               size = 2.8) +

    
    geom_errorbar(data = cho_pct[cho_pct$age_range !="Total", ],
                  limits,
                  width = 0.2,
                  size = 0.8) +
    geom_point(data = cho_pct[9, ], aes(x = age_range, y = mortality),
               size = 3.5, color = "red3") +
    geom_errorbar(data = cho_pct[cho_pct$age_range =="Total", ],
                  limits,
                  width = 0.2,
                  size = 0.8, color = "red3") 
}

proportionDeathStyle <- function(base_prop_death){
  base_prop_death + 
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
}