deathsBasePlot <- function(x, l_size){
  ggplot(data = x) +
    # Color code the years - annotate is easier for some reasons:
    # http://goo.gl/7snZ8T
    # annotate("rect", fill = "grey", alpha = 0.3,
    #          xmin = start_53, xmax = end_53,
    #          ymin = yrng[1], ymax = yrng[2]) +
    geom_line(size = l_size,
              aes(x = date, y = dead,
                  group = cause, color = cause, linetype = cause))
  
  # 
}

deathPoints <- function(death_base, p_size, space = TRUE){
  if(space){
    death_base +
      geom_point(size = p_size + 2.5,
                 color = "white",
                 aes(x = date, y = dead,
                     group = cause)) +
      geom_point(size = p_size,
                 aes(x = date, y = dead,
                     group = cause, color = cause))
  } else {
    death_base +
      geom_point(size = p_size,
                 aes(x = date, y = dead,
                     group = cause, color = cause))
  }
  # Overplot white points for effect
}

deathsStyles <- function(deaths_base, txt_size){
  txt_size
  deaths_base + 
    xlab("Copenhagen") +
    ylab("Mortality counts") +
    scale_x_date(date_breaks = "4 month", date_labels = "%b %Y")+
    scale_y_continuous(breaks = seq(0, 4000, 1000)) +
    
    scale_color_manual(name = "Cause of death",
                       labels = c("All-cause", "Cholera", "Diarrhea"),
                       values = c("orange3", "dodgerblue4", "green4"))+
    scale_linetype_manual(name = "Cause of death",
                          labels = c("All-cause", "Cholera", "Diarrhea"),
                          values = c("twodash", "solid", "solid"))+
    #ggtitle ("Monthly all-cause mortality") +
    theme_classic() +
    theme(legend.title = element_text(size = txt_size, face = "bold"),
          legend.position = c(0.82, 0.6),
          legend.text = element_text(size = txt_size),
          axis.text.x = element_text(size = txt_size, angle = 35, hjust = 1, vjust = 1),
          axis.text.y = element_text(size = txt_size),
          axis.title.x = element_text(size = txt_size,
                                      margin = margin(5,0,0,0)),
          axis.title.y = element_text(size = txt_size,
                                      margin = margin(0,10,0,0)),
          plot.margin = unit(c(0.4,0.3,0.5,0.5), 'lines'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = txt_size-2, face = "bold"))
  # increase size of line symbol on legand. 
  # guides(color = guide_legend(keywidth = 2.8, keyheight = 1.8,
  #                             override.aes = list(size = 1.5))) 
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

SeasonPlot <- function(cases_all, lab_size = 6, lab_x, txt_size, line_size,
                       broad_street = FALSE, broad_street_data) {
  txt_size
  base_plot <- ggplot()
  if(broad_street){
    base_plot <- base_plot + 
      geom_line(data = broad_st,
                aes(x = season, y = deaths),
                group = 1,
                size = line_size,
                color = "grey") +
      annotate("text", x = lab_x + 67, y = 110,
               color="gray", label="London 1854 \n (mortality counts)", size = lab_size) 
  }
  
  base_plot <- base_plot +
    geom_line(data = cases_all,
              aes(x = season, y = cases_norm,
                  group = city, color = city),
              size= line_size) +
    annotate("text", x = lab_x + 15, y = 14,
             color="#E69F00", label="Copenhagen", size = lab_size) + 
    annotate("text", x = lab_x + 56, y = 28,
             color="#006DDB", label="Aalborg", size = lab_size) +
    annotate("text", x = lab_x + 105, y = 125,
             color="green4", label="Korsoer 1857", size = lab_size) +
    xlab("Date") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = txt_size),
          axis.text.x = element_text(size = txt_size),
          axis.text.y = element_text(size = txt_size),
          axis.title.x = element_text(size = txt_size, vjust = -0.1),
          axis.title.y = element_text(size = txt_size,
                                      margin = margin(0,10,0,0)),
          plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines')) +
    coord_cartesian(ylim = c(5,max(all_cases$cases_norm))) +
    scale_y_continuous("Incidence per 10,000 people",
                       sec.axis = sec_axis(~. * 1, name = "Mortality counts (London)")) +
    scale_color_manual(breaks = c('copenhagen', 'aalborg', 'korsoer'),
                       values = c("#006DDB", "#E69F00", "green4"))
  return(base_plot)
}

cholMortPlot <- function(p_size, line_size){
  ggplot() +
    # Style the age 0 - 70 +. Will style the "total" group separately
    geom_line(data = chol_burden[chol_burden$age_range != "Total", ],
              position = dodge,
              aes(x = age_range, y = pe, group = plot_var2, color = plot_var2),
              size = line_size, alpha = 0.9) +
    
    # geom_point(data = chol_burden[chol_burden$age_range != "Total", ], 
    #            position = dodge,
    #            color = "white",
    #            aes(x = age_range, y = pe, group = plot_var2,
    #                shape = plot_var2),  size = p_size) +
    # 
    geom_point(data = chol_burden[chol_burden$age_range != "Total", ], 
               position = dodge,
               aes(x = age_range, y = pe, group = plot_var2, color = plot_var2,
                   shape = plot_var2),  size = p_size) +
    geom_errorbar(data = chol_burden[chol_burden$age_range != "Total", ],
                  limits, position = dodge, width = 0.2) +
    
    # Same but only for the "Total" group
    geom_point(data = chol_burden[chol_burden$age_range == "Total", ], 
               position = dodge2,
               aes(x = age_range, y = pe, group = plot_var2, color = plot_var2,
                   shape = plot_var2),  size = 3) +
    geom_errorbar(data = chol_burden[chol_burden$age_range == "Total", ],
                  limits, position = dodge2, size = 1.0, width = 0.4)
}

mortPlotStyle <- function(base_chol_mort, txt_size){
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
                       labels = c('Korsoer morbidity rate', 'Korsoer mortality rate', " ",
                                  'Aalborg morbidity rate', 'Aalborg mortality rate', "",
                                  'Copenhagen morbidity rate', 'Copenhagen mortality rate'))+
    
    scale_shape_manual(drop = FALSE, # stops R gropping empty "dummy" factors
                       values = c(16, 17, 33,
                                  16, 17, 32,
                                  16, 17),
                       breaks = c('korsoer.total_attack', 'korsoer.total_mort_rate', "a",
                                  'aalborg.total_attack', 'aalborg.total_mort_rate', "b",
                                  'cph.total_attack', 'cph.total_mort_rate'),
                       labels = c('Korsoer morbidity rate', 'Korsoer mortality rate', " ",
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
          legend.text = element_text(size = txt_size),
          legend.key.height = unit(0.6, "cm"),
          axis.text.x = element_text(size = txt_size,
                                     angle = 45, vjust = .8, hjust = 1.0,
                                     margin = margin(0,0,0,0)),
          axis.text.y = element_text(size = txt_size),
          axis.title.x = element_text(size = txt_size,
                                      margin = margin(10, 0, 4, 0)),
          axis.title.y = element_text(size = txt_size,
                                      margin = margin(0,0,0,0)),
          plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines'))
}


cholProportionDeaths <- function(line_size) {
  ggplot() +
    geom_line(data = cho_pct[1:8, ], aes(x = age_range, y = mortality, group = 1),
              size = line_size) +
    geom_point(data = cho_pct[1:8, ],
               color = "white",
               aes(x = age_range, y = mortality),
               size = 5.9) +
    
    geom_point(data = cho_pct[1:8, ], aes(x = age_range, y = mortality),
               size = 2.8) +
    
    geom_errorbar(data = cho_pct[cho_pct$age_range !="Total", ],
                  limits,
                  width = 0.2,
                  size = 0.7) +
    geom_point(data = cho_pct[9, ], aes(x = age_range, y = mortality),
               size = 3.0, color = "red3") +
    
    geom_errorbar(data = cho_pct[cho_pct$age_range =="Total", ],
                  limits,
                  width = 0.2,
                  size = 0.7, color = "red3") 
}

proportionDeathStyle <- function(base_prop_death, txt_size){
  base_prop_death + 
    xlab("Age group") +
    ylab("Proportion of all annual deaths\nattributed to cholera") +
    scale_y_continuous(breaks = seq(0, 0.8, 0.2),
                       limits = c(0, 0.8)) + 
    theme_classic() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = txt_size, angle = 45, vjust = 0.5),
          axis.text.y = element_text(size = txt_size),
          axis.title.x = element_text(size = txt_size),
          axis.title.y = element_text(size = txt_size,
                                      margin = margin(0,10,0,0)),
          plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines'))
}

rrAgeGender <- function(rr_data, pd, pd2, line_size){
  force(rr_data)
  pd <- position_dodge(pd)
  pd2 <- position_dodge(pd2)
  
  # Set levels to set proper plotting order
  rr_data$city <- factor(rr_data$city, levels = c("cph", "aalborg", "korsoer"))
  levels(rr_data$city) <- c("Copenhagen", "Aalborg", "Korsoer (1857)")
  
  # set limits for error bars: http://goo.gl/4QE74U
  limits = aes(ymax = up95, ymin = low95, color = city, x = age_range)
  x <- "age_range"
  
  # To specify that CPH is plotted first -> change factors
  
  
  
  ggplot() +
    geom_point(data = rr_data[rr_data[[x]] != "Total", ],
               aes(x = age_range,
                   y = rr,
                   color = city,
                   shape = city),
               position = pd,
               size = 2.3) +
    
    geom_errorbar(data = rr_data[rr_data$age_range !="Total", ],
                  limits,
                  width = 0.3,
                  size = line_size,
                  position = pd) +
    # Separate series for the "Total" RR so that it can have different styling
    geom_point(data = rr_data[rr_data$age_range =="Total", ],
               aes(x = age_range,
                   y = rr,
                   color = city,
                   shape = city),
               position = pd2,
               size = 3.0) +
    geom_errorbar(data = rr_data[rr_data$age_range =="Total", ],
                  limits,
                  width = 0.4,
                  size = line_size,
                  position = pd2)
}

rrAgeGenderStyle <- function(base_plot, txt_size, ylabel,
                             x_title = FALSE){
  out <- base_plot + 
    coord_cartesian(ylim = c(0, 4)) +
    scale_color_manual(values = c("orange2","dodgerblue4", "green4"),
                       breaks = c('Copenhagen', 'Aalborg', 'Korsoer (1857)'),
                       labels = c('Copenhagen', 'Aalborg', "Korsoer (1857)")) +
    scale_shape_manual(values = c(8, 17, 15),
                       breaks = c('Copenhagen', 'Aalborg', 'Korsoer (1857)'),
                       labels = c('Copenhagen', 'Aalborg', "Korsoer (1857)"))  +
    geom_hline(yintercept = 1) +
    xlab("") +
    ylab(paste("Relative risk \nof cholera",ylabel, sep = " ")) +
    # annotate("text", x = 8.5, y = 4.0, label = notes) +
    # ggtitle (title)  +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(x = 0.88, y = .8),
          legend.text = element_text(size = txt_size),
          axis.text.x = element_text(size = txt_size, angle = 45, vjust = 1.3, hjust = 1.5),
          axis.text.y = element_text(size = txt_size),
          axis.title.x = element_text(size = txt_size,
                                      vjust = -0.1),
          axis.title.y = element_text(size = txt_size,
                                      margin = margin(0,10,0,0)),
          plot.title = element_text(size = txt_size),
          plot.margin = unit(c(0,0.3,0.1,0.5), 'lines'))
  if(x_title){
    out <- out +
      xlab("Age range")
    
  }
  return(out)
}

r0Plot <- function(r0_data, pd, line_size){
  r0_data
  pd <- position_dodge(pd)
  
  ggplot(data = r0_data,
         aes(x = city, y = pe, color = method)) +
    geom_point(position = pd, aes(shape = method),
               size = 3) +
    geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                  width = 0.15,
                  size = line_size,
                  position = pd) +
    coord_cartesian(ylim = c(0, max(r0_data$ci_u)))
}

r0PlotStyle <- function(base_plot, txt_size){
  base_plot + 
    xlab("Town") + 
    ylab(expression(Repdocutive~number~"("*R[0]*")")) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.89),
          legend.text = element_text(size = txt_size),
          axis.text.x = element_text(size = txt_size,
                                     margin = margin(5,0,10,0)),
          axis.text.y = element_text(size = txt_size,
                                     margin = margin(0, 2, 0, 0)),
          axis.title.x = element_text(size = txt_size,
                                      margin = margin(0, 0, 4, 0)),
          axis.title.y = element_text(size = txt_size,
                                      margin = margin(0,10,0,0)),
          plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines')) +
    
    scale_color_manual(values = c("orange2", "dodgerblue4"),
                       breaks = c("EG", "ML"),
                       labels = c("Exponential \nGrowth\n",
                                  "Maximum \nLikelihood")) +
    scale_shape_manual(values = c(19, 17),
                       breaks = c("EG", "ML"),
                       labels = c("Exponential \nGrowth\n",
                                  "Maximum \nLikelihood")) +
    guides(color = guide_legend(keywidth = 2.3, keyheight = 1.8)) 
  
  
}


quarterPanelIncidence <- function(combined) {
  ggplot (combined,
          aes( x = week.id,
               y = sick.total.week / est.pop.1853*1000,
               group = quarter))+
    geom_line(size = 1) +
    geom_vline( xintercept = 5, linetype = 2, color = "black", alpha = 0.5) +
    facet_wrap(~quarter) +
    xlab("Week index") +
    ylab("Incidence per 1000 people") +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12, vjust = -0.1),
          axis.title.y = element_text(size = 12, vjust = 0.5),
          strip.background = element_rect(color = '#F0F0F0', fill = '#F0F0F0')) 
}
