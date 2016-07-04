rm(list = ls())
library(devtools)
install_github("matthew-phelps/CholeraDataDK")
library(CholeraDataDK)
library(R0)
library(ggplot2)


# LOAD --------------------------------------------------------------------
cases <- cholera_daily_data
aalborg <- cases[cholera_daily_data$city == "aalborg", ]
korsoer <- cases[cholera_daily_data$city == "korsoer", ]
cph <- cases[cholera_daily_data$city == "copenhagen", ]

# Get generation time: 10.1371/journal.pntd.0001901
mGT <- generation.time("lognormal", c(2.4, 2))
# Check generation time
# plot(mGT)



# Aalborg------------------------------------------------------------
# Sensitivity
sens_time <-sensitivity.analysis(aalborg$cases, mGT, begin = 1:6, end = 15:20,
                                 est.method = c("EG", "ML"), sa.type = "time")
# plot(sens_time)
 sens_time <-sensitivity.analysis(aalborg$cases, mGT, begin = 1:6, end = 15:20,
                                  est.method = "ML", sa.type = "time")
 
# plot(sens_time, what = "criterion")

aal <- estimate.R(aalborg$cases, GT = mGT, t = aalborg$day_index, begin = 1, end = 16, methods = c("EG", "ML", "TD"), nsim = 1500)

# Put R0 and CI into data frame so can plot each citys estimates on one graph
# For TD we average over the exponential growth phase
pe <- c(aal$estimates$EG$R, aal$estimates$ML$R, mean(aal$estimates$TD$R))
ci_l<- c(aal$estimates$EG$conf.int[1],
         aal$estimates$ML$conf.int[1],
         mean(aal$estimates$TD$conf.int[[1]]))
ci_u <- c(aal$estimates$EG$conf.int[2],
          aal$estimates$ML$conf.int[2],
          mean(aal$estimates$TD$conf.int[[2]]))
method <- c("EG", "ML", "TD")
r0 <- data.frame(pe, ci_l, ci_u, method)
r0$city <- "Åalborg"
rm(aal, ci_u, ci_l, method, pe)


# Generation time Sensitivity
sens_GT <-sensitivity.analysis(aalborg$cases,
                             GT.type = "lognormal",
                             GT.mean = seq(3,7,0.5),
                             GT.sd.range = 2,
                             begin = 1, end = 17,
                             est.method = "EG",
                              sa.type = "GT")
# plot(x=sens_GT[,"GT.Mean"],
#      xlab="mean GT (days)",
#      y=sens_GT[,"R"],
#      ylim=c(1.2, 4.1),
#      ylab="R0 (95% CI)",
#      type="p", pch=19,
#      col="black",
#      main="Sensitivity of R0 to mean Generation Time")
# arrows(x0=as.numeric(sens_GT[,"GT.Mean"]),
#        y0=as.numeric(sens_GT[,"CI.lower"]),
#        y1=as.numeric(sens_GT[,"CI.upper"]),
#        angle=90,
#        code=3,
#        col="black",
#        length=0.05)


# KORSOER -----------------------------------------------------------------

sens_time <-sensitivity.analysis(korsoer$cases, mGT, begin = 1:5, end = 14:18,
                                 est.method = "EG", sa.type = "time")
# plot(sens_time)
kor <- estimate.R(korsoer$cases, GT = mGT, t = korsoer$day_index, begin = 1, end = 15, methods = c("EG", "ML", "TD"), nsim = 1500)

# plot(kor)
pe <- c(kor$estimates$EG$R, kor$estimates$ML$R, mean(kor$estimates$TD$R))
ci_l<- c(kor$estimates$EG$conf.int[1],
         kor$estimates$ML$conf.int[1],
         mean(kor$estimates$TD$conf.int[[1]]))
ci_u <- c(kor$estimates$EG$conf.int[2],
          kor$estimates$ML$conf.int[2],
          mean(kor$estimates$TD$conf.int[[2]]))
method <- c("EG", "ML", "TD")
r0_2 <- data.frame(pe, ci_l, ci_u, method)
r0_2$city <- "Korsør"
r0 <- rbind(r0, r0_2)
rm(kor, r0_2, ci_u, ci_l, method, pe)



# plotfit(kor)
# plot(sens_time, what = "criterion")
# Sensitivity:
sens_GT <-sensitivity.analysis(korsoer$cases,
                               GT.type = "lognormal",
                               GT.mean = seq(2,7,0.5),
                               GT.sd.range = 1.5,
                               begin = 1, end = 17,
                               est.method = "EG",
                               sa.type = "GT")
# plot(x=sens_GT[,"GT.Mean"],
#      xlab="mean GT (days)",
#      y=sens_GT[,"R"],
#      ylim=c(1.2, 7.1),
#      ylab="R0 (95% CI)",
#      type="p", pch=19,
#      col="black",
#      main="Sensitivity of R0 to mean Generation Time")
# arrows(x0=as.numeric(sens_GT[,"GT.Mean"]),
#        y0=as.numeric(sens_GT[,"CI.lower"]),
#        y1=as.numeric(sens_GT[,"CI.upper"]),
#        angle=90,
#        code=3,
#        col="black",
#        length=0.05)

# CPH -----------------------------------------------------------------

sens_time <-sensitivity.analysis(cph$cases, mGT, begin = 1:11, end = 30:40,
                                 est.method = "EG", sa.type = "time")
# plot(sens_time)
# Because the sensitivity matrix is not so clear, look at deviance R-sq data
# plot(sens_time, what = "criterion")

cph_r0 <- estimate.R(cph$cases, GT = mGT, t = cph$day_index, begin = 1, end = 39, methods = c("EG", "ML", "TD"), nsim = 1500)
# plotfit(cph_r0)
pe <- c(cph_r0$estimates$EG$R, cph_r0$estimates$ML$R, mean(cph_r0$estimates$TD$R))
ci_l<- c(cph_r0$estimates$EG$conf.int[1],
         cph_r0$estimates$ML$conf.int[1],
         mean(cph_r0$estimates$TD$conf.int[[1]]))
ci_u <- c(cph_r0$estimates$EG$conf.int[2],
          cph_r0$estimates$ML$conf.int[2],
          mean(cph_r0$estimates$TD$conf.int[[2]]))
method <- c("EG", "ML", "TD")
r0_2 <- data.frame(pe, ci_l, ci_u, method)
r0_2$city <- "Copenhagen"
r0 <- rbind(r0, r0_2)
rm(cph_r0, r0_2, ci_u, ci_l, method, pe)
r0$city <- factor(r0$city)



# Sensitivity:
sens_GT <-sensitivity.analysis(korsoer$cases,
                               GT.type = "lognormal",
                               GT.mean = seq(2,6,0.5),
                               GT.sd.range = 1,
                               begin = 1, end = 17,
                               est.method = "EG",
                               sa.type = "GT")
# plot(x=sens_GT[,"GT.Mean"],
#      xlab="mean GT (days)",
#      y=sens_GT[,"R"],
#      ylim=c(1.2, 4.1),
#      ylab="R0 (95% CI)",
#      type="p", pch=19,
#      col="black",
#      main="Sensitivity of R0 to mean Generation Time")
# arrows(x0=as.numeric(sens_GT[,"GT.Mean"]),
#        y0=as.numeric(sens_GT[,"CI.lower"]),
#        y1=as.numeric(sens_GT[,"CI.upper"]),
#        angle=90,
#        code=3,
#        col="black",
#        length=0.05)



# R0 plots for cities -----------------------------------------------------
pd <- position_dodge(0.4)

# Remove time-dependent
r0 <- r0[r0$method != "TD", ]
R0 <- ggplot(data = r0,
             aes(x = city, y = pe, color = method)) +
  geom_point(position = pd,
             size = 2) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                width = 0.15,
                size = 1,
                position = pd) +
  ggtitle(expression(R[0]*" estimates")) +
  ylab(expression(R[0])) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.85),
        axis.text.x = element_text(size = 14, angle = 0, vjust = 0.9),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18,
                                    face = "bold",
                                    vjust = 1.3),
        plot.title = element_text(size = 18, face="bold")) +
  scale_y_continuous(limits = c(1, max(r0$ci_u))) +
  scale_color_manual(values = c("orange", "royalblue2"),
                       breaks = c("EG", "ML"),
                       labels = c("Exponential \nGrowth\n",
                                 "Maximum \n Likelihood"))
R0



ggsave(filename = "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Output/F9 - R0.jpg",
       plot = R0,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)
