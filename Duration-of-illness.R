# Author: Matthew Phelps
# Desc: Estimating the duration of infectiousness based upon hospital admission
# data in Nykoebing

rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       script.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1",
       script.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1")
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data")

ifelse(grepl("wrz741", getwd()),
       check.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data/data to check",
       check.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/data to check")


setwd(data.path)

graphics.off()

#devtools::install_github('matthew-phelps/CholeraDataDK')
library(tidyr)
library(ggplot2)
library(data.table)
library(Publish)
library(CholeraDataDK)
library(MASS) # fit to distribution


# LOAD --------------------------------------------------------------------

hosp <- cph_hosp_stay_data



# CLEANING ----------------------------------------------------------------
# Remove records with neither release nor death data
x <- ifelse(is.na(hosp$death) & is.na(hosp$release), F, T)
hosp <- hosp[x, ]

# Assign outcome variable
x <- gather(hosp, "event", "date", 3:4)
hosp <- x[complete.cases(x), ]

hosp$duration <- as.numeric(hosp$date - hosp$admission)
x <- ifelse(hosp$duration < 0, T, F)
y <- ifelse(hosp_1$duration > 30, T, F)
to_check <- hosp[x, ]

# output data cleaning for Mads to check
setwd(check.path)
write.csv2(to_check, file = "hosp_data_to_check.csv")

# For now: remove problemative records
hosp_1 <- hosp[!x, ]
y <- ifelse(hosp_1$duration > 30, T, F) # examine good cut-off value by exp qqplots
hosp_1 <- hosp_1[!y, ]


# split into separate hospitals
hosp_grp <- split(hosp_1, f = hosp_1$hospital)



# EXPLORING ---------------------------------------------------------------
hosp_1 <- data.table(hosp_1)
hosp_1[, list(median = median(duration)), by = hospital]

ggplot(hosp_1) +
  geom_density(aes(duration, color = hospital, fill = hospital),
               alpha = 0.1)

# Do they come from an exponential distribution http://goo.gl/CjHlxW:
x <-  hosp_grp$`Frelser Arbejdshus`$duration
x2 <- hosp_grp$`Frue Arbejdshus`$duration
x3 <- hosp_grp$`Sankt Annae`$duration
x4 <- hosp_grp$Suhmsgade$duration
x5 <- hosp_1$duration
p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(x,p=p) # percentiles of the sample distribution
q <- quantile(x2,p=p) # percentiles of the sample distribution
q <- quantile(x3,p=p) # percentiles of the sample distribution
q <- quantile(x4,p=p) # percentiles of the sample distribution
q <- quantile(x5,p=p) # percentiles of the sample distribution

plot(qexp(p) ,q, main="Exponential Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qexp,col="blue", lty=2)

# Find lambda of distr for city:
lambda <- fitdistr(x5, "exponential")
1/lambda$estimate


plot(density(rexp(100, fit1$estimate)))
plot(density(hosp_grp$`Frue Arbejdshus`$duration))
lines(density(hosp_grp$`Frelser Arbejdshus`$duration), col = "red")
lines(density(hosp_grp$`Sankt Annae`$duration), col = "blue")
lines(density(hosp_grp$Suhmsgade$duration), col = "green")

d <- density(hosp$duration[hosp$duration < 50 & hosp$duration >0])
plot(d)
mean(hosp$duration[hosp$duration < 50 & hosp$duration >0])
median(hosp$duration[hosp$duration < 50 & hosp$duration >0])

density(x)
