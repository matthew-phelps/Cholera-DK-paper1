# Author: Matthew Phelps
# Desc: Regerssion for coastal towns

rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1")

ifelse(grepl("wrz741", getwd()),
       gis.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data/GIS/cholera_dk_towns",
       gis.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/GIS/cholera_dk_towns")
setwd(gis.path)
graphics.off()

library('rgdal')
library(maptools)
library(dplyr)
library(spdep)
library(spatstat)
library(spgwr)
# LOAD --------------------------------------------------------------------

neg1 <- readOGR(".", "cholera_neg_coast")
pos1 <- readOGR(".", "cholera_pos_coast")
all <- spRbind(neg1, pos1)
proj4string(all)
all$join_FID <- all$name <- all$id <- NULL
# Re-code all cholera = 3 towns to cholera = 0 (i.e no cholera)
all$cholera <- ifelse(all$cholera != 1, 0, 1)
all$size[is.na(all$size)] <- 1

# Convert to ppp to use to spatstats: https://goo.gl/nwHJ5z


ban <- gwr.sel(cholera ~ Port, data = all, adapt = T)
mo <- gwr(cholera ~ Port, data = all, adapt = ban)
mo2 <- gwr(cholera ~ Port + distance, data = all, adapt = ban)
mo2
all_ppp <- as.ppp(all)
all_data <- all@data
plot(all_ppp)

# Summary STATS
summary(all_ppp)
plot(Smooth(all_ppp))
median(all_data$size, na.rm = T)




# PPM REGRESSION ----------------------------------------------------------

ppm(all_ppp ~ Port, data = all_data)

slrm(all_ppp ~ all_ppp$Port)
X <- copper$SouthPoints

# CREAT LIST OF NEIGHBORS ----------------------------------------------------------
# Sources: http://goo.gl/pjTuWc (ppt)
# K-nearest neighbors using 4 neigbhors. There doesn't seem to be good aggrement
# on best number of K to use - more seems to produce less error, but I don't
# know on what upper limit should be
coords <- coordinates(all)
IDs <- row.names(as(all, "data.frame"))
all_k4 <- knn2nb(knearneigh(coords, k = 4), row.names = IDs)
plot(all)
plot(all_k4, coords, add=T)


# Weights based on distance. Intuitively this seems more appropriate to me since
# we suspect cholera can only travel a certain distance overland. I.e. the
# physical phenomena has a max distance compenent
all_k1 <- knn2nb(knearneigh(coords, k = 1), row.names = IDs)
plot(all)
plot(all_k1, coords, add = T)
dist <- unlist(nbdists(all_k1, coords)) # distance to nearest neighbor
summary(dist)

dist_k <- dnearneigh(coords, d1 = 0, d2 = 20000, row.names = IDs)
plot(all)
plot(dist_k, coords, add = T)


# WEIGHT NEIGHBORS --------------------------------------------------------
all_k4_w <- nb2listw(all_k4, style = "B")
all_k4_w




# EXAMINE SPATIAL AUTOCORRELATION -----------------------------------------
moran.test(all$cholera, listw = all_k4_w, alternative = "two.sided")
moran.plot(all$cholera, all_k4_w, labels = as.character(all$name))

# GROUP PREDICTORS --------------------------------------------------------

all$coast <- ifelse(all$distance < 5, 1, 0)


# REGRESSION --------------------------------------------------------------
bin_w <- nb2listw()
lagsarlm()

