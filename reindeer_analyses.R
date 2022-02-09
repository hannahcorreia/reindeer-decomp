#### Load libraries ####
library(mgcv) # GAM
library(mgcViz) # Nice GAM smooths plotting
library(reshape2) # Melting of data
library(lubridate) # For date operations
library(ggplot2); library(viridis)
library(ggmap) # For different Norway map in ggplot2
library(ggrepel) # For label repelling in ggplot2
library(grid); library(gridExtra) # For clim.win plots of week spans
library(ggforce)
# library(stringr)
# library(FSA)
# library(DataCombine)
library(ggeffects); library(itsadug) # For plots of interaction terms in GAM

# # Sliding window code
# source("MAE_sliding-window.R")

# Secondary libraries for spatio-temporal plots/variograms
library(raster)
library(gstat)
library(spacetime)
library(zoo)
library(forecast)

# Plot/graphics default parameters
par.default <- par(no.readonly=TRUE)

#### Location of images/plots
imgfiles <- "/path/to/img/folder"
#### Location of data files, etc.
datafiles <- "/path/to/datafiles/folder"


#### Norway map data (used for GAM & sp plots)
library(maps)
library(mapdata)
library(mapproj)
library(rgeos)

library(sp)
library(rgdal)
library(plyr)
library(dplyr)

nok <- map_data("world2Hires", "Norway") 
coordinates(nok) <- c("long", "lat")
proj4string(nok) <- CRS("+proj=longlat +datum=WGS84")
nokutm <- spTransform(nok, CRS("+proj=utm +zone=33 ellps=WGS84"))
nokutm.df <- as.data.frame(nokutm) #in UTM
nok.df <- as.data.frame(nok)
nok.df2 <- data.frame(Longitude = nok.df$long, 
                      Latitude = nok.df$lat,
                      order = nok.df$order) #in lon/lat

## Red-green color-blind friendly palette (from https://colorbrewer2.org/ 
## and https://gka.github.io/palettes/#/100|d|e66101,fdb863,ffffff|ffffff,b2abd2,5e3c99|1|1)
# This one's center color is white
cb_pal <- c("#e66101","#e76507","#e8680e","#e96c13","#eb6f18","#ec731d","#ed7621",
            "#ee7926","#ef7d2a","#f0802e","#f18332","#f28736","#f38a3a","#f48d3e",
            "#f59042","#f69347","#f7974b","#f89a4f","#f99d53","#faa058","#faa35c",
            "#fba660","#fca965","#fdad69","#fdb06e","#feb373","#ffb677","#ffb97c",
            "#ffbc82","#ffc087","#ffc38c","#ffc691","#ffc997","#ffcc9c","#ffd0a2",
            "#ffd3a7","#ffd6ad","#ffd9b2","#ffdcb8","#ffdfbe","#ffe2c3","#ffe5c9",
            "#ffe8cf","#ffebd5","#ffeedb","#fff1e1","#fff4e7","#fff6ed","#fff9f3",
            "#fffcf9","#fbfbfd","#f8f7fb","#f4f3f9","#f1eff7","#edebf4","#eae7f2",
            "#e6e3f0","#e3dfee","#dfdbec","#dcd7ea","#d9d3e8","#d5cfe6","#d2cbe4",
            "#cfc7e2","#cbc3e0","#c8bfde","#c5bbdc","#c1b7da","#beb3d8","#bbafd5",
            "#b8abd3","#b5a7d1","#b2a3cf","#ae9fcd","#ab9bcb","#a897c9","#a593c7",
            "#a28fc5","#9f8cc3","#9c88c1","#9984bf","#9680bd","#927cbb","#8f78b9",
            "#8c75b7","#8971b5","#866db3","#8369b1","#8065af","#7d62ad","#7a5eab",
            "#775aa9","#7456a7","#7153a5","#6e4fa3","#6b4ba1","#67479f","#64449d",
            "#61409b","#5e3c99")
# This one's center color is light yellow
cb_pal2 <- c('#e66101', '#e76407', '#e8680d', '#e96b12', '#ea6f17', '#eb721b', 
             '#ec761f', '#ed7923', '#ee7c27', '#ef7f2a', '#f0832e', '#f18631', 
             '#f28935', '#f38c38', '#f48f3b', '#f5933f', '#f59642', '#f69945', 
             '#f79c48', '#f89f4c', '#f8a24f', '#f9a552', '#faa856', '#faac59', 
             '#fbaf5c', '#fbb25f', '#fcb563', '#fcb866', '#fdbb69', '#fdbe6d', 
             '#fec170', '#fec474', '#ffc777', '#ffca7a', '#ffce7e', '#ffd181', 
             '#ffd485', '#ffd788', '#ffda8c', '#ffdd90', '#ffe093', '#ffe497', 
             '#ffe79a', '#ffea9e', '#ffeda1', '#fff0a5', '#fff3a9', '#fff6ac', 
             '#fff9b0', '#fffcb3', '#fcfbb9', '#f8f7ba', '#f5f2bb', '#f1eebc', 
             '#eeeabd', '#ebe6be', '#e7e2bf', '#e4dec0', '#e1dac0', '#ddd6c1', 
             '#dad2c1', '#d7cdc1', '#d3c9c1', '#d0c5c1', '#cdc1c1', '#c9bdc1', 
             '#c6b9c1', '#c3b5c1', '#c0b1c0', '#bcadc0', '#b9a9c0', '#b6a6bf', 
             '#b3a2be', '#af9ebe', '#ac9abd', '#a996bc', '#a692bb', '#a38eba', 
             '#9f8ab9', '#9c87b8', '#9983b7', '#967fb6', '#937bb5', '#9077b3', 
             '#8d74b2', '#8970b1', '#866caf', '#8368ae', '#8065ac', '#7d61ab', 
             '#7a5da9', '#775aa7', '#7456a6', '#7152a4', '#6d4ea2', '#6a4ba1', 
             '#67479f', '#64439d', '#61409b', '#5e3c99')
# Modified wes_anderson Zissou1 palette
wes_zissou <- c('#3b9ab2', '#3e9bb3', '#419cb3', '#459db4', '#489fb4', '#4ca0b4', 
                '#4fa1b4', '#53a2b4', '#57a3b4', '#5aa4b4', '#5ea5b4', '#62a6b3', 
                '#66a7b3', '#69a8b2', '#6da9b1', '#71aab0', '#75abaf', '#79acae', 
                '#7cadac', '#80aeab', '#84afa9', '#88b0a8', '#8bb1a6', '#8fb2a4', 
                '#93b3a2', '#97b4a0', '#9ab59d', '#9eb69b', '#a2b798', '#a5b896', 
                '#a9b993', '#acba90', '#b0bb8c', '#b3bc89', '#b7bd86', '#babe82', 
                '#bebf7e', '#c1c07a', '#c5c176', '#c8c272', '#cbc36d', '#cfc469', 
                '#d2c564', '#d5c65e', '#d8c659', '#dcc753', '#dfc84c', '#e2c945', 
                '#e5ca3d', '#e8cb35', '#ebc929', '#ecc728', '#ecc427', '#edc226', 
                '#edbf25', '#edbd24', '#eeba23', '#eeb722', '#eeb521', '#eeb220', 
                '#efaf1f', '#efad1e', '#efaa1d', '#efa71c', '#f0a51b', '#f0a21a', 
                '#f09f19', '#f09c18', '#f09a17', '#f09716', '#f19415', '#f19114', 
                '#f18e13', '#f18c12', '#f18910', '#f1860f', '#f1830e', '#f1800d', 
                '#f17d0c', '#f17a0b', '#f1760a', '#f17309', '#f17008', '#f16d07', 
                '#f16906', '#f16606', '#f16305', '#f15f04', '#f15b04', '#f15703', 
                '#f15302', '#f14f02', '#f14b01', '#f14601', '#f14101', '#f13c00', 
                '#f13600', '#f12f00', '#f12600', '#f21a00')

## Load reindeer data
final.data <- read.csv("reindeerNOR_decomp.csv")

head(final.data)
newdat <- final.data

## Create categorical elevation variable
newdat$elev_cat <- NA
newdat[newdat$elev < 400, ]$elev_cat <- "low"
newdat[newdat$elev >= 400 & newdat$elev < 800, ]$elev_cat <- "mid"
newdat[newdat$elev >= 800, ]$elev_cat <- "high"
newdat$elev_cat <- as.factor(newdat$elev_cat)

## Create catagorical y variable 
south <- newdat$y<7200000; newdat[south, "y_cat"] <- "south"
mid <- newdat$y>=7200000 & newdat$y<=7600000; newdat[mid, "y_cat"] <- "mid"
north <- newdat$y>7600000; newdat[north, "y_cat"] <- "north"

newdat$dist <- as.factor(newdat$dist)
newdat$Grp_Tveraa <- as.factor(newdat$Grp_Tveraa)
nina.data <- newdat



#### Model selection CV: decomposed or undecomposed? ####

# Looking at the form of the model:
# Would putting sp-alt in NDVI create a better model?
# Would spatio-temp NDVI create a better model?
# Would regional NDVI be better?

## Random 10-fold, herd 10-fold, and regional k-fold cross-validation
# (perfomed on CASIC, see reindeerCV_casic.R file for details)

# Models used in CV

# model NDVI regional window (no spt smooths)
z0 <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
            ti(BodyCond_PrevYr) + ti(pop_density) + 
            ti(BodyCond_PrevYr, pop_density) + s(elev, by = factor(y_cat)) + 
            s(ndvi_regn.win) + s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) +
            s(snow_auc) + s(snowstart_doy), 
          data = nina.data, family = scat)
summary(z0)$r.sq; AIC(z0)
# model NDVI national window (no spt smooths)
z1 <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
            ti(BodyCond_PrevYr) + ti(pop_density) + 
            ti(BodyCond_PrevYr, pop_density) + s(elev, by = factor(y_cat)) + 
            s(mean_ndvi) + s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) +
            s(snow_auc) + s(snowstart_doy), 
          data = nina.data, family = scat)
summary(z1)$r.sq; AIC(z1)
# spt smooths on body cond, pop_density, NDVI (regional), & snow AUC
z4 <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
            te(x,y,yr, by = BodyCond_PrevYr) + 
            te(x,y,yr, by = pop_density) + 
            s(BodyCond_PrevYr, pop_density) + s(elev, by = factor(y_cat)) + 
            te(x,y,yr, by = ndvi_regn.win) +
            s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) +
            te(x,y,yr, by = snow_auc) + s(snowstart_doy), 
          data = nina.data, family = scat)
summary(z4)$r.sq; AIC(z4)
# spt smooths on body cond, pop_density, NDVI (national), & snow AUC
z5 <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
            te(x,y,yr, by = BodyCond_PrevYr) + 
            te(x,y,yr, by = pop_density) + 
            s(BodyCond_PrevYr, pop_density) + s(elev, by = factor(y_cat)) + 
            te(x,y,yr, by = mean_ndvi) +
            s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) +
            te(x,y,yr, by = snow_auc) + s(snowstart_doy), 
          data = nina.data, family = scat)
summary(z5)$r.sq; AIC(z5)
# decomposed model (includes mean and cv national NDVI decomposed)
z6 <- gam(CalfHarvest_AvgMass ~ 
            ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
            ti(density_t) + ti(density_sp) + ti(density_resid) +
            ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
            ti(bodycond_resid, density_resid) + 
            s(elev, by = factor(y_cat)) +
            s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
            s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
            s(peakndvi_doy_resid, k = 5) + 
            s(springo_t, k = 5) + s(springo_sp, k = 5) + 
            s(springo_resid, k = 5) +
            s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
            s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
          data = nina.data, family = scat)
summary(z6)$r.sq; AIC(z6)


# Create training datasets for herd and regional 10-fold here:
library(groupdata2)
set.seed(36849)
#herd CV
k <- 10
train_set <- fold(nina.data, k = k, id_col = "dist")
train_set <- train_set[order(train_set$.folds,
                             train_set$dist, train_set$yr),]
save(train_set, file = "train_set.Rda")



#### CV results from "reindeerCV_manuscript.R" ####
# Random 10-fold CV results
r10e <- read.table("../cross_validation/cv.errors.R.txt", header = T)
head(r10e)
apply(r10e,2,median) # mean absolute CV errors

r10e.bymethod <- data.frame(method = matrix(NA, nrow = 50), 
                            absCVerr = matrix(NA, nrow = 50))
r10e.bymethod[1:10,1] <- "basicmod_NDVIregn"
r10e.bymethod[1:10,2] <- as.vector(r10e[,2])
r10e.bymethod[11:20,1] <- "basicmod_NDVInatl"
r10e.bymethod[11:20,2] <- as.vector(r10e[,3])
r10e.bymethod[21:30,1] <- "spt_all.NDVIregn"
r10e.bymethod[21:30,2] <- as.vector(r10e[,4])
r10e.bymethod[31:40,1] <- "spt_all.NDVInatl"
r10e.bymethod[31:40,2] <- as.vector(r10e[,5])
r10e.bymethod[41:50,1] <- "decomp_mod"
r10e.bymethod[41:50,2] <- as.vector(r10e[,6])

r10e.bymethod$method <- as.factor(r10e.bymethod$method)
r10e.bymethod$method <- factor(r10e.bymethod$method, 
                               levels(r10e.bymethod$method)[c(2,1,5,4,3)])
kruskal.test(absCVerr ~ method, data = r10e.bymethod)
# KW not significant, therefore pairwise test not needed
# pairwise.wilcox.test(r10e.bymethod$absCVerr, r10e.bymethod$method, 
#                      p.adjust.method = "bonferroni",
#                      paired = TRUE)
# ## calculate mean CVE for each model (same as above)
# aggregate(.~ method, r10e.bymethod, function(x) mean(x, na.rm = T))


# Herd group 10-fold CV results
h10e <- read.table("../cross_validation/cv.errors.H.txt", header = T)
head(h10e)
apply(h10e,2,median) # median absolute CV errors

h10e.bymethod <- data.frame(method = matrix(NA, nrow = 50), 
                            absCVerr = matrix(NA, nrow = 50))
h10e.bymethod[1:10,1] <- "basicmod_NDVIregn"
h10e.bymethod[1:10,2] <- as.vector(h10e[,2])
h10e.bymethod[11:20,1] <- "basicmod_NDVInatl"
h10e.bymethod[11:20,2] <- as.vector(h10e[,3])
h10e.bymethod[21:30,1] <- "spt_all.NDVIregn"
h10e.bymethod[21:30,2] <- as.vector(h10e[,4])
h10e.bymethod[31:40,1] <- "spt_all.NDVInatl"
h10e.bymethod[31:40,2] <- as.vector(h10e[,5])
h10e.bymethod[41:50,1] <- "decomp_mod"
h10e.bymethod[41:50,2] <- as.vector(h10e[,6])

h10e.bymethod$method <- as.factor(h10e.bymethod$method)
kruskal.test(absCVerr ~ method, data = h10e.bymethod)
# KW significant, therefore pairwise test needed
# order levels of method by decreasing median CVE
# h10e.bymethod$method <- factor(h10e.bymethod$method,levels(h10e.bymethod$method)[c(5,2,1,4,3)])
# order by method
h10e.bymethod$method <- factor(h10e.bymethod$method, levels(h10e.bymethod$method)[c(2,1,5,4,3)])
pairwise.wilcox.test(h10e.bymethod$absCVerr, h10e.bymethod$method, 
                     p.adjust.method = "bonferroni",
                     paired = TRUE, alternative = "less") #hard to set reference grp
ggplot(h10e.bymethod, aes(x = method, y = absCVerr)) + geom_boxplot()

## Better/more intuitive way to view results
library(tidyverse)
library(rstatix)
stat.test <- h10e.bymethod  %>%
  wilcox_test(absCVerr ~ method, paired = TRUE, p.adjust.method = "bonferroni", 
              ref.group = "decomp_mod", alternative = "less") %>%
  add_significance("p.adj")

h10e.bymethod  %>%
  wilcox_effsize(absCVerr ~ method, paired = TRUE, ref.group = "decomp_mod")





#### Plot results of base national NDVI GAM model ####
base.gam <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
                  ti(BodyCond_PrevYr) + ti(pop_density) + 
                  ti(BodyCond_PrevYr, pop_density) + s(elev, by = factor(y_cat)) + 
                  s(mean_ndvi) + s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) +
                  s(snow_auc) + s(snowstart_doy), 
                data = nina.data, family = scat, method = "REML")

# Summary of GAM
summary(base.gam)

# Plot smooth functions
b <- getViz(base.gam) #from library(mgcViz)
# p2 <- plot(b, select = c(2,3,5,6,7,8))
# print(p2, ask = FALSE, pages = 1)
# plotRGL(sm(b, 1), fix = c("yr" = 1999), residuals = F)

p0 <- plot(base.gam, select = 1, color = "heat")
pls <- plotSlice(x = sm(b, 1), 
                 fix = list("yr" = c(1985,1988,1992,1995,1999,2003,2006,2010,2013)), 
                 a.facet = list(nrow = 3))
p0a <- pls + l_fitRaster() + l_fitContour() + theme(panel.spacing = unit(0.5, "lines"))
p1 <- plot(sm(b, 2)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(B["u,v,t-1"])),
       y = "Calf mass (kg)")
p2 <- plot(sm(b, 3)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(N["u,v,t"])),
       y = "Calf mass (kg)")
# p3 <- vis.gam(base.gam, view = c("BodyCond_PrevYr", "pop_density"),
#               plot.type = "contour", main = "",
#               xlab = expression(paste(B["u,v,t-1"])),
#               ylab = expression(paste(N["u,v,t"])))
p3 <- plot(sm(b, 4)) + l_fitRaster() + l_fitContour() + ggtitle("") + 
  labs(x = expression(paste(B["u,v,t-1"])),
       y = expression(paste(N["u,v,t"])))
p4 <- plot(sm(b, 5)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(elev["u,v,t"],~~"(mid Norway)")),
       y = "Calf mass (kg)")
p5 <- plot(sm(b, 6)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(elev["u,v,t"],~~"(north Norway)")),
       y = "Calf mass (kg)")
p6 <- plot(sm(b, 7)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(elev["u,v,t"],~~"(south Norway)")),
       y = "Calf mass (kg)")
p7 <- plot(sm(b, 8)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(nNDVI["u,v,t"])),
       y = "Calf mass (kg)")
p8 <- plot(sm(b, 9)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(peakNDVIdoy["u,v,t"])),
       y = "Calf mass (kg)")
p9 <- plot(sm(b, 10)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(SOdoy["u,v,t"])),
       y = "Calf mass (kg)")
p10 <- plot(sm(b, 11)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(snow["u,v,t"])),
       y = "Calf mass (kg)")
p11 <- plot(sm(b, 12)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(WPGSdoy["u,v,t"])),
       y = "Calf mass (kg)")

print(plot(b, allTerms = T), ask = FALSE, pages = 1)

ml <- list(p1[["ggObj"]], p2[["ggObj"]], p3[["ggObj"]], p4[["ggObj"]], 
           p5[["ggObj"]], p6[["ggObj"]], p7[["ggObj"]], p8[["ggObj"]],
           p9[["ggObj"]], p10[["ggObj"]], p11[["ggObj"]])
grid.arrange(grobs = ml, nrow = 3)


## New plots
setwd(imgfiles)
setEPS()
postscript("base_smooths.eps", width = 10, height = 10)
grid.arrange(grobs = ml, nrow = 4)
dev.off()
setwd(datafiles)





#### Plot results of FINAL DECOMPOSED GAM model ####
decomp.gam <- gam(CalfHarvest_AvgMass ~ 
                    ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                    ti(density_t) + ti(density_sp) + ti(density_resid) +
                    ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                    ti(bodycond_resid, density_resid) + 
                    s(elev, by = factor(y_cat)) +
                    s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                    s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                    s(peakndvi_doy_resid, k = 5) + 
                    s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                    s(springo_resid, k = 5) +
                    s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                    s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                  data = nina.data, family = scat, method = "REML")
# Summary of GAM
summary(decomp.gam)

# Plot smooth functions
b <- getViz(decomp.gam) #from library(mgcViz)
# p2 <- plot(b, select = c(2,3,5,6,7,8))
# print(p2, ask = FALSE, pages = 1)

p1t <- plot(sm(b, 1)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(B["time"])),
       y = "Calf mass (kg)")
p1sp <- plot(sm(b, 2)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(B["space"])),
       y = "Calf mass (kg)")
p1r <- plot(sm(b, 3)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(B["resid"])),
       y = "Calf mass (kg)")

p2t <- plot(sm(b, 4)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(N["time"])),
       y = "Calf mass (kg)")
p2sp <- plot(sm(b, 5)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(N["space"])),
       y = "Calf mass (kg)")
p2r <- plot(sm(b, 6)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(N["resid"])),
       y = "Calf mass (kg)")

p3t <- plot(sm(b, 7)) + l_fitRaster() + l_fitContour() + ggtitle("") + 
  labs(x = expression(paste(B["time"])),
       y = expression(paste(N["time"])))
p3sp <- plot(sm(b, 8)) + l_fitRaster() + l_fitContour() + ggtitle("") + 
  labs(x = expression(paste(B["space"])),
       y = expression(paste(N["space"])))
p3r <- plot(sm(b, 9)) + l_fitRaster() + l_fitContour() + ggtitle("") + 
  labs(x = expression(paste(B["resid"])),
       y = expression(paste(N["resid"])))

p4 <- plot(sm(b, 10)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(elev["u,v,t"],~~"(mid Norway)")),
       y = "Calf mass (kg)")
p5 <- plot(sm(b, 11)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(elev["u,v,t"],~~"(north Norway)")),
       y = "Calf mass (kg)")
p6 <- plot(sm(b, 12)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(elev["u,v,t"],~~"(south Norway)")),
       y = "Calf mass (kg)")

p7t <- plot(sm(b, 13)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(nNDVI["time"])),
       y = "Calf mass (kg)")
p7sp <- plot(sm(b, 14)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(nNDVI["space"])),
       y = "Calf mass (kg)")
p7r <- plot(sm(b, 15)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(nNDVI["resid"])),
       y = "Calf mass (kg)")

p8t <- plot(sm(b, 16)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(peakNDVIdoy["time"])),
       y = "Calf mass (kg)")
p8sp <- plot(sm(b, 17)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(peakNDVIdoy["space"])),
       y = "Calf mass (kg)")
p8r <- plot(sm(b, 18)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(peakNDVIdoy["resid"])),
       y = "Calf mass (kg)")

p9t <- plot(sm(b, 19)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(SOdoy["time"])),
       y = "Calf mass (kg)")
p9sp <- plot(sm(b, 20)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(SOdoy["space"])),
       y = "Calf mass (kg)")
p9r <- plot(sm(b, 21)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(SOdoy["resid"])),
       y = "Calf mass (kg)")

p10t <- plot(sm(b, 22)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(snow["time"])),
       y = "Calf mass (kg)")
p10sp <- plot(sm(b, 23)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(snow["space"])),
       y = "Calf mass (kg)")
p10r <- plot(sm(b, 24)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(snow["resid"])),
       y = "Calf mass (kg)")

p11t <- plot(sm(b, 25)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(WPGSdoy["time"])),
       y = "Calf mass (kg)")
p11sp <- plot(sm(b, 26)) + l_rug() + l_ciPoly() + l_fitLine(color = "black") + 
  labs(x = expression(paste(WPGSdoy["space"])),
       y = "Calf mass (kg)")
p11r <- plot(sm(b, 27)) + l_rug() + l_ciPoly() + l_fitLine(color = "red") + 
  labs(x = expression(paste(WPGSdoy["resid"])),
       y = "Calf mass (kg)")


print(plot(b, allTerms = T), ask = FALSE, pages = 1)

ml1 <- list(p1t[["ggObj"]], p1sp[["ggObj"]], p1r[["ggObj"]],
            p2t[["ggObj"]], p2sp[["ggObj"]], p2r[["ggObj"]],
            p3t[["ggObj"]], p3sp[["ggObj"]], p3r[["ggObj"]],
            p4[["ggObj"]], p5[["ggObj"]], p6[["ggObj"]])
ml2 <- list(p7t[["ggObj"]], p7sp[["ggObj"]], p7r[["ggObj"]],
            p8t[["ggObj"]], p8sp[["ggObj"]], p8r[["ggObj"]],
            p9t[["ggObj"]], p9sp[["ggObj"]], p9r[["ggObj"]],
            p10t[["ggObj"]], p10sp[["ggObj"]], p10r[["ggObj"]],
            p11t[["ggObj"]], p11sp[["ggObj"]], p11r[["ggObj"]])
#grid.arrange(grobs = ml1, nrow = 5)


setwd(imgfiles)
setEPS()
postscript("decomp_smooths1.eps", width = 10, height = 10)
grid.arrange(grobs = ml1, nrow = 4)
dev.off()

setEPS()
postscript("decomp_smooths2.eps", width = 10, height = 12)
grid.arrange(grobs = ml2, nrow = 5)
dev.off()
setwd(datafiles)


# Quick version of plotting smooths
plot(decomp.gam, shade = TRUE, shade.col="rosybrown2", 
     scheme = 2, mgp = c(2,1,0), scale = 0) # different y-axis for each variable

# 3D plots: juvenile mass as z-axis; plotted on map
vis.gam(base.gam, view=c("x","y"), asp=1,
        plot.type = "contour", too.far = 0.1,
        color = "topo", main = "te(x,y,yr, d=c(2,1))")
points(nokutm.df$long, nokutm.df$lat, cex = 0.3, pch = 16) # simple norway map
# Plot specific years
#1990
vis.gam(base.gam, view=c("x","y"), plot.type = "contour", 
        color="terrain", too.far = 0.1, cond = list(yr=1990), 
        zlim=range(decomp.gam$linear.predictors), asp=1) # plot of contour
points(nokutm.df$long, nokutm.df$lat, cex = 0.3, pch = 16) # norway map
#2010
vis.gam(base.gam, view=c("x","y"), plot.type = "contour", 
        color="terrain", too.far = 0.1, cond = list(yr=2010), 
        zlim=range(decomp.gam$linear.predictors), asp=1) # plot of contour
points(nokutm.df$long, nokutm.df$lat, cex = 0.3, pch = 16) # norway map





#### Fancy Norway maps using shapefiles w/ GAM predictions ####
library(raster); library(rgeos); library(plyr)
fylke1 <- shapefile("NOR_n5000_shape/NO_Arealdekke_pol.shp")
fylke <- fylke1[fylke1$OBJTYPE!="Havflate",]
norway1 <- gUnaryUnion(fylke)
plot(norway1, border = "darkgrey")
# Map for ggplot2 (less intensive)
Norway <- raster::getData("GADM", country="NO", level=0)
Norway1 <- raster::getData("GADM", country="NO", level=1)
# for NO country outline
nor <- gSimplify(Norway, tol=0.01, topologyPreserve=TRUE)
nor_df <- fortify(nor)
#for NO region outline
nor1 <- gSimplify(Norway1, tol=0.01, topologyPreserve=TRUE)
nor1_df <- fortify(nor1)

# Transform data utm33 to lon/lat
calfdata.lonlat <- data.frame(x = nina.data$x, y = nina.data$y)
coordinates(calfdata.lonlat) <- c("x", "y")
proj4string(calfdata.lonlat) <- CRS("+proj=utm +zone=33 ellps=WGS84")
calfdata.utm <- spTransform(calfdata.lonlat, CRS("+proj=longlat +datum=WGS84"))
calfdata.utm2 <- as.data.frame(calfdata.utm)
nina.data.lonlat <- nina.data
nina.data.lonlat$x <- calfdata.utm2$x; nina.data.lonlat$y <- calfdata.utm2$y



#### Plots for understanding decomposed B x N interactions ####
## Time plot
time <- distinct(nina.data[c("yr","bodycond_t","density_t")])
timedf <- melt(time, id.vars = "yr", variable.name = "type")

predmass_t <- cbind(nina.data[c("yr","bodycond_t","density_t")], 
                    decomp.gam$fitted.values)
colnames(predmass_t)[4] <- "predmass"

## scale values
predmass_t$bodycond_t <- scale(predmass_t$bodycond_t, scale = FALSE)
predmass_t$density_t <- scale(predmass_t$density_t, scale = FALSE)
predmass_t$predmass <- scale(predmass_t$predmass, scale = FALSE)

library(wesanderson)
wblue <- wes_palette("Zissou1", n = 5, type = "discrete")[1]
wred <- wes_palette("Zissou1", n = 5, type = "discrete")[5]
wgreen <- wes_palette("Darjeeling1", n = 5, type = "discrete")[2]
wyel <- wes_palette("Cavalcanti1", n = 5, type = "discrete")[1]
wpurp <- wes_palette("Rushmore1", n = 5, type = "discrete")[4]

cbfgreen <- "#1b9e77"
cbforange <- "#d95f02"
cbfpurp <- "#7570b3"

ptime <- ggplot(predmass_t, aes(x = yr)) + 
  geom_hline(yintercept = 0, color = "grey") + 
  stat_summary(aes(y = predmass, color = "response"), 
               fun.y = mean, geom = "line", size = 1.2) +
  stat_summary(aes(y = predmass), geom = "ribbon", fun.data = mean_cl_normal,
               fill = "darkgreen", alpha = 0.2) +
  #geom_smooth(aes(y = predmass), color = wgreen, alpha = 0.5, method = "loess") +
  geom_line(aes(y = bodycond_t, colour = "body"), size = 1) + 
  geom_line(aes(y = density_t, colour = "density"), size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Population density [no./sq.km]")) +
  scale_colour_manual(values = c(cbforange, cbfpurp, cbfgreen), 
                      labels = c(expression(paste(B["time"])), 
                                 expression(paste(N["time"])),
                                 expression(paste(widehat(Y))) )) +
  labs(y = "Mass [kg]", x = "Year", colour = "Variable") + 
  theme_bw() +
  theme(legend.position = c(0.9, 0.825))

setwd(imgfiles)
ggsave("BNY_time.eps", ptime, device=cairo_ps, width = 9, height = 4, units = "in",
       fallback_resolution = 600)
setwd(datafiles)


## Space
# GAM should only really be used to plot the same locations
# try using bigger grid & finding nearest neighbors to herd locations
#### MUST BE ON nina.data.lonlat FOR LON/LAT COORDINATES
gam2.lonlat <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)), 
                   data = nina.data.lonlat, family = scat, 
                   method = "REML")
# create new prediction grid
crds <- nor1_df
grid.x <- with(gam2.lonlat$var.summary,
               seq(min(c(x, crds$long)), max(c(x, crds$long)), by = 0.5))
grid.y <- with(gam2.lonlat$var.summary,
               seq(min(c(y, crds$lat)), max(c(y, crds$lat)), by = 0.25))
pmass <- expand.grid(
  x = grid.x,
  y = grid.y
)
# before assigning data, need to obtain only data near station points
# convert to sp object
pmass.sp <- pmass
coordinates(pmass.sp) <- c("x", "y")
proj4string(pmass.sp) <- CRS("+proj=longlat +datum=WGS84")
# unique stations/herds in original data
unq.loc_dat <- nina.data.lonlat[!duplicated(nina.data.lonlat$dist), c(1,4,5)]
unq.loc <- nina.data.lonlat[!duplicated(nina.data.lonlat$dist), c(1,4,5)]
coordinates(unq.loc) <- c("x", "y")
proj4string(unq.loc) <- CRS("+proj=longlat +datum=WGS84")
#### Obtain 20 closest coords to each herd location
library(geosphere)
dist <- gDistance(pmass.sp, unq.loc, byid=TRUE)
# For the 20 closest points, need 30 minimum for each location
# with closest points in pmass.sp to a herd in unq.loc
d <- apply(dist, 1, function(x) which(x == min(x)))
d <- rbind(d, matrix(NA, nrow = 19, ncol = length(d)))
for(i in 2:20){
  min.d <- apply(dist, 1, function(x) order(x, decreasing=F)[i])
  d[i,] <- unname(min.d)
}
# columns are ID numbers of unq.loc, rows are the 1st to 6th closest pts
# entries are the ID numbers of the pmass.sp locations that are closest to 
# unq.loc (herd locations)
#### keep only those obs which ID matches from d
d <- rbind(unq.loc_dat$dist, d)
temp <- pmass[d[2:21,],]
temp$dist <- NA
for(i in 1:nrow(temp)){
  t <- which(d == round(as.numeric(row.names.data.frame(temp[i,]))), arr.ind = T)
  temp[i,]$dist <- unname(d[1,t[2]])
}
# now need to add data that is similar to the real data for each location
rownames(temp) <- c()
temp1 <- temp[rep(row.names(temp), 6), 1:3]
temp1$yr <- c(rep(1985, nrow(temp)), rep(1990, nrow(temp)),
              rep(1995, nrow(temp)), rep(2000, nrow(temp)), 
              rep(2005, nrow(temp)), rep(2010, nrow(temp)))

# New variable "coordinates" with (lat, lon); define coord system
temp1$lon <- temp1$x
temp1$lat <- temp1$y
coordinates(temp1) <- ~x + y
proj4string(temp1) <- CRS("+proj=longlat") 
gridded(temp1) <- TRUE

locs <- distinct(nina.data.lonlat[,c("dist","x","y")])
dsetY <- cbind(nina.data.lonlat[c("dist")], decomp.gam$fitted.values)
colnames(dsetY)[2] <- "predmass"
dsetYm <- aggregate(dsetY$predmass,
                    by = list(dist = dsetY$dist),
                    function(x) mean(x, na.action = na.pass, na.rm = TRUE))
colnames(dsetYm)[2] <- "predmass"
dsetYm2 <- merge(dsetYm, distinct(nina.data.lonlat[c("dist","bodycond_sp")]), 
                 by = "dist")
dsetYm3 <- merge(dsetYm2, distinct(nina.data.lonlat[c("dist","density_sp")]), 
                 by = "dist")
dsetYm4 <- psdf <- merge(dsetYm3, locs, by = "dist")

# New variable "coordinates" with (lat, lon); define coord system
dsetYm4$lon <- dsetYm4$x
dsetYm4$lat <- dsetYm4$y
coordinates(dsetYm4) <- ~x + y; proj4string(dsetYm4) <- CRS("+proj=longlat") 

dsetYm4 <- dsetYm4[!is.na(dsetYm4$predmass),]

# IDW for bodycond B
idwB <- idw(formula = bodycond_sp ~ 1, locations = dsetYm4,
            newdata = temp1)  # apply idw model for the data
idwB.latlon <- spTransform(idwB, CRS("+proj=longlat +datum=WGS84 +no_defs"))  
idwB.output = as.data.frame(idwB.latlon)  # output is defined as a data table

# IDW for density N
idwN <- idw(formula = density_sp ~ 1, locations = dsetYm4,
            newdata = temp1)  # apply idw model for the data
idwN.latlon <- spTransform(idwN, CRS("+proj=longlat +datum=WGS84 +no_defs"))  
idwN.output = as.data.frame(idwN.latlon)  # output is defined as a data table

# IDW for response Y
idwY <- idw(formula = predmass ~ 1, locations = dsetYm4,
            newdata = temp1)  # apply idw model for the data
idwY.latlon <- spTransform(idwY, CRS("+proj=longlat +datum=WGS84 +no_defs"))  
idwY.output = as.data.frame(idwY.latlon)  # output is defined as a data table

dY <- data.frame(
  bodycond = idwB.output$var1.pred,
  density = idwN.output$var1.pred,
  predmass = idwY.output$var1.pred,
  x = idwY.output$x,
  y = idwY.output$y
)

dspace <- dY


# keep only points within Norway map
inout = over(
  SpatialPoints(temp1[,c("lon","lat")], proj4string=CRS(projection(Norway))),
  as(Norway,"SpatialPolygons")
)
ds2 <- dspace[!is.na(inout),]

library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")[c(1:25,seq(26, 74, by = 3),75:100)]

pB <- ggplot(nor_df, aes(long,lat)) + 
  # raster creates smaller points that ignore sizing
  geom_raster(data = ds2, aes(x = x, y = y, fill = bodycond)) +
  geom_path(data=nor_df, aes(long,lat, group=group), 
            color="black", size=0.2) +
  #theme(aspect.ratio=1) + 
  scale_fill_gradientn(expression(paste(B["space"]," (kg)")), 
                       colours = wes_zissou,
                       # colours = pal,
                       breaks = c(17, 19, 21, 23),
                       labels = c(17, 19, 21, 23)) + 
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() + coord_fixed(ratio = 2) + theme(legend.position="bottom")

pN <- ggplot(nor_df, aes(long,lat)) + 
  # raster creates smaller points that ignore sizing
  geom_raster(data = ds2, aes(x = x, y = y, fill = density)) +
  geom_path(data=nor_df, aes(long,lat, group=group), 
            color="black", size=0.2) +
  #theme(aspect.ratio=1) + 
  scale_fill_gradientn(expression(paste(N["space"]," (no./sq.km)")), 
                       colours = wes_zissou,
                       # colours = pal,
                       breaks = c(2, 5, 8, 11, 14),
                       labels = c(2, 5, 8, 11, 14)) + 
  labs(x = "Longitude", y = "") +
  theme_bw() + coord_fixed(ratio = 2) + theme(legend.position="bottom")

pY <- ggplot(nor_df, aes(long,lat)) + 
  # raster creates smaller points that ignore sizing
  geom_raster(data = ds2, aes(x = x, y = y, fill = predmass)) +
  geom_path(data=nor_df, aes(long,lat, group=group), 
            color="black", size=0.2) +
  #theme(aspect.ratio=1) + 
  scale_fill_gradientn("Average juvenile\nbody mass (kg)\n", 
                       colours = wes_zissou,
                       # colours = pal,
                       breaks = c(17, 19, 21, 23),
                       labels = c(17, 19, 21, 23)) + 
  labs(x = "Longitude", y = "") +
  theme_bw() + coord_fixed(ratio = 2) + theme(legend.position="bottom")

g1 <- grid.arrange(pB,pN,pY, ncol = 3)

setwd(imgfiles)
ggsave("BNY_space.eps", g1, device=cairo_ps, width = 9, height = 4, units = "in",
       fallback_resolution = 600)
setwd(datafiles)


## Residual
# New variable "coordinates" with (lat, lon); define coord system
temp1$lon <- temp1$x
temp1$lat <- temp1$y
coordinates(temp1) <- ~x + y
proj4string(temp1) <- CRS("+proj=longlat") 
gridded(temp1) <- TRUE

## Go through each year
yrs <- c(1985:2013)
for(j in yrs){
  
  dset <- nina.data.lonlat[which(nina.data.lonlat$yr==j),]
  
  dgam <- mutate(dset,
                 predmass = predict(decomp.gam, dset, type = "response"))
  
  dsetYm <- dgam[c("yr","x","y","bodycond_resid","density_resid","predmass")]
  
  # New variable "coordinates" with (lat, lon); define coord system
  dsetYm$lon <- dsetYm$x
  dsetYm$lat <- dsetYm$y
  coordinates(dsetYm) <- ~x + y; proj4string(dsetYm) <- CRS("+proj=longlat") 
  
  dsetYm <- dsetYm[!is.na(dsetYm$predmass),]
  
  # IDW for bodycond B
  idwB <- idw(formula = bodycond_resid ~ 1, locations = dsetYm,
              newdata = temp1)  # apply idw model for the data
  idwB.latlon <- spTransform(idwB, CRS("+proj=longlat +datum=WGS84 +no_defs"))  
  idwB.output = as.data.frame(idwB.latlon)  # output is defined as a data table
  
  # IDW for density N
  idwN <- idw(formula = density_resid ~ 1, locations = dsetYm,
              newdata = temp1)  # apply idw model for the data
  idwN.latlon <- spTransform(idwN, CRS("+proj=longlat +datum=WGS84 +no_defs"))  
  idwN.output = as.data.frame(idwN.latlon)  # output is defined as a data table
  
  # IDW for response Y
  idwY <- idw(formula = predmass ~ 1, locations = dsetYm,
              newdata = temp1)  # apply idw model for the data
  idwY.latlon <- spTransform(idwY, CRS("+proj=longlat +datum=WGS84 +no_defs"))  
  idwY.output = as.data.frame(idwY.latlon)  # output is defined as a data table
  
  dtemp <- data.frame(
    yr = rep(j, nrow(idwY.output)),
    bodycond = idwB.output$var1.pred,
    density = idwN.output$var1.pred,
    predmass = idwY.output$var1.pred,
    x = idwY.output$x,
    y = idwY.output$y
  )
  
  if(j==1985){
    dmass2 <- dtemp
  }else(
    dmass2 <- rbind(dmass2, dtemp)
  )
  
}


# keep only points within Norway map
inout = over(
  SpatialPoints(temp1[,c("lon","lat")], proj4string=CRS(projection(Norway))),
  as(Norway,"SpatialPolygons")
)
ds3 <- dmass2[!is.na(inout),]

library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")[c(1:25,seq(26, 74, by = 3),75:100)]

pBr <- ggplot(nor_df, aes(long,lat)) + 
  # raster creates smaller points that ignore sizing
  geom_raster(data = ds3, aes(x = x, y = y, fill = bodycond)) +
  geom_path(data=nor_df, aes(long,lat, group=group), 
            color="black", size=0.05) +
  facet_wrap(~yr, nrow = 6) +
  #theme(aspect.ratio=1) + 
  scale_fill_gradientn(expression(paste(B["resid"])), 
                       colours = wes_zissou[c(1:25,seq(26, 74, by = 3),75:100)],
                       # colours = rev(cb_pal2[c(1:25,seq(26, 74, by = 3),75:100)]),
                       breaks = c(-26, -22, -18, -14),
                       labels = c(-26, -22, -18, -14)) + 
  labs(x = "Longitude", y = "Latitude") +
  # lims(x = c(8,max(nor_df$long)), y = c(60,max(nor_df$lat))) +
  theme_minimal() + coord_fixed(ratio = 2) + 
  theme(legend.position = "bottom", 
        panel.spacing.x = unit(0.1, "lines"), panel.spacing.y = unit(0.1, "lines"))

pNr <- ggplot(nor_df, aes(long,lat)) + 
  # raster creates smaller points that ignore sizing
  geom_raster(data = ds3, aes(x = x, y = y, fill = density)) +
  geom_path(data=nor_df, aes(long,lat, group=group), 
            color="black", size=0.05) +
  facet_wrap(~yr, nrow = 6) +
  #theme(aspect.ratio=1) + 
  scale_fill_gradientn(expression(paste(N["resid"])), 
                       colours = wes_zissou[c(1:25,seq(26, 74, by = 3),75:100)],
                       # colours = rev(cb_pal2[c(1:25,seq(26, 74, by = 3),75:100)]),
                       breaks = c(-8, -5, -2, 1),
                       labels = c(-8, -5, -2, 1)) + 
  labs(x = "Longitude", y = "") +
  # lims(x = c(8,max(nor_df$long)), y = c(60,max(nor_df$lat))) +
  theme_minimal() + coord_fixed(ratio = 2) + 
  theme(legend.position="bottom", 
        panel.spacing.x = unit(0.1, "lines"), panel.spacing.y = unit(0.1, "lines"))

pYr <- ggplot(nor_df, aes(long,lat)) + 
  # raster creates smaller points that ignore sizing
  geom_raster(data = ds3, aes(x = x, y = y, fill = predmass)) +
  geom_path(data=nor_df, aes(long,lat, group=group), 
            color="black", size=0.05) +
  facet_wrap(~yr, nrow = 6) +
  #theme(aspect.ratio=1) + 
  scale_fill_gradientn("Average juvenile\nbody mass (kg)\n", 
                       colours = wes_zissou[c(1:25,seq(26, 74, by = 3),75:100)],
                       # colours = rev(cb_pal2[c(1:25,seq(26, 74, by = 3),75:100)]),
                       breaks = c(15, 17, 19, 21, 23, 25),
                       labels = c(15, 17, 19, 21, 23, 25)) + 
  labs(x = "Longitude", y = "") +
  # lims(x = c(8,max(nor_df$long)), y = c(60,max(nor_df$lat))) +
  theme_minimal() + coord_fixed(ratio = 2) + 
  theme(legend.position="bottom", 
        panel.spacing.x = unit(0.1, "lines"), panel.spacing.y = unit(0.1, "lines"))


setwd(imgfiles)
ggsave("B_resid.eps", pBr, device=cairo_ps, width = 8, height = 11, units = "in",
       fallback_resolution = 800)
ggsave("N_resid.eps", pNr, device=cairo_ps, width = 8, height = 11, units = "in",
       fallback_resolution = 800)
ggsave("Y_resid.eps", pYr, device=cairo_ps, width = 8, height = 11, units = "in",
       fallback_resolution = 800)
setwd(datafiles)





#### Cohen's f Effect sizes for final basic GAM model of calf body mass ####

# Full model
fullmod <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
                 ti(BodyCond_PrevYr) + 
                 ti(pop_density) + ti(BodyCond_PrevYr, pop_density) + 
                 s(elev, by = factor(y_cat)) + 
                 s(mean_ndvi) + s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) + 
                 s(snow_auc) + s(snowstart_doy),
               data = nina.data, family = scat, method = "REML")
dev.expl.full <- summary(fullmod)$dev.expl

# Cohen's f for spatio-temporal smoother
dloc <- gam(CalfHarvest_AvgMass ~ 
              ti(BodyCond_PrevYr) + 
              ti(pop_density) + ti(BodyCond_PrevYr, pop_density) + 
              s(elev, by = factor(y_cat)) + 
              s(mean_ndvi) + s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) + 
              s(snow_auc) + s(snowstart_doy),
            sp = fullmod$sp[-c(1,2)], data = nina.data, 
            family = scat(theta = c(4.003,1.239)), method = "REML")
dev.expl.loc <- summary(dloc)$dev.expl
f.sq.loc <- (dev.expl.full - dev.expl.loc)/(1-dev.expl.full)

# Cohen's f for body condition
dbcond <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
                ti(pop_density) + ti(BodyCond_PrevYr, pop_density) + 
                s(elev, by = factor(y_cat)) + 
                s(mean_ndvi) + s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) + 
                s(snow_auc) + s(snowstart_doy),
              sp = fullmod$sp[-3], data = nina.data, 
              family = scat(theta = c(4.003,1.239)), method = "REML")
dev.expl.bcond <- summary(dbcond)$dev.expl
f.sq.bcond <- (dev.expl.full - dev.expl.bcond)/(1-dev.expl.full)

# Cohen's f for population density
dpopd <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
               ti(BodyCond_PrevYr) + 
               ti(BodyCond_PrevYr, pop_density) + 
               s(elev, by = factor(y_cat)) + 
               s(mean_ndvi) + s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) + 
               s(snow_auc) + s(snowstart_doy),
             sp = fullmod$sp[-4], data = nina.data, 
             family = scat(theta = c(4.003,1.239)), method = "REML")
dev.expl.popd <- summary(dpopd)$dev.expl
f.sq.dpopd <- (dev.expl.full - dev.expl.popd)/(1-dev.expl.full)

# Cohen's f for interaction body x pop
dbxp <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
              ti(BodyCond_PrevYr) + 
              ti(pop_density) + 
              s(elev, by = factor(y_cat)) + 
              s(mean_ndvi) + s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) + 
              s(snow_auc) + s(snowstart_doy),
            sp = fullmod$sp[-c(5,6)], data = nina.data, 
            family = scat(theta = c(4.003,1.239)), method = "REML")
dev.expl.bxp <- summary(dbxp)$dev.expl
f.sq.bxp <- (dev.expl.full - dev.expl.bxp)/(1-dev.expl.full)

# Cohen's f for elevation
delev <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
               ti(BodyCond_PrevYr) + 
               ti(pop_density) + ti(BodyCond_PrevYr, pop_density) + 
               s(mean_ndvi) + s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) + 
               s(snow_auc) + s(snowstart_doy),
             sp = fullmod$sp[-c(7,8,9)], data = nina.data, 
             family = scat(theta = c(4.003,1.239)), method = "REML")
dev.expl.elev <- summary(delev)$dev.expl
f.sq.elev <- (dev.expl.full - dev.expl.elev)/(1-dev.expl.full)

# Cohen's f for NDVI
dndvi <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
               ti(BodyCond_PrevYr) + 
               ti(pop_density) + ti(BodyCond_PrevYr, pop_density) + 
               s(elev, by = factor(y_cat)) + 
               s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) + 
               s(snow_auc) + s(snowstart_doy),
             sp = fullmod$sp[-c(10)], data = nina.data, 
             family = scat(theta = c(4.003,1.239)), method = "REML")
dev.expl.ndvi <- summary(dndvi)$dev.expl
f.sq.ndvi <- (dev.expl.full - dev.expl.ndvi)/(1-dev.expl.full)

# Cohen's f for peakNDVIdoy
dpndvi <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
                ti(BodyCond_PrevYr) + 
                ti(pop_density) + ti(BodyCond_PrevYr, pop_density) + 
                s(elev, by = factor(y_cat)) + 
                s(mean_ndvi) + s(springo_doy, k = 5) + 
                s(snow_auc) + s(snowstart_doy),
              sp = fullmod$sp[-c(11)], data = nina.data, 
              family = scat(theta = c(4.003,1.239)), method = "REML")
dev.expl.pndvi <- summary(dpndvi)$dev.expl
f.sq.pndvi <- (dev.expl.full - dev.expl.pndvi)/(1-dev.expl.full)

# Cohen's f for SOdoy
dSO <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
             ti(BodyCond_PrevYr) + 
             ti(pop_density) + ti(BodyCond_PrevYr, pop_density) + 
             s(elev, by = factor(y_cat)) + 
             s(mean_ndvi) + s(peak_ndvi_doy, k = 5) + 
             s(snow_auc) + s(snowstart_doy),
           sp = fullmod$sp[-c(12)], data = nina.data, 
           family = scat(theta = c(4.003,1.239)), method = "REML")
dev.expl.SO <- summary(dSO)$dev.expl
f.sq.SO <- (dev.expl.full - dev.expl.SO)/(1-dev.expl.full)

# Cohen's f for snow
dsnow <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
               ti(BodyCond_PrevYr) + 
               ti(pop_density) + ti(BodyCond_PrevYr, pop_density) + 
               s(elev, by = factor(y_cat)) + 
               s(mean_ndvi) + s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) + 
               s(snowstart_doy),
             sp = fullmod$sp[-c(13)], data = nina.data, 
             family = scat(theta = c(4.003,1.239)), method = "REML")
dev.expl.snow <- summary(dsnow)$dev.expl
f.sq.snow <- (dev.expl.full - dev.expl.snow)/(1-dev.expl.full)

# Cohen's f for snowstart
dsnowst <- gam(CalfHarvest_AvgMass ~ te(x,y,yr, d=c(2,1)) + 
                 ti(BodyCond_PrevYr) + 
                 ti(pop_density) + ti(BodyCond_PrevYr, pop_density) + 
                 s(elev, by = factor(y_cat)) + 
                 s(mean_ndvi) + s(peak_ndvi_doy, k = 5) + s(springo_doy, k = 5) + 
                 s(snow_auc),
               sp = fullmod$sp[-c(14)], data = nina.data, 
               family = scat(theta = c(4.003,1.239)), method = "REML")
dev.expl.snowst <- summary(dsnowst)$dev.expl
f.sq.snowst <- (dev.expl.full - dev.expl.snowst)/(1-dev.expl.full)

## Some code for proportion deviance explained, by Wood
## Null model
nullmod <- gam(CalfHarvest_AvgMass ~ 1, data = nina.data, family = scat, method = "REML")
## Proportion of deviance explained
(deviance(dloc) - deviance(fullmod))/deviance(nullmod) ## prop explained by spt term
(deviance(dbcond) - deviance(fullmod))/deviance(nullmod) ## prop explained by bodycond
(deviance(dpopd) - deviance(fullmod))/deviance(nullmod) ## prop explained by pop_density
(deviance(dbxp) - deviance(fullmod))/deviance(nullmod) ## prop explained by b x p interaction
(deviance(delev) - deviance(fullmod))/deviance(nullmod) ## prop explained by elevation
(deviance(dndvi) - deviance(fullmod))/deviance(nullmod) ## prop explained by national meanNDVI
(deviance(dpndvi) - deviance(fullmod))/deviance(nullmod) ## prop explained by peakNDVIdoy
(deviance(dSO) - deviance(fullmod))/deviance(nullmod) ## prop explained by spring onset
(deviance(dsnow) - deviance(fullmod))/deviance(nullmod) ## prop explained by snow AUC
(deviance(dsnowst) - deviance(fullmod))/deviance(nullmod) ## prop explained by snow start DOY





#### Cohen's f Effect sizes for final decomp GAM model of calf body mass ####

## Also includes code for proportion of deviance explained, if needed
# # Wood's oridinal code for prop deviance expl
# ## fit full and reduced models...
# b <- gam(y~s(x1)+s(x2))
# b1 <- gam(y~s(x1),sp=b$sp[1])
# b2 <- gam(y~s(x2),sp=b$sp[2])
# b0 <- gam(y~1)
# ## calculate proportions deviance explained...
# (deviance(b1)-deviance(b))/deviance(b0) ## prop explained by s(x2)
# (deviance(b2)-deviance(b))/deviance(b0) ## prop explained by s(x1)

## Full model
fullmod <- gam(CalfHarvest_AvgMass ~ 
                 ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                 ti(density_t) + ti(density_sp) + ti(density_resid) +
                 ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                 ti(bodycond_resid, density_resid) + 
                 s(elev, by = factor(y_cat)) +
                 s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                 s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                 s(peakndvi_doy_resid, k = 5) + 
                 s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                 s(springo_resid, k = 5) +
                 s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                 s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
               data = nina.data, family = scat, method = "REML")
dev.expl.full <- summary(fullmod)$dev.expl

# Cohen's f for TOTAL body condition
dbcond <- gam(CalfHarvest_AvgMass ~ 
                ti(density_t) + ti(density_sp) + ti(density_resid) +
                ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                ti(bodycond_resid, density_resid) + 
                s(elev, by = factor(y_cat)) +
                s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                s(peakndvi_doy_resid, k = 5) + 
                s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                s(springo_resid, k = 5) +
                s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
              sp = fullmod$sp[-c(1,2,3)], data = nina.data, 
              family = scat(theta = c(3.904,1.128)), method = "REML")
dev.expl.bcond <- summary(dbcond)$dev.expl
f.sq.bcond <- (dev.expl.full - dev.expl.bcond)/(1-dev.expl.full)
# Cohen's f for DECOMPOSED body condition
decomp_bt <- gam(CalfHarvest_AvgMass ~ 
                   ti(bodycond_sp) + ti(bodycond_resid) + 
                   ti(density_t) + ti(density_sp) + ti(density_resid) +
                   ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                   ti(bodycond_resid, density_resid) + 
                   s(elev, by = factor(y_cat)) +
                   s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                   s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                   s(peakndvi_doy_resid, k = 5) + 
                   s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                   s(springo_resid, k = 5) +
                   s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                   s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                 sp = fullmod$sp[-1], data = nina.data, 
                 family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_bsp <- gam(CalfHarvest_AvgMass ~ 
                    ti(bodycond_t) + ti(bodycond_resid) + 
                    ti(density_t) + ti(density_sp) + ti(density_resid) +
                    ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                    ti(bodycond_resid, density_resid) + 
                    s(elev, by = factor(y_cat)) +
                    s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                    s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                    s(peakndvi_doy_resid, k = 5) + 
                    s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                    s(springo_resid, k = 5) +
                    s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                    s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                  sp = fullmod$sp[-2], data = nina.data, 
                  family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_br <- gam(CalfHarvest_AvgMass ~ 
                   ti(bodycond_t) + ti(bodycond_sp) + 
                   ti(density_t) + ti(density_sp) + ti(density_resid) +
                   ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                   ti(bodycond_resid, density_resid) + 
                   s(elev, by = factor(y_cat)) +
                   s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                   s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                   s(peakndvi_doy_resid, k = 5) + 
                   s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                   s(springo_resid, k = 5) +
                   s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                   s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                 sp = fullmod$sp[-3], data = nina.data, 
                 family = scat(theta = c(3.904,1.128)), method = "REML")
# temporal body condition
dev.expl.decomp_bt <- summary(decomp_bt)$dev.expl
f.sq.decomp_bt <- (dev.expl.full - dev.expl.decomp_bt)/(1-dev.expl.full)
# spatial body condition
dev.expl.decomp_bsp <- summary(decomp_bsp)$dev.expl
f.sq.decomp_bsp <- (dev.expl.full - dev.expl.decomp_bsp)/(1-dev.expl.full)
#residual body condition
dev.expl.decomp_br <- summary(decomp_br)$dev.expl
f.sq.decomp_br <- (dev.expl.full - dev.expl.decomp_br)/(1-dev.expl.full)


# Cohen's f for TOTAL pop_density
dpop_density <- gam(CalfHarvest_AvgMass ~ 
                      ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                      ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                      ti(bodycond_resid, density_resid) + 
                      s(elev, by = factor(y_cat)) +
                      s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                      s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                      s(peakndvi_doy_resid, k = 5) + 
                      s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                      s(springo_resid, k = 5) +
                      s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                      s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                    sp = fullmod$sp[-c(4,5,6)], data = nina.data, 
                    family = scat(theta = c(3.904,1.128)), method = "REML")
dev.expl.pop_density <- summary(dpop_density)$dev.expl
f.sq.pop_density <- (dev.expl.full - dev.expl.pop_density)/(1-dev.expl.full)
# Cohen's f for DECOMPOSED population density
decomp_nt <- gam(CalfHarvest_AvgMass ~ 
                   ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                   ti(density_sp) + ti(density_resid) +
                   ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                   ti(bodycond_resid, density_resid) + 
                   s(elev, by = factor(y_cat)) +
                   s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                   s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                   s(peakndvi_doy_resid, k = 5) + 
                   s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                   s(springo_resid, k = 5) +
                   s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                   s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                 sp = fullmod$sp[-4], data = nina.data, 
                 family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_nsp <- gam(CalfHarvest_AvgMass ~ 
                    ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                    ti(density_t) + ti(density_resid) +
                    ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                    ti(bodycond_resid, density_resid) + 
                    s(elev, by = factor(y_cat)) +
                    s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                    s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                    s(peakndvi_doy_resid, k = 5) + 
                    s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                    s(springo_resid, k = 5) +
                    s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                    s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                  sp = fullmod$sp[-5], data = nina.data, 
                  family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_nr <- gam(CalfHarvest_AvgMass ~ 
                   ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                   ti(density_t) + ti(density_sp) + 
                   ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                   ti(bodycond_resid, density_resid) + 
                   s(elev, by = factor(y_cat)) +
                   s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                   s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                   s(peakndvi_doy_resid, k = 5) + 
                   s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                   s(springo_resid, k = 5) +
                   s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                   s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                 sp = fullmod$sp[-6], data = nina.data, 
                 family = scat(theta = c(3.904,1.128)), method = "REML")
# temporal pop density
dev.expl.decomp_nt <- summary(decomp_nt)$dev.expl
f.sq.decomp_nt <- (dev.expl.full - dev.expl.decomp_nt)/(1-dev.expl.full)
# spatial pop density
dev.expl.decomp_nsp <- summary(decomp_nsp)$dev.expl
f.sq.decomp_nsp <- (dev.expl.full - dev.expl.decomp_nsp)/(1-dev.expl.full)
#residual pop density
dev.expl.decomp_nr <- summary(decomp_nr)$dev.expl
f.sq.decomp_nr <- (dev.expl.full - dev.expl.decomp_nr)/(1-dev.expl.full)


# Cohen's f for TOTAL body x pop
dbody_pop <- gam(CalfHarvest_AvgMass ~ 
                   ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                   ti(density_t) + ti(density_sp) + ti(density_resid) +
                   s(elev, by = factor(y_cat)) +
                   s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                   s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                   s(peakndvi_doy_resid, k = 5) + 
                   s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                   s(springo_resid, k = 5) +
                   s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                   s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                 sp = fullmod$sp[-c(7:12)], data = nina.data, 
                 family = scat(theta = c(3.904,1.128)), method = "REML")
dev.expl.body_pop <- summary(dbody_pop)$dev.expl
f.sq.body_pop <- (dev.expl.full - dev.expl.body_pop)/(1-dev.expl.full)
# Cohen's f for DECOMPOSED body x pop
decomp_bnt <- gam(CalfHarvest_AvgMass ~ 
                    ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                    ti(density_t) + ti(density_sp) + ti(density_resid) +
                    ti(bodycond_sp, density_sp) + 
                    ti(bodycond_resid, density_resid) + 
                    s(elev, by = factor(y_cat)) +
                    s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                    s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                    s(peakndvi_doy_resid, k = 5) + 
                    s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                    s(springo_resid, k = 5) +
                    s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                    s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                  sp = fullmod$sp[-c(7,8)], data = nina.data, 
                  family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_bnsp <- gam(CalfHarvest_AvgMass ~ 
                     ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                     ti(density_t) + ti(density_sp) + ti(density_resid) +
                     ti(bodycond_t, density_t) + 
                     ti(bodycond_resid, density_resid) + 
                     s(elev, by = factor(y_cat)) +
                     s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                     s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                     s(peakndvi_doy_resid, k = 5) + 
                     s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                     s(springo_resid, k = 5) +
                     s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                     s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                   sp = fullmod$sp[-c(9,10)], data = nina.data, 
                   family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_bnr <- gam(CalfHarvest_AvgMass ~ 
                    ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                    ti(density_t) + ti(density_sp) + ti(density_resid) +
                    ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                    s(elev, by = factor(y_cat)) +
                    s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                    s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                    s(peakndvi_doy_resid, k = 5) + 
                    s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                    s(springo_resid, k = 5) +
                    s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                    s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                  sp = fullmod$sp[-c(11,12)], data = nina.data, 
                  family = scat(theta = c(3.904,1.128)), method = "REML")
# temporal body x pop
dev.expl.decomp_bnt <- summary(decomp_bnt)$dev.expl
f.sq.decomp_bnt <- (dev.expl.full - dev.expl.decomp_bnt)/(1-dev.expl.full)
# spatial body x pop
dev.expl.decomp_bnsp <- summary(decomp_bnsp)$dev.expl
f.sq.decomp_bnsp <- (dev.expl.full - dev.expl.decomp_bnsp)/(1-dev.expl.full)
#residual body x pop
dev.expl.decomp_bnr <- summary(decomp_bnr)$dev.expl
f.sq.decomp_bnr <- (dev.expl.full - dev.expl.decomp_bnr)/(1-dev.expl.full)


# Cohen's f for TOTAL elevation
delev <- gam(CalfHarvest_AvgMass ~ 
               ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
               ti(density_t) + ti(density_sp) + ti(density_resid) +
               ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
               ti(bodycond_resid, density_resid) + 
               s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
               s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
               s(peakndvi_doy_resid, k = 5) + 
               s(springo_t, k = 5) + s(springo_sp, k = 5) + 
               s(springo_resid, k = 5) +
               s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
               s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
             sp = fullmod$sp[-c(13,14,15)], data = nina.data, 
             family = scat(theta = c(3.904,1.128)), method = "REML")
dev.expl.elev <- summary(delev)$dev.expl
f.sq.elev <- (dev.expl.full - dev.expl.elev)/(1-dev.expl.full)


# Cohen's f for TOTAL national NDVI
dndvi <- gam(CalfHarvest_AvgMass ~ 
               ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
               ti(density_t) + ti(density_sp) + ti(density_resid) +
               ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
               ti(bodycond_resid, density_resid) + 
               s(elev, by = factor(y_cat)) +
               s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
               s(peakndvi_doy_resid, k = 5) + 
               s(springo_t, k = 5) + s(springo_sp, k = 5) + 
               s(springo_resid, k = 5) +
               s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
               s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
             sp = fullmod$sp[-c(16,17,18)], data = nina.data, 
             family = scat(theta = c(3.904,1.128)), method = "REML")
dev.expl.ndvi <- summary(dndvi)$dev.expl
f.sq.ndvi <- (dev.expl.full - dev.expl.ndvi)/(1-dev.expl.full)
# Cohen's f for DECOMPOSED national NDVI
decomp_ndvit <- gam(CalfHarvest_AvgMass ~ 
                      ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                      ti(density_t) + ti(density_sp) + ti(density_resid) +
                      ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                      ti(bodycond_resid, density_resid) + 
                      s(elev, by = factor(y_cat)) +
                      s(meanndvi_sp) + s(meanndvi_resid) +
                      s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                      s(peakndvi_doy_resid, k = 5) + 
                      s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                      s(springo_resid, k = 5) +
                      s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                      s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                    sp = fullmod$sp[-16], data = nina.data, 
                    family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_ndvisp <- gam(CalfHarvest_AvgMass ~ 
                       ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                       ti(density_t) + ti(density_sp) + ti(density_resid) +
                       ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                       ti(bodycond_resid, density_resid) + 
                       s(elev, by = factor(y_cat)) +
                       s(meanndvi_t) + s(meanndvi_resid) +
                       s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                       s(peakndvi_doy_resid, k = 5) + 
                       s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                       s(springo_resid, k = 5) +
                       s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                       s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                     sp = fullmod$sp[-17], data = nina.data, 
                     family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_ndvir <- gam(CalfHarvest_AvgMass ~ 
                      ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                      ti(density_t) + ti(density_sp) + ti(density_resid) +
                      ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                      ti(bodycond_resid, density_resid) + 
                      s(elev, by = factor(y_cat)) +
                      s(meanndvi_t) + s(meanndvi_sp) + 
                      s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                      s(peakndvi_doy_resid, k = 5) + 
                      s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                      s(springo_resid, k = 5) +
                      s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                      s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                    sp = fullmod$sp[-18], data = nina.data, 
                    family = scat(theta = c(3.904,1.128)), method = "REML")
# temporal NDVI
dev.expl.decomp_ndvit <- summary(decomp_ndvit)$dev.expl
f.sq.decomp_ndvit <- (dev.expl.full - dev.expl.decomp_ndvit)/(1-dev.expl.full)
# spatial NDVI
dev.expl.decomp_ndvisp <- summary(decomp_ndvisp)$dev.expl
f.sq.decomp_ndvisp <- (dev.expl.full - dev.expl.decomp_ndvisp)/(1-dev.expl.full)
#residual NDVI
dev.expl.decomp_ndvir <- summary(decomp_ndvir)$dev.expl
f.sq.decomp_ndvir <- (dev.expl.full - dev.expl.decomp_ndvir)/(1-dev.expl.full)


# Cohen's f for TOTAL peakNDVIdoy
dpndvi <- gam(CalfHarvest_AvgMass ~ 
                ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                ti(density_t) + ti(density_sp) + ti(density_resid) +
                ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                ti(bodycond_resid, density_resid) + 
                s(elev, by = factor(y_cat)) +
                s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                s(springo_resid, k = 5) +
                s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
              sp = fullmod$sp[-c(19,20,21)], data = nina.data, 
              family = scat(theta = c(3.904,1.128)), method = "REML")
dev.expl.pndvi <- summary(dpndvi)$dev.expl
f.sq.pndvi <- (dev.expl.full - dev.expl.pndvi)/(1-dev.expl.full)
# Cohen's f for DECOMPOSED peakNDVIdoy
decomp_pndvit <- gam(CalfHarvest_AvgMass ~ 
                       ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                       ti(density_t) + ti(density_sp) + ti(density_resid) +
                       ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                       ti(bodycond_resid, density_resid) + 
                       s(elev, by = factor(y_cat)) +
                       s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                       s(peakndvi_doy_sp, k = 5) + 
                       s(peakndvi_doy_resid, k = 5) + 
                       s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                       s(springo_resid, k = 5) +
                       s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                       s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                     sp = fullmod$sp[-19], data = nina.data, 
                     family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_pndvisp <- gam(CalfHarvest_AvgMass ~ 
                        ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                        ti(density_t) + ti(density_sp) + ti(density_resid) +
                        ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                        ti(bodycond_resid, density_resid) + 
                        s(elev, by = factor(y_cat)) +
                        s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                        s(peakndvi_doy_t, k = 5) + 
                        s(peakndvi_doy_resid, k = 5) + 
                        s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                        s(springo_resid, k = 5) +
                        s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                        s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                      sp = fullmod$sp[-20], data = nina.data, 
                      family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_pndvir <- gam(CalfHarvest_AvgMass ~ 
                       ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                       ti(density_t) + ti(density_sp) + ti(density_resid) +
                       ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                       ti(bodycond_resid, density_resid) + 
                       s(elev, by = factor(y_cat)) +
                       s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                       s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                       s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                       s(springo_resid, k = 5) +
                       s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                       s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                     sp = fullmod$sp[-21], data = nina.data, 
                     family = scat(theta = c(3.904,1.128)), method = "REML")
# temporal peakNDVIdoy
dev.expl.decomp_pndvit <- summary(decomp_pndvit)$dev.expl
f.sq.decomp_pndvit <- (dev.expl.full - dev.expl.decomp_pndvit)/(1-dev.expl.full)
# spatial peakNDVIdoy
dev.expl.decomp_pndvisp <- summary(decomp_pndvisp)$dev.expl
f.sq.decomp_pndvisp <- (dev.expl.full - dev.expl.decomp_pndvisp)/(1-dev.expl.full)
#residual peakNDVIdoy
dev.expl.decomp_pndvir <- summary(decomp_pndvir)$dev.expl
f.sq.decomp_pndvir <- (dev.expl.full - dev.expl.decomp_pndvir)/(1-dev.expl.full)


# Cohen's f for TOTAL SOdoy
dSO <- gam(CalfHarvest_AvgMass ~ 
             ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
             ti(density_t) + ti(density_sp) + ti(density_resid) +
             ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
             ti(bodycond_resid, density_resid) + 
             s(elev, by = factor(y_cat)) +
             s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
             s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
             s(peakndvi_doy_resid, k = 5) +
             s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
             s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
           sp = fullmod$sp[-c(22,23,24)], data = nina.data, 
           family = scat(theta = c(3.904,1.128)), method = "REML")
dev.expl.SO <- summary(dSO)$dev.expl
f.sq.SO <- (dev.expl.full - dev.expl.SO)/(1-dev.expl.full)
# Cohen's f for DECOMPOSED SOdoy
decomp_SOt <- gam(CalfHarvest_AvgMass ~ 
                    ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                    ti(density_t) + ti(density_sp) + ti(density_resid) +
                    ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                    ti(bodycond_resid, density_resid) + 
                    s(elev, by = factor(y_cat)) +
                    s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                    s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                    s(peakndvi_doy_resid, k = 5) +
                    s(springo_sp, k = 5) + 
                    s(springo_resid, k = 5) +
                    s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                    s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                  sp = fullmod$sp[-22], data = nina.data, 
                  family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_SOsp <- gam(CalfHarvest_AvgMass ~ 
                     ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                     ti(density_t) + ti(density_sp) + ti(density_resid) +
                     ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                     ti(bodycond_resid, density_resid) + 
                     s(elev, by = factor(y_cat)) +
                     s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                     s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                     s(peakndvi_doy_resid, k = 5) +
                     s(springo_t, k = 5) + 
                     s(springo_resid, k = 5) +
                     s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                     s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                   sp = fullmod$sp[-23], data = nina.data, 
                   family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_SOr <- gam(CalfHarvest_AvgMass ~ 
                    ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                    ti(density_t) + ti(density_sp) + ti(density_resid) +
                    ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                    ti(bodycond_resid, density_resid) + 
                    s(elev, by = factor(y_cat)) +
                    s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                    s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                    s(peakndvi_doy_resid, k = 5) +
                    s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                    s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                    s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                  sp = fullmod$sp[-24], data = nina.data, 
                  family = scat(theta = c(3.904,1.128)), method = "REML")
# temporal SOdoy
dev.expl.decomp_SOt <- summary(decomp_SOt)$dev.expl
f.sq.decomp_SOt <- (dev.expl.full - dev.expl.decomp_SOt)/(1-dev.expl.full)
# spatial SOdoy
dev.expl.decomp_SOsp <- summary(decomp_SOsp)$dev.expl
f.sq.decomp_SOsp <- (dev.expl.full - dev.expl.decomp_SOsp)/(1-dev.expl.full)
#residual SOdoy
dev.expl.decomp_SOr <- summary(decomp_SOr)$dev.expl
f.sq.decomp_SOr <- (dev.expl.full - dev.expl.decomp_SOr)/(1-dev.expl.full)


# Cohen's f for TOTAL snow AUC
dsnow <- gam(CalfHarvest_AvgMass ~ 
               ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
               ti(density_t) + ti(density_sp) + ti(density_resid) +
               ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
               ti(bodycond_resid, density_resid) + 
               s(elev, by = factor(y_cat)) +
               s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
               s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
               s(peakndvi_doy_resid, k = 5) +
               s(springo_t, k = 5) + s(springo_sp, k = 5) + 
               s(springo_resid, k = 5) +
               s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
             sp = fullmod$sp[-c(25,26,27)], data = nina.data, 
             family = scat(theta = c(3.904,1.128)), method = "REML")
dev.expl.snow <- summary(dsnow)$dev.expl
f.sq.snow <- (dev.expl.full - dev.expl.snow)/(1-dev.expl.full)
# Cohen's f for DECOMPOSED snow AUC
decomp_snowt <- gam(CalfHarvest_AvgMass ~ 
                      ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                      ti(density_t) + ti(density_sp) + ti(density_resid) +
                      ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                      ti(bodycond_resid, density_resid) + 
                      s(elev, by = factor(y_cat)) +
                      s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                      s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                      s(peakndvi_doy_resid, k = 5) +
                      s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                      s(springo_resid, k = 5) +
                      s(snowauc_sp) + s(snowauc_resid) +
                      s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                    sp = fullmod$sp[-25], data = nina.data, 
                    family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_snowsp <- gam(CalfHarvest_AvgMass ~ 
                       ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                       ti(density_t) + ti(density_sp) + ti(density_resid) +
                       ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                       ti(bodycond_resid, density_resid) + 
                       s(elev, by = factor(y_cat)) +
                       s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                       s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                       s(peakndvi_doy_resid, k = 5) +
                       s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                       s(springo_resid, k = 5) +
                       s(snowauc_t) + s(snowauc_resid) +
                       s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                     sp = fullmod$sp[-26], data = nina.data, 
                     family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_snowr <- gam(CalfHarvest_AvgMass ~ 
                      ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                      ti(density_t) + ti(density_sp) + ti(density_resid) +
                      ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                      ti(bodycond_resid, density_resid) + 
                      s(elev, by = factor(y_cat)) +
                      s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                      s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                      s(peakndvi_doy_resid, k = 5) +
                      s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                      s(springo_resid, k = 5) +
                      s(snowauc_t) + s(snowauc_sp) + 
                      s(snowstart_t) + s(snowstart_sp) + s(snowstart_resid), 
                    sp = fullmod$sp[-27], data = nina.data, 
                    family = scat(theta = c(3.904,1.128)), method = "REML")
# temporal snow AUC
dev.expl.decomp_snowt <- summary(decomp_snowt)$dev.expl
f.sq.decomp_snowt <- (dev.expl.full - dev.expl.decomp_snowt)/(1-dev.expl.full)
# spatial snow AUC
dev.expl.decomp_snowsp <- summary(decomp_snowsp)$dev.expl
f.sq.decomp_snowsp <- (dev.expl.full - dev.expl.decomp_snowsp)/(1-dev.expl.full)
#residual snow AUC
dev.expl.decomp_snowr <- summary(decomp_snowr)$dev.expl
f.sq.decomp_snowr <- (dev.expl.full - dev.expl.decomp_snowr)/(1-dev.expl.full)


# Cohen's f for TOTAL snow start
dsnowst <- gam(CalfHarvest_AvgMass ~ 
                 ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                 ti(density_t) + ti(density_sp) + ti(density_resid) +
                 ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                 ti(bodycond_resid, density_resid) + 
                 s(elev, by = factor(y_cat)) +
                 s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                 s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                 s(peakndvi_doy_resid, k = 5) +
                 s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                 s(springo_resid, k = 5) +
                 s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid), 
               sp = fullmod$sp[-c(28,29,30)], data = nina.data, 
               family = scat(theta = c(3.904,1.128)), method = "REML")
dev.expl.snowst <- summary(dsnowst)$dev.expl
f.sq.snowst <- (dev.expl.full - dev.expl.snowst)/(1-dev.expl.full)
# Cohen's f for DECOMPOSED snow start
decomp_snowstt <- gam(CalfHarvest_AvgMass ~ 
                        ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                        ti(density_t) + ti(density_sp) + ti(density_resid) +
                        ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                        ti(bodycond_resid, density_resid) + 
                        s(elev, by = factor(y_cat)) +
                        s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                        s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                        s(peakndvi_doy_resid, k = 5) +
                        s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                        s(springo_resid, k = 5) +
                        s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                        s(snowstart_sp) + s(snowstart_resid), 
                      sp = fullmod$sp[-28], data = nina.data, 
                      family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_snowstsp <- gam(CalfHarvest_AvgMass ~ 
                         ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                         ti(density_t) + ti(density_sp) + ti(density_resid) +
                         ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                         ti(bodycond_resid, density_resid) + 
                         s(elev, by = factor(y_cat)) +
                         s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                         s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                         s(peakndvi_doy_resid, k = 5) +
                         s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                         s(springo_resid, k = 5) +
                         s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                         s(snowstart_t) + s(snowstart_resid), 
                       sp = fullmod$sp[-29], data = nina.data, 
                       family = scat(theta = c(3.904,1.128)), method = "REML")
decomp_snowstr <- gam(CalfHarvest_AvgMass ~ 
                        ti(bodycond_t) + ti(bodycond_sp) + ti(bodycond_resid) + 
                        ti(density_t) + ti(density_sp) + ti(density_resid) +
                        ti(bodycond_t, density_t) + ti(bodycond_sp, density_sp) + 
                        ti(bodycond_resid, density_resid) + 
                        s(elev, by = factor(y_cat)) +
                        s(meanndvi_t) + s(meanndvi_sp) + s(meanndvi_resid) +
                        s(peakndvi_doy_t, k = 5) + s(peakndvi_doy_sp, k = 5) + 
                        s(peakndvi_doy_resid, k = 5) +
                        s(springo_t, k = 5) + s(springo_sp, k = 5) + 
                        s(springo_resid, k = 5) +
                        s(snowauc_t) + s(snowauc_sp) + s(snowauc_resid) +
                        s(snowstart_t) + s(snowstart_sp), 
                      sp = fullmod$sp[-30], data = nina.data, 
                      family = scat(theta = c(3.904,1.128)), method = "REML")
# temporal snow start
dev.expl.decomp_snowstt <- summary(decomp_snowstt)$dev.expl
f.sq.decomp_snowstt <- (dev.expl.full - dev.expl.decomp_snowstt)/(1-dev.expl.full)
# spatial snow start
dev.expl.decomp_snowstsp <- summary(decomp_snowstsp)$dev.expl
f.sq.decomp_snowstsp <- (dev.expl.full - dev.expl.decomp_snowstsp)/(1-dev.expl.full)
#residual snow start
dev.expl.decomp_snowstr <- summary(decomp_snowstr)$dev.expl
f.sq.decomp_snowstr <- (dev.expl.full - dev.expl.decomp_snowstr)/(1-dev.expl.full)


## Proportion of deviance explained
## Null model
nullmod <- gam(CalfHarvest_AvgMass ~ 1, data = nina.data, family = scat, method = "REML")
## 

# deviances explained for body mass in prev yr (bodycond)
(deviance(dbcond) - deviance(fullmod))/deviance(nullmod) ## total prop explained by bodycond
(deviance(decomp_bt) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_t)
(deviance(decomp_bsp) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_sp)
(deviance(decomp_br) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_resid)

# deviance explained for population density
(deviance(dpop_density) - deviance(fullmod))/deviance(nullmod) ## total prop explained by pop density
(deviance(decomp_nt) - deviance(fullmod))/deviance(nullmod) ## prop explained by temporal pop density
(deviance(decomp_nsp) - deviance(fullmod))/deviance(nullmod) ## prop explained by spatial pop density
(deviance(decomp_nr) - deviance(fullmod))/deviance(nullmod) ## prop explained by residual pop density

# deviance explained for b x pop interaction
(deviance(dbody_pop) - deviance(fullmod))/deviance(nullmod) ## total prop explained by pop density
(deviance(decomp_bnt) - deviance(fullmod))/deviance(nullmod) ## prop explained by temporal pop density
(deviance(decomp_bnsp) - deviance(fullmod))/deviance(nullmod) ## prop explained by spatial pop density
(deviance(decomp_bnr) - deviance(fullmod))/deviance(nullmod) ## prop explained by residual pop density

# deviance explained for elevation 
(deviance(delev) - deviance(fullmod))/deviance(nullmod)

# deviance explained for national mean NDVI
(deviance(dndvi) - deviance(fullmod))/deviance(nullmod) ## total prop explained by meanNDVI
(deviance(decomp_ndvit) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(meanNDVI_t)
(deviance(decomp_ndvisp) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(meanNDVI_sp)
(deviance(decomp_ndvir) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(meanNDVI_resid)

# deviances explained for peakNDVIdoy
(deviance(dpndvi) - deviance(fullmod))/deviance(nullmod) ## total prop explained by bodycond
(deviance(decomp_pndvit) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_t)
(deviance(decomp_pndvisp) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_sp)
(deviance(decomp_pndvir) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_resid)

# deviances explained for spring onset
(deviance(dSO) - deviance(fullmod))/deviance(nullmod) ## total prop explained by bodycond
(deviance(decomp_SOt) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_t)
(deviance(decomp_SOsp) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_sp)
(deviance(decomp_SOr) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_resid)

# deviances explained for snow AUC
(deviance(dsnow) - deviance(fullmod))/deviance(nullmod) ## total prop explained by bodycond
(deviance(decomp_snowt) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_t)
(deviance(decomp_snowsp) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_sp)
(deviance(decomp_snowr) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_resid)

# deviances explained for snot start DOY
(deviance(dsnowst) - deviance(fullmod))/deviance(nullmod) ## total prop explained by bodycond
(deviance(decomp_snowstt) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_t)
(deviance(decomp_snowstsp) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_sp)
(deviance(decomp_snowstr) - deviance(fullmod))/deviance(nullmod) ## prop explained by s(bodycond_resid)




