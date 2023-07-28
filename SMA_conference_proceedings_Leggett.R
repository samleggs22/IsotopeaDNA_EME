#Graphs and Stats for "Isotopes and aDNA" paper in SMA conference proceedings
#S.A. Leggett,  Uni of Edinburgh. 
# 2022-23

library(readr)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(ggExtra)
library(viridis)
library(ggsci)
library(ggpubr)
library(dplyr)
library(latex2exp)
library(gridExtra)
library(ggpmisc)
library(forcats)
library(ggridges)
library(reshape2)
library(rworldmap)
library(rworldxtra)
library(scatterpie)
library(maps)
library(maptools)
library(readr)
library(readxl)
library("rgdal")
library("raster")
library("maps")
library("maptools")
library("rasterVis")
library("mvnmle")
library("mixtools")
library("fossil")
library(sp)
library(maptools)
library(rgdal)
library(classInt)
library(assignR)
library(colorRamps)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cellWise)
library(ggsci)
library(ggpmisc)
library(ggpubr)
library(ggridges)
library(magrittr)
library(BEST) #check BEST package for other dependencies
library(raincloudplots)
library(latex2exp)
library(viridis)
library(ggrepel)
library(cowplot)
library(lavaan)
library(plyr)
library(caTools)
library(bitops)
library(Hmisc)
library(ggdist)
library(tidyquant)
library(see)

###############
# SET WORKING DIRECTORY 
###############
setwd("")


##############################################
## PIECHART MAPS ##
##############################################

# map with pie charts of % non-locals based on DELTA18O dw-map values
library(readxl)
K3 <- read_excel("~/piechart_map_england.xlsx")
View(K3)

uk_ireland_map <- c("UK", "Ireland")
# Retrieve the map data
ukie.eu.maps <- map_data("world", region = uk_ireland_map)

coldwarmlocality<-ggplot(ukie.eu.maps, aes(long, lat))+
  theme_bw()+
  geom_map(map=ukie.eu.maps, aes(map_id=region), fill="grey", color="grey") +
  coord_quickmap()+geom_scatterpie(aes(x=LON, y=LAT, group=INDI), data=K3,
                                   cols=c("P1", "P2", "P3"), color=NA, pie_scale = 6) + coord_equal() + scale_fill_manual(values = c("#1E88E5", "#FFC107", "#D81B60"), name = expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030) Group")), labels = c("Cold non-local", "'Local'", "Warm non-local"))+
  ggsn::scalebar(x.min=-10, x.max=-6.25, y.min=49.375, y.max= 51.5, dist = 100, dist_unit = "km", transform = TRUE, model = "WGS84", height = 0.08, st.dist=0.1)+
  ggsn::north(x.min=1.25, x.max=2.25, y.min=60.0, y.max= 61.25, location = "topright", scale = 1.0, symbol = 12)+
  theme(axis.text=element_blank(),axis.title=element_blank(),axis.ticks = element_blank(),legend.text = element_text(size=30), legend.title = element_blank(), legend.position = c(0.25,0.9))
coldwarmlocality

# map with pie charts of % non-locals based on both oxygen and strontium proxies

K4 <- read_excel("~/Piechart_local_nonlocal.xlsx")
View(K4)

uk_ireland_map <- c("UK", "Ireland")
# Retrieve the map data
ukie.eu.maps <- map_data("world", region = uk_ireland_map)

percentlocal<-ggplot(ukie.eu.maps, aes(long, lat)) +
  theme_bw()+
  geom_map(map=ukie.eu.maps, aes(map_id=region), fill="grey", color="grey") +
  coord_quickmap()+geom_scatterpie(aes(x=LON, y=LAT, group=INDI), data=K4,
                                   cols=c("P1", "P2"), color=NA, pie_scale = 6) + coord_equal() + scale_fill_manual(values = c("darkslategray","#CC79A7"), labels = c("Local", "Non-local"))+
  ggsn::scalebar(x.min=-10, x.max=-6.25, y.min=49.375, y.max= 51.5, dist = 100, dist_unit = "km", transform = TRUE, model = "WGS84", height = 0.08, st.dist=0.1)+
  ggsn::north(x.min=1.25, x.max=2.25, y.min=60.0, y.max= 61.25, location = "topright", scale = 1.0, symbol = 12)+
  theme(axis.text=element_blank(),axis.title=element_blank(),axis.ticks = element_blank(),legend.text = element_text(size=30), legend.title = element_blank(), legend.position = c(0.25,0.9))
percentlocal


#map for women

K5 <- read_excel("~/Women_Piechart_Region.xlsx")
View(K5)

uk_ireland_map <- c("UK", "Ireland")
# Retrieve the map data
ukie.eu.maps <- map_data("world", region = uk_ireland_map)

Fpercentlocal<-ggplot(ukie.eu.maps, aes(long, lat)) +
  theme_bw()+
  geom_map(map=ukie.eu.maps, aes(map_id=region), fill="grey", color="grey") +
  coord_quickmap()+geom_scatterpie(aes(x=LON, y=LAT, group=INDI), data=K5,
                                   cols=c("P1", "P2"), color=NA, pie_scale = 6) + coord_equal() + scale_fill_manual(values = c("darkslategray","#CC79A7"), labels = c("Local", "Non-local"))+
  ggsn::scalebar(x.min=-10, x.max=-6.25, y.min=49.375, y.max= 51.5, dist = 100, dist_unit = "km", transform = TRUE, model = "WGS84", height = 0.08, st.dist=0.1)+
  ggsn::north(x.min=1.25, x.max=2.25, y.min=60.0, y.max= 61.25, location = "topright", scale = 1.0, symbol = 12)+
  theme(axis.text=element_blank(),axis.title=element_blank(),axis.ticks = element_blank(),legend.text = element_text(size=30), legend.title = element_blank(), legend.position = c(0.25,0.9))
Fpercentlocal


#map for men
K6 <- read_excel("~/Men_Piechart_Region.xlsx")
View(K6)

uk_ireland_map <- c("UK", "Ireland")
# Retrieve the map data
ukie.eu.maps <- map_data("world", region = uk_ireland_map)

Mpercentlocal<-ggplot(ukie.eu.maps, aes(long, lat)) +
  theme_bw()+
  geom_map(map=ukie.eu.maps, aes(map_id=region), fill="grey", color="grey") +
  coord_quickmap()+geom_scatterpie(aes(x=LON, y=LAT, group=INDI), data=K6,
                                   cols=c("P1", "P2"), color=NA, pie_scale = 6) + coord_equal() + scale_fill_manual(values = c("darkslategray","#CC79A7"), labels = c("Local", "Non-local"))+
  ggsn::scalebar(x.min=-10, x.max=-6.25, y.min=49.375, y.max= 51.5, dist = 100, dist_unit = "km", transform = TRUE, model = "WGS84", height = 0.08, st.dist=0.1)+
  ggsn::north(x.min=1.25, x.max=2.25, y.min=60.0, y.max= 61.25, location = "topright", scale = 1.0, symbol = 12)+
  theme(axis.text=element_blank(),axis.title=element_blank(),axis.ticks = element_blank(),legend.text = element_text(size=30), legend.title = element_blank(), legend.position = c(0.25,0.9))
Mpercentlocal


#arrange all 4 maps 

Figure2<-ggarrange(percentlocal,
                   coldwarmlocality,
                   Fpercentlocal,
                   Mpercentlocal,
                   labels = c("A", "B", "C","D"), 
                   font.label=list(size=45), hjust = -0.5, vjust=3, ncol = 2, nrow = 2, common.legend = FALSE)
Figure2

ggsave(filename="Fig_2.tiff", plot=Figure2, device="tiff",
       path="~/", height=20, width=20, units="in", dpi=600, limitsize = FALSE)

ggsave(filename="Fig_2.jpeg", plot=Figure2, device="jpeg",
       path="~/", height=30, width=20, units="in", dpi=600, limitsize = FALSE)


##############################################
## Probabilistic MAPS ##
##############################################

#West Heslerton 19 individuals

####################################################################################
# Isoscape probability mapping from Colleter et al 2021 and Bataille et al 2021
####################################################################################

####################################################################################
# Define new functions
####################################################################################

## function returns raster of posterior probabilities for univariate normal data
calcCellProb <- function(x,isoscape,std){
  m <- getValues(isoscape)
  s <- getValues(std) # use this is you have raster of variances
  m <- dnorm(x,mean=m,sd=s)
  cell.dens <- setValues(isoscape,m)
  return(cell.dens)
}

## function returns raster of posterior probabilities for bivariate normal data
## x is the unknown tissue of interest, will have two values, one for each isotope
## m is a 2-D vector, all the values in the raster for each isotope
## v is the same as m, but for variances
## r is a single number - the covariance. Can be vector if estimated as non-stationary
## ras is a raster that will serve as a template for the final product
calcCellProb2D <- function(x,m,v,r,ras) {
  pd <- 1/(2*pi*sqrt(v[,1])*sqrt(v[,2])*sqrt(1-r^2))*exp(-(1/(2*(1-r^2)))*
                                                           ((x[1]-m[,1])^2/v[,1]+(x[2]-m[,2])^2/v[,2]-(2*r*(x[1]-m[,1])*
                                                                                                         (x[2]-m[,2]))/(sqrt(v[,1])*sqrt(v[,2]))))
  pdras <- setValues(ras,pd)
  return(pdras)
}

## function returns raster of posterior probability distribution
calcPostProb <- function(x){
  pp <- x/cellStats(x,sum)
  return(pp)
}


## function to get normalized cell probabilites
calcNormProb <- function(x){
  np <- x/cellStats(x,max)
  return(np)
}


####################################################################################
## Data input: read in tissue isotope data, GIS raster models
####################################################################################
iso.data <- read.csv("WestHeslerton_isoscape.csv") # tissue isotope data

### Download d18O isoscape rasters at RCWIP http://www-naweb.iaea.org/napc/ih/IHS_resources_rcwip.html
### input the annual amount-weighted mean and sd from RCWIP

rain_d18O_aaw <- raster("~/O18_H2_Ann_GS/RCWIP_grid_5_14_100_20130502.tif")  # this is baseline isoscape
rain_d18O_err_aaw <- raster("~/O18_H2_Ann_GS/RCWIP_grid_err_5_14_100_20130502.tif")  # this is baseline isoscape

### Download 87Sr/86Sr isoscape rasters and uncertainty at https://drive.google.com/drive/folders/1g9rCGo3Kd3hz2o5JKkSbgNsGJclvsuQm?usp=sharing
### See Bataille et al. 2020 for details
rf_sr<-raster("~/rf_plantsoilmammal1.tif")
rf_sr_err <-raster("~/srse.tif")

#Clip data to study area Europe
#europe<-as.vector(c(-500000,1000000,5000000,6500000))
europe<-as.vector(c(-860000,2700000,4300000,7900000))
rangemap<-crop(rf_sr/rf_sr, europe, snap='near')
writeRaster(rangemap, "range.tif",overwrite=TRUE)
sr<-rf_sr*rangemap
sr.se<-rf_sr_err*rangemap

###Reproject d18O isoscape to Sr projection and resolution
d18O <- projectRaster(rain_d18O_aaw,sr, method="bilinear")
d18O.se<-projectRaster(rain_d18O_err_aaw,sr, method="bilinear")+1###Add 1 per mile uncertainty due to regression equation

#re-project the site coordinates to get the site to plot (thanks to P. Schauer)
myproj <- proj4string(origins)#get the CRS string
iso.data.sp <- iso.data #make a copy
coordinates(iso.data.sp) <- ~ Long + Lat #specify the coordinates
proj4string(iso.data.sp) <- CRS("+proj=longlat +datum=WGS84") #project first
iso.data.sp <- spTransform(iso.data.sp, myproj) #then re-project using the projection from the base map

wrld_simpl2 <- wrld_simpl #make a copy
wrld_simpl2 <- spTransform(wrld_simpl2, myproj) #this is to add in country borders etc. if you want to as they have the same projection issues as the site coords

####################################################################################
# Don't have d34S data so not including it
####################################################################################

###########################d18O single isotope assignments from Levinson #########################################################
####################################################################################
distance.list <- vector("list", length(iso.data[,1]))
origins <- stack()
for (i in seq(along=iso.data[,1])){
  pp <- calcCellProb(iso.data$d18Odwlevinson[i],d18O,d18O.se) # compute probs
  np <- calcNormProb(pp)
  origins <- raster::stack(origins,np)
}

#re-project the site coordinates to get the site to plot (thanks to P. Schauer)
myproj <- proj4string(origins)#get the CRS string
iso.data.sp <- iso.data #make a copy
coordinates(iso.data.sp) <- ~ Long + Lat #specify the coordinates
proj4string(iso.data.sp) <- CRS("+proj=longlat +datum=WGS84") #project first
iso.data.sp <- spTransform(iso.data.sp, myproj) #then re-project using the projection from the base map

wrld_simpl2 <- wrld_simpl #make a copy
wrld_simpl2 <- spTransform(wrld_simpl2, myproj) #this is to add in country borders etc. if you want to as they have the same projection issues as the site coords

## summary map plots
pdf("maps_d18O_annual_westheslerton_levinson.pdf")
opar<-par()
par(mfrow=c(1,1),mar=c(2,3,2,2)+0.01)
xl<-length(iso.data[,1])
for (i in seq(along=iso.data[,1])){
  plot(origins[[i]],axes = FALSE)
  #plot(wrld_simpl,add=T)
  # plot known origin and individual label info
  #points(s.data$Long,s.data$Lat,pch=21, col="black", bg=colcode, lwd=0.4, cex=symbol.size)
  points(iso.data.sp[i,],col="black",cex=0.5, pch=16)
  #text(-5,47,paste("??34S=",iso.data$d34S[i],"???"),cex=0.70)
  text(-55000, 7650000,paste(iso.data$ID[i]),cex=0.70)
  #legend(-13, 65, legend=names(attr(colcode, "table")),
  #      fill=attr(colcode, "palette"), cex=0.6, bty="n")
  #hist(distance.list[[i]],xlim=c(0,xl),xlab="",ylab="",main="")
}
par(opar)
dev.off()
rm(i)

###Store posterior probability maps in raster stack
O<-origins



###########################d18O single isotope assignments from Chenery #########################################################
####################################################################################
distance.list <- vector("list", length(iso.data[,1]))
origins <- stack()
for (i in seq(along=iso.data[,1])){
  pp <- calcCellProb(iso.data$d18Odwchenery[i],d18O,d18O.se) # compute probs
  np <- calcNormProb(pp)
  origins <- raster::stack(origins,np)
}

#re-project the site coordinates to get the site to plot (thanks to P. Schauer)
myproj <- proj4string(origins)#get the CRS string
iso.data.sp <- iso.data #make a copy
coordinates(iso.data.sp) <- ~ Long + Lat #specify the coordinates
proj4string(iso.data.sp) <- CRS("+proj=longlat +datum=WGS84") #project first
iso.data.sp <- spTransform(iso.data.sp, myproj) #then re-project using the projection from the base map

wrld_simpl2 <- wrld_simpl #make a copy
wrld_simpl2 <- spTransform(wrld_simpl2, myproj) #this is to add in country borders etc. if you want to as they have the same projection issues as the site coords

## summary map plots
pdf("maps_d18O_annual_westheslerton_chenery.pdf")
opar<-par()
par(mfrow=c(1,1),mar=c(2,3,2,2)+0.01)
xl<-length(iso.data[,1])
for (i in seq(along=iso.data[,1])){
  plot(origins[[i]],axes = FALSE)
  #plot(wrld_simpl,add=T)
  # plot known origin and individual label info
  #points(s.data$Long,s.data$Lat,pch=21, col="black", bg=colcode, lwd=0.4, cex=symbol.size)
  points(iso.data.sp[i,],col="black",cex=0.5, pch=16)
  #text(-5,47,paste("??34S=",iso.data$d34S[i],"???"),cex=0.70)
  text(-55000, 7650000,paste(iso.data$ID[i]),cex=0.70)
  #legend(-13, 65, legend=names(attr(colcode, "table")),
  #      fill=attr(colcode, "palette"), cex=0.6, bty="n")
  #hist(distance.list[[i]],xlim=c(0,xl),xlab="",ylab="",main="")
}
par(opar)
dev.off()
rm(i)

###Store posterior probability maps in raster stack
O2<-origins




###########################SR single isotope assignments#########################################################

distance.list <- vector("list", length(iso.data[,1]))
origins <- stack()
for (i in seq(along=iso.data[,1])){
  pp <- calcCellProb(iso.data$Sr[i],sr,sr.se) # compute probs
  np <- calcNormProb(pp)
  origins <- raster::stack(origins,np)
  
}

## summary map plots
pdf("maps_sr_westheslerton.pdf")
opar<-par()
par(mfrow=c(1,1),mar=c(2,3,2,2)+0.01)
xl<-length(iso.data[,1])
for (i in seq(along=iso.data[,1])){
  plot(origins[[i]],axes = FALSE)
  #plot(wrld_simpl,add=T)
  # plot known origin and individual label info
  #points(s.data$Long,s.data$Lat,pch=21, col="black", bg=colcode, lwd=0.4, cex=symbol.size)
  points(iso.data.sp[i,],col="black",cex=0.5, pch=16)
  #text(-5,47,paste("??34S=",iso.data$d34S[i],"???"),cex=0.70)
  text(-55000, 7650000,paste(iso.data$ID[i]),cex=0.70)
  #legend(-13, 65, legend=names(attr(colcode, "table")),
  #      fill=attr(colcode, "palette"), cex=0.6, bty="n")
  #hist(distance.list[[i]],xlim=c(0,xl),xlab="",ylab="",main="")
}
par(opar)
dev.off()
rm(i)

###Store posterior probability maps in raster stack
Sr87_86Sr<-origins

###########################d18O and 87Sr/86Sr dual assignments#########################################################
###Combine d18O and 87Sr/86Sr assuming independence, using the Levinson outputs
OSr<-O*Sr87_86Sr

pdf("maps_OSr_westheslerton_levinson.pdf")
opar<-par()
par(mfrow=c(1,1),mar=c(2,3,2,2)+0.01)
xl<-length(iso.data[,1])
for (i in seq(along=iso.data[,1])){
  plot(OSr[[i]],axes = FALSE)
  #plot(wrld_simpl,add=T)
  # plot known origin and individual label info
  #points(s.data$Long,s.data$Lat,pch=21, col="black", bg=colcode, lwd=0.4, cex=symbol.size)
  points(iso.data.sp[i,],col="black",cex=0.5, pch=16)
  #text(-5,47,paste("??34S=",iso.data$d34S[i],"???"),cex=0.70)
  text(-55000, 7650000,paste(iso.data$ID[i]),cex=0.70)
  #legend(-13, 65, legend=names(attr(colcode, "table")),
  #      fill=attr(colcode, "palette"), cex=0.6, bty="n")
  #hist(distance.list[[i]],xlim=c(0,xl),xlab="",ylab="",main="")
}
par(opar)
dev.off()
rm(i)

###########################d18O and 87Sr/86Sr dual assignments#########################################################
###Combine d18O and 87Sr/86Sr assuming independence, using the Levinson outputs
OSr2<-O2*Sr87_86Sr

pdf("maps_OSr_westheslerton_chenery.pdf")
opar<-par()
par(mfrow=c(1,1),mar=c(2,3,2,2)+0.01)
xl<-length(iso.data[,1])
for (i in seq(along=iso.data[,1])){
  plot(OSr2[[i]],axes = FALSE)
  #plot(wrld_simpl,add=T)
  # plot known origin and individual label info
  #points(s.data$Long,s.data$Lat,pch=21, col="black", bg=colcode, lwd=0.4, cex=symbol.size)
  points(iso.data.sp[i,],col="black",cex=0.5, pch=16)
  #text(-5,47,paste("??34S=",iso.data$d34S[i],"???"),cex=0.70)
  text(-55000, 7650000,paste(iso.data$ID[i]),cex=0.70)
  #legend(-13, 65, legend=names(attr(colcode, "table")),
  #      fill=attr(colcode, "palette"), cex=0.6, bty="n")
  #hist(distance.list[[i]],xlim=c(0,xl),xlab="",ylab="",main="")
}
par(opar)
dev.off()
rm(i)

######## Buckland Dover maps ########
iso.data <- read.csv("DBC_isoscapes.csv") # tissue isotope data


### Download d18O isoscape rasters at RCWIP http://www-naweb.iaea.org/napc/ih/IHS_resources_rcwip.html
### input the annual amount-weighted mean and sd from RCWIP

rain_d18O_aaw <- raster("~/O18_H2_Ann_GS/RCWIP_grid_5_14_100_20130502.tif")  # this is baseline isoscape
rain_d18O_err_aaw <- raster("~/O18_H2_Ann_GS/RCWIP_grid_err_5_14_100_20130502.tif")  # this is baseline isoscape

### Download 87Sr/86Sr isoscape rasters and uncertainty at https://drive.google.com/drive/folders/1g9rCGo3Kd3hz2o5JKkSbgNsGJclvsuQm?usp=sharing
### See Bataille et al. 2020 for details
rf_sr<-raster("~/rf_plantsoilmammal1.tif")
rf_sr_err <-raster("~/srse.tif")

#Clip data to study area Europe
#europe<-as.vector(c(-500000,1000000,5000000,6500000))
europe<-as.vector(c(-860000,2700000,4300000,7900000))
rangemap<-crop(rf_sr/rf_sr, europe, snap='near')
writeRaster(rangemap, "range.tif",overwrite=TRUE)
sr<-rf_sr*rangemap
sr.se<-rf_sr_err*rangemap

###Reproject d18O isoscape to Sr projection and resolution
d18O <- projectRaster(rain_d18O_aaw,sr, method="bilinear")
d18O.se<-projectRaster(rain_d18O_err_aaw,sr, method="bilinear")+1###Add 1 per mile uncertainty due to regression equation



###########################d18O single isotope assignments from Chenery #########################################################
####################################################################################
distance.list <- vector("list", length(iso.data[,1]))
origins <- stack()
for (i in seq(along=iso.data[,1])){
  pp <- calcCellProb(iso.data$d18Owchenery[i],d18O,d18O.se) # compute probs
  np <- calcNormProb(pp)
  origins <- raster::stack(origins,np)
}

#re-project the site coordinates to get the site to plot (thanks to P. Schauer)
myproj <- proj4string(origins)#get the CRS string
iso.data.sp <- iso.data #make a copy
coordinates(iso.data.sp) <- ~ Long + Lat #specify the coordinates
proj4string(iso.data.sp) <- CRS("+proj=longlat +datum=WGS84") #project first
iso.data.sp <- spTransform(iso.data.sp, myproj) #then re-project using the projection from the base map

wrld_simpl2 <- wrld_simpl #make a copy
wrld_simpl2 <- spTransform(wrld_simpl2, myproj) #this is to add in country borders etc. if you want to as they have the same projection issues as the site coords

## summary map plots
pdf("maps_d18O_annual_DoverBuckland_chenery.pdf")
opar<-par()
par(mfrow=c(1,1),mar=c(2,3,2,2)+0.01)
xl<-length(iso.data[,1])
for (i in seq(along=iso.data[,1])){
  plot(origins[[i]],axes = FALSE)
  #plot(wrld_simpl,add=T)
  # plot known origin and individual label info
  #points(s.data$Long,s.data$Lat,pch=21, col="black", bg=colcode, lwd=0.4, cex=symbol.size)
  points(iso.data.sp[i,],col="black",cex=0.5, pch=16)
  #text(-5,47,paste("??34S=",iso.data$d34S[i],"???"),cex=0.70)
  text(-55000, 7650000,paste(iso.data$ID[i]),cex=0.70)
  #legend(-13, 65, legend=names(attr(colcode, "table")),
  #      fill=attr(colcode, "palette"), cex=0.6, bty="n")
  #hist(distance.list[[i]],xlim=c(0,xl),xlab="",ylab="",main="")
}
par(opar)
dev.off()
rm(i)

###Store posterior probability maps in raster stack
O3<-origins


##### The End #####
