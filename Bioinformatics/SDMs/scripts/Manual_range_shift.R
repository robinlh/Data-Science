############################################################################ ##
## Load packages and world shapefile                                       ####
############################################################################ ##
library(stringr)
require(spatial.tools)
require(tidyverse)
library(maptools)
require(here)
library(raster)
library(rgdal)
require(plotrix)
library(viridis)

worldshp <- data(wrld_simpl)

############################################################################ ##
###############################################################################
## Choose Species                                                          ####

#Species current

protea_compacta_current <- raster(here("Protea compacta/Soils_cape_no8/Protea compacta_current_bin_50.tif"))

#Species 2040
protea_compacta_cc_2040 <- raster(here("Data\\Protea compacta\\Soils_cape_no8\\Protea compacta_cc2040_bin_50.tif"))
protea_compacta_cn_2040 <- raster(here("Data\\Protea compacta\\Soils_cape_no8\\Protea compacta_cn2040_bin_50.tif"))
protea_compacta_gf_2040 <- raster(here("Data\\Protea compacta\\Soils_cape_no8\\Protea compacta_gf2040_bin_50.tif"))
protea_compacta_he_2040 <- raster(here("Data\\Protea compacta\\Soils_cape_no8\\Protea compacta_he2040_bin_50.tif"))
protea_compacta_mp_2040 <- raster(here("Data\\Protea compacta\\Soils_cape_no8\\Protea compacta_mp2040_bin_50.tif"))

#2070
protea_compacta_cc_2070 <- raster(here("Data\\Protea compacta\\Soils_cape_no8\\Protea compacta_cc2070_bin_50.tif"))
protea_compacta_cn_2070 <- raster(here("Data\\Protea compacta\\Soils_cape_no8\\Protea compacta_cn2070_bin_50.tif"))
protea_compacta_gf_2070 <- raster(here("Data\\Protea compacta\\Soils_cape_no8\\Protea compacta_gf2070_bin_50.tif"))
protea_compacta_he_2070 <- raster(here("Data\\Protea compacta\\Soils_cape_no8\\Protea compacta_he2070_bin_50.tif"))
protea_compacta_mp_2070 <- raster(here("Data\\Protea compacta\\Soils_cape_no8\\Protea compacta_mp2070_bin_50.tif"))

############################################################################ ##

##Current-2040 comparison
# Raster for current distribution
tif_current<- protea_compacta_current
# Raster for future distribution
tif_cc2040_bin <- protea_compacta_cc_2040

#When comparing 2070 with current or 2040 it has to be cropped in order to be compared
# since 2070 raster is different size. THerefore use code below for that:

###current-2070 comparison
# Raster for current distribution
#tif_current <- protea_compacta_current
# Raster for future distribution
#tif_cc2070_bin_raw <- protea_compacta_cc_2070

#masking the 2070 with other .tif in order to get rid of NAs
#tif_cc2070_bin <- mask(tif_cc2070_bin_raw,tif_cc_2040_bin)


############################################################################ ##
## Choose Country                                                          ####
############################################################################ ##

selected_country <- "South Africa"
wstcp <- shapefile(paste0("Data/sa_PROVINCE__WESTERN CAPE.shp")) 
country <- wstcp


{
  ############################################################################ ##
  ## Stack and mask Rasters to selected country                              ####
  ############################################################################ ##
  
  country <- wstcp
  spp <- stack(tif_current,tif_cc2040_bin)
  spp_selected_country <- mask(crop(spp,extent(country)),country)
  
  ## Plot current and future range of selected species
  #plot(spp_selected_country)
  
  ## Create Table for Proportion Changes
  Prop <- as.data.frame(cellStats(spp_selected_country,stat = 'sum'))
  colnames(Prop) <- "Presence"
  Prop$Total <- (ncell(spp_selected_country))
  Prop$Percentage <- as.integer((Prop$Presence/Prop$Total)*100)
  rownames(Prop)<-c("Current","Future")
  Prop <- t(Prop)
  
  
  ############################################################################ ##
  ## Transition between current and future range                             ####
  ############################################################################ ##
  
  ## Create matrix for classification
  m <- matrix(c(-Inf,0,0,0,13,1,13,20,2,20,23,4,23,Inf,3),
              ncol=3,byrow=TRUE)
  ## Create values for transition classification
  curr_spp_country = (spp_selected_country[[1]]+1)*2
  fut_spp_country = (spp_selected_country[[2]]+1)*10
  curr_fut_country =curr_spp_country+fut_spp_country
  curr_fut_clas_country <- reclassify(curr_fut_country,m)
  ## Plot country transition map
  #plot(curr_fut_clas_country)
  #plot(country,add=TRUE)
  
  ## Stack current, future and transition maps
  spp_country_tran <- stack(spp_selected_country,curr_fut_clas_country)
  
  ## Plot current, future and transition maps
  ## And display table of current, future and transition values
  
  breaks <- seq(0.5, 4.5, by = 1)
  cols <- viridis(length(breaks)-1)
  
  par(mfrow = c(2,2))
  plot(spp_country_tran[[1]],main = "Current")
  plot(spp_country_tran[[2]],main = "Future")
  plot(spp_country_tran[[3]],breaks = breaks, col = cols,main = "Range Shift")
  
  #plot(spp_country_tran,breaks = breaks, col = cols, main = c("Current","Future","Range Shift"))
  
  
  # addtable2plot(1 ,-100,Prop,bty="o",display.rownames=TRUE,hlines=TRUE,
  #               vlines=TRUE)
  
}
###############################################################################

layout(1)
plot(country)

plot(spp_country_tran[[1]],main = "current",box = FALSE, axes = FALSE)
plot(country, add = TRUE)



plot(spp_country_tran[[2]],main = "2040",box = FALSE, axes = FALSE)
plot(country, add = TRUE)



plot(spp_country_tran[[3]],breaks = breaks, col = c("azure1","azure4","black","purple"),main = "Range Shift (current-2040)",box = FALSE, axes = FALSE)
plot(country, add = TRUE)









#####
kuduhist <- data.frame(rasterToPoints(spp_country_tran))
kuduhist$layer <- as.factor(kuduhist$layer)

#Presence in time 1
(sum(kuduhist[3]))/(count(kuduhist[3]))*100
#Presence in time 2
(sum(kuduhist[4]))/(count(kuduhist[3]))*100

#Use values in summary to plug into following
kudsum <- summary(kuduhist)
kudsum

#Use values 1, 2, 3 and 4 from summary to work out following

#1 Total number absent and still absent
83524/(count(kuduhist[3]))*100

#2 % Loss from time diff
1076/(count(kuduhist[3]))*100

#3 % There in both time periods or range overlap
1771/(count(kuduhist[3]))*100

#4 % gain in time period
304/(count(kuduhist[3]))*100