#install raster data packages
install.packages("raster")
#load library of packages
library(raster)
library(ggplot2)
library(rgdal)
#read in oneida data folder
dirR <- "\\Users\\clare\\Documents\\Data Science\\Activity 8\\a08.zip\\a08\\oneida"
#read in sentinel data
rdatB2 <- raster(paste0(dirR,"\\sentinel\\T18TVN_20190814T154911_B02_20m.tif"))
