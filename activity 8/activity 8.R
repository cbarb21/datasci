#install raster data packages
install.packages("raster")
#load library of packages
library(raster)
library(ggplot2)
library(rgdal)
#read in oneida data folder
dirR <- "\\Users\\clare\\Documents\\Data Science\\Activity 8\\a08\\a08\\oneida"
#read in sentinel data
rdatB2 <- raster(paste0(dirR,"\\sentinel\\T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"\\sentinel\\T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"\\sentinel\\T18TVN_20190814T154911_B04_20m.tif"))
rdatB8 <- raster(paste0(dirR,"\\sentinel\\T18TVN_20190814T154911_B08_20m.tif"))
#plot reflectance of blue light
plot(rdatB2/10000)
#create stack of color rasters
rgbS <- stack(rdatB4, rdatB3, rdatB2)/10000
#plot stack to create composite true color image
plotRGB(rgbS, scale=2)
#add contrast stretch to make image easier to see
plotRGB(rgbS, stretch="lin")
#use maxpixels to get full resolution image
plotRGB(rgbS, stretch="lin", maxpixels=rgbS@nrows*rgbS@ncols)
#Question 3
#find total number of pixels in raster 
rgbS@nrows*rgbS@ncols
#Question 4
#create false color stack of NRG
nrgS <- stack(rdatB8, rdatB4, rdatB3)
#create false color map using NRG stack
plotRGB(nrgS, stretch="lin", maxpixels=nrgS@nrows*nrgS@ncols)
#calculate NDVI using N-R/N+R
NDVI <- (rdatB8-rdatB4)/(rdatB8+rdatB4)
#visualize NDVI for Oneida Lake
plot(NDVI)
#read in landcover points data
algae <- readOGR(paste0(dirR,"\\Oneida\\algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"\\Oneida\\agriculture.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"\\Oneida\\forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"\\Oneida\\water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"\\Oneida\\wetlands.shp"), verbose=FALSE)
#plot points over true color map
plotRGB(rgbS, stretch="lin",maxpixels=2297430)
plot(algae, add=TRUE, col=rgb(0.5,0.5,0.5,0.5), pch=19)
plot(agri, add=TRUE, col=rgb(0.75,0.5,0.5,0.5), pch=19)
plot(forest, add=TRUE, col=rgb(0.75,0.75,0.25,0.5), pch=19)
plot(water, add=TRUE, col=rgb(0.33,0.75,0.75,0.5), pch=19)
plot(wetlands, add=TRUE, col=rgb(0.33,0.33,0.65,0.5), pch=19)
legend("bottomleft", c("algae","agri","forest","water","wetlands"), pch=19, col=c(rgb(0.5,0.5,0.5,0.5),rgb(0.75,0.5,0.5,0.5),rgb(0.75,0.75,0.25,0.5),rgb(0.33,0.75,0.75,0.5),rgb(0.33,0.33,0.65,0.5)), bty="n", cex=0.75)
#set up data frame for point coordinates
landExtract <-  data.frame(landcID = as.factor(rep(c("algae","water","agri","forest","wetland"),each=120)), x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],forest@coords[,1],wetlands@coords[,1]), y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],forest@coords[,2],wetlands@coords[,2]))
#create a stack of all bands
allbands <- stack (rdatB2, rdatB3, rdatB4, rdatB8)/10000
#add the raster reflectance values from the stack to point coordinates and classes using the extract function
ExtractOut <- raster::extract(allbands,landExtract[,2:3])
#name the bands in new ExtractOut dataset
colnames(ExtractOut) <- c("B02", "B03", "B04", "B08")
#combine original coordinate data with raster reflectance data
rasterEx <- cbind(landExtract, ExtractOut)
#look at data
head(rasterEx)
#make a plot of reflectance for different landcover classes
ggplot(data=rasterEx, aes(x=B02, y=B03, color=landcID))+ geom_point(alpha=0.6)+ theme_classic()
#Question 7
#make plot of infrared reflectance vs each band of visible light for different landcover classes
ggplot(data=rasterEx, aes(x=B08, y=B02, color=landcID))+ geom_point(alpha=0.6)+ theme_classic()
ggplot(data=rasterEx, aes(x=B08, y=B03, color=landcID))+ geom_point(alpha=0.6)+ theme_classic()
ggplot(data=rasterEx, aes(x=B08, y=B04, color=landcID))+ geom_point(alpha=0.6)+ theme_classic()
#Question 8
#extract landcover points from NDVI raster
NDVIExt <- raster::extract(NDVI, landExtract[,2:3])
#add NDVI values to land category data frame
NDVIset <- cbind(landExtract, NDVIExt)
#make violin plots showing NDVI values for agri, forest, and wetland
ggplot(NDVIset[NDVIset$landcID==c("agri", "forest", "wetland"), ], aes(x=landcID, y=NDVIExt))+geom_violin(fill="#98E8D6")+geom_boxplot(width=0.1,size=0.25, fill="grey90")+theme_classic()+labs(y="NDVI", x="land cover type")
