#read in beaver dam data
datB <- read.csv("\\Users\\clare\\Documents\\Data Science\\Activity 4\\beaver_dam.csv")
head(datB)
#create scatter plot of data
plot(datB$dams.n, datB$area.ha, pch=19, col="royalblue4", ylab="Surface water area (ha)", xlab="Number of beaver dams")
#set up regression
dam.mod <- lm(datB$area.ha~datB$dams.n)
#get standardized residuals
dam.res <- rstandard(dam.mod)
#check normality of residuals using qqtest
qqnorm(dam.res)
#add qqline
qqline(dam.res)
#check normality of residuals using Shapiro-Wilks test
shapiro.test(dam.res)
#make residual plot
plot(datB$dams.n, dam.res, xlab="Beaver dams", ylab="Standardized residual")
#add horizontal line at zero
abline(h=0)
#print out regression table
summary(dam.mod)
#rerun original plot of dams and water area (line 5)
#add regression line to plot of dams and water area
abline(dam.mod, lwd="2")

#read in leaf bud data
pheno <- read.csv("\\Users\\clare\\Documents\\Data Science\\Activity 4\\red_maple_pheno.csv")
#set up panel of plots comparing day of leaf out with max temp and precipitation
par(mfrow=c(1,2))
plot(pheno$Tmax, pheno$doy, pch=19, col="royalblue4", ylab="Day of leaf out", xlab="Maximum temperature (C)")
plot(pheno$Prcp, pheno$doy, pch=19, col="royalblue4", ylab="Day of leaf out", xlab="Precipitation (mm)")
#make plot comparing day of leaf out with latitude
plot(pheno$Lat, pheno$doy, pch=19, col="royalblue4", ylab="Day of leaf out", xlab="Latitude")
#make plot comparing day of leaf out with elevation
plot(pheno$elev, pheno$doy, pch=19, col="royalblue4", ylab="Day of leaf out", xlab="Elevation (m)")
#make plot comparing day of leaf out with minimum temperature
plot(pheno$Tmin, pheno$doy, pch=19, col="royalblue4", ylab="Day of leaf out", xlab="Minimum temperature (C)")
#make plot comparing day of leaf out with urban/rural category
pheno$siteDesc <- as.factor(pheno$siteDesc)
plot(pheno$doy ~ pheno$siteDesc, ylab="Day of leaf out", xlab="Site type")
#generate series of covariance plots to test for multi-collinearity
plot( ~ pheno$Lat+pheno$Tmax+pheno$Tmin+pheno$Prcp+pheno$elev+pheno$siteDesc)
#code urban and rural as 0 and 1 for regression analysis
pheno$urID <- ifelse(pheno$siteDesc=="Urban", 1, 0)
#set up multiple regression
mlr <- lm(pheno$doy ~ pheno$Tmax+pheno$Prcp+pheno$elev+pheno$urID)
mlr.standard <- rstandard(mlr)
mlFitted <- fitted(mlr)
#test assumption of normal residuals using qqnorm test
qqnorm(mlr.standard)
qqline(mlr.standard)
#test assumption of equal variance by creating residual plot
plot(mlFitted, mlr.standard, xlab="Fitted", ylab="Standardized residual")
abline(h=0)
#print out regression table
summary(mlr)