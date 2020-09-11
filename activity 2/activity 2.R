#create vector of heights
heights <- c(30,41,20,22)
#convert from m to cm
heights_cm <- heights*100
heights_cm
#look at first tree height
heights [1]
#look at second and third tree heights
heights [2:3]
#set up matrix with two columns and fill in by rows
#first argument is the vector of numbers to fill matrix
Mat <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat
#set up matrix that fills by columns
#first argument is the vector of numbers to fill matrix
mat.bycol <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
mat.bycol
#look at row 1, collum 2
mat.bycol[1,2]
#look at all numbers in row 2
mat.bycol[2,]
#look at all numbers in column 1
mat.bycol[,1]
#read in weather station file from data folder
datW <- read.csv("C:\\Users\\clare\\Downloads\\Environmental Data Science\\a02\\noaa2011124.csv")
#get more info about data frame
str(datW)
#create example character data vector
char <- c(MORRISVILLE, LIVERMORE, ABERDEEN, MORMON, MANDAN, ANYTOWN)
#create example numeric data vector
numb <- c(1.3,0.0,0.0,5.1,0.0)
numb
#create example integer data vector
inte <- c(1930,1932,1965,1972,1981)
inte_1 <- as.integer(inte)
#create example factor data vector
fact <- c(1,2,3,4,5)
#convert character data into factor data
datW$NAME <- as.factor(datW$NAME)
datW$siten <- as.numeric(datW$NAME)
datW$NAME
#find out all unique site names
levels(datW$NAME)
#look at mean max temp for Aberdeen
#with na.rm argument set to true to ignore NA
mean(datW$TMAX[datW$NAME=="ABERDEEN, WA US"], na.rm=TRUE)
#look at sd
sd(datW$TMAX[datW$siten==1], na.rm=TRUE)
#calculate average daily temp
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
#get the mean average temp for all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)
averageTemp
#change column names
colnames(averageTemp) <- c("NAME", "MAAT")
averageTemp
#convert level to number for factor data type
datW$siteN <- as.numeric(datW$NAME)
#make a histogram for Aberdeen
hist(datW$TAVE[datW$NAME=="ABERDEEN, WA US"], freq=FALSE, main=paste(levels(datW$NAME)[1]), xlab="Average Daily Temperature (degrees C)", ylab="Relative Frequency", col="grey75", border="white")
help(hist)
hist(datW$TAVE[datW$NAME=="LIVERMORE, CA US"], freq=FALSE, main=paste(levels(datW$NAME)[2]), xlab="Average Daily Temperature (degrees C)", ylab="Relative Frequency", col="grey75", border="white")
help(dnorm)
#pnorm
pnorm(0, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE), sd(datW$TAVE[datW$siteN==1], na.rm=TRUE))
#probability of temp below 10
pnorm(5, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE), sd(datW$TAVE[datW$siteN==1], na.rm=TRUE))
#probability of temp between 0 and 10
pnorm(10, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE), sd(datW$TAVE[datW$siteN==1], na.rm=TRUE))-pnorm(0, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE), sd(datW$TAVE[datW$siteN==1], na.rm=TRUE))
#probability of temp above 20
1-pnorm(20, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE), sd(datW$TAVE[datW$siteN==1], na.rm=TRUE))
#calculating value of 95th quantile
qnorm(0.95, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE), sd(datW$TAVE[datW$siteN==1], na.rm=TRUE))
#calculate frequency of values at or above that result if mean increased 4
mean(datW$TAVE, na.rm=TRUE)
pnorm(18.51026, mean(15.72872), sd(datW$TAVE[datW$siteN==1], na.rm=TRUE))
#create histogram for Aberdeen's precipitation
hist(datW$PRCP[datW$NAME=="ABERDEEN, WA US"], freq=FALSE, main=paste(levels(datW$NAME)[1]), xlab="Daily Precipitation", ylab="Relative Frequency", col="grey75", border="white")
#calculate annual precipitation
annP <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum", na.rm=TRUE)
annP
colnames(annP) <- c("NAME", "YEAR", "AP")
#make histogram for Aberdeen's annual precipitation
hist(annP$AP[annP$NAME=="ABERDEEN, WA US"], freq=FALSE, main=paste("ABERDEEN, WA US"), xlab="Annual Precipitation", ylab="Relative Frequency", col="grey75", border="white")
#make histogram for Mandan Station's annual precipitation
hist(annP$AP[annP$NAME=="MANDAN EXPERIMENT STATION, ND US"], freq=FALSE, main=paste("MANDAN EXPERIMENT STATION, ND US"), xlab="Annual Precipitation", ylab="Relative Frequency", col="grey75", border="white")
#calculate likelihood of annual precip below 700mm in Aberdeen
pnorm(700, mean(annP$AP[annP$NAME=="ABERDEEN, WA US"]), sd(annP$AP[annP$NAME=="ABERDEEN, WA US"]))
#calculate likelihood of annual precip below 700mm in Mandan Station
pnorm(700, mean(annP$AP[annP$NAME=="MANDAN EXPERIMENT STATION, ND US"]), sd(annP$AP[annP$NAME=="MANDAN EXPERIMENT STATION, ND US"], na.rm=TRUE))
