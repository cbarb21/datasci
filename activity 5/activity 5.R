#read in weather station data
datW <- read.csv("\\Users\\clare\\Documents\\Data Science\\Activity 5\\noaa2011124.csv")
#convert name column to factor
datW$NAME <- as.factor(datW$NAME)
#set up vector of names
nameS <- levels(datW$NAME)
nameS
#make dataframe with just precipitation, year and site name
#omit NA data
datP <- na.omit(data.frame(PRCP=datW$PRCP, NAME=datW$NAME, year=datW$year))
#calculate total annual precipitation using aggregate
precip <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN=sum, na.rm=TRUE)
#rename columns
colnames(precip) <- c("NAME", "year", "TotalP")
#add x column with aggregate data
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="length")$x
#make new dataframe omitting years with fewer than 364 days of data
pr <- precip[precip$ncount >=364, ]
#look at only livermore ca data
ca <- pr[pr$NAME == nameS[2], ]
#look at only morrisville ny data
ny <- pr[pr$NAME == nameS[5], ]
#make plot of ca precipitation
plot(ca$year, ca$TotalP, type="b", pch=19, ylab="Annual precipitation (mm)", xlab="Year", yaxt="n", ylim=(c(0, 1600)))
#add y axis with more legible labeling
axis(2, seq(200,800, by=200), las=2)
#add ny data to plot (and extend range using ylim in original plot)
points(ny$year, ny$TotalP, type="b", pch=19, col="tomato3")
#add legend
legend("topleft", c("California", "New York"), col=c("black", "tomato3"), pch=19, lwd=1, bty="n")
#Question 3
#make dataframe with just max temp, year, and site name 
#omit NA data
datT <- na.omit(data.frame(TMAX=datW$TMAX, NAME=datW$NAME, year=datW$year))
#calculate mean annual maximum temp using aggregate function
tmax <- aggregate(datT$TMAX, by=list(datT$NAME, datT$year), FUN=mean, na.rm=TRUE)
#rename columns
colnames(tmax) <- c("NAME", "year", "mean max temp")
#add x column with aggregate data
tmax$ncount <- aggregate(datT$TMAX, by=list(datT$NAME, datT$year), FUN="length")$x
#make new dataframe omitting years with fewer than 364 days of data
tma <- tmax[tmax$ncount >=364, ]
#look only at ny data
ny <- tma[tma$NAME == nameS[5], ]
#look only at nd data
nd <- tma[tma$NAME == nameS[3], ]
#make plot of ny max temp
plot(ny$year, ny$`mean max temp`, type="b", pch=19, ylab="Mean annual maximum temperature (C)", xlab="Year", yaxt="n", ylim=(c(8, 16)), col=rgb(0,0,0,0.5))
#add y axis with more legible labeling
axis(2, seq(8,16, by=2), las=2)
#add nd data to ny plot
points(nd$year, nd$`mean max temp`, type="b", pch=19, col=rgb(1,0.38,0.27,0.5))
#add legend
legend("topleft", c("New York", "North Dakota"), col=c(rgb(0,0,0,0.5), rgb(1,0.38,0.27,0.5)), pch=19, lwd=1, bty="n")

#install ggplot package and add it to library
install.packages("ggplot2")
library(ggplot2)
#plot annual precipitation data for all sites
ggplot(data=pr, aes(x=year, y=TotalP, color=NAME))+geom_point(alpha=0.5)+geom_path(alpha=0.5)+labs(x="year", y="Annual Precipitation")+theme_classic()+scale_color_manual(values = c("#1B9E77","#66A61E","#D95F02","#E7298A","#7570B3"))
#make violin plot of daily minimum temperature for each site
ggplot(data=datW, aes(x=NAME, y=TMIN))+geom_violin(fill=rgb(0.933, 0.950, 0.98))+geom_boxplot(width=0.2,size=0.25, fill="grey90")+theme_classic()
#look specifically at data from 1974 in Mormon Flat, AZ
sub <- datW[datW$NAME == nameS[4] & datW$year == 1974,]
#convert date data from character to date format
sub$DATE <- as.Date(sub$DATE, "%Y-%m-%d")
#make plot of daily maximum temperature from 1974 in Mormon Flat 
ggplot(data=sub, aes(x=DATE, y=TMAX))+geom_point()+geom_path()+theme_classic()+labs(x="year", y="Maximum temperature (C)")
#make barplot of daily precipitation from 1974 in Mormon Flat
ggplot(data=sub, aes(x=DATE, y=PRCP))+geom_col(fill="royalblue3")+theme_classic()+labs(x="year", y="Daily precipitation (mm)")
#look specifically at data from 1974 in Aberdeen, WA
aber <- datW[datW$NAME ==nameS[1] & datW$year == 1974,]
#convert date data from character to date format
aber$DATE <- as.Date(aber$DATE, "%Y-%m-%d")
#make plot of daily maximum temperature from 1974 in Aberdeen
ggplot(data=aber, aes(x=DATE, y=TMAX))+geom_point()+geom_path()+theme_classic()+labs(x="year", y="Maximum temperature (C)")
#make a barplot of daily precipitation from 1974 in Aberdeen
ggplot(data=aber, aes(x=DATE, y=PRCP))+geom_col(fill="royalblue3")+theme_classic()+labs(x="year", y="Daily precipitation (mm)")
#Question 9
#make dataframe with just min temp, year and site name
datM <- na.omit(data.frame(TMIN=datW$TMIN, NAME=datW$NAME, year=datW$year))
#rename columns
colnames(datM) <- c("MinTemp", "Name", "Year")
#aggregate data to show number of observations
ncount <- aggregate(datM$MinTemp, by=list(datM$Year, datM$Name), FUN="length")
ncount
#make dataframe of just data from Aberdeen, WA between 2000-2019
datA <- datM[datM$Name == nameS[1] & datM$Year >= 2000, ]
#make violin boxplot for this data
ggplot(data=datA, aes(x=Year, y=MinTemp, group=Year))+geom_violin(fill="#98E8D6")+geom_boxplot(width=0.2,size=0.25, fill="grey90")+theme_classic()+labs(y="Daily Minimum Temperature (C)")+ggtitle("Minimum Temperature in Aberdeen, Washington")