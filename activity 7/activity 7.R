#import air quality data for Bakersfield in 2020
Bak19 <- read.csv("\\Users\\clare\\Documents\\Data Science\\Activity 7\\ad_viz_plotval_data.csv")
#find mean and standard deviation in particulate pollution
mean(Bak19$Daily.Mean.PM2.5.Concentration)
sd(Bak19$Daily.Mean.PM2.5.Concentration)

#make plot of monthly mean daily particulate pollution
#make a month column
Bak19$Date <- as.Date(Bak19$Date, "%m/%d/%Y")
install.packages("lubridate")
library(lubridate)
Bak19$month <- month(Bak19$Date)
#make a data frame of monthly average particulate matter concentrations
aveM <- aggregate(Bak19$Daily.Mean.PM2.5.Concentration, by=list(Bak19$month), FUN="mean")
colnames(aveM) <- c("month", "PM2.5Concentration")
#plot dataframe
plot(aveM$PM2.5Concentration~as.factor(aveM$month), main="2019, Bakersfield, CA", xlab="Month", ylab="Daily PM2.5 Concentration (ug/m3 LC)")
