library(ggplot2)
library(lubridate)
library(dplyr)
#import air quality data for San Jose in 2020
San19 <- read.csv("\\Users\\clare\\Documents\\Data Science\\Final Project\\ad_viz_plotval_data_San_Jose.csv")
San19$Date <- as.Date(San19$Date, "%m/%d/%Y")
#import air quality data for Eureka in 2019
Eur19 <- read.csv("\\Users\\clare\\Documents\\Data Science\\Final Project\\ad_viz_plotval_data_Eureka.csv")
Eur19$Date <- as.Date(Eur19$Date, "%m/%d/%Y")

#preliminary summary statistics
#mean particulate matter concentration for San Jose
mean(San19$Daily.Mean.PM2.5.Concentration)
sd(San19$Daily.Mean.PM2.5.Concentration)
#mean particulate matter concentration for Eureka
mean(Eur19$Daily.Mean.PM2.5.Concentration)
sd(Eur19$Daily.Mean.PM2.5.Concentration)

#Graph 1
#summary graph of daily air quality data from San Jose
#make a dataframe of the dates for each local fire
Sanfire <- data.frame("Date"=as.Date(c("2019-07-15", "2019-07-24","2019-09-21","2019-06-10","2019-08-15","2019-07-02","2019-06-09"), "%Y-%m-%d"), "Daily.Mean.PM2.5.Concentration"=c(5,5,5,5,5,5,5))
#make plot of daily PM2.5 concentration for San Jose
#add line at EPA safety threshold
#add points for dates of four local fires
ggplot(data=San19, aes(x=Date, y=Daily.Mean.PM2.5.Concentration))+
  geom_line(color="blue")+
  ggtitle("Daily Average PM 2.5 Concentration for San Jose in 2019")+
  labs(y="Daily Mean PM2.5 Concentration")+
  theme_classic()+geom_hline(yintercept=12, linetype="dashed", color="tomato3")+
  geom_point(data=Sanfire, aes(x=Date, y=Daily.Mean.PM2.5.Concentration))


#Graph 2
#comparison of peak and low month for San Jose air quality data
#add day column to San Jose data table
San19$day <- day(San19$Date)
#subset tables of just month 11 and month 2
San_feb <- San19[San19$month==2,]
San_nov <- San19[San19$month==11,]
#graph february data and add a second line for november
ggplot(data=San_feb, aes(x=day, y=Daily.Mean.PM2.5.Concentration))+
  geom_line(color="blue")+
  ggtitle("Average Daily PM 2.5 Concentration for San Jose in 2019")+
  labs(y="Daily Mean PM 2.5 Concentration")+
  theme_classic()+
  geom_line(data=San_nov, aes(x=day, y= Daily.Mean.PM2.5.Concentration), color="tomato3")
#t-test of february and november data
#make one data frame of both months
San.both <- data.frame(month=c(San_feb$month,San_nov$month),PM2.5=c(San_feb$Daily.Mean.PM2.5.Concentration,San_nov$Daily.Mean.PM2.5.Concentration))
#check assumptions for february data
#run a regression
feb.mod <- lm(San.both$PM2.5[San.both$month=="2"] ~ San.both$month[San.both$month=="2"])
#get standardized residuals
feb.res <- rstandard(feb.mod)
#set up qqplot
qqnorm(feb.res)
#add qqline
qqline(feb.res)
#perform shapiro-wilkes test for normality
shapiro.test(feb.res)
#complete same tests for november data
#run a regression
nov.mod <- lm(San.both$PM2.5[San.both$month=="11"] ~ San.both$month[San.both$month=="11"])
#get standardized residuals
nov.res <- rstandard(nov.mod)
#set up qqplot
qqnorm(nov.res)
#add qqline
qqline(nov.res)
#perform shapiro-wilkes test for normality
shapiro.test(nov.res)
#perform t test
month.dif <- t.test(San.both$PM2.5 ~ as.factor(San.both$month))
month.dif

#Graph 3
#comparison and statistical analysis of differences of fire season air quality for San Jose and Eureka
#make a data frame of monthly average particulate matter concentrations for San Jose
San19$month <- month(San19$Date)
San.month <- aggregate(San19$Daily.Mean.PM2.5.Concentration, by=list(San19$month), FUN="mean")
colnames(San.month) <- c("month", "PM2.5Concentration")
#make a data frame of monthly average particulate matter concentrations for Eureka
Eur19$month <- month(Eur19$Date)
Eur.month <- aggregate(Eur19$Daily.Mean.PM2.5.Concentration, by=list(Eur19$month), FUN="mean")
colnames(Eur.month) <- c("month", "PM2.5Concentration")
#plot monthly data for San Jose and Eureka
ggplot(data=San.month, aes(x=month, y=PM2.5Concentration))+
  geom_line(color="blue")+theme_classic()+
  ggtitle("Daily Mean PM 2.5 Concentrations in 2019 for San Jose and Eureka")+
  geom_line(data=Eur.month, aes(x=month, y=PM2.5Concentration), color="tomato3")
