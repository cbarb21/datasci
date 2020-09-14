#import lemming herbivory data
ch4 <- read.csv("C:\\Users\\clare\\Documents\\Data Science\\Activity 3\\lemming_herbivory.csv")
ch4$herbivory <- as.factor(ch4$herbivory)
#make boxplot to visualize data
plot(ch4$CH4_Flux ~ ch4$herbivory, xlab="Treatment", ylab="CH4 Fluxes (mgC (m -2 day -1)")
#make sure data is normally distributed for each treatment
shapiro.test(ch4$CH4_Flux[ch4$herbivory=="Ctl"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory=="Ex"])
#test for equal variance using Bartlett test
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)
#perform t-test, with CH4_Flux as the dependent variable
t.test(ch4$CH4_Flux ~ ch4$herbivory)
#import insect data
datI <- read.csv("C:\\Users\\clare\\Documents\\Data Science\\Activity 3\\insect_richness.csv")
datI$urbanName <- as.factor(datI$urbanName)
#make sure data is normally distributed for each urban type
shapiro.test(datI$Richness[datI$urbanType=="1"])
shapiro.test(datI$Richness[datI$urbanType=="3"])
shapiro.test(datI$Richness[datI$urbanType=="8"])
shapiro.test(datI$Richness[datI$urbanType=="9"])
#test for equal variance using Bartlett test
bartlett.test(datI$Richness ~ datI$urbanType)
#specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)
#run ANOVA
in.aov <- aov(in.mod)
#print out ANOVA table
summary(in.aov)
#run Tukey HSD
tukeyT <- TukeyHSD(in.aov)
tukeyT
#make a plot to visualize tukeyT
plot(tukeyT, cex.axis=0.75)
#calculate means across factor data
tapply(datI$Richness, datI$urbanName, "mean")
#set up contingency table for Farnsworth's data
species <- matrix(c(18,8,15,32), ncol=2, byrow=TRUE)
colnames(species) <- c("Not Protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increasing")
#make mosaic plot to visualize contingency table
mosaicplot(species, xlab="Population Status", ylab="Legal Protection", main="Legal protection impacts on populations")
#conduct a chi-squared test
chisq.test(species)