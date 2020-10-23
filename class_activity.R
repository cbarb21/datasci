#look at data
head(iris)
iris
#load packages
library(dplyr)

#create data fra e of max height data
height <- data.frame(Species=c("virginica","setosa","versicolor"), Height.cm=c(60,100,11.8))
#join max height data in new data frame
iris.height <- left_join(iris, height, by="Species")
#create data frame of sepal length
sepal.length <- data.frame(Sepal.Length=iris$Sepal.Length, Species=iris$Species)
#join sepal length and max height
sep.height <- left_join(sepal.length, height, by="Species")

#use apply function to take mean of sepal and petal length and width for setosa
apply(iris[iris$Species=="setosa", 1:4], 2, "mean")

#use for loop to take mean of sepal and petal length and width for setosa
aves <- numeric()
for(i in 1:4){aves[i] <- mean(iris[iris$Species=="setosa", i])}
