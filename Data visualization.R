iris.data <- read.csv("iris_Data.csv")
attach(iris.data)
summary(iris.data)
dim(iris.data)
iris.data$Species <- as.factor(iris.data$Species)
class.species <- table(iris.data$Species)
Petal.length.mean <- t(data.frame(by(iris.data$Petal.Length, iris.data$Species, mean)))
Petal.length.sd <- t(data.frame(by(iris.data$Petal.Length, iris.data$Species, sd)))
colnames(Sepal.length.mean)
barplot(Petal.length.mean, main="Bar plot of Petal length", xlab= "Species", 
        ylab="Petal length", col= "skyblue")

Sepal.length.mean <- t(data.frame(by(iris.data$Sepal.Length, iris.data$Species, mean)))
Sepal.length.sd <- t(data.frame(by(iris.data$Sepal.Length, iris.data$Species, sd)))
barplot(Sepal.length.mean, main= "Barplot", col="cyan")
install.packages("gplots")
library(gplots)
barplot2(Sepal.length.mean, names.arg = colnames(Sepal.length.mean), 
         xlab = "Variety", ylab = "Sepal length", main = "Barplot",
         plot.ci = TRUE, ci.l = Sepal.length.mean - Sepal.length.sd, 
         ci.u = Sepal.length.mean + Sepal.length.sd)
class.species <- table(iris.data$Species)
SpeciesId <- as.numeric(iris.data$Species)
plot(iris.data$Petal.Length, iris.data$Petal.Width, pch=SpeciesId, col= SpeciesId)
legend(2,4, legend=c("setosa", "versicolor", "virginica"),
       fill= c("black", "green", "red"))
#Box plot for all the variables i.e. sepal length, petal length (next class assignment)
