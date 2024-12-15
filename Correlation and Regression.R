#Correlation

iris.data <- read.csv("iris_Data.csv")
head(iris.data,5)
cor(iris.data$Sepal.Length, iris.data$Sepal.Width)
iris.data$Species
crr1 <- cor(iris.data$Sepal.Length[1:50], iris.data$Sepal.Width[1:50])
cor(iris.data$Petal.Length, iris.data$Petal.Width)
colnames(iris.data)
library(tidyverse)
iris.data <- iris.data %>% select(-Species)
head(iris.data)
cor(iris.data)
pairs(iris.data, pch= 16, col= "cyan")
library(corrplot)
corrplot(cor(iris.data), method = "number", type = "upper")
iris.data
corrplot(as.matrix(iris.data), is.corr = F, method = "square")
t.test <- cor.test(iris$Sepal.Length[1:50], iris.data$Sepal.Width[1:50])
library(GGally)
ggpairs(iris.data)
ggpairs(iris.data, columns = 1: ncol(iris.data),
        title = "Plot", upper = list(continuous="cor"),
        lower = list(continuous= "smooth", mapping = NULL))

#Regression (Simple linear regression)
reg.pl.pw <- lm(iris.data$Petal.Width ~ iris.data$Petal.Length +
                  iris.data$Sepal.Width + iris.data$Sepal.Length)
summary(reg.pl.pw)
