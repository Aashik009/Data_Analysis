library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
#Simple linear regression
iris.data <- read.csv("iris_Data.csv")
regression.simple <- lm(iris.data$Petal.Width ~ iris.data$Petal.Length)
summary(regression.simple)
regression.simple.graph <- ggplot(iris.data, aes(iris.data$Petal.Width,
      iris.data$Petal.Length))+ 
  geom_point(shape=21, fill= "cyan", size=2)+
  geom_smooth(method = "lm", col= "black", size= 1.5)+
  labs(title = "Simple regression line", x= "Petal.width", y= "Petal.length")

#Multiple linear regression
regression.multiple <- lm(iris.data$Petal.Width ~ iris.data$Petal.Length + 
                          iris.data$Sepal.Length + iris.data$Sepal.Width)
summary(regression.multiple)

#Non linear regression
library(minpack.lm)
x <- c(0, 1, 2, 3, 4, 5)
y <- c(1,2,4,8,16,32)
plot(x,y)
start.value <- c(a=4, b=2)
non.linear.reg <- nls(y ~ a*exp(b*x), start = start.value, algorithm = "port",
                      control = nls.control(maxiter = 1000))
summary(non.linear.reg)
x <- seq(1:10)
y <- x^2 + x + rnorm(10,0,10)
plot(x,y, ylim = c(1,200))
data1 <- data.frame(x,y)
fit1 <- lm(y ~ poly(x,2), data = data1)
summary(fit1)
fit2 <- lm(y ~ x + x^2)
summary(fit2)
x <- runif(20, 55, 75)
y <- runif(20, 75, 200)
plot(x,y)
hist(y)
mann.whitny.test <- wilcox.test(y,x,conf.int = TRUE, alternative = "less")
mann.whitny.test <- wilcox.test(y,x,conf.int = TRUE)

will.sign.test <- wilcox.test(y,x,conf.int = TRUE, paired = TRUE)
head(iris.data)
kruskal.walis.test <- kruskal.test(iris.data$Petal.Length, 
                                   as.factor(iris.data$Species))
matrix_55 <- matrix(rnorm(25, 0, 1), nrow = 5, ncol = 5)
rownames(matrix_55) <- c("R1", "R2", "R3", "R4", "R5")
colnames(matrix_55) <- c("X1", "X2", "X3", "X4", "X5")
dimnames(matrix_55) <- list(c("P1","P2","P3","P4","P5"), 
                            c("Y1", "Y2", "Y3", "Y4", "Y5"))
matrix_51 <- matrix(rnorm(5), nrow = 5, ncol = 1)
dimnames(matrix_51) <- list(c("P1","P2","P3","P4","P5"), 
                            c("Y1"))
result <- matrix_55 %*% matrix_51
