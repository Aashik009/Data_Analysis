#Central tendency and dispersion

dat <- iris
head(dat)
str(dat)
mode(dat)

#mean
mean(dat$Sepal.Length)
sum(dat$Sepal.Length)/length(dat$Sepal.Length)

#calculate 10% trimmed mean
mean(dat$Sepal.Length, trim = 0.1)

#Geometric mean in vector
exp(mean(log(dat$Sepal.Length)))

#Geometric mean in vector with zero or negatives
exp(mean(log(dat$Sepal.Length[dat$Sepal.Length > 0])))

library("psych") #Needed for harmonic mean
install.packages("psych")
harmonic.mean(dat$Sepal.Length)

#Median
median(dat$Sepal.Length)

#Quantiles
quantile(dat$Sepal.Length, 0.5)
quantile(dat$Sepal.Length, 0.25) #First quartile
quantile(dat$Sepal.Length, 0.4) #4th decile

#Mode
max(table(dat$Sepal.Length))

#Interquartile range
quantile(dat$Sepal.Length, 0.75) - quantile(dat$Sepal.Length, 0.25)
IQR(dat$Sepal.Length)

#Range
range(dat$Sepal.Length)

#Standard deviation and variance
sd(dat$Sepal.Length)
var(dat$Sepal.Length)
sqrt(var(dat$Sepal.Length))
#or
lapply(dat[,-5], sd)
lapply(dat[,-5], mean)
lapply(dat[,c(1,3)], mean)
lapply(dat[1:50,c(1,3)], mean)
dim(dat)

#Summary min, max, 1st quartile, 2nd quartile, 3rd quartile
summary(dat)
by(dat, dat$Species, summary)
#Coefficient of variation
sd(dat$Sepal.Length) / mean(dat$Sepal.Length)  * 100

