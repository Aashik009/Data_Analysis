Data.generation <- runif(n=10, min = 0, max = 1)
Data.generation.uniform <- runif(n=1000, min = 30, max = 50)
dev.off()
par(mar=c(2,5,2,2))
hist(Data.generation.uniform, xlab = "Random value", ylab = "Frequency", main = "Histogram",
     col = "cyan", cex.lab= 1.5, cex.axis= 1.5, border = "black")

Data.generation.Snormal <- rnorm(n=50, mean = 0, sd=1)
Data.generation.Snormal <- rnorm(n=1000, mean = 0, sd=1.5)
hist(Data.generation.Snormal, xlab = "Random value", ylab = "Frequency", main = "Histogram",
     col = "cyan", cex.lab= 1.5, cex.axis= 1.5, border = "black")
Data.generation.Snormal <- rnorm(n=1000, mean = 0, sd=4)
Data.generation.poisson  <- rpois(n=1000, lambda = 1.5)
hist(Data.generation.poisson, xlab = "Random value", ylab = "Frequency", main = "Histogram",
     col = "cyan", cex.lab= 1.5, cex.axis= 1.5, border = "black")

Data.generation.binomial <- rbinom(n=20, size = 1, prob = 0.5)
hist(Data.generation.binomial, xlab = "Random value", ylab = "Frequency", main = "Histogram",
     col = "cyan", cex.lab= 1.5, cex.axis= 1.5, border = "black")

Random.data <- sample(x=1:10, size = 20)
c(rep(x=0.05, times= 5), rep(x=0.15, times=5))
rep(10,10)
Species <- c("Species_A","Species_B", "Species_C")
Species.sample <- sample(x=Species, size = 30, replace = TRUE, prob = c(0.5,0.25,0.25))

x <- matrix(seq(0,16, by=2), 3, 3)
colnames(x) <- c("x1", "x2", "x3")
dim(x)
summary(x)
str(x)
colMeans(x)
x1 <- t(x)
x2 <- diag(x)
x3 <- solve(x)
y <- matrix(seq(20,28,1), 3, 3)
z <- x%*%y
z <- x+y
z <- y-x
z1 <- attach(data.frame(z))
z1$X3
