x <- c(28, 45, 32, 29, 41, 23, 38, 49, 25, 36, 30, 27, 43, 21, 39, 34, 26, 40, 37, 24,
       42, 31, 22, 44, 33, 20, 46, 35, 48, 37, 39, 29)
mean(x)
length(x)
SE.x <- sd(x)/sqrt(length(x))
H0 <- 28
z <- (mean(x) - H0)/SE.x
z.result <- t.test(x)
confidence <- z.result$conf.int
