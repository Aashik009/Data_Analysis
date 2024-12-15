data()
mtcars
View(mtcars)
??mtcars
data <- table(mtcars$gear, mtcars$carb)
chi <- chisq.test(data)
factor(mtcars$carb)
chi$statistic
observed_counts <- chi$observed
expected_counts <- chi$expected
print(round(chi$expected), 2)
#create heatmap for percentage contributions
#Calculate contribution to chi-square statistic
contributions <- (observed_counts-expected_counts)^2 / expected_counts
total <- sum(contributions)
#Calculate percentage contributions
total_chi_square <- chi$statistic
percentage_contributions <- 100 * contributions / total
#print percentage contributions
print(round(percentage_contributions, 2))

library(pheatmap)
pheatmap(percentage_contributions, display_numbers = TRUE,
         cluster_rows = FALSE, cluster_cols = FALSE,
         main = "Percentage contributions to Chi-sq statistic")

correlation_matrix <- cor(iris[, 1:4])
pheatmap(correlation_matrix, display_numbers = TRUE)

#Exporting

png(file = "corrplot.png", width = 4000, height = 4000, units = "px",
    res = 600) 
corrplot::corrplot(correlation_matrix, tl.cex = 2)
dev.off()

#Work on air quality data set
#table(is.na(airquality))

??ggqqplot
library(ggpubr)
library(ggplot2)
ggqqplot(iris$Sepal.Length)
shapiro.test(iris$Sepal.Length) 
#If p value is less than alpha, significant, null rejected, data non-normal
x <- rnorm(100,0,1)
shapiro.test(scale(x))
ggqqplot(x, color = "deeppink")

