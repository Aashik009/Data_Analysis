#Purposes of PCA
IrisData <- as.data.frame(read.csv("iris_Data.csv"))
#log transform
log.ir <- log(IrisData[,1:4])
ir.species <- IrisData[,5]
rownames(log.ir) = make.names(IrisData[,5], unique = T)
#Principal component
Iris.PCA <- prcomp(log.ir, center = T, scale. = T)
summary(Iris.PCA)

library(factoextra)
fviz_eig(Iris.PCA, addlabels = T)
fviz_pca_var(Iris.PCA, col.var = "cyan")
fviz_cos2(Iris.PCA, choice = "var", axes = 1:2)
fviz_pca_var(Iris.PCA, col.var = "cos2", gradient.cols = c("black", "deeppink",
                                                       "orange"), repel = T)
library(corrplot)
var <- get_pca_var(Iris.PCA)
corrplot(var$cos2, is.corr = F)
fviz_pca_ind(Iris.PCA, col.ind = "cos2", gradient.cols = c("darkorchid", "gold",
                                                            "black"), repel= T)
library(psych)
pairs.panels(log.ir, gap = 0, bg= c("red", "yellow", "orange")[ir.species],
             pch=21)
pairs.panels(Iris.PCA$x, gap = 0, bg= c("red", "yellow", "orange")[ir.species],
             pch=21)
library(ggbiplot)
g <- ggbiplot(Iris.PCA, obs.scale = 1, var.scale = 1, groups = ir.species,
              ellipse = T, circle = T, ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- theme(legend.direction = 'horizontal',
           legend.position = 'top')
print(g)
#Mid upto RCBD
#Generate Data
#Block, treatment(make it significant by increasing mean while generating), yield
#Anova
#Interpretation
#Mean comparison, bar graph
#Specific, interpretation again
#Factorial, strip plot for final