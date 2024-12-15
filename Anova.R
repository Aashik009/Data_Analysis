RBD.Data <- read.csv("RBD.csv")
dim(RBD.Data)
RBD.Data <- RBD.Data[,1:3]
Treat <- c("Treatment1","Treatment2","Treatment3", "Treatment4", "Treatment5",
           "Treatment6", "Treatment7")
blk <- c("Block1", "Block2", "Block3", "Block4")
t <- length(Treat)
b <- length(blk)
trt <- gl(t,1,t*b, factor(Treat))
trt <- as.factor(rep(Treat, b))
block <- c(rep("Block1",t),rep("Block2",t),rep("Block3",t),rep("Block4",t))
block <- as.factor(block)
ANOVA.RBD <- aov(RBD.Data$YIELD ~ block + trt, data = RBD.Data)
summary(ANOVA.RBD)
library(agricolae)
Post.Hoc <- with(RBD.Data, HSD.test(YIELD, trt, DFerror = 18, MSerror = 19.6))

boxplot(RBD.Data$YIELD ~ trt,xlab= "Treatment",
        ylab= "Yield", col= "green", las=1)

Post.Hoc.Mean <- Post.Hoc$means
Post.Hoc.Mean.Ascending <- Post.Hoc.Mean[order(-Post.Hoc.Mean$YIELD)]

SD <- Post.Hoc$means[,2]
Post.Hoc.lettering <- Post.Hoc$groups
Treat.Mean <- Post.Hoc.lettering[,1]

library(lattice)
library(car)
library(gplots)
Barplot.Treat <- barplot2(Treat.Mean,
                          names.arg=rownames(Post.Hoc.lettering),
                          xlab=" treatment", ylab="Yield", 
                          main = "barplot of Treatments", las=1,
                          plot.ci = TRUE,
                          ci.l = Treat.Mean-SD,
                          ci.u = Treat.Mean+SD)
#Soil er quality (Experimental plot effect) same thakle CRD, alada hoile RCBD

