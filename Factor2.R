Factor.Data <- read.csv("2Fact_Split_plot.csv")

dim(Factor.Data)
blk <- c("Block1","Block2","Block3")
variety <- c("Variety1", "Variety2", "Variety3")
nitrogen <- c("Nitrogen1", "Nitrogen2", "Nitrogen3", "Nitrogen4", "Nitrogen5")
b <- length(blk)
v <- length(variety)
n <- length(nitrogen)

block <- gl(b, v*n, b*v*n, factor(blk))
Var.factor <- gl(v, n, b*v*n, factor(variety))
Nitro.factor <- gl(n, 1, b*v*n, factor(nitrogen))

library(lattice)
library(car)
#REPLICAT VARIETY NITROGEN YIELD
xyplot(YIELD ~ Nitro.factor|Var.factor, data = Factor.Data,
       pch= 19, col="red")
xyplot(YIELD ~ Var.factor|Nitro.factor, data = Factor.Data,
       pch= 19, col="blue")
xyplot(YIELD ~ Nitro.factor:Var.factor, data = Factor.Data,
       pch=19, col="cyan" )
ANOVA.CRD.Fact <- aov(YIELD ~ Var.factor + Nitro.factor +
                        Var.factor*Nitro.factor, data = Factor.Data)
summary(ANOVA.CRD.Fact)

Factor.Data <- data.frame(Factor.Data, LA = rnorm(45, 4.5, 0.57))
attach(Factor.Data)
ANOVA.CRD.Fact <- aov(LA ~ Var.factor + Nitro.factor +
                        Var.factor*Nitro.factor, data = Factor.Data)
summary(ANOVA.CRD.Fact)

boxCox(ANOVA.CRD.Fact)
hist(ANOVA.CRD.Fact$residuals)
library(agricolae)
Post.Hoc.Test <- with(Factor.Data, HSD.test(YIELD, Var.factor, 
                                            DFerror = 30, MSerror = 0.083))
Post.Hoc.Test <- with(Factor.Data, HSD.test(LA, Var.factor, 
                                            DFerror = 30, MSerror = 0.083))
Post.Hoc.Test <- with(Factor.Data, HSD.test(LA, Nitro.factor, 
                                            DFerror = 30, MSerror = 0.083))
Post.Hoc.Test <- with(Factor.Data, HSD.test(YIELD, Var.factor:Nitro.factor, 
                                            DFerror = 30, MSerror = 0.083))
Post.Hoc.Test$means
Mean.Matrix <- Post.Hoc.Test$means
Mean.Matrix <- Mean.Matrix[order(Mean.Matrix$YIELD, decreasing = TRUE),]

Mu_Treat <- Mean.Matrix$YIELD
SE_Treat <- Mean.Matrix$std/sqrt(Mean.Matrix$r)

library(gplots)
dev.off()
par(mar=c(10,4,1,1))

Bar.plot <- barplot2(Mu_Treat, names.arg = rownames(Mean.Matrix), 
                     xlab = "Treatment combinations", ylab = 
                       "Mean Yield", plot.ci = TRUE, ci.l = Mu_Treat-SE_Treat,
                     ci.u = Mu_Treat+SE_Treat, col = "cyan", las=2)
text(Bar.plot, 0, Post.Hoc.Test$groups[,2],
     cex= 1.5, pos= 3, col= "blue")

ANOVA.RBD.Fact <- aov(YIELD ~ block + Var.factor + Nitro.factor +
                        Var.factor*Nitro.factor, data = Factor.Data)
summary(ANOVA.RBD.Fact)

ANOVA.SPD <- aov(YIELD ~ block + Var.factor + (block:Var.factor)+
                   Nitro.factor + Var.factor*Nitro.factor, data = Factor.Data)
summary(ANOVA.SPD)
Post.Hoc.Test <- with(Factor.Data, HSD.test(YIELD, Nitro.factor, 
                                            DFerror = 24, MSerror = 0.044))

Post.Hoc.Test$means
Mean.Matrix <- Post.Hoc.Test$means
Mean.Matrix <- Mean.Matrix[order(Mean.Matrix$YIELD, decreasing = TRUE),]

Mu_Treat <- Mean.Matrix$YIELD
SE_Treat <- Mean.Matrix$std/sqrt(Mean.Matrix$r)

Bar.plot <- barplot2(Mu_Treat, names.arg = rownames(Mean.Matrix), 
                     xlab = "Treatment combinations", ylab = 
                       "Mean Yield", plot.ci = TRUE, ci.l = Mu_Treat-SE_Treat,
                     ci.u = Mu_Treat+SE_Treat, col = "cyan", las=2)
