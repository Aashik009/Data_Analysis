Factorial.Data <- read.csv("2Fact_Split_plot.csv")
dim(Factorial.Data)
Variety.Treat <- c("Variety1","Variety2", "Variety3")
Nitrogen.Treat <- c("Nitrogen1","Nitrogen2","Nitrogen3","Nitrogen4","Nitrogen5")
blk <- 3
vr <- length(Variety.Treat)
nitro <- length(Nitrogen.Treat)
Block <- gl(blk, vr*nitro, blk*vr*nitro)       #gl is factor function

Variety.Factor <- gl(vr, nitro, blk*vr*nitro, factor(Variety.Treat))
Nitrogen.Factor <- gl(nitro, 1, blk*vr*nitro, factor(Nitrogen.Treat))

ANOVA.CRD.Factorial <- aov(YIELD ~ Variety.Factor + Nitrogen.Factor + 
                             Variety.Factor*Nitrogen.Factor, 
                           data = Factorial.Data)
summary(ANOVA.CRD.Factorial)
