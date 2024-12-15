# Two Factor factorial design RCBD Data
# Block, FactorA(Nitrogen), FactorB(Variety)

Nitro <- rep(c("Nitro1", "Nitro2", "Nitro3", "Nitro4", "Nitro5"), 12)
Block <- c(rep("Block1",20), rep("Block2",20), rep("Block3",20))
Variety <- c(rep("Variety1",5), rep("Variety2",5), rep("Variety3",5),
             rep("Variety4",5))
Nitrogen <- as.factor(Nitro)
Block <- as.factor(Block)
Variety <- as.factor(Variety)
DF <- data.frame(Block, Variety, Nitrogen)

Yield <- c(runif(20,10,20), runif(20,15,25), runif(20,20,30))
DF_main <- data.frame(Block, Variety, Nitrogen, Yield)
Anova.RCBD <- aov(Yield ~ Block + Variety + Nitrogen, data = DF_main)
summary(Anova.RCBD)

