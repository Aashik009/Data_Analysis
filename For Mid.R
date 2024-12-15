Block<-c(rep("Block1",20),rep("Block2",20),rep("Block3",20))
Variety<-rep(c(rep("variety1",5),rep("variety2",5),rep("variety3",5),rep("variety4",5)),3)
Nitrogen<-rep(c("Nitrogen1","Nitrogen2","Nitrogen3","Nitrogen4","Nitrogen5"),12)
Blocks<- as.factor(Block)
Varieties<-as.factor(Variety)
Nitrogens<-as.factor(Nitrogen)
Data<-data.frame(Blocks,Varieties,Nitrogens)
Yield<-c(rnorm(5,20,1),rnorm(5,25,0.6),rnorm(5,22,6.2),rnorm(5,29,1.2),rnorm(5,20,2),
         rnorm(5,25,1),rnorm(5,22,1.2),rnorm(5,29,1.2),rnorm(5,20,1),rnorm(5,25,1.8),
         rnorm(5,22,1.7),rnorm(5,29,1.2))
Data1<-data.frame(Blocks,Varieties,Nitrogens,Yield)
##Anova for split plot design model
ANOVA.RCBD<-aov(Yield~ Blocks + Varieties + Blocks:Varieties + Nitrogens +
                  Varieties*Nitrogens,data = Data1)
summary(ANOVA.RCBD)

library(agricolae)
Post.Hoc.Test <- with(Data1, HSD.test(Yield, Varieties:Nitrogens, 
                                      DFerror = 32, MSerror = 4.12))
#This will be included in final with the question on photo
#Mid upto RBD 
