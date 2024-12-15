library(ggplot2)
library(ggExtra)
dat <- iris
head(dat)
#dotplot
ggplot(dat) + 
  aes(x = Species, y = Sepal.Length) +
  geom_dotplot(binaxis = "y", stackdir = "center")

#scatterplot
ggplot(dat, aes(Sepal.Length, Petal.Length))+
  geom_point() #add geometry

#Add aesthetics to geometry
ggplot(data=dat, mapping=aes(x=Sepal.Length, y=Petal.Length)) +
  geom_point(aes(color=Species))

ggplot(data=dat, mapping=aes(x=Sepal.Length, y=Petal.Length)) +
  geom_point(aes(color="deeppink"))

#Shape aesthetics
ggplot(data=dat, mapping=aes(x=Sepal.Length, y=Petal.Length)) +
  geom_point(aes(shape = Species))

ggplot(data=dat, mapping=aes(x=Sepal.Length, y=Petal.Length)) +
  geom_point(shape = 24)

ggplot(data=dat, mapping=aes(x=Sepal.Length, y=Petal.Length)) +
  geom_point(aes(shape = Species, color=Species))

#Size aesthetics
ggplot(data=dat, mapping=aes(x=Sepal.Length, y=Petal.Length)) +
  geom_point(aes(size = Species, color=Species, shape = Species))

#line plot
plot(dat$Sepal.Length, type = "l")

#density plot
plot(density(dat$Sepal.Length))

g <- ggplot(dat) + aes(x = Sepal.Length, y = Petal.Length) +
  geom_point(aes(shape = Species, color = Species)) +
  geom_smooth(method= "lm", se=F) +
  labs(
    title = "Plot",
    x = "Sepal Length",
    y = "Petal Length") +
  theme(legend.position = "none")
ggMarginal(g, type = "histogram", fill= "transparent")
ggMarginal(g, type = "boxplot", fill= "transparent")

#Histogram
#g <- ggplot(dat, aes(Sepal.Length)) + scale_fill_brewer()

#Boxplot
ggplot(data=iris,
       aes(x=Species,
           y=Sepal.Length,
           fill=Species))+
  geom_boxplot()+ #geom_boxplot(outlier.shape=NA, show.legend)
  labs(
    title = "Box and Whisker plot",
    x = "Species",
    y = "Sepal.Length") +
  theme(legend.position = "none")

#Box + violin
ggplot(data=iris,
       aes(x=Species,
           y=Sepal.Length,
           fill=Species))+
  geom_violin()
