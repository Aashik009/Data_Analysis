timeSeries.Data <- read.csv("Tseries training.csv", row.names = 1)
head(timeSeries.Data, 5)
str(timeSeries.Data)
summary(timeSeries.Data) # colnames =
library(ggplot2)
library(reshape2)
Tseriesplot.raw <- data.frame(x=as.numeric(rownames(timeSeries.Data)),
                    timeSeries.Data)
head(Tseriesplot.raw, 5)
Tseriesplot.raw <- melt(Tseriesplot.raw, id.vars = "x")
head(Tseriesplot.raw, 5)
cols <- c("deeppink", "purple", "red", "blue", "green","black", "yellow" )
Tseries.plot <- ggplot(Tseriesplot.raw, aes(x=x, y=value, color= variable))+
  geom_line(linetype=1, lwd=1.3)+ 
  scale_color_manual(values = cols)

library(GGally)
ggpairs(timeSeries.Data, columns= 1: ncol(timeSeries.Data),
       upper= list(continuous="cor"),
       lower=list(continuous="smooth"),
       mapping=NULL)

library(tseries)
timeSeries.Data <- log(data.frame(timeSeries.Data))
attach(timeSeries.Data)
Adf.test <- adf.test(timeSeries.Data$BRP)
PP.test <- pp.test(timeSeries.Data$CO2)

library(forecast)
ARIMA <- auto.arima(timeSeries.Data$CO2)

hist(ARIMA$residuals)
forcast.BRP <- forecast(ARIMA)
autoplot(forcast.BRP)

library(ARDL)
colnames(timeSeries.Data)
ARDL.YBR <- auto_ardl(YBR ~ ABR + ATBS + AHBS + CO2,
                      data= timeSeries.Data, max_order = 5)
ARDL.YBR$top_orders
bst.model <-ARDL.YBR$best_model
summary(ARDL.YBR$best_model)
ECM.Model <- recm(bst.model, case = 3)
summary(ECM.Model)
ECM.Model$coefficients
hist(bst.model$residuals)

library(lmtest)
library(tseries)
library(strucchange)
bgtest(bst.model, order = 5)
