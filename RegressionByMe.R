rm(list = ls())
setwd("/media/parik/New Volume/SDM/R Lab")

##install.packages("ISLR")
library("ISLR")

dim(Auto)
names(Auto)

d <- as.data.frame(Auto)
d2 <- as.data.frame(Auto) 

d
str(Auto)

large_model <- lm(mpg ~ ., data = d)
summary(large_model)

sd(d$cylinders)

meanAll= sapply(d[, c(1:8)], mean)
sdAll = sapply(d[,c(1:8)], sd)
medAll = sapply(d[,c(1:8)], median)


library(ggplot2)


#############################
## BIVARIATE SCATTER PLOTS
#############################

plotCylinder=ggplot(df, aes(x=displacement, y=mpg, label=rownames(d3))) 
  + geom_point(size=2, shape=23, color= 'blue') + geom_text( check_overlap= TRUE) + geom_label()

ggplot(d, aes(x=cylinders, y=mpg)) + geom_point(size=2, shape=23, color= 'blue')
plotDisp  =ggplot(d, aes(x=displacement, y=mpg)) + geom_point(size=2, shape=23, color= 'blue')
plotHP    =ggplot(d, aes(x=horsepower, y=mpg)) + geom_point(size=2, shape=23, color= 'blue')
plotWeight =ggplot(d, aes(x=weight, y=mpg)) + geom_point(size=2, shape=23, color= 'blue')
plotAcc     =ggplot(d, aes(x=acceleration, y=mpg)) + geom_point(size=2, shape=23, color= 'blue')
plotYear    =ggplot(d, aes(x=year, y=mpg)) + geom_point(size=2, shape=23, color= 'blue')
plotOrigin  =ggplot(d, aes(x=origin, y=mpg)) + geom_point(size=2, shape=23, color= 'blue')

PLOTa=ggplot(d3, aes(x=displacement, y=mpg, label=rownames(d3))) + geom_point(size=2, shape=23, color= 'blue') + geom_text( check_overlap= TRUE) + geom_label()

scatterPlots = c(plotCylinder, plotDisp, plotHP, plotWeight, plotAcc, plotYear, plotOrigin)

plotCylinder
ggsave("plotCylinder.png", width = 5, height = 5)
plotDisp
ggsave("plotDisp.png", width = 5, height = 5)
plotHP
ggsave("plotHP.png", width = 5, height = 5)
plotWeight
ggsave("plotWeight.png", width = 5, height = 5)
plotAcc
ggsave("plotAcc.png", width = 5, height = 5)
plotYear
ggsave("plotYear.png", width = 5, height = 5)
plotOrigin
ggsave("plotOrigin.png", width = 5, height = 5)

#####################################
## REMOVING OUTLIERS
#####################################




