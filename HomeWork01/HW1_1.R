rm(list = ls())
setwd("/media/parik/New Volume/SDM/R Lab/HomeWork01")

##install.packages("ISLR")
library("ISLR")
library("ggplot2")
dim(Auto)
str(Auto)
summary(Auto)

##########As the 'name' column carries no significance in order to predict the mpg, it should not be considered for the analysis

df <-subset.data.frame(Auto, select = mpg:origin)
str(df)
dfTemp <- df

meanAll= sapply(d[, c(1:8)], mean)
sdAll = sapply(d[,c(1:8)], sd)
medAll = sapply(d[,c(1:8)], median)


##########################################################################
## HISTOGRAMS 
##########################################################################
png("histCylinders.png")
hist(df$cylinders, breaks=10, col="red", xlab="Miles Per Gallon", main="Histogram with Normal Curve", prob=TRUE) 
lines(density(df$cylinders), col="blue", lwd=2) # add a density estimate with defaults
lines(density(df$cylinders, adjust=2), lty="dotted", col="darkgreen", lwd=2) 
dev.off()

##Using GGPLOT2
histCyl <- ggplot(df, aes(x = cylinders) ) +geom_histogram( aes(y = ..density..), binwidth = 1, fill ='#00C4B0', alpha=0.7) + geom_density(col="red") 
histDisplacement <- ggplot(df, aes(x = displacement), colour = blue ) + geom_histogram( aes(y = ..density..), binwidth = 10, fill ='#00C4B0', alpha=0.7) + geom_density(col="red") 
histWeight <- ggplot(df, aes(x = weight), colour = blue ) + geom_histogram( aes(y = ..density..), binwidth = 40, fill ='#00C4B0', alpha=0.7) + geom_density(col="red") 
histAcceleration <- ggplot(df, aes(x = acceleration), colour = blue ) + geom_histogram( aes(y = ..density..), binwidth = 1, fill ='#00C4B0', alpha=0.7) + geom_density(col="red") 
histOrigin <- ggplot(df, aes(x = origin), colour = blue ) + geom_histogram( aes(y = ..density..), binwidth = 1, fill ='#00C4B0', alpha=0.7) + geom_density(col="red") 
histYear <- ggplot(df, aes(x = year), colour = blue ) + geom_histogram( aes(y = ..density..), binwidth = 1, fill ='#00C4B0', alpha=0.7) + geom_density(col="red") 
histHP <- ggplot(df, aes(x = horsepower), colour = blue ) + geom_histogram( aes(y = ..density..), binwidth = 40, fill ='#00C4B0', alpha=0.7) + geom_density(col="red") 

ggsave(filename = "histDisplacement.png", plot = histDisplacement, height = 5, width = 5) 
ggsave(filename = "histWeight.png", plot = histWeight, height = 5, width = 5) 
ggsave(filename = "histAcceleration.png", plot = histAcceleration, height = 5, width = 5) 
ggsave(filename = "histOrigin.png", plot = histOrigin, height = 5, width = 5) 
ggsave(filename = "histYear.png", plot = histYear, height = 5, width = 5) 
ggsave(filename = "histHP.png", plot = histHP, height = 5, width = 5) 

#########################################################################
## BOX PLOTS
#########################################################################

png("boxCylinders.png")
boxCylinders  = boxplot(mpg~cylinders,data=df, main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon")
dev.off()
boxweight  = boxplot(mpg~acceleration,data=df, main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon")


png("boxYear.png")
boxYear = boxplot(mpg~year,data=df, main="Car Milage Data", xlab="Years", ylab="Miles Per Gallon")
dev.off()

png("boxOrigin.png")
boxOrigin = boxplot(mpg~origin,data=df, main="Car Milage Data", xlab="origin", ylab="Miles Per Gallon")
dev.off()

###################REMOVING OUTLIERS

df <- df[!((df$mpg>=25 & df$cylinders ==6) + (df$mpg>=21 & df$cylinders ==8) + (df$mpg>= 38 & (df$origin == 2 | df$origin ==1))),]

boxCylinders1  = boxplot(mpg~cylinders,data=t1, main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon")
boxOrigin2 = boxplot(mpg~origin,data=t1, main="Car Milage Data", xlab="origin", ylab="Miles Per Gallon")


##########################################################################
## BIVARIATE SCATTER PLOTS AFTER REMOVING OUTLIER
##########################################################################

plotCylinder = ggplot(df, aes(x=cylinders, y=mpg)) + geom_point(size=2, shape=23, color= 'blue') 
plotDisplacement    = ggplot(df, aes(x=displacement, y=mpg)) + geom_point(size=2, shape=23, color= 'blue') 
plotHP      = ggplot(df, aes(x=horsepower, y=mpg)) + geom_point(size=2, shape=23, color= 'blue') 
plotWeight  = ggplot(df, aes(x=weight, y=mpg)) + geom_point(size=2, shape=23, color= 'blue') 
plotAcceleration     = ggplot(df, aes(x=acceleration, y=mpg)) + geom_point(size=2, shape=23, color= 'blue') 
plotYear    = ggplot(df, aes(x=year, y=mpg)) + geom_point(size=2, shape=23, color= 'blue') 
plotOrigin  = ggplot(df, aes(x=origin, y=mpg)) + geom_point(size=2, shape=23, color= 'blue') 


ggsave(filename = "plotCylinder.png", plot = plotCylinder, height = 5, width = 5) 
ggsave(filename = "plotDisplacement.png", plot = plotDisplacement, height = 5, width = 5) 
ggsave(filename = "plotHP.png", plot = plotHP, height = 5, width = 5) 
ggsave(filename = "plotWeight.png", plot = plotWeight, height = 5, width = 5) 
ggsave(filename = "plotAcceleration.png", plot = plotAcceleration, height = 5, width = 5) 
ggsave(filename = "plotYear.png", plot = plotYear, height = 5, width = 5) 
ggsave(filename = "plotOrigin.png", plot = plotOrigin, height = 5, width = 5) 

########################
##SAVING INTO .rds aand .RData FILE
############
saveRDS(df,file="cleanData.rds")
save(df, file = "cleanData.RData")
