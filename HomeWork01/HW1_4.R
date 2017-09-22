rm(list=ls())
#install.packages("MASS")
library("MASS")
library("ggplot2")
setwd("/media/parik/New Volume/SDM/R Lab/HomeWork01")


data <- as.data.frame(Boston)
str(data)
pairs(data)



panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, font, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}
pairs(data, lower.panel = panel.smooth, upper.panel = panel.cor, bg = c("red", "green3", "blue", "yellow"), cex =1, pch = 21,cex.labels = 2, font.labels = 2)


#######################################
hist(Boston$crim[Boston$crim>1], breaks = 0 + (0:10)*10, xlab ="Crime Rate per capita", main = "Crime rate with breaks at 0,10")
length(Boston$chas[Boston$crim>20 & Boston$chas==0])


hist(Boston$tax[Boston$tax>1], breaks = 150 + (0:8)*100, xlab ="Tax Rate", main = "Boston tax rate with breaks at 150,250")
length(Boston$chas[Boston$tax>400 & Boston$chas==1])
length(Boston$chas[Boston$tax>400 & Boston$chas==0])

hist(Boston$ptratio[Boston$ptratio>1], breaks = 10 + (0:10)*2.5, xlab ="Pupil-Teacher Ratio", main = "pupil teacher ratio with breaks at 10,12.5..")

##############################################
########FINDING MAX OF CRIME, TAX AND PUPIL TECHER RATIO
##############################################
str(data)

##CRIME
data$crim
data[which.max(data[,'crim']),1]
histCrime <- ggplot(data, aes(x = crim), colour = blue ) + geom_histogram( binwidth = 5) 
range1 = max(data[,1]) - min(data[,1])

##TAX

data[which.max(data[,10]),10]
histTax <- ggplot(data, aes(x = tax), colour = blue ) + geom_histogram( binwidth = 10) 
range2 = max(data[,10]) - min(data[,10])

##PUPIL-TEACHER RATIO

data[which.max(data[,11]),11]
histPTratio <- ggplot(data, aes(x = ptratio), colour = blue ) + geom_histogram( binwidth = 10) 
range3 = max(data[,11]) - min(data[,11])


######################################################
###ROOMS PER DWELLING
#######################################################

##MORE THAN 7 

nrow((data[data$rm > 7 & data$chas == 0,]))


##MORE THAN 8
nrow((data[data$rm > 7 & data$chas == 0,]))   
nrow((data[data$rm > 7 & data$chas == 1,])) 

nrow((data[data$rm > 8 & data$chas == 0,]))  
nrow((data[data$rm > 8 & data$chas == 1,])) 
