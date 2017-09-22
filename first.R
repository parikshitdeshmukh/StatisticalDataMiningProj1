#Set working Directory

setwd("/media/parik/New Volume/SDM/R Lab")

#first need to install packages
install.packages("DAAG")
install.packages("lattice")
installed.packages("MASS")

source("https://bioconductor.org/biocLite.R")
biocLite("geneplotter")


#loading packages for this porject
library("DAAG")
library("lattice")
library("MASS")
library("geneplotter")


######## Working with Possums

?possum
dim((possum))

#femles data

fossum <- subset(possum, sex=="f")
dim(fossum)

write.table(fossum,file="tablefossum.txt",sep = "\t", row.names = FALSE, col.names = names(fossum))
temp <- read.delim("tablefossum.txt", sep = "\t", header = TRUE)
temp[8]
names(possum)
totlngth
par(mfrow=c(1,2))
hist(temp[8], ylim = c(0,22))

?hist

