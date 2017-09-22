rm(list = ls())
library("ElemStatLearn")
install.packages("RWeka")
library("RWeka")
install.packages("FNN")
library("FNN")

#############################################################
##GETTING 2 AND 3 DIGIT'S DATA FROM THE ZIP TRAIN AND TEST DATABASES
##############################################################
zipTr<- zip.train
dim(zipTr)
zipTr <- as.data.frame(zipTr)
zipTr[, 1] <- as.factor(zipTr[, 1])  
needTr <- zipTr[zipTr$V1 ==2 | zipTr$V1 == 3,]
dim(needTr)


zipTest<- zip.test
dim(zipTest)
zipTest <- as.data.frame(zipTest)
zipTest[, 1] <- as.factor(zipTest[, 1])  

needTest <- zipTest[zipTest$V1 ==2 | zipTest$V1 == 3,]
dim(needTest)

needTest[needTest$V1 == 2,-2]

zPlot <- ggplot(needTr, aes(V2,V3, label=rownames(needTr))) + geom_point(aes(colour = as.factor(V1))) +geom_text( check_overlap= TRUE) + geom_label()
zTPlot <- ggplot(needTest, aes(V2,V3)) + geom_point(aes(colour = as.factor(V1))) + theme(legend.position = "none")

dim(needTr)
quartz()
plot(g)

####################################################
## Using CLASSIFIER TO TRAIN MODEL RWEKA
###################################################
model.knn1 <- IBk(zipTr$V1~., data = zipTr)
summary(model.knn1)

model.knn <- IBk(needTr$V1~ ., data = needTr)
summary(model.knn)

#####USING MODEL TO PREDICT TEST DATA
prediction.knn <- predict(model.knn, newdata = needTest, type = "class")
#### TABULATING RESULTS
table(`Actual Class` = needTest$V1, `Predicted Class` = prediction.knn)

error.rate.knn <- sum(needTest$V1!= prediction.knn)/nrow(needTest)
print(paste0("Accuary (Precision): ", 1 - error.rate.knn))

##########FNN
model.fnn <- knn(needTr[, -1], needTest[, -1], needTr$V1, k = 2)
summary(model.fnn)

#####USING MODEL TO PREDICT TEST DATA
prediction.fnn <- predict(model.fnn, x[45,], type = "class")
#### TABULATING RESULTS
table(`Actual Class` = needTest$V1, `Predicted Class` = prediction.fnn)
needTest[,1] <- as.numeric(needTest[, 1]) 



##################################
## NORMAL KNN METHOD
require(class)
knnplot <- function(needTr, needTest, k){
  KNN <- knn(needTr[, c(-1)], needTest, needTr$V1, k)
  needTest$predict <- KNN
  
  # change factor to numeric
  needTest$z <- c(0, 1)[sapply(needTest$predict, as.numeric)]
  
  title = paste("k=", as.character(k), sep ="")
  
  g <- ggplot(data = needTest, aes(V2, V3)) + geom_point(aes(colour = predict), size = 0.5) + geom_contour(aes(z=z), colour = 'black', size = 0.1) + theme(legend.position = "none") + labs(title = title)
  
  #add the training points in
  g <- g + geom_point(data = needTr, aes(V2, V3 ,colour = as.factor(V1), shape = 'x'))
  
  return(g)
  
}

p <- knnplot(needTr, needTest, 5)

filer <- paste("k", c(1:10), ".png", sep="")
for (i in 1:10){
  p <- knnplot(train, test, i)
  ggsave(filename = filer[i], plot = p, height = 5, width = 5)
}


#######################################################
# This code utilizes kNN for different values of k
#
# Rachael Hageman Blair
# Created: 8/1/2012
# Modified: 9/12/2015
#######################################################


setwd("/media/parik/New Volume/SDM/R Lab")

#install.packages("ggplot2")
library("ggplot2")

##################################
#  Load and visualize the data
#  
##################################
data <- read.delim("clipped_data.txt", sep = "\t", header= FALSE)
train <- data.frame(X1 = data[,1], X2 = data[,2], Y = data[,3])
dim(train)
head(train)
g <- ggplot(train, aes(X1,X2)) + geom_point(aes(colour = as.factor(Y))) + theme(legend.position = "none")
quartz()
plot(g)
ggsave(filename = "orig.png", plot = g, height = 5, width = 5)

######################################################
#  Create a test data set .... 
#  a grid of values spanning the ranges of X1 and X2
#  (long term goal: visualization)
######################################################
minX1 <- min(train$X1)
minX2 <- min(train$X2)
maxX1 <- max(train$X1)
maxX2 <- max(train$X2)

# ranges
X1.range <- seq(from = minX1, to = maxX1, length.out = 100)
X2.range <- seq(from = minX2, to = maxX2, length.out = 100)

# Create the test set
test <- data.frame(X1 = rep(X1.range, 100), X2 = rep(X2.range, each = 100))
g2 <- ggplot(test, aes(X1,X2)) + geom_point(size = 0.5)
quartz()
plot(g2)

################################
## Try different values of k
################################
require(class)
knnplot <- function(train, test, k){
  KNN <- knn(train[, c('X1', 'X2')], test, train$Y, k)
  test$predict <- KNN
  
  # change factor to numeric
  test$z <- c(0, 1)[sapply(test$predict, as.numeric)]
  
  title = paste("k=", as.character(k), sep ="")
  
  g <- ggplot(data = test, aes(X1,X2)) + geom_point(aes(colour = predict), size = 0.5) + geom_contour(aes(z=z), colour = 'black', size = 0.1) + theme(legend.position = "none") + labs(title = title)
  
  #add the training points in
  g <- g + geom_point(data = train, aes(X1,X2,colour = as.factor(Y), shape = 'x'))
  
  return(g)
  
}

###############################################
## Try differnt values of k, and save
###############################################
filer <- paste("k", c(1:10), ".png", sep="")
for (i in 1:10){
  p <- knnplot(train, test, i)
  ggsave(filename = filer[i], plot = p, height = 5, width = 5)
}





###############################
### LINEAR REGRESSION
##########################

lr.model <- lm(V1 ~ .-V1, data= needTr)
summary(lr.model)
prediction.lr <- predict(lr.model, needTest, se.fit = TRUE)

prediction.lr <- predict(lr.model, needTest[34,], se.fit = TRUE)

table(`Actual Class` = needTest$V1, `Predicted Class` = prediction.lr )
















