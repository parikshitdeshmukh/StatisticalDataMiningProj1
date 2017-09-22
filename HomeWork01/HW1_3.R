rm(list = ls())
library("ElemStatLearn")
#install.packages("RWeka")
library("RWeka")
#install.packages("FNN")
library("FNN")


#############################################################
##GETTING 2 AND 3 DIGIT'S DATA FROM THE ZIP TRAIN AND TEST DATABASES
##############################################################
zipTr <- as.data.frame(zip.train)
#zipTr[, 1] <- as.factor(zipTr[, 1])  
zipTr <- zipTr[zipTr$V1 ==2 | zipTr$V1 == 3,]
dim(zipTr)

zipTest <- as.data.frame(zip.test)
#zipTest[, 1] <- as.factor(zipTest[, 1])  
zipTest <- zipTest[zipTest$V1 ==2 | zipTest$V1 == 3,]
dim(zipTest)system.time(


########################################################################################################
##>>>>>>>>>USING KNN CLASSIFICATION<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
########################################################################################################

system.time(model.knn <- knn(zipTr, zipTest, zipTr$V1, k = 2))
summary(model.knn)
str(model.knn)

model.knn <-as.data.frame(model.knn)
knn<-model.knn$model.knn[1:364]
# 
# #####USING MODEL TO PREDICT TEST DATA
# prediction.knn2 <- predict(model.knn2 ,  zipTest, type = "class")
#### TABULATING RESULTS
table(`Actual Class` = zipTest$V1, `Predicted Class` = knn)

##ERROR RATE
error.rate.knn <- sum(zipTest$V1!= knn)/nrow(zipTest)## >>>>>>>>>>>> 0.02197802
print(paste0("Accuary (Precision): ", 1 - error.rate.knn)) ## >>>>>>>> 0.978022


for (i in 1:10){
  p <- knnplot(train, test, i)
  ggsave(filename = filer[i], plot = p, height = 5, width = 5)
}




########################################################################################################
### >>>>>>>>>>>>>>>>>>>>> USING LINEAR REGRESSION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
########################################################################################################
system.time(lr.model <- lm(zipTr$V1 ~ ., data= zipTr))
summary(lr.model)##>>>>>>>>>>>>>>>>>>>>> We get RSE here

system.time(prediction.lr <- predict(lr.model, zipTest, se.fit = TRUE))
lr<- as.data.frame(prediction.lr$fit)
lr<- lr$`prediction.lr$fit

#### APPROXIMATING THE VALUE TO NEAREST INTEGER 2 OR 3
for (i in 1:364){
  
  if(lr[i]> 2.5) lr[i]=3 else lr[i] =2
  
}

error.rate.lr <- sum(zipTest$V1 != lr)/nrow(zipTest) #### >>>> 0.04120879
print(paste0("Accuary (Precision): ", 1 - error.rate.lr)) ## >>>>>>>>  0.9587912

table(`Actual Class` = zipTest$V1, `Predicted Class` =lr)









###############################################
##
## Since the erro rate for KNN is o.o2 and for Linear Regression 0.04, it can be concluded
## that the KNN Cassification algorithm is more accurate.
##
##
##
##
###############################################
