rm(list = ls())
library("ElemStatLearn")
#install.packages("RWeka")
library("RWeka")
#install.packages("FNN")
library("FNN")
setwd("/media/parik/New Volume/SDM/R Lab/HomeWork01")


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
dim(zipTest)


########################################################################################################
##>>>>>>>>>USING KNN CLASSIFICATION<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
########################################################################################################
knnCompute<- function(train, test, cl, k){
            
          system.time(model.knn <- knn(train, test, cl, k))
          return(model.knn)
}

k=1
error.rate.knn=0

for (i in seq(1,15,2)){
  model.knn=knnCompute(zipTr, zipTest, zipTr$V1, i);
  model.knn <-as.data.frame(model.knn);
  knn<-model.knn$model.knn[1:364];
  error.rate.knn[k] <- sum(zipTest$V1!= knn)/nrow(zipTest);
  print(paste0("Accuary (Precision): ", 1 - error.rate.knn));
  k=k+1;
}
print(error.rate.knn)


################################
##>> PLotting Error for k=1,3,5,7,9,11,13,15
##################################


png("ErroRate_vs_Knn.png")
m=c(1,3,5,7,9,11,13,15)
plot(m, error.rate.knn,type = "l" , main="Error rate Vs K",xlab = "K", ylab = "Error Rate")
dev.off()
# 
# #####USING MODEL TO PREDICT TEST DATA
#prediction.knn2 <- predict(model.knn ,  zipTest, type = "class")
#### TABULATING RESULTS
#table(`Actual Class` = zipTest$V1, `Predicted Class` = knn)

##Minimum ERROR RATE
min.error.rate.knn <- min(error.rate.knn)## >>>>>>>>>>>> 0.02472527
print(paste0("Accuary (Precision): ", 1 - min.error.rate.knn)) ## >>>>>>>> 0.978022



########################################################################################################
### >>>>>>>>>>>>>>>>>>>>> USING LINEAR REGRESSION <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
########################################################################################################
system.time(lr.model <- lm(zipTr$V1 ~ ., data= zipTr))
summary(lr.model)##>>>>>>>>>>>>>>>>>>>>> We get RSE here

system.time(prediction.lr <- predict(lr.model, zipTest, se.fit = TRUE))
lr<- as.data.frame(prediction.lr$fit)
lr<- lr$`prediction.lr$fit`

#### APPROXIMATING THE VALUE TO NEAREST INTEGER 2 OR 3
for (i in 1:364){
  
  if(lr[i]> 2.5) lr[i]=3 else lr[i] =2
  
}

### ERROR RATE
error.rate.lr <- sum(zipTest$V1 != lr)/nrow(zipTest) #### >>>> 0.04120879
print(paste0("Accuary (Precision): ", 1 - error.rate.lr)) ## >>>>>>>>  0.9587912

table(`Actual Class` = zipTest$V1, `Predicted Class` =lr)


###############################################
##
## Since the erro rate for KNN is o.o2 and for Linear Regression 0.04, it can be concluded
## that the KNN Cassification algorithm is more accurate.
##
##
###############################################
