setwd("/media/parik/New Volume/SDM/R Lab/HomeWork01")

 <- readRDS("cleanData.rds")
betaRdata<- load("data.RData")

load("/media/parik/New Volume/SDM/R Lab/cleanData.Rda", ex <- new.env())
ls.str(ex) 



large_model <- lm(mpg ~ ., data=df)
summary <- summary(large_model)
coef <- summary$coefficients
maxPr <- which.max(coef[,"Pr(>|t|)"])
###Acceleration
## REMOVING THE ACCELERATION AS IT IS LEAST SIGNIFICANT 
df$acceleration <- NULL
str(df)


