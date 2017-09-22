rm(list = ls())
setwd("/media/parik/New Volume/SDM/R Lab/HomeWork01")

#############################################################
#####LOADING THE CLEANED DATA INTO DF
#############################################################
#newDF <- readRDS("cleanData.rds")
alpha<- load("data.RData")


#######################################################################
##   MULTIPLE REGRESSION USING LINEAR MODEL LM
#######################################################################

linear_model1 <- lm(mpg ~ ., data=df)
summary <- summary(linear_model1)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.4049  -1.8142   0.0321   1.7865  13.9253 
# 
# Coefficients:
#                 Estimate   Std. Error  t value  Pr(>|t|)    
# (Intercept)     -8.204304   4.418235  -1.857    0.0641 .  
# cylinders      -0.689268   0.304391  -2.264    0.0241 *  
#  displacement   0.014193   0.007136   1.989    0.0475 *  
#   horsepower   -0.021568   0.012666  -1.703    0.0894 .  
## weight        -0.005469   0.000607  -9.011   < 2e-16 ***
#   acceleration  -0.123369   0.095996  -1.285    0.1996    
## year           0.665571   0.047868   13.904   < 2e-16 ***
#   origin        1.440087   0.257512   5.592  4.38e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.001 on 367 degrees of freedom
# Multiple R-squared:  0.8419,	Adjusted R-squared:  0.8389 
# F-statistic: 279.2 on 7 and 367 DF,  p-value: < 2.2e-16


coef <- summary$coefficients

########## MAXIMUM SIGNIFICANCE
minPr <- which.min(coef[,"Pr(>|t|)"])
### SO WEIGHT AND YEAR ARE HAVING MAXIMUM SIGNIFICANT RELATIONHIP TO MPG

##########  MINIMUM SIGNIFICANCE
maxPr <- which.max(coef[,"Pr(>|t|)"])
###Acceleration

############################################################
##>>>>>>>>>>>>>>>  YEAR


linear_model2 <- lm(mpg ~ .-year, data=df)
summary <- summary(linear_model2)


########################################################
### USING INTERACTIONS TO FIT MODEL
#######################################################


linear_model3 <- lm(mpg ~ cylinders+weight+horsepower+displacement+origin+year + (cylinders*displacement) + (cylinders*horsepower) +
                    (cylinders * weight) + (cylinders*origin) + (cylinders*year) + (displacement * horsepower)
                    + (displacement*weight) + (horsepower*weight) + (origin*cylinders) + (origin*displacement) + (origin*horsepower)
                    + (origin*weight) + (year*cylinders) + (year*weight) + (year*horsepower) + (year*displacement) + (year*origin)
                    + (year*horsepower), data=df)

summary(linear_model3)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             -6.153e+01  3.011e+01  -2.044 0.041715 *  
#   cylinders                1.451e+01  7.404e+00   1.959 0.050842 .  
# weight                  -2.024e-02  1.441e-02  -1.405 0.160989    
# horsepower               5.234e-01  2.491e-01   2.101 0.036338 *  
#   displacement            -2.075e-01  1.822e-01  -1.139 0.255588    
# origin                  -4.110e-01  5.566e+00  -0.074 0.941174    
# year                     1.413e+00  3.774e-01   3.744 0.000212 ***
#   cylinders:displacement  -1.034e-02  6.099e-03  -1.695 0.090863 .  
# cylinders:horsepower    -1.057e-03  1.704e-02  -0.062 0.950568    
# cylinders:weight         8.706e-04  6.685e-04   1.302 0.193704    
# cylinders:origin        -9.523e-02  4.750e-01  -0.200 0.841218    
# cylinders:year          -1.945e-01  9.174e-02  -2.121 0.034659 *  
#   horsepower:displacement  2.334e-04  1.894e-04   1.232 0.218832    
# weight:displacement      3.417e-05  1.211e-05   2.821 0.005051 ** 
#   weight:horsepower       -6.396e-05  2.449e-05  -2.612 0.009381 ** 
#   displacement:origin      1.846e-02  1.672e-02   1.104 0.270318    
# horsepower:origin       -8.356e-02  2.130e-02  -3.923 0.000105 ***
#   weight:origin            1.582e-03  1.310e-03   1.207 0.228155    
# weight:year              1.093e-04  1.712e-04   0.638 0.523625    
# horsepower:year         -3.688e-03  2.982e-03  -1.237 0.216985    
# displacement:year        1.303e-03  2.244e-03   0.581 0.561761    
# origin:year              3.162e-02  6.642e-02   0.476 0.634285    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.426 on 353 degrees of freedom
# Multiple R-squared:  0.9007,	Adjusted R-squared:  0.8948 


###################
##FROM ABOVE RESULT WE CAN OBSERVE THAT BELOW INTERACTIONS ARE SIGNIFICANT AS THEY HAVE A LOWER "Pr|t|" VALUE
#>>>>>>>>>>>
##>> horsepower:origin ;  weight:horsepower ;  weight:displacement ; cylinders:year.




