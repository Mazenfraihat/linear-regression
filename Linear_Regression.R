# ------------------------------------------------------------
# Horsepower Prediction using Linear Regression in R
# Author: Mazen Fraihat
# Description: Compares simple and multiple linear models 
#   to predict automobile horsepower using the imports-85 dataset.
# ------------------------------------------------------------

#==== 0.0  Introduction ====

## ----0.1 - Load Libraries and Data----
library(ISLR)
head(imports.85)

## -----0.2 - Explore Data----
plot(imports.85)
plot(imports.85[, 16:25])


# ==== 1.0 - Model 1 ====
# Single Predictor (1x)


## 1.1 - relationship ####
#Find the relationship between horsepower and other variables

# Effect of price on horsepower
m1.price = lm(horsepower ~ price, data=imports.85)
summary(m1.price) 
#R^2 = 0.6583

# Effect of highway MPG on horsepower
m1.highwaympg = lm(horsepower ~ highwaympg, data=imports.85)
summary(m1.highwaympg)
#R^=2 0.6577

# Effect of peak RPM on horsepower
m1.rpm = lm(horsepower ~ peakrpm, data=imports.85)
summary(m1.rpm)
#R^2 = 0.005097

# Effect of engine size on horsepower
m1.enginesize = lm(horsepower ~ enginesize, data=imports.85)
summary(m1.enginesize)
#R^2 = 0.7131 


##1.2 - Plotting Relationship####
# Plot single graph with best line in color

plot(imports.85$enginesize, imports.85$horsepower,
     xlab = "Engine Size",
     ylab = "Horsepower",
     main = "Model 1: Horsepower vs Engine Size",
     pch = 19, col = "steelblue")
abline(m1.enginesize, col = "red", lwd = 3)
legend("topleft", legend="Fitted Linear Model",col="red", lwd=3)

## 1.3 - AIC ####

AIC(m1.price, m1.highwaympg, m1.rpm, m1.enginesize)
# m1.enginesize had the best AIC and R^2

##1.4 - Model Plot####
# Plot model diagnostics for the linear model
plot(m1.enginesize)

##1.5 - Quadratic Model####
# Fit the quadratic model (adds enginesize^2 term)
m1.enginesize.sq = lm(horsepower ~ enginesize + I(enginesize^2), data=imports.85)
summary(m1.enginesize.sq)

###1.5.1 - AIC Quadratic####
# Compare AIC values for all single-predictor models including quadratic
AIC(m1.price, m1.highwaympg, m1.rpm, m1.enginesize, m1.enginesize.sq)

###1.5.2 - Plot Quadratic Model####
#Plot model diagnostics for the quadratic model
plot(m1.enginesize.sq)



##1.6 - Inverse####
#Using an inverted term instead of squared
m1.enginesize.inv = lm(horsepower ~ enginesize + I(1/enginesize), data=imports.85)
summary(m1.enginesize.inv)
###1.6.1 Comparing AIC####
AIC(m1.price, m1.highwaympg, m1.rpm, m1.enginesize, m1.enginesize.inv)





##1.7 - Comparing Plots####

####1.7.1 - Original plot####
plot(imports.85$enginesize, imports.85$horsepower)
abline(m1.enginesize, col='yellow', lwd=3)


####1.7.2 - Modified plot####

## Generate a smooth sequence of x values for the quadratic curve
x= imports.85$enginesize
xmesh = seq(0.5*min(x), 2*max(x), by=0.1) #getting lots of x values to draw the lin through

# Predict horsepower using the quadratic model
yhat = predict(m1.enginesize.sq,
               newdata=data.frame(enginesize=xmesh))

# Plot observed data and fitted models
plot(imports.85$enginesize, imports.85$horsepower, xlab="Engine Size", ylab="Horsepower", main="Horsepower vs Engine_size")
abline(m1.enginesize, col='red', lwd=3)
lines(xmesh, yhat, col='green', lwd=3)

# Add legend
legend("topleft", #where to draw the legend
       c("Linear", "Quadratic"),
       lty=c(1,1), #
       lwd=c(2,2), #line thickness
       col=c("red", "green") #colors of line"
)













#2.0 - Model 2####
# Model 2 – Multiple Predictors (Multiple Linear Regression)
# Testing how horsepower is affected by both engine size and price

##2.1 - Relation Ship####


###2.1.1 - Normal####

#enginesize + price 
m2.enginesize.price = lm(horsepower ~ enginesize + price, data=imports.85)
summary(m2.enginesize.price)
AIC(m2.enginesize.price)
#R^2 = 0.7296
#AIC = 1704.006

#enginesize + bore
m2.enginesize.bore = lm(horsepower ~ enginesize + bore, data=imports.85)
summary(m2.enginesize.bore)
AIC(m2.enginesize.bore)
#R^2 = 0.7216
#AIC = 1709.586

#enginesize + price + bore + price + wheelbase
m3.enginesize.bore.price.wheelbase = lm(horsepower ~ enginesize + bore + price + wheelbase, data=imports.85)
summary(m3.enginesize.bore.price.wheelbase)
AIC(m3.enginesize.bore.price.wheelbase)
#R^2 = 0.7694
#AIC = 1675.196



###2.1.2 - Quadratic####
m2.horsepower.price.sq = lm(horsepower ~ enginesize + I(enginesize^2) + price + I(price^2), data=imports.85)
summary(m2.horsepower.price.sq)
#R^2 = 0.7683

##2.2 - AIC####
AIC(m1.enginesize, m1.enginesize.sq, m2.horsepower.price, m2.horsepower.price.sq)


##2.3 - Over fit####

###2.3.1 - Normal####
overfit = lm(horsepower ~ ., data=imports.85)
summary(overfit)

###2.3.2 - Cleaned####
overfit2 = lm(horsepower ~ . -make, data=imports.85)
summary(overfit2) 

##2.4 - Interaction term####
m2.interaction = lm(horsepower ~ enginesize*price, data=imports.85)
summary(m2.interaction)


## 2.5 - Getting RSS for m2.enginesize.price ####
yhat_m2 = predict(m2.enginesize.price)
rss_m2.enginesize.price = sum((imports.85$horsepower - yhat_m2)^2); rss_m2.enginesize.price

