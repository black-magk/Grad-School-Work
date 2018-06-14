## --------------------- ##
##  Class 7 Code         ##
## Regression Mechanics  ##
## and reading data      ##
## --------------------- ##

rm(list=ls())
install.packages("OIdata")
library(OIdata)
data(birds)

## Plot the relationship between height and speed
plot(birds$height, birds$speed, xlab = "height of bird-plane collision", ylab = "speed at impact", 
     main = "Airplane - Bird Collisions")
abline(coef(lm(speed~height, data=birds)))

## Compute the correlation ##
cor(birds$height, birds$speed)  ## Returns NA
cor(birds$height, birds$speed, na.rm=TRUE)  ## works for mean, sd; not here

arrr     = cor(birds$height, birds$speed, use="complete.obs")  ## moderately strong correlation
rsquared = arrr^2

## Compute the regression "by hand"

birds = birds[complete.cases(birds[,c("speed","height")]),]  ## Drops missing heights or speeds


arrr     = cor(birds$height, birds$speed)  ## moderately strong correlation
rsquared = arrr^2

xbar_ht = mean(birds$height)
xbar_sp = mean(birds$speed)

s_ht = sd(birds$height)
s_sp = sd(birds$speed)

## Compute the slope
b1 = arrr*(s_sp/s_ht)
b1_alt = cov(birds$height, birds$speed)/var(birds$height)

## Use the slope to compute the intercept
b0 = xbar_sp - b1*xbar_ht

## Use the canned regression function
lm(speed~height, data = birds)        ## a lot more efficient

## But, oh... so much more!
my_regression = lm(speed~height, data = birds) 

## Try plotting it.
par(mfrow=c(2,2))
plot(my_regression)    ## gives diagnostic plots

residuals(my_regression)  ## computes residuals
hist(residuals(my_regression))  ## computes residuals
coef(my_regression)   ## Just the coefficients, can be useful

## Get standard errors, and the "regression table" ##
summary(my_regression)
coef(summary(my_regression))  ## just the regression table

## Does the regression get the right parameters?  ##
## Let's do a simulation

set.seed(209)
beta0 = 2
beta1 = 4

stor_mat = matrix(rep(NA, 2000), ncol=2)  ## do 1000 replications

for(i in 1:1000){
  ## simulate data
  X = runif(500, -20, 20)
  e = rnorm(500, 0, sd = 5)
  Y = beta0 + beta1*X + e
  
  stor_mat[i,] <- coef(lm(Y~X))
}
par(mfrow=c(2,1))
hist(stor_mat[,1], main = "sampling distribution of intercept", xlab = "b0")
hist(stor_mat[,2], main = "sampling distribution of slope", xlab = "b1")

## Some additional useful commands ##
colMeans(stor_mat)
apply(stor_mat, 1, mean)
apply(stor_mat, 2, mean)

apply(stor_mat, 2, sd)

coef(summary(lm(Y~X)))[,2]
## Reading data in to do a regression ##

msft_returns = read.csv("Dropbox/Business Stat Class/msft_returns.csv", header=TRUE)

## format the date 
head(msft_returns)
msft_returns$Date = as.Date(msft_returns$Date, "%m/%d/%Y")

plot(msft_returns$Date, msft_returns$MSFT, type ="l", xlab="Date", ylab="Microsoft Return")
plot(msft_returns$Date, msft_returns$SP500, type ="l", xlab="Date", ylab="S&P 500 Return")

par(mfrow=c(1,1))
plot(msft_returns$SP500, msft_returns$MSFT, xlab="S&P500 Return", ylab="Microsoft Return")

lm(MSFT~SP500, data=msft_returns)
summary(lm(MSFT~SP500, data=msft_returns))

