## --------------------- ##
##  Class 7 Code         ##
## Regression Mechanics  ##
## and reading data      ##
## --------------------- ##

## This is the code that I wrote during class to cover the part I couldn't read in at the time 
## when I realized I didn't have admin privileges on the classroom computer.

library(foreign)
edu <- read.dta("C://Users//joco0003//Desktop//edudat2.dta")

## Plots: getting used to data
plot(wage~educ, data=edu)
boxplot(wage~educ, data=edu)
boxplot(log(wage)~educ, data=edu)

## Scatter plot: plot the fitted line
plot(log(wage)~educ, data=edu)
mylm = lm(log(wage)~educ, data=edu)
abline(coef(mylm))

## by hand, regression ##
cov_yx = cov(log(edu$wage), edu$educ)
sd_x  = sd(edu$educ)
sd_y  = sd(log(edu$wage))
cor_yx = cor(log(edu$wage), edu$educ)

bet1 = cov_yx/(sd_x^2)
bet1.alt = cor_yx*(sd_y/sd_x)
cor_yx^2
plot(mylm)


