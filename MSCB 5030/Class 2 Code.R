## MSBC 5030    ##
## Class 2 Code ##
## -------------##

rm(list=ls())  ## clear the workspace
die_roll = 1:5

two_dice <- expand.grid(roll1 = die_roll, roll2 = die_roll)
two_dice ## report to console

## Define some RVs ##
# two_dice$sum_two <- two_dice$roll1 + two_dice$roll2 (old way)
two_dice <- within(two_dice, sum_two <- roll1+roll2)
two_dice <- within(two_dice, sum_two_fac <- as.factor(sum_two))

summary(two_dice$sum_two_fac)/nrow(two_dice)  ## Frequency Distribution
table(two_dice$sum_two)/nrow(two_dice)  ## Does the same thing without clutter
summary(two_dice$sum_two)

two_dice <- within(two_dice, increase <- roll2 - roll1)
two_dice <- within(two_dice, increase_fac <- as.factor(increase))

## Frequency Distribution
summary(two_dice$increase_fac)/nrow(two_dice) 
table(two_dice$increase)/nrow(two_dice)        ## Does the same thing without clutter

## Store the frequency distributions
tab_sum = table(two_dice$sum_two)/nrow(two_dice)
tab_inc = table(two_dice$increase)/nrow(two_dice)

attributes(tab_sum)
tab_sum
names(tab_sum)
val_sum = as.numeric(names(tab_sum))  ## Need the values of RV outcomes for Expected Value
val_inc = as.numeric(names(tab_inc))

## Compute the expected value ##
## Idea is to multiply values by probabilities and add up
val_sum*tab_sum
EV_sum = sum(val_sum*tab_sum)  ## add up multiplied val*probs

EV_sum2 = val_sum %*% tab_sum  ## does the same thing, just stores differently
                               ## The %*% is matrix multiplication

EV_inc = sum(val_inc*tab_inc)
EV_inc2 = val_inc %*% tab_inc

## Population standard deviation ##

## do it for the sum of two dice

sq_diff_from_EV = (val_sum - EV_sum)^2
var_sum = sum(sq_diff_from_EV*tab_sum)
sqrt(var_sum)

cu_diff_from_EV = (val_sum - EV_sum)^3
sk_sum = sum(cu_diff_from_EV*tab_sum)


qu_diff_from_EV = (val_sum - EV_sum)^4
qu_sum = sum(qu_diff_from_EV*tab_inc)  ## normal is 3

normm = rnorm(1000)
kurtosis(normm) ## if you could, it would be 3.

## By comparison, compute the sample standard deviation ##

## first need to sample. can just sample from the original column in "two_dice"
set.seed(42)
X_sum = sample(two_dice$sum_two, size=5000000, replace=TRUE)
sd(X_sum)  ## Not quite 2


## Here try a "for loop" ##
## Exercise for the enthusiastic.  Try to figure out what the lesson is here ... ##
all_sd_50 =NULL
for(i in 1:1000){
  X_sum = sample(two_dice$sum_two, size=50, replace=TRUE)
  this_sd = sd(X_sum)  
  all_sd_50  = c(all_sd_50, this_sd)
}

hist(all_sd_50)

all_sd_500 =NULL
for(i in 1:1000){
  X_sum = sample(two_dice$sum_two, size=500, replace=TRUE)
  this_sd = sd(X_sum)  
  all_sd_500  = c(all_sd_500, this_sd)
}
summary(all_sd_500)
hist(all_sd_500)


all_sd_5000 =NULL
for(i in 1:1000){
  X_sum = sample(two_dice$sum_two, size=5000, replace=TRUE)
  this_sd = sd(X_sum)  
  all_sd_5000  = c(all_sd_5000, this_sd)
}
summary(all_sd_5000)
hist(all_sd_5000)
