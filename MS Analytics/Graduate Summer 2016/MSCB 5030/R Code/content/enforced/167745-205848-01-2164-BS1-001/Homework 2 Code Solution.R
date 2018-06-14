## -------------- ##
## HW2 Code       ##
## -------------- ##

set.seed(50302)
rm(list=ls())   ## Clear the workspace

## Problem (1) Generate Data
## (a) and (b)

x_norm = rnorm(500, mean = 0.2, sd = 0.1)
x_bin  = rbinom(500, 40, 0.05)

## (2) Sample Characteristics

## (a) means
xbar_n = mean(x_norm)  ## Compute the sample means
xbar_b = mean(x_bin)

xbar_n  ## Print to console
xbar_b


## (b) standard deviations
sd_n = sd(x_norm)  ## Compute sample standard deviations
sd_b = sd(x_bin)

sd_n  ## Print to console
sd_b

## (c) standard errors

se_n = sd_n/sqrt(500)
se_b = sd_b/sqrt(500)

se_n
se_b 

## (d) Comparisons
## d.i.

z_n = (xbar_n - 0.2)/sd_n  ## computes the number of standard deviations distance
z_b = (xbar_b - 2)/sd_b    ## between xbar and mu

z_n
z_b

## d.ii.

z_n2 = (xbar_n - 0.2)/se_n  ## Does the same with standard errors.
z_b2 = (xbar_b - 2)/se_b

z_n2
z_b2

## d.iii.
## the first "z-score" in d.i is better for individual observations than for means
## the second "z-score" in d.ii is better for quantifying how different the means are from one another.

## (3) Plotting.

par(mfrow=c(1,1))

## a.i. Histograms
hist(x_norm)
hist(x_bin)

## a.ii. Scatterplot
plot(x_norm, x_bin, xlab = "Normal RV", ylab = "Binomial RV", main = "Relating Normal to Binomial (Independent case)")

## a.iii. Side-by-side boxplots
boxplot(x_norm~x_bin, xlab = "Binomial Variable", ylab = "Normal Variable", main = "Side-by-Side Boxplots")

## a.iv.  Two-panel of histograms
par(mfrow=c(2, 1))
hist(x_norm)
hist(x_bin)

## (4) The slightly more involved setting. 

x_draw4 = rbinom(500, 40, 0.05)
x_mix = rep(NA, 500)

for(i in 1:500){
  this_draw = x_draw4[i]
  this_mu = this_draw*0.5
  this_sd = 0.15*this_draw^2
  x_mix[i] = rnorm(1, mean = this_mu, sd = this_sd)
}

### ----------------------------------------###
### What follows is a repeat of (2) and (3) ###
### ----------------------------------------###

## (2) Sample Characteristics

## (a) means
xbar_n2 = mean(x_mix)  ## Compute the sample means
xbar_b2 = mean(x_draw4)

xbar_n2  ## Print to console
xbar_b2


## (b) standard deviations
sd_n2 = sd(x_mix)  ## Compute sample standard deviations
sd_b2 = sd(x_draw4)

sd_n2  ## Print to console
sd_b2

## (c) standard errors

se_n2 = sd_n/sqrt(500)
se_b2 = sd_b/sqrt(500)

se_n2
se_b2 

## (d) Comparisons
## d.i.

z_n3 = (xbar_n - 0.2)/sd_n  ## computes the number of standard deviations distance
z_b3 = (xbar_b - 2)/sd_b    ## between xbar and mu

z_n3
z_b3

## d.ii.

z_n4 = (xbar_n - 0.2)/se_n  ## Does the same with standard errors.
z_b4 = (xbar_b - 2)/se_b

z_n4
z_b4

## d.iii.
## the first "z-score" in d.i is better for individual observations than for means
## the second "z-score" in d.ii is better for quantifying how different the means are from one another.

## (3) Plotting.

par(mfrow=c(1,1))

## a.i. Histograms
hist(x_mix)
hist(x_draw4)

## a.ii. Scatterplot
plot(x_mix, x_draw4, xlab = "Normal RV", ylab = "Binomial RV", main = "Relating Normal to Binomial (Independent case)")

## a.iii. Side-by-side boxplots
boxplot(x_mix~x_draw4, xlab = "Binomial Variable", ylab = "Normal Variable", main = "Side-by-Side Boxplots")

## a.iv.  Two-panel of histograms
par(mfrow=c(2, 1))
hist(x_mix)
hist(x_draw4)

## x_draw4 is quite similar to x_bin, but now x_mix is no longer a normal distribution
## x_mix is also quite related to x_draw4.  Both the mean and variance increase as the 
## realization of the x_draw4 variable gets higher, and this is apparent in the 
## scatterplot as well as the side-by-side boxplots.
