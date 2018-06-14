## ----------------- ##
## MSBC 5030         ##
## Class 4 Code      ##
## ----------------- ##

## Illustrate the purpose of a for loop ##

## Want to compute the mean of a bunch of independently drawn samples
## Let's do "a bunch == 3"

set.seed(209)
mu_x = c(NA, NA,NA)   ## This is "preallocating" a vector ##

x = rnorm(100, mean= 5, sd= 10)
mu_x[1] = mean(x)

mu_x  ## display what happened
## [1] 5.977297       NA       NA

x <- rnorm(100, mean=5, sd = 10)
mu_x[2] <- mean(x)
x <- rnorm(100, mean=5, sd = 10)
mu_x[3] <- mean(x)

mu_x  ## display what happened
## [1] 5.977297 5.592956       4.605644

## The above situation is messy   ##
## because much of the code is    ##
## redundant                      ##

## instead, use a for loop

set.seed(209)
mu_x_for = c(NA, NA, NA)
for(i in 1:3){   ## tells R to do what is in brackets for i=1, i=2, i=3, and you can use i in here too.
  x <- rnorm(100, mean=5, sd= 10)
  mu_x_for[i] <- mean(x)
}

## Does the same thing ##
mu_x
mu_x_for

## The advantage kicks in when calculating *many* things ##

## Let's Compute the mean of 500 samples ##

set.seed(209)
mu_x = rep(NA, 50000)   ## Repeats NA 500 times.  I do this to have a place to store the calculated means in the for loop.
for(i in 1:50000){
  x <- rnorm(100, mean=5, sd= 10)
  mu_x[i] <- mean(x)
  i
}

mu_x  ## note: There is minimal work to go from 3 calculations to 500 calculations

hist(mu_x)
## ------------------- ##
## Conceptual Exercise ##
## ------------------- ##

## What does the population distribution look like?
## Case of Normal(5, 10)
## Take a really big sample (one million obs) to approximate the true distribution ##

x_pop <- rnorm(1000000, mean=5, sd = 10)  ## A cheat to get a "population like" object
hist(x_pop)             ## Histogram
plot(density(x_pop))    ## Density Plot
plot(density(x_pop), main = "Plot of Normal(mu = 5, sd = 10)")    ## Give a custom title
plot(density(x_pop), main = "Plot of Normal(mu = 5, sd = 10)", xlab = "X")    ## Relabel the axis

## note: Line 72 is completely self contained, don't need 68 and 69, just showing what each option does

## Normal, so 68, 95, 99.7 Rule
abline(h=0, lty="dotted")  ## Give a gridline

abline(v=-5, lty="dashed")    ## one sd away
abline(v=15, lty="dashed")

abline(v=-15, col = "red", lty="dashed")   ## two sd away
abline(v=25, col="red", lty="dashed")

abline(v=-25, col = "blue", lty="dashed")   ## three sd away
abline(v=35, col="blue", lty="dashed")


## Can "compute" the 68-95-99.7 Rule, too ##
mean(x_pop > -5 & x_pop < 15)  
mean(x_pop > -15 & x_pop < 25) 
mean(x_pop > -25 & x_pop < 35)  


## Instead of describing the population, let's compute a sample ##
set.seed(2009)
x_sample = sample(x_pop, size = 100, replace=TRUE)
hist(x_sample)
plot(density(x_sample), main="Plot of Sample from N(5,10)", xlab = "Sample Values")

## Let's overlay the 68-95-99.7 Rule from the population ##

abline(h=0, lty="dotted")  ## Give a gridline

abline(v=-5, lty="dashed")    ## one sd away
abline(v=15, lty="dashed")

abline(v=-15, col = "red", lty="dashed")   ## two sd away
abline(v=25, col="red", lty="dashed")

abline(v=-25, col = "blue", lty="dashed")   ## three sd away
abline(v=35, col="blue", lty="dashed")

## Let's calculate the 68-95-99.7 Rule ##

mean(x_sample > -5 & x_sample < 15)  ## 68  is about 57... kinda
mean(x_sample > -15 & x_sample < 25)  ## 95 is about 94
mean(x_sample > -25 & x_sample < 35)  ## 99.7 is about 99

## Point: The sample is not exact, but it is pretty close

mean(x_sample)  ## The population mean is 5

## What if I took another sample?
x_sample = sample(x_pop, size = 100, replace=TRUE)
mean(x_sample)   ## Still not 5... but different.  Sampling variability.


## What if I took 5000 samples, and stored the mean of each sample?
set.seed(2009)
X_BAR = rep(NA, 5000)

for(i in 1:5000){
  x_sample = sample(x_pop, size = 100, replace=TRUE)
  X_BAR[i] = mean(x_sample)   
}

head(X_BAR) ## Note: Because of set seed, the first two samples are the same
            ##       as before, but there are 4998 additional samples.  Each is different.

## In repeated samples, calculated statistics have a distribution ##

hist(X_BAR)
plot(density(X_BAR))

## Average of the sample means is the population mean
mean(x_pop)
mean(X_BAR)

## Standard deviation of the sample means is smaller than the population sd
sd(X_BAR)
sd(x_pop)

## Standard deviation of the sample means is called the standard error ##
sd(x_pop)/sqrt(100)   # Need to divided pop sd by square root of the sample size to get std err.
sd(X_BAR)

## Note: We could have done the above by sampling 100 observations from a N(5,10) using 
##       rnorm(100, mean = 5, sd = 10) rather than sampling from an approximate population
##       but I wanted to show that sample and rnorm both do sampling.

X_BAR = rep(NA, 5000)

for(i in 1:5000){
  x_sample = rnorm(100, mean = 5, sd = 10)
  X_BAR[i] = mean(x_sample)   
}

plot(density(X_BAR))

## Average of the sample means is the population mean
mean(x_pop)
mean(X_BAR)

## Standard deviation of the sample means is smaller than the population sd
sd(X_BAR)
sd(x_pop)

## Standard deviation of the sample means is called the standard error ##
sd(x_pop)/sqrt(100)   # Need to divided pop sd by square root of the sample size to get std err.
sd(X_BAR)


## What happens when the sample size goes up? ##
set.seed(200898388)
sample_size = c(10, 50, 100, 3000)
par(mfrow=c(3,2))

for(samp in sample_size){
  X_BAR = rep(NA, 5000)

  for(i in 1:5000){
    x_sample = rnorm(samp, mean = 5, sd = 10)
    X_BAR[i] = mean(x_sample)   
  }

  plot(density(X_BAR), main=paste("Distribution of X-bar for N = ", samp), xlim=c(0,10), xlab = "X_bar")
  cat(mean(X_BAR), "\n")
}

par(mfrow=c(1,1))

## What happens when we change the shape of the distribution? ##
## Consider a uniform population
set.seed(299288928)
x_pop_unif = runif(1000000, -10, 10)
plot(density(x_pop_unif), main="Uniform Population Distribution", sub = "Approximate Distribution", xlab = "X")

  X_BAR = rep(NA, 5000)
  
  for(i in 1:5000){
    x_sample = runif(100, -10, 10)
    X_BAR[i] = mean(x_sample)   
  }
  
  plot(density(X_BAR), main=paste("Distribution of X-bar for Uniform RV"), xlab = "X_bar")


## What happens when we increase the sample size? ##

set.seed(20088388)
sample_size = c(1, 2, 3, 5, 10, 50, 100, 500, 1000)
par(mfrow=c(3,3))
for(samp in sample_size){
  X_BAR = rep(NA, 5000)
  
  for(i in 1:5000){
    x_sample = runif(samp, -10,  10)
    X_BAR[i] = mean(x_sample)   
  }
  
  plot(density(X_BAR), main=paste("Distribution of X-bar for N = ", samp), xlab = "X_bar")
  cat(mean(X_BAR), "\n")
}


## What happens when we change the shape of the distribution? ##
## Consider a binomial population
set.seed(299288928)
x_pop_binom = rbinom(1000000, 100, 0.02)
par(mfrow=c(1,1))
hist((x_pop_binom), main="Binomial Population Distribution", sub = "Approximate Distribution", xlab = "X")

## Because it is discrete, need hist() rather than plot(density)
## Note: both discrete and right skewed.

X_BAR = rep(NA, 5000)

for(i in 1:5000){
  x_sample = rbinom(100, 100, 0.02)
  X_BAR[i] = mean(x_sample)   
}

plot(density(X_BAR), main=paste("Distribution of X-bar for Binomial RV"), xlab = "X_bar")


## What happens when we increase the sample size? ##

set.seed(20088388)
sample_size = c(1, 2, 3, 5, 10, 50, 100, 500, 1000)
par(mfrow=c(3,3))
for(samp in sample_size){
  X_BAR = rep(NA, 5000)
  
  for(i in 1:5000){
    x_sample = rbinom(samp, 100, 0.02)
    X_BAR[i] = mean(x_sample)   
  }
  
  plot(density(X_BAR), main=paste("Distribution of X-bar for N = ", samp), xlab = "X_bar")
  cat(mean(X_BAR), "\n")
}
