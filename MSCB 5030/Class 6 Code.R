## ---------------- ##
## Class 6 Script   ##
## ---------------- ##

## Let's generate 5000 repeated samples of N = 100 observations
## from a population distribution Normal(mu = 5, sig = 10) 
set.seed(13)

X_BAR = rep(NA, 5000)
SE    = rep(NA, 5000)
SD    = rep(NA, 5000)
MAX   = rep(NA, 5000)

for(i in 1:5000){
  x_sample = rnorm(100, mean = 5, sd = 10)
  X_BAR[i] = mean(x_sample) 
  SD[i]    = sd(x_sample)
  SE[i]    = sd(x_sample)/sqrt(100)
  MAX[i]   = max(x_sample)
}

plot(density(X_BAR), main = "Sampling Distribution of X_BAR", xlab= "X_BAR")
abline(v=5, col = "red")
abline(v=3, col = "blue")
abline(v=7, col = "blue")

### Note: These two numbers are close.
sd(X_BAR)   ## Repeated samples sigma_xbar
mean(SE)    ## Typical value of one sample SE = S_x / sqrt(N)

## SE is an estimator of sd(X_BAR)

x = (-3500:3500)/1000  #
d = dnorm(x)
plot(x,d, type = "l")

tv = dt(x, df=2)
lines(x,tv, col="red", lty="dashed")

tv = dt(x, df=199)
lines(x,tv, col="blue", lty="dashed")

## Plot a normal density (population)

x = (-45000:65000)/1000
d = dnorm(x, mean = 5, sd = 10)
plot(x, d, type="l", main = "Population and Sampling Distribution", xlab = "X or X_BAR", ylab= "density",ylim = c(0,0.15), xlim = c(-20,30))

## Plot a normal density (sampling distribution)
x = (-45000:65000)/1000
d = dnorm(x, mean = 5, sd = 10/sqrt(10))
lines(x, d, lty="dashed", col="blue")

x = (-45000:65000)/1000
d = dnorm(x, mean = 5, sd = 10/sqrt(100))
lines(x, d, lty="dotted", col="red")

## Stop and return to Unit 5 Slides ##



## Suppose we obtain a sample and compute a sample mean (X_BAR) of 10 ## 
## How unlikely is that?

mu_null = 5
X_BAR   = 10
se_null = 10/sqrt(10)

abline(v=X_BAR, col="red")
abline(h=0, lty="dashed")

## "One Sided"
pnorm(X_BAR, mean = mu_null, sd = se_null, lower.tail = FALSE) ## area to the right of red vertical line
pnorm(X_BAR, mean = mu_null, sd = se_null) ## area to the right of red vertical line

abline(v=0, col="red")

## "Two Sided"
2*pnorm(X_BAR, mean = mu_null, sd = se_null, lower.tail = FALSE) ## area to the right of red vertical line

## --------------------------------------------------------------- ##
## This was fine, but try a standardized way to look at it         ##
## --------------------------------------------------------------- ##

test_stat = (X_BAR - mu_null)/se_null

## The test_stat has a Normal(0, 1) distribution ##

## Plot this distribution.
x = (-3500:3500)/1000
d = dnorm(x, mean=0, sd = 1)
plot(x, d, type="l", main = "Standard Normal Distribution", xlab = "Z-Scores", ylab= "density")
abline(h=0, lty = "dashed")
abline(v=test_stat, col="red")

## "One sided" ##
pnorm(test_stat, lower.tail=FALSE)

## "Two sided" ##
abline(v=-test_stat, col="red")
2*pnorm(test_stat, lower.tail=FALSE)

## What if we "assumed" the wrong value for the mean, say mu_null = 0 

mu_null = 0
X_BAR   = 10
se_null = 10/sqrt(10)

test_stat2 = (X_BAR- mu_null)/se_null

abline(v=test_stat2, col="blue", lty="dashed")
abline(v=-test_stat2, col="blue", lty="dashed")

pnorm(test_stat2, lower.tail=FALSE)   ## one-sided (probability to the right of the blue dashed line)
2*pnorm(test_stat2, lower.tail=FALSE) ## two-sided (probability outside of blue dashed lines)

### implement it on one sample ##

x_sample
x_bar   = mean(x_sample)
std_err = sd(x_sample)/sqrt(100)
x_bar

## Is this statistically different from mu = 5? ##
test_stat = (x_bar - 0)/std_err

round(2*pnorm(abs(test_stat), lower.tail=FALSE), 3)   ## abs() is the absolute value, calculation only works in general like this

## The technically "right" way to do this. test_stat isn't *exactly* normal.
2*pt(abs(test_stat), df = 99, lower.tail=FALSE)   ## abs() is the absolute value, calculation only works in general like this

## A canned function to do it all at once
t.test(x_sample, mu=5)      ## "Fail to reject the null"

## What about other null hypotheses?
t.test(x_sample, mu = 7.5)  ## "Reject the null"

## Instead of the hypothesis test, let's construct a confidence interval ##

## for normal sampling distribution ##
lower = x_bar - 1.96*std_err
upper = x_bar + 1.96*std_err

c(lower, upper)  ## contains 5, but not 7.5 ... CIs can be used to test many hypotheses at once.
