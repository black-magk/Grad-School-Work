
## ------------------------- ##
## MSBC 5030 Homework 1 Code ##
## ------------------------- ##
## Author: Tony Cookson      ##
## Date: 24 May 2015         ##
## Version 1.0               ##
## ------------------------- ##

## Part TWO R Code ##

set.seed(5030)   ## Make Replicable

## (1) Sample Spaces ##

## (a) Generate a sample space for an eight sided die. ##
S_oct = 1:8

## (b) Generate a sample space of a coin toss with outcomes "H" and "T"
S_coin = c("H", "T")

## (2) Sampling  ##
samp_size = 100        ## Note: Can prespecify an option that gets used over and over again to make code easy to change

## (a) sample from discrete sample spaces in (1)
X_oct = sample(S_oct, size = samp_size, replace = TRUE)
X_coin = sample(S_coin, size = samp_size, replace = TRUE)

## (b) sample from continuous distributions (i) Uniform[-1,1], (ii) Normal(mu = 5, sd = 1)
X_uni = runif(samp_size, -1, 1)
X_norm = rnorm(samp_size, 5, 1)

## (3) Summarize the samples
## (a) basic summaries

## (i) summary() function

summary(X_oct)
summary(X_coin)
summary(X_uni)
summary(X_norm)

## Note: On the above samples, X_oct and X_coin do not make much sense because they are categories
##       rather than numbers to be averaged.

## (ii) coerce to factors, and summarize qualitative variables
X_oct_fac = as.factor(X_oct)
X_coin_fac    = as.factor(X_coin)

summary(X_oct_fac)
summary(X_coin_fac)

## Alternatively, use the table() function as we did in class [Avoids having to generate a new object]
table(X_oct)
table(X_coin)

## Note: learning to use as.factor() builds character. It also builds knowledge about factors vs. strings or
##       numerics.

## (3b) summarize the standard deviation for each sample

sd(X_oct)
sd(X_coin)
sd(X_uni)
sd(X_norm)

## What do we learn here?  
## i) With X_oct, R will compute a standard deviation, even though the numeric values are categories.
##    Be careful in this instance because the interpretation is a little odd.
## ii) With X_coin, R just simply won't compute a standard deviation because the variable is not quantitative.
##
## iii) With X_uni, the sample standard deviation is pretty close to the true sd for a unif[-1,1].
##      Looking it up on Wikipedia ("standard deviation of uniform distribution"), true sd is sqrt(8/12)
##      which is 0.816.
## 
## iv) With X_norm, the sample sd is prettly close to the true sd of 1.  These are not exact due to random
##     sampling errors.  As the sample size gets larger, these errors will tend to decline.  But, a sample
##     of only 100 observations is not too large.