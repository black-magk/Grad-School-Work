## ---------------------------- ##
## In-Sample Relatedness        ##
## ---------------------------- ##

rm(list=ls())
set.seed(5)

## Generate two variables that are unrelated ##
X = rnorm(500, mean=5, sd = 2)
Y = rnorm(500, mean=2, sd = 6)

cov(X,Y)
sd(X)
sd(Y)
cor(X,Y)   ## Compute the sample correlation, not exactly zero.

par(mfrow=c(2,1))
cor_store = rep(NA, 10000)
for(i in 1:10000){                 ## Simulate 1000 correlations this way.
  X = rnorm(500, mean=5, sd = 2)
  Y = rnorm(500, mean=2, sd = 6)
  cor_store[i] = cor(X,Y)
}


round(mean(cor_store), 3)
sd(cor_store)

hist(cor_store, xlim=c(-1,1))  ## 0 is the most likely correlation, just given this.

for(i in 1:10000){                 ## Simulate 1000 correlations this way.
  X = rnorm(5000, mean=5, sd = 2)
  Y = rnorm(5000, mean=2, sd = 6)
  cor_store[i] = cor(X,Y)
}

round(mean(cor_store), 3)
sd(cor_store)

hist(cor_store, xlim=c(-1,1))  ## 0 is the most likely correlation, just given this.
par(mfrow=c(1,1))

plot(X,Y)        ## show what unrelated looks like



### ---------------------------- ##
## consider a related exercise   ##
### ---------------------------- ##
set.seed(209)

X = rnorm(5000, mean=5, sd=2)

### Now, generate a new variable ##

Z = (X - 5)^2   ## Z is completely dependent on X
cor(X,Z)

cor_store = rep(NA, 1000)

for(i in 1:1000){                 ## Simulate 1000 Correlations
  X = rnorm(5000, mean=5, sd = 2)
  Z = (X - 5)^2
  Z2= Z*20000 + 10000000000000
  cor_store[i] = cor(X,Z2)
}

hist(cor_store)  ## Zero is a very likely correlation 
mean(cor_store)  ## Average correlation is zero
sd(cor_store)    ## Fairly precise

## Note: Z is uncorrelated, but not independent.  Also, the mean of Y depends on the value of X.
##       So, not mean independent.
plot(X,Z2)

Z2 = 2*X +1000000
## ------------------------------ ##
## Let's consider another example ##
## ------------------------------ ##

set.seed(5)

X = rnorm(500, 0, 1)
W = rnorm(500, 5, 1*X^2)

cor(X,W)

plot(X,W, main = "Plot of W versus X", cex=0.5, pch=17)   ## Note that a distinct pattern is in the plot.

lines(lowess(W~X), lty = "dashed", col="red", type="l")   ## Draw an approximation to E[W | X]
                                                ## Looks mean independent (the pattern isn't about the mean)
points(lowess(W~X), lty = "dashed", col="red")   ## Draw an approximation to E[W | X]
set.seed(5)

cor_store = rep(NA, 1000)

for(i in 1:1000){
  X = rnorm(500, 0, 1)
  W = rnorm(500, 5, 1*X^2)
  cor_store[i] = cor(X,W)
}

mean(cor_store)  ## the average correlation is zero
hist(cor_store)  ## the distribution and most likely outcomes are centered on zero.


## All of these examples show uncorrelated.

X = rnorm(500, 5, 2)
M = rnorm(500, 5+2*X, 3)

plot(X,M, pch= "+", cex =0.5)   ## Clearly related


lines(lowess(M~X), col="red")   ## Draw an approximation to E[W | X]


cor(X,M)    ## Strongly correlated


cor_store = rep(NA, 1000)

set.seed(209)
for(i in 1:1000){
  X = rnorm(500, 5, 2)
  M = rnorm(500, 5+2*X, 3)
  cor_store[i] = cor(X,M)
}

mean(cor_store)
sd(cor_store)
hist(cor_store, xlim=c(-1,1))
