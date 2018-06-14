## ----------------- ##
## MSBC 5030         ##
## Quant Methods     ##
## Intro to R Script ##
## ----------------- ##

## Generate a Sample Space ##

## Five sided dice ##
S = c(1,2,3,4,5)
S2 = 1:5
S3 = c("one", "two", "three", "four", "five")

## Useful to check the "class()"
class(S2)
class(S3)

## Sample from the five sided dice ##
sample(S)
sample(S)     ## Different
sample(S3)    ## Works with 'strings' 'characters'

set.seed(3001)
sample(S)
set.seed(3001)
sample(S)     ## Same

## Randomly pick *one* roll ##
sample(S, size=1)

## Randomly pick *six* rolls ##
sample(S, size=6)   ## Error
sample(S, size=6, replace=TRUE)  ## This is like rolling the dice over and over again.

## OK... Let's try something                   ##
## Run the experiment with the five sided die  ##

## Is 1000 "many"?

set.seed(3001)
X = sample(S, size = 1000, replace=TRUE)
summary(X)
X_fac = as.factor(X)
summary(X_fac)/1000

set.seed(3002)
X = sample(S, size = 1000, replace=TRUE)
summary(X)
summary(as.factor(X)) / 1000

## Is 1000000 "many"?

set.seed(3001)
X = sample(S, size = 1000000, replace=TRUE)
summary(X)
summary(as.factor(X)) / 1000000

set.seed(3002)
X = sample(S, size = 1000000, replace=TRUE)
summary(X)
summary(as.factor(X)) / 1000000

## Second Exercise in R ##
Y = runif(10000000, min=-20, max=20)
plot(density(Y))  ## Plot the density
hist(Y)           ## Plot the histogram (takes longer)

mean(Y)  ## Average
sd(Y)
median(Y)
summary(Y)

Grneg10 =  as.numeric(Y > -10)
Grneg10 <-  Y > -10

## Inspect the first part of the sample ##
head(Grneg10)
head(Y)

head(Grneg10,20)
head(Y,20)

## Fraction Greater than -10%
summary(Grneg10)

## Other indicators for Exercise II ##
Gr0 =  as.numeric(Y>0)
Grpos10 = as.numeric(Y>10)

## Each Probability for Exercise II
mean(Grneg10)
mean(Gr0)
mean(Grpos10)

## Return to the Dice Exercise ##

two_dice = expand.grid(roll1 =S,roll2= S2)  ## Create and work with a "data frame"

class(two_dice)  ## Check that it is a data frame
head(two_dice)   ## Examine the top six rows of the data frame
names(two_dice)  ## See the names of the variables

two_dice$roll1      ## Data frames have variables the '$' extractor is how to grab them.
two_dice[,"roll1"]  ## Alternatively, can extract this way using the column names [rows,columns].
two_dice[,1]        ## Or, use column position

## Generate New Variables ##
two_dice$even_roll1  <- (two_dice$roll1 %in% c(2,4))  ## Define "even on the first roll"
two_dice$odd_roll2   = (two_dice$roll2 %in% c(1,3,5)) ## Define "odd on the second roll"

## Alternative
even_roll1 = two_dice$roll1 %in% c(2,4)
two_dice$even_roll1 = even_roll1
two_dice_mod = cbind(two_dice, even_roll1)
head(two_dice_mod)

## check it
head(two_dice)
two_dice$even_roll1_num = as.numeric(two_dice$roll1 %in% c(2,4))
two_dice$odd_roll2_num  = as.numeric(two_dice$roll2 %in% c(1,3,5))

two_dice$evenodds = two_dice$even_roll1_num + two_dice$odd_roll2_num
summary(as.factor(two_dice$evenodds))/25
summary(as.factor(two_dice$evenodds))/nrow(two_dice)

Sam = sample(two_dice$evenodds, size=1000000, replace=TRUE)
summary(as.factor(Sam))/1000000


## RV of the sum of two rolls ##

two_dice$sumtwo  = two_dice$roll1 + two_dice$roll2
summary(as.factor(two_dice$sumtwo))/25
