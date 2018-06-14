## MSBC 5030    ##
## Class 3 Code ##
## -------------##

rm(list=ls())  ## clear the workspace
set.seed(503003)

## Let's build an experiment within R with several RVs ##

## Setting 1: An assistant professor seeks tenure by attempting to publish high caliber research
##   For the first part, let's suppose that 3 successes are required to get tenure and that the
##   professor is allowed to initiate 30 attempts.

p = 0.05    ## Probability a paper is a success for each paper written.
N = 30      ## Number of papers to write

help(rbinom)

tenure_cases = rbinom(1000, N, p)  # Sampling 1000 binomial RVs, each is a "tenure case"
mean(tenure_cases >= 3)

## Plot the tenure cases

hist(tenure_cases)  ## note that the shape is right skewed.

## Setting 2: A salesman is allowed to make 100 cold calls.  To get a bonus, he must
##   convince 3 people to buy a stock from the pink sheets.

p = 0.015
N = 100

bonuses = rbinom(1000, N, p)  # Sampling 1000 binomial RVs, each is a "tenure case"
mean(bonuses >= 3)

hist(bonuses)  ## again skewed to the right

## Setting 3: A baseball player has 800 at bats in a season.  To make the Hall of Fame, he
## must get 250 hits this season.  His career batting average is 0.300

p = 0.300
N = 800

hits = rbinom(1000, N, p)  # Sampling 1000 binomial RVs, each is a "tenure case"
mean(hits >= 250)

hall_of_fame = hits >= 250
mean(hall_of_fame)
hist(hits)  ## starting to look symmetric

## What if we approximate the binomial using a normal distribution?

meen       = p*N
var_e_ance = p*(1-p)*N 

hits_approx = rnorm(1000, mean = meen, sd = sqrt(var_e_ance))

hist(hits_approx)

## A better comparison, a two panel plot

par(mfrow=c(2, 1))  ## sets up a panel of plots with 2 rows and 1 column... then we fill it in

hist(hits)
hist(hits_approx)

## Build a data set for another way to summarize data
hitz = data.frame(hits = hits, kind = "hits")
hitz_approx = data.frame(hits = hits_approx, kind = "hits_approx")

dat = rbind(hitz, hitz_approx)

par(mfrow=c(1,1))  ## All plot panels are set "permanently" until we change it back

boxplot(hits~kind, data=dat)  ## A great way to visually compare distributions!


## For something a bit more interesting... what are the likely outcomes for this 
## season's NL West standings
set.seed(2015)
N = 162
dodgers = rbinom(1000, N, 0.519) # Sampling 1000 binomial RVs, each is a "tenure case"
rockies = rbinom(1000, N, 0.460)  ## success rates taken from the May 30th standings 
giants  = rbinom(1000, N, 0.604)
padres  = rbinom(1000, N, 0.385)
dbacks  = rbinom(1000, N, 0.434)


dodg = data.frame(wins = dodgers, team = "dodgers")
rock = data.frame(wins = rockies, team = "rockies")
gants = data.frame(wins = giants, team = "giants")
pads = data.frame(wins = padres, team = "padres")
dbax = data.frame(wins = dbacks, team = "dbacks")

dat = rbind(dodg, rock, gants, pads, dbax)

par(mfrow=c(1,1))  ## All plot panels are set "permanently" until we change it back

boxplot(wins~team, data=dat)  ## A great way to visually compare distributions!

