### -------------------- ###
### Class 8 Code         ###
### -------------------- ###


## --------------------- ##
## Example 1: Crime and  ##
## Neighborhoods in      ##
## Denver                ##
## --------------------- ##

nbhds = read.csv("Dropbox/denver_nbhds.csv", head=TRUE)

names(nbhds)  ## see what is available

## Lunch causes crime?
lm_lunch = lm(crime_rate~pct_freelunch, data=nbhds)
summary(lm_lunch)  ## no! a third factor is likely driving both.

lm_lunch2 = lm(crime_rate~pct_freelunch, data=nbhds[c(-7),])
summary(lm_lunch2)  ## no! a third factor is likely driving both.

## Let's examine how changes in crime depend on changes in population ##

lm_ch= lm(pct_crime_ch~pct_pop_ch, data=nbhds)   ## 
summary(lm_ch)

## Look for outliers
hist(nbhds$pct_pop_ch)
hist(nbhds$pct_crime_ch)

## construct residual plot ##
r = residuals(lm_ch)
plot(nbhds$pct_pop_ch,r, xlab="Percentage population change", ylab = "Residuals")

## Different solutions ##

## Drop the outlier
nbhds_noout = nbhds[nbhds$pct_pop_ch < 50, ]   ## drop the outliers

lm_chout = lm(pct_crime_ch~pct_pop_ch, data=nbhds_noout)
summary(lm_chout)

plot(nbhds$pct_pop_ch, nbhds$pct_crime_ch, xlab= "Population Change (%)", ylab = "Crime Change (%)")
plot(pct_crime_ch~pct_pop_ch, data = nbhds)
abline(coef(lm_chout), col= "red")
abline(coef(lm_ch), col= "red", lty="dotted")

## Winsorize
hi = quantile(nbhds$pct_pop_ch, 0.99)
nbhds$W_pct_pop_ch = ifelse(nbhds$pct_pop_ch>hi,hi, nbhds$pct_pop_ch)

lm_chwin = lm(pct_crime_ch~W_pct_pop_ch, data=nbhds)
summary(lm_chwin)

## Quantile Regression  ## The minimum absolute deviation (easy to do in R)
install.packages("quantreg")
library(quantreg)
qr_ch = rq(pct_crime_ch~pct_pop_ch, data=nbhds)
summary(qr_ch)

crime_lots = lm(crime_rate~pct_children+pct_freelunch+pct_hhinc, data=nbhds)
summary(crime_lots)
## --------------------- ##
## Example 2: Speed and  ##
## gender and height of  ##
## UCLA students         ##
## --------------------- ##

speed = read.csv("Dropbox/Business Stat Class/speed_gender_height.csv", head=TRUE)

sp_lm = lm(speed~height, data=speed)
summary(sp_lm)                    ## strong relationship between height and fastest speed driven

## try something that doesn't look like it would work:
gend_lm = lm(speed~gender, data=speed)
summary(gend_lm)

## Another way to see the relationship (side-by-side boxplots)
boxplot(speed~gender, data=speed, notch=TRUE)  ## the notch does a visual hypothesis test

## Is height related to speed "within" gender?
fem = speed[speed$gender=="female",]
mal = speed[speed$gender=="male", ]

sp_lm_m = lm(speed~height, data=mal)
summary(sp_lm_m)                    ## strong relationship between height and fastest speed driven

par(mfrow=c(1,2))
plot(speed~height, data=mal, pch="+", col = "blue", cex = 0.5)
abline(coef(lm(speed~height, data=mal)), col = "red")
title("Regression for Males")

sp_lm_f = lm(speed~height, data=fem)
summary(sp_lm_f)        

plot(speed~height, data=fem, pch="+", col = "blue", cex = 0.5)
abline(coef(lm(speed~height, data=fem)), col = "red")
title("Regression for Females")

## ---------------------------------------------------- ##
## There has to be a more efficient way to do this...   ##
## ---------------------------------------------------- ##

sp_mult = lm(speed~height+gender, data=speed)
summary(sp_mult)

sp_mult2 = lm(speed~height*gender, data=speed)
summary(sp_mult2)

sp_mult3 = lm(speed~height+gender+height:gender, data=speed)
summary(sp_mult3)

install.packages("memisc")
library(memisc)
mtable(sp_mult, sp_mult2, sp_mult3)

## ---------------------------------------- ##
## Example 3: Urban Owners Data Set         ##
## Percent Owner Occupied versus Urban      ##
## ---------------------------------------- ##

urb = read.csv("Dropbox//Business Stat Class//urban_owners.csv", header=TRUE)

urban1 = lm(pct_owner_occupied~poppct_urban, data=urb)
summary(urban1)           ## Strong Negative Relationship

par(mfrow=c(1,1))
plot(pct_owner_occupied~poppct_urban, data=urb, xlab="Pct Population Urban", ylab = "Pct Urban Occupied")
abline(coef(urban1))
title("Owner Occupied (%) versus Population Urban (%)")


plot(pct_owner_occupied~poppct_urban, data=urb, xlab="Pct Population Urban", ylab = "Pct Urban Occupied", ylim=c(0, 100), xlim=c(0,100))
abline(coef(urban1))
title("Owner Occupied (%) versus Population Urban (%), Different Scale")

## Do a Residual plot
par(mfrow=c(1,1))
urb$res = residuals(urban1)
plot(res~poppct_urban, data=urb, main="Residual Plot")
abline(h=0, col="red", lty="dashed")


## Mini-mult example

urblm0 = lm(pct_owner_occupied~poppct_urban+areapct_urban, data = urb)
urblm = lm(pct_owner_occupied~poppct_urban+areapct_urban+popden_urban, data = urb)
urblm2 = lm(pct_owner_occupied~poppct_urban+areapct_urban+popden_urban+popden_rural, data = urb)

mtable(urblm0, urblm, urblm2)


## ---------------------------------------- ##
## Example 3.5: Beta and Alpha              ##
## with Jet Blue Data                       ##
## ---------------------------------------- ##

bigblue = read.csv("Dropbox//Business Stat Class//bigblue.csv", header=TRUE)

blu_lm = lm(rJBLU~rSP500, data = bigblue)
blu_lm2 = lm(rJBLU~rSP500+rOIL, data = bigblue)

msft_lm = lm(rMSFT~rSP500, data = bigblue)
msft_lm2 = lm(rMSFT~rSP500+rOIL, data = bigblue)

library(memisc)

mtable(blu_lm, blu_lm2, msft_lm, msft_lm2)

## ---------------------------------------- ##
## Example 4: Total Housing Units vs        ##
## state population.                        ##
## ---------------------------------------- ##

## Examine Total Housing Units

hist(urb$total_housing_units_2000)
hist(log(urb$total_housing_units_2000))

hist(urb$pop_st)
hist(log(urb$pop_st))

## Regression in "levels" (outliers and all)
levls = lm(total_housing_units_2000~pop_st, data=urb)
summary(levls)
plot(total_housing_units_2000~pop_st, data=urb)  ## has outliers

## Regression of level on log ##
lev_log = lm(total_housing_units_2000~log(pop_st), data=urb)
summary(lev_log)

plot(total_housing_units_2000~log(pop_st), data=urb)  ## not linear

## Regression in "logs"
lawd_log = lm(log(total_housing_units_2000)~log(pop_st), data=urb)
summary(lawd_log)

plot(log(total_housing_units_2000)~log(pop_st), data=urb)  ## Looks reasonable, linear, predictive, etc.


## ---------------------------------------- ##
## Example 6: Baseball Stats                ##
## How to win baseball games?               ##
## ---------------------------------------- ##

mlb = read.csv("Dropbox//Business Stat Class//mlb11.csv", header=TRUE)

hitz = lm(wins~hits, data=mlb)
summary(hitz)
plot(wins~hits, data = mlb)

batt = lm(wins~bat_avg, data = mlb)
batt = lm(wins~I(100*bat_avg), data = mlb)
summary(batt)
plot(wins~bat_avg, data=mlb)

runz = lm(wins~runs, data = mlb)
summary(runz)
plot(wins~runs, data=mlb)

homers = lm(homeruns~runs, data = mlb)
summary(homers)
plot(wins~homeruns, data=mlb)

all_lm = lm(wins~homeruns+runs+bat_avg+hits, data=mlb)

int_lm = lm(wins~(homeruns+runs)*I(bat_avg>0.250), data=mlb)

## ---------------------------------------- ##
## Example 7: Beta and Alpha                ##
## with microsoft data                      ##
## ---------------------------------------- ##

msft_returns = read.csv("Dropbox/Business Stat Class/msft_returns2.csv", header=TRUE)

## format the date 
head(msft_returns)
msft_returns$Date = as.Date(msft_returns$Date, "%m/%d/%Y")

par(mfrow=c(3,1))
plot(msft_returns$Date, msft_returns$MSFT, type ="l", xlab="Date", ylab="Microsoft Return")
plot(msft_returns$Date, msft_returns$SP500, type ="l", xlab="Date", ylab="S&P 500 Return")
plot(msft_returns$Date, msft_returns$Russ3K, type ="l", xlab="Date", ylab="Russell 3000 Return")

par(mfrow=c(2,1))
plot(msft_returns$SP500, msft_returns$MSFT, xlab="S&P500 Return", ylab="Microsoft Return")
plot(msft_returns$Russ3K, msft_returns$MSFT, xlab="Russell 3000 Return", ylab="Microsoft Return")

## Using S&P 500 as measure of market
sp_msft = lm(MSFT~SP500, data=msft_returns)
summary(sp_msft)

## Using Russell 3000 as measure of market
ru_msft = lm(MSFT~Russ3K, data=msft_returns)
summary(ru_msft)

## Do pre-post Lehman split ... uses logical statements to select the rows we want
pre_leh = msft_returns[msft_returns$Date < as.Date("2008-09-15"),]
post_leh = msft_returns[msft_returns$Date > as.Date("2008-09-15"),]


## Using S&P 500 as measure of market
sp_msft_pre = lm(MSFT~SP500, data=pre_leh)
summary(sp_msft_pre)

## Using Russell 3000 as measure of market
ru_msft_pre = lm(MSFT~Russ3K, data=pre_leh)
summary(ru_msft_pre)


## Using S&P 500 as measure of market
sp_msft_post = lm(MSFT~SP500, data=post_leh)
summary(sp_msft_post)

## Using Russell 3000 as measure of market
ru_msft_post = lm(MSFT~Russ3K, data=post_leh)
summary(ru_msft_post)

## Look up beta on yahoo finance, and it is 0.73
## How?
msft_returns = msft_returns[complete.cases(msft_returns),]
msft_last60 = msft_returns[1:60,]
sp_yahoo = lm(MSFT~SP500,data= msft_last60)

msft_last36 = msft_returns[1:36,]
sp_yahoo2 = lm(MSFT~SP500,data= msft_last36)

msft_last36_1 = msft_returns[2:37,]
sp_yahoo3 = lm(MSFT~SP500,data= msft_last36_1)

beta_data = NULL

for(i in 1:(nrow(msft_returns)-35)){
  beg = i
  end = i+35
  thisdat = msft_returns[beg:end,]
  bet_date    = msft_returns[beg,"Date"]
  bet    = coef(lm(MSFT~SP500, data=thisdat))[2]  ## just grab beta
  bet_df = data.frame(date = bet_date, beta = bet)
  beta_data = rbind(beta_data, bet_df)
}

par(mfrow=c(1,1))
plot(beta~date, data=beta_data, type = "l")
abline(h=1, col="blue", lty="dashed")
title("Beta Changes Over Time")

## Adjusted Beta ##
beta_data$adj_beta = (2/3)*beta_data$beta + (1/3)*1

lines(adj_beta~date, data=beta_data, col="red")


## ---------------------------------------- ##
## Example 8: Bootstrapping and skewed      ##
##            distributions.                ##
## ---------------------------------------- ##

urb = read.csv("Dropbox//Business Stat Class//urban_owners.csv", header=TRUE)

## Bootstrap Illustration on the mean of percent vacant
set.seed(209)
pv = urb$pct_vacant
N  = length(pv)
B = 500    ## Number of bootstrap re-samples
boot_stor = rep(NA, B)

for(b in 1:B){
  pv_boot = sample(pv, size = N, replace=TRUE)
  boot_stor[b] = mean(pv_boot)
}

sd(boot_stor)   ## Bootstrapped Standard Error
mean(pv)        ## Estimate
mean(boot_stor) ## Average of Bootstrapped Re-Samples

## This is a case where the original formula is valid ##
## So, it should match the true value
##--------------- ##

sd(pv)/sqrt(N)  ## Pretty close to the bootstrapped SE.


## Slightly more complicated example ##


urban1 = lm(pct_owner_occupied~poppct_urban, data=urb)
summary(urban1)           ## Strong Negative Relationship

par(mfrow=c(1,1))
plot(pct_owner_occupied~poppct_urban, data=urb, xlab="Pct Population Urban", ylab = "Pct Owner Occupied")

## Assumption is violated!  Not constant variance + outliers!
## Ideal for bootstrapping ##

set.seed(209)
N  = nrow(urb)
B = 5000    ## Number of bootstrap re-samples
boot_stor = matrix(rep(NA, 2*B), ncol=2)

for(b in 1:B){
  urb_idx = sample(1:N, size = N, replace=TRUE)
  boot_urb = urb[urb_idx, ]
  boot_lm = lm(pct_owner_occupied~poppct_urban, data=boot_urb)
  boot_stor[b,] = coef(boot_lm)
}

colMeans(boot_stor)
apply(boot_stor, 2, sd)
sd(boot_stor[,1])
sd(boot_stor[,2])
