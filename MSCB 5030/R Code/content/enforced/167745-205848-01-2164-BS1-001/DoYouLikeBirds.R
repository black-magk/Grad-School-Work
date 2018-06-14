## --------------------- ##
## Birds R Problem       ##
## Solution              ##
## --------------------- ##

## Question 1. Reading in and loading data.
## install the data package and load the 'birds' data ##
rm(list=ls())
install.packages("OIdata")
library(OIdata)
data(birds)

## Question 2. Descriptives
## part a: produce a scatter plot ##
plot(height~speed, data=birds)  ## plot indicates a fairly strong, non-linear relationship

## part b: side-by-side boxplot; phase of flight.
boxplot(height~phase_of_flt, data=birds) ## all of the phases of flight that are "on the ground"
                                         ## have a height of zero, which makes sense.

## Question 3. Splitting, splicing and dicing.

## part a: focus on one and two engine planes

birds12 = birds[birds$num_eng %in% c(1,2), ]

## part b.
tapply(birds12$speed, birds12$num_eng, mean, na.rm=TRUE)
tapply(birds12$speed, birds12$num_eng, sd, na.rm=TRUE)

## part c.
length_sans_nas = function(x){
  return(sum(!is.na(x)))
}

tapply(birds12$speed, birds12$num_eng, length_sans_nas)

## part d. Confidence interval for difference in means
xbarz = tapply(birds12$speed, birds12$num_eng, mean, na.rm=TRUE)
sez   = tapply(birds12$speed, birds12$num_eng, sd, na.rm=TRUE)/sqrt(tapply(birds12$speed, birds12$num_eng, length_sans_nas))

diff_meanz = xbarz[2] - xbarz[1]       ## compute the difference in means
se_diff    = sqrt(sez[2]^2+sez[1]^2)   ## compute the standard error of the difference
lower = diff_meanz - 1.96*se_diff      ## lower part of the interval
upper = diff_meanz + 1.96*se_diff      ## upper part of the interval

c(lower, upper)

## Question 4. Histogram and Inference for height.

## part a.
hist(birds$height)  ## most likely at low elevations.

## part b. 
t.test(birds$height)   ## gives the 95 percent CI as part of the output

## or...

xbar = mean(birds$height, na.rm=TRUE)
se_xbar = sd(birds$height, na.rm=TRUE)/sqrt(length_sans_nas(birds$height))

lower = xbar - 1.96*se_xbar
upper = xbar +1.96*se_xbar
c(lower, upper)

## Above was using normal approximation, but using t-dist gives 'right' critical value.
lower = xbar - qt(0.975, df = length_sans_nas(birds$height)-1)*se_xbar
upper = xbar + qt(0.975, df = length_sans_nas(birds$height)-1)*se_xbar
c(lower, upper)
