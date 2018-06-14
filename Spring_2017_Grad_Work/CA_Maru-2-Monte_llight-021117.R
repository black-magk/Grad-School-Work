# MARU 2: Monte Carlo Simulation _ llight-021117
setwd("~/Desktop")

load("customers.rdata")
c <- customers	
cam <- c
sample.size <- length(cam)
ss <- sample.size

data = read.csv('mda.csv')
d <- data[3,]
dt <- as.data.frame(t(d))
# contact.cost                                          300
# response.rate                                       0.005
# workers.needed                                          1
# worker.labor.cost.per.hr                             1500
# instructors.needed                                      1
# instructor.labor.cost.per.hr                         4500
# price.per.hr                                         7500
# annual.hours                                           20
# retention.rate                                        0.6
# interest.rate											0.1

ac <- rep(d$contact.cost / d$response.rate, ss)
ad <- rep(d$interest.rate, ss)

# run a simulation that randomly samples values for the annual margin and retention rate from a probability distribution of your choosing 
# annual margin
hours <- rpois(ss, d$annual.hours)

# we chose to use the poisson distribution for annual hours with a lamba of 20 (the average). We felt that this distribution was appropriate because it is often used to approximate arrivals. It also adjusts predictions in accordance to the law of large numbers (this distribution becomes more normal as lamba increases).
revenue.per.hour <- d$price.per.hr
cost.per.hour <- (d$workers.needed * d$worker.labor.cost.per.hr) + (d$instructors.needed * d$instructor.labor.cost.per.hr) 
profit.per.hour <- revenue.per.hour - cost.per.hour
am <- hours * profit.per.hour

# retention rate
ar <- round( rbeta(ss,5,1), 4)
# we chose the beta distribution with an alpha = 5 and beta = 1. This was to simulate loyalty, accounting for how short-lasting customers are removed from the sample set as it ages.

# and computes CLV for a large number of “virtual customers.” 
clv <- round( am / (1 + ad - ar) - ac, 2)

#visualize the data in a histogram or other format and calculate summary statistics. Based on this past data, decide on an appropriate distribution to sample annual margins for your virtual customer. 
# histograms for comparison:
h.clv.est = hist(am)
h.clv.true = hist(cam)

par(mfrow = c(1,2))

h.clv.est$density = h.clv.est$counts / sum(h.clv.est$counts) * 100
plot(h.clv.est, freq=FALSE, main = 'Histogram of Estimated Customer Annual Margins', xlab = 'Estimated Annual Margins', xlim = c(0,60000), ylim = c(0,50))

h.clv.true$density = h.clv.true$counts / sum(h.clv.true$counts) * 100
plot(h.clv.true, freq=FALSE, main = 'Histogram of Actual Customer Annual Margins', xlim = c(0,60000), xlab = 'Actual Annual Margins',  ylim = c(0,50))







# Q1.	Value concentration. Value concentration refers to how much of your aggregate CLV is concentrated in a subset of customers. If value concentration is high, it means a small number of customers is responsible for a large percentage of your margins. Assess how value concentration depends on the distribution types and parameters you choose for the margin and retention rate distributions. Does value concentration depend on assumptions about how these variables are distributed? If so, how? 
percent.contribution <- clv / sum(clv)
pc <- round(percent.contribution, 4)
sum(pc) # ~100%
sum(percent.contribution) # 100%
pc <- sort(pc, decreasing = TRUE)
pc
length( pc[which(pc < 0) ] ) / ss # 7.4% of customers estimated to have negative clvs
length( pc[which(pc > 0.001) ] ) / ss # 64.4% of customers estimated to have negative clvs
hist(pc)

clv <- sort(clv, decreasing = TRUE)
clv
sum(clv)

q <- quantile(clv, probs = seq(0, 1, 0.05), na.rm = FALSE)
round( sum( clv[which(clv > q[17])] ) / sum( clv ), 4)
# 20% of customers generate 46.7% of profits
round( sum( clv[which(clv > q[19])] ) / sum( clv ), 4)
# 10% of customers generate 27.5% of profits
round( sum( clv[which(clv < q[11])] ) / sum( clv ), 4)
# the bottom 50% of customers only account for 16.4% of profits
round( length( clv[which(clv < 0)] ) / length( clv ), 4)
# 8.4% of customers incur losses; they never become profitable to serve.









# Q2.	Comparison to aggregate level analysis. In Maru Batting Center I (last week’s case) you calculated an average CLV for customers in the elite ballplayer segment. Does the average CLV based on the simulation agree with the number you calculated in part 1? Do assumptions about the distribution of margins appear to affect the extent of agreement or disagreement? How about assumptions about retention rates? Speculate on why the individual analysis does or does not agree with the aggregate level analysis from part 1. 









# Q3.	Finally, do you have any high-level recommendations about the kind of data that it would be useful to track or collect in order to improve the simulation and get more precise estimates of CLV?
# retention rate













































