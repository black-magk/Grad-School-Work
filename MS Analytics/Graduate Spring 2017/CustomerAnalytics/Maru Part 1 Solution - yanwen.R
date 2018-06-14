
# Assignment solultion to Maru Batting prepared by Yanwen WANG


# SET WORKING DIRECTORY
# setwd("E:/Dropbox/Teaching/CustomerAnalytics_2017/Lecture 3 - Customer Lifetime Value")
# READ DATA
maru.data<-read.csv("maru_data_assignment2.csv")


# CALCULATE ANNUAL MARGIN
maru.data$total.cost.per.hr<-maru.data$instructor.labor.cost.per.hr* maru.data$instructors.needed + maru.data$worker.labor.cost.per.hr* maru.data$workers.needed
maru.data$margin.hr<-maru.data$price.per.hr-maru.data$total.cost.per.hr
maru.data$annual.margin<-maru.data$margin.hr*maru.data$annual.hours



# answer to question 1 on page 5
# CALCULATE ACQUISITION COST
maru.data$acquisition.cost<-maru.data$contact.cost/maru.data$response.rate
maru.data$acquisition.cost




# answer to question 2
# BREAKEVEN WITHOUT DISCOUNTING
maxperiod = 6
period = seq(1,maxperiod)

cf = matrix(0,nrow=5,ncol=maxperiod)
npv = matrix(0,nrow=5,ncol=maxperiod)

for (t in 1:maxperiod){
  cf[,t] = maru.data$annual.margin*maru.data$retention.rate^(t-1)
  npv[,t] = rowSums(as.matrix(cf[,1:t])) - maru.data$acquisition.cost
}

# print out cash flow matrix 
cf
# print out npv to see since which period the npv>=0
npv
# you can try the following instead of eyeballing
#apply(npv,1, function(x) which.max(x>=0))




# PROBLEM 3: COMPUTE CLV (ASSUMING INFINITE TIME HORIZON)
#CLV = (M * ( (1+D) / (1 + D - R))) - AC Using formula #1
#Create a new column with CLV assuming numbers from case
maru.data$clv <- (maru.data$annual.margin* ((1+maru.data$interest.rate) / (1 + maru.data$interest.rate - maru.data$retention.rate))) - maru.data$acquisition.cost

# if you use formula 3...
# maru.data$clv <- (maru.data$annual.margin / (1 + maru.data$interest.rate - maru.data$retention.rate)) - maru.data$acquisition.cost



# PROBLEM 5: CHIYODA WARD
clv.littleleaguers.now<-maru.data[maru.data$X=="little leaguers",]$clv
clv.littleleaguers.chiyoda<-5000*(1+.1)/(1+.1-.65)-(600/.08)
clv.littleleaguers.now
clv.littleleaguers.chiyoda



# PROBLEM 6: ELITE BALLPLAYERS DISCOUNT

clv.eliteballplayers.now<-maru.data$clv[which(maru.data$X=="elite ballplayers (party)")]
clv.eliteballplayers.discount<-((7000-6000)*20)*(1+.1)/(1+.1-.75)-(50000)+500*20


# PROBLEM 7: ELITE BALLPLAYERS BAT
clv.eliteballplayers.bat<-((7500-6000)*20)*(1+.1)/(1+.1-.6)-(12500/.29+10000)




# SENSITIVITY ANALYSIS

#creates scenario values
ac <- seq(from = 40000, to = 60000, by = 5000)
am <- seq(from = 20000, to = 40000, by = 5000)
rr <- seq(from = 0.30, to = 0.90, by = 0.12)

#Matrix of all pairwise comparisons

values <- expand.grid(ac=ac,am=am,rr=rr)

#adds constant interest rate column
values$ir <- 0.10

#computes CLV for all scenarios
values$clv <- values$am * ( (1+values$ir) / (1 + values$ir - values$rr)) - values$ac

write.csv(values,"sensitivity.csv",row.names=F)


#some descriptives
negativeclvpercent = sum(values$clv < 0)/ length(values$clv)
worsethanlittleleaguerspercent=sum(values$clv < maru.data$clv[1])/length(values$clv)


#visualization using scatter plot
scatter.smooth(x=values$ac, y=values$clv)
scatter.smooth(x=values$am, y=values$clv)
scatter.smooth(x=values$rr, y=values$clv)

