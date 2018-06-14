
#Solution to Maru Part II, Monte Carlo Simulation of Individual-Level CLV
 rm(list=ls())

#read in maru data
maru.data<-read.csv("maru_data_assignment2.csv")


#CALCULATE ANNUAL MARGIN
maru.data$total.cost.per.hr<-maru.data$instructor.labor.cost.per.hr* maru.data$instructors.needed + maru.data$worker.labor.cost.per.hr* maru.data$workers.needed
maru.data$margin.hr<-maru.data$price.per.hr-maru.data$total.cost.per.hr
maru.data$annual.margin<-maru.data$margin.hr*maru.data$annual.hours

# CALCULATE ACQUISITION COST
maru.data$acquisition.cost<-maru.data$contact.cost/maru.data$response.rate

#Create a new column with CLV assuming numbers from case
maru.data$clv <- (maru.data$annual.margin* ((1+maru.data$interest.rate) / (1 + maru.data$interest.rate - maru.data$retention.rate))) - maru.data$acquisition.cost


elite.ballplayers<-maru.data[4 ,]
elite.ballplayers.subset<-subset(elite.ballplayers, select = c("acquisition.cost","annual.margin","retention.rate"))
elite.ballplayers.subset<-data.matrix(elite.ballplayers.subset)


#CLV for elite ballplayers calcuated using aggregate values
elite.ballplayers.clvaggregate<-maru.data$clv[4]




#simulate CLV for a bunch of elite ballplayers

# Set Seed for Random Number Generation
set.seed(123456)


#load in customer margin data and analyze it
load("customers.rdata")
hist(customers)
margin.mean<-mean(customers)
margin.sd<-sd(customers)


d<-.1
ac<-elite.ballplayers.subset[1]
m<-elite.ballplayers.subset[2]
r<-elite.ballplayers.subset[3]

num.samples=10000

d.vec<-rep(d, num.samples)
ac.vec<-rep(ac, num.samples)
m.vec<-rnorm(num.samples,margin.mean,margin.sd)

#Use Beta distribution to determine retention rate value


#Beta with slight skew 
a<-5
b<-a*(1-r)/r
r.vec<-rbeta(num.samples,a,b)
hist(r.vec, main= "Retention Rate")


 
#Beta with "Bathtub Shape"
a<-.5
b<-a*(1-r)/r
r.vec<-rbeta(num.samples,a,b)
hist(r.vec, main= "Retention Rate")

 
 
#Beta with sharp peak
a<-100
b<-a*(1-r)/r
r.vec<-rbeta(num.samples,a,b)
hist(r.vec, main= "Retention Rate")



#CLV = (M * ( (1+d) / (1 + d - r))) - AC
clv.vec=(m.vec* ((1+d.vec)/(1+d.vec-r.vec)))-ac.vec

mean(clv.vec)
median(clv.vec)
hist(clv.vec, main = "CLV")
 

 
#Create "Whale Plot" to depict value concentration
 
whale.data <- as.data.frame(clv.vec[order(-clv.vec)]) #sort customers decreasing order of CLV
colnames(whale.data)<-"clv.ordered"
 
whale.data$percent <- (whale.data$clv.ordered/sum(whale.data$clv.ordered))
whale.data$cumpercent<-100*cumsum(whale.data$percent)
 
whale.data$customernumber<-seq(1,num.samples)
whale.data$percentofcustomers<-100*whale.data$customernumber/num.samples
 
plot(whale.data$percentofcustomers,whale.data$cumpercent, type="l",
        ylim=c(0,150),main="Whale Curve for Maru Batting"
        ,xlab = "Percent of Customers", ylab = "Percent of Aggregate CLV")
abline(100,0,col=2)

plot(whale.data$percentofcustomers,whale.data$clv.ordered, type="l",
      main="Whale Curve for Maru Batting",
     xlab = "Percent of Customers", ylab = "Percent of Aggregate CLV")
 
 


