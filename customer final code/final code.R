## Final Code

#install.packages("dplyr")
library(dplyr)

## loading csv file

file.choose()

transactions <- read.csv("/Users/landon/Desktop/customer final code/transactions2 .csv")



##linear Regression

wrong_allpredictors <- lm(Order_Quantity ~ Gender + Married + Income + Loyalty + 
                            Lag_Purchase + Lag_Order_Quantity, data=transactions)

summary(wrong_allpredictors)

anova(wrong_allpredictors)


## add predictions to csv file 

transactions$wrong_allpredictors <- predict(wrong_allpredictors, transactions, type="response")

## create a new variable

transactions$Purchase <- ifelse(transactions$Order_Quantity == 0, 0, 1) 

## logistic Regression


logit_demographics <- glm(Purchase ~ Gender + Married + Income + Loyalty, data=transactions, family=binomial(link="logit"))

## predictions for for logistic
predict <- predict(logit_demographics, type = 'response')


## classification matrix
table(transactions$Purchase, predict > 0.5)

## ROCR Curve

library(ROCR)
ROCRpred <- prediction(predict, transactions$Purchase)

ROCRperf <- performance(ROCRpred, 'tpr','fpr')

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


#specify date format for dat column
CDNow$DATE<-as.Date(CDNow$DATE,format="%m/%d/%y")


## Split Data into Two
## Spliting data by a specific column value 
date.cutoff<-as.Date(c("1997-09-30"))
CDNow.calibration<-CDNow[CDNow$DATE<=date.cutoff,]
CDNow.validation<-CDNow[CDNow$DATE>date.cutoff,]

##grouping

## group by ID and find mean of the Dollars
monetary<-aggregate(CDNow$DOLLARS, by = list(CDNow$ID), FUN = mean)

## assign column names
# assigning column names with a c bind

colnames(monetary)<-c("ID","monetary")


## frequency
##find frequency from aggregate function


frequency<-aggregate(CDNow$DOLLARS, by = list(CDNow$ID), FUN = length)
colnames(frequency)<-c("ID","frequency")

## finding a max value in a grouping

last.purchase<-aggregate(CDNow$DATE, by = list(CDNow$ID), FUN = max)
colnames(last.purchase)<-c("ID","last.purchase")


## merge data frames from grouped column i.e ID

# merge separate summary frames into one

CDNow.summary<-merge(monetary,frequency, by="ID")
CDNow.summary<-merge(CDNow.summary,last.purchase, by="ID")
CDNow.summary<-merge(CDNow.summary,num.cds, by="ID")

##RECENCY- most recent purchase - oldest purchase

CDNow.summary$recency<-as.numeric(max(CDNow.summary$last.purchase)-CDNow.summary$last.purchase)

#categorizing a variable based on another column

#creates another column which returns a 1 if customer purchases in the validation period, 0 otherwise
CDNow.summary.all$retained<-as.numeric(!is.na(CDNow.summary.all$monetary.val))


##merging a calibration and validation set into a major frame where Nas in the validation are considered not being retained

CDNow.summary.all<-merge(CDNow.summary.calibration, CDNow.summary.validation, by = "ID", all.x = TRUE)

#quantile analysis - breaking it into quantile analysis 
CDNow.summary.all$monetaryquantile <- ntile(CDNow.summary.all$monetary.cal,10)  

## grouping to find retention per quantile group

##giving rise to quantile percentage breakdown amongst quantiles

monetary.quantile.summary<-aggregate(CDNow.summary.all$retained, by = list(CDNow.summary.all$monetaryquantile), FUN = mean)
colnames(monetary.quantile.summary)<-c("quantile","retention.percentage")

## ploting retention across grouped retention rate percentages
plot(monetary.quantile.summary$quantile, monetary.quantile.summary$retention.percentage, type="l")


##ordering frame - ordering a frame from highest to lowest by quantile within the monetary.quantile.summary

monetary.quantile.summary<-monetary.quantile.summary[order(-monetary.quantile.summary$quantile),]


## Cum lift plot of retention

#cumulative lift charts allow you to look at the predictablility of responses based on testing a random sample of a group or set of individual responses and the accuracy of the model
#monetary


## Find overall response rete
overall.responserate<-mean(CDNow.summary.all$retained)


# create frame that orders by quantile
monetary.quantile.summary<-monetary.quantile.summary[order(-monetary.quantile.summary$quantile),]

# create column in that dataframe that brings in breakdown of how many are in each quantile
monetary.quantile.summary$n<-table(CDNow.summary.all$monetaryquantile)

monetary.quantile.summary$cum.n<-cumsum(monetary.quantile.summary$n)

# bring in values of the number retained in each quantile by multiplying above by percentage
monetary.quantile.summary$n.retained<-monetary.quantile.summary$n*monetary.quantile.summary$retention.percentage
monetary.quantile.summary$cum.n.retained<-cumsum(monetary.quantile.summary$n.retained)
monetary.quantile.summary$cumlift<-(monetary.quantile.summary$cum.n.retained/monetary.quantile.summary$cum.n)/overall.responserate

plot(-monetary.quantile.summary$quantile, monetary.quantile.summary$cumlift, type="l", )

plot(monetary.quantile.summary$quantile, monetary.quantile.summary$cumlift, type="l", lwd=5, xlab="monetary quantile", ylab="cumulative lift",ylim=c(1,3), xlim=rev(range(monetary.quantile.summary$quantile)))
abline(1,0, col="red", lwd=5)



### cum lift chart for logistic 

CDNow.summary.all$logisticquantile <- ntile(CDNow.summary.all$logisticfit,10)  

logistic.quantile.summary<-aggregate(CDNow.summary.all$retained, by = list(CDNow.summary.all$logisticquantile), FUN = mean)
colnames(logistic.quantile.summary)<-c("quantile","retention.percentage")
logistic.quantile.summary$n<-table(CDNow.summary.all$logisticquantile)
plot(logistic.quantile.summary$quantile, logistic.quantile.summary$retention.percentage, type="l")

logistic.quantile.summary<-logistic.quantile.summary[order(-logistic.quantile.summary$quantile),]

logistic.quantile.summary$cum.n<-cumsum(logistic.quantile.summary$n)
logistic.quantile.summary$n.retained<-logistic.quantile.summary$n*logistic.quantile.summary$retention.percentage
logistic.quantile.summary$cum.n.retained<-cumsum(logistic.quantile.summary$n.retained)
logistic.quantile.summary$cumlift<-(logistic.quantile.summary$cum.n.retained/logistic.quantile.summary$cum.n)/overall.responserate

plot(logistic.quantile.summary$quantile, logistic.quantile.summary$cumlift, type="l", lwd=5, xlab="quantile", ylab="cumulative lift",ylim=c(1,3), xlim=rev(range(logistic.quantile.summary$quantile)))
abline(1,0, col="red", lwd=5)




