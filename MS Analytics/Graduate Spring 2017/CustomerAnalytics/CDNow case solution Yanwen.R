
#set working directory

#load transaction data and retention data
CDNow<-read.csv("cdnow_students_transaction.csv")

#specify date format for dat column
CDNow$DATE<-as.Date(CDNow$DATE,format="%m/%d/%y")


#Split the data into calibration and validation samples at 9-30-97

date.cutoff<-as.Date(c("1997-09-30"))
CDNow.calibration<-CDNow[CDNow$DATE<=date.cutoff,]
CDNow.validation<-CDNow[CDNow$DATE>date.cutoff,]

#create new data frames that summarize by ID: monetary = average spend, frequency = number of separate ordders
#last.purchase = date of most recent purchase, num.cds = average number of CDs ordered

monetary<-aggregate(CDNow$DOLLARS, by = list(CDNow$ID), FUN = mean)
colnames(monetary)<-c("ID","monetary")

monetary.calibration<-aggregate(CDNow.calibration$DOLLARS, by = list(CDNow.calibration$ID), FUN = mean)
colnames(monetary.calibration)<-c("ID","monetary.cal")

monetary.validation<-aggregate(CDNow.validation$DOLLARS, by = list(CDNow.validation$ID), FUN = mean)
colnames(monetary.validation)<-c("ID","monetary.val")


frequency<-aggregate(CDNow$DOLLARS, by = list(CDNow$ID), FUN = length)
colnames(frequency)<-c("ID","frequency")

frequency.calibration<-aggregate(CDNow.calibration$DOLLARS, by = list(CDNow.calibration$ID), FUN = length)
colnames(frequency.calibration)<-c("ID","frequency.cal")

frequency.validation<-aggregate(CDNow.validation$DOLLARS, by = list(CDNow.validation$ID), FUN = length)
colnames(frequency.validation)<-c("ID","frequency.val")


last.purchase<-aggregate(CDNow$DATE, by = list(CDNow$ID), FUN = max)
colnames(last.purchase)<-c("ID","last.purchase")

last.purchase.calibration<-aggregate(CDNow.calibration$DATE, by = list(CDNow.calibration$ID), FUN = max)
colnames(last.purchase.calibration)<-c("ID","last.purchase.cal")

last.purchase.validation<-aggregate(CDNow.validation$DATE, by = list(CDNow.validation$ID), FUN = max)
colnames(last.purchase.validation)<-c("ID","last.purchase.val")


num.cds<-aggregate(CDNow$CDS, by = list(CDNow$ID), FUN = mean)
colnames(num.cds)<-c("ID","num.cds")

num.cds.calibration<-aggregate(CDNow.calibration$CDS, by = list(CDNow.calibration$ID), FUN = mean)
colnames(num.cds.calibration)<-c("ID","num.cds.cal")

num.cds.validation<-aggregate(CDNow.validation$CDS, by = list(CDNow.validation$ID), FUN = mean)
colnames(num.cds.validation)<-c("ID","num.cds.val")

# merge separate summary frames into one

CDNow.summary<-merge(monetary,frequency, by="ID")
CDNow.summary<-merge(CDNow.summary,last.purchase, by="ID")
CDNow.summary<-merge(CDNow.summary,num.cds, by="ID")

CDNow.summary.calibration<-merge(monetary.calibration,frequency.calibration, by="ID")
CDNow.summary.calibration<-merge(CDNow.summary.calibration,last.purchase.calibration, by="ID")
CDNow.summary.calibration<-merge(CDNow.summary.calibration,num.cds.calibration, by="ID")

CDNow.summary.validation<-merge(monetary.validation,frequency.validation, by="ID")
CDNow.summary.validation<-merge(CDNow.summary.validation,last.purchase.validation, by="ID")
CDNow.summary.validation<-merge(CDNow.summary.validation,num.cds.validation, by="ID")



#create recency variable by subtracting most recent purchase from the last purchase in the entire transaction dataset
CDNow.summary$recency<-as.numeric(max(CDNow.summary$last.purchase)-CDNow.summary$last.purchase)
CDNow.summary.calibration$recency<-as.numeric(max(CDNow.summary.calibration$last.purchase)-CDNow.summary.calibration$last.purchase)


#Merge calibration and validation samples into wide format where NAs in the validation indicate not being retained
CDNow.summary.all<-merge(CDNow.summary.calibration, CDNow.summary.validation, by = "ID", all.x = TRUE)
                                
#creates another column which returns a 1 if customer purchases in the validation period, 0 otherwise
CDNow.summary.all$retained<-as.numeric(!is.na(CDNow.summary.all$monetary.val))



#install.packages("dplyr")
library(dplyr)
#CDNow.summary.all$frequencyquantile <- ntile(CDNow.summary.all$frequency.cal,3)  


#quantile analysis
CDNow.summary.all$monetaryquantile <- ntile(CDNow.summary.all$monetary.cal,10)  
CDNow.summary.all$recencyquantile <- ntile(CDNow.summary.all$recency,10)  


monetary.quantile.summary<-aggregate(CDNow.summary.all$retained, by = list(CDNow.summary.all$monetaryquantile), FUN = mean)
colnames(monetary.quantile.summary)<-c("quantile","retention.percentage")
monetary.quantile.summary$n<-table(CDNow.summary.all$monetaryquantile)
plot(monetary.quantile.summary$quantile, monetary.quantile.summary$retention.percentage, type="l")


recency.quantile.summary<-aggregate(CDNow.summary.all$retained, by = list(CDNow.summary.all$recencyquantile), FUN = mean)
colnames(recency.quantile.summary)<-c("quantile","retention.percentage")
recency.quantile.summary$n<-table(CDNow.summary.all$recencyquantile)
plot(recency.quantile.summary$quantile, recency.quantile.summary$retention.percentage, type="l")



#cumulative lift charts
#monetary

overall.responserate<-mean(CDNow.summary.all$retained)

monetary.quantile.summary<-monetary.quantile.summary[order(-monetary.quantile.summary$quantile),]
monetary.quantile.summary$cum.n<-cumsum(monetary.quantile.summary$n)
monetary.quantile.summary$n.retained<-monetary.quantile.summary$n*monetary.quantile.summary$retention.percentage
monetary.quantile.summary$cum.n.retained<-cumsum(monetary.quantile.summary$n.retained)
monetary.quantile.summary$cumlift<-(monetary.quantile.summary$cum.n.retained/monetary.quantile.summary$cum.n)/overall.responserate

plot(-monetary.quantile.summary$quantile, monetary.quantile.summary$cumlift, type="l", )

plot(monetary.quantile.summary$quantile, monetary.quantile.summary$cumlift, type="l", lwd=5, xlab="monetary quantile", ylab="cumulative lift",ylim=c(1,3), xlim=rev(range(monetary.quantile.summary$quantile)))
abline(1,0, col="red", lwd=5)




#recency
recency.quantile.summary$cum.n<-cumsum(recency.quantile.summary$n)
recency.quantile.summary$n.retained<-recency.quantile.summary$n*recency.quantile.summary$retention.percentage
recency.quantile.summary$cum.n.retained<-cumsum(recency.quantile.summary$n.retained)
recency.quantile.summary$cumlift<-(recency.quantile.summary$cum.n.retained/recency.quantile.summary$cum.n)/overall.responserate

plot(recency.quantile.summary$quantile, recency.quantile.summary$cumlift, type="l",lwd=5, xlab="recency quantile", ylab="cumulative lift",ylim=c(1,3) )
abline(1,0, col="red", lwd=5)



#Linear regression
CDNow.linearfit <-lm(retained ~ recency + frequency.cal + monetary.cal, data=CDNow.summary.all)
summary(CDNow.linearfit)
CDNow.summary.all$linearfit <- predict(CDNow.linearfit, type="response")
hist(CDNow.summary.all$linearfit)

plot(CDNow.summary.all$monetary.cal,CDNow.summary.all$linearfit)
plot(CDNow.summary.all$frequency.cal,CDNow.summary.all$linearfit)
plot(CDNow.summary.all$recency,CDNow.summary.all$linearfit)


#logistic regression

CDNow.logisticfit <- glm(retained ~ recency + frequency.cal + monetary.cal, data=CDNow.summary.all, family =binomial(link = "logit"))
summary(CDNow.logisticfit)
CDNow.summary.all$logisticfit <- predict(CDNow.logisticfit,type="response")
hist(CDNow.summary.all$logisticfit)

plot(CDNow.summary.all$monetary.cal,CDNow.summary.all$logisticfit)
plot(CDNow.summary.all$frequency.cal,CDNow.summary.all$logisticfit)
plot(CDNow.summary.all$recency,CDNow.summary.all$logisticfit)


#lift chart for logistic regression
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




