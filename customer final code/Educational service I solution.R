#Yanwen Wang
#University of Colorado Leeds School of Business
#Customer Analytics
#Updated March 2017

rm(list = ls())
file.choose()
education<-read.csv("/Users/landon/Desktop/customer final code/educational service long.csv")

education
#Descriptive analysis
head(education2)<-education[which(education$cancelnow==0),]
functions<-aggregate(cancelnow ~ time, data = education2, FUN = length)
functions$PercAlive <- functions$cancelnow/length(unique(education$custid))

functions$Hazard[1]<-1-(functions$cancelnow[1]/length(unique(education$custid)))
for (i in 2:11){
  functions$Hazard[i] <- 1-(functions$PercAlive[i]/functions$PercAlive[i-1])
}

plot(1,type='n',xlim=c(0,12),ylim=c(0,1),xlab='TIME', ylab='PERCENT SURVIVORS')
lines(functions$time[1:11], functions$PercAlive[1:11], type="o", col="blue", lwd=1)

plot(1,type='n',xlim=c(0,12),ylim=c(0,0.4),xlab='TIME', ylab='DEFECTION LIKELIHOOD')
lines(functions$time[1:11], functions$Hazard[1:11], type="o", col="blue", lwd=1)

#Logistic regression analysis
logit1 <- glm(cancelnow ~ time,  data=education, family=binomial(link = logit))
summary(logit1)
functions$HazardLogit1 <- predict(logit1, functions, type="response")

plot(1,type='n',xlim=c(0,12),ylim=c(0,0.4),xlab='TIME', ylab='DEFECTION LIKELIHOOD')
lines(functions$time[1:11], functions$Hazard[1:11], type="o", col="blue", lwd=1)
lines(functions$time[1:11], functions$HazardLogit1[1:11], type="o", col="red", lwd=1)

education$time6 <- ifelse(education$time==6, 1,0)
logit2 <- glm(cancelnow ~ time + time6,  data=education, family=binomial(link = logit))
summary(logit2)
functions$time6 <- ifelse(functions$time==6, 1,0)
functions$HazardLogit2 <- predict(logit2, functions, type="response")


# alternatively you can do the prediction at each customer-time level and average by time period
education$predict <- predict(logit2, education, type="response")
education <- as.data.table(education)
education_agg <- education[, list(predictprob=mean(predict)), by=time]

plot(1,type='n',xlim=c(0,12),ylim=c(0,0.4),xlab='TIME', ylab='DEFECTION LIKELIHOOD')
lines(functions$time[1:11], functions$Hazard[1:11], type="o", col="blue", lwd=1)
lines(functions$time[1:11], functions$HazardLogit1[1:11], type="o", col="red", lwd=1)
lines(functions$time[1:11], functions$HazardLogit2[1:11], type="o", col="orange", lwd=1)
#plot out the predicted hazard rate using the alternative method in line 43
#lines(education_agg$time[1:11], education_agg$predictprob[1:11], type="o", col="green", lwd=1)


functions$PercAliveLogit1[1]<- 1 - functions$HazardLogit1[1]
for (i in 2:11){
  functions$PercAliveLogit1[i] <- functions$PercAliveLogit1[i-1] - (functions$HazardLogit1[i]*functions$PercAliveLogit1[i-1])
}

functions$PercAliveLogit2[1]<- 1 - functions$HazardLogit2[1]
  for (i in 2:11){
  functions$PercAliveLogit2[i] <- functions$PercAliveLogit2[i-1] - (functions$HazardLogit2[i]*functions$PercAliveLogit2[i-1])
}

plot(1,type='n',xlim=c(0,12),ylim=c(0,1),xlab='TIME', ylab='PERCENT SURVIVORS')
lines(functions$time[1:11], functions$PercAlive[1:11], type="o", col="blue", lwd=1)
lines(functions$time[1:11], functions$PercAliveLogit1[1:11], type="o", col="red", lwd=1)
lines(functions$time[1:11], functions$PercAliveLogit2[1:11], type="o", col="orange", lwd=1)



