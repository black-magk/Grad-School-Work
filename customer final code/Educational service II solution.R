#Yanwen Wang
#University of Colorado Leeds School of Business
#Customer Analytics
#Updated April 2017

rm(list = ls())
file.choose()

education<-read.csv("/Users/landon/Desktop/customer final code/educational service long.csv")

head(education)
#SURVIVAL AND HAZARD FUNCTIONS BY INITIAL CONTRACT LENGTH

#Initial contract length = 1
education.startlen1 <- subset(education, startlen==1) 

education2.startlen1 <- education.startlen1[which(education.startlen1$cancelnow==0),]

head(startlen1)

startlen1<-aggregate(cancelnow ~ time, data = education2.startlen1, FUN = length)

startlen1$PercAlive <- startlen1$cancelnow/length(unique(education.startlen1$custid))
startlen1$Hazard <- seq(1,11,1)

for (i in 2:11){
  startlen1$Hazard[i] <- 1-(startlen1$PercAlive[i]/startlen1$PercAlive[i-1])
}

startlen1$Hazard[1]<-1-(startlen1$cancelnow[1]/length(unique(education.startlen1$custid)))

#Initial contract length = 6
education.startlen6 <- subset(education, startlen==6) 
education2.startlen6 <- education.startlen6[which(education.startlen6$cancelnow==0),]
startlen6<-aggregate(cancelnow ~ time, data = education2.startlen6, FUN = length)
startlen6$PercAlive <- startlen6$cancelnow/length(unique(education.startlen6$custid))
startlen6$Hazard <- seq(1,11,1)

for (i in 2:11){
  startlen6$Hazard[i] <- 1-(startlen6$PercAlive[i]/startlen6$PercAlive[i-1])
}

startlen6$Hazard[1]<-1-(startlen6$cancelnow[1]/length(unique(education.startlen6$custid)))

#Initial contract length = 12
education.startlen12 <- subset(education, startlen==12) 
education2.startlen12 <- education.startlen12[which(education.startlen12$cancelnow==0),]
startlen12<-aggregate(cancelnow ~ time, data = education2.startlen12, FUN = length)
startlen12$PercAlive <- startlen12$cancelnow/length(unique(education.startlen12$custid))
startlen12$Hazard <- seq(1,11,1)

for (i in 2:11){
  startlen12$Hazard[i] <- 1-(startlen12$PercAlive[i]/startlen12$PercAlive[i-1])
}

startlen12$Hazard[1]<-1-(startlen12$cancelnow[1]/length(unique(education.startlen12$custid)))

#Survival Functions
plot(1,type='n',xlim=c(0,12),ylim=c(0,1),xlab='TIME', ylab='PERCENT SURVIVORS')
lines(startlen1$time[1:11], startlen1$PercAlive[1:11], type="o", col="blue", lwd=1)
lines(startlen6$time[1:11], startlen6$PercAlive[1:11], type="o", col="red", lwd=1)
lines(startlen12$time[1:11], startlen12$PercAlive[1:11], type="o", col="orange", lwd=1)

#Hazard Functions
plot(1,type='n',xlim=c(0,12),ylim=c(0,0.4),xlab='TIME', ylab='DEFECTION LIKELIHOOD')
lines(startlen1$time[1:11], startlen1$Hazard[1:11], type="o", col="blue", lwd=1)
lines(startlen6$time[1:11], startlen6$Hazard[1:11], type="o", col="red", lwd=1)
lines(startlen12$time[1:11], startlen12$Hazard[1:11], type="o", col="orange", lwd=1)




#Logistic regression model 
education$startlen.c<-as.factor(education$startlen)


##creating a dummy based on two factors
education$dummy.startlen6.time6 <- ifelse(education$time==6 & education$startlen==6,1,0)

logit.stable <- glm(cancelnow ~  time + startlen.c + time:startlen.c + dummy.startlen6.time6,  data=education, family=binomial(link = logit))
summary(logit.stable)

time <- seq(1,11,1)
startlen.c <- c("1","6","12")
dummy.startlen6.time6 <- 0

visuals.stable<-expand.grid(time,startlen.c,dummy.startlen6.time6)

colnames(visuals.stable)<-c("time","startlen.c","dummy.startlen6.time6")
visuals.stable$dummy.startlen6.time6 <- ifelse(visuals.stable$time==6 & visuals.stable$startlen==6,1,0)
visuals.stable$hazard.predictions <- predict(logit.stable, visuals.stable, type="response")


# alternatively you can do the prediction at each customer-time level and average by time period
#education$predict <- predict(logit.stable, education, type="response")
#education <- as.data.table(education)
#education_agg <- education[, list(predictprob=mean(predict)), by=list(time,startlen.c)]


plot(1,type='n',xlim=c(0,12),ylim=c(0,0.4),xlab='TIME', ylab='DEFECTION LIKELIHOOD')
lines(startlen1$time[1:11], startlen1$Hazard[1:11], type="o", col="blue", lwd=1)
lines(startlen6$time[1:11], startlen6$Hazard[1:11], type="o", col="red", lwd=1)
lines(startlen12$time[1:11], startlen12$Hazard[1:11], type="o", col="orange", lwd=1)

lines(visuals.stable$time[1:11], visuals.stable$hazard.predictions[1:11], type="o", col="blue", lwd=4)
lines(visuals.stable$time[12:22], visuals.stable$hazard.predictions[12:22], type="o", col="red", lwd=4)
lines(visuals.stable$time[23:33], visuals.stable$hazard.predictions[23:33], type="o", col="orange", lwd=4)

# plot out the predicted hazard rate using the alternative method in line 84
#lines(education_agg$time[1:11], education_agg$predictprob[1:11], type="o", col="blue", lwd=3)
#lines(education_agg$time[12:22], education_agg$predictprob[12:22], type="o", col="red", lwd=3)
#lines(education_agg$time[23:33], education_agg$predictprob[23:33], type="o", col="orange", lwd=3)




#TRIGGER EVENTS
head(education.startlen6)
## Analysis for 6 months initial contract length
education.startlen6$lagnotest <- ifelse(education.startlen6$lagtest==0,1,0)
education.startlen6$contractup <- ifelse(education.startlen6$payleft==0,1,0)

education.startlen6$testsassigned<-4
education.startlen6$cumtestsassigned<-ave(education.startlen6$testsassigned,education.startlen6$custid,FUN=cumsum)
education.startlen6$cumlagtest<-ave(education.startlen6$lagtest,education.startlen6$custid,FUN=cumsum)
education.startlen6$propcumlagtest<- education.startlen6$cumlagtest/education.startlen6$cumtestsassigned

logit.triggers <- glm(cancelnow ~  time + propcumlagtest + lagnotest + contractup,  data=education.startlen6, family=binomial(link = logit))
summary(logit.triggers)

time <- seq(1,11,1)
lagnotest <- c(0,1)
contractup <- c(0,1)
propcumlagtest <- c(0.70,1)

visuals.triggers<-expand.grid(time,lagnotest,contractup,propcumlagtest)

colnames(visuals.triggers)<-c("time","lagnotest","contractup","propcumlagtest")
visuals.triggers$hazard.predictions <- predict(logit.triggers, visuals.triggers, type="response")

#contract up & perfect effort vs. contract up & imperfect effort
plot(1,type='n',xlim=c(0,12),ylim=c(0,0.1),xlab='TIME', ylab='DEFECTION LIKELIHOOD')
lines(visuals.triggers$time[23:33], visuals.triggers$hazard.predictions[23:33], type="o", col="blue", lwd=1)
lines(visuals.triggers$time[67:77], visuals.triggers$hazard.predictions[67:77], type="o", col="red", lwd=1)

