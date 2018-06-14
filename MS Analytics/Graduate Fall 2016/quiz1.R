data(state)

library(ggplot2)

statedata = cbind(data.frame(state.x77), state.abb,state.area,state.center, state.division, state.name, state.region)

str(statedata)

statedata$Population
#1
library(ggplot2)
plot1<-ggplot(data=statedata, aes(x=x, y=y,color= state.name))+geom_point()+xlab("longitude")+ylab("latitude")+theme_bw()
plot1

#2
x<-tapply(statedata$HS.Grad,list(statedata$state.region),mean)
x


#3

y<-tapply(statedata$Murder,list(statedata$state.region),median)
boxplot(statedata$Murder ~ statedata$state.region)
#4

NE<-subset(statedata,statedata $state.region=="Northeast")
NE
head( NE[ order(NE$Murder, decreasing=TRUE), ], 10 )

#New York in the Northeast Region

#5
Mod.1<-lm(Life.Exp~Population+Income+Illiteracy+Murder+ HS.Grad+Frost+Area,data=statedata)


summary(Mod.1)

The coefficient for incom is -2.180e-05
#6
plot(statedata$Income, statedata$Life.Exp)

#As income increases...life expectancy seems to increase
#7
#The model contains more factors (predictors) for life expectancy. The plot simply plotted two variables. The model labeled mod.1 contained 7 predictors.

#8
#we can eliminate variables and still have a large R-squared values based on p value. View the new model below:
Mod.2<-lm(Life.Exp~Population+Murder+ HS.Grad+Frost,data=statedata)

summary(Mod.2)

#This model has an Rsquared value of .736 still. The previous model had an Rsquared of .7362.

#9

predictions<-sort(predict(Mod.2))
predictions

statedata$state.name[which.max(statedata$Life.Exp)]

statedata$state.name[which.min(statedata$Life.Exp)]


#10
sort(Mod.2$residuals)

#Max:Maine
#Min:Hawaii


