
library(visreg)
library(cvTools)

#IMPORT DATASET
pilgrim <- read.csv("pilgrim A2 data part1.csv")


#Part 1: Interaction Modeling

#Model with no interactions, continuous treatment of age and income

maineffectsmodel <- lm(Profit99 ~ Online99 + Tenure99 + Inc99 + Age99, data=pilgrim)
summary(maineffectsmodel)


#Visualize fit as a function of demographics
visreg(maineffectsmodel,"Inc99")
visreg(maineffectsmodel,"Tenure99")
visreg(maineffectsmodel,"Age99")

#Models with interactions between demographics and online/offline. Income is the only one that significantly interacts.
#including the income interaction appears to improve the model a little bit based on fit statistics.

incomeinteractionmodel <- lm(Profit99 ~ Online99 + Tenure99 + Inc99 + Age99 + Inc99:Online99, data=pilgrim)
summary(incomeinteractionmodel)
visreg(incomeinteractionmodel,"Inc99", by="Online99", overlay="True")

tenureinteractionmodel <- lm(Profit99 ~ Online99 + Tenure99 + Inc99 + Age99 + Tenure99:Online99, data=pilgrim)
summary(tenureinteractionmodel)
visreg(tenureinteractionmodel,"Tenure99", by="Online99", overlay="True")

ageinteractionmodel <- lm(Profit99 ~ Online99 + Tenure99 + Inc99 + Age99 + Age99:Online99, data=pilgrim)
summary(ageinteractionmodel)
visreg(ageinteractionmodel,"Age99", by="Online99", overlay="True")



#PART 2: ANALYSIS OF EXPERIMENT

#Read in data

pilgrim.experiment <- read.csv("pilgrim A2 data part 2 experiment.csv")

#create a new variable for change in profit from 1999 to 2000

pilgrim.experiment$profitchange<-pilgrim.experiment$Profit00-pilgrim.experiment$Profit99

#Split the data into experimental and control groups to simplify analysis. Visualize distribution of profitchange in each group. 
#Use matched t-test to see if profit significantly changed from 99 to 00 (Could also use single-sample test on profitchange)

pilgrim.experiment.exp<-pilgrim.experiment[1:300,]
hist(pilgrim.experiment.exp$profitchange)
t.test(pilgrim.experiment.exp$Profit00,pilgrim.experiment.exp$Profit99,paired=TRUE)

pilgrim.experiment.control<-pilgrim.experiment[301:600,]
hist(pilgrim.experiment.control$profitchange)
t.test(pilgrim.experiment.control$Profit00,pilgrim.experiment.control$Profit99,paired=TRUE)

#Run t-tests to test for randomization. No significant differences across groups on profit or demographics in 99. 
#Randomization appears good. 

t.test(pilgrim.experiment.exp$Profit99, pilgrim.experiment.control$Profit99)
t.test(pilgrim.experiment.exp$Age99, pilgrim.experiment.control$Age99)
t.test(pilgrim.experiment.exp$Profit99, pilgrim.experiment.control$Profit99)

t.test(pilgrim.experiment.exp$Inc99, pilgrim.experiment.control$Inc99)
t.test(pilgrim.experiment.exp$Tenure99, pilgrim.experiment.control$Tenure99)


#Independeint t-test to see if experimental group has larger profit change than control group. It does. 
t.test(pilgrim.experiment.exp$profitchange, pilgrim.experiment.control$profitchange)

#box plot to show the difference. Note, this is a terrible graphic. How can you make a better one?
boxplot(pilgrim.experiment$profitchange~pilgrim.experiment$condition)


#MAKING PREDICTIONS WITH REGRESSION

#Read in data on targets

pilgrim.targets <- read.csv("pilgrim A2 data part 2 targets.csv")


#try a model with demographics plus profit99


model1 <- lm(Profit00 ~ Profit99 + Tenure99 + Inc99 + Age99, data=pilgrim.experiment.exp)
summary(model1)

#use cross validation to test fit

set.seed(123)
test <- cvFit(model1,y=pilgrim.experiment.exp$Profit00, data=pilgrim.experiment.exp, K=10, R=10)
summary(test)

#Visualize fit of this model
visreg(model1,"Profit99")
visreg(model1,"Tenure99")
visreg(model1,"Inc99")
visreg(model1,"Age99")



#try a model with just profit99

model2 <- lm(Profit00 ~ Profit99, data=pilgrim.experiment.exp)
summary(model2)

test <- cvFit(model2,y=pilgrim.experiment.exp$Profit00, data=pilgrim.experiment.exp, K=10, R=10)
summary(test)

visreg(model2,"Profit99")



#try a crazy model

model3 <- lm(Profit00 ~ Profit99 + Tenure99 + Inc99 + Age99 + I(Inc99^2) + I(Inc99^3) + I(Inc99^4) + I(Inc99^5), data=pilgrim.experiment.exp)
summary(model3)

visreg(model3,"Inc99")

test <- cvFit(model3,y=pilgrim.experiment.exp$Profit00, data=pilgrim.experiment.exp, K=10, R=10)
summary(test)


#Pick the simplest model and make predictions for the targets

pilgrim.targets$bestmodel <- predict(model2, pilgrim.targets, type="response")

#calculate expected profit change
pilgrim.targets$expectedprofitchange<-pilgrim.targets$bestmodel-pilgrim.targets$Profit99

#Average benefit is $38
summary(pilgrim.targets$expectedprofitchange)




#VALIDATING THE MODELS

#Read in the data for the targets including the profit in 2000
pilgrim.validation <- read.csv("pilgrim A2 data part 2 validation.csv")


#Test how well each of my three models fits the actual data by calculating root mean square error. As expected the simple model performs the best. 
pilgrim.validation$model1 <- predict(model1, pilgrim.validation, type="response")
pilgrim.validation$model1se<-(pilgrim.validation$Profit00-pilgrim.validation$model1)^2
sqrt(mean(pilgrim.validation$model1se, na.rm=TRUE))


pilgrim.validation$model2 <- predict(model2, pilgrim.validation, type="response")
pilgrim.validation$model2se<-(pilgrim.validation$Profit00-pilgrim.validation$model2)^2
sqrt(mean(pilgrim.validation$model2se, na.rm=TRUE))


pilgrim.validation$model3 <- predict(model3, pilgrim.validation, type="response")
pilgrim.validation$model3se<-(pilgrim.validation$Profit00-pilgrim.validation$model3)^2
sqrt(mean(pilgrim.validation$model3se, na.rm=TRUE))









