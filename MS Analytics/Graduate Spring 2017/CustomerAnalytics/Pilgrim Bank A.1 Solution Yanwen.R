
#SET WORKING DIRECTORY
#IMPORT DATASET
pilgrim <- read.csv("pilgrimA1data.csv")

#CREATE PROFITABILITY SKEW (= WHALE PLOT)

pilgrim.ordered <- pilgrim[order(-pilgrim$Profit99),] #sort customers in increasing order of Profit99
pilgrim.ordered$CumProfit99 <- cumsum((pilgrim.ordered$Profit99)/sum(pilgrim.ordered$Profit99)) #create column with cumulative percent of total Profit99
pilgrim.ordered$customernumber <- seq(1:dim(pilgrim.ordered)[1])
pilgrim.ordered$cumpercentofcustomers <-pilgrim.ordered$customernumber/max(pilgrim.ordered$customernumber)

#whale plot
plot(x=pilgrim.ordered$cumpercentofcustomers, y=pilgrim.ordered$CumProfit99, type="l",
     xlab="Percent of Customers",
     ylab="Percent of Profit")
abline(h=1, col="blue")

#determine percentage of best customers needed to have same total profit
#hundredprofit.customernumber<-min(pilgrim.ordered[pilgrim.ordered$CumProfit99>1,9])-1 #find customer number of first row where cum profit goes above 1 and subtract one
hundredprofit.customernumber<-which.max(pilgrim.ordered$CumProfit99>1)-1 #find customer number of first row where cum profit goes above 1 and subtract one
hundredprofit.percent <- pilgrim.ordered[hundredprofit.customernumber,10] #percent of customers responsible for 100% of profit



#percentage of best customers needed to maximize profit
maxprofit.percent<-pilgrim.ordered[which.max(pilgrim.ordered$CumProfit99),10]


#Various ways to visualize heterogeneity in profit
#install.packages("ggplot2")
library(ggplot2)

ggplot(pilgrim, aes(x = Profit99)) + geom_histogram(binwidth = 50, fill = "blue") #Histogram using ggplot2 package, bin width of 50, and blue fill
ggplot(pilgrim, aes(x = Profit99)) + geom_density(fill = "grey50") #Density plot instead of histogram, and grey fill
ggplot(pilgrim, aes(x=Tenure99, y=Profit99)) + geom_point() + geom_smooth() #smoothed fit curve
ggplot(pilgrim, aes(x=Tenure99, y=Profit99)) + geom_point() + geom_smooth() #smoothed fit curve
ggplot(pilgrim, aes(x=Tenure99, y=Profit99)) + geom_point() + geom_smooth(method="lm") #linear regression line
ggplot(pilgrim, aes(x=as.factor(Online99), y=Profit99)) + geom_boxplot() #Boxplot split out by online/offline 
ggplot(pilgrim, aes(x=as.factor(Age99), y=Profit99)) + geom_boxplot() #Boxplot split out by age groups
ggplot(pilgrim, aes(x=as.factor(Age99), y=Profit99)) + geom_violin() #Violin plot split out by age groups
ggplot(pilgrim, aes(x=as.factor(Inc99), y=Profit99)) + geom_boxplot() #Boxplot split out by income groups
ggplot(pilgrim, aes(x=as.factor(District99), y=Profit99)) + geom_boxplot() #Boxplot split out by district groups

#INTERCEPT-ONLY MODEL
interceptOnly <-lm(Profit99 ~ 1 , data=pilgrim) #Estimate intercept-only regression model
summary(interceptOnly) #Analyze regression output
confint(interceptOnly, level=0.95) #Determine 95% confidence interval for parameters of regression model

#ANALYZE EFFECT OF ONLINE VS. OFFLINE NOT CONTROLLING FOR CUSTOMER BACKGROUND CHARACTERISTICS
noBackground <-lm(Profit99 ~ Online99 , data=pilgrim)
summary(noBackground) #No significant effect of online vs. offline?

#ANALYZE EFFECT OF ONLINE VS. OFFLINE CONTROLLING FOR BACKGROUND CHARACTERISTICS, AND ELIMINATING OBSERVATIONS WITH MISSING VALUES

#age and income as categorical variables
backgroundNoMissingCateg <-lm(Profit99 ~ Online99 + Tenure99 + as.factor(Age99) + as.factor(Inc99) + as.factor(District99) , data=pilgrim)
summary(backgroundNoMissingCateg)

ggplot(pilgrim, aes(y = Profit99, x = as.factor(Online99))) + geom_boxplot() #Boxplot split out by groups
ggplot(pilgrim, aes(y = Profit99, x = as.factor(Age99))) + geom_boxplot() #Boxplot split out by groups
ggplot(pilgrim, aes(y = Profit99, x = as.factor(Inc99))) + geom_boxplot() #Boxplot split out by groups
ggplot(pilgrim, aes(y = Profit99, x = as.factor(District99))) + geom_boxplot() #Boxplot split out by groups

#age and income as continuous variables
backgroundNoMissingCont <-lm(Profit99 ~ Online99 + Tenure99 + Age99 + Inc99 + as.factor(District99) , data=pilgrim)
summary(backgroundNoMissingCont)



#age and income as continuous variables with quadratic effects

backgroundNoMissingContwithquad <-lm(Profit99 ~ Online99 + Tenure99 + Age99 + Inc99 + as.factor(District99) + I(Age99^2) + I(Inc99^2) , data=pilgrim)
summary(backgroundNoMissingContwithquad)


#age and income as continuous variables with log effects

backgroundNoMissingContwithlog <-lm(Profit99 ~ Online99 + Tenure99 + Age99 + Inc99 + as.factor(District99) + I(log(Age99)) + I(log(Inc99)) , data=pilgrim)
summary(backgroundNoMissingContwithlog)


#install.packages("visreg")





library(visreg)
visreg(backgroundNoMissingCont,"Tenure99")
visreg(backgroundNoMissingCont,"Age99")
visreg(backgroundNoMissingCont,"Inc99")



#DEALING WITH MISSING DATA
summary(pilgrim) #detect missing values (NA's) for Age99 and Inc99

#approach 1: dummy variable

pilgrim$Age99.dmiss <- ifelse(is.na(pilgrim$Age99), 0, pilgrim$Age99) #substitute missing age values with zeros
pilgrim$Inc99.dmiss <- ifelse(is.na(pilgrim$Inc99), 0, pilgrim$Inc99) #substitute missing income values with zeros
pilgrim$dummyAgeMiss <- ifelse(pilgrim$Age99.dmiss==0, 1,0) #dummy variable is 0 if age is observed, and 1 if missing
pilgrim$dummyIncMiss <- ifelse(pilgrim$Inc99.dmiss==0, 1,0) #dummy variable is 0 if income is observed, and 1 if missing

#are customers with missing values different from customers without missing values?
t.test(pilgrim$Profit99 ~ pilgrim$dummyAgeMiss)
t.test(pilgrim$Profit99 ~ pilgrim$dummyIncMiss)

t.test(pilgrim$Online99 ~ pilgrim$dummyAgeMiss)
t.test(pilgrim$Online99 ~ pilgrim$dummyIncMiss)

t.test(pilgrim$Tenure99 ~ pilgrim$dummyAgeMiss)
t.test(pilgrim$Tenure99 ~ pilgrim$dummyIncMiss)

#Modeling profit using all customers
dummyModel <-lm(Profit99 ~ Online99 + Age99.dmiss + dummyAgeMiss + Inc99.dmiss + dummyIncMiss 
                + Tenure99 + as.factor(District99), data=pilgrim)

summary(dummyModel)

mean(pilgrim$Age99, na.rm=TRUE)
mean(pilgrim$Inc99, na.rm=TRUE)

#approach 2: imputation

#impute mean age/income for missing values
pilgrim$Age99.m <- ifelse(is.na(pilgrim$Age99), round(mean(pilgrim$Age99, na.rm=TRUE)), pilgrim$Age99)
pilgrim$Inc99.m <- ifelse(is.na(pilgrim$Inc99), round(mean(pilgrim$Inc99, na.rm=TRUE)), pilgrim$Inc99)

imputationMeanModel <-lm(Profit99 ~ Online99 + Age99.m + Inc99.m + Tenure99 + as.factor(District99), data=pilgrim)
summary(imputationMeanModel)

#impute age/income based on tenure and district
fit <-lm(Age99 ~ Tenure99 + as.factor(District99) , data=pilgrim) #regress age on district and tenure for non-missing observations
pilgrim$Age99.p <- round(predict(fit, pilgrim, type="response"), digits=0) #predict age values based on model above
pilgrim$Age99.impute <- ifelse(is.na(pilgrim$Age99), pilgrim$Age99.p, pilgrim$Age99) #create new age variable with predicted value if age is not observed

fit <-lm(Inc99 ~ Tenure99 + as.factor(District99) , data=pilgrim) #regress income on district and tenure for non-missing observations
pilgrim$Inc99.p <- round(predict(fit, pilgrim, type="response"), digits=0) #predict income values based on model above
pilgrim$Inc99.impute <- ifelse(is.na(pilgrim$Inc99), pilgrim$Inc99.p, pilgrim$Inc99) #create new income variable with predicted value if age is not observed

imputationRegressionModel <-lm(Profit99 ~ Online99 + Age99.impute + Inc99.impute + Tenure99 + as.factor(District99), data=pilgrim)
summary(imputationRegressionModel)

#approach 3: imputation + dummy variable

#for mean imputation
imputationMeanDummyModel <-lm(Profit99 ~ Online99 + Age99.m + dummyAgeMiss + Inc99.m + dummyIncMiss
                          + Tenure99 + as.factor(District99) , data=pilgrim)
summary(imputationMeanDummyModel)

#for imputation based on tenure and district
imputationRegressionDummyModel <-lm(Profit99 ~ Online99 + Age99.impute + dummyAgeMiss + Inc99.impute + dummyIncMiss
                          + Tenure99 + as.factor(District99), data=pilgrim)
summary(imputationRegressionDummyModel)



