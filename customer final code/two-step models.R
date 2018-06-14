
rm(list = ls())

#SET WORKING DIRECTORY

#IMPORT DATASET

file.choose()
transactions <- read.csv("/Users/landon/Desktop/customer final code/transactions2 .csv")


head(transactions)
#VISUALIZE ORDER QUANTITY

#install.packages("ggplot2")
library(ggplot2)
ggplot(transactions, aes(x = Order_Quantity)) + geom_histogram(binwidth = 10, fill = "black") + labs(x="Order Quantity", y = "Frequency")
hist(transactions$Order_Quantity,breaks=50)

#THE WRONG WAY
wrong_allpredictors <- lm(Order_Quantity ~ Gender + Married + Income + Loyalty + 
                             Lag_Purchase + Lag_Order_Quantity, data=transactions)
summary(wrong_allpredictors)


transactions$wrong_allpredictors <- predict(wrong_allpredictors, transactions, type="response")


#A BETTER WAY
#create new variable
transactions$Purchase <- ifelse(transactions$Order_Quantity == 0, 0, 1) 

transactions
#build response model using logistic regression with demo variables
logit_demographics <- glm(Purchase ~ Gender + Married + Income + Loyalty, data=transactions, family=binomial(link="logit"))
summary(logit_demographics)

transactions$logit_demographics <- predict(logit_demographics, transactions, type="response")

predict <- predict(logit_demographics, type = 'response')
summary(transactions$logit_demographics)

purchase_demopredict <- as.numeric(transactions$logit_demographics>.5)
purchase_observed <- transactions$Purchase
table(purchase_demopredict,purchase_observed)


#build response model using logistic regression with behavioral variables
logit_behavioral <- glm(Purchase ~ Lag_Purchase + Lag_Order_Quantity, data=transactions, family=binomial(link="logit"))
summary(logit_behavioral)

head(transactions)
transactions$logit_behavioral <- predict(logit_behavioral, transactions, type="response")
summary(transactions$logit_behavioral)

purchase_behavpredict <- as.numeric(transactions$logit_behavioral>.5)
purchase_observed <- transactions$Purchase
table(purchase_behavpredict,purchase_observed)

plot(transactions$Lag_Order_Quantity, transactions$logit_behavioral)



#build response model using logistic regression with behavioral and demo variables
logit_allpredictors <- glm(Purchase ~ Gender + Married + Income + Loyalty + 
                             Lag_Purchase + Lag_Order_Quantity, data=transactions, family="binomial")
summary(logit_allpredictors)
transactions$logit_allpredictors <- predict(logit_allpredictors, transactions, type="response")

purchase_allpredict <- as.numeric(transactions$logit_allpredictors>.5)
purchase_observed <- transactions$Purchase
table(purchase_allpredict,purchase_observed)


plot(transactions$Lag_Purchase, transactions$logit_allpredictors)
plot(transactions$Lag_Order_Quantity, transactions$logit_allpredictors)



#build conditional-spend model using linear regression
linear_demographics <- lm(Order_Quantity ~ Gender + Married + Income + Loyalty, data=subset(transactions,Purchase==1))
summary(linear_demographics)
transactions$linear_demographics <- predict(linear_demographics, transactions, type="response")
plot(transactions$Income, transactions$linear_demographics)



linear_behavioral <- lm(Order_Quantity ~ Lag_Purchase + Lag_Order_Quantity, data=subset(transactions,Purchase==1))
summary(linear_behavioral)
transactions$linear_behavioral <- predict(linear_behavioral, transactions, type="response")
plot(transactions$Lag_Purchase, transactions$linear_behavioral)
plot(transactions$Lag_Order_Quantity, transactions$linear_behavioral)



linear_allpredictors <- lm(Order_Quantity ~ Gender + Married + Income + Loyalty + 
                             Lag_Purchase + Lag_Order_Quantity, data=subset(transactions,Purchase==1))
summary(linear_allpredictors)
transactions$linear_allpredictors <- predict(linear_allpredictors, transactions, type="response")

plot(transactions$Lag_Purchase, transactions$linear_allpredictors)
plot(transactions$Lag_Order_Quantity, transactions$linear_allpredictors)
plot(transactions$Income, transactions$linear_allpredictors)
plot(transactions$Gender, transactions$linear_allpredictors)




#combine models to predict future spend
transactions$future_spend_predict <- transactions$logit_allpredictors*transactions$linear_allpredictors
plot(transactions$Lag_Purchase, transactions$future_spend_predict)
plot(transactions$Lag_Order_Quantity, transactions$future_spend_predict)




#COMPARISON OF THE WRONG AND THE BETTER WAY
plot(transactions$wrong_allpredictors, transactions$future_spend_predict, xlim=range(0:400), ylim=range(0:400))
+ abline (0,1)
plot(transactions$wrong_allpredictors, transactions$Order_Quantity, xlim=range(0:400), ylim=range(0:400))
plot(transactions$future_spend_predict, transactions$Order_Quantity, xlim=range(0:400), ylim=range(0:400))



# mean absolute deviation across different models
mean(abs(transactions$future_spend_predict-transactions$Order_Quantity))
mean(abs(transactions$wrong_allpredictors-transactions$Order_Quantity))

