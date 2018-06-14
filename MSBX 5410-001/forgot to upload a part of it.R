install.packages('ggplot2')
library(ggplot2)

head(diamonds)

diamonds$boxsize<-diamonds$x*diamonds$y*diamonds$z

head(diamonds)


head(diamonds)
head(diamonds1<- data.frame(diamonds[ order(diamonds$boxsize, decreasing=TRUE), ]))


nrow(diamonds)
54940*.005


install.packages('ggplot2')
library(ggplot2)

#1-examine relationship

diamonds$boxsize<-diamonds$x*diamonds$y*diamonds$z

nfive<-quantile(diamonds$boxsize, prob=.995)
five<-quantile(diamonds$boxsize,prob= .005)

head(nfive)
head(five)

new<-diamonds[which(diamonds$boxsize <= nfive & five>= five),]


a<-lm(new$price~new$boxsize)
summary(a)
b<-lm(new$price~new$cut)



summary(b)


new$cut <-factor(new$cut,ordered=FALSE)

mod.1<-lm(new$price~new$cut+new$boxsize)
summary(mod.1)

#a)
(cutsub1<- subset(new, new$cut =='Fair'))
(cutsub2<- subset(new, new$cut =='Ideal'))
total<-rbind(cutsub1,cutsub2)

qplot(total$boxsize,total$price)
#b)

new$cut <-factor(new$cut,ordered=FALSE)

mod.1<-lm(new$price~new$cut+new$boxsize)
summary(mod.1) 

mod.1<-lm(new$price~new$cut)
summary(mod.1)

#intercept means... that when boxsize and cut of diamond are held constant at zero that the price on avg of that diamond is approx $3,685 dollars. However, its hard to interpret that if the  diamond does not have a shape .

When box size is 130.. the predicted price of a Very good diamond is 

Verygoodprice<- -3.685e+03+1.214e+03*(1)+ 4.934e+01*130
Verygoodprice
approx $3942.2 


#Since the fair coefficient is eliminated from the model we have the boxsize plus the intercept... this we have an avg fair diamond price of 

-3.685e+03  +4.934e+01*80
approx $262.2
coef(mod.1)

summary(lm(price ~ factor(cut), data = new))

head(new)

install.packages('dummies-package')

library(dummies-package)






#using dummy variables

A <- data.frame(model.matrix(new$price ~ new$cut+new$boxsize))
head(A)

z<-lm(new$price~ A$new.cutGood+ A$new.cutVery.Good+ A$new.cutPremium+ A$new.cutIdeal+A$new.boxsize)

summary(z)

cor(new$price,A$new.cutGood)
cor(new$price,A$new.cutVery.Good)
cor(new$price,A$new.cutPremium)
cor(new$price,A$new.cutIdeal)
cor(new$price,A$new.boxsize)


