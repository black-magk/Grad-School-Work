A2_part_1 <- read.csv("/Users/landon/Desktop/pa21.csv")

##Part 1: Interaction Modeling

##QUESTION!
A2_part_1_1<-data.frame(A2_part_1)
#check
head(A2_part_1_1,20)

#f#actoring dummy variables
A2_part_1_1$Age99.f <- factor(A2_part_1_1$Age99)
is.factor(A2_part_1_1$Age99.f)
A2_part_1_1$Inc99.f <- factor(A2_part_1_1$Inc99)
is.factor(A2_part_1_1$Inc99.f)
reg.1<-lm(Profit99~ Inc99.f+Age99.f + Online99, data= A2_part_1_1)

summary(reg.1)

##The results show that whether you are online or not is significant 
##having a positive effect.It also appears from the coefficients and the 
##levels of statistical significance that being in the highest income bracket and 
##being the oldest in the set have the greatest positive impact on this particular profit model.


##QUESTION 2!

reg.2<-lm(Profit99~Inc99.f*Online99, data= A2_part_1_1)
summary(reg.2)

##There appears to be only one interaction between online/offline in conjunction with Inc 
##that is statistically significant and worth noting; Being online and in the final income 
##bracket had the largest effect on profits

reg.3<-lm(Profit99~Age99.f*Online99+Inc99.f*Online99, data= A2_part_1_1)
summary(reg.3)
##There appears to be no interaction between Online/offline in conjunction with
##Age bracket that is statically significant or worth noting.

## QUESTION 3!
##MAIN MODEL
colnames(A2_part_1_1)
reg.f<-lm(Profit99~Online99+ Tenure99+Age99.f+Inc99.f+Online99*Age99.f+Online99*Inc99.f, data= A2_part_1_1)
summary(reg.f)
anova(reg.f)
reg.f.f<-step(reg.f, direction = "backward", trace=FALSE ) 
summary(reg.f.f)


cor(A2_part_1_1$Profit99,A2_part_1_1$Age99.f)

reg.f.f.f<-lm(log(Profit99)~Online99+ Tenure99+Age99.f+Inc99.f+Online99*Age99.f+Online99*Inc99.f, data= A2_part_1_1)


summary(reg.f.f.f)
plot.1<-plot(reg.f$residuals)


plot.2<-plot(reg.f)


## It is clear a linear model does not fit this set of data points. Recommendation would be to "zoom"
##in on the data and determine if a polynomial relationship exists. 

##QUESTION 4!


##Part 1 suggested that online banking seems to have a positive effect. However,
## when you include all the variables factored including interactions with the online99 variable
## you notice that the estimate on online99 becomes negative and not statistically significant. 
## Looking that the interaction terms it may have a positive effect in the singular case that those 
##online of the age group "5." An interesting cavot is when you use backwards elimination to minimize the variables
## used in the model while achieving a maximum "best fit", it actually eliminates the interaction terms
##from the model entirely.
