# classification tree exercise
# Dan Zhang, 09-07-2014

rm(list=ls())
library(tree)
#setwd('C:/Users/dazh1150/Google Drive/teaching/Fall2015/Data Analytics/lab/docs')
setwd('C:/Users/dazh1150/Dropbox/Fall2016/Advanced Data Analytics/lab/docs')

# Q1: 31.59%
# neighbors group has the largest fraction of voters (0.3779)
gerber = read.csv('gerber.csv')
summary(gerber)
mean(gerber[gerber$hawthorne==1,]$voting)
mean(gerber[gerber$civicduty==1,]$voting)
mean(gerber[gerber$neighbors==1,]$voting)
mean(gerber[gerber$self==1,]$voting)


# Q2
# (a) (134513+51966)/nrow(gerber) = 0.5419578
glm.fit = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber,family=binomial)
summary(glm.fit)
gerber.pred = ifelse(predict(glm.fit,type='response')>0.3,1,0)
table(gerber.pred,gerber$voting)

# (b) 235388/nrow(gerber) = 0.6841004
gerber.pred = ifelse(predict(glm.fit,type='response')>0.5,1,0)
table(gerber.pred,gerber$voting)

#Q3
# (a) B
tree.gerber = tree(voting ~ civicduty + hawthorne 
                   + self + neighbors, data=gerber)
plot(tree.gerber)

# (b) B
# fraction of "civil duty" people voted is 0.3145
tree.gerber = tree(voting ~ civicduty + hawthorne+ self + neighbors, data=gerber,control=tree.control(nobs=nrow(gerber),mindev = 0))

# Q4: B
tree.gerber = tree(voting ~ control, data=gerber,control=tree.control(nobs=nrow(gerber),mindev = 0))
tree.gerber1 = tree(voting ~ control+sex, data=gerber,control=tree.control(nobs=nrow(gerber),mindev = 0))


# Q5: A
glm.fit = glm(voting ~ control+sex, data=gerber,family=binomial)
summary(glm.fit)

# Q6: 0.0003
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(glm.fit, newdata=Possibilities, type="response")

# Q7: D
glm.fit2 = glm(voting ~ sex + control + sex:control, 
               data=gerber, family=binomial)
summary(glm.fit2)

