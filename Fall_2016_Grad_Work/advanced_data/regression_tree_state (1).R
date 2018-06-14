# state data revisited solution
# Dan Zhang, 09/26/2014

data(state)
statedata = data.frame(state.x77)

#Q1: 
# (a) 0.6921823
lm.fit = lm(Life.Exp~.,data=statedata)
lm.summary=summary(lm.fit)
lm.summary$adj.r.squared

# (b) 23.2971
sum((statedata$Life.Exp-predict(lm.fit))^2)

# (c): 0.712569
lm.fit1 = lm(Life.Exp~Population+Murder+Frost+HS.Grad,data=statedata)
lm.summary1=summary(lm.fit1)
lm.summary1$adj.r.squared

# (b) 23.3080
sum((statedata$Life.Exp-predict(lm.fit1))^2)


# Q2
# (a): Murder, Area, HS.Grad
library(tree)
tree.state = tree(Life.Exp~.,data=statedata)
plot(tree.state)
text(tree.state)

# (b) 23.6428
sum((statedata$Life.Exp-predict(tree.state))^2)

# Q3
# (a) 32.8655
set.seed(200)
cv.state = cv.tree(tree.state)
plot(cv.state$size,cv.state$dev,type='b')
prune.state = prune.tree(tree.state,best=3)
plot(prune.state)
text(prune.state,pretty=0)
sum((statedata$Life.Exp-predict(prune.state))^2)

# (b) Yes




