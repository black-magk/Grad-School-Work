
#--------------------------------------------#
# solutions for practice problems, midterm 2 #
#--------------------------------------------#

###- Linear Regression w/ Interaction -### 

# predicting mpg using engine size (disp), # cylinders, and interaction
mod.1 <- lm(mpg ~ cyl*disp, data=mtcars)
summary(mod.1)

# predicted price of a car with 6 cyl and 200 disp
CYL <- 6
DISP <- 200
PREDICTION <- 	coef(mod.1)["(Intercept)"] +
				coef(mod.1)["cyl"] * CYL +
				coef(mod.1)["disp"] * DISP +
				coef(mod.1)["cyl:disp"] * CYL * DISP
unname(PREDICTION)



# same regression as mod.1 with cyl now as categorical
mod.2 <- lm(mpg ~ factor(cyl)*disp, data=mtcars)
summary(mod.2)

# predicted price of a car with 6 cyl and 200 disp
CYL6dummy <- 1
DISP <- 200
PREDICTION <- 	coef(mod.2)["(Intercept)"] +
				coef(mod.2)["factor(cyl)6"] * CYL6dummy +
				coef(mod.2)["disp"] * DISP +
				coef(mod.2)["factor(cyl)6:disp"] * CYL6dummy * DISP
unname(PREDICTION)



# same as mod.1, but predicting log(mpg)
mod.3 <- lm(log(mpg) ~ cyl*disp, data=mtcars)
summary(mod.3)

# predicted price of a car with 6 cyl and 200 disp
CYL <- 6
DISP <- 200
logPREDICT <-	coef(mod.3)["(Intercept)"] +
				coef(mod.3)["cyl"] * CYL +
				coef(mod.3)["disp"] * DISP +
				coef(mod.3)["cyl:disp"] * CYL * DISP
unname(exp(logPREDICT))



###- Logistic Regression -### 

mtcars$goodMPG <- ifelse(mtcars$mpg > 20, 1, 0)

# logistic regression
mod.4 <- glm(goodMPG ~ disp, data=mtcars, family=binomial)

# probability a car with disp = 200 has goodMPG = 1
DISP <- 200
PROB <-	exp( coef(mod.4)["(Intercept)"] + coef(mod.4)["disp"]*DISP ) /
		(1 + exp( coef(mod.4)["(Intercept)"] + coef(mod.4)["disp"]*DISP ))
unname(PROB)