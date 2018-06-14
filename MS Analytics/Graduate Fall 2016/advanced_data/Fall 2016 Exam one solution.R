# Exam one solution 
# Dan Zhang, CU Boulder, October 1, 2016

#1. B
#2. C
#3. B
#4. D
#5. D
#6. D
#7. A
#8. D
#9. D
#10. B
#11. C
#12. D
#13. C
#14. C

# answers to midterm exam questions
# 15. 310
#Counts the number of rows in the dataset
nrow(boulder.exam)
# 16. 2276
#tapply takes the dataframe and can take to variables at once and apply a function
tapply(boulder.exam$SQFT,boulder.exam$ZIP,mean)

# 17. BATHS
#taking the correlation of the dataset allows you to view correlation of all the variables
cor(boulder.exam[,-c(1,2,10)])

# 18. 0.643

#lm function is a regression model function
lm.fit = lm(LIST.PRICE~SQFT,data=boulder.exam)
summary(lm.fit)

# 19. 1153
#by using coef you are essentially plugging in values into your formula from the previous regression model.
coef(lm.fit)[1]+coef(lm.fit)[2]*3000

# 20. Yes. New adjusted r-squared 0.7869
# using the log() you take the log of that specific variable and test it that way
lm.fit = lm(log(LIST.PRICE)~log(SQFT),data=boulder.exam)
summary(lm.fit)

#21. 1027 
exp(coef(lm.fit)[1]+coef(lm.fit)[2]*log(3000))

#22. 0.7481
boulder.exam1 = boulder.exam
#removes that specified column
boulder.exam1$LOT.SIZE = NULL
lm.fit = lm(LIST.PRICE~.,data=boulder.exam1)
summary(lm.fit)

#23. still about the same 0.7488
lm.fit = lm(LIST.PRICE~.+PARKING.SPOTS*SQFT,data=boulder.exam1)
summary(lm.fit)

# 24. 248815.1
lm.fit = lm(LIST.PRICE~.,data=boulder.exam1[1:200,])
mean((boulder.exam1$LIST.PRICE-predict(lm.fit,newdata=boulder.exam1))[201:310]^2)

# 25. 93
sum(is.na(boulder.exam$LOT.SIZE))

# 26 6840
mean(boulder.exam$LOT.SIZE[boulder.exam$LOT.SIZE<=15000],na.rm=TRUE)

# 27 0.0995
boulder.exam1$LOT.SIZE = boulder.exam$LOT.SIZE
#sets where certain variable is equal to na we make it now equal to 6840
boulder.exam1$LOT.SIZE[is.na(boulder.exam1$LOT.SIZE)]=6840
lm.fit = lm(LIST.PRICE~LOT.SIZE,data=boulder.exam1)
summary(lm.fit)

# 28 7.186e-03

# 29 0.1615
boulder.exam1 = na.omit(boulder.exam)
lm.fit = lm(LOT.SIZE~.-LIST.PRICE,data=boulder.exam1)
summary(lm.fit)

# 30 11.66







