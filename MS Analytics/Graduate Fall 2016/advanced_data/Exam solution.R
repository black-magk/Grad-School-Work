# answers to midterm exam questions
# 15. 310

# 16. 2276
tapply(boulder.exam$SQFT,boulder.exam$ZIP,mean)

# 17. BATHS
cor(boulder.exam[,-c(1,2,10)])

# 18. 0.643
lm.fit = lm(LIST.PRICE~SQFT,data=boulder.exam)
summary(lm.fit)

# 19. 1153
coef(lm.fit)[1]+coef(lm.fit)[2]*3000

# 20. Yes. New adjusted r-squared 0.7869
lm.fit = lm(log(LIST.PRICE)~log(SQFT),data=boulder.exam)
summary(lm.fit)

#21. 1027 
exp(coef(lm.fit)[1]+coef(lm.fit)[2]*log(3000))

#22. 0.7481
boulder.exam1 = boulder.exam
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
boulder.exam1$LOT.SIZE[is.na(boulder.exam1$LOT.SIZE)]=6840
lm.fit = lm(LIST.PRICE~LOT.SIZE,data=boulder.exam1)
summary(lm.fit)

# 28 7.186e-03

# 29 0.1615
boulder.exam1 = na.omit(boulder.exam)
lm.fit = lm(LOT.SIZE~.-LIST.PRICE,data=boulder.exam1)
summary(lm.fit)

# 30 11.66







