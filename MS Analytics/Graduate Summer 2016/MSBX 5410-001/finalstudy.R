install.packages('ggplot2')
library(ggplot2)

head(diamonds)
##//finding info in dataframe
##finding a certain mean in a given data frame example

mean(diamonds[diamonds$cut=="Ideal",]$price)

#Better subseting procedures

idealDiamonds<- subset(diamonds,cut=="Ideal")

#finding mean of a subset and combining additionall criteria

mean(subset(diamonds,cut=="Ideal")$price)

mean(subset(diamonds,cut=="Ideal" & table>61)$price)

# initialize a vector 

diamonds$BigPrice<-rep(NA, nrow(diamonds))

#tables- helpful when want to look at a matrix fo data

table(diamonds$color,diamonds$cut)

#tables with criteria added

mytable<-table(diamonds$color,diamonds$cut=="Fair")

#prop.table()
#useful for finding probabilities


prop.table(table(diamonds$color,diamonds$cut),margin=1)

# index a table
mytable[2,2]


#aggregate function
#useful for finding statistics on data.frames

aggregate(price~cut,FUN=mean,data=diamonds)

aggregate(price~cut+color,FUN=mean,data=diamonds)


##load csv properly



orders 
<-read.csv("~/Desktop/orders.csv",stringsAsFactors=FALSE)


##create a datafram with data.frame

x<-data.frame(aggregate(price~cut+color,FUN=mean,data=diamonds))

head(diamonds)

#renaming column headers
names(diamonds)[2]<-"Testing123"
head(diamonds)

#ording a dataframe descending

head(diamonds[order(diamonds$depth,decreasing=TRUE),])

#replacing formatting errors

mid$removeMinutes <- sub( pattern="m ", replacement=".", x=mid$sessionTime )

mid$removeSeconds <- sub(pattern="s", replacement="", x=mid$removeMinutes )


#histogram
hist(diamonds$price)
#boxplot
boxplot(carat~price,data=diamonds)

#cut() function

myVec<- 1:10
data.frame(myVec,"cut"=cut(myVec,breaks=c(1,4,7,10),include.lowest=TRUE))
#more cut() FUNCTION EXAMPLE
diamonds$caratBin<-cut(diamonds$carat,breaks=10)
head(diamonds$caratBin)

#quantile() function

quantile(myVec)

quantile(myVec,probs=c(0,.3,.7,1))

#quantile() + cut() function

data.frame(myVec,"cut"=cut(myVec,breaks=quantile(myVec),include.lowest=TRUE))

#merge data.frame
#ex

newdf<- merge(df1,df2,by="common_column")



#using ggplot

head(diamonds)


# find median weight (carat) by bin
mCaratDF <- aggregate(carat~caratBin,FUN=median, data=diamonds)
# find median price by bin
mPriceDF<-aggregate(price~caratBin,FUN=median,data=diamonds)

byBin<-merge(mCaratDF, mPriceDF, by="caratBin")
head(byBin)

ggplot(data=byBin,aes(x=price,y=carat))+geom_point()+xlab("TESTING AXIS")+ylab("TESTING Y AXIS")+theme_bw()
ggplot

install.packages("dplyer")

#more on graphs

cars<- read.csv("Downloads/cardata.csv")
head(cars)
#qplot()

#geom_smooth adds smooth line in graph ( trendline essentially)

qplot(x=Mileage,y=Price,data=cars)+geom_smooth(method="lm")



#regression using lm()

Mod.1<-lm(Price~Mileage,data=cars)
summary(Mod.1)

#coefficients

coef(Mod.1)

#plotting residuals

qplot(resid(Mod.1))

#formula for finding results from predicted model

unname(coef(Mod.1)[1]+coef(Mod.1)[2]*2500)


newCars<-cars[c(108:110, 148:150, 201:206, 211:215, 738:740),]

Mod.2<-lm(Price~Mileage,data=newCars)
summary(Mod.2)

#How to eliminate  outliers

diamonds$boxsize<-diamonds$x*diamonds$y*diamonds$z
#breaks in data in this case lower and upper percentagess

myBreaks<-quantile(diamonds$boxsize,probs=c(.005,.995))
myBreaks

#new Sub with breaks
diamonds$mid99<-cut(diamonds$boxsize,breaks=myBreaks,include.lowest=TRUE)
#workiing with a subset that cuts off ends
d99<-subset(diamonds,is.na(mid99)==FALSE)


head(d99)

plotdata<-subset(d99, Testing123== "Ideal"|Testing123== "Fair")

##ggplot expanded
gg<-ggplot(data=plotdata,aes(x=boxsize,y=price,color=Testing123))+geom_point(alpha=1/5,size=1/10)+geom_smooth(method="lm",size=1/2)+scale_color_manual(values=c("blue","orange"))+theme_bw()

gg

ggplot( data=plotdata,aes(x=boxsize,y=price,color=cut))+geom_point(alpha=1/5,size=1/10)+geom_smooth(method="lm",size=1/2)+xlab("box size")+ylab("price")+scale_color_manual(values=c("blue","orange"))+theme_bw()

# unorderthe factor

d99$cut<-factor(d99$cut,ordered=FALSE)

#creating dummy variables

d99$fairDummy<-ifelse(d99$Testin123=="Fair",1,0)
d99$goodDummy<-ifelse(d99$Testing123=="Good",1,0)
d99$vGoodDummy<-ifelse(d99$Testing123=="Very Good",1,0)
d99$premDummy<-ifelse(d99$Testing123=="Premium",1,0)
d99$idealDummy<-ifelse(d99$Testing123=="Ideal",1,0)


d99

#ggplot with multiple colors

ggg<-ggplot(data=cars, aes(x=Mileage, y=Price,color=Make))+geom_point()
ggg

head(newCars)

ccc<-ggplot(data=cars, aes(x=jitter(Cruise), y=Price,color=factor(Leather)))+geom_point()+scale_x_continuous("Cruise", breaks=c(0,1), labels=c("no","yes"))+scale_color_manual("Leather Seats", breaks=c(0,1),labels=c("no","yes"), values=c("orange","blue"))
ccc
#aggregating data.frame
plotData<-aggregate(Price~Cruise+Leather, FUN=mean, data=cars)
#finding standard deviation
plotData$SD<-aggregate(Price~Cruise+Leather, FUN=sd, data=cars)$Price


plotData$N<-aggregate(Price~Cruise+Leather, FUN=length, data=cars)$Price
#finding standard eror
plotData$SE<-plotData$SD/ sqrt(plotData$N)


plotData$Price_pSE<-plotData$Price+ plotData$SE

plotData$Price

plotData$Price_mSE<-plotData$Price-plotData$SE

fff<-ggplot(data=plotData, aes(x=factor(Cruise), y=Price,fill=factor(Leather)))+geom_bar(stat="identity", position=position_dodge(width=0.9))+geom_errorbar(aes(ymin=Price_mSE, ymax=Price_pSE), position=position_dodge(width=0.9), width=.1)



fff



##finding predicted values in table
mod.2 <-lm( Price ~ Cruise * Leather, data=cars )

#creates variables to be plugged in
predictDF<-data.frame(Cruise = c(0,0,1,1),Leather = c(0,1,0,1))

predictDF

#generates formula to plug in above values in mod.2 regression
predictDF$pPrice2 <-predict(mod.2, predictDF)

# prints results
print(predictDF)




#taking out outliers with tukeys method

# load full data set
 hyb.raw <- read.csv("Downloads/hybrid_reg.csv", stringsAsFactors=FALSE)
 
 # find quantiles and calculate IQR
 
 Q1 <- quantile(hyb.raw$msrp, probs=.25)
 
 Q3 <- quantile(hyb.raw$msrp, probs=.75)
 
 IQR <- Q3-Q1
 
 # removeoutliers a la Tukey
 
 hyb <- subset(hyb.raw, msrp > Q1-IQR*1.5 & msrp < Q3+IQR*1.5)
 
 nrow(hyb.raw)-nrow(nrow)
 
 
 # howmanyoutliersremoved?nrow(hyb.raw)-nrow(hyb)


##using if statements to generate values in a new column in your data frame

hyb$carTruck <- ifelse( hyb$class %in% c("C","M","TS","L"), "car", "truck")

head(hyb$carTruck,40)

### logit functional regressions

age.data <- data.frame(age =c(27,30,32,33,35,40,44,45,50,58,59,60),buyer = c(0,0,1,0,0,1,1,1,0,1,1,1))

age.data

summary(lm(buyer ~ age, data=age.data))


summary(	glm(buyer	~age,	family=binomial(link="logit"),				data=age.data))








