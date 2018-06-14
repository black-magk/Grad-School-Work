#1-Remove observations for which MSRP is considered an outlier by Tukey’s proposed methodology (http://www.edgarstat.com/tukeys_outliers_help.cfm). How many observations were removed? Use this subset for the remainder of the questions. 

##installed referenceIntervals package that utilizes Tukeys interquartile fences.
##download X11 so package would run in R
##updated to R 3.3.1
##install ggplot2

install.packages('referenceIntervals')
library(referenceIntervals)
install.packages('ggplot2')
library(ggplot2)
#downloaded data
hybrid<- read.csv("Downloads/hybrid_reg.csv")
#check
head(hybrid)


#unlisted
hybrid2 <- as.data.frame(lapply(hybrid, unlist))

head(hybrid2)
#used referenceinterval package command "horn.outliers" to return a subset of the data with removed outliers. Data returned subset and $outliers numeric(0) of MSRP
horn.outliers(hybrid2$msrp)


#2-Create a visualization, with “year” on the x-axis and “price” on the y-axis. For each year, plot a single point representing the median price (“msrp”) for hybrid vehicles sold that year. Use geom_pointrange() to display the range in prices for each year (bottom of the point range should represent the cheapest vehicle that year, top of the point range should represent the most expensive vehicle that year). 

##checked years in set
unique(hybrid2$year)

#creating subsets to find median msrp per given year
#gotta be an easier way
a<-subset( hybrid2, year==1997 )
m1<-median(a$msrp)
b<-subset( hybrid2, year==2000 )
m2<-median(a$msrp)
c<-subset( hybrid2, year==2001 )
m3<-median(a$msrp)
d<-subset( hybrid2, year==2002 )
m4<-median(a$msrp)
e<-subset( hybrid2, year==2003 )
m5<-median(a$msrp)
f<-subset( hybrid2, year==2004 )
m6<-median(a$msrp)
g<-subset( hybrid2, year==2005 )
m7<-median(a$msrp)
h<-subset( hybrid2, year==2006 )
m8<-median(a$msrp)
i<-subset( hybrid2, year==2007 )
m9<-median(a$msrp)
j<-subset( hybrid2, year==2008 )
m10<-median(a$msrp)
k<-subset( hybrid2, year==2009 )
m11<-median(a$msrp)
l<-subset( hybrid2, year==2010 )
m12<-median(a$msrp)
m<-subset( hybrid2, year==2011 )
m13<-median(a$msrp)
n<-subset( hybrid2, year==2012 )
m14<-median(a$msrp)
o<-subset( hybrid2, year==2013 )
m15<-median(a$msrp)
m15

#vector collecting medians over the years
y<-c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)

# vector collecting years
z<-unique(hybrid2$year)

#creates new dataframe with above vectors
z_name <- "Year"
y_name <- "MedianMSRP"

require(reshape2)
df <- data.frame(z,y)
colnames(df) <- c(z_name, y_name)
print(df)

#Attempt at using ggplot
xx<-ggplot( data=df, aes(x=Year, y=MedianMSRP,))+ geom_pointrange(ymin=24000,ymax=26000)+  theme_bw()
#prints plot
xx

#3-Create a new column representing whether the vehicle is a car or a truck. Consider the “C”, “M”, “TS” and “L” classes as “cars” and everything else as “trucks”. 


#ifstatement

hybrid2$truckoCar<-ifelse(hybrid2$class=="C","cars",ifelse(hybrid2$class=="M","cars",ifelse(hybrid2$class=="TS","cars",ifelse(hybrid2$class=="L","cars","Trucks"))))

#check

head(hybrid2$truckoCar,50)




#44) Conduct a regression predicting price (“msrp”) based on whether the vehicle is a car or truck, the vehicle’s fuel economy (“mpg”), and the interaction between these two variables. Provide an interpretation for the intercept of this model and all three coefficients. 

head(hybrid2)

#coding in binary
# 1 if car
# 0 if truck

hybrid2$truckoCarcoded<-ifelse(hybrid2$truckoCar=="cars",1,0)

#check

head(hybrid2$truckoCarcoded,50)


#regression

regression.1<-lm( msrp ~ truckoCarcoded+mpg+mpg*truckoCarcoded , data=hybrid2 )
summary(regression.1)

#intercept-When a vehicle has zero miles and is neither a car or truck its avg predicted msrp is approx $71k

#truckoCarcoded-When a vehicle is a car on avg predicted from a model the avg predicted msrp will be approx $10K + $71K.

#mpg- For every additional 1 the avg predicted mpq will decrease by approx $1k.

#truckoCarcoded:mpg- with interaction terms you can determine the partial effect. In this case when a vehicle is a car the predicted avg MSRP is to decrease by  approx -$200 dollars in comparison to its truck counterpart.


#5 From the regression in part 4, write out the regression equation. What is the predicted price (from the model) for a truck that gets 30 mpg? What about a car that gets 25 mpg? 

#establish values (Q1)

truckoCarcoded<-0
mpg<-30
#equation-run
predictedmsrp<-71526.8+ 10922.3*truckoCarcoded+-988.0 *mpg-201.0*truckoCarcoded*mpg

#printed predictedmsrp

predictedmsrp

#41886


#establish values (Q2)

truckoCarcoded<-1
mpg<-25

#equation-run

predictedmsrp<-71526.8+ 10922.3*truckoCarcoded+-988.0 *mpg-201.0*truckoCarcoded*mpg

#printed predictedmsrp

predictedmsrp

#52724





