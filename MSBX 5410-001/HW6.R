#1-Remove observations for which MSRP is considered an outlier by Tukey’s proposed methodology (http://www.edgarstat.com/tukeys_outliers_help.cfm). How many observations were removed? Use this subset for the remainder of the questions. 

##installed referenceIntervals package that utilizes Tukeys interquartile fences.
##download X11 so package would run in R
##updated to R 3.3.1


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


unique(hybrid2$year)
table(hybrid2$year)
summary(unique(hybrid2$year))
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

y<-c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)

z<-unique(hybrid2$year)


z_name <- "Year"
y_name <- "MedianMSRP"

require(reshape2)
df <- data.frame(z,y)
colnames(df) <- c(z_name, y_name)
print(df)


xx<-ggplot( data=df, aes(x=Year, y=MedianMSRP,))+ geom_pointrange(ymin=24000,ymax=26000)+  theme_bw()
xx
#3-Create a new column representing whether the vehicle is a car or a truck. Consider the “C”, “M”, “TS” and “L” classes as “cars” and everything else as “trucks”. 

head(hybrid2)

a<-unique(hybrid2$class)
a
hybrid2$truckoCar<-ifelse(hybrid2$class=="C","cars",ifelse(hybrid2$class=="M","cars",ifelse(hybrid2$class=="TS","cars",ifelse(hybrid2$class=="L","cars","Trucks"))))
hybrid2$truckoCar<-ifelse(hybrid2$class=="M","cars",0)
hybrid2$truckoCar<-ifelse(hybrid2$class=="TS","cars",0)
hybrid2$truckoCar<-ifelse(hybrid2$class=="L","cars",0)
hybrid2$truckoCar<-ifelse(hybrid2$class!="cars","Trucks",0)
hybrid2$truckorCar


unique(hy)







