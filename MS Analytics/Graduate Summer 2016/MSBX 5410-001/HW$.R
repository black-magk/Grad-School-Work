install.packages('ggplot2')
library(ggplot2)
a) Creating a new column in the data.frame called “caratBin”. 

diamonds$caratBin <- rep(NA, length.out=nrow(diamonds)) 

b) Assigning each diamond to 1 of 10 possible values of “caratBin”, such that each “bin” has approximately the same number of data points. The “quantile()” and “cut()” functions may be useful here. The first bin should be something like “(.2, .31]” (diamonds greater than .2 carats and less than or equal to .31 carats). 

diamonds$caratBin<-cut(diamonds$carat, quantile(diamonds$carat,probs=seq(from=0,to=1,by=0.1)))
diamonds$caratBin


c) Finding the median weight (in carats) and median price of the diamonds in each bin. 

data.frame(aggregate(diamonds$carat~diamonds$caratBin,FUN=median))
data.frame(aggregate(diamonds$price~diamonds$caratBin,FUN=median))  
d) Creating a new data.frame with three columns: “caratBin”, “carat_medianOfBin”, and “price_medianOfBin”. There should be one row for each of the 10 bins.

x<-data.frame(aggregate(diamonds$carat~diamonds$caratBin,FUN=mean)) 
y<-data.frame(aggregate(diamonds$carat~diamonds$caratBin,FUN=median))
z<-data.frame(aggregate(diamonds$price~diamonds$caratBin,FUN=median))  
x<-x$diamonds.caratBin
y<-y$diamonds.carat
z<-z$diamonds.price
x_name <- "caratBin"
y_name <- "carat_medianOfBin"
z_name <- "price_medianOfBin"

df <- data.frame(x,y,z)
names(df) <- c(x_name,y_name,z_name)
print(df)

qplot(x=z,y=y)

down vote
 e) Using ggplot() or qplot() to generate a new plot from this data.frame of “carat_medianOfBin” (y-axis) by “price_medianOfBin” (x-axis).
 
 
qplot(x=z,y=y)