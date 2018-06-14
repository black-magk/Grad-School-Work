

#installing approprate packages


install.packages("qplot")
install.packages("ggplot")
install.packages("ggplot2")
library("ggplot2")


college<- read.csv("Desktop/colleges.csv")

#view data
head(college)
head(college[1])

#elminating NA's in Math.SAT columns


#compare MATH.SAT scores
college1.0<-subset( college, Math.SAT >"0")

#removing NAs in the Graduation.rate

college2.0<-subset( college1.0, Graduation.rate >"0")


#View check

head(college2.0)


#remove NAs in the  X..new.stud..from.top.10. column


college3.0<-subset( college1.0,X..new.stud..from.top.10.>"0")

#view check


head(college3.0)

college4.0<-subset( college3.0,Verbal.SAT>"0")



#After sufficient subsetting I wanted to look at the comparison of Math.SAT vs Verbal.SAT scores. 


graph2.0<-plot(college4.0$Math.SAT,college4.0$Verbal.SAT)


graph2.0

#What we see is there is an upward trend between the two scores.library(qplot)



college2.0$ X..new.stud..from.top.10.

#After sufficient subsetting I wanted to look at the comparison of Math.SAT vs Verbal.SAT scores while color coding for whether the school is private or public

#using ggplot2 I wanted to add more layers to the graphical data. I used the code below:



b



ggg

head(college3.0[3],1)

college$Public..1...Private..2.






 


