
IMDB<-read.csv("Downloads/IMDB.csv",stringsAsFactors=FALSE)
head(IMDB)
##QUESTION 1

aDie<-1:6
set .seed(1);

myRolls<-sample(aDie,size=1000,replace=TRUE)

head(myRolls)


#Count the number of times that a larger roll direclty follows a smaller roll.

myRolls


##QUESTION 2-Python



##QUESTION 3

#A) of the movies with an average rating below five, which has been rated the most times?

head(IMDB)

q3a<-subset(IMDB, rating<5)
head(q3a)

head( q3a[ order(q3a$votes, decreasing=TRUE), ], 1 )

#answer: Batman & Robin

#B) What percentage are classified as belonging to more than one genre?

#greater than 1 it is classified as more than 1 genre
IMDB$more_than_one_genre<-IMDB$Action+IMDB$Animation+IMDB$Comedy+IMDB$Drama+IMDB$Documentary+IMDB$Romance+IMDB$Short

q3b<-subset(IMDB,more_than_one_genre>1)
head(q3b)

nrow(q3b)
#15537
nrow(IMDB)
#58788

nrow(q3b)/nrow(IMDB)
#aprox 26.4%

#C) what movie is clasfied as a short haas the longest run time?

q3c<-subset(IMDB,Short>0)
q3c

head(q3c[order(q3c$length,decreasing=TRUE),],1)

# title: Jaar Leuven Kort running for 240 as a classified "Short"

#D) Run a regrssion removing rows for which budget is zero or Na, predicting the natural log of votes using the natural log of budgets.From this model, what is the predicted number of votes a movie with a budget sof 1M will get?

q3d<-subset(IMDB,budget>0)
head(q3d,2)

regression.q3d <-lm( log(votes) ~ log(budget) , data=q3d )
summary(regression.q3d)

 votes<-exp(-3.891044+0.709939*log(1000000))
 votes #371.34 





predict(regression.q3d)


install.packages('ggplot2')

library(ggplot2)



q3d$actual_votes_logged_value<-log(q3d$votes)

q3d$predicted_votes_logged_value<-predict(regression.q3d)

qplot(q3d$budget,q3d$actual_votes_logged_value)


#e)I AM NOT 100% SURE WHAT THIS QUESTION IS ASKING SO I PROVIDED TWO VISUALIZATIONS EACH EXPLAINED Below

#BELOW GRAPHS BUDGET VS. THE ACTUAL_VOTES_LOGGED_VALUES FITTED WITH A  BEST FIT LINE

gg<-ggplot(data=q3d,aes(x=budget,y=actual_votes_logged_value,c))+geom_point(alpha=1/5,size=1/10)+geom_smooth()+scale_color_manual(values=c("blue","orange"))+theme_bw()
gg


#BELOW GRAPHS BUDGET VS. THE _VOTES_LOGGED_VALUES FITTED WITH A BEST FIT LINE


gg.1<-ggplot(data=q3d,aes(x=budget,y=actual_votes_logged_value,c))+geom_point(alpha=1/5,size=1/10)+geom_smooth()+scale_color_manual(values=c("blue","orange"))+theme_bw()

gg.1

#f) Create a second visualization, witht he x asix equal to year and the y asix equal to number of votes. Plot the median number of votes per year so one dot per yer. Use error ars or lines to illustrate the IQR of the number of votes per year
head(IMDB)

plotDF <- aggregate(votes ~ year, FUN=median, data=IMDB)
plotDF$minvotes <- aggregate(votes~ year, FUN=min, data=IMDB)$votes
plotDF$maxvotes <- aggregate(votes ~ year, FUN=max, data=IMDB)$votes

plotDF

plotDFgraph<-ggplot(data=plotDF, aes(x=year, y=votes))+geom_point()+geom_pointrange(aes(ymin=minvotes, ymax=maxvotes))+xlab("year")+ylab("min, median, and max votes")+theme_bw()

plotDFgraph




##Question 4

Salaries<-read.csv("Downloads/Salaries.csv",stringsAsFactors=FALSE)

unique(Salaries$rank)
head(Salaries)


#A)- run a regression where you try to predict salary using yrs.sicne.phd, rank, and their interaction

#create dummies
Salaries$AsstProf<-ifelse(Salaries$rank=="AsstProf",1,0)
Salaries$Prof<-ifelse(Salaries$rank=="Prof",1,0)
Salaries$AssocProf<-ifelse(Salaries$rank=="AssocProf",1,0)



#since rank is categorical to find the predicted salary we will need multiple interaction terms.


q4a<-lm(salary~yrs.since.phd+AsstProf*yrs.since.phd+AssocProf*yrs.since.phd+Prof*yrs.since.phd, data=Salaries)
summary(q4a)



##It apears from the regression output that collinearity existed and in order for the model to continue it had to drop a 2 values: 1 interaction and 1 other involving "Prof"

#B)-According to your model, what is the best prediction for the salary of an associate professor who recieved a Phd 10 years ago?



q4b.answer<-unname(coef(q4a)[1]+coef(q4a)[2]*10+coef(q4a)[4]*1+coef(q4a)[7]*10*1)



#Well

 q4b.answer
 
 #96126.12
# _________________________________
 #C) according to your model, what is the predicted pay increase for a person who had had a Phd for 5 years and gets promoted from assistant to aossociat Prof
 
 
# Well find initial    


q4c.answer<-unname(coef(q4a)[1]+coef(q4a)[2]*5+coef(q4a)[3]*1+coef(q4a)[6]*10*1)


q4c.answer

#78291


q4c.answer.1<-unname(coef(q4a)[1]+coef(q4a)[2]*5+coef(q4a)[4]*1+coef(q4a)[7]*10*1)


q4c.answer.1
#96134.2

pay_increase<-q4c.answer.1-q4c.answer
pay_increase

#17843.2


#D)-Creat new column in the data to represent the dummy coded variable in the regression from part a. Reproduct the the regrsssion estuls using these variables and not using the interactin symbols in the lm() function

Salaries$interaction1<-Salaries$AsstProf*Salaries$yrs.since.phd
Salaries$interaction2<-Salaries$AssocProf*Salaries$yrs.since.phd
Salaries$interaction3<-Salaries$Prof*Salaries$yrs.since.phd

q4d<-lm(salary~yrs.since.phd+AsstProf+AssocProf+Prof+interaction1+interaction2+interaction3,data=Salaries)

summary(q4d)



test.answer<-unname(coef(q4d)[1]+coef(q4d)[2]*5+coef(q4d)[3]*1+coef(q4d)[6]*10*1)


test.answer

#check good

#E) create a visualization to further examine the relationship between yrs.since.phd,rank,salary

q4e<-ggplot(data=Salaries,aes(x=yrs.since.phd,y=salary,color=rank))+geom_point(alpha=1/5,size=1/10)+geom_smooth(method="lm",size=1/2)+scale_color_manual(values=c("blue","orange","green"))+theme_bw()


q4e














