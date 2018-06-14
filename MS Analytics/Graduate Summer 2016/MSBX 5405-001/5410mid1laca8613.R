



##Question 1

set.seed(1);

 diceRolls <- sample(1:6, size=10000, replace=TRUE)
 #a) in the simulation, what was the most commonly rolled number?
 
 head(diceRolls,5)
 table(diceRolls)
 sum(table(diceRolls))
 nRolled<-length(diceRolls)
 nRolled
 rolls1<-1692
 rolls2<-1631
 rolls3<-1724
 rolls4<-1586
 rolls5<-1633
 rolls6<-1734 #most common
 
 
 rolls1Percentage<-rolls1/nRolled
 rolls2Percentage<-rolls2/nRolled
 rolls3Percentage<-rolls3/nRolled
 rolls4Percentage<-rolls4/nRolled
 rolls5Percentage<-rolls5/nRolled
 rolls6Percentage<-rolls6/nRolled
 
 rolls1Percentage
 rolls2Percentage
 rolls3Percentage
 rolls4Percentage
 rolls5Percentage
 rolls6Percentage
 
 #check
 sum(rolls1Percentage,
 rolls2Percentage,
 rolls3Percentage,
 rolls4Percentage,
 rolls5Percentage,
 rolls6Percentage)
 
 #b in the simulation, how many times was 3 rolled on an odd numbered roll
 #odd selector
 
 set.seed(1);
 diceRolls <- sample(1:6, size=10000, replace=TRUE)
 diceRolls
 diceRolls_even<-seq(from=2,to=length(diceRolls), by=2)
 diceRolls_odd<-seq(from=1,to=length(diceRolls), by=2)
 
 
 
 (diceRolls[diceRolls_odd])==3
 sum((diceRolls[diceRolls_odd])==3)
 #832
 
 #in the simulation, how many tiems was 1 rolled immediately followed by 2? for example, inthe sequence of rolls" 1,2,4,1,3,2,6,5,1,2?
 diceRolls <- sample(1:6, size=10000, replace=TRUE)
 diceRolls==c(1,2)
 
 
sum(diceRolls==c(1,2))

#1664 times

#check

diceRolls2= c(1,2,4,1,3,2,6,5,1,2)
diceRolls2
diceRolls2==c(1,2)


### using the match function on the diceRolls i attempted to zero out everying that wasnt a 1 or 0.. then I was looking for a way to eliminate those not in sequence of 1,2....following that I would use a sum function as i would create a data.frame from this vector yeilding the correct amount of times 1 followed by 2 occured
match(diceRolls,c(1,2),nomatch = 0,incomparables=NULL)
 
set.seed(1); 
diceRolls <- sample(1:6, size=10000, replace=TRUE)
head(diceRolls,20)









#Question 2
#a-How many sessions are in the data set?
mid<- read.csv("Desktop/Grad School/MSBX 5410-001/midterm1.csv")
head(mid)
summary(mid)

length(unique(mid$sessionID))
#23782

#b How many unique users are in the data set?
length(unique(mid$userID))
#2999

#c What % of the sessions resulted in a purchase? (if totalSpent=0, there was no purchase in that sessions?)


aggregate(mid$totalSpent~mid$userID,FUN=sum)


x<-data.frame(subset( mid, mid$totalSpent==0.00)) 
length(x$totalSpent)

#16227 out of original

16227/23782*100
#68.23% of the purchases had a total spent of zero; thus..

(1-16227/23782)*100
#31.76 % resulted in a purchase

#d for only the sessions that resulted in a purchase (totalspent>0, what was the avg mean and median amount spent?

x1<-data.frame(subset( mid, mid$totalSpent>0.00))
x1
length(x1$totalSpent)

mean(x1$totalSpent)
median(x1$totalSpent) 

#e How many times did user 3000 visit the website?

x2<-data.frame(subset( mid, mid$userID==3000))
length(x2$userID)
#8

#f What was the largest single transaction in the data(totaSpent) and which customer was responsible for this tranaction(userID)?


mid[mid$totalSpent==max(mid$totalSpent), ]

#appears userID 1160 out of Colorado had the most spent in a single transaction at "87.1"

#question 3


#What is the average mean and median length of a session on the website in minutes?




mid$removeMinutes <- sub( pattern="m ", replacement=".", x=mid$sessionTime )

mid$removeSeconds <- sub(pattern="s", replacement="", x=mid$removeMinutes )



mid$sTime <- as.numeric(mid$removeSeconds)


head(mid)
mean(mid$sTime)
median(mid$sTime)

#Question 4

#Within the data set, calc how much each custoemr userID spent in total. print the top 10 highest spending customers in order of amount spent( #1= most spent)
##below answers first part
data.frame(aggregate(mid$totalSpent~mid$userID,FUN=sum))

##below answers second part
head( mid[ order(mid$totalSpent, decreasing=TRUE), ], 10 )


#Question 5 (5 pts)

#Create a new column in the data called "pageViewsBinned". "PageViews Binned" should take the value of less than 50, 50 to 100, 100 to 150, or 150 or more depending on the corresponding values of page Views for the session. The lower value for each bin should be inclusive and the upper excluse so pagesViews   50 should in in the 50 to 100 bin and 100 should be in the 100 to 150 bin




mid$pageViewsBinned <- rep(NA, length.out=nrow(mid))

for(i in 1:length(mid$pageViews)) 
{
	{
	if(mid$pageViews[i] >= 150) 
	{
		mid$pageViewsBinned[i] <-1
	} 
	else if(mid$pageViews[i] >= 100)
	 {
	 	mid$pageViewsBinned[i] <-2
	 } 
	 else if(mid$pageViews[i] >= 50)
	  {
	  	mid$pageViewsBinned[i] <-3	
	 } 
	 else
	 {
	 	mid$pageViewsBinned[i] <-4
	 }
	 }
}

head(mid)

#above I coded a loop using categoricals 1,2,3,4.... 1="150 or more", 2="100 to 150", 3="50 to 100" & 4= "less than 50"


mid$pageViewsBinned


x5<-data.frame(subset( mid, mid$pageViewsBinned==1 ))
bin1<-length(x5$pageViewsBinned)
x6<-data.frame(subset( mid, mid$pageViewsBinned==2 ))
bin2<-length(x6$pageViewsBinned)
x7<-data.frame(subset( mid, mid$pageViewsBinned==3 ))
bin3<-length(x7$pageViewsBinned)
x8<-data.frame(subset( mid, mid$pageViewsBinned==4 ))
bin4<-length(x8$pageViewsBinned)
print(bin1)
print(bin2)
print(bin3)
print(bin4)

#Question 6
head(mid,20)
#a how many sessions happened form each type of system?

iphone<- data.frame(subset( mid, mid$browser=="safari" & mid$deviceType=="phone" ))
totaliphone<-length(iphone$sessionID)
totaliphone


mac<- data.frame(subset( mid, mid$browser=="safari" & mid$deviceType=="computer" ))
totalmac<-length(mac$sessionID)
totalmac

android<- data.frame(subset( mid, mid$browser=="chrome" & mid$deviceType=="phone" ))
totalandroid<-length(android$sessionID) 
totalandroid

PC<-data.frame(subset( mid, mid$browser=="chrome" & mid$deviceType=="computer" ) )
totalPC<-length(PC$sessionID)
totalPC
#b how much total revenue was gnerated from each type of system?

head(iphone$totalSpent,5)
sum(iphone$totalSpent)

head(mac$totalSpent,5)
sum(mac$totalSpent)

head(android$totalSpent,5)
sum(android$totalSpent)

head(PC$totalSpent,5)
sum(PC$totalSpent)

#C what percentage of sessions from each typ of system were made by user in california?

iphone_California<-data.frame(subset(iphone,iphone$location=="California"))



length(iphone_California$location)
4348/totaliphone #65.35% of those using iphones were made by user in California

##______________________

mac_California<-data.frame(subset(mac,mac$location=="California"))




length(mac_California$location)
2004/totalmac #65.94% of those using macs were made by user in California


##______________________

PC_California<-data.frame(subset(PC,PC$location=="California"))




length(PC_California$location)
7169/totalPC #67.68% of those using PCs were made by user in California


##______________________

android_California<-data.frame(subset(android,android$location=="California"))




length(android_California$location)
2342/totalandroid #66.95% of those using andriods were made by user in California













 
 
