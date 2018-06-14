
##\\ Below reads in a csv file
practiceData<- read.csv("Desktop/Grad School/MSBX 5410-001/practiceData.csv")


summary(practiceData)

##\\displays 6 first responses
head(practiceData)


##\\ class defines the type of data


class(practiceData)

##\\ displays type of variables in the dataframe
str(practiceData)

#########How many unique customers are in the data set?


## \\unique() finds unique numbers in a column or given selection

length(unique(practiceData$userID))



#########What is the total amount spent in the data set?


##\\creates a new column in dataframe with "dollars taken off"

practiceData$removeDollars <- sub( pattern=" dollars ", replacement=".", x=practiceData$transactionSpent )

##\\An addtionall column is created but notice it references the dollars column that has the "cents" to take it off

practiceData$removeCents <- sub(pattern=" cents", replacement="", x=practiceData$removeDollars )

##finally the new column is converted to a numeric and generates a new column that lists numbers that can use the sum function
practiceData$tSpent <- as.numeric(practiceData$removeCents )


sum( practiceData$tSpent )
mean(practiceData$tSpent )

summary(practiceData$tSpent )

head(practiceData)


###########How many transactions were there in each category?

?aggregate


#\\sums the data in column 1 within column 2

###aggregate(column 1~column 2,FUN=sum)


aggregate(practiceData$tSpent~practiceData$category,FUN=sum)

#\\counts with lengt() the data in column 1 within column 2

###aggregate(df$column 1~df$column 2,FUN=length)

##make sure to reference back to the dataframe



aggregate(practiceData$tSpent~practiceData$category,FUN=length)



##What was the total amount spent in each category?
aggregate(practiceData$tSpent~practiceData$category,FUN=sum
##What was the largest single transaction in each category?

aggregate(practiceData$tSpent~practiceData$category,FUN=max)


total_customer_spend<- aggregate(practiceData$tSpent~practiceData$userID,FUN=order)

head(total_customer_spend)
head(total_customer_spend, 3)

practiceData$userID<-userID
practiceData$tSpent<-totalSpent

head(practiceData)

##6) Create a new data frame that has the total amount spent per customer. This data frame should have two columns called: “userID” and “totalSpent” (you might need to rename them). It should have one row for each customer in the data set. Which customer spent the most money? 

##\\below creates a new dataframe from the existing data frame

new_data_frame<- data.frame(practiceData$userID,practiceData$tSpent)

head(new_data_frame)
##\\renaming column headers

names(new_data_frame)[1] <-"userID"
names(new_data_frame)[2] <-"totalSpent"
head(new_data_frame)

max(new_data_frame$totalSpent)

##\\locates the row where in this case totalSpent is Max'd out

new_data_frame[new_data_frame$totalSpent==max(new_data_frame$totalSpent), ]






##7) Create a new column in your new data frame called “spentRank” that ranks the customers by how much they spent in total. The person who spent the most should get the rank “1”, the person who spent the second most should get the rank “2”, etc. (If there are ties break them however you want.) 

##\\ generating new column based on prexisting column and adding it to your new dataframe(df)


new_data_frame$spentRank<- rank(new_data_frame$totalSpent)

##\\indexing with subset command subset(df,"column in df"== "value")

subset( new_data_frame, spentRank=="500" ) 
 


#########8) Print the rows in the data frame representing the top 10 highest spending customers (in order: rank=1 to rank=10).


##\\ displays a short list in the df where you've ordered a specific column...flip FALSE to TRUE to get descending values

head( new_data_frame[ order(new_data_frame$spentRank, decreasing=FALSE), ], 10 )


####___________________________________________________
##1) Write a loop that calculates the product of “myVector” (in other words, multiplies everything in myVector together). It should be able to handle a loop of any length. 

myVector<-1:4


final_prod <- 1;
for (i in 1:length(myVector)){
   final_prod <- final_prod * myVector[i];
}

print(final_prod)

##2) Use the following code to simulate 100 coin flips (the “set.seed” part will make sure everyone generates the same random samples): set.seed(1); coinflips <- sample(c("HEADS","TAILS"), size=100, replace=TRUE) Use a for loop to count the number of heads and tails in the vector “coinflips”. 


set.seed(1); 
coinflips <- sample(c("HEADS","TAILS"), size=100, replace=TRUE)
head_count<-0
tail_count<-0

for (i in 1:length(coinflips))
{ 
	{
	if(coinflips[i]=="HEADS")
	{
	head_count<-head_count+1
	}
	else {coinflips[i]=="TAILS"}
	}
}

print(head_count)
print(tail_count)
	
