

#===================================
# PART 1 solutions
#===================================


# load data
myData <- read.csv( "~/Desktop/practiceData.csv", stringsAsFactors=FALSE )


# Question 1 - unique cusomters
length( unique( myData$userID ) )


# Question 2 - total spent
myData$removeDollars <- sub( pattern=" dollars ", replacement=".", x=myData$transactionSpent )
myData$removeCents <- sub(pattern=" cents", replacement="", x=myData$removeDollars )
myData$tSpent <- as.numeric( myData$removeCents )
sum( myData$tSpent )


# Question 3 - number of transaction by category
aggregate( tSpent ~ category, FUN=length, data=myData )

# (these also work)
nrow( myDataFrame[ myDataFrame$category=="office supplies", ] )
nrow( myDataFrame[ myDataFrame$category=="food", ] )
#
nrow( subset( myDataFrame, category=="office supplies" ) )
nrow( subset( myDataFrame, category=="food" ) )
#
table( myDataFrame$category )


# Question 4 - total spent per category
aggregate( tSpent ~ category, FUN=sum, data=myData )


# Question 5 - largest single transaction per category
aggregate( tSpent ~ category, FUN=max, data=myData )


# Question 6 - create data set of total spent by each customer
byCustomer <- aggregate( tSpent ~ userID, FUN=sum, data=myData )
names(byCustomer)[2] <- "totalSpent"
byCustomer[ byCustomer$totalSpent==max(byCustomer$totalSpent), ]


# Question 7 - create "rank" column, with rank=1 for most spent, rank=2 for second most spent, etc.
byCustomer$spentRank <- rank(-byCustomer$totalSpent)


# Question 8 - print the 10 highest spending customers in order (from rank=1 to rank=10)
head( byCustomer[ order(byCustomer$totalSpent, decreasing=TRUE), ], 10 )



#===================================
# PART 2 solutions
#===================================


# Question 1 - loop that calculates the product of a vector

# set up a test vector (you should try several of these)
myVector <- 1:10

# initialize product to 1 (since 1 times anything equals that thing)
product <- 1

# loop over elements in myVector
for (i in myVector) {
	
	# during each iteration, multiply current product by new element
	product <- product * myVector[i]
	
}

# check answer
print( product )
prod( myVector )


# Question 2 - counting heads and tails

# simulate 100 coin flips
set.seed(1); coinflips <- sample(c("HEADS","TAILS"), size=100, replace=TRUE)

# initialize counting variables at zero
nHeads <- 0
nTails <- 0

# loop to count
for (i in 1:length(coinflips)) {
	if (coinflips[i] == "HEADS") {
		nHeads <- nHeads + 1
	} else {
		nTails <- nTails + 1
	}
}

# check solution
print( nHeads )
print( nTails )
table( coinflips )


# Question 3 - cumulative price of diamonds

# load ggplot2
require(ggplot2)

# save first 100 rows of diamonds data set as new data set
first100diamonds <- diamonds[1:100,]

# create "cumulativePrice" column
first100diamonds$cumulativePrice <- rep(NA, length.out=nrow(first100diamonds))

# for loop to calculate cumulative price
for (i in 1:nrow(first100diamonds)) {
	first100diamonds$cumulativePrice[i] <- sum(first100diamonds$price[1:i])
}

# look at data to see if it worked
head(first100diamonds)

