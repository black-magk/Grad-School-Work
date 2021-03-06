#SAVE "Movie Ratings 2017" as "movieratingnew.csv" on desktop



# Adjust code below to load path correctly




#### ENTER NAME
r.for<-"Aimee" 


rm(list=ls())

#In-class collaborative filtering demo


###LANDON NEW CODE ####

a<-file.choose()

a

#copy and paste file into the code below

movie.rating<-read.csv("/Users/landon/Desktop/movieratingnew.csv")

movie.rating.transpose<-t(movie.rating)


movie.rating.transpose[is.na(movie.rating.transpose)] <- 0


movie.rating.tranpose.1<-data.frame(movie.rating.transpose)

movie.rating.tranpose.1


ncol(movie.rating.transpose)
cor(movie.rating.transpose)
nrow(movie.rating.transpose)
### LANDON END CODE ####

#NAME SELECTION ##
r.for<-"X267" 



#The name of the customer for whom we want recommendations
num.recommendations<-2 #the number of recommended movies to return
num.neighbors<-5 #the number of nearest neighbors to use for the calculation


movies.correlation<-cor(data.matrix(movie.rating.tranpose.1[2:51,2:267]), use="pairwise.complete.obs") 




### LANDON END CODE ####


#you may get an error about Standard deviation = 0...
#This is telling you that for some customers there is no overlap in ratings, so correlation can't be computed and NA is entered.


#order matrix by correlation
nearest.neighbors<-movies.correlation[order(-movies.correlation[,r.for]),r.for,drop=FALSE]
nearest.neighbors<-as.data.frame(nearest.neighbors)

#Remove self from neighbors
nearest.neighbors<-nearest.neighbors[rownames(nearest.neighbors)!=r.for, , drop=FALSE]

#Remove NAs
nearest.neighbors<-nearest.neighbors[complete.cases(nearest.neighbors), , drop=FALSE]

#Get k nearest neighbors
nearest.neighbors<-nearest.neighbors[1:num.neighbors, ,drop=FALSE]


#Calculate weights -- weights are proportional to similarity
weights<-nearest.neighbors/sum(nearest.neighbors)
colnames(weights)<-c("weight")


#Get ratings for nearest neighbors
ratings.neighbors<-t(movie.ratings[,rownames(nearest.neighbors)])


#Use a for loop to calculate the weighted average for each movie, weighted by the weights calculated above
predicted.scores<-c()

for (i in 1:dim(ratings.neighbors)[2]){
  predicted.scores[i]<-weighted.mean(ratings.neighbors[,i], t(weights), na.rm=TRUE)
}

predicted.scores<-as.data.frame(predicted.scores)
rownames(predicted.scores)<-movie.ratings[,1]


# Cut down the predicted scores to only the movies that the customer has not seen and sort by score
predicted.scores.unseen<-predicted.scores[is.na(movie.ratings[,r.for]), , drop=FALSE]
predicted.scores.unseen<-predicted.scores.unseen[order(-predicted.scores.unseen), ,drop=FALSE]


#recommend the top n movies. 
recommendations<-predicted.scores.unseen[1:num.recommendations,,drop=FALSE]
recommendations
