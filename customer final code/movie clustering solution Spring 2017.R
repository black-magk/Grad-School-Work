rm(list = ls())

#setwd

file.choose()
all.raw<-read.csv("/Users/landon/Desktop/customer final code/movie viewing data for students binary.csv")

### PART 1 ###
head(all.clean)
all.clean<-all.raw[complete.cases(all.raw),] #casewise deletion of missing data
length(rownames(all.raw))-length(rownames(all.clean)) #43 cases removed
movies<-all.clean[,2:51] #create dataset with clustering variables only (i.e., all seen_xxx variables)
demos<-all.clean[,52:57] #create dataset with demographics only

#Get overall viewing proportions by movie
overall.proportions<-sort(colMeans(movies),decreasing=TRUE)  #create matrix with proportion of views for each movie
barplot(overall.proportions, names.arg=names(overall.proportions),las=2, ylim=0:1)
#Make the plot window wide enough and it will be readable
#las=2 makes label text perpendicular to axis

#Get by-person viewing proportions
sort(rowMeans(movies)) #no one watched 0 movies, 3 watched 2 movies, 1 watched all movies
mean(rowMeans(movies)) #Average person watched 38% of movies
median(rowMeans(movies))#Median person watched 36% of movies
hist(rowMeans(movies),breaks=10,xlab="Proportion of Movies Watched", main="Histogram of % Movies Watched")

### PART 2 ###

#Transform the data such that each variable has a mean of 0 and a standard deviation of 1
movies<-scale(movies,center=TRUE,scale=TRUE)
movies
# Determine initial number of clusters based on Within-cluster sum of squares
wss<-c()
max.clusters<-25
for (i in 1:max.clusters) wss[i] <- sum(kmeans(movies, centers=i)$withinss)
plot(1:25, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# No clear elbow, let's start with 4

### PART 3 ###

#Ordinary K-means
num.clusters<-4
set.seed (12345)

movies.k4<-kmeans(movies, num.clusters,nstart=50)


fitted(movies.k4, method = c("centers", "classes"),size)
k4.means<-t(aggregate(movies,by=list(movies.k4$cluster),FUN=mean)) #Create dataset with cluster means
k4.means<-k4.means[-c(1), ] 

summary(movies.k4)
colMeans(k4.means) 
movies.k4$size
summary(k4.means[order(-k4.means[,3]),]) #Cluster 3 has seen 72% of movies. These are heavy watchers. Small cluster.
nrow(k4.means[order(-k4.means[,2]),]) #Cluster 2 has seen 30% of movies, mostly fantasy/sci-fi
z<-k4.means[order(-k4.means[,1]),]
nrow(z)#Cluster 1 has seen 23% of movies, mostly drama/crime
k4.means[order(-k4.means[,4]),] #Cluster 4 has seen 49% of movies. Not seen many recent movies.

### PART 4 ###

demos$cluster1 <- ifelse(movies.k4$cluster==1,1,0)
demos$cluster2 <- ifelse(movies.k4$cluster==2,1,0)
demos$cluster3 <- ifelse(movies.k4$cluster==3,1,0)
demos$cluster4 <- ifelse(movies.k4$cluster==4,1,0)

#Summary statistics by cluster
aggregate(demos[,1:6],by=demos[,7:10],FUN=summary)

#Predicting cluster membership
logit1 <- glm(cluster1 ~ age + edu + income + gender + white.nonwhite,  data=demos, family=binomial(link = logit))
summary(logit1)
demos$cluster1prob <- predict(logit1, demos, type="response") #cluster 1 tends to be older




logit2 <- glm(cluster2 ~ age + edu + income + gender + white.nonwhite,  data=demos, family=binomial(link = logit))
summary(logit2)
demos$cluster2prob <- predict(logit2, demos, type="response") #cluster 2 tends to be younger, and female

logit3 <- glm(cluster3 ~ age + edu + income + gender + white.nonwhite,  data=demos, family=binomial(link = logit))
summary(logit3)
demos$cluster4prob <- predict(logit3, demos, type="response") #cluster 3 tends to have higher income

logit4 <- glm(cluster4 ~ age + edu + income + gender + white.nonwhite,  data=demos, family=binomial(link = logit))
summary(logit4)
demos$cluster4prob <- predict(logit4, demos, type="response") #cluster 4 tends to be male

### BONUS: SPHERICAL K-MEANS (COSINE DISTANCE) ###
library(skmeans)#if you don't have it installed use install.packages("skmeans")
library(cluster)
library(clue)

#How many clusters?
#Get within sum of squares for 1 cluster
remaining.disimilarity <- numeric()
#Add within sum of squares for other clusters
max.clusters<-25
for (i in 2:max.clusters) remaining.disimilarity[i-1] <- 1 - as.numeric(cl_validity(skmeans(as.matrix(movies),i))["Dissimilarity accounted for"])
plot(2:25, remaining.disimilarity, type="b", xlab="Number of Clusters", ylab="Remaining Disimilarity")
#try 4 clusters?

movies.sk4<-skmeans(as.matrix(movies),4)


tapply(movies.sk4$cluster,movies.sk4$cluster,length) 

sk4.means<-t(aggregate(movies,by=list(movies.sk4$cluster),mean))
sk4.means<-sk4.means[-c(1), ]              

colMeans(sk4.means) #Get % movies watched by cluster; more balanced than ordinary K-means

#RESULTS ARE UNSTABLE

sk4.means[order(-sk4.means[,1]),] #Cluster 1 has seen 25% of movies, mix of older and more recent movies at top
sk4.means[order(-sk4.means[,2]),] #Cluster 2 has seen 32% of movies, mostly fantasy/sci-fi
sk4.means[order(-sk4.means[,3]),] #Cluster 3 has seen 47% of movies, espcially classic older movies at the top 
sk4.means[order(-sk4.means[,4]),] #Cluster 4 has seen 28% of movies. Mostly drama/crime. Mostly older movies



