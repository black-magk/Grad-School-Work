# cluster analysis exercise with airline data
# Dan Zhang, 09-02-2014

rm(list=ls())
setwd('C:/Users/dazh1150/Google Drive/teaching/Fall2015/Data Analytics/lab/docs')

# Q1
# 1(a): BonusTrans, FlightTrans
# 1(b): Balance, BonusMiles
airlines = read.csv('AirlinesCluster.csv')
summary(airlines)

# Q2: C

# Q3
# 3(a): FlightMiles
# 3(b): DaysSinceEnroll
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)
apply(airlinesNorm,2,sd)

# Q4
# 4(a): 2
distance = dist(airlinesNorm, method = 'euclidean')
airlinesCluster = hclust(distance,method='ward.D')
plot(airlinesCluster)

# 4(b): 776
clusterGroups = cutree(airlinesCluster,k=5) 
sum(clusterGroups==1)

# 4(c): DaysSinceEnroll
n = length(airlines)
a = matrix(0,n,5)

for (i in 1:length(airlines)){
  a[i,] = tapply(airlines[[i]], clusterGroups, mean)
}
print(a)
apply(a,1,which.max)


# 4(d): B

# 4(e): QualMiles, FlightMiles, FlightTrans

# 4(f): D

# 4(g): Balance, BonusMiles, BonusTrans

# 4(h): (C)

# 4(i): none

# 4(j): E

# 4(k): none

# 4(l): A

# Q5
set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
str(KMC)

n = length(airlines)
a = matrix(0,n,5)
for (i in 1:length(airlines)){
  a[i,] = tapply(airlines[[i]], KMC$cluster, mean)
}
print(a)
apply(a,1,which.max)

# 5(a): 2

# 5(b): C

