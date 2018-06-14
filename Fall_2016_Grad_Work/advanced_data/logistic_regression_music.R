# logistic regression exercise
# Dan Zhang, 08-30-2014

rm(list=ls())
#setwd('/Users/dazh1150/Google Drive/teaching/Fall2015/Data Analytics/lab/docs')

setwd('C:/Users/dazh1150/Dropbox/Fall2016/Advanced Data Analytics/lab/docs')


# Q1
songs=read.csv('songs.csv')
#1(a): 373
sum(songs$year==2010)

#1(b): 18
sum(songs$artistname=='Michael Jackson')

#1(c): 
#songtitle Top10
#4329 You Rock My World     1
#6207 You Are Not Alone     1
#6210    Black or White     1
#6218 Remember the Time     1
#6915     In The Closet     1
songs[songs$artistname=='Michael Jackson'&songs$Top10==1,c('songtitle','Top10')]

#1(d): 0, 1, 3, 4, 5, 7
unique(songs$timesignature)

#1(e): 4
table(songs$timesignature)

#1(f): Wanna Be Startin' Somethin'
songs[which.max(songs$tempo),'songtitle']

# Q2: 7201
SongsTrain = subset(songs,year<=2009)
SongsTest = subset(songs,year>2009)
dim(SongsTrain)

# Q3: 4827.2
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10~.,data=SongsTrain,family=binomial)
summary(SongsLog1)

# Q4: B

# Q5: B

# Q6: A; no

# Q7: correlation between loudness and energy is 0.7399
# A
cor(SongsTrain$loudness,SongsTrain$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

# Q8: Yes
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Q9: (309+19)/373 = 0.8794
table(SongsTest$Top10,as.numeric(predict(SongsLog3,newdata=SongsTest,type='response')>0.45))

# Q10: 314/373 = 0.8418
table(SongsTest$Top10,rep(0,373))

# Q11
table(SongsTest$Top10,as.numeric(predict(SongsLog3,newdata=SongsTest,type='response')>0.45))
#0   1
#0 309   5
#1  40  19
# Hence: 19 songs are correctly predicted as Top 10 hits
# 5 non-hit songs are prediected as Top 10 hits
# Sensitivity = True negative rate = 19/59 = 0.3220
# Specificity = True positive rate = 309/314 = 0.9841
# A, D




























