 Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")  
  
 install.packages(Needed, dependencies=TRUE) 
 
 install.packages("slam")
 
 library(slam)
   
   
 install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")


cname <- file.path("~", "Desktop", "texts")   
cname 

dir(cname) 


library(tm)  

 
docs <- Corpus(DirSource(cname))   

summary(docs)   



# removes punctuation
docs <- tm_map(docs, removePunctuation)

#remove numbers
docs <- tm_map(docs, removeNumbers) 
#converting to lowercase

docs <- tm_map(docs, tolower) 

#remove stopper words
docs <- tm_map(docs, removeWords, stopwords("english")) 

#stemming words
  library(SnowballC)   
docs <- tm_map(docs, stemDocument) 


#white space removal

docs <- tm_map(docs, stripWhitespace)  



Finished preprocessing
docs <- tm_map(docs, PlainTextDocument)  


dtmImproved<-DocumentTermAmtrix(docs,control=list(wordlengths=c(3,15),bounds = list(global =c(10,Inf))))






