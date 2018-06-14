tix<-read.csv("Desktop/tix.csv") 

install.packages("stringr")

library(stringr)

tix.frame<-data.frame(tix)

head(tix.frame)

tix.frame$event<- (str_extract(tix.frame$event_name, "[aA-zZ]+"))

(tix.frame$event)


tix.frame$date_code <- as.numeric(str_extract(tix.frame$event_name, "[0-9]+"))

unique(tix.frame$date_code)





my.data <- c("aaa", "b11", "b21", "b101", "b111", "ccc1", "ddd1", "ccc20", "ddd13")
#
# extract numbers only
my.data.num <- as.numeric(str_extract(my.data, "[0-9]+"))
#
# check output
my.data.num
[1]  NA  11  21 101 111   1   1  20  13
#
# extract characters only
my.data.cha <- (str_extract(my.data, "[aA-zZ]+"))
# 
# check output
my.data.cha
[1] "aaa" "b"   "b"   "b"   "b"   "ccc" "ddd" "ccc" "ddd"
