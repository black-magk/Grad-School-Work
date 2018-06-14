##############
###MAY NEED TO UPDATE JAVA
###LOAD PACKAGES

install.packages("XLConnect")
library(XLConnect)

install.packages("xlsx")
library(xlsx)



require(xlsx)






install.packages("openxlsx")
library(openxlsx)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("ggplot2")
library(ggplot2)



##################---------------------------------------

#Shortcut to call files

#Mac Users- place excel files on desktop------------------------------------

brands <- c("mtdew", "reeses", "shocktop", "snickers","official-channels" )
dataFiles <- paste0("~/Desktop/Carr_Unrein/",brands,".xlsx")


###############-------------------------------------------------------

require(XLConnect) 

#################REESES##################--------------------------------

#load entire workbook per brand----------------------------------------------
reeses<- XLConnect::loadWorkbook(dataFiles[2])


#loads all data into example: "mtdew_all" dataframe.--------------------------

reeses<- XLConnect::loadWorkbook(dataFiles[2])



#Label individual tabs based on date------------------------------------------
jan.11_reeses <- readWorksheet(reeses, 1) 

#call individual tabs at your leisure with cmd below ---------------------------
jan.11_reeses

jan.16_reeses <- readWorksheet(reeses, 2) 
jan.23_reeses<- readWorksheet(reeses, 3) 
jan.30_reeses <- readWorksheet(reeses, 4) 
feb.06_reeses<- readWorksheet(reeses, 5) 
feb.13_reeses <- readWorksheet(reeses, 6) 


#####################################MTDEW#######-------------------------------

mtdew<- XLConnect::loadWorkbook(dataFiles[1])


jan.11_mtdew <- readWorksheet(mtdew, 1) 

jan.11_mtdew
jan.16_mtdew <- readWorksheet(mtdew, 2) 
jan.23_mtdew<- readWorksheet(mtdew, 3) 
jan.30_mtdew <- readWorksheet(mtdew, 4) 
feb.06_mtdew<- readWorksheet(mtdew, 5) 
feb.13_mtdew <- readWorksheet(mtdew, 6) 



#############################SHOCKTOP###########---------------------------

shocktop<- XLConnect::loadWorkbook(dataFiles[3])


jan.11_shocktop<- readWorksheet(shocktop, 1) 
jan.16_shocktop<- readWorksheet(shocktop, 2) 
jan.23_shocktop<- readWorksheet(shocktop, 3) 
jan.30_shocktop <- readWorksheet(shocktop, 4) 
feb.06_shocktop<- readWorksheet(shocktop, 5) 
feb.13_shocktop <- readWorksheet(shocktop, 6) 


##################SNICKERS##############---------------------------------------

snickers <- read.xlsx(dataFiles[4])

jan.11_snickers <- read.xlsx(dataFiles[4], sheet = 1)
jan.16_snickers <- read.xlsx(dataFiles[4], sheet = 2)
jan.23_snickers <- read.xlsx(dataFiles[4], sheet = 3)
jan.30_snickers <- read.xlsx(dataFiles[4], sheet = 4)
feb.06_snickers <- read.xlsx(dataFiles[4], sheet = 5)
feb.13_snickers <- read.xlsx(dataFiles[4], sheet = 6)



#load official channels excel workbook----------------------------------------

officialchannels <- read.xlsx(dataFiles[5])

#Check it loaded properly-----------------------------------------------------
officialchannels


#PART 1 Did the views of official brand content rise after the Super Bowl?-----



#BELOW WE SUBSET THE SHEETS BY DATE; FILTERED TO ONLY INCLUDE THOSE WITH THE OFFICIAL CHANNEL----

#Date Vector--------------------------------------------------------------------------------------
observationDates <- (c("2016-01-11","2016-01-16","2016-01-23", "2016-01-30", "2016-02-06", "2016-02-13"))



#SNICKERS

#Below subsets the data based on the given primary channel-------------------------------------
jan.11_snickers_a<-subset( jan.11_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 
#view the subset------------------------------------------------------------------------
jan.11_snickers_a
jan.16_snickers_a<-subset( jan.16_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 
jan.23_snickers_a<-subset( jan.23_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 
jan.30_snickers_a<-subset( jan.30_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 
feb.06_snickers_a<-subset( feb.06_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 
feb.13_snickers_a<-subset( feb.13_snickers, channel_id== "UCDviI62w0VbD_9oRNkV1Uig" ) 


#REESES-------------------------------------------------------------------
jan.11_reeses_a<-subset( jan.11_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 
jan.16_reeses_a<-subset( jan.16_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 
jan.23_reeses_a<-subset( jan.23_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 
jan.30_reeses_a<-subset( jan.30_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 
feb.06_reeses_a<-subset( feb.06_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 
feb.13_reeses_a<-subset( feb.13_reeses, channel_id== "UCc9-kl38p_uzQDVrT06VTxA" ) 

f
#MTDEW-------------------------------------------------------------------
jan.11_mtdew_a<-subset( jan.11_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 
jan.16_mtdew_a<-subset( jan.16_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 
jan.23_mtdew_a<-subset( jan.23_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 
jan.30_mtdew_a<-subset( jan.30_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 
feb.06_mtdew_a<-subset( feb.06_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 
feb.13_mtdew_a<-subset( feb.13_mtdew, channel_id== "UCsdqpqgsSFTRSnyyqHVSgyw" ) 


#SHOCKTOP-------------------------------------------------------------------
jan.11_shocktop_a<-subset( jan.11_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 
jan.16_shocktop_a<-subset( jan.16_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 
jan.23_shocktop_a<-subset( jan.23_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 
jan.30_shocktop_a<-subset( jan.30_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 
feb.06_shocktop_a<-subset( feb.06_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 
feb.13_shocktop_a<-subset( feb.13_shocktop, channel_id== "UCpfUlFZlM1yfcJG5dl9fwyg" ) 


#######################GRAPHS###############################-------------------------------------------------------------------

####SHOCKTOP#####-------------------------------------------------------------------

#Sum's the views of shocktop from your newly formed subset containing only those with primary channels


a<-sum(jan.11_shocktop_a$views)
b<-sum(jan.16_shocktop_a$views)
c<-sum(jan.23_shocktop_a$views)
d<-sum(jan.30_shocktop_a$views)
e<-sum(feb.06_shocktop_a$views)
f<-sum(feb.13_shocktop_a$views)


#put values intro vector-------------------------------------------------------------------
shocktop_views_string<-c(a,b,c,d,e,f)

#Create seperate data.frame that combines the observation dates with corresponding view count-------------------------------------------------------------------

shock_frame<-data.frame(cbind(observationDates,shocktop_views_string))
shock_frame$views1000s<-shocktop_views_string/1000

shock_frame

#sets graph equal too "ggg" using ggplot framework-------------------------------------------------------------------
ggg<-ggplot(data=shock_frame, aes(x=observationDates, y=views1000s, fill=observationDates))+geom_bar(stat="identity") + scale_fill_brewer(palette="Spectral")+coord_flip() +ggtitle("Official Shock Top Views")+ xlab("Dates")+ylab("View Count ( in Thousands)")+theme_bw()


#view graph-------------------------------------------------------------------
ggg

####MT DEW##### SAME STEPS AS ABOVE...REPEAT-------------------------------------------------------------------

g<-sum(jan.11_mtdew_a$views)
h<-sum(jan.16_mtdew_a$views)
i<-sum(jan.23_mtdew_a$views)
j<-sum(jan.30_mtdew_a$views)
k<-sum(feb.06_mtdew_a$views)
l<-sum(feb.13_mtdew_a$views)

mtdew_views_string<-c(g,h,i,j,k,l)

dew_frame<-data.frame(cbind(observationDates,mtdew_views_string))
dew_frame$views1000s<-mtdew_views_string/1000

dew_frame


hhh<-ggplot(data=dew_frame, aes(x=observationDates, y=views1000s, fill=observationDates))+geom_bar(stat="identity") + scale_fill_brewer(palette="Spectral")+coord_flip() +ggtitle("Official Mt Dew Views")+ xlab("Dates")+ylab("View Count ( in Thousands)")+theme_bw()

hhh


#####REEESES#####-------------------------------------------------------------------


m<-sum(jan.11_reeses_a$views)
n<-sum(jan.16_reeses_a$views)
o<-sum(jan.23_reeses_a$views)
p<-sum(jan.30_reeses_a$views)
q<-sum(feb.06_reeses_a$views)
r<-sum(feb.13_reeses_a$views)

reeses_views_string<-c(m,n,o,p,q,r)

reeses_frame<-data.frame(cbind(observationDates,reeses_views_string))
reeses_frame$views1000s<-reeses_views_string/1000

reeses_frame


iii<-ggplot(data=reeses_frame, aes(x=observationDates, y=views1000s, fill=observationDates))+geom_bar(stat="identity") + scale_fill_brewer(palette="Spectral")+coord_flip() +ggtitle("Official Reeses Views")+ xlab("Dates")+ylab("View Count ( in Thousands)")+theme_bw()

iii



###SNICKERS####-------------------------------------------------------------------

s<-sum(jan.11_snickers_a$views)
t<-sum(jan.16_snickers_a$views)
u<-sum(jan.23_snickers_a$views)
v<-sum(jan.30_snickers_a$views)
w<-sum(feb.06_snickers_a$views)
x<-sum(feb.13_snickers_a$views)

snickers_views_string<-c(s,t,u,v,w,x)

snickers_frame<-data.frame(cbind(observationDates,snickers_views_string))
snickers_frame$views1000s<-snickers_views_string/1000

shock_frame


jjj<-ggplot(data=snickers_frame, aes(x=observationDates, y=views1000s, fill=observationDates))+geom_bar(stat="identity") + scale_fill_brewer(palette="Spectral")+coord_flip() +ggtitle("Official Snickers Views")+ xlab("Dates")+ylab("View Count ( in Thousands)")+theme_bw()


