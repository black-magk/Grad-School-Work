##############
###MAY NEED TO UPDATE JAVA
###LOAD PACKAGES

install.packages("XLConnect")
library(XLConnect)

install.packages("xlsx")
library(xlsx)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages('openxlsx')
library("openxlsx")


##################

#Shortcut to call files

#Mac Users- place excel files on desktop

brands <- c("mtdew", "reeses", "shocktop", "snickers","official-channels" )
dataFiles <- paste0("~/Desktop/Zefr_R_project/",brands,".xlsx")




###############

require(XLConnect) 

#################REESES##################

#load entire workbook per brand
reeses<- XLConnect::loadWorkbook(dataFiles[2])




#loads all data into example: "mtdew_all" dataframe.

reeses<- XLConnect::loadWorkbook(dataFiles[2])



#Label individual tabs based on date
jan.11_reeses <- readWorksheet(reeses, 1) 

#call individual tabs at your leisure with cmd below 
jan.11_reeses

jan.16_reeses <- readWorksheet(reeses, 2) 
jan.23_reeses<- readWorksheet(reeses, 3) 
jan.30_reeses <- readWorksheet(reeses, 4) 
feb.06_reeses<- readWorksheet(reeses, 5) 
feb.13_reeses <- readWorksheet(reeses, 6) 


#####################################MTDEW#######

mtdew<- XLConnect::loadWorkbook(dataFiles[1])


jan.11_mtdew <- readWorksheet(mtdew, 1) 
jan.16_mtdew <- readWorksheet(mtdew, 2) 
jan.23_mtdew<- readWorksheet(mtdew, 3) 
jan.30_mtdew <- readWorksheet(mtdew, 4) 
feb.06_mtdew<- readWorksheet(mtdew, 5) 
feb.13_mtdew <- readWorksheet(mtdew, 6) 



#############################SHOCKTOP###########

shocktop<- XLConnect::loadWorkbook(dataFiles[3])


jan.11_shocktop<- readWorksheet(shocktop, 1) 
jan.16_shocktop<- readWorksheet(shocktop, 2) 
jan.23_shocktop<- readWorksheet(shocktop, 3) 
jan.30_shocktop <- readWorksheet(shocktop, 4) 
feb.06_shocktop<- readWorksheet(shocktop, 5) 
feb.13_shocktop <- readWorksheet(shocktop, 6) 


##################SNICKERS##############


jan.11_snickers <- read.xlsx(dataFiles[4], sheet = 1, startRow = 1, colNames = TRUE)
jan.16_snickers <- read.xlsx(dataFiles[4], sheet = 2, startRow = 1, colNames = TRUE)
jan.23_snickers <- read.xlsx(dataFiles[4], sheet = 3, startRow = 1, colNames = TRUE)
jan.30_snickers <- read.xlsx(dataFiles[4], sheet = 4, startRow = 1, colNames = TRUE)
feb.06_snickers <- read.xlsx(dataFiles[4], sheet = 5, startRow = 1, colNames = TRUE)
feb.13_snickers <- read.xlsx(dataFiles[4], sheet = 6, startRow = 1, colNames = TRUE)


## Split Up Official and User Based Videos

##Mountain Dew

mtdewofficial11 <- subset(jan.11_mtdew, OffBin1 == 1)
mtdewofficial16 <- subset(jan.16_mtdew, OffBin1 == 1)
mtdewofficial23 <- subset(jan.23_mtdew, OffBin1 == 1)
mtdewofficial30 <- subset(jan.30_mtdew, OffBin1 == 1)
mtdewofficial6 <- subset(feb.06_mtdew, OffBin1 == 1)
mtdewofficial13 <- subset(feb.13_mtdew, OffBin1 == 1)

mtdewuser11 <- subset(jan.11_mtdew, OffBin1 == 0)
mtdewuser16 <- subset(jan.16_mtdew, OffBin1 == 0)
mtdewuser23 <- subset(jan.23_mtdew, OffBin1 == 0)
mtdewuser30 <- subset(jan.30_mtdew, OffBin1 == 0)
mtdewuser6 <- subset(feb.06_mtdew, OffBin1 == 0)
mtdewuser13 <- subset(feb.13_mtdew, OffBin1 == 0)

## Reeses

reesesofficial11 <- subset(jan.11_reeses, OffBin1 == 1)
reesesofficial16 <- subset(jan.16_reeses, OffBin1 == 1)
reesesofficial23 <- subset(jan.23_reeses, OffBin1 == 1)
reesesofficial30 <- subset(jan.30_reeses, OffBin1 == 1)
reesesofficial6 <- subset(feb.06_reeses, OffBin1 == 1)
reesesofficial13 <- subset(feb.13_reeses, OffBin1 == 1)

reesesuser11 <- subset(jan.11_reeses, OffBin1 == 0)
reesesuser16 <- subset(jan.16_reeses, OffBin1 == 0)
reesesuser23 <- subset(jan.23_reeses, OffBin1 == 0)
reesesuser30 <- subset(jan.30_reeses, OffBin1 == 0)
reesesuser6 <- subset(feb.06_reeses, OffBin1 == 0)
reesesuser13 <- subset(feb.13_reeses, OffBin1 == 0)

## Shocktop

shocktopofficial11 <- subset(jan.11_shocktop, OffBin.1 == 1)
shocktopofficial16 <- subset(jan.16_shocktop, OffBin.1 == 1)
shocktopofficial23 <- subset(jan.23_shocktop, OffBin1 == 1)
shocktopofficial30 <- subset(jan.30_shocktop, OffBin.1 == 1)
shocktopofficial6 <- subset(feb.06_shocktop, OffBin1 == 1)
shocktopofficial13 <- subset(feb.13_shocktop, OffBin1 == 1)

shocktopuser11 <- subset(jan.11_shocktop, OffBin.1 == 0)
shocktopuser16 <- subset(jan.16_shocktop, OffBin.1 == 0)
shocktopuser23 <- subset(jan.23_shocktop, OffBin1 == 0)
shocktopuser30 <- subset(jan.30_shocktop, OffBin.1 == 0)
shocktopuser6 <- subset(feb.06_shocktop, OffBin1 == 0)
shocktopuser13 <- subset(feb.13_shocktop, OffBin1 == 0)

## Snickers

snickersofficial11 <- subset(jan.11_snickers, OffBin1 == 1)
snickersofficial16 <- subset(jan.16_snickers, OffBin1 == 1)
snickersofficial23 <- subset(jan.23_snickers, OffBin1 == 1)
snickersofficial30 <- subset(jan.30_snickers, OffBin1 == 1)
snickersofficial6 <- subset(feb.06_snickers, OffBin1 == 1)
snickersofficial13 <- subset(feb.13_snickers, OffBin1 == 1)

snickersuser11 <- subset(jan.11_snickers, OffBin1 == 0)
snickersuser16 <- subset(jan.16_snickers, OffBin1 == 0)
snickersuser23 <- subset(jan.23_snickers, OffBin1 == 0)
snickersuser30 <- subset(jan.30_snickers, OffBin1 == 0)
snickersuser6 <- subset(feb.06_snickers, OffBin1 == 0)
snickersuser13 <- subset(feb.13_snickers, OffBin1 == 0)

##SUM THE ENGAGEMENTS

## Mountain Dew

mtdewuserengagementsum <- sum(mtdewuser6$Engagement, mtdewuser13$Engagement, mtdewuser30$Engagement, mtdewuser23$Engagement, mtdewuser16$Engagement, mtdewuser11$Engagement)
mtdewofficialengagementsum <- sum(mtdewofficial6$Engagement, mtdewofficial13$Engagement, mtdewofficial30$Engagement, mtdewofficial23$Engagement, mtdewofficial16$Engagement, mtdewofficial11$Engagement)

## Reeses

reesesuserengagementsum <- sum(reesesuser6$Engagement, reesesuser13$Engagement, reesesuser30$Engagement, reesesuser23$Engagement, reesesuser16$Engagement, reesesuser11$Engagement)
reesesofficialengagementsum <- sum(reesesofficial6$Engagement, reesesofficial13$Engagement, reesesofficial30$Engagement, reesesofficial23$Engagement, reesesofficial16$Engagement, reesesofficial11$Engagement)

## Shocktop

shocktopuserengagementsum <- sum(shocktopuser6$Engagement, shocktopuser13$Engagement, shocktopuser30$Engagement, shocktopuser23$Engagement, shocktopuser16$Engagement, shocktopuser11$Engagement)
shocktopofficialengagementsum <- sum(shocktopofficial6$Engagement, shocktopofficial13$Engagement, shocktopofficial30$Engagement, shocktopofficial23$Engagement, shocktopofficial16$Engagement, shocktopofficial11$Engagement)

## Snickers

snickersuserengagementsum <- sum(snickersuser6$Engagement, snickersuser13$Engagement, snickersuser30$Engagement, snickersuser23$Engagement, snickersuser16$Engagement, snickersuser11$Engagement)
snickersofficialengagementsum <- sum(snickersofficial6$Engagement, snickersofficial13$Engagement, snickersofficial30$Engagement, snickersofficial23$Engagement, snickersofficial16$Engagement, snickersofficial11$Engagement)

## ALL WEEKS BEFORE THE SUPERBOWL

## Mountain Dew

mtdewuserengagementtotalpresuperbowl <- sum(mtdewuser6$Engagement, mtdewuser30$Engagement, mtdewuser23$Engagement, mtdewuser16$Engagement, mtdewuser11$Engagement)
mtdewofficialengagementtotalpresuperbowl <- sum(mtdewofficial6$Engagement, mtdewofficial30$Engagement, mtdewofficial23$Engagement, mtdewofficial16$Engagement, mtdewofficial11$Engagement)

## Reeses

reesesuserengagementtotalpresuperbowl <- sum(reesesuser6$Engagement, reesesuser30$Engagement, reesesuser23$Engagement, reesesuser16$Engagement, reesesuser11$Engagement)
reesesofficialengagementtotalpresuperbowl <- sum(reesesofficial6$Engagement, reesesofficial30$Engagement, reesesofficial23$Engagement, reesesofficial16$Engagement, reesesofficial11$Engagement)

## Shocktop

shocktopuserengagementtotalpresuperbowl <- sum(shocktopuser6$Engagement, shocktopuser30$Engagement, shocktopuser23$Engagement, shocktopuser16$Engagement, shocktopuser11$Engagement)
shocktopofficialengagementtotalpresuperbowl <- sum(shocktopofficial6$Engagement, shocktopofficial30$Engagement, shocktopofficial23$Engagement, shocktopofficial16$Engagement, shocktopofficial11$Engagement)

## Snickers

snickersuserengagementtotalpresuperbowl <- sum(snickersuser6$Engagement, snickersuser30$Engagement, snickersuser23$Engagement, snickersuser16$Engagement, snickersuser11$Engagement)
snickersofficialengagementtotalpresuperbowl <- sum(snickersofficial6$Engagement, snickersofficial30$Engagement, snickersofficial23$Engagement, snickersofficial16$Engagement, snickersofficial11$Engagement)

## 1 WEEK BEFORE THE SUPERBOWL

## Mountain Dew

mtdewuserengagementtotal1wkpresuperbowl <- sum(mtdewuser6$Engagement)
mtdewofficialengagementtotal1wkpresuperbowl <- sum(mtdewofficial6$Engagement)

## Reeses

reesesuserengagementtotal1wkpresuperbowl <- sum(reesesuser6$Engagement)
reesesofficialengagementtotal1wkpresuperbowl <- sum(reesesofficial6$Engagement)

## Shocktop

shocktopuserengagementtotal1wkpresuperbowl <- sum(shocktopuser6$Engagement)
shocktopofficialengagementtotal1wkpresuperbowl <- sum(shocktopofficial6$Engagement)

## Snickers

snickersuserengagementtotal1wkpresuperbowl <- sum(snickersuser6$Engagement)
snickersofficialengagementtotal1wkpresuperbowl <- sum(snickersofficial6$Engagement)

## AFTER THE SUPERBOWL

## Mountain Dew

mtdewuserengagementtotalpostsuperbowl <- sum(mtdewuser13$Engagement)
mtdewofficialengagementtotalpostsuperbowl <- sum(mtdewofficial13$Engagement)

## Reeses

reesesuserengagementtotalpostsuperbowl <- sum(reesesuser13$Engagement)
reesesofficialengagementtotalpostsuperbowl <- sum(reesesofficial13$Engagement)

## Shocktop

shocktopuserengagementtotalpostsuperbowl <- sum(shocktopuser13$Engagement)
shocktopofficialengagementtotalpostsuperbowl <- sum(shocktopofficial13$Engagement)

## Snickers

snickersuserengagementtotalpostsuperbowl <- sum(snickersuser13$Engagement)
snickersofficialengagementtotalpostsuperbowl <- sum(snickersofficial13$Engagement)

##SUM THE VIEWS

## Mountain Dew

mtdewuserviewsum <- sum(mtdewuser6$views, mtdewuser13$views, mtdewuser30$views, mtdewuser23$views, mtdewuser16$views, mtdewuser11$views)
mtdewofficialviewsum <- sum(mtdewofficial6$views, mtdewofficial13$views, mtdewofficial30$views, mtdewofficial23$views, mtdewofficial16$views, mtdewofficial11$views)

## Reeses

reesesuserviewsum <- sum(reesesuser6$views, reesesuser13$views, reesesuser30$views, reesesuser23$views, reesesuser16$views, reesesuser11$views)
reesesofficialviewsum <- sum(reesesofficial6$views, reesesofficial13$views, reesesofficial30$views, reesesofficial23$views, reesesofficial16$views, reesesofficial11$views)

## Shocktop

shocktopuserviewsum <- sum(shocktopuser6$views, shocktopuser13$views, shocktopuser30$views, shocktopuser23$views, shocktopuser16$views, shocktopuser11$views)
shocktopofficialviewsum <- sum(shocktopofficial6$views, shocktopofficial13$views, shocktopofficial30$views, shocktopofficial23$views, shocktopofficial16$views, shocktopofficial11$views)

## Snickers

snickersuserviewsum <- sum(snickersuser6$views, snickersuser13$views, snickersuser30$views, snickersuser23$views, snickersuser16$views, snickersuser11$views, na.rm=TRUE)
snickersofficialviewsum <- sum(snickersofficial6$views, snickersofficial13$views, snickersofficial30$views, snickersofficial23$views, snickersofficial16$views, snickersofficial11$views, na.rm=TRUE)

## ALL WEEKS BEFORE THE SUPERBOWL

## Mountain Dew

mtdewuserviewtotalpresuperbowl <- sum(mtdewuser6$views, mtdewuser30$views, mtdewuser23$views, mtdewuser16$views, mtdewuser11$views)
mtdewofficialviewtotalpresuperbowl <- sum(mtdewofficial6$views, mtdewofficial30$views, mtdewofficial23$views, mtdewofficial16$views, mtdewofficial11$views)

## Reeses

reesesuserviewtotalpresuperbowl <- sum(reesesuser6$views, reesesuser30$views, reesesuser23$views, reesesuser16$views, reesesuser11$views)
reesesofficialviewtotalpresuperbowl <- sum(reesesofficial6$views, reesesofficial30$views, reesesofficial23$views, reesesofficial16$views, reesesofficial11$views)

## Shocktop

shocktopuserviewtotalpresuperbowl <- sum(shocktopuser6$views, shocktopuser30$views, shocktopuser23$views, shocktopuser16$views, shocktopuser11$views)
shocktopofficialviewtotalpresuperbowl <- sum(shocktopofficial6$views, shocktopofficial30$views, shocktopofficial23$views, shocktopofficial16$views, shocktopofficial11$views)

## Snickers

snickersuserviewtotalpresuperbowl <- sum(snickersuser6$views, snickersuser30$views, snickersuser23$views, snickersuser16$views, snickersuser11$views, na.rm=TRUE)
snickersofficialviewtotalpresuperbowl <- sum(snickersofficial6$views, snickersofficial30$views, snickersofficial23$views, snickersofficial16$views, snickersofficial11$views, na.rm=TRUE)

## 1 WEEK BEFORE THE SUPERBOWL

## Mountain Dew

mtdewuserviewtotal1wkpresuperbowl <- sum(mtdewuser6$views)
mtdewofficialviewtotal1wkpresuperbowl <- sum(mtdewofficial6$views)

## Reeses

reesesuserviewtotal1wkpresuperbowl <- sum(reesesuser6$views)
reesesofficialviewtotal1wkpresuperbowl <- sum(reesesofficial6$views)

## Shocktop

shocktopuserviewtotal1wkpresuperbowl <- sum(shocktopuser6$views)
shocktopofficialviewtotal1wkpresuperbowl <- sum(shocktopofficial6$views)

## Snickers

snickersuserviewtotal1wkpresuperbowl <- sum(snickersuser6$views, na.rm=TRUE)
snickersofficialviewtotal1wkpresuperbowl <- sum(snickersofficial6$views, na.rm=TRUE)

## AFTER THE SUPERBOWL

## Mountain Dew

mtdewuserviewtotalpostsuperbowl <- sum(mtdewuser13$views)
mtdewofficialviewtotalpostsuperbowl <- sum(mtdewofficial13$views)

## Reeses

reesesuserviewtotalpostsuperbowl <- sum(reesesuser13$views)
reesesofficialviewtotalpostsuperbowl <- sum(reesesofficial13$views)

## Shocktop

shocktopuserviewtotalpostsuperbowl <- sum(shocktopuser13$views)
shocktopofficialviewtotalpostsuperbowl <- sum(shocktopofficial13$views)

## Snickers

snickersuserviewtotalpostsuperbowl <- sum(snickersuser13$views, na.rm=TRUE)
snickersofficialviewtotalpostsuperbowl <- sum(snickersofficial13$views, na.rm=TRUE)







## MOUNTAIN DEW GRAPHS



mtdewuserviewtotal11presuperbowl <- sum(mtdewuser11$views)
mtdewofficialviewtotal11presuperbowl <- sum(mtdewofficial11$views)
mtdewuserviewtotal16presuperbowl <- sum(mtdewuser16$views)
mtdewofficialviewtotal16presuperbowl <- sum(mtdewofficial16$views)
mtdewuserviewtotal23presuperbowl <- sum(mtdewuser23$views)
mtdewofficialviewtotal23presuperbowl <- sum(mtdewofficial23$views)
mtdewuserviewtotal30presuperbowl <- sum(mtdewuser30$views)
mtdewofficialviewtotal30presuperbowl <- sum(mtdewofficial30$views)

mtdewuserengagementtotal11presuperbowl <- sum(mtdewuser11$Engagement)
mtdewofficialengagementtotal11presuperbowl <- sum(mtdewofficial11$Engagement)
mtdewuserengagementtotal16presuperbowl <- sum(mtdewuser16$Engagement)
mtdewofficialengagementtotal16presuperbowl <- sum(mtdewofficial16$Engagement)
mtdewuserengagementtotal23presuperbowl <- sum(mtdewuser23$Engagement)
mtdewofficialengagementtotal23presuperbowl <- sum(mtdewofficial23$Engagement)
mtdewuserengagementtotal30presuperbowl <- sum(mtdewuser30$Engagement)
mtdewofficialengagementtotal30presuperbowl <- sum(mtdewofficial30$Engagement)

mdu1 <- mtdewuserengagementtotal11presuperbowl/mtdewuserviewtotal11presuperbowl
mdu2 <- mtdewuserengagementtotal16presuperbowl/mtdewuserviewtotal16presuperbowl
mdu3 <- mtdewuserengagementtotal23presuperbowl/mtdewuserviewtotal23presuperbowl
mdu4 <- mtdewuserengagementtotal30presuperbowl/mtdewuserviewtotal30presuperbowl
mdu5 <- mtdewuserengagementtotal1wkpresuperbowl/mtdewuserviewtotal1wkpresuperbowl
mdu6 <- mtdewuserengagementtotalpostsuperbowl/mtdewuserviewtotalpostsuperbowl

mdo1 <- mtdewofficialengagementtotal11presuperbowl/mtdewofficialviewtotal11presuperbowl
mdo2 <- mtdewofficialengagementtotal16presuperbowl/mtdewofficialviewtotal16presuperbowl
mdo3 <- mtdewofficialengagementtotal23presuperbowl/mtdewofficialviewtotal23presuperbowl
mdo4 <- mtdewofficialengagementtotal30presuperbowl/mtdewofficialviewtotal30presuperbowl
mdo5 <- mtdewofficialengagementtotal1wkpresuperbowl/mtdewofficialviewtotal1wkpresuperbowl
mdo6 <- mtdewofficialengagementtotalpostsuperbowl/mtdewofficialviewtotalpostsuperbowl

postdate <- c('1/11/16', '1/11/16', '1/16/16', '1/16/16', '1/23/16', '1/23/16', '1/30/16', '1/30/16', '2/06/16', '2/06/16', '2/13/16', '2/13/16')
engagement <- c(mdo1*100, mdu1*100, mdo2*100, mdu2*100, mdo3*100, mdu3*100, mdo4*100, mdu4*100, mdo5*100, mdu5*100, mdo6*100, mdu6*100)
official <- c('o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u')

amtdew <- data.frame(postdate, engagement, official)

ggg<-ggplot(data=amtdew,aes(x=postdate,y=engagement,fill= official))+geom_line(aes(colour=official, group=official))+scale_fill_brewer(palette="Spectral")+ggtitle("Mountain Dew Engagement as % of Exposure")+xlab("Date")+ylab("Engagement %")+theme_bw()
ggg

## Reeses GRAPHS



reesesuserviewtotal11presuperbowl <- sum(reesesuser11$views)
reesesofficialviewtotal11presuperbowl <- sum(reesesofficial11$views)
reesesuserviewtotal16presuperbowl <- sum(reesesuser16$views)
reesesofficialviewtotal16presuperbowl <- sum(reesesofficial16$views)
reesesuserviewtotal23presuperbowl <- sum(reesesuser23$views)
reesesofficialviewtotal23presuperbowl <- sum(reesesofficial23$views)
reesesuserviewtotal30presuperbowl <- sum(reesesuser30$views)
reesesofficialviewtotal30presuperbowl <- sum(reesesofficial30$views)

reesesuserengagementtotal11presuperbowl <- sum(reesesuser11$Engagement)
reesesofficialengagementtotal11presuperbowl <- sum(reesesofficial11$Engagement)
reesesuserengagementtotal16presuperbowl <- sum(reesesuser16$Engagement)
reesesofficialengagementtotal16presuperbowl <- sum(reesesofficial16$Engagement)
reesesuserengagementtotal23presuperbowl <- sum(reesesuser23$Engagement)
reesesofficialengagementtotal23presuperbowl <- sum(reesesofficial23$Engagement)
reesesuserengagementtotal30presuperbowl <- sum(reesesuser30$Engagement)
reesesofficialengagementtotal30presuperbowl <- sum(reesesofficial30$Engagement)

ru1 <- reesesuserengagementtotal11presuperbowl/reesesuserviewtotal11presuperbowl
ru2 <- reesesuserengagementtotal16presuperbowl/reesesuserviewtotal16presuperbowl
ru3 <- reesesuserengagementtotal23presuperbowl/reesesuserviewtotal23presuperbowl
ru4 <- reesesuserengagementtotal30presuperbowl/reesesuserviewtotal30presuperbowl
ru5 <- reesesuserengagementtotal1wkpresuperbowl/reesesuserviewtotal1wkpresuperbowl
ru6 <- reesesuserengagementtotalpostsuperbowl/reesesuserviewtotalpostsuperbowl

ro1 <- reesesofficialengagementtotal11presuperbowl/reesesofficialviewtotal11presuperbowl
ro2 <- reesesofficialengagementtotal16presuperbowl/reesesofficialviewtotal16presuperbowl
ro3 <- reesesofficialengagementtotal23presuperbowl/reesesofficialviewtotal23presuperbowl
ro4 <- reesesofficialengagementtotal30presuperbowl/reesesofficialviewtotal30presuperbowl
ro5 <- reesesofficialengagementtotal1wkpresuperbowl/reesesofficialviewtotal1wkpresuperbowl
ro6 <- reesesofficialengagementtotalpostsuperbowl/reesesofficialviewtotalpostsuperbowl

postdate <- c('1/11/16', '1/11/16', '1/16/16', '1/16/16', '1/23/16', '1/23/16', '1/30/16', '1/30/16', '2/06/16', '2/06/16', '2/13/16', '2/13/16')
engagement <- c(ro1*100, ru1*100, ro2*100, ru2*100, ro3*100, ru3*100, ro4*100, ru4*100, ro5*100, ru5*100, ro6*100, ru6*100)
official <- c('o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u')

areeses <- data.frame(postdate, engagement, official)

hhh<-ggplot(data=areeses,aes(x=postdate,y=engagement,fill= official))+geom_line(aes(colour=official, group=official))+scale_fill_brewer(palette="Spectral")+ggtitle("Reeses Engagement as % of Exposure")+xlab("Date")+ylab("Engagement %")+theme_bw()
hhh

## Shocktop GRAPHS



shocktopuserviewtotal11presuperbowl <- sum(shocktopuser11$views)
shocktopofficialviewtotal11presuperbowl <- sum(shocktopofficial11$views)
shocktopuserviewtotal16presuperbowl <- sum(shocktopuser16$views)
shocktopofficialviewtotal16presuperbowl <- sum(shocktopofficial16$views)
shocktopuserviewtotal23presuperbowl <- sum(shocktopuser23$views)
shocktopofficialviewtotal23presuperbowl <- sum(shocktopofficial23$views)
shocktopuserviewtotal30presuperbowl <- sum(shocktopuser30$views)
shocktopofficialviewtotal30presuperbowl <- sum(shocktopofficial30$views)

shocktopuserengagementtotal11presuperbowl <- sum(shocktopuser11$Engagement)
shocktopofficialengagementtotal11presuperbowl <- sum(shocktopofficial11$Engagement)
shocktopuserengagementtotal16presuperbowl <- sum(shocktopuser16$Engagement)
shocktopofficialengagementtotal16presuperbowl <- sum(shocktopofficial16$Engagement)
shocktopuserengagementtotal23presuperbowl <- sum(shocktopuser23$Engagement)
shocktopofficialengagementtotal23presuperbowl <- sum(shocktopofficial23$Engagement)
shocktopuserengagementtotal30presuperbowl <- sum(shocktopuser30$Engagement)
shocktopofficialengagementtotal30presuperbowl <- sum(shocktopofficial30$Engagement)

shocku1 <- shocktopuserengagementtotal11presuperbowl/shocktopuserviewtotal11presuperbowl
shocku2 <- shocktopuserengagementtotal16presuperbowl/shocktopuserviewtotal16presuperbowl
shocku3 <- shocktopuserengagementtotal23presuperbowl/shocktopuserviewtotal23presuperbowl
shocku4 <- shocktopuserengagementtotal30presuperbowl/shocktopuserviewtotal30presuperbowl
shocku5 <- shocktopuserengagementtotal1wkpresuperbowl/shocktopuserviewtotal1wkpresuperbowl
shocku6 <- shocktopuserengagementtotalpostsuperbowl/shocktopuserviewtotalpostsuperbowl

shocko1 <- shocktopofficialengagementtotal11presuperbowl/shocktopofficialviewtotal11presuperbowl
shocko2 <- shocktopofficialengagementtotal16presuperbowl/shocktopofficialviewtotal16presuperbowl
shocko3 <- shocktopofficialengagementtotal23presuperbowl/shocktopofficialviewtotal23presuperbowl
shocko4 <- shocktopofficialengagementtotal30presuperbowl/shocktopofficialviewtotal30presuperbowl
shocko5 <- shocktopofficialengagementtotal1wkpresuperbowl/shocktopofficialviewtotal1wkpresuperbowl
shocko6 <- shocktopofficialengagementtotalpostsuperbowl/shocktopofficialviewtotalpostsuperbowl

postdate <- c('1/11/16', '1/11/16', '1/16/16', '1/16/16', '1/23/16', '1/23/16', '1/30/16', '1/30/16', '2/06/16', '2/06/16', '2/13/16', '2/13/16')
engagement <- c(shocko1*100, shocku1*100, shocko2*100, shocku2*100, shocko3*100, shocku3*100, shocko4*100, shocku4*100, shocko5*100, shocku5*100, shocko6*100, shocku6*100)
official <- c('o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u')

ashock <- data.frame(postdate, engagement, official)

iii<-ggplot(data=ashock,aes(x=postdate,y=engagement,fill= official))+geom_line(aes(colour=official, group=official))+scale_fill_brewer(palette="Spectral")+ggtitle("Shocktop Engagement as % of Exposure")+xlab("Date")+ylab("Engagement %")+theme_bw()
iii

## Snickers GRAPHS



snickersuserviewtotal11presuperbowl <- sum(snickersuser11$views, na.rm = TRUE)
snickersofficialviewtotal11presuperbowl <- sum(snickersofficial11$views, na.rm = TRUE)
snickersuserviewtotal16presuperbowl <- sum(snickersuser16$views, na.rm = TRUE)
snickersofficialviewtotal16presuperbowl <- sum(snickersofficial16$views, na.rm = TRUE)
snickersuserviewtotal23presuperbowl <- sum(snickersuser23$views, na.rm = TRUE)
snickersofficialviewtotal23presuperbowl <- sum(snickersofficial23$views, na.rm = TRUE)
snickersuserviewtotal30presuperbowl <- sum(snickersuser30$views, na.rm = TRUE)
snickersofficialviewtotal30presuperbowl <- sum(snickersofficial30$views, na.rm = TRUE)

snickersuserengagementtotal11presuperbowl <- sum(snickersuser11$Engagement, na.rm = TRUE)
snickersofficialengagementtotal11presuperbowl <- sum(snickersofficial11$Engagement, na.rm = TRUE)
snickersuserengagementtotal16presuperbowl <- sum(snickersuser16$Engagement, na.rm = TRUE)
snickersofficialengagementtotal16presuperbowl <- sum(snickersofficial16$Engagement, na.rm = TRUE)
snickersuserengagementtotal23presuperbowl <- sum(snickersuser23$Engagement, na.rm = TRUE)
snickersofficialengagementtotal23presuperbowl <- sum(snickersofficial23$Engagement, na.rm = TRUE)
snickersuserengagementtotal30presuperbowl <- sum(snickersuser30$Engagement, na.rm = TRUE)
snickersofficialengagementtotal30presuperbowl <- sum(snickersofficial30$Engagement, na.rm = TRUE)

snickersu1 <- snickersuserengagementtotal11presuperbowl/snickersuserviewtotal11presuperbowl
snickersu2 <- snickersuserengagementtotal16presuperbowl/snickersuserviewtotal16presuperbowl
snickersu3 <- snickersuserengagementtotal23presuperbowl/snickersuserviewtotal23presuperbowl
snickersu4 <- snickersuserengagementtotal30presuperbowl/snickersuserviewtotal30presuperbowl
snickersu5 <- snickersuserengagementtotal1wkpresuperbowl/snickersuserviewtotal1wkpresuperbowl
snickersu6 <- snickersuserengagementtotalpostsuperbowl/snickersuserviewtotalpostsuperbowl

snickerso1 <- snickersofficialengagementtotal11presuperbowl/snickersofficialviewtotal11presuperbowl
snickerso2 <- snickersofficialengagementtotal16presuperbowl/snickersofficialviewtotal16presuperbowl
snickerso3 <- snickersofficialengagementtotal23presuperbowl/snickersofficialviewtotal23presuperbowl
snickerso4 <- snickersofficialengagementtotal30presuperbowl/snickersofficialviewtotal30presuperbowl
snickerso5 <- snickersofficialengagementtotal1wkpresuperbowl/snickersofficialviewtotal1wkpresuperbowl
snickerso6 <- snickersofficialengagementtotalpostsuperbowl/snickersofficialviewtotalpostsuperbowl

postdate <- c('1/11/16', '1/11/16', '1/16/16', '1/16/16', '1/23/16', '1/23/16', '1/30/16', '1/30/16', '2/06/16', '2/06/16', '2/13/16', '2/13/16')
engagement <- c(snickerso1*100, snickersu1*100, snickerso2*100, snickersu2*100, snickerso3*100, snickersu3*100, snickerso4*100, snickersu4*100, snickerso5*100, snickersu5*100, snickerso6*100, snickersu6*100)
official <- c('o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u')

asnickers <- data.frame(postdate, engagement, official)

jjj<-ggplot(data=asnickers,aes(x=postdate,y=engagement,fill= official))+geom_line(aes(colour=official, group=official))+scale_fill_brewer(palette="Spectral")+ggtitle("Snickers Engagement as % of Exposure")+xlab("Date")+ylab("Engagement %")+theme_bw()
jjj

postdate <- c('1/11/16', '1/11/16', '1/16/16', '1/16/16', '1/23/16', '1/23/16', '1/30/16', '1/30/16', '2/06/16', '2/06/16', '2/13/16', '2/13/16')
engagement <- c(shocktopofficialengagementtotal11presuperbowl, shocktopuserengagementtotal11presuperbowl, shocktopofficialengagementtotal16presuperbowl, shocktopuserengagementtotal16presuperbowl, shocktopofficialengagementtotal23presuperbowl, shocktopuserengagementtotal23presuperbowl, shocktopofficialengagementtotal30presuperbowl, shocktopuserengagementtotal30presuperbowl, shocktopofficialengagementtotal1wkpresuperbowl, shocktopuserengagementtotal1wkpresuperbowl, shocktopofficialengagementtotalpostsuperbowl, shocktopuserengagementtotalpostsuperbowl)
official <- c('o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u')

aashe <- data.frame(postdate, engagement, official)

kkk<-ggplot(data=aashe,aes(x=postdate,y=engagement,fill= official))+geom_line(aes(colour=official, group=official))+scale_fill_brewer(palette="Spectral")+ggtitle("Shocktop Engagement")+xlab("Date")+ylab("Engagement")+theme_bw()
kkk

postdate <- c('1/11/16', '1/11/16', '1/16/16', '1/16/16', '1/23/16', '1/23/16', '1/30/16', '1/30/16', '2/06/16', '2/06/16', '2/13/16', '2/13/16')
engagement <- c(snickersofficialengagementtotal11presuperbowl/1000, snickersuserengagementtotal11presuperbowl/1000, snickersofficialengagementtotal16presuperbowl/1000, snickersuserengagementtotal16presuperbowl/1000, snickersofficialengagementtotal23presuperbowl/1000, snickersuserengagementtotal23presuperbowl/1000, snickersofficialengagementtotal30presuperbowl/1000, snickersuserengagementtotal30presuperbowl/1000, snickersofficialengagementtotal1wkpresuperbowl/1000, snickersuserengagementtotal1wkpresuperbowl/1000, snickersofficialengagementtotalpostsuperbowl/1000, snickersuserengagementtotalpostsuperbowl/1000)
official <- c('o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u')

aasne <- data.frame(postdate, engagement, official)

lll<-ggplot(data=aasne,aes(x=postdate,y=engagement,fill= official))+geom_line(aes(colour=official, group=official))+scale_fill_brewer(palette="Spectral")+ggtitle("Snickers Engagement (in thousands)")+xlab("Date")+ylab("Engagement (in thousands)")+theme_bw()
lll

postdate <- c('1/11/16', '1/11/16', '1/16/16', '1/16/16', '1/23/16', '1/23/16', '1/30/16', '1/30/16', '2/06/16', '2/06/16', '2/13/16', '2/13/16')
engagement <- c(reesesofficialengagementtotal11presuperbowl, reesesuserengagementtotal11presuperbowl, reesesofficialengagementtotal16presuperbowl, reesesuserengagementtotal16presuperbowl, reesesofficialengagementtotal23presuperbowl, reesesuserengagementtotal23presuperbowl, reesesofficialengagementtotal30presuperbowl, reesesuserengagementtotal30presuperbowl, reesesofficialengagementtotal1wkpresuperbowl, reesesuserengagementtotal1wkpresuperbowl, reesesofficialengagementtotalpostsuperbowl, reesesuserengagementtotalpostsuperbowl)
official <- c('o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u')

aare <- data.frame(postdate, engagement, official)

mmm<-ggplot(data=aare,aes(x=postdate,y=engagement,fill= official))+geom_line(aes(colour=official, group=official))+scale_fill_brewer(palette="Spectral")+ggtitle("Reeses Engagement")+xlab("Date")+ylab("Engagement")+theme_bw()
mmm

postdate <- c('1/11/16', '1/11/16', '1/16/16', '1/16/16', '1/23/16', '1/23/16', '1/30/16', '1/30/16', '2/06/16', '2/06/16', '2/13/16', '2/13/16')
engagement <- c(mtdewofficialviewtotal11presuperbowl/1000, mtdewuserviewtotal11presuperbowl/1000, mtdewofficialviewtotal16presuperbowl/1000, mtdewuserviewtotal16presuperbowl/1000, mtdewofficialviewtotal23presuperbowl/1000, mtdewuserviewtotal23presuperbowl/1000, mtdewofficialviewtotal30presuperbowl/1000, mtdewuserviewtotal30presuperbowl/1000, mtdewofficialviewtotal1wkpresuperbowl/1000, mtdewuserviewtotal1wkpresuperbowl/1000, mtdewofficialviewtotalpostsuperbowl/1000, mtdewuserviewtotalpostsuperbowl/1000)
official <- c('o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u')

aamv <- data.frame(postdate, engagement, official)

nnn<-ggplot(data=aamv,aes(x=postdate,y=engagement,fill= official))+geom_line(aes(colour=official, group=official))+scale_fill_brewer(palette="Spectral")+ggtitle("Mountain Dew Views (in thousands)")+xlab("Date")+ylab("Views (in thousands)")+theme_bw()
nnn

postdate <- c('1/11/16', '1/11/16', '1/16/16', '1/16/16', '1/23/16', '1/23/16', '1/30/16', '1/30/16', '2/06/16', '2/06/16', '2/13/16', '2/13/16')
engagement <- c(reesesofficialviewtotal11presuperbowl/1000, reesesuserviewtotal11presuperbowl/1000, reesesofficialviewtotal16presuperbowl/1000, reesesuserviewtotal16presuperbowl/1000, reesesofficialviewtotal23presuperbowl/1000, reesesuserviewtotal23presuperbowl/1000, reesesofficialviewtotal30presuperbowl/1000, reesesuserviewtotal30presuperbowl/1000, reesesofficialviewtotal1wkpresuperbowl/1000, reesesuserviewtotal1wkpresuperbowl/1000, reesesofficialviewtotalpostsuperbowl/1000, reesesuserviewtotalpostsuperbowl/1000)
official <- c('o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u')

aarv <- data.frame(postdate, engagement, official)

ooo<-ggplot(data=aarv,aes(x=postdate,y=engagement,fill= official))+geom_line(aes(colour=official, group=official))+scale_fill_brewer(palette="Spectral")+ggtitle("Reeses Views (in thousands)")+xlab("Date")+ylab("Views (in thousands)")+theme_bw()
ooo

postdate <- c('1/11/16', '1/11/16', '1/16/16', '1/16/16', '1/23/16', '1/23/16', '1/30/16', '1/30/16', '2/06/16', '2/06/16', '2/13/16', '2/13/16')
engagement <- c(shocktopofficialviewtotal11presuperbowl/1000, shocktopuserviewtotal11presuperbowl/1000, shocktopofficialviewtotal16presuperbowl/1000, shocktopuserviewtotal16presuperbowl/1000, shocktopofficialviewtotal23presuperbowl/1000, shocktopuserviewtotal23presuperbowl/1000, shocktopofficialviewtotal30presuperbowl/1000, shocktopuserviewtotal30presuperbowl/1000, shocktopofficialviewtotal1wkpresuperbowl/1000, shocktopuserviewtotal1wkpresuperbowl/1000, shocktopofficialviewtotalpostsuperbowl/1000, shocktopuserviewtotalpostsuperbowl/1000)
official <- c('o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u')

aashockv <- data.frame(postdate, engagement, official)

ppp<-ggplot(data=aashockv,aes(x=postdate,y=engagement,fill= official))+geom_line(aes(colour=official, group=official))+scale_fill_brewer(palette="Spectral")+ggtitle("Shocktop Views (in thousands)")+xlab("Date")+ylab("Views (in thousands)")+theme_bw()
ppp

postdate <- c('1/11/16', '1/11/16', '1/16/16', '1/16/16', '1/23/16', '1/23/16', '1/30/16', '1/30/16', '2/06/16', '2/06/16', '2/13/16', '2/13/16')
engagement <- c(snickersofficialviewtotal11presuperbowl/1000000, snickersuserviewtotal11presuperbowl/1000000, snickersofficialviewtotal16presuperbowl/1000000, snickersuserviewtotal16presuperbowl/1000000, snickersofficialviewtotal23presuperbowl/1000000, snickersuserviewtotal23presuperbowl/1000000, snickersofficialviewtotal30presuperbowl/1000000, snickersuserviewtotal30presuperbowl/1000000, snickersofficialviewtotal1wkpresuperbowl/1000000, snickersuserviewtotal1wkpresuperbowl/1000000, snickersofficialviewtotalpostsuperbowl/1000000, snickersuserviewtotalpostsuperbowl/1000000)
official <- c('o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u', 'o', 'u')

aasv <- data.frame(postdate, engagement, official)

ppp<-ggplot(data=aasv,aes(x=postdate,y=engagement,fill= official))+geom_line(aes(colour=official, group=official))+scale_fill_brewer(palette="Spectral")+ggtitle("Snickers Views (in millions)")+xlab("Date")+ylab("Views (in millions)")+theme_bw()
ppp

mlikeofficialsum <- sum(mtdewofficial6$likes, mtdewofficial13$likes, mtdewofficial11$likes, mtdewofficial23$likes, mtdewofficial30$likes, mtdewofficial16$likes)
mlikeusersum <- sum(mtdewuser6$likes, mtdewuser13$likes, mtdewuser11$likes, mtdewuser23$likes, mtdewuser30$likes, mtdewuser16$likes)

mcommentofficialsum <- sum(mtdewofficial6$comments, mtdewofficial13$comments, mtdewofficial11$comments, mtdewofficial23$comments, mtdewofficial30$comments, mtdewofficial16$comments)
mcommentusersum <- sum(mtdewuser6$comments, mtdewuser13$comments, mtdewuser11$comments, mtdewuser23$comments, mtdewuser30$comments, mtdewuser16$comments)

postdate <- c("Comments", "Comments", "Likes", "Likes", "Total Engagement", "Total Engagement")
engagement <- c(mcommentofficialsum/100, mcommentusersum/100, mlikeofficialsum/100, mlikeusersum/100, mtdewofficialengagementsum/100, mtdewuserengagementsum/100)
official <- c('o', 'u', 'o', 'u', 'o', 'u')

totalengagementchart <- data.frame(postdate, engagement, official)

qqq <- ggplot(data=totalengagementchart, aes(x=postdate, y=engagement, fill=official)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +ggtitle("Total Mountain Dew Engagements (in hundreds)")+xlab("Interaction Type")+ylab("Interactions (in hundreds)")

qqq

rlikeofficialsum <- sum(reesesofficial6$likes, reesesofficial13$likes, reesesofficial11$likes, reesesofficial23$likes, reesesofficial30$likes, reesesofficial16$likes)
rlikeusersum <- sum(reesesuser6$likes, reesesuser13$likes, reesesuser11$likes, reesesuser23$likes, reesesuser30$likes, reesesuser16$likes)

rcommentofficialsum <- sum(reesesofficial6$comments, reesesofficial13$comments, reesesofficial11$comments, reesesofficial23$comments, reesesofficial30$comments, reesesofficial16$comments)
rcommentusersum <- sum(reesesuser6$comments, reesesuser13$comments, reesesuser11$comments, reesesuser23$comments, reesesuser30$comments, reesesuser16$comments)

postdate <- c("Comments", "Comments", "Likes", "Likes", "Total Engagement", "Total Engagement")
engagement <- c(rcommentofficialsum/1000, rcommentusersum/1000, rlikeofficialsum/1000, rlikeusersum/1000, reesesofficialengagementsum/1000, reesesuserengagementsum/1000)
official <- c('o', 'u', 'o', 'u', 'o', 'u')

reeseschart <- data.frame(postdate, engagement, official)

rrr <- ggplot(data=reeseschart, aes(x=postdate, y=engagement, fill=official)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +ggtitle("Total Reeses Engagements (in thousands)")+xlab("Interaction Type")+ylab("Interactions (in thousands)")

rrr

shocklikeofficialsum <- sum(shocktopofficial6$likes, shocktopofficial13$likes, shocktopofficial11$likes, shocktopofficial23$likes, shocktopofficial30$likes, shocktopofficial16$likes)
shocklikeusersum <- sum(shocktopuser6$likes, shocktopuser13$likes, shocktopuser11$likes, shocktopuser23$likes, shocktopuser30$likes, shocktopuser16$likes)

shockcommentofficialsum <- sum(shocktopofficial6$comments, shocktopofficial13$comments, shocktopofficial11$comments, shocktopofficial23$comments, shocktopofficial30$comments, shocktopofficial16$comments)
shockcommentusersum <- sum(shocktopuser6$comments, shocktopuser13$comments, shocktopuser11$comments, shocktopuser23$comments, shocktopuser30$comments, shocktopuser16$comments)

postdate <- c("Comments", "Comments", "Likes", "Likes", "Total Engagement", "Total Engagement")
engagement <- c(shockcommentofficialsum/1000, shockcommentusersum/1000, shocklikeofficialsum/1000, shocklikeusersum/1000, shocktopofficialengagementsum/1000, shocktopuserengagementsum/1000)
official <- c('o', 'u', 'o', 'u', 'o', 'u')

shockchart <- data.frame(postdate, engagement, official)

rrr <- ggplot(data=shockchart, aes(x=postdate, y=engagement, fill=official)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +ggtitle("Total Shocktop Engagements (in thousands)")+xlab("Interaction Type")+ylab("Interactions (in thousands)")

rrr

slikeofficialsum <- sum(snickersofficial6$likes, snickersofficial13$likes, snickersofficial11$likes, snickersofficial23$likes, snickersofficial30$likes, snickersofficial16$likes)
slikeusersum <- sum(snickersuser6$likes, snickersuser13$likes, snickersuser11$likes, snickersuser23$likes, snickersuser30$likes, snickersuser16$likes)

scommentofficialsum <- sum(snickersofficial6$comments, snickersofficial13$comments, snickersofficial11$comments, snickersofficial23$comments, snickersofficial30$comments, snickersofficial16$comments)
scommentusersum <- sum(snickersuser6$comments, snickersuser13$comments, snickersuser11$comments, snickersuser23$comments, snickersuser30$comments, snickersuser16$comments)

postdate <- c("Comments", "Comments", "Likes", "Likes", "Total Engagement", "Total Engagement")
engagement <- c(scommentofficialsum/1000000, scommentusersum/1000000, slikeofficialsum/1000000, slikeusersum/1000000, snickersofficialengagementsum/1000000, snickersuserengagementsum/1000000)
official <- c('o', 'u', 'o', 'u', 'o', 'u')

schart <- data.frame(postdate, engagement, official)

sss <- ggplot(data=schart, aes(x=postdate, y=engagement, fill=official)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +ggtitle("Total Snickers Engagements (in millions)")+xlab("Interaction Type")+ylab("Interactions (in millions)")

sss

postdate <- c("Views Before Super Bowl","Views Before Super Bowl", "Views After Super Bowl","Views After Super Bowl", "Total Views", "Total Views")
engagement <- c(mtdewofficialviewtotalpresuperbowl/1000, mtdewuserviewtotalpresuperbowl/1000, mtdewofficialviewtotalpostsuperbowl/1000, mtdewuserviewtotalpostsuperbowl/1000, mtdewofficialviewsum/1000, mtdewuserviewsum/1000)
official <- c('o', 'u', 'o', 'u', 'o', 'u')

schart <- data.frame(postdate, engagement, official)

ttt <- ggplot(data=schart, aes(x=postdate, y=engagement, fill=official)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +ggtitle("Total Mountain Dew Views (in thousands)")+xlab("Time Frame")+ylab("Views (in thousands)")

ttt

postdate <- c("Views Before Super Bowl","Views Before Super Bowl", "Views After Super Bowl","Views After Super Bowl", "Total Views", "Total Views")
engagement <- c(reesesofficialviewtotalpresuperbowl/1000000, reesesuserviewtotalpresuperbowl/1000000, reesesofficialviewtotalpostsuperbowl/1000000, reesesuserviewtotalpostsuperbowl/1000000, reesesofficialviewsum/1000000, reesesuserviewsum/1000000)
official <- c('o', 'u', 'o', 'u', 'o', 'u')

reesesviewchart <- data.frame(postdate, engagement, official)

uuu <- ggplot(data=reesesviewchart, aes(x=postdate, y=engagement, fill=official)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +ggtitle("Total Reeses Views (in millions)")+xlab("Time Frame")+ylab("Views (in millions)")

uuu

postdate <- c("Views Before Super Bowl","Views Before Super Bowl", "Views After Super Bowl","Views After Super Bowl", "Total Views", "Total Views")
engagement <- c(shocktopofficialviewtotalpresuperbowl/1000000, shocktopuserviewtotalpresuperbowl/1000000, shocktopofficialviewtotalpostsuperbowl/1000000, shocktopuserviewtotalpostsuperbowl/1000000, shocktopofficialviewsum/1000000, shocktopuserviewsum/1000000)
official <- c('o', 'u', 'o', 'u', 'o', 'u')

shocktopviewchart <- data.frame(postdate, engagement, official)

vvv <- ggplot(data=shocktopviewchart, aes(x=postdate, y=engagement, fill=official)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +ggtitle("Total Shocktop Views (in millions)")+xlab("Time Frame")+ylab("Views (in millions)")

vvv

postdate <- c("Views Before Super Bowl","Views Before Super Bowl", "Views After Super Bowl","Views After Super Bowl", "Total Views", "Total Views")
engagement <- c(snickersofficialviewtotalpresuperbowl/1000000, snickersuserviewtotalpresuperbowl/1000000, snickersofficialviewtotalpostsuperbowl/1000000, snickersuserviewtotalpostsuperbowl/1000000, snickersofficialviewsum/1000000, snickersuserviewsum/1000000)
official <- c('o', 'u', 'o', 'u', 'o', 'u')

snickersviewchart <- data.frame(postdate, engagement, official)

www <- ggplot(data=snickersviewchart, aes(x=postdate, y=engagement, fill=official)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +ggtitle("Total Snickers Views (in millions)")+xlab("Time Frame")+ylab("Views (in millions)")

www





