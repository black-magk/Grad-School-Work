##############
###MAY NEED TO UPDATE JAVA
###LOAD PACKAGES

install.packages("XLConnect")
library(XLConnect)

install.packages("xlsx")
library(xlsx)




##################

#Shortcut to call files

#Mac Users- place excel files on desktop

brands <- c("mtdew", "reeses", "shocktop", "snickers","official-channels" )
dataFiles <- paste0("~/Desktop/",brands,".xlsx")


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

snickers <- read.xlsx(dataFiles[4])

jan.11_snickers <- read.xlsx(dataFiles[4], sheet = 1)
jan.16_snickers <- read.xlsx(dataFiles[4], sheet = 2)
jan.23_snickers <- read.xlsx(dataFiles[4], sheet = 3)
jan.30_snickers <- read.xlsx(dataFiles[4], sheet = 4)
feb.06_snickers <- read.xlsx(dataFiles[4], sheet = 5)
feb.13_snickers <- read.xlsx(dataFiles[4], sheet = 6)



#load official channels excel workbook

officialchannels <- read.xlsx(dataFiles[5])

#Check it loaded properly
officialchannels