#September 13, 2016

#Sample code for ZEFR project that shows how to read in the YouTube data and create a data frame listing unique video IDs and cumulative views on the different dates.

#install.packages("openxlsx")
library(openxlsx) 

brands <- c("mtdew", "reeses", "shocktop", "snickers" )
dataFiles <- paste0("data-F16/",brands,".xlsx")
observationDates <- as.Date(c("2016-01-11","2016-01-16","2016-01-23", "2016-01-30", "2016-02-06", "2016-02-13"))


colNamesForTables <- c(colnames(read.xlsx(dataFiles[1], sheet=1)), "brandname", "datecollected")

#alldata is the master data frame to store all the data
alldata <- data.frame()

#Outer "for" loop = a loop through the four files.
for(fileIndex in 1:length(dataFiles)){
  fileName <- dataFiles[fileIndex]
  
  #Inner "for" loop = a loop through the tabs in the spreadsheet.
  for(tabIndex in 1:length(observationDates)){
  
    #read in a tab (worksheet) from the file. Save it in a dataframe I call dataFromTab.
    dataFromTab <- read.xlsx(fileName, sheet=tabIndex)
    #create column with brand
    dataFromTab$brandname <- brands[fileIndex]   
    #create column with date
    dataFromTab$datecollected <- observationDates[tabIndex] 
    #add columns to master data frame (called alldata)
    alldata <- rbind(alldata, dataFromTab[,colNamesForTables])
  } #end for tabIndex
} #end for fileIndex