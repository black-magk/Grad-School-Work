GT<-read.csv("~/Desktop/GT.new.csv",stringsAsFactors=FALSE)
head(GT)
GT.paid<-subset(GT, ransompaid> 0)
head(GT.paid)

install.packages('dummies-package')

library(dummies-package)


mtcars[grep("Merc", rownames(mtcars)), ]

#renaming column
names(GT.paid)[names(GT.paid) == 'hostkidoutcome_txt'] <- 'hostoutcome'
head(GT.paid)


unique(GT.paid$hostoutcome)
GT.paid

GT.host <- GT.paid[grep("Hostage(s) released by perpetrators", GT.paid$hostoutcome), ]

#sub 1
GT.host<-subset(GT.paid, hostoutcome=="Hostage(s) released by perpetrators"|hostoutcome=="Hostage(s) killed (not during rescue attempt)")
head(GT.host)

GT.host


#sub 2
GT.attack<-subset(GT.host, attacktype1_txt="Hostage Taking (Kidnapping)")
head(GT.attack)

#before categorical
#sub
GT.important<-subset(GT.attack[,c(1,7,11,14,16,20)])
GT.important2<-subset(GT.important,ndays>0)
GT.important2

#create dummies
GT.important2$Firearm_weap<-ifelse(GT.important2$weaptype1_txt=="Firearms",1,0)

GT.important2$Explosive_weap<-ifelse(GT.important2$weaptype1_txt=="Explosives/Bombs/Dynamite",1,0)

GT.important2$Other_weap<-ifelse(GT.important2$weaptype1_txt=="Unknown",1,0)

GT.important2



x<-unique(GT.important2$region_txt)
x[1]

## more categorical
GT.important2$Central_America_Cari<-ifelse(GT.important2$region_txt==x[1],1,0)

GT.important2$Sub_Saharan_Africa<-ifelse(GT.important2$region_txt==x[3],1,0)
GT.important2$South_America<-ifelse(GT.important2$region_txt==x[2],1,0)
GT.important2$SouthEast_Asia<-ifelse(GT.important2$region_txt==x[4],1,0)
GT.important2$South_Asia<-ifelse(GT.important2$region_txt==x[6],1,0)
GT.important2$Western_Europe<-ifelse(GT.important2$region_txt==x[5],1,0)
GT.important2$Middle_east_North_Africa<-ifelse(GT.important2$region_txt==x[7],1,0)



## additional categorical
y<-unique(GT.important2$hostoutcome)
y

GT.important2$released<-ifelse(GT.important2$hostoutcome==y[1],1,0)

GT.important2



GT.important3<-subset(GT.important2[,c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)])
GT.important3

z<-colnames(GT.important3)
z
regression.test<-glm(ransompaid~ransomamt,data=GT.important3)



safety.1<-glm(released~Firearm_weap+Explosive_weap+Other_weap+ Central_America_Cari+Sub_Saharan_Africa+South_America+SouthEast_Asia+South_Asia+Western_Europe+Middle_east_North_Africa+ransomamt+ransompaid,data=GT.important3)








summary(safety.1)


#Location
#ransom amount
#ransom paid
#type of weapon in use
#0......1

coef(safety.1)[10]


Olympics-Travel Destination

 #brazil(South America) 2016


# Micheal Phelps is kidnapped at gun point in Brazil while traveling for the olympic games at 8:00am and has a race that night at 6:pm.. The group that has kidnapped him is a known terrorist group and demands 10Million dollars from the U.S. Government. Since the U.S Government does not negotiate with terrorist they do not pay a a dime to the kidnappers. Whats the probability you are released safely?



#Location= Brazil (south america)
#ransom amount-($20,000,000.00)
#ransom paid ($0.00)
#type of weapon in use (Pistol)




Brazil<-unname(coef(safety.1)[1]+coef(safety.1)[2]*1+coef(safety.1)[3]*0+coef(safety.1)[5]*0+coef(safety.1)[6]*0+coef(safety.1)[7]*1+coef(safety.1)[8]*0+coef(safety.1)[9]*0+coef(safety.1)[10]*0+coef(safety.1)[12]*20000000+coef(safety.1)[13]*0) 

Brazil

37% 






#U.K (Western_Europe) London Olympics
#The lead signer of one direction went to watch the Men's U.S Water Polo Team Play a match in London for the Olympics ,but was kidnapped by a crazy fan. Since the lead singer was going to be knighted by the queen later that week the government was inclined to comply with kidnappers. The kidnappers demanded 16Million dollars; Thus 16 million dollars was paid. Additionally a ban on guns in the U.K left the kidnapper no other option than to strap the singer with explosives attached to a dead man switch. What is the probabability he is returned unscathed?


#Location= U.K (Western Europe)
#ransom amount-($16,000,000.00)
#ransom paid ($16,000,000.00)
#type of weapon in use (Explosive)




London<-unname(coef(safety.1)[1]+coef(safety.1)[2]*0+coef(safety.1)[3]*1+coef(safety.1)[5]*0+coef(safety.1)[6]*0+coef(safety.1)[7]*0+coef(safety.1)[8]*0+coef(safety.1)[9]*0+coef(safety.1)[10]*1+coef(safety.1)[12]*16000000+coef(safety.1)[13]*16000000) 

London

# 68% 






