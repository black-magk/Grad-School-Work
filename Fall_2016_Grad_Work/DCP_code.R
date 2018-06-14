#read in csv data
dcp<-read.csv("Desktop/DCP.csv") 

head(dcp$Birthdate)

#install packages
install.packages("stringr")
library(stringr)

 install.packages("arm")
library(arm)

install.packages("xlsx")
library(xlsx)

install.packages("stats")
library(stats)

install.packages("rpart")
library(rpart)

install.packages("rattle")
library(rattle)

install.packages('grid')
library(grid)

install.packages('rpart.plot')
library(rpart.plot)

install.packages('partykit')
library(partykit)


library(RColorBrewer)

#Selecting certain columns

modifiedDCP = dcp[,c(1, 7, 8, 9, 10, 11,14,15,16, 17, 18, 19, 20, 21, 22, 23,24,25,26,28,29)]

modifiedDCP.1<-data.frame(modifiedDCP)

modifiedDCP.2<-subset(na.omit(modifiedDCP.1), Employee.Status== "A"|Employee.Status== "R" |EEO.1.Job.Category != "N")


5-12

n

#Employee.Type Coded
modifiedDCP.2$Employee.Type.mod<-ifelse(modifiedDCP.2$Employee.Type=="H",1,0)

#Employee.Status.mod

modifiedDCP.2$Employee.Status.mod<-ifelse(modifiedDCP.2$Employee.Status=="R",1,0)

#way to remove junk columns

#df.test= subset(modifiedDCP.2, select = -c(Service.Date.year.mod) )

#split data into manageable############################################
a<-str_split_fixed(modifiedDCP.2$Original.Hire.Date, "/", 3)

modifiedDCP.2$Original.Hire.Date.Year.mod<-as.numeric(a[,3])

b<-str_split_fixed(modifiedDCP.2$Effective.Date, "/", 3)

modifiedDCP.2$Effective.Date.Year.mod<-as.numeric(b[,3])

# Years worked
modifiedDCP.2$Years.Worked<-(modifiedDCP.2$Effective.Date.Year.mod-modifiedDCP.2$Original.Hire.Date.Year.mod)

#breaking birthday data

c<-str_split_fixed(modifiedDCP.2$Birthdate, "/", 3)

#Birthday year column
modifiedDCP.2$Birthday.year.mod<-as.numeric(c[,3])

modifiedDCP.2$Age<-(2016- modifiedDCP.2$Birthday.year.mod)

head(modifiedDCP.2$Age)
###################################################################

#Birthday grouping
modifiedDCP.2$birthday.b1950<-ifelse(modifiedDCP.2$Birthday.year.mod< 1950,1,0)
modifiedDCP.2$birthday.b1960<-ifelse(modifiedDCP.2$Birthday.year.mod< 1960 & modifiedDCP.2$Birthday.year.mod>=1950,1,0)
modifiedDCP.2$birthday.b1970<-ifelse(modifiedDCP.2$Birthday.year.mod< 1970 & modifiedDCP.2$Birthday.year.mod>=1960,1,0)
modifiedDCP.2$birthday.b1980<-ifelse(modifiedDCP.2$Birthday.year.mod< 1980 & modifiedDCP.2$Birthday.year.mod>=1970,1,0)
modifiedDCP.2$birthday.b1990<-ifelse(modifiedDCP.2$Birthday.year.mod< 1990 & modifiedDCP.2$Birthday.year.mod>=1980,1,0)
modifiedDCP.2$birthday.b2000<-ifelse(modifiedDCP.2$Birthday.year.mod< 2000 & modifiedDCP.2$Birthday.year.mod>=1990,1,0)
modifiedDCP.2$birthday.b2010<-ifelse(modifiedDCP.2$Birthday.year.mod< 2010 & modifiedDCP.2$Birthday.year.mod>=2000,1,0)
modifiedDCP.2$birthday.b2020<-ifelse(modifiedDCP.2$Birthday.year.mod< 2020 & modifiedDCP.2$Birthday.year.mod>=2010,1,0)


#RACE categorized
modifiedDCP.2$hispanic.Latino<-ifelse(modifiedDCP.2$Race.Ethnicity== "1" ,1,0)
modifiedDCP.2$White<-ifelse(modifiedDCP.2$Race.Ethnicity== "2" ,1,0)
modifiedDCP.2$Black<-ifelse(modifiedDCP.2$Race.Ethnicity== "3" ,1,0)
modifiedDCP.2$Asian<-ifelse(modifiedDCP.2$Race.Ethnicity== "4" ,1,0)
modifiedDCP.2$Nat.Hawaiian<-ifelse(modifiedDCP.2$Race.Ethnicity== "5" ,1,0)
modifiedDCP.2$American.Indian.Alaska.Native<-ifelse(modifiedDCP.2$Race.Ethnicity== "6" ,1,0)
modifiedDCP.2$Two.or.more<-ifelse(modifiedDCP.2$Race.Ethnicity== "7" ,1,0)
modifiedDCP.2$Not.applicable<-ifelse(modifiedDCP.2$Race.Ethnicity== "8" ,1,0)
modifiedDCP.2$not.specified<-ifelse(modifiedDCP.2$Race.Ethnicity== "9" ,1,0)


#Full time categorized
modifiedDCP.2$Full.Part.Time.mod<-ifelse(modifiedDCP.2$Full.Part.Time== "F" ,1,0)


#gender categorized
modifiedDCP.2$Gender.Cat<-ifelse(modifiedDCP.2$Gender== "M" ,1,0)


#categorized EEO.JOB.GROUP
modifiedDCP.2$Vice.President.and.Above<-ifelse(modifiedDCP.2$EEO.Job.Group== "1" ,1,0)
modifiedDCP.2$Director<-ifelse(modifiedDCP.2$EEO.Job.Group== "12" ,1,0)
modifiedDCP.2$Manager<-ifelse(modifiedDCP.2$EEO.Job.Group== "21" ,1,0)
modifiedDCP.2$Supervisor<-ifelse(modifiedDCP.2$EEO.Job.Group== "22" ,1,0)
modifiedDCP.2$Administrative.Professional<-ifelse(modifiedDCP.2$EEO.Job.Group== "31" ,1,0)
modifiedDCP.2$Technical.Professional<-ifelse(modifiedDCP.2$EEO.Job.Group== "32" ,1,0)
modifiedDCP.2$Technician<-ifelse(modifiedDCP.2$EEO.Job.Group== "41" ,1,0)
modifiedDCP.2$Clerical.1<-ifelse(modifiedDCP.2$EEO.Job.Group== "61" ,1,0)
modifiedDCP.2$Sr.Clerical<-ifelse(modifiedDCP.2$EEO.Job.Group== "62" ,1,0)
modifiedDCP.2$Crafts.and.Skilled.Operators<-ifelse(modifiedDCP.2$EEO.Job.Group== "71" ,1,0)
modifiedDCP.2$Semi.Skilled.Operators.and.Trainees<-ifelse(modifiedDCP.2$EEO.Job.Group== "81" ,1,0)
modifiedDCP.2$Laborers<-ifelse(modifiedDCP.2$EEO.Job.Group== "91" ,1,0)


#cateogirzed EEO.1.Job.Category
modifiedDCP.2$Exec.Sr.Level.Officials<-ifelse(modifiedDCP.2$EEO.1.Job.Category== "1" ,1,0)
modifiedDCP.2$First.Mid.lvl.Officials<-ifelse(modifiedDCP.2$EEO.1.Job.Category== "2" ,1,0)
modifiedDCP.2$Professionals<-ifelse(modifiedDCP.2$EEO.1.Job.Category== "3" ,1,0)
modifiedDCP.2$Technicians<-ifelse(modifiedDCP.2$EEO.1.Job.Category== "4" ,1,0)
modifiedDCP.2$Sales.workers<-ifelse(modifiedDCP.2$EEO.1.Job.Category== "5" ,1,0)
modifiedDCP.2$Administrative.Support.workers<-ifelse(modifiedDCP.2$EEO.1.Job.Category== "6" ,1,0)
modifiedDCP.2$craft.workers<-ifelse(modifiedDCP.2$EEO.1.Job.Category== "7" ,1,0)
modifiedDCP.2$operatives<-ifelse(modifiedDCP.2$EEO.1.Job.Category== "8" ,1,0)
modifiedDCP.2$laborers.helpers<-ifelse(modifiedDCP.2$EEO.1.Job.Category== "9" ,1,0)
modifiedDCP.2$Job.category.not.specified<-ifelse(modifiedDCP.2$EEO.1.Job.Category== "N" ,1,0)


#Job specific categorized
modifiedDCP.2$Borger.Field<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FSB" ,1,0)
modifiedDCP.2$Clerical.NE<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FSC" ,1,0)
modifiedDCP.2$Executive<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FSX" ,1,0)
modifiedDCP.2$Exempt<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FSE" ,1,0)
modifiedDCP.2$Fld.Svcs.Fld<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FSN" ,1,0)
modifiedDCP.2$Fld.Svcs.Techn<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FST" ,1,0)
modifiedDCP.2$Marysville.Field.Hrly.NEd<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FSM" ,1,0)
modifiedDCP.2$Wyoming.Clerical<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FWC" ,1,0)
modifiedDCP.2$Wyoming.Exempt<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FWE" ,1,0)
modifiedDCP.2$Wyoming.Tech.NE<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FWT" ,1,0)
modifiedDCP.2$Fld.Svcs.Qualified<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FSQ" ,1,0)
modifiedDCP.2$Fld.Svcs.ETU<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FSU" ,1,0)
modifiedDCP.2$FS.Val.Verde<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "FSV" ,1,0)
modifiedDCP.2$Pipelines.Nonexempt<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "PNE" ,1,0)
modifiedDCP.2$TEPPCO.Exempt<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "TEX" ,1,0)
modifiedDCP.2$TEPPCO.FieldOps.NE<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "TFO" ,1,0)
modifiedDCP.2$TEPPCO.Field.Technicians<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "TFT" ,1,0)
modifiedDCP.2$TEPPCO.Non.Exempt.Houston.Field<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "TNE" ,1,0)
modifiedDCP.2$DUK.Unevaluated.Nonexempt<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "UNE" ,1,0)
modifiedDCP.2$DUK.Unevaluated.Exempt<-ifelse(modifiedDCP.2$Sal.Admin.Plan== "UXM" ,1,0)






#logistic regression
###########START HERE
#Cleanup data.. return to csv. then reload in

write.xlsx(modifiedDCP.2, "Desktop/DC_cleaned.xlsx")


#then reloaded
dcp_cleaned<-read.csv("Desktop/DC_cleaned_1.csv") 

dcp_cleaned$Age<-(2016- modifiedDCP.2$Birthday.year.mod)


#selecting specific subset

dcp_cleaned_A = dcp_cleaned[,c(1, 3, 4, 13,14,15,16, 17, 18, 19, 20, 21, 22, 23,24,25,26,28,29,30,31,32,33,3,4,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66)]


#viewing subset
head(dcp_cleaned_A)
head(dcp_cleaned)



#model test one
mod.1<-bayesglm(Employee.Status.mod ~ ., family = "binomial", data = dcp_cleaned_A)
summary(mod.1)



#testing tree classification
plot(tree.state)
text(tree.state)

summary(mod.2)


#generating table
 mod.1.pred = ifelse(predict(mod.1,type='response')>0.5,1,0)
 
 
 
 #ROC curve
binary.model <- rpart(mod.2,cp=.02) 

#ROC curve print
rpart.plot(binary.model)

 
table(mod.1.pred,dcp_cleaned$Employee.Status.mod)

mod.2<-step(bayesglm(Employee.Status.mod ~ ., family = "binomial", data = dcp_cleaned_A),direction="backward")


head(dcp_cleaned_A)


#final subset

dcp_cleaned_B= dcp_cleaned[,c(1, 2, 3, 4, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,38, 39, 40, 41,42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66)]


#final regression model

mod.2<-step(bayesglm(Employee.Status.mod ~ ., family = "binomial", data = dcp_cleaned_B),direction="backward")
2672.04
head(dcp_cleaned_B[,25])
head(dcp_cleaned_B)
24 57
 mod.1.pred = ifelse(predict(mod.2,type='response')>0.5,1,0)

prp(mod.2)
head(mod.1.pred,	100) 
 x<-predict(mod.2)

table(mod.1.pred,dcp_cleaned_B$Employee.Status.mod)

zz<-(6367+250)/(6367+250+151+446)

zz



summary(mod.2)
warnings()

tree.gerber = tree(mod.2)
plot(tree.gerber)
text(tree.gerber)

prune.state = prune.tree(tree.gerber,best=8)
prune.state
plot(prune.state)
text(prune.state)


# more packages
install.packages('rpart')
library(rpart)
prp(prune.state)


install.packages('rattle')
library(rattle)



# expanding on classification tree
fancyRpartPlot(prune.state)

prp(rxAddInheritance(prune.state))

rattle()


mod.3<-glm(Employee.Status.mod ~ .,data = dcp_cleaned)

step


mod.5<-bayesglm(Employee.Status.mod~ Annual.Rate+ Employee.Type.mod+ Years.Worked+birthday.b1950           +            birthday.b1960,family = "binomial", data = dcp_cleaned)


library(tree)

mod.5


colnames(dcp_cleaned)
summary(mod.1)



head(modifiedDCP.2,2)



x<-1-(672.8/4340.9)
x
y<-1-(2479.5/4577.7)
y





