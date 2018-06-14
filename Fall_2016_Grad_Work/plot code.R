
install.packages("RColorBrewer")

library(RColorBrewer)



####SHOCKTOP#####

#Sum's the views of shocktop from your newly formed subset containing only those with primary channels


a<-sum(jan.11_shocktop_a$views)
b<-sum(jan.16_shocktop_a$views)
c<-sum(jan.23_shocktop_a$views)
d<-sum(jan.30_shocktop_a$views)
e<-sum(feb.06_shocktop_a$views)
f<-sum(feb.13_shocktop_a$views)


#put values intro vector
shocktop_views_string<-c(a,b,c,d,e,f)

#Create seperate data.frame that combines the observation dates with corresponding view count

shock_frame<-data.frame(cbind(observationDates,shocktop_views_string))
shock_frame$views1000s<-shocktop_views_string/1000

shock_frame

#sets graph equal too "ggg" using ggplot framework
ggg<-ggplot(data=shock_frame, aes(x=observationDates, y=views1000s, fill=observationDates))+geom_bar(stat="identity") + scale_fill_brewer(palette="Spectral")+coord_flip() +ggtitle("Official Shock Top Views")+ xlab("Dates")+ylab("View Count ( in Thousands)")+theme_bw()


#view graph
ggg

####MT DEW##### SAME STEPS AS ABOVE...REPEAT

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

library("ggplot2")

install.packages("RColorBrewer")

library(RColorBrewer)

hhh<-ggplot(data=dew_frame, aes(x=observationDates, y=views1000s, fill=observationDates))+geom_bar(stat="identity") + scale_fill_brewer(palette="Spectral")+coord_flip() +ggtitle("Official Mt Dew Views")+ xlab("Dates")+ylab("View Count ( in Thousands)")+theme_bw()
hhh



#####REEESES#####



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



###SNICKERS####

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

jjj