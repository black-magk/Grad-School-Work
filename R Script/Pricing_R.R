tix<-read.csv("Desktop/tix.csv") 

head(tix)

tix_21<-subset( tix, row_name=="21" )

colnames(tix_21)
cor(tix_21)


tix_21$section_name.f <- factor(tix_21$section_name)
is.factor(tix_21$section_name.f)

tix_21$seat_num.f <- factor(tix_21$seat_num)
is.factor(tix_21$seat_num.f)


tix_21$price_code.f <- factor(tix_21$price_code)
is.factor(tix_21$price_code.f)


tix_21$last_seat.f <- factor(tix_21$last_seat)
is.factor(tix_21$last_seat.f)

tix_21$event_name.f <- factor(tix_21$event_name)
is.factor(tix_21$event_name.f)

tix_21$Season_end.f <- factor(tix_21$Season_end)
is.factor(tix_21$Season_end.f)

cor(tix_21$section_name,tix_21$seat_num)


tix_21$Season12_13<-ifelse(tix_21$Season_end== "2012-2013" ,1,0)
tix_21$Season13_14<-ifelse(tix_21$Season_end== "2013-2014" ,1,0)
tix_21$Season14_15<-ifelse(tix_21$Season_end== "2014-2015" ,1,0)
tix_21$Season15_16<-ifelse(tix_21$Season_end== "2015-2016" ,1,0)

mod.1<-lm(price_per_seat~section_name.f + seat_num.f+num_seats+price_code.f+last_seat+event_name.f+num_seats*event_name.f, data=tix_21)

step(mod.1, direction = "backward", trace=FALSE )

fullmodel<-(Eeff~NDF+ADF+CP+NEL+DMI+FCM,data=na.omit(phuong))
step(fullmodel, direction = "backward", trace=FALSE ) 

summary(mod.1)


x<-lm(formula = price_per_seat ~ price_code.f + event_name.f, data = tix_21)
unique(x)


25


153

5
summary(x)
library('ggplot2')
summarySE(tix_21)

tgc <- summarySE(tg, measurevar="len", groupvars=c("supp","dose"))


tix_21<-subset( tix, Season_end=="2012-2013" )


