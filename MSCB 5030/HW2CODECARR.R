##1
set.seed(800)
install.packages("OIdata")

library(OIdata)

##2
data(birds)

summary(birds)


#a
summary(birds$height)
plot(birds$height,birds$speed)
X = birds$height 
Y = birds$speed
cor(X, Y,use="complete")

#b
## a.iii. Side-by-side boxplots

boxplot(birds$height,birds$phase_of_flt)

#3
boxplot(birds$speed,birds$num_engs)
## if I were a bird ultimately I would like to see zero engines in my airspace. However, this is very unlikely and it looks like if It was a very fast plane with 1 engine that traveled greater than 300. The likely hood of collision diminishes
#a
birds12 = birds[birds$num_eng %in% c(1,2), ]
mean_speed_num= tapply(birds12$speed, birds12$num_eng, mean, na.rm=TRUE)
help(tapply)
sd_speed_num= tapply(birds12$speed, birds12$num_eng, sd, na.rm=TRUE)
table(birds12$num_engs)

SE_speed_num = sd_speed_num/(15343)^(1/2)

SE_speed_num

Lower_conf_speed_num = mean_speed_num - (1.96)*SE_speed_num
Upper_conf_speed_num = mean_speed_num + (1.96)*SE_speed_num

Lower_conf_speed_num
Upper_conf_speed_num
                

## The conf interval for speed is (84.406,85.2991)
## The conf interval for num_engs( 138.708, 140.0471)

##4

hist(birds$height)
#the graph is right skewed. The dist suggests at low altitudes more likely to hit birds

T= mean(birds$height, na.rm = TRUE)
Z = sd(birds$height, na.rm = TRUE)
SE_height = Z/(19302)^(1/2)
SE_height
Lower_conf_height = T - 1.96*SE_height
Upper_conf_height = T + 1.96*SE_height
Lower_conf_height
Upper_conf_height
summary(birds)
help(sd)

summary(birds$height)
c(Lower_conf_height, Upper_conf_height)
