set.seed(800)
#1a
read.csv("/Users/landon/Desktop/Grad School/MSCB 5030/YoutubeTimeWatched.csv")
#1b
##date_id	video_length_seconds	total_views	avg_watch_time	video_title	content

relative_attention = YoutubeTimeWatched.csv$avg_watch_time

Youtube = read.csv("/Users/landon/Desktop/Grad School/MSCB 5030/YoutubeTimeWatched.csv", head=TRUE)
summary(Youtube)
names(Youtube)
#1c(i)
relative_attention = Youtube$avg_watch_time/Youtube$video_length_seconds

summary(relative_attention)
#1c(ii)
n = 680
sd(relative_attention, na.rm = TRUE)
sd_relative_attention = sd(relative_attention, na.rm = TRUE)
se_relative_attention = sd_relative_attention/(n)^(1/2)
conf_int = 1.96
mean(relative_attention, na.rm = TRUE)

Lower_conf = mean(relative_attention, na.rm = TRUE) - conf_int*se_relative_attention
Upper_conf = mean(relative_attention, na.rm = TRUE) + conf_int*se_relative_attention
c(Lower_conf,Upper_conf)
#1c(iii)
boxplot(relative_attention~Youtube$content, notch = TRUE)
##explanation: Yes; relative_attention varies systematically by content type. Notice the spread on R
#1d(i)

## coming in for office Hours next week to work on R weaknesses************

##Below I attempted to make categorical variables by setting saturday and sunday equal to 1. Clearly this work around did not work and I look forward to seeing the "work-around"
## I tried different ways--no luck
Youtube$date_id[Youtube$date_id = "1/1/12"] = 1
weekend = Youtube$date_id[Youtube$date_id = 1/3/12] = 0
weekend = Youtube$date_id[Youtube$date_id = 1/3/12] = 0
weekend = Youtube$date_id[Youtube$date_id = 1/4/12] = 0
weekend = Youtube$date_id[Youtube$date_id = 1/5/12] = 0
weekend = Youtube$date_id[Youtube$date_id = 1/6/12] = 0
weekend = Youtube$date_id[Youtube$date_id = 1/7/12] = 1  

weekend

boxplot(relative_attention~weekend, notch = TRUE)

Youtube$date_id
#1e(i & ii)
boxplot(Youtube$video_length_seconds~relative_attention)
cor(Youtube$video_length_seconds,relative_attention,na.rm = TRUE)
boxplot(Youtube$total_views~relative_attention, notch =TRUE)
