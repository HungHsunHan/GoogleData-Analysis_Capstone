library(lubridate)
library(dplyr)
library(ggplot2)
month_4 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202004-divvy-tripdata.csv")
month_5 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202005-divvy-tripdata.csv")
month_6 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202006-divvy-tripdata.csv")
month_7 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202007-divvy-tripdata.csv")
month_8 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202008-divvy-tripdata.csv")
month_9 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202009-divvy-tripdata.csv")
month_10 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202010-divvy-tripdata.csv")
month_11 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202011-divvy-tripdata.csv")
month_12 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202012-divvy-tripdata.csv")
month_1 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202101-divvy-tripdata.csv")
month_2 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202102-divvy-tripdata.csv")
month_3 <- read.csv("/Users/hanhongxun/Desktop/Google Data Analysis/case study1/202103-divvy-tripdata.csv")

# change data type from char to numeric for certain files
month_12$start_station_id <-as.numeric(month_12$start_station_id)
month_1$start_station_id <-as.numeric(month_1$start_station_id)
month_2$start_station_id <-as.numeric(month_2$start_station_id)
month_3$start_station_id <-as.numeric(month_3$start_station_id)
month_12$end_station_id <-as.numeric(month_12$end_station_id)
month_1$end_station_id <-as.numeric(month_1$end_station_id)
month_2$end_station_id <-as.numeric(month_2$end_station_id)
month_3$end_station_id <-as.numeric(month_3$end_station_id)

all_data <- bind_rows(month_5,month_6,month_7,month_8,month_9,month_10,month_11,month_12,month_1,month_2,month_3,month_4)

all_data<- all_data %>% 
  mutate(started_at = ymd_hms(started_at)) %>% 
  mutate(ended_at = ymd_hms(ended_at)) %>% 
  mutate(length_of_ride = ended_at-started_at)

library("hydroTSM")
all_data$seasons <- time2season(all_data$started_at,out.fmt = "seasons")

# replace value where value<0 
all_data$length_of_ride[all_data$length_of_ride<0]<-all_data$length_of_ride[all_data$length_of_ride<0]*-1

# convert weekdays to number
all_data$day <- weekdays(as.Date(all_data$started_at))
all_data$day_num <- wday(as.Date(all_data$started_at))


# find mode of weekdays by using function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mean_length <-mean(as.numeric(all_data$length_of_ride))
max_length <-max(as.numeric(all_data$length_of_ride))
mode_day <- Mode(all_data$day)

casual_rider_tabel <- all_data %>% 
  filter(member_casual =="casual")

member_rider_tabel <- all_data %>% 
  filter(member_casual =="member")


mean_length_casual <- mean(as.numeric(casual_rider_tabel$length_of_ride))
  
mean_length_member <- mean(as.numeric(member_rider_tabel$length_of_ride))
mode_day_casaul <- Mode(casual_rider_tabel$day)
mode_day_member <- Mode(member_rider_tabel$day)

day_casual <- casual_rider_tabel %>% 
  group_by(day,day_num) %>% 
  summarize(Sconds = mean(as.numeric(length_of_ride)))  
day_casual$Type <-"Casual"


day_member <- member_rider_tabel %>% 
  group_by(day,day_num) %>% 
  summarize(Sconds = mean(as.numeric(length_of_ride)))
day_member$Type <-"Member"

combined <- bind_rows(day_casual,day_member)

# Group by season
season_casual <- casual_rider_tabel %>% 
  group_by(seasons) %>% 
  summarize(Sconds = mean(as.numeric(length_of_ride)))  
season_casual$Type <-"Casual"


season_member <- member_rider_tabel %>% 
  group_by(seasons) %>% 
  summarize(Sconds = mean(as.numeric(length_of_ride)))
season_member$Type <-"Member"

season_combined <- bind_rows(season_casual,season_member)



# Col plot: x=, y= can be excluded like aes(wday(day_num,label = TRUE),Sconds, fill = Type)
ggplot(data = combined) + geom_col(aes(x = wday(day_num,label = TRUE), y = Sconds, fill = Type),position='dodge')+
  labs(x="Weekdays", y = "Avg Length", title="Average Ride length for users by Weekdays")+
  theme(plot.title = element_text(hjust = 0.5)) 


# Barplot: avg_length by day, stat='identity' is to present values instead of counts!!! By default is "bin" for counts
# Use "dodge" to show indivisual heights rather than stacking together by default 
## wday: convert number to Weekdays
ggplot(data = all_data)+ geom_bar(mapping = aes(wday(day_num,label = TRUE),fill =member_casual), position='dodge')+
  labs(x="Weekdays", y = "Cont", title="Number of rides for users by Weekdays")+
  theme(plot.title = element_text(hjust = 0.5)) 



## group by seasons
ggplot(data = season_combined) + geom_col(aes(x = seasons, y = Sconds, fill = Type),position='dodge')+
  labs(x="Seasons", y = "Avg Length", title="Average Ride length for users by Seasons")+
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data = all_data)+ geom_bar(mapping = aes(seasons,fill =member_casual), position='dodge')+
  labs(x="Seasons", y = "Cont", title="Number of rides for users by Seasons")+
  theme(plot.title = element_text(hjust = 0.5)) 


## group by biketype

ggplot(data = all_data)+ geom_bar(mapping = aes(rideable_type,fill =member_casual), position='dodge')+
  labs(x="Bike types", y = "Cont", title="Number of rides for users by Bike type")+
  theme(plot.title = element_text(hjust = 0.5)) 
