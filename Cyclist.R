#load libraries
install.packages("tidyverse")
library(tidyverse) 
library(lubridate) #dates
library(readr) #load CSVs
library(ggplot2) #plotting
library(dplyr)

#reading all required files
aug_22 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202208-divvy-tripdata.csv")
sep_22 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202209-divvy-publictripdata.csv")
oct_22 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202210-divvy-tripdata.csv")
nov_22 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202211-divvy-tripdata.csv")
dec_22 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202212-divvy-tripdata.csv")
jan_23 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202301-divvy-tripdata.csv")
feb_23 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202302-divvy-tripdata.csv")
mar_23 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202303-divvy-tripdata.csv")
apr_23 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202304-divvy-tripdata.csv")
may_23 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202305-divvy-tripdata.csv")
jun_23 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202306-divvy-tripdata.csv")
jul_23 <- read_csv("~/OneDrive/DATA/DOCS - Education/TEMPS/Data_Analytics Certification_GOOGLE/cap_stone/202307-divvy-tripdata.csv")

#combining individual files of the year into 1 large table
all_trips <- rbind(aug_22, sep_22, oct_22, nov_22, dec_22, jan_23, feb_23, mar_23, apr_23, may_23, jun_23, jul_23)
colnames(all_trips)

unique(all_trips$rideable_type)

#replacing docked_bike with classic_bike
all_trips <- all_trips %>%
  mutate(rideable_type = ifelse(rideable_type == 'docked_bike', 'classic_bike', rideable_type))
unique(all_trips$rideable_type)

# Remove rows with 'classic_bike' and null start or end station
filtered_all_trips <- all_trips %>%
  filter(!(rideable_type == 'classic_bike' & (is.na(start_station_name) | is.na(end_station_name))))

#remove rows with no lat or lng values
all_trips <- all_trips[!is.na(all_trips$start_lat), ]
all_trips <- all_trips[!is.na(all_trips$start_lng), ]
all_trips <- all_trips[!is.na(all_trips$end_lat), ]
all_trips <- all_trips[!is.na(all_trips$end_lng), ]

#creating a new column called ride_length
ride_length <- ((all_trips$ended_at - all_trips$started_at)/60)
all_trips$ride_length <- as.numeric(ride_length)

#creating a new column showing the specific day of rental
all_trips$day_of_week <- wday(all_trips$started_at, label = TRUE, week_start = 1)

#creating a new column showing the specific month of rental
all_trips$rental_month <- month(all_trips$started_at, label = TRUE)

#removing values of ride_length less than 1 min and more than 24 hours
all_trips <- all_trips[all_trips$ride_length > 1, ]
all_trips <- all_trips[all_trips$ride_length < 1440, ]

#removing all rows with null values since they will not help with further analysis
all_trips <- na.omit(all_trips)

#removing columns start_station_id and end_station_id
all_trips <- select(all_trips, -start_station_id, -end_station_id)

# Total Rides in the Year - Casual vs Member
all_trips %>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual)  %>% 
  ggplot(aes(x = member_casual, 
             y = number_of_rides, 
             fill = member_casual)) + 
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Total Rides per Year', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Rider Group') + ylab('Number of Rides') +
  geom_text(aes(label = number_of_rides), vjust = 0)

#Total rides in the year by month - Casual vs Member
all_trips %>%
  group_by(member_casual, rental_month) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = rental_month,
             y = number_of_rides,
             fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(fill = 'Rider Group',
       title = 'Total Rides per Month',
       subtitle = 'Casual Riders vs Members',
       x = 'Month',
       y = 'Number of Rides') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = number_of_rides),
            position = position_dodge(width = 0.9),
            size = 3, angle = 90,
            hjust = "left")

#Average trip duration by member/casual
all_trips %>% 
  group_by(member_casual) %>% 
  summarise(average_duration = mean(ride_length)) %>% 
  ggplot(aes(x = member_casual, 
             y = average_duration, 
             fill = member_casual)) +
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Average Trip Duration', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Rider Group') + ylab('Trip Duration in Minutes') +
  geom_text(aes(label = round(average_duration, digits = 2)), vjust = 0)

#Average trip duration per month by member/casual
all_trips %>% 
  group_by(member_casual, rental_month) %>% 
  summarise(average_duration = mean(ride_length)) %>% 
  ggplot(aes(x = rental_month, 
             y = average_duration, 
             fill = member_casual)) +
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Average Trip Duration by Month', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Month') + ylab('Trip Duration in Minutes') +
  geom_text(aes(label = round(average_duration, digits = 2), hjust = "right"), position = position_dodge(width = 0.9),size = 3, angle = 90)

#average trip duration by days of the week
all_trips %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week)  %>%
  ggplot(aes(x = day_of_week, 
             y = average_duration, 
             fill = member_casual)) +
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Average Trip Duration by Weekday', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Day of Week') + ylab('Trip Duration in Minutes') +
  geom_text(aes(label = round(average_duration, digits = 2), hjust = "right"), position = position_dodge(width = 0.9),size = 3, angle = 90)

#Bike Preference per Rider Group
all_trips %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = rideable_type, 
             y = number_of_rides, 
             fill = member_casual)) + 
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Total Rides per Year by Bike Type', subtitle = 'Casual Riders vs Members') +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Bike Type') + ylab('Number of Rides') +
  geom_text(aes(label = number_of_rides), position = position_dodge(width = 0.9), vjust = 0)

#Average trip duration per bike type
all_trips %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = rideable_type, 
             y = average_duration, 
             fill = member_casual)) +
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Average Trip Duration by Bike Type', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Bike Type') + ylab('Trip Duration in Minutes') +
  geom_text(aes(label = round(average_duration, digits = 2)), position = position_dodge(width = 0.9), vjust = 0)
