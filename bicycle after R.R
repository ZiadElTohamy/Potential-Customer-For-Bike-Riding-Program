install.packages("tidyverse")
install.packages("dplyr")
install.packages("rgl")
install.packages("qpcR")   
install.packages("ggplt2")
install.packages("lubridate")
install.packages("magrittr")
library('magrittr')
library("rgl")
library("qpcR")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("lubridate")

options(rgl.useNULL = TRUE) ## needed to bind data

#read in the past month of data

x12<-read_csv("~/Desktop/case study bicycle/202208-divvy-tripdata.csv")

total <- rbind(x12)
View(total)

## removing rows in data that has null in it
totalC <- na.omit(total) 
View(totalC)

##arranging data due to start date
arrange(totalC,started_date)
View(totalC)

##filter data to only select the data where start time is smaller than end time
dataTC<- (totalC$started_at < totalC$ended_at) 

total_trips<-subset(totalC,dataTC) 
View(total_trips)

##defined the two columns as string data type
total_trips<- mutate(total_trips, trip_id = as.character(ride_id)
           ,rideable_type = as.character(rideable_type))

##arrange data according to trip id
arrange(total_trips,trip_id)
View(total_trips)

total_trips <-  total_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"member" = "Subscriber"
                                ,"casual" = "Customer"))
View(total_trips)

total_trips$date <- as.Date(total_trips$started_at) #The default format is yyyy-mm-dd
total_trips$month <- format(as.Date(total_trips$date), "%m")
total_trips$day <- format(as.Date(total_trips$date), "%d")
total_trips$year <- format(as.Date(total_trips$date), "%Y")
total_trips$day_of_week <- format(as.Date(total_trips$date), "%A")

View(total_trips)

total_trips$ride_length <- (difftime(total_trips$ended_at,total_trips$started_at, units ="mins"))

total_trips<- mutate(total_trips, ride_length = as.numeric(ride_length)) #changing the ride length from fact to numeric

str(total_trips) #checking the structure of dataset

is.numeric(total_trips$ride_length) #check if all ride length is numeric

mean(total_trips$ride_length) #straight average (total ride length / rides)
median(total_trips$ride_length) #midpoint number in the ascending array of ride lengths
max(total_trips$ride_length) #longest ride
min(total_trips$ride_length) #shortest ride

summary(total_trips$ride_length)

aggregate(total_trips$ride_length ~ total_trips$member_casual, FUN = mean)        
aggregate(total_trips$ride_length ~ total_trips$member_casual, FUN = max)
aggregate(total_trips$ride_length ~ total_trips$member_casual, FUN = min)
aggregate(total_trips$ride_length ~ total_trips$member_casual, FUN = median)

aggregate(total_trips$ride_length ~ total_trips$member_casual + total_trips$day_of_week, FUN = mean)


View(total_trips)

# See the average ride time by each day for members vs casual users
aggregate(total_trips$ride_length ~ total_trips$member_casual + total_trips$day_of_week, FUN = mean)

# Notice that the days of the week are out of order.
total_trips$day_of_week <- ordered(total_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# the average ride time by each day for members vs casual users
aggregate(total_trips$ride_length ~ total_trips$member_casual + total_trips$day_of_week, FUN = mean)


eff_trips_duration <-(total_trips$ride_length> 12) 
View(eff_trips_duration)
accurate_trips <-subset(total_trips,eff_trips_duration) 

##defining riders category based on their ride time if below 15 then he's not a potential customer to plan to add him to a member
Customer_def<- accurate_trips %>% 
  mutate(potential_cust = case_when(ride_length > 15 & member_casual=='Customer' ~ "Potential",
                                    ride_length < 15 & member_casual=='Customer' ~ "Non Potential",
                                    member_casual=='Subscriber' ~ "Member"))
#adding the data to new table after filtering out the non potential customers to focus on promoting to valued potential customers
potential_customer <- Customer_def$potential_cust != "Non Potential"
potential_customer<- subset(Customer_def,potential_customer) 

View(potential_customer)

# insights to get more info between potential customers and actual members
aggregate(potential_customer$ride_length ~ potential_customer$potential_cust , FUN = mean)

sum(potential_customer$potential_cust=="Potential")/sum(potential_customer$potential_cust=="Member")

#checking if there is different spelling in entries of ride-type type before aggregating
unique(potential_customer$rideable_type) # only three entries so it's ready to be aggregated

#average electric bikes used 
sum(potential_customer$rideable_type=="electric_bike")/(sum(potential_customer$rideable_type=="classic_bike")+sum(potential_customer$rideable_type=="docked_bike"))

# visualizing between potential and members about their use of bicycle through the weekdays
potential_customer %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(potential_cust, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(potential_cust, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = potential_cust)) +
  geom_col(position = "dodge") + labs(title = "Use of bicycle Through  Weekdays")


# create a visualization for average duration
potential_customer %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(potential_cust, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(potential_cust, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = potential_cust)) +
  geom_col(position = "dodge") + labs(title = "Average Duration of Rides")



  
write.csv(potential_customer, "Cyclistic.csv")
