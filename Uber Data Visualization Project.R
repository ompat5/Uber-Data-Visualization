library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)


# Colour vector to be used when creating data visuals
colours = c("#f18719", "#665555", "#54278d", "#37a3b3", "#f5e840", "#7ad414", "#0ef890")

            
# Manipulating Data 
aprData <- read.csv(file = "uber-raw-data-apr14.csv")
mayData <- read.csv(file = "uber-raw-data-may14.csv")
junData <- read.csv(file = "uber-raw-data-jun14.csv")
julData <- read.csv(file = "uber-raw-data-jul14.csv")
augData <- read.csv(file = "uber-raw-data-aug14.csv")
sepData <- read.csv(file = "uber-raw-data-sep14.csv")

data2014 <- rbind(aprData,mayData, junData, julData, augData, sepData)

data2014$Date.Time <- as.POSIXct(data2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data2014$Time <- format(as.POSIXct(data2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

data2014$Date.Time <- ymd_hms(data2014$Date.Time)

data2014$day <- factor(day(data2014$Date.Time))
data2014$month <- factor(month(data2014$Date.Time, label = TRUE))
data2014$year <- factor(year(data2014$Date.Time))
data2014$dayofweek <- factor(wday(data2014$Date.Time, label = TRUE))

data2014$hour <- factor(hour(hms(data2014$Time)))
data2014$minute <- factor(minute(hms(data2014$Time)))
data2014$second <- factor(second(hms(data2014$Time)))

# 1. Trips for Every Hour
hourData <- data2014 %>%
  group_by(hour) %>%
    dplyr::summarize(Total = n())
datatable(hourData)

ggplot(hourData, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "blue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# 2. Trips by Hours and Months
monthHour <- data2014 %>%
  group_by(month, hour) %>%
    dplyr::summarize(Total = n())

ggplot(monthHour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

# 3. Trips for Days in Month

dayGroup <- data2014 %>%
  group_by(day) %>%
    dplyr::summarize(Total = n()) 
datatable(dayGroup)

ggplot(dayGroup, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "blue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# 4. Trips by Days and Months
dayMonthGroup <- data2014 %>%
  group_by(month, dayofweek) %>%
    dplyr::summarize(Total = n())

ggplot(dayMonthGroup, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colours)

# 5. Trips in Months of Year
monthGroup <- data2014 %>%
  group_by(month) %>%
    dplyr::summarize(Total = n()) 
datatable(monthGroup)

ggplot(monthGroup, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colours)

# 6. Trips by Bases
ggplot(data2014, aes(Base)) + 
  geom_bar(fill = "red") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

# 7. Trips by Bases and Months
ggplot(data2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colours)

# 8. Trips by Bases and Day of Week
ggplot(data2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colours)

# 9. HeatMap of Day and Hour
dayHourGroup <- data2014 %>%
  group_by(day, hour) %>%
    dplyr::summarize(Total = n())
datatable(dayHourGroup)

ggplot(dayHourGroup, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

# 10. HeatMap of Day and Month
ggplot(dayMonthGroup, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

# 11. Geo-Plot of ride in New York
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")