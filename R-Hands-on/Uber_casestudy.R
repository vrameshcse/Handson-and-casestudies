############# Uber Case Study##################

############# Data Prepartion##################

library(ggplot2)
library(lubridate)
library(scales)

#Create a data frame names ubercase and import the data
ubercase <-
  read.csv(
    "Uber request data.csv",
    na.strings = "NA",
    strip.white = T,
    stringsAsFactors = F
  )

#convert the date/time attriubute to date&Time type
ubercase$Date <- as.Date(ubercase$Date, "%d-%m-%Y")
ubercase$Request.time.converted <-
  strptime(ubercase$Request.time, format = "%T")
ubercase$dropoff.time.converted <-
  strptime(ubercase$Dropoff.time, "%r")

#Question#1
ggplot(ubercase, aes(
  x = sapply(strsplit(ubercase$Request.time, split = ":"), '[', 1),
  fill = factor(Pickup.point)
)) + geom_bar(position = "dodge") + xlab("Hour of the day") +
  ylab("# of request") + ggtitle("Hourwise trip at Airport/City")

#Question 2
# Pre_Morning = 00 to 04 hrs
# Morning_Rush = 05 to 09 hrs
# Day_Time = 10 to 16 hrs
# Evening_Rush = 17 to 21 hrs
# Late_Night = 22 to 23 hrs

ubercase$Time_slot <-
  sapply(hour(strptime(ubercase$Request.time, format = "%T")), function(x)
    ifelse(x >= 0 &
             x <= 4, "Pre_Morning", (ifelse(
               x >= 5 &
                 x <= 9, "Morning_Rush", (ifelse(x >= 10 &
                                                   x <= 16, "Day_Time", (
                                                     ifelse(x >= 17 & x <= 21, "Evening_Rush", "Late_Night")
                                                   )))
             ))))

ubercase$Time_slot <-
  factor(
    ubercase$Time_slot,
    levels = c(
      "Pre_Morning",
      "Morning_Rush",
      "Day_Time",
      "Evening_Rush",
      "Late_Night"
    ),
    ordered = T
  )

ggplot(ubercase, aes(x = Time_slot)) + geom_bar(fill = "#0055ef") +
  geom_text(aes(y = (..count..), label = (..count..), vjust=1),
            stat = "count") + xlab("Time slot") + ylab("# of Request") + ggtitle("Request at various Timeslots")

######################## Problem Identification #############################
# Question 1:
ggplot(ubercase, aes(x = Time_slot)) +
  geom_bar(aes(fill = Status), position = "stack") +
  geom_text(aes(y = (..count..), label = (..count..)), stat = "count", hjust =
              1) +
  facet_wrap( ~ Pickup.point) + coord_flip() + xlab("Time slot") + ylab("# of Request") +
  ggtitle("Trip status Vs Pickup point")

##############################-----Problem1-----##############################
#Problem 1: Number of cancelled trips during morning rush hours

#Question 1: To identify the percentage of request for each status based on the pick up point
ggplot(
  subset(ubercase, ubercase$Time_slot == "Morning_Rush"),
  aes(x = Pickup.point, fill = Status)
) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(
    aes(y = ((..count..) /  sum(..count..)), label = scales::percent((..count..) /
                                                                       sum(..count..))),
    stat = "count",
    vjust = 1,
    hjust = "middle",
    position = "stack",
    alpha = 0.5
  ) +
  scale_y_continuous(labels = percent) + xlab("Pickup point") +
  ylab("% of Request") + ggtitle("Number of request at morning rush")

#Question 2:To identify the number of request for each status based on the pick up point.
ggplot(
  subset(ubercase, ubercase$Time_slot == "Morning_Rush"),
  aes(x = Pickup.point, fill = Status)
) +
  geom_bar(aes(y = (..count..))) +
  geom_text(
    aes(y = (..count..), label = (..count..)),
    stat = "count",
    vjust = 1,
    hjust = "middle",
    position = "stack",
    alpha = 0.5
  ) + xlab("Pickup point") +
  ylab("% of Request") + ggtitle("% or request at morning rush")

# Question 3:
# Mere 20% of the request is from airport out of the total request
# during the morning rush hours. Also, the probability of a driver
# in airport getting a trip back to city during day time hours is
# just 28%(475/1693) and thus, drivers are reluctant to accept rides
# to airport drop which results in trip cancellation

# Question 4:
# 1. Discount the service fee charged by Uber on the trip which may benefit the driver
# and also may help to,
#  i.	Make cars available to customers
#  ii.Reduce trip cancellation
# 2. Provide offers/discounts to customers on Uber share for airport trips

##############################-----End of Problem1-----##############################

##############################-----Problem2-----##############################
#Problem 2: Cars unavailability during Evening rush hours

#Question 1: To identify the percentage of request for each status based on the pick up point
ggplot(
  subset(ubercase, ubercase$Time_slot == "Evening_Rush"),
  aes(x = Pickup.point, fill = Status)
) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(
    aes(y = ((..count..) /  sum(..count..)), label = scales::percent((..count..) /
                                                                       sum(..count..))),
    stat = "count",
    vjust = 1,
    hjust = "middle",
    position = "stack"
  ) +
  scale_y_continuous(labels = percent) + xlab("Pickup point") +
  ylab("% of Request") + ggtitle("Number of request at evening rush")

#Question 2: To identify the number of request for each status based on the pick up point.
ggplot(
  subset(ubercase, ubercase$Time_slot == "Evening_Rush"),
  aes(x = Pickup.point, fill = Status)
) +
  geom_bar(aes(y = (..count..))) +
  geom_text(
    aes(y = (..count..), label = (..count..)),
    stat = "count",
    vjust = 1,
    hjust = "middle",
    position = "stack"
  ) + xlab("Pickup point") +
  ylab("% of Request") + ggtitle("% of request at evening rush")

# Question 3:
# The gap is due the supply to the airport during day time/evening hours
# time slot is less, and the demand is high during evening rush hours.
# During evening hours the demand is 1866 (total request at airport) whereas
# the supply is 424(trips completed from city to airport). Only 31% of the cars
# are available of the total request made at airport during evening rush hours.
# Also, as more number of requests is cancelled during the morning rush hours
# & less demand to airport trip during day time hours also contributes to the
# demand supply gap

# Question 4:
# 1. Discount the service fee charged by Uber on the trip
#  i. during the evening rush hours which will make idle drivers in city to drive towards airport finding for a trip during end of day time hours as the probability of getting a trip from airport is high
#  ii.during the Day time hours for the trips made within airport that will make them engaged till evening rush hours.
# 2. Provide specific offers/discounts on Uber share to customers who avail trip from airport

##############################-----End of Problem2-----##############################
