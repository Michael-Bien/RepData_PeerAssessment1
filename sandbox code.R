setwd("C:\\Users\\Natal\\OneDrive\\Documents\\Mike\\Reproducible Research\\Week2")
library(dplyr)
library(ggplot2)

#1:  Load the data

activity <- read.csv("activity.csv")
str(activity) #steps (int) | date (chr) | interval (int)

#2: Process/Transform the data
#I did this becaues the date field imported as a character field, but is a date

activity$date <- as.Date(activity$date)

#1. Calculate total steps per day

steps_per_day <- activity %>%
  group_by(date) %>%
  summarize(daily_total = sum(steps, na.rm=TRUE))

#date(date) | daily_total (integer)

#2. Make a histogram of the steps taken per day

hist(steps_per_day$daily_total, breaks=10, main="Histogram of average steps per day", xlab="Steps per day")

#3. Calculate and report the mean and median of the total number of steps taken per day

daily_mean <- round(mean(steps_per_day$daily_total, na.rm=TRUE))
daily_median <- median(steps_per_day$daily_total, na.rm=TRUE)


#1. Make a time series plot of the 5 minute interval (x axis) and the average number of steps taken, averaged across all days (y axis)
#start by summarizing the average steps per interval

interval_summary <- activity %>%
  group_by(interval) %>%
  summarize(ave_steps = mean(steps, na.rm=TRUE))


#the plot
with(interval_summary,
     {plot(interval, ave_steps, type="l", main = "Average Steps per Interval", xlab="Time Interval", ylab = "Average Number of Steps")})

#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

# interval_max is the row from interval_summary that has the max average steps
interval_max <- interval_summary[which.max(interval_summary$ave_steps),]
interval_max

#1. Calculate and report the total number of missing values in the dataset

sum(is.na(activity$steps))

#2. Devise a strategy for filling in all of the missing values in the dataset.

#   Here we apply a simple strategy to fill missing values with the mean value for that interval.

steps_per_interval <- activity %>%
  group_by(interval) %>%
  summarize(interval_mean = round(mean(steps, na.rm=TRUE)))
#steps_per_interval
#interval (int) | interval_mean (num)
#Activity
#steps (int) | date (chr) | interval (int)

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity_w_mean <- merge(activity, steps_per_interval, by="interval")
#interval | steps | date | interval_mean
#interval_mean is numeric

#fill in NA values

activity_w_mean$steps <- with(activity_w_mean, {
  ifelse(!is.na(steps) , #not an NA
         steps, #keep steps as is
         interval_mean) #update to the mean value
} #end the with clause
) #end the with statement



#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

steps_per_day_imputed <- activity_w_mean %>%
  group_by(date) %>%
  summarize(daily_total = sum(steps, na.rm=TRUE))
str(steps_per_day_imputed)

hist(steps_per_day_imputed$daily_total, breaks=10, main="Histogram of steps per day (imputed where NA)", xlab="Steps per day")

daily_mean2 <- round(mean(steps_per_day_imputed$daily_total, na.rm=TRUE))
daily_median2 <- median(steps_per_day_imputed$daily_total, na.rm=TRUE)
daily_mean
daily_mean2  #this is the same, but I don't believe that.
daily_median
daily_median2  #this is the same, but I don't believe that.

#1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

activity_w_mean$weekend <- with(activity_w_mean, {
  ifelse(weekdays(date) %in% c("Saturday", "Sunday"),
         "weekend",
         "weekday")
})



#2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

steps_by_daytype <- activity_w_mean %>%
  group_by(weekend, interval) %>%
  summarize(interval_mean = round(mean(steps, na.rm=TRUE)))

ggplot(steps_by_daytype, aes(interval, interval_mean)) +
  ggtitle("Activity Pattern -- weekday vs. weekend") +
  ylab("Mean number of steps") +
  xlab("Time Interval") +
  geom_line() +
  facet_grid(rows=vars(weekend))