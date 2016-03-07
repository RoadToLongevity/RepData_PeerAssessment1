# ---
#   title: "Reproducible Research: Peer Assessment 1"
# output: 
#   html_document:
#   keep_md: true
# ---

## Initial Setup

# The ggplot2 package is required for this script and can be installed with:
# install.packages("ggplot2")
require(ggplot2)
require(lubridate)
require(dplyr)

# Set working directory to directory containing the data and script
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)


## Loading and preprocessing the data

activity <- read.csv(unz("activity.zip", "activity.csv"))
# clean activity with no NA values:
#activity_no_na <- na.omit(activity_raw)
# clean activity with NA values in steps column set to 0
# activity_na_zero <- activity
# activity_na_zero$steps[is.na(activity_na_zero$steps)] <- 0

activity$date <- as.Date(activity$date)

## What is mean total number of steps taken per day?
steps_per_date <- aggregate(steps~date, activity, sum)

steps_per_date_histogram <-
  hist(steps_per_date$steps, main="Histogram of Steps",
       xlab="Steps", ylab="Frequency")
steps_per_date_mean <- mean(steps_per_date$steps)
steps_per_date_median <- median(steps_per_date$steps)


## What is the average daily activity pattern?
steps_per_interval <- aggregate(steps~interval, activity, mean)
steps_per_interval_ts <-
  plot(x=steps_per_interval$interval, y=steps_per_interval$steps, type="l",
       main="Time Series of Average Steps", xlab="Interval", ylab="Steps")
# don't try editing what's below.  Yes it's horrible.
interval_with_max_mean_steps <- 
  steps_per_interval[steps_per_interval$steps == max(steps_per_interval$steps),]$interval
# most steps get taken around 8:35am


## Imputing missing values
number_of_rows_with_na <- sum(is.na(activity$steps))
print(sprintf("The number of rows with NA is: %04d", number_of_rows_with_na))

activity_na_corrected <- activity
all_na <- is.na(activity_na_corrected$steps)
acivity_avg <- tapply(activity_na_corrected$steps, activity_na_corrected$interval, mean, na.rm=TRUE)
activity_na_corrected$steps[all_na] <- acivity_avg[as.character(activity_na_corrected$interval[all_na])]

steps_per_date_corrected <- aggregate(steps~date, activity_na_corrected, sum)
steps_per_date_histogram_corrected <-
  hist(steps_per_date_corrected$steps, main="Histogram of Steps Corrected",
       xlab="Steps", ylab="Frequency")
steps_per_date_mean_corrected <- mean(steps_per_date_corrected$steps)
steps_per_date_median_corrected <- median(steps_per_date_corrected$steps)
# very little difference in the overall mean and median of steps per date
# for initial activity vs activity with na replaced with mean values per interval


## Are there differences in activity patterns between weekdays and weekends?


for (i in 1:nrow(activity_na_corrected)) {
  if (weekdays(activity_na_corrected$date[i]) == "Saturday" |
      weekdays(activity_na_corrected$date[i]) == "Sunday") {
    activity_na_corrected$daytype[i] = "weekend"
  } else {
    activity_na_corrected$daytype[i] = "weekday"
  }
}


activity_no_na <- activity_na_corrected %>%
  group_by(interval, daytype) %>%
  summarize(steps = mean(steps))

panelplot <- ggplot(activity_no_na, aes(x=interval, y=steps, color = daytype)) +
  geom_line() +
  facet_wrap(~daytype, ncol = 1, nrow=2)

print(panelplot)


