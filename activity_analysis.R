# Loading library
library(dplyr)
library(ggplot2)
# Unzip files
unzip(zipfile = "activity.zip")

# Load file to the workspace
activity <- read.csv("activity.csv", header = TRUE)

# Check the data set
head(activity)
str(activity)

# change the class of date and check the data set
activity$date <- as.Date(as.character(activity$date, "%Y%m%d"))
str(activity)

# Sum the number of steps per day
steps_count <- activity %>% group_by(date) %>% summarize(total_steps = sum(steps, na.rm = TRUE))
# More detailed analysis of steps per day
# steps_details <- activity %>% group_by(date) %>% summarize(total_steps = sum(steps, na.rm = TRUE), avg_steps = mean(steps, na.rm = TRUE), med_steps = median(steps, na.rm = TRUE))
# Print a summary of the data frame steps_count
summary(steps_count)
# Plot in histogram of the frequency of the total number of steps per day and save it in png
png(filename = "plot1.png")

with(steps_count, hist(total_steps, breaks = 30, col = "springgreen3"))
rug(steps_count$total_steps)

dev.off()

# Summary of the total number of steps per day
summary(steps_count)

# Average of steps per interval
steps_mean_interval <- activity %>% group_by(interval) %>% summarize(mean_steps = mean(steps, na.rm = TRUE))

# Plot in time series of the averaged number of steps per 5 min interval and save it in png
png(filename = "plot2.png")

with(steps_mean_interval, plot(x = interval, y = mean_steps, type = "l", col = "springgreen3", xlab = "Intervals", ylab = "Averaged steps"))
title("Averaged of steps per interval")

dev.off()

# Determine the interval where the max averaged number of steps
steps_mean_interval[which(steps_mean_interval$mean_steps == max(steps_mean_interval$mean_steps)), ]

# Missing value in the entire dataset percentage
missing_values <- 100 * mean(is.na(activity$steps))

## Replace the missing values by the average number of steps for their respective interval.
# length of the interval 
# int_len <- length(unique(activity$interval))
# length of the interval 
date_len <- length(unique(activity$date))
# Generate a vector steps with the mean of each day of the length of the data frame
steps_mean_int_temp <- rep(steps_mean_interval$mean_steps, each = date_len)
# Data frame activity group by date and interval
activity_new <- activity %>% arrange(date, interval)
# index Na value in the data frame steps_mean_interval
idx <- which(is.na(activity_new$steps))
# Replace NA by average of the number of steps per interval
activity_new$steps[idx] = steps_mean_int_temp[idx]
# Update steps count with the new data frame
steps_count <- activity_new %>% group_by(date) %>% summarize(total_steps = sum(steps, na.rm = TRUE))

# Plot in histogram of the frequency of the total number of steps per day and save it in png
png(filename = "plot3.png")

with(steps_count, hist(total_steps, breaks = 30, col = "springgreen3"))
rug(steps_count$total_steps)

dev.off()
# get a summary of the new data frame
summary(steps_count$total_steps)

# Define patterns
activity_new <- activity_new %>% mutate(days = weekdays(date))

# Create identifiers table
days <- unique(activity_new$days)
classifier <- rep(c("weekday", "weekend"), c(5,2))
week_identifier <- cbind(days,classifier)

activity_new <- merge(activity_new, week_identifier, by = "days", all.x = TRUE)
head(activity_new)

# Average of steps per interval
steps_mean_interval_new <- activity_new %>% group_by(interval, classifier) %>% summarize(mean_steps = mean(steps, na.rm = TRUE))

png(filename = "plot4.png")

g <- ggplot(steps_mean_interval_new, aes(interval, mean_steps))
g + geom_line(aes(color = classifier)) + facet_grid(classifier~.) + labs(x = "Interval", y = "Average steps", title = "Average steps for each 5-min interval")

dev.off()

