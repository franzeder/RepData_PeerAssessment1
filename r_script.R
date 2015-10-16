###
# Reproducable Research -- Peer Assessment 1
###

setwd("/Volumes/USBmain/Forschung/R/coursera/reproducable research/PA 1/RepData_PeerAssessment1")
require(dplyr)
require(ggplot2)

# 1. Loading and Preprocessing the data-----------------------------------------
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
activity <- select(activity, date, interval, steps)

# 2. What is the mean total number of steps taken per day?----------------------

# 2.1. Calculate total number of steps per day
total.day <- activity %>%
        na.omit() %>%
        group_by(date) %>%
        summarise(total.steps = sum(steps))
                          

# 2.2. Make a histrogram of the total number of steps taken each day
ggplot(total.day, aes(x = factor(date), weight = total.steps)) +
        geom_histogram(fill="#2b8cbe") +
        ggtitle("Total number of steps taken per day") +
        # name x and y axis
        xlab("Date") + ylab("number of steps") +
        # change background color and color of grid, remove legend
        theme(panel.background = element_rect(fill = "#FFFFFF", colour="#000000"), 
              panel.grid.major = element_line(colour = "#bdbdbd", linetype = "dotted"),
              axis.text.x = element_text(angle = 90, hjust = 1), 
              legend.position="none")

# 2.3. Calculate and report the mean and the median of the total number of steps taken
## per day.
mean.day <- round(mean(total.day$total.steps), 2)

median.day <- median(total.day$total.steps)



# 3. What is the average daily activity pattern?

# 3.1. Make a time series plot (i.e. type = "l") of the five-minute interval (x-axis)
## and the average number of steps taken, averaged across all day (y-axis)
steps.interval <- activity %>%
        na.omit() %>%
        group_by(interval) %>%
        summarise(mean.steps = mean(steps))

ggplot(steps.interval, aes(x = interval, y = mean.steps)) +
        geom_line(aes(col = "#e41a1c")) +
        # add title
        ggtitle("Average number of steps per interval across all days") +
        # name x and y axis
        xlab("Interval") + ylab("average number of steps") +
        # change breaks of x-axis
        scale_x_continuous(breaks = seq(0, 2355, 125)) +
        # change background color and color of grid, remove legend
        theme(panel.background = element_rect(fill = "#FFFFFF", colour="#000000"), 
              panel.grid.major = element_line(colour = "#bdbdbd", linetype = "dotted"),
              legend.position="none")

# 3.2. Which 5-minute interval, on average across all the days in the dataset, contains
## the maximum number of steps?
max.intervall <- steps.interval$interval[which.max(steps.interval$mean.steps)]
max.steps <- steps.interval$mean.steps[which(steps.interval == max.intervall)]


# 4. Inputing missing values

# 4.1. Calculate and report the total number of mussing values in the dataset
missing <- sum(is.na(activity$steps))

# 4.2. Devise a strategy for filling in all the missing values in the dataset.
# 4.3. Create a new dataset that is equal to the original but with the missing
## data filled in.

activity2 <- activity
for (i in 1:nrow(activity2)){
       if (is.na(activity2$steps[i])){
                df <- subset(steps.interval, interval == activity2$interval[i])
                activity2$steps[i] <- df$mean.steps    
       }
}
head(activity2)

# 4.4 Make a histogram of the total number of steps taken each day and calculate
## and report the MEAN and MEDIAN total number of steps taken per day.

total.day2 <- activity2 %>%
        group_by(date) %>%
        summarise(total.steps = sum(steps))

ggplot(total.day2, aes(x = factor(date), weight = total.steps)) +
        geom_histogram(fill="#2b8cbe") +
        ggtitle("Total number of steps taken per day (including estimation for missing values") +
        # name x and y axis
        xlab("Date") + ylab("number of steps") +
        # change background color and color of grid, remove legend
        theme(panel.background = element_rect(fill = "#FFFFFF", colour="#000000"), 
              panel.grid.major = element_line(colour = "#bdbdbd", linetype = "dotted"),
              axis.text.x = element_text(angle = 90, hjust = 1), 
              legend.position="none")

mean.day2 <- mean(total.day2$total.steps)

median.day2 <- mean(total.day2$total.steps)

# 5. Are there differences in activity patterns between weekdays and weekends?

# 5.1. Create a new varibale in the dataset with two levels - "weekday" and
## "weekend" indicating whether a given date is a weekday or weekend day
activity.days <- activity2
activity.days$wdwe <- NA
for (i in 1:nrow(activity.days)){
        day <- weekdays(activity.days$date[i])
        if(day %in% c("Samstag", "Sonntag")){
                activity.days$wdwe[i] <- "weekend"}
        else
                {activity.days$wdwe[i] <- "weekday"}
}
activity.days$wdwe <- as.factor(activity.days$wdwe)
str(activity.days)

# 5.2. Make a panel plot containing a time series plot of the 5-minute interval
## (x-axis) and the average number of steps taken, averaged across all weekday
## days and weekend days (y-axis).
steps.interval2 <- activity.days %>%
        group_by(interval, wdwe) %>%
        summarise(mean.steps = mean(steps))

ggplot(steps.interval2, aes(x = interval, y = mean.steps)) +
        geom_line(aes(col = "#e41a1c")) +
        # two panels
        facet_grid(wdwe ~ .) +
        # add title
        ggtitle("Average number of steps per interval across all days (weekdays/weekend") +
        # name x and y axis
        xlab("Interval") + ylab("average number of steps") +
        # change breaks of x-axis
        scale_x_continuous(breaks = seq(0, 2355, 125)) +
        # change background color and color of grid, remove legend
        theme(panel.background = element_rect(fill = "#FFFFFF", colour="#000000"), 
              panel.grid.major = element_line(colour = "#bdbdbd", linetype = "dotted"),
              legend.position="none")



```{r setup}
knitr::opts_chunk$set(cache = TRUE)
```