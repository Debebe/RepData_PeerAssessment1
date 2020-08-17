


library(readr)
library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)




##Set the working directory to a folder that contains the data


setwd("~/Documents/GitHub/RepData_PeerAssessment1")



## Loading and preprocessing the data




data <- fread("curl https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip | funzip")

## What is mean total number of steps taken per day?

#The total number of steps taken per day is **570,608**  

  
total_number_steps_per_day <- data[, .(steps_day= sum(steps)), by= date]
head(total_number_steps_per_day)


#Excluding missing data


missing_excluded <-na.omit(total_number_steps_per_day$steps_day)
sum(missing_excluded)


### 2. Histogram of the total number of steps taken each day



total_number_steps_per_day_NA <- data[, .(steps_day= sum(steps)), by= date]

a<-qplot(total_number_steps_per_day_NA$steps, geom="histogram", main='Number of steps-with NA excluded',
         fill=I('pink'), col=I('pink'), alpha=I(0.5))  + xlab('number of steps') + ylab('density')

ggsave(a, file = 'fig.1_histogram_of_total_number_of_steps_day.png')


### 3. Mean and median number of   each day 

#The mean and median steps taken per day is **10766.19** and **10765** steps respectively.

#### Mean


mean(missing_excluded)



#### Median


median(missing_excluded)



## What is the average daily activity pattern?
#Here I am calculating mean steps by 5-minute intervals

time_series <-data %>%
  replace(is.na(.), 0) %>%  # replace NAs with 0 for computation
  group_by(interval) %>%
  summarise(steps = mean(steps))
head(time_series)


#1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**
  

ggplot(time_series, aes(interval, steps)) + geom_line(col='red') +xlab('5- Minute intervals') + 
  ylab('Average steps taken ~ daily') +ggtitle("Timeseries plot")
ggsave('fig.2_time_series_plot_of_5_munte_interval.png')





#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**
  
  
interval_at_max_steps <- time_series$interval[which.max(time_series$steps)]
interval_at_max_steps



## Imputing missing values

sum(is.na(data$steps)) 
#Imputation- the mean over the entire variable is used to impute missing values


dat <- data


# uses mean from all dates where values are recorded
dat$steps[is.na(dat$steps)] <- mean(dat$steps, na.rm=TRUE)

total_number_steps_per_day_noNA <- dat[, .(steps_day= sum(steps)), by= date]

b<-qplot(total_number_steps_per_day_noNA$steps, geom="histogram", main='Number of steps-no NA',
      fill=I('pink'), col=I('pink'), alpha=I(0.5))  + xlab('number of steps') + ylab('density')
a+b

ggsave(b, file = 'fig.3_histogram_of_total_number_of_steps_day_updated.png')
```

#### Mean


total_number_steps_per_day <- dat[, .(steps_day= sum(steps)), by= date]

mean(total_number_steps_per_day$steps)


#### Median


median(total_number_steps_per_day$steps)
```

## Are there differences in activity patterns between weekdays and weekends?


library(lubridate)
dat$date <- as.Date(dat$date)
dat$dateType <- ifelse(weekdays(dat$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
head(dat)




ggplot(dat, aes(x =interval , y=steps, color=dateType)) +
       geom_line() + labs(title = "Panel plots of steps in Weekdays vs Weekends", x = "5-minute interval", y = "Total Number of Steps") +facet_wrap(~ dateType)

ggsave('fig.4_panel_plots.png')


