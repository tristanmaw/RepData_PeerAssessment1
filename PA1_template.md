---
title: "Reproducible research project 1"
author: "TM"
date: "November 6, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


# Analysis

## Loading and preprocessing the data
### 1.Code for reading in the dataset and/or processing the data
```{r}
echo = TRUE
setwd("D:/R/Coursera/Reproducible research/Course project 1")

activitydata <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

### 2. Histogram of the total number of steps taken each day
```{r}
# Sumarize to date level
echo = TRUE
day_steps <- aggregate(steps ~ date, data = activitydata, sum, na.rm = TRUE)

#Create simple histogram
echo = TRUE
hist(x = day_steps$steps, breaks = 10, main = "Distribution of daily steps", xlab = "steps per day")
```



### 3. Mean and median number of steps taken each day
```{r}
echo = TRUE
summary(day_steps$steps, na.rm = TRUE)
```


## What is the average daily activity pattern?

###  4. Time series plot of the average number of steps taken
```{r}
echo = TRUE
interval_steps <- aggregate(steps ~ interval, data = activitydata, mean, na.rm=TRUE)
plot(interval_steps$interval,interval_steps$steps)
lines(interval_steps$interval,interval_steps$steps)
```


## 5. The 5-minute interval that, on average, contains the maximum number of steps
```{r}
echo = TRUE
head(
      (interval_steps[order(interval_steps$steps, decreasing = TRUE), ]
    ), n=1)
```


## Imputing missing values
### 6. Code to describe and show a strategy for imputing missing data
```{r}
echo = TRUE
#identify how many nulls there are and which columns

summary(activitydata)

# replace NA with mean of steps for relevant interval 

# join the average number of steps in each interval to main table

names(interval_steps) <- c('interval_avg','Steps_avg')

activitydata2 <- merge(activitydata,interval_steps, by.x = "interval", by.y = "interval_avg", all.x = TRUE)


# QA the join

head(
      activitydata2[order(activitydata2$date), ]
)


# create a loop to replace NA's with average value for interval


for(i in 1:nrow(activitydata)) {

if(is.na(activitydata2$steps[i])) {
  
  activitydata2$steps[i] <- activitydata2$Steps_avg[i]
  
}}
```




## 7. Histogram of the total number of steps taken each day after missing values are imputed

```{r}
echo = TRUE
#Sumarize to date level
day_steps2 <- aggregate(steps ~ date, data = activitydata2, sum)


hist(day_steps2$steps, breaks = 10, main = "Distribution of daily steps, NA filled", xlab = "steps per day")
```


## Are there differences in activity patterns between weekdays and weekends?

### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r}
echo = TRUE
# dataset for weekend
weekend_weekday <- activitydata2

# change from factor to date
weekend_weekday$day_of_week2 <- as.Date(weekend_weekday$date)

#append weekday
weekend_weekday$day_of_week <- weekdays(weekend_weekday$day_of_week2)

#subset files
weekend <- subset(weekend_weekday,day_of_week == c("Saturday","Sunday"))
weekdays <- subset(weekend_weekday,day_of_week != "Saturday" & day_of_week != "Sunday")


#aggregate to get average steps by interval
weekend_interval_steps <- aggregate(steps ~ interval, data = weekend, mean, na.rm=TRUE)
weekdays_interval_steps <- aggregate(steps ~ interval, data = weekdays, mean, na.rm=TRUE)

#add label for weekend or weekday
weekend_interval_steps$weekend_flag <- "Weekend"
weekdays_interval_steps$weekend_flag <- "Weekday"

#bring back together
week_combined <- rbind(weekend_interval_steps,weekdays_interval_steps)


# Create factor variable (just noticed instruction to do this...)
week_combined$weekend_flag <- factor(week_combined$weekend_flag)

#Create panel plots
library("lattice")
xyplot(steps~interval | weekend_flag, data = week_combined,
       main = "Comparison of weekend and weekday steps by time of day",
       layout = c(1,2), 
       type = "l",
)
```

