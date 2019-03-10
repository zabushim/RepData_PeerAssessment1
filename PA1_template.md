---
title: "Reproducible Research"
author: "Ziyad Abushima"
date: "March 10, 2019"
output: 
  html_document:
    keep_md: true
---



## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 

The variables included in this dataset are:

**steps**: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰) </br>
**date**: The date on which the measurement was taken in YYYY-MM-DD format </br>
**interval**: Identifier for the 5-minute interval in which measurement was taken </br>
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset. 

## Loading and preprocessing the data
Unzip data to obtain a csv file.


```r
library(ggplot2)
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'))
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
```

## Reading csv Data into Data.Table. 

```r
data <- read.csv("data/activity.csv")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
omitdata <- na.omit(data)
stepsTotal <- aggregate(steps ~ date, omitdata, sum)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day. 


```r
ggplot(stepsTotal, aes(steps)) +
    geom_histogram(bins = 20,col = "black", fill = "lightblue", alpha = 0.3) +
    labs(title = "Number Daily Steps per Day", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
stepsMean <- mean(stepsTotal$steps)
stepsMedian <- median(stepsTotal$steps)
```

Mean: 1.0766189\times 10^{4} and Median: 10765 steps taken per day

## What is the average daily activity pattern?

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsInterval <- aggregate(steps ~ interval, omitdata, mean)
ggplot(stepsInterval, aes(interval, steps)) +
  geom_line(col = "red") +
  labs(x = "Time Intervals", y = "Average Steps per Day", title = "Average Number Of Steps Taken Across All Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsInterval[stepsInterval$steps == max(stepsInterval$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
length(which(is.na(data$steps)))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Filling in missing values with median of dataset. 
imputedata <- data
imputedata <- merge(aggregate(steps ~ interval, data, mean), imputedata, by = "interval", all.x = TRUE, all.y = TRUE)
imputedata <- imputedata[order(imputedata$date), ]
for(i in 1:nrow(imputedata)){
  if(is.na(imputedata$steps.y[i])){
    imputedata$steps.y[i] <- imputedata$steps.x[i]
  }
}
length(which(is.na(imputedata$steps)))
```

```
## [1] 0
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
imputedata <- imputedata[, c(1,3,4)]
colnames(imputedata) <- c('interval','steps','date')
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# total number of steps taken per day
stepsimputeTotal <- aggregate(steps ~ date, imputedata, sum)
# mean and median total number of steps taken per day
ggplot(stepsimputeTotal, aes(steps)) +
    geom_histogram(bins = 20,col = "black", fill = "lightblue", alpha = 0.3) +
    labs(title = "Number Daily Steps per Day", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
stepsimputeMean <- mean(stepsimputeTotal$steps)
stepsimputeMedian <- median(stepsimputeTotal$steps)
```

Type of Estimate | Mean_Steps | Median_Steps
--- | --- | ---
Activity with missing data | 1.0766189\times 10^{4} | 10765
Activity with imputed mean data | 1.0766189\times 10^{4} | 1.0766189\times 10^{4}

Based on the above, it seems that the difference is quite neglible because of our imputing strategy (imputed utilizing the mean of the 5-minute interval across all days).

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
imputedata$dayType <- ifelse(as.POSIXlt(imputedata$date)$wday %in% c(0,6), 'weekend', 'weekday')
head(imputedata)
```

```
##     interval     steps       date dayType
## 1          0 1.7169811 2012-10-01 weekday
## 63         5 0.3396226 2012-10-01 weekday
## 128       10 0.1320755 2012-10-01 weekday
## 205       15 0.1509434 2012-10-01 weekday
## 264       20 0.0754717 2012-10-01 weekday
## 327       25 2.0943396 2012-10-01 weekday
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
meanimputedata <- aggregate(steps ~ interval + dayType, data=imputedata, mean)
ggplot(meanimputedata, aes(interval, steps, color = dayType)) + 
  geom_line() +
  facet_grid(dayType ~ .) +
  labs(title = "Difference Between Weekdays and Weekends", x = "Time Interval", y = "Average Number of Steps") + facet_wrap(~dayType , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
