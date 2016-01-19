# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Load the data


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```

2. Process/transform the data(if necessary) into a format suitable for your analysis

```r
data$date <- as.Date(data$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day.

```r
totalStepsTaken <- aggregate(steps ~ date, data=data, sum, na.rm=TRUE)
```

2. Make a Histogram of the total number of steps taken each day.

```r
steps <- totalStepsTaken$steps
hist(steps, main="Average Steps Per Day", xlab="Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\

3. Calculate and report the mean and median of the total number of steps taken per day.

```r
mean(steps)
```

```
## [1] 10766.19
```

```r
median(steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type="1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
timeSeries <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(row.names(timeSeries), timeSeries, type="l", xlab="5-minute interval", ylab="Avg steps taken", main="Average steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)\

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(which.max(timeSeries))
```

```
## [1] "835"
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset.

```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy I will use means to fill in the NA spots.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
nas <- which(is.na(data$steps))
means <- rep(mean(data$steps, na.rm=TRUE), times=length(nas))
data[nas, "steps"] <- means
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalStepsTaken <- aggregate(steps ~ date, data=data, sum, na.rm=TRUE)
newSteps <- totalStepsTaken$steps
hist(steps, main="Average Steps Per Day", xlab="Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)\


```r
mean(newSteps)
```

```
## [1] 10766.19
```

```r
median(newSteps)
```

```
## [1] 10766.19
```

Inputing the missing data slightly changed the median.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
weekendDays <- c("Saturday", "Sunday")
weekDays <- weekdays(data$date)
data <- cbind(data, weekDays)
day <- sapply(weekDays, USE.NAMES = FALSE, function(day){
  if(day %in% weekendDays) {
    "weekend"
  } else {
    "weekday"
  }
})
data <- cbind(data, day)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)
means <- aggregate(data$steps, by=list(data$day, data$weekDays, data$interval), mean)
names(means) <- c("day", "weekday", "interval", "mean")
xyplot(mean ~ interval | day,
       means,
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Average number of steps", 
       layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)\
