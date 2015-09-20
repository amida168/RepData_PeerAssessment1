# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data (i.e. read.csv())


```r
# load the libraries
library(ggplot2)
library(sqldf)

# read the data
activity <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
# convert the date column from factor to date
activity[,2]<- as.Date(as.character(activity[,2]))
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day


```r
act_bydate <- aggregate(steps ~ date, data=activity, FUN="sum")

g1 <- ggplot(act_bydate, aes(x=steps))
p1 <- g1 + geom_histogram() + theme_bw() 
print(p1)
```

![](PA1_template_files/figure-html/2-1-1.png) 

2. Calculate and report the mean and median total number of steps taken
per day  

```r
mean(act_bydate$steps)
```

```
## [1] 10766.19
```

```r
median(act_bydate$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)  

```r
act_pattern <- aggregate(steps ~ interval, data=activity, FUN="mean")

g2 <- ggplot(act_pattern, aes(x=interval, y=steps))
p2 <- g2 + geom_line() + theme_bw() + ylab("Average Steps") + xlab("Interval")
print(p2)
```

![](PA1_template_files/figure-html/3-1-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?  

```r
act_pattern[which.max(act_pattern$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
Ans: I am going to fill in all the missing values with the mean for that 5-minute interval.  

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity2 <- activity

for(i in 1:nrow(activity2)) {
    if (is.na(activity2[i,1])) {
        activity2[i,1] <-  
            sqldf(paste("select steps from act_pattern where interval =",  
            activity2[i,3]))
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
act_bydate2 <- aggregate(steps ~ date, data=activity2, FUN="sum")

g3 <- ggplot(act_bydate2, aes(x=steps))
p3 <- g3 + geom_histogram() + theme_bw() 
print(p3)
```

![](PA1_template_files/figure-html/4-4-1.png) 

```r
mean(act_bydate2$steps)
```

```
## [1] 10766.19
```

```r
median(act_bydate2$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activity2$day <- as.factor(ifelse((weekdays(activity2$date) %in% c("Saturday", "Sunday")), "weekend", "weekday"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
act_pattern2 <- aggregate(steps ~ interval + day, data=activity2, FUN="mean")

g4 <- ggplot(act_pattern2, aes(x=interval, y=steps))
p4 <- g4 + geom_line() + theme_bw() + ylab("Average Steps") + 
    xlab("Interval") + facet_wrap(~day, ncol = 1, nrow = 2) +
    aes(color = day)
print(p4)
```

![](PA1_template_files/figure-html/5-2-1.png) 
