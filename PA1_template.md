# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
    # Install library
    library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.1
```

```r
    library(lattice)

    ### written in English of week
    lct <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

```r
    # Load the data
    ds <- read.csv("activity.csv", header=T)

    # Make a dataset of a total number of steps taken per day
    dsDayTotal <- ddply(na.omit(ds), .(date), numcolwise(sum))

    # Make a dataset of a average number of steps taken per the 5-minute interval,averaged across all days 
    dsIntervalAve <- unique( ddply(na.omit(ds), .(interval), numcolwise(ave)) )
```


## What is mean total number of steps taken per day?


```r
    # Make a histogram of the total number of steps taken each day
    hist(dsDayTotal$steps, main='the total number of steps taken each day', xlab='steps')
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
    # Calculate and report the mean and median
    mean(dsDayTotal$steps)
```

```
## [1] 10766.19
```

```r
    median(dsDayTotal$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
    # Make a time series plot of the 5-minute interval and the average number of steps taken
    plot(dsIntervalAve$interval, dsIntervalAve$steps, type="l",
          main="the average number of steps", xlab="5-minute interval", ylab="steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
    # the 5-minute interval contains the maximum number of steps
    subset(dsIntervalAve, steps == max(dsIntervalAve$steps))
```

```
##      interval    steps
## 5460      835 206.1698
```


## Imputing missing values


```r
    # Calculate and report the total number of missing values in the dataset 
    nrow(ds[is.na(ds$steps)==TRUE,])
```

```
## [1] 2304
```

```r
    #fill in all of the missing values by the average number of steps of the 5-minute interval
    dsNoNAWK <- merge(ds[is.na(ds$steps)==TRUE,], dsIntervalAve, by='interval')
    dsNoNA <- data.frame(steps=dsNoNAWK$steps.y, date=dsNoNAWK$date, interval=dsNoNAWK$interval)
    dsNoNA <- rbind( dsNoNA, na.omit(ds) )

    # Make a dataset of a total number of steps taken per day
    dsNoNADayTotal <- ddply(na.omit(dsNoNA), .(date), numcolwise(sum))

    # Make a histogram of the total number of steps taken each day
    hist(dsNoNADayTotal$steps, main='the total number of steps taken each day', xlab='steps')
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
    # Calculate and report the mean and median
    mean(dsNoNADayTotal$steps)
```

```
## [1] 10766.19
```

```r
    median(dsNoNADayTotal$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?


```r
    # Create a new factor variable in the dataset with two levels
    dsWeekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    dsWeekday <- as.factor(ifelse(is.element(weekdays( as.Date(dsNoNA$date) ),dsWeekday), "Weekday",
                                  "Weekend"))

    # Make a dataset of a total number of steps taken per day
    dsNoNAWeekday <- cbind(dsNoNA,dsWeekday)
    colnames(dsNoNAWeekday) <- c("steps","date","interval","Weekdaytype")
    dsDayTotalWeek <- ddply(dsNoNAWeekday, .(interval,Weekdaytype), numcolwise(sum))

    # Make a panel plot 
    xyplot(steps ~ interval | Weekdaytype, dsDayTotalWeek, layout=c(1,2), type="l")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
    ### written in English of week
    Sys.setlocale("LC_TIME", lct)
```

```
## [1] "Japanese_Japan.932"
```


