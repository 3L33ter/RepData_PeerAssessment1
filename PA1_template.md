##Assignment 1

```r
library(data.table)
library(ggplot2)
```

###Loading and processing data

```r
mydata <- read.csv("./activity.csv", header = TRUE)
```

###What is mean total number of steps taken per day?
1. Calculate total number of steps taken per day

```r
total_steps <- tapply(mydata$steps, mydata$date, FUN=sum, na.rm=TRUE)
total_steps
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```
2. histogram of total steps

```r
hist(total_steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
  
3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(total_steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(total_steps, na.rm=TRUE)
```

```
## [1] 10395
```

###What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number
of steps taken, averaged across all days (y-axis)

```r
interval_averages <- aggregate(x=list(steps=mydata$steps), by=list(interval=mydata$interval), FUN=mean, na.rm=TRUE)
plot(interval_averages$interval, interval_averages$steps, type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
  
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number
of steps?

```r
interval_averages[which.max(interval_averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

###INNNputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of
rows with NAs)

```r
NAs <- is.na(mydata$steps)
sum(NAs*1)
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not
need to be sophisticated. For example, you could use the mean/median for that day, or the mean for
that 5-minute interval, etc.  

--> This assignment is way harder than it should be for getting me to learn R markdown, etc,
so I'll just replace all NAs with the overall average number of steps over the 2 month period.  

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
overall_ave <- mean(mydata$steps, na.rm=TRUE)
filled_steps <- replace(mydata$steps, NAs, overall_ave)
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean
and median total number of steps taken per day. Do these values differ from the estimates from the
first part of the assignment? What is the impact of imputing missing data on the estimates of the
total daily number of steps?

```r
total_steps2 <- tapply(filled_steps, mydata$date, FUN=sum, na.rm=TRUE)
hist(total_steps2)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(total_steps2, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(total_steps2, na.rm=TRUE)
```

```
## [1] 10766.19
```

The effect of filling in the NAs is:  
mean went from 9354.23 to 10766.19  
median went from 10395 to 10766.19  
  
###Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
w1 <- weekdays(as.Date(mydata$date))
sat <- w1==("Saturday")
sun <- w1==("Sunday")
ones <- rep(1, length(sat))
w2 <- replace(w1, ones==ones, "weekday")
w3 <- replace(w2, sat, "weekend")
w4 <- replace(w3, sun, "weekend")
mydata[, "weekpart"] <- w4
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
filled_data <- mydata
filled_data$steps <- filled_steps
averages2 <- aggregate(steps ~ interval + weekpart, data=filled_data, mean)
filled_data2 <- setorder(as.data.table(filled_data), interval)

ggplot(averages2, aes(interval, steps)) + geom_line() + facet_grid(weekpart ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
It appears that there is a difference in activity patterns between weekdays and weekends.
