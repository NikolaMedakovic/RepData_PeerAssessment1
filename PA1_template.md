# Assignment 1

## Loading and preprocessing the data
* Load the data (i.e. read.csv())

```r
library(plyr)
```

```
## 
## Attaching package: 'plyr'
## 
## The following object is masked _by_ '.GlobalEnv':
## 
##     ozone
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.1
```

```r
setwd("D:\\R_files")
f <- read.csv("activity.csv")
```
  * Process/transform the data (if necessary) into a format suitable for your analysis

```r
f$date <- as.Date(f$date, format="%Y-%m-%d")
d <- f[!is.na(f$steps),]
```



## What is mean total number of steps taken per day?
* Make a histogram of the total number of steps taken each day  

```r
temp <- aggregate(d$steps, list(d$date), sum)
temp <- temp[ order(temp[,1]), ]
names(temp) <- c("x", "y")
hist(temp$y, xlab="Steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 
  
* Calculate and report the mean and median total number of steps taken per day

```r
mnsteps <- mean(temp$y)
mdsteps <- median(temp$y)
mnsteps
```

```
## [1] 10766
```

```r
mdsteps
```

```
## [1] 10765
```
  



## What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
temp <- aggregate(d$steps, list(d$interval), mean)
names(temp) <- c("x", "y")
#qplot(x, y, data=temp)
plot(temp$x, temp$y, xlab="", ylab= "", type= "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
  
* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 

```r
temp[ order(-temp[,2]), ][1,1]
```

```
## [1] 835
```


## Imputing missing values

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
onlynas <- f[is.na(f$steps),]
nrow(onlynas)
```

```
## [1] 2304
```
* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
withNA <- f
noNA <- f

len <- nrow(withNA)
for (i in 1:len) {    
    if (is.na(noNA[i,1])) {
        curInterval <- noNA[i,3]
        noNA[i,1] <- mean(withNA[withNA[3]==curInterval, 1], na.rm=TRUE)
    }     
}
```


* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
temp <- aggregate(noNA$steps, list(noNA$date), sum)
temp <- temp[ order(temp[,1]), ]
names(temp) <- c("x", "y")
hist(temp$y, xlab="Steps per day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 
  
  The impact is that the mean number of steps occurs more frequently.
 
 
## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
newCol <- vector()
for (i in 1:len) {    
    wkd <- weekdays(noNA[i,2])    
    newVal <- ifelse (wkd %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "weekday", "weekend")     
    newCol <- append(newCol, newVal)
    
    
}
noNA <- cbind(noNA, newCol)


p1 <- subset(noNA, noNA$newCol=="weekday")
p2 <- subset(noNA, noNA$newCol=="weekend")

p1agg <- aggregate(p1$steps, list(p1$interval), mean)
p2agg <- aggregate(p2$steps, list(p2$interval), mean)

names(p1agg) <- c("x", "y")
names(p2agg) <- c("x", "y")
```
  
    
* Weekday

```r
plot(p1agg$x, p1agg$y, xlab="5-minute intervals", ylab= "Mean number of steps", type= "l")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
  
* Weekend

```r
plot(p2agg$x, p2agg$y, xlab="5-minute intervals", ylab= "Mean number of steps", type= "l")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 




