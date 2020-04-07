---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
Setting default knitr options


```r
knitr::opts_chunk$set(echo= TRUE)
```

## Loading and preprocessing the data
Show any code that is needed to

1. Load the data (i.e. read.csv())

```r
activity <- read.csv("activity.csv")
```
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity <- transform(activity, date = as.Date(date, "%Y-%m-%d"))
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
activityDay <- aggregate(steps ~ date, activity, sum)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(activityDay$steps,breaks = 10,xlab = "Steps",main = "Histogram of steps by date")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
dev.copy(png, file = "figure/plot1.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

3. Calculate and report the mean and median of the total number of steps taken per day


```r
activityDayMean <- aggregate(steps ~ date, activity, mean)
activityDayMedian <- aggregate(steps ~ date, activity, median)
par(mar=c(5, 4, 4, 8.1), xpd=TRUE)
with(activityDayMean,plot(date,steps,type="l",col="red",main="Mean and median steps"))
with(activityDayMedian,lines(date,steps,type="l",col="blue"))
legend("topright", inset=c(-0.3,0),lty = c(1, 1, 1), col = c("red", "blue"), legend = c("Mean", "Median"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
dev.copy(png, file = "figure/plot2.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days 

```r
activityIntervalMean <- aggregate(steps ~ interval, activity, mean)
with(activityIntervalMean,plot(interval,steps,type="l",col="red",main="Average steps across all days"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dev.copy(png, file = "figure/plot3.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The 5-minute intervar which contains the maximun number of steps is:


```r
activityIntervalMean[activityIntervalMean$steps == max(activityIntervalMean$steps),1]
```

```
## [1] 835
```
## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
The total number of NAs is:


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
activityImp <-activity
for(i in 1:nrow(activityImp)){
  interval <-activityImp[i,3]
  if(is.na(activityImp[i,1]))
    activityImp[i,1] <- activityIntervalMean[activityIntervalMean$interval==interval,2]
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
str(activityImp)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
#Histogram
activityDay <- aggregate(steps ~ date, activityImp, sum)
hist(activityDay$steps,breaks = 10,xlab = "Steps",main = "Histogram of steps by date(Imputed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
dev.copy(png, file = "figure/plot4.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
#Mean and Median
activityDayMeanImp <- aggregate(steps ~ date, activityImp, mean)
activityDayMedianImp <- aggregate(steps ~ date, activityImp, median)
par(mar=c(5, 4, 4, 10), xpd=TRUE)
with(activityDayMean,plot(date,steps,type="l",col="red",main="Mean and median steps(Imputed)"))
with(activityDayMedian,lines(date,steps,type="l",col="blue"))
with(activityDayMeanImp,lines(date,steps,type="l",col="green"))
with(activityDayMedianImp,lines(date,steps,type="l",col="yellow"))
legend("topright", inset=c(-0.47,0),lty = c(1, 1, 1), col = c("red", "blue","green","yellow")
       , legend = c("Mean", "Median","Mean(Imputed)","Median(Imputed)"))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
dev.copy(png, file = "figure/plot5.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

The impact of imputing missing data


```r
par(mar=c(5, 4, 4, 8.1), xpd=TRUE)
activityDayImp <- aggregate(steps ~ date, activity, sum)
with(activityDay,plot(date,steps,type="l",col="red",main="Total steps(Imputed)"))
with(activityDayImp,lines(date,steps,type="l",col="blue"))
legend("topright", inset=c(-0.3,0),lty = c(1, 1, 1), col = c("red", "blue","green","yellow")
       , legend = c("Original", "Imputed"))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
dev.copy(png, file = "figure/plot6.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```




## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity$wday <-factor(weekdays(activity$date),
       levels = c("lunes", "martes", "miercoles", "jueves","viernes","sabado","domingo"),
       labels = c("weekday","weekday","weekday","weekday","weekday","weekend","weekend"))
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
activityWday <- aggregate(steps ~ interval+wday, activity, mean)
library(lattice) 
with(activityWday, 
  xyplot(steps~interval|wday,
            main="Average steps by weekday",
            xlab="Interval",
         type= "l",layout=c(1,2))
)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
dev.copy(png, file = "figure/plot7.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
