# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
if(!file.exists('activity.csv')) {
  unzip('activity.zip')
}
data = read.csv('activity.csv')
```
## What is mean total number of steps taken per day?
Ignoring missing values here are the total number of steps taken per day and the mean and median values.

```r
perDay = rowsum(data$steps, data$date, na.rm=TRUE)
plot(as.Date(rownames(perDay)), perDay, t='h', ylab="Number of Steps", xlab="Date", main="Total number of steps per day", lwd=6, lend="square")
abline(h=mean(perDay), col="blue")
abline(h=median(perDay), col="green")
legend("topright", lty=1, col=c("blue", "green"), legend=c(paste("Mean:", round(mean(perDay))), paste("Median:", median(perDay))))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 


## What is the average daily activity pattern?
If we instead average the steps taken per 5-minutes interval across all days we get this plot.

```r
perInterval=aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
plot(perInterval, t='l', ylab="Number of Steps", xlab="5-minute interval", main="Average number of steps per 5-minute interval")
meanInterval = perInterval[which.max(perInterval[,2]),][,1]
abline(v=meanInterval, col="blue")
legend("topright", lty=1, col=c("blue"), legend=c(paste("Max interval:", meanInterval)))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Imputing missing values

```r
paste("The data contains", sum(is.na(data$steps)), "missing values.")
```

[1] "The data contains 2304 missing values."

We will fill in the missing values in the data with the average value for the given 5-minute interval across all days.


```r
dataWithoutNA = data
dataWithoutNA$date = as.Date(dataWithoutNA$date)
for(missing in which(is.na(dataWithoutNA$steps))) {
  dataWithoutNA[missing,]$steps = perInterval[perInterval[,1]==dataWithoutNA[missing,]$interval,][,2]
}
perDay = rowsum(dataWithoutNA$steps, dataWithoutNA$date, na.rm=TRUE)
plot(as.Date(rownames(perDay)), perDay, t='h', ylab="Number of Steps", xlab="Date", main="Total number of steps per day", lwd=6, lend="square")
abline(h=mean(perDay), col="blue")
abline(h=median(perDay), col="green")
legend("topright", lty=1, col=c("blue", "green"), legend=c(paste("Mean:", round(mean(perDay))), paste("Median:", round(median(perDay)))))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

We can see that the mean and median for the total number of steps per day have increased after imputing the missing values in this way.

## Are there differences in activity patterns between weekdays and weekends?


```r
library('dplyr')
library('ggplot2')
Sys.setlocale("LC_TIME", "en_US")
```

```
## [1] "en_US"
```

```r
dataWithoutNA$typeOfDay = as.factor(ifelse(weekdays(dataWithoutNA$date) %in% c('Sunday', 'Saturday'), 'weekend', 'weekday'))
byTypeAndInterval = group_by(dataWithoutNA, typeOfDay, interval)
summarised = summarise(byTypeAndInterval, steps=mean(steps))
qplot(interval, steps, data=summarised, facets=typeOfDay ~ ., geom='line', xlab="Interval", ylab="Number of steps", main="Average # of steps in each interval per weekdays and weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 
