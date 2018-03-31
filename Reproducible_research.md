--- 
output: 
  html_document: 
    keep_md: true 

---
COURSERA: REPRODUCIBLE RESEARCH- PROJECT 1
==========================================
**Hetvee Patel**    
March 31, 2018  


**Loading and preprocessing the data**  

1. Loading the dataset "activity.csv"

```r
activity<-read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
  
2. Processing the above dataset

```r
#creating the day variable
activity$day <- weekdays(as.Date(activity$date))
```

```
## Warning in strptime(xx, f <- "%Y-%m-%d", tz = "GMT"): unknown timezone
## 'zone/tz/2018c.1.0/zoneinfo/Asia/Kolkata'
```

```r
#changing the date format
activity$date<- as.POSIXct(activity$date, format="%Y-%m-%d")

##pulling data without nas
activity_clean <- activity[!is.na(activity$steps),]
head(activity_clean)
```

```
##     steps       date interval     day
## 289     0 2012-10-02        0 Tuesday
## 290     0 2012-10-02        5 Tuesday
## 291     0 2012-10-02       10 Tuesday
## 292     0 2012-10-02       15 Tuesday
## 293     0 2012-10-02       20 Tuesday
## 294     0 2012-10-02       25 Tuesday
```

**What is mean total number of steps taken per day?**  
  
1. Calculate the total number of steps taken per day  


```r
stepsByDay <- tapply(activity_clean$steps, activity_clean$date, sum, na.rm=TRUE)
stepsByDay
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        126      11352      12116      13294      15420      11015 
## 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 
##      10139      15084      13452      10056      11829      10395 
## 2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 
##       8821      13460       8918       8355       2492       6778 
## 2012-10-27 2012-10-28 2012-10-29 2012-10-30 2012-10-31 2012-11-02 
##      10119      11458       5018       9819      15414      10600 
## 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-11 
##      10571      10439       8334      12883       3219      12608 
## 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 
##       8841       4472      12787      20427      21194      14478 
## 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      11834      11162      13646      10183       7047
```
  
2. Make a histogram of the total number of steps taken each day  

```r
steps_table <- aggregate(activity_clean$steps ~ activity_clean$date, FUN=sum, )
colnames(steps_table)<- c("Date", "Steps")
hist(steps_table$Steps, main= "Total number of steps taken per day", xlab="Steps", col="red")
```

![](Reproducible_research_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
  
3. Calculate and report the mean and median of the total number of steps taken per day  

```r
summary(steps_table$Steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

From thw summary chart we can note that the average(mean) number of steps taken each day was 10766 steps and the median number of steps taken each day was 10765 steps.  

**What is the average daily activity pattern?**  

1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

```r
library(plyr)
library(ggplot2)

time_table <- ddply(activity_clean, .(interval), summarize, Avg = mean(steps))
##line plot of mean steps per interval
p <- ggplot(time_table, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Mean Steps per Interval")
```

![](Reproducible_research_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
  
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?    

```r
#maximum steps
max_steps <- max(time_table$Avg)
#interval with maximum steps
time_table[time_table$Avg==max_steps,1]
```

```
## [1] 835
```
  
**Inputing missing values**  
 
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  
???s)

```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```
  
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.  

STRATEGY: Replace each NA value with the mean of the "steps"" attribute.  

```r
# Find the NA positions
activity_na <- which(is.na(activity$steps))

# Create a vector of means
activity_mean <- rep(mean(activity$steps, na.rm=TRUE), times=length(activity_na))
```
  
3. Create a new dataset that is equal to the original dataset but with the missing data filled in. 

```r
# Replace the NAs by the means
activity[activity_na, "steps"] <- activity_mean
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

```r
# Sum of steps each day without NAs
steps_sum <- aggregate(activity$steps, by=list(activity$date), FUN=sum)

names(steps_sum) <- c("Date", "Total")
#histogram of total number of steps
hist(steps_sum$Total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="yellow", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Total number of steps taken each day\n(NA replaced by mean value)")
```

![](Reproducible_research_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
  
**Are there differences in activity patterns between weekdays and weekends?**  

1. Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.  

```r
activity_new<- mutate(activity, day = ifelse(weekdays(activity$date) == "Saturday" | weekdays(activity$date) == "Sunday", "weekend", "weekday"))
activity_new$day<-as.factor(activity_new$day)
head(activity_new)
```

```
##     steps       date interval     day
## 1 37.3826 2012-10-01        0 weekday
## 2 37.3826 2012-10-01        5 weekday
## 3 37.3826 2012-10-01       10 weekday
## 4 37.3826 2012-10-01       15 weekday
## 5 37.3826 2012-10-01       20 weekday
## 6 37.3826 2012-10-01       25 weekday
```
  
2. Make a panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  

```r
library(lattice)
## Summarize data by interval and type of day
interval_table <- ddply(activity_new, .(interval, day), summarize, Avg = mean(steps))

##Plot data in a panel plot
xyplot(Avg~interval|day, data=interval_table, type="l",  layout = c(1,2), main="Average Steps per Interval Based on Type of Day", ylab="Average Number of Steps", xlab="Interval")
```

![](Reproducible_research_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
  
  
  
