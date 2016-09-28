# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data (i.e. read.csv())   

```r
Act_Data <- read.csv(unz(description = "activity.zip",filename = "activity.csv"),header = TRUE)
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
Act_Data$date <- as.Date(Act_Data$date)
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day


```r
Tot_per_Day <- aggregate(steps ~ date, data = Act_Data,FUN = sum, na.rm=TRUE)
```
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(Tot_per_Day$steps,xlab = "Total steps per day [Steps]",main = "Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
library(pander)
panderOptions('round', 4)
day_mean <- mean(Tot_per_Day$steps)
day_median <- median(Tot_per_Day$steps)
```

The daily mean is _10766_ steps and the daily median is _10765_ steps. 

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
Mean_per_Int <- aggregate(steps ~ interval, data = Act_Data,FUN = mean, na.rm=TRUE)
plot(Mean_per_Int$interval,Mean_per_Int$steps,type="l",xlab = "Interval [-]",ylab = " Step averaged across all days [Steps]")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_int <- Mean_per_Int[which.max(Mean_per_Int$steps),]$interval
max_int
```

```
## [1] 835
```

Maximum average across all days is in interval 835.  

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
num_NA <- sum(is.na(Act_Data$steps))
num_NA
```

```
## [1] 2304
```
The number of NA's is 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The choosen strategy is to take the mean of the 5-minute inteval for all days for the NA

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
Act_Data_mod <- Act_Data
Logic_NA <- is.na(Act_Data_mod$steps)
for(i in 1:length(Logic_NA)){
        if (Logic_NA[i]){
                NA_int <- Act_Data_mod$interval[[i]]
                temp_steps <- Act_Data_mod[Act_Data_mod$interval==NA_int,]
                int_mean <- mean(temp_steps$steps,na.rm=TRUE)
                Act_Data_mod$steps[i]<-int_mean
        }
}
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
Tot_per_Day_mod <- aggregate(steps ~ date, data = Act_Data_mod,FUN = sum, na.rm=TRUE)
hist(Tot_per_Day_mod$steps,xlab = "Total steps per day [Steps]")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
day_mean_mod <- mean(Tot_per_Day_mod$steps)
day_median_mod <- median(Tot_per_Day_mod$steps)
```
The daily mean with filled in missing data is _10766_ steps and the daily median with filled in missing data is _10766_ steps. The absolute difference is _0_ for the mean and _1.189_ for the median.   

## Are there differences in activity patterns between weekdays and weekends?   
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
library(timeDate)
# Set up for getting the weekdays in english instead of swedish
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
day.factor <- ifelse(isWeekend(Act_Data_mod$date,wday=1:5),"Weekend","Weekday")

Act_Data_mod_fact <- cbind(Act_Data_mod,day.factor)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
Mean_per_fact <- aggregate(steps ~ interval + day.factor, data = Act_Data_mod_fact,FUN = mean)
xyplot(steps ~ interval| factor(day.factor), 
           data = Mean_per_fact,
           type = "l",
           xlab = "Interval",
           ylab = "Steps",
           layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
