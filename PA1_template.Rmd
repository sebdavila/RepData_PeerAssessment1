Reproducible Research: Peer Assessment 1
==========================================
Author: Sebastián Dávila
Date: Sep 20 / 2015

## Basic settings

```{r}
echo = TRUE  # The code is visible in case someone want to evaluate it
options(scipen = 1)  # Turning off scientific notations
```

## Loading and preprocessing the data

```{r processdata}
unzip("activity.zip")
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))
NoNA <- na.omit(data)
rownames(NoNA) <- 1:nrow(NoNA)
head(NoNA)
dim(NoNA)
library(ggplot2)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.
* Make a histogram of the total number of steps taken each day *
```{r totalstepsplot}
ggplot(NoNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "chocolate2", fill = "chocolate2", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

Calculate and report the mean and median total number of steps taken per day

Mean total number of steps taken per day:

```{r totalstepsmean}
totalSteps <- aggregate(NoNA$steps, list(Date = NoNA$date), FUN = "sum")$x
mean(totalSteps)
```

Median total number of steps taken per day:

```{r totalstepsmedian}
median(totalSteps)
```

## What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
AvgSteps <- aggregate(NoNA$steps, list(interval = as.numeric(as.character(NoNA$interval))), FUN = "mean")
names(AvgSteps)[2] <- "MeanOfSteps"

ggplot(AvgSteps, aes(interval, MeanOfSteps)) + geom_line(color = "chocolate2", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
AvgSteps[AvgSteps$MeanOfSteps == max(AvgSteps$MeanOfSteps), ]
```

## Imputing missing values

* The total number of rows with NAs:

```{r}
sum(is.na(data))
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy is to use the mean for that 5-minute interval to fill each NA value in the steps column.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
NewData <- data 
for (i in 1:nrow(NewData)) {
    if (is.na(NewData$steps[i])) {
        NewData$steps[i] <- AvgSteps[which(NewData$interval[i] == AvgSteps$interval), ]$MeanOfSteps
    }
}

head(NewData)
sum(is.na(NewData))
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```{r}
ggplot(NewData, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "chocolate2",
                                             fill = "chocolate2",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```{r}
newTotalSteps <- aggregate(NewData$steps, 
                           list(Date = NewData$date), 
                           FUN = "sum")$x
NewMean <- mean(newTotalSteps)
NewMean
```

Median total number of steps taken per day:

```{r}
NewMedian <- median(newTotalSteps)
NewMedian
```

Compare them with the two before imputing missing data:

```{r}
OldMean <- mean(totalSteps)
OldMedian <- median(totalSteps)
NewMean - OldMean
NewMedian - OldMedian
```

So, after imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
head(NewData)
NewData$weekdays <- factor(format(NewData$date, "%A"))
levels(NewData$weekdays)
levels(NewData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(NewData$weekdays)
table(NewData$weekdays)
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
AvgSteps <- aggregate(NewData$steps, 
                      list(interval = as.numeric(as.character(NewData$interval)), 
                           weekdays = NewData$weekdays),
                      FUN = "mean")
names(AvgSteps)[3] <- "MeanOfSteps"
library(lattice)
xyplot(AvgSteps$MeanOfSteps ~ AvgSteps$interval | AvgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

