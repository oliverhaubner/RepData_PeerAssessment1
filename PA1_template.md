------------------------------------------------------------------------

Loading and Preprocessing the Data
==================================

    setwd("~/COURSERA/Reproducible Research/Week2/repdata%2Fdata%2Factivity")
    activity <- read.csv("activity.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    library(dplyr)
    activity <- mutate(activity, date = as.Date(date, format = "%Y-%m-%d"))
    activityTidy <- activity[!is.na(activity$steps),]

------------------------------------------------------------------------

What is the mean total number of steps taken each day?
======================================================

1.  Calculate the total number of steps per day

<!-- -->

    sumStep = aggregate(steps~date, activityTidy, sum)

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    png(filename = "Plot1.png")
    hist(sumStep$steps, main = "Histogram of steps per day", xlab = "Sum of steps per day")
    dev.off()

    ## png 
    ##   2

1.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    mean(sumStep$steps, na.rm = T)

    ## [1] 10766.19

    median(sumStep$steps, na.rm = T)

    ## [1] 10765

#### The daily median is **10765**

#### The daily mean is **10766.19**

------------------------------------------------------------------------

What is the average daily activity pattern?
===========================================

1.  Make a time series plot

<!-- -->

    avgStep = aggregate(steps~interval, activityTidy, mean)
    png(filename = "Plot2.png")
    plot(avgStep, type = "l", main = "Plot of average steps per interval", sub = "averaged across all observed days, ignoring NAs", xlab = "5min Intervals", ylab = "Number of steps")
    dev.off()

    ## png 
    ##   2

1.  Find interval with maximum number of steps

<!-- -->

    maxIdx = which.max(avgStep$steps)
    maxInterval = avgStep[maxIdx, 'interval']
    maxAvgStep = avgStep[maxIdx, 'steps']

#### The interval **835** is the maximum interval with **206.1698113** steps.

------------------------------------------------------------------------

Imputing missing values
=======================

1.  Calculate and report number of missing values

<!-- -->

    totalNA = sum(is.na(activity$steps))

#### The total number of missing values for steps is **2304**

1.  Devise a strategy for filling out missing values  
    --&gt; Replace every NA with the calculated average for the
    respective time interval

2.  Create new dataset with the missing data filled in

<!-- -->

    naIdx = which(is.na(activity$steps))
    naInterval = activity[naIdx, 3]
    fillSteps = sapply(naInterval, function(x) {avgStep[(avgStep$interval==x), 2]})

    activityFill = activity
    activityFill[naIdx, 'steps'] = fillSteps

    sumStepFill = aggregate(steps~date, activityFill, sum)

1.  Histogram of new dataset (sumStepFill)

<!-- -->

    png(filename = "Plot3.png")
    hist(sumStepFill$steps, main = "Histogram of steps per day (After Imputation)", xlab = "Sum of steps per day")
    dev.off()

    ## png 
    ##   2

    mean(sumStepFill$steps, na.rm = T)

    ## [1] 10766.19

    median(sumStepFill$steps, na.rm = T)

    ## [1] 10766.19

##### The mean has not changed after imputing the NAs. The median, however, has changed its value after the modification of the data.

------------------------------------------------------------------------

Differences in activity patterns between weekdays and weekends
==============================================================

    ##Add new column to activityFill dataframe to indicate weekday (output is in German in my case)
    activityFill2 <- activityFill
    activityFill2$weekday <- weekdays(activityFill2$date)
    activityFill2$weekend <- activityFill2$weekday%in%c("Samstag", "Sonntag")
    head(activityFill2)

    ##       steps       date interval weekday weekend
    ## 1 1.7169811 2012-10-01        0  Montag   FALSE
    ## 2 0.3396226 2012-10-01        5  Montag   FALSE
    ## 3 0.1320755 2012-10-01       10  Montag   FALSE
    ## 4 0.1509434 2012-10-01       15  Montag   FALSE
    ## 5 0.0754717 2012-10-01       20  Montag   FALSE
    ## 6 2.0943396 2012-10-01       25  Montag   FALSE

    ##Plot
    library(lattice)
    png(filename= "Plot4.png")
    xyplot(steps ~ interval | weekend, data = activityFill2, layout = c(1, 2), main = "Avg. steps per interval on weekends [above] and on weekdays [below]")
    dev.off()

    ## png 
    ##   2
