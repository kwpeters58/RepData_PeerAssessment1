---
title: "RepData_PeerAssessment1"
author: "Ken Peters"
date: "7/16/2020"
output: 
    html_document:
        keep_md: true
---

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


## Assignment

This assignment will be described in multiple parts. You will need to
write a report that answers the questions detailed below. Ultimately,
you will need to complete the entire assignment in a **single R
markdown** document that can be processed by **knitr** and be
transformed into an HTML file.

Throughout your report make sure you always include the code that you
used to generate the output you present. When writing code chunks in
the R markdown document, always use `echo = TRUE` so that someone else
will be able to read the code. **This assignment will be evaluated via
peer assessment so it is essential that your peer evaluators be able
to review the code for your analysis**.

For the plotting aspects of this assignment, feel free to use any
plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the [GitHub repository created for this
assignment](http://github.com/rdpeng/RepData_PeerAssessment1). You
will submit this assignment by pushing your completed files into your
forked repository on GitHub. The assignment submission will consist of
the URL to your GitHub repository.

NOTE: The GitHub repository also contains the dataset for the
assignment so you do not have to download the data separately.



### Loading and preprocessing the data


```r
data <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")

##Looking at a summary and the structure for the dataset:

summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
## Looking at the first 6 rows of the dataset:

head(data)
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


## Create a Histogram of the total number of steps taken each day


```r
steps_each_day <- aggregate(steps ~ date, data, sum)
hist(steps_each_day$steps, main = paste("Number of Steps Each Day"), col="violetred1",xlab="Number of Steps")
```

![](RepData_PeerAssessment1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
## Calculate the Mean and median number of steps taken each day

stepsmean <- mean(steps_each_day$steps)
stepsmean
```

```
## [1] 10766.19
```

```r
stepsmedian <- median(steps_each_day$steps)
stepsmedian
```

```
## [1] 10765
```
## Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the ## average number of steps taken, averaged across all days (y-axis)
# averaged across all days


```r
Steps5min <- aggregate(steps ~ interval, data, mean)   
plot(Steps5min$interval,Steps5min$steps, type='l',col='navyblue',
     xlab='5 Minute Intervals',lwd=3,
     ylab='Average number of steps taken',
     main ='Average number of steps taken in 5-minute intervals, \naveraged across all days',col.lab = "red", col.main = "red")
```

![](RepData_PeerAssessment1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Determine which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?


```r
max_interval <- Steps5min[which.max(Steps5min$steps),1]
hours <- max_interval%/%100 
minutes <- max_interval%%100
```
## The time is 8:35

### Imputing missing values

 ## The Code that describes and shows a strategy for imputing missing data follows:

## Calculate and report the total number of missing values in the dataset
## (i.e. the total number of rows with `NA`s)


```r
sum(is.na(data[,1]))
```

```
## [1] 2304
```

## A strategy for filling in all of the missing values in the dataset. Use the mean for that 5-minute interval.

```r
Steps5min_rep <- as.data.frame(sapply(Steps5min, rep.int, times=61))
newdata <- data  
for (i in 1:length(newdata[,1])){ 
      if(is.na(newdata[i,1])==TRUE){
            newdata[i,1]= Steps5min_rep[i,2] # missing values replaced
      }}
```
## Create a new dataset that is equal to the original dataset but with the missing data filled in.
## newdata is that dataset

## Make a histogram of the total number of steps taken each day and Calculate and 
## report the **mean** and **median** total number of steps taken per day. 
## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
new_steps_each_day <- aggregate(steps ~ date, newdata, sum)
par(mfrow=c(2,1))
hist(steps_each_day$steps, main = paste("Number of Steps Each Day with Missing Data Removed"), col="olivedrab2",xlab="Number of Steps", col.lab = "red", col.main = "red")
hist(new_steps_each_day$steps, main = paste("Number of Steps Each Day with Missing Data not Removed"), col="maroon3",xlab="Number of Steps", col.lab = "red", col.main = "red")
```

![](RepData_PeerAssessment1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Calculate the Mean and median number of steps taken each day


```r
newstepsmean <- mean(new_steps_each_day$steps)
newstepsmean
```

```
## [1] 10766.19
```

```r
newstepsmedian <- median(new_steps_each_day$steps)
newstepsmedian
```

```
## [1] 10766.19
```

## The mean with the missing data removed is 1.0766189\times 10^{4} and with the missing data is 1.0766189\times 10^{4}.
## The meadian with the missing data removed is 1.0766189\times 10^{4} and with the missing data is 10765.


### Are there differences in activity patterns between weekdays and weekends?

### Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels -- 
## "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
weekday <- weekdays(as.Date(data[, 2]), abbreviate = FALSE)
dataday <- data
dataday$weekday <- weekday
for (i in 1:nrow(dataday)) {
      if (dataday$weekday[i] == "Saturday"){
            dataday$weekday[i]= "weekend"
            } 
      else if (dataday$weekday[i] == "Sunday"){
            dataday$weekday[i]= "weekend"
           }
      else {dataday$weekday[i]= "weekday"
      }}
```

## Make a panel plot containing a time series plot (i.e. `type = "l"`) of 
## the 5-minute interval (x-axis) and the average number of steps taken, averaged across 
## all weekday days or weekend days (y-axis).



```r
##install.packages("dplyr")
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.2
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
Weekend <- filter(dataday, weekday == "weekend")
Avesteps_weekend <- aggregate(steps ~ interval, Weekend, mean)
Weekday <- filter(dataday, weekday == "weekday")
Avesteps_weekday <- aggregate(steps ~ interval, Weekday, mean)
par(mfrow=c(2,1), mar=c(3,4,4,4))

plot(Avesteps_weekend$interval,Avesteps_weekend$steps, type='l',col='navyblue',
                                                    xlab='5 Minute Intervals',lwd=3,
                                                    ylab='Average number of steps taken',
                                                    main ='Average number of steps taken in 5-minute intervals, \naveraged for Weekends',col.lab = "red", col.main = "red")
 

plot(Avesteps_weekday$interval,Avesteps_weekday$steps, type='l',col='green2',
                                                    xlab='5 Minute Intervals',lwd=3,
                                                    ylab='Average number of steps taken',
                                                    main ='Average number of steps taken in 5-minute intervals, \naveraged for Weekdays',col.lab = "orange2", col.main = "orange2")
```

![](RepData_PeerAssessment1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



