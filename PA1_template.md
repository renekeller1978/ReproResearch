---
title: "Reproducible Research - Part 1 - Movement"
author: "Rene Keller"
date: "5/3/2020"
output: 
  html_document: 
    keep_md: yes
keep_md: TRUE
---



## Downloading and reading file into R

The source dataset will be downloaded from the following URL: <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>.

Then unzipped nd opened by R
Store dataset in dataset variable


```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile="movement.zip")
unzip("movement.zip")

dataset<-read.csv("activity.csv")
```

## Steps per day

This section computes a histogram for the steps taken per day. 
First the dataset is aggregated over the dates, then the basic histogram is created, then mean and median are being computed.


```r
stepsPerDay<-aggregate(dataset[, 'steps'], by=list( dataset$date), FUN=sum) 
hist(stepsPerDay$x)
```

![](PA1_template_files/figure-html/stepsperday-1.png)<!-- -->

```r
mmean<-mean(stepsPerDay$x,na.rm=TRUE)
mmedian<-median(stepsPerDay$x,na.rm=TRUE)
```

The mean is 1.0766189\times 10^{4} and the median is 10765.

## Daily Activity pattern

This section describes the daily activity patterns by time interval


```r
intervalPattern<-aggregate(dataset[, 'steps'], by=list( dataset$interval), FUN=mean, na.rm=TRUE)
plot(intervalPattern$Group.1,intervalPattern$x,type='l')
```

![](PA1_template_files/figure-html/intervalpattern-1.png)<!-- -->

```r
maxSteps<-max(intervalPattern$x)
maxInterval<-which.max(intervalPattern$x)
```

Time interval with maximum number of steps is 104 with 206.1698113 steps

## Missing values

In this section we first calculate the number of NAs for steps in the dataset. then I replace the NAs with the average for that time interval over all days (I reuse the dataset in the previous section) and store the result in a different dataset (named missingvalsRemoved).



```r
##calsulate the number of NAs
numNAs=sum(is.na(dataset$steps))

##join averages into dataset
names(intervalPattern)<-c("interval","mean")
library(dplyr)
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
missingvalsRemoved<-inner_join(dataset,intervalPattern,by="interval")

##create new dataset
missingvalsRemoved$stepsClean<-missingvalsRemoved$steps

##replace NAs with the mean from interval
missingvalsRemoved[is.na(missingvalsRemoved$stepsClean),"stepsClean"]<-missingvalsRemoved[is.na(missingvalsRemoved$stepsClean),"mean"]


stepsPerDay2<-aggregate(missingvalsRemoved[, 'stepsClean'], by=list( missingvalsRemoved$date), FUN=sum) 
hist(stepsPerDay2$x)
```

![](PA1_template_files/figure-html/missingvalues-1.png)<!-- -->

```r
mmean2<-mean(stepsPerDay2$x,na.rm=TRUE)
mmedian2<-median(stepsPerDay2$x,na.rm=TRUE)
```

In the dataset there were 2304 missing values. See above for the updated histogram. 

The new mean is 1.0766189\times 10^{4} and the mdeian is 1.0766189\times 10^{4}. The difference to the old mean/median is 0 / -1.1886792

## Weekday/weekend patterns
In this section we create two columns, one depicting the day of the week, the second (a boolean column) indicates whether the days is on the weekend or not.
Then I create a line chart with both lines indicated.


```r
##adding columns for day of week and weekend
missingvalsRemoved$weekday<-weekdays(as.Date(missingvalsRemoved$date),abbreviate=TRUE)
missingvalsRemoved$weekend<-substring(missingvalsRemoved$weekday,1,1)=="S"

##aggregate over weekend and intervals
intervalPattern2<-aggregate(missingvalsRemoved[, 'steps'], by=list( missingvalsRemoved$interval,missingvalsRemoved$weekend), FUN=mean, na.rm=TRUE)

##Plot the graph for weekends
plot(intervalPattern2[intervalPattern2$Group.2,"Group.1"],intervalPattern2[intervalPattern2$Group.2,"x"],type='l',col="red")
##add weekdays
lines(intervalPattern2[!intervalPattern2$Group.2,"Group.1"],intervalPattern2[!intervalPattern2$Group.2,"x"],col="blue")
##add legend
legend("topleft",legend=c("Weekend","Weekday"),col=c("red","blue"), lty=1)
```

![](PA1_template_files/figure-html/weekdays-1.png)<!-- -->

