

```r
---
title: "PA1_template.Rmd"
output: html_document
---

For this assignment, first we will need to upload the data and save it to a variable called "data".
```

```
## Error: <text>:6:5: unexpected symbol
## 5: 
## 6: For this
##        ^
```

```r
data <- read.csv("activity.csv")
```

The document does not have the correct formats, as we would like, so we will transform them.


```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.2.5
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
data$date <- as.character(data$date)
data$date <- as.Date(data$date)
```

Lastly, we will answer the questions required.

##1. What is the total number of steps taken per day?
For this question, we will do several steps. First, we will transform tha steps variable from factor to number. Then we calculate the number of steps per day, and then we make a mean of the steps per day. Lastly, we make a histogram to show the number of steps per day. 

```r
data$steps <- as.integer(data$steps)
stepsperday <- tapply(data$steps, data$date, sum, na.rm = TRUE)
```
###Histogram of steps per day

```r
hist(stepsperday, main = "Steps per day", xlab = "day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
###Mean and median steps per day

```r
meansteps <- sapply(stepsperday, mean)
mediansteps <- sapply(stepsperday, median)
```
###Reporting results

```r
barplot(meansteps, main = "Average steps per day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
barplot(mediansteps, main = "Median steps per day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png)

##2. What is the daily activity pattern?
###Time series plot of the average numbers of steps taken 

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
int <- data %>% filter(!is.na(steps)) %>% 
      group_by(interval) %>% 
      summarize(steps = mean(steps))
plot(int, data$steps, type = "l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
###Determining the five minute interval, that, on average, contains the max number of steps taken

```r
int[which.max(int$steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
##      (int)    (dbl)
## 1      835 206.1698
```

##3. Imputing missing values
###Determining the total number of NAs in the dataset

```r
sum(is.na(data))
```

```
## [1] 2304
```
###Creating a NA filling strategy
We will use the mean as a filling strategy of each day.

```r
l <- nrow(data)
for (i in 1:l) {
    if (is.na(data$steps[i])) {
          data$steps[i] <- tapply(data$steps, data$date, mean)
    }
}
```

```
## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length

## Warning in data$steps[i] <- tapply(data$steps, data$date, mean): number of
## items to replace is not a multiple of replacement length
```

```r
sum(is.na(data))
```

```
## [1] 2304
```
###Comparing results with the first part

```r
hist(stepsperday)
```

![plot of chunk comparison](figure/comparison-1.png)

```r
barplot(meansteps, main = "Average steps per day")
```

![plot of chunk comparison](figure/comparison-2.png)

```r
barplot(mediansteps, main = "Median steps per day")
```

![plot of chunk comparison](figure/comparison-3.png)

##4. Are there differences in activity patterns between weekdays and weekends?
###Create a new factor variable

```r
weekday <- ifelse(weekdays(data$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
weekday <- as.factor(weekday)
data <- cbind(data, weekday)
str(data)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekday : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

###Make a panel plot

```r
par(2,1)
```

```
## [[1]]
## NULL
## 
## [[2]]
## NULL
```

```r
plot(meansteps, weekday = 1, type = "l",main = "Weekend")
```

```
## Warning in plot.window(...): "weekday" is not a graphical parameter
```

```
## Warning in plot.xy(xy, type, ...): "weekday" is not a graphical parameter
```

```
## Warning in axis(side = side, at = at, labels = labels, ...): "weekday" is
## not a graphical parameter

## Warning in axis(side = side, at = at, labels = labels, ...): "weekday" is
## not a graphical parameter
```

```
## Warning in box(...): "weekday" is not a graphical parameter
```

```
## Warning in title(...): "weekday" is not a graphical parameter
```

![plot of chunk panelplot](figure/panelplot-1.png)

```r
plot(meansteps, weekday = 0, type = "l",main = "Weekday")
```

```
## Warning in plot.window(...): "weekday" is not a graphical parameter
```

```
## Warning in plot.xy(xy, type, ...): "weekday" is not a graphical parameter
```

```
## Warning in axis(side = side, at = at, labels = labels, ...): "weekday" is
## not a graphical parameter

## Warning in axis(side = side, at = at, labels = labels, ...): "weekday" is
## not a graphical parameter
```

```
## Warning in box(...): "weekday" is not a graphical parameter
```

```
## Warning in title(...): "weekday" is not a graphical parameter
```

![plot of chunk panelplot](figure/panelplot-2.png)
```

