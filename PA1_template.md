# Reproducible Research: Peer Assessment 1
Jeff Collier  



## Loading and preprocessing the data

Loading the data into a data frame called "activity". Data is located in a local directory and is unzipped using unz before reading


```r
activity <- read.csv(unz("./activity.zip", filename="activity.csv"))
```
## What is mean total number of steps taken per day?
In the following histogram, I am ignoring the missing values in the dataset and creating a histogram related to steps/interval. First the daily steps are calculated then the historgram is created


```r
daily <- aggregate(steps ~ date, data = activity, sum)

g <- ggplot(daily, aes(steps))
g + geom_histogram(bins = 20)
```

![](PA1_template_files/figure-html/Steps Daily-1.png)<!-- -->

The following calculates and report the mean and median of the total number of steps taken per day


```r
mean(daily$steps)
```

```
## [1] 10766.19
```

## What is the average daily activity pattern?

```r
g <- ggplot(activity, aes(x = interval, y = steps))
g + geom_line()
```

![](PA1_template_files/figure-html/Steps Plot-1.png)<!-- -->

```r
maxSteps <- activity[!is.na(activity$steps) & activity$steps == max(activity$steps, na.rm = T),]
maxSteps
```

```
##       steps       date interval
## 16492   806 2012-11-27      615
```

## Imputing missing values

```r
missing <- sum(is.na(activity$steps))
missing
```

```
## [1] 2304
```

```r
percentMissing <- missing / nrow(activity)
percentMissing
```

```
## [1] 0.1311475
```
I manipulated the data a bit to get some basic information about where the NAs might be.


```r
dates <- unique(activity$date)
length(dates)
```

```
## [1] 61
```

```r
dim(daily)
```

```
## [1] 53  2
```
I find 61 unique dates but only 53 for which I can get a mean number of steps, perhaps because the entire day is NA. Next I attempt to replace NA with the average per time interval rather than per day.


```r
intervals <- aggregate(steps ~ interval, data = activity, mean)
dim(intervals)
```

```
## [1] 288   2
```

```r
head(intervals)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
ggplot(intervals, aes(steps)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
The number of intervals is 288 which is what we would expect for a full 24 hour day with 5 minute intervals. Looking further I see that the first few intervals have a very small number of steps but a histogram shows that many intervals have a large number of steps and there seem to be two central areas one around 40ish steps and another around 0. This seems very consistent with expected life patters, So I'm going to use the interval mean to replace NAs.


```r
activity.nona <- activity %>%  
  group_by(interval) %>% 
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))

activity.nona <- as.data.frame(activity.nona)

daily.nona <- aggregate(steps ~ date, data = activity.nona, sum)
daily$category <- "original"
daily.nona$category <- "no-na values"
daily.plot <-rbind(daily, daily.nona)

g <- ggplot(daily.plot, aes(steps))
g + geom_histogram(bins = 20) + facet_grid(.~category)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
any(is.na(activity))
```

```
## [1] TRUE
```

```r
any(is.na(activity.nona))
```

```
## [1] FALSE
```


## Are there differences in activity patterns between weekdays and weekends?
