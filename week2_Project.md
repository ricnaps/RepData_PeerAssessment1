Week 2 Project 1


```r
library(lubridate)
library(ggplot2)
library(knitr)
library(dplyr)
```

## Cleaning data

```r
data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character", "integer"))
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

```r
data$date <-ymd(data$date)

str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
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

## Compute for the totaldailysteps and plot the result
## compute for the total steps per day


```r
totaldailysteps <- data %>% 
  filter(!is.na(steps)) %>% 
  group_by(date) %>% 
  summarize(steps = sum(steps)) %>%
  print
```

```
## # A tibble: 53 x 2
##          date steps
##        <date> <dbl>
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## # ... with 43 more rows
```

## Plotting the total daily steps with histogram


```r
hist(x=totaldailysteps$steps, 
     col = "blue", 
     breaks = 20, 
     xlab = "Daily Total Step", 
     ylab = "Frequency",
     main = "Distribution of daily total (missing data are ignored)")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

##computing the mean and median of steps


```r
mean_steps <- mean(totaldailysteps$steps, na.rm = TRUE)
median_steps <- median(totaldailysteps$steps, na.rm = TRUE)

five_minute_interval <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
```

##Plot the mean and median


```r
ggplot(five_minute_interval, aes(x = interval, y = steps)) + geom_line(color  = "blue")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

## Compute the total steps

```r
five_minute_interval[which.max(five_minute_interval$steps),]
```

```
## # A tibble: 1 x 2
##   interval    steps
##      <int>    <dbl>
## 1      835 206.1698
```

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

##Calculating total daily steps with missing data


```r
full_data <- data
data_with_nas <- is.na(full_data$steps)
mean_interval <- tapply(full_data$steps, full_data$interval, mean, na.rm = TRUE, simplify = TRUE)
full_data$steps[data_with_nas] <- mean_interval[as.character(full_data$interval[data_with_nas])]

sum(is.na(full_data$steps))
```

```
## [1] 0
```

```r
complete_data_total_daily_steps <- full_data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
```

```
## # A tibble: 61 x 2
##          date    steps
##        <date>    <dbl>
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## # ... with 51 more rows
```

##Plotting of steps per day including those  missing data


```r
ggplot(complete_data_total_daily_steps, aes(x = steps)) +
  geom_histogram(fill = "blue",  binwidth = 1000) + 
  labs(title = "Steps per day with missing data", x = "Steps per day", y = "Frequency")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

###Calculating the steps for weekday and weekend interval


```r
mean_step_full_data <- mean(complete_data_total_daily_steps$steps, na.rm = TRUE)
median_step_full_data <- median(complete_data_total_daily_steps$steps, na.rm = TRUE)


full_data <- mutate(full_data, weektype = ifelse(weekdays(full_data$date) == "Saturday" | weekdays(full_data$date) == "Sunday", "weekend", "weekday"))
full_data$weektype <- as.factor(full_data$weektype)
head(full_data)
```

```
##       steps       date interval weektype
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```

```r
full_data_interval <- full_data %>%
  group_by(interval, weektype) %>%
  summarize(steps = mean(steps))
```
###Plotting the steps in weekday and weekend interval


```r
ggplot(full_data_interval, aes(x = interval, y = steps, color = weektype)) +
  geom_line() + facet_wrap(~weektype, ncol = 1, nrow = 2)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
