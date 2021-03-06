library(lubridate)
library(ggplot2)
library(knitr)
library(dplyr)

#Cleaning data
data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character", "integer"))
head(data)

data$date <-ymd(data$date)

str(data)
head(data)

# Compute for the totaldailysteps and plot the result
# compute for the total steps per day
totaldailysteps <- data %>% 
  filter(!is.na(steps)) %>% 
  group_by(date) %>% 
  summarize(steps = sum(steps)) %>%
  print

# method 1
hist(x=totaldailysteps$steps, 
     col = "blue", 
     breaks = 20, 
     xlab = "Daily Total Step", 
     ylab = "Frequency",
     main = "Distribution of daily total (missing data are ignored)")

#computing the mean and median of steps
mean_steps <- mean(totaldailysteps$steps, na.rm = TRUE)
median_steps <- median(totaldailysteps$steps, na.rm = TRUE)

five_minute_interval <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))


ggplot(five_minute_interval, aes(x = interval, y = steps)) + geom_line(color  = "blue")

five_minute_interval[which.max(five_minute_interval$steps),]


sum(is.na(data$steps))

full_data <- data
data_with_nas <- is.na(full_data$steps)
mean_interval <- tapply(full_data$steps, full_data$interval, mean, na.rm = TRUE, simplify = TRUE)
full_data$steps[data_with_nas] <- mean_interval[as.character(full_data$interval[data_with_nas])]

sum(is.na(full_data$steps))

complete_data_total_daily_steps <- full_data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print

ggplot(complete_data_total_daily_steps, aes(x = steps)) +
  geom_histogram(fill = "blue",  binwidth = 1000) + 
  labs(title = "Steps per day with missing data", x = "Steps per day", y = "Frequency")

mean_step_full_data <- mean(complete_data_total_daily_steps$steps, na.rm = TRUE)
median_step_full_data <- median(complete_data_total_daily_steps$steps, na.rm = TRUE)


full_data <- mutate(full_data, weektype = ifelse(weekdays(full_data$date) == "Saturday" | weekdays(full_data$date) == "Sunday", "weekend", "weekday"))
full_data$weektype <- as.factor(full_data$weektype)
head(full_data)

full_data_interval <- full_data %>%
  group_by(interval, weektype) %>%
  summarize(steps = mean(steps))

ggplot(full_data_interval, aes(x = interval, y = steps, color = weektype)) +
  geom_line() + facet_wrap(~weektype, ncol = 1, nrow = 2)
