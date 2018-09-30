---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
*Author: Scott Robertson*  
*OS: macOS 10.14*  
*R version: 3.5.1*  
*Required packages: dplyr, ggplot2, mice, ggpubr*

## Loading and preprocessing the data

```r
# Installing and loading required packages
if("dplyr" %in% rownames(installed.packages()) == FALSE){
      install.packages("dplyr")
}
if("ggplot2" %in% rownames(installed.packages()) == FALSE){
      install.packages("ggplot2")
}
if("mice" %in% rownames(installed.packages()) == FALSE){
      install.packages("mice")
}
if("ggpubr" %in% rownames(installed.packages()) == FALSE){
      install.packages("ggpubr")
}

library(dplyr)
library(ggplot2)
library(mice)
library(ggpubr)
```

1. Load the data (i.e. `read.csv()`)


```r
# Set working directory to local Github repo
setwd("~/R Programming/Coursera/RepData_PeerAssessment1")

# Unzip the activity.zip file
unzip("activity.zip")

# Read data into R and save as activity
activity <- read.csv("activity.csv", header = TRUE, sep = ",")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
# Convert date column into date format and interval column into factor format
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
activity$interval <- as.factor(activity$interval)
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
# Create a summary table of total steps per day
daily_sum <- activity %>% group_by(date) %>% summarise(steps = sum(steps))

# Generate historgram of total steps per day
ggplot(data = daily_sum, aes(x = steps)) + 
      geom_histogram(fill = "blue", binwidth = 1000) +
      labs(y = "Count of days in bin",
           x = "Sum of steps per day",
           title = "Total steps per day") 
```

![](PA1_template_files/figure-html/Total Daily Steps Histogram-1.png)<!-- -->

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
# Calculate mean and median number of steps per day
mean_steps <- mean(daily_sum$steps, na.rm = TRUE)
median_steps <- median(daily_sum$steps, na.rm = TRUE)
```

Mean number of steps per day:  **1.076619\times 10^{4}**  
Mean number of steps per day:  **10765**

## What is the average daily activity pattern?

```r
# Create a summary table of mean steps per interval
steps_per_interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
steps_per_interval$interval <- as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])

# Create a time series graph of the interval data
fig_2 <- ggplot(data = steps_per_interval, aes(x = interval, y = steps)) +
      geom_line(color = "red", size=1) +
      labs(y = "Mean steps",
           x = "Interval",
           title = "Mean steps per daily interval")

# Print histogram
fig_2
```

![](PA1_template_files/figure-html/Average Daily Activity-1.png)<!-- -->

```r
# Find the 5 minute interval with the maximum average number of steps
max_steps_interval <- steps_per_interval[which.max(steps_per_interval$steps),]
```

Max step interval: **835**  
Average number of steps: **206**

## Imputing missing values

```r
# Count the number of NA entries in the data set
na_entries <- sum(is.na(activity$steps))
all_entries <- nrow(activity)
percentage_na <- all_entries/na_entries
```

Number of NA entries: **2304**  
Percentage of NA entries: **7.625%**


```r
# Impute missing values using MICE package
impute_activty <- mice(activity, m = 1, maxit = 5, method = 'pmm', seed = 42)
```

```
## 
##  iter imp variable
##   1   1  steps
##   2   1  steps
##   3   1  steps
##   4   1  steps
##   5   1  steps
```

```r
# Extract imputed data into dataframe
clean_data <- complete(impute_activty)

# Create a summary table of total steps per day
clean_daily_sum <- aggregate(steps ~ date, data = clean_data, FUN = sum)

# Generate historgram of total steps per day
fig_1 <- ggplot(data = daily_sum, aes(x = steps)) + 
      geom_histogram(fill = "blue", binwidth = 1000) +
      labs(y = "Count of days in sample",
           x = "Number of steps per day") +
      ylim(0,10)

fig_2 <- ggplot(data = clean_daily_sum, aes(x = steps)) + 
      geom_histogram(fill = "green", binwidth = 1000) +
      labs(y = "Count of days in sample",
           x = "Number of steps per day") +
      ylim(0,10)

# Print histograms side by side to compare shape changes
ggarrange(fig_1, fig_2, labels = c("Total steps per day - Original Data", "Total steps per day - Imputed Data"), font.label = list(size = 10, face = "bold"), ncol = 1, nrow = 2)
```

![](PA1_template_files/figure-html/Impute Missing Values-1.png)<!-- -->

```r
# Calculate mean and median number of steps per day
clean_mean_steps <- mean(clean_daily_sum$steps)
clean_median_steps <- median(clean_daily_sum$steps)
```

: Imputed data comparison table

| Meassure | Original Data| Imputed Data| Variation|
|:---------|-------------:|------------:|---------:|
| Mean Steps | 1.0766189\times 10^{4}| 1.0635492\times 10^{4}| -130.696876|
| Median Steps | 10765| 10600| -165|


## Are there differences in activity patterns between weekdays and weekends?
