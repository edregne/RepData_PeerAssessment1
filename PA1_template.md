---
title: "Reproducible Research: Peer Assessment 1"
author: "Me"
date: "8/20/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Read in activity data

```{r movement}
movement <- read.csv("activity.csv")
movement$date <- as.Date(movement$date)
```

# Calculate total number of steps taken each day

``` {r }
DaySteps <- aggregate(movement$steps ~ movement$date, FUN = sum)
DaySteps$steps <- as.numeric(DaySteps$`movement$steps`)
```
# Histogram of the total number of steps taken each day
```{r }
hist(DaySteps$steps, breaks=7, xlab="Number of Steps", main="Total Steps per Day", col="blue")
```

# Calculate mean steps
``` {r }
MeanSteps <- aggregate(movement$steps ~ movement$date, FUN = mean)
```

# Calculate median
```{r }
MedianSteps <- median(DaySteps$steps, na.rm = TRUE)
```

# Line chart of the average number of steps per 5 minute interval 
``` {r }
interval_steps <- aggregate(movement$steps ~ movement$interval, FUN = mean)
colnames(interval_steps) <- c("interval", "steps")

plot(interval_steps$interval, interval_steps$steps, 
     type="line", 
     main="Average Number of Steps Every 5 Minutes",
     xlab="5 Minute Interval", 
     ylab="Avg Steps")
```
# 5 minute interval with the maximum number of steps
```{r }
MaxSteps <- interval_steps[interval_steps$steps == max(interval_steps$steps), ]
```

# Number of missing values
```{r }
MissingSteps <- nrow(movement[is.na(movement$steps), ])
```

# Imputing missing steps in activity data

# Activity data is separated into two data frames, one for intervals having steps another for missing entries.
```{r }
has_steps <- movement[!is.na(movement$steps), ]
na_steps <- movement[is.na(movement$steps), ]
```

# From the previously created data frame for average steps by interval, merge average values to the data frame for missing steps.
```{r }
StepsFiller <- merge(na_steps, interval_steps, by = "interval")
```

# Drop missing steps from the data frame.
```{r }
StepsFiller <- StepsFiller[c("steps.y", "date", "interval")]
```

# Combine the data frames back together now that missing values have been loaded with average steps.
```{r }
colnames(StepsFiller) <- c("steps", "date", "interval")
imputed_steps <- rbind(has_steps, StepsFiller)
colnames(imputed_steps) <- c("imputed_steps", "date", "interval")
```

# Merge original data with the data step containing imputed steps for additional analysis.
```{r }
movement_imput <- merge(movement, imputed_steps, by = c("date", "interval"))
```

# Imputed mean 
```{r }
DayStepsImput <- aggregate(movement_imput$imputed_steps ~ movement_imput$date, FUN = sum)
DayStepsImput$source <- "Imputed Data"
colnames(DayStepsImput) <- c("date", "steps", "source")

colnames(DaySteps) <- c("date", "steps", "drop")
DaySteps <- DaySteps[ ,c(1:2)]
DaySteps$source <- "Original"

i_mean <- mean(DayStepsImput$steps)
i_median <- median(DayStepsImput$steps)

DayStepsGraph <- rbind(DaySteps, DayStepsImput)
```

# Plot showing the increase in the frequency of the observed steps.
```{r }
library(ggplot2)
ggplot(DayStepsGraph, aes(steps, fill = source)) + geom_histogram(alpha = 0.5, position = "identity")
```

# Analyze steps by weekdays.
```{r }
library(dplyr)
library(lattice)
```

# Use weekday function to get days
```{r }
movement_imput$weekday <- weekdays(movement_imput$date)
```

# Separate week days and weekend days and designate accordingly
```{r }
steps_weekdays <- movement_imput[movement_imput$weekday == "Monday" | movement_imput$weekday == "Tuesday" |
                                        movement_imput$weekday == "Wednesday" | movement_imput$weekday == "Thursday"                                         | movement_imput$weekday == "Friday", ]
steps_weekdays$day <- "Week Day"
steps_weekends <- movement_imput[movement_imput$weekday == "Saturday" | movement_imput$weekday == "Sunday", ]
steps_weekends$day <- "Weekend"

movement_day <- rbind(steps_weekdays, steps_weekends)
```

# Summarize data over intervals and days to get the average.
```{r }
movement_day <- movement_day %>%
        group_by(interval, day) %>%
        summarize(steps = mean(imputed_steps))
```

# Create the lattice plot comparing the average steps for week days and weekends.
```{r }
xyplot(steps~interval|day, data=movement_day, type="l",  layout = c(1,2),
       main="Average Steps for Weekdays and Weekends", 
       ylab="Average Number of Steps", xlab="5 Minute Interval")
```


