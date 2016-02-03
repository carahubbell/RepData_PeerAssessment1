## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. 

This project makes use of data from a personal activity monitoring
device. The device collects data at 5 minute intervals throughout the
day. The data consists of two months of information from an anonymous
individual collected during the months of October and November 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this report can be downloaded from the following link and are also made available within this repository.

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in the dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)
* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format
* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations.

## Analysis

### Loading and preprocessing the data

```R
#Unzip the activity file. Be sure it is in your wd.
list.files() #to see what is in the wd
unzip("./activity.zip")
list.files() #to see what we unzipped
data<-read.csv("activity.csv")
View(data)
```

We can see that ```data``` consists of 17,568 observations of 3 variables: steps, date, and interval. Some of the values for steps are missing (coded as ```NA```). Now we will process the data a bit to make our analysis easier.

```R
#Change date from a factor variable to a date variable
data$date<-as.Date(as.character(data$date), format="%Y-%m-%d")

#Get a list of unique dates for use later
dates<-unique(data$date)
```

### What is the mean total number of steps taken per day?

For this part of the analysis, we will ignore any missing values in the dataset.

```R
#Calculate the total steps by day, ignoring NA values
totalsteps<-tapply(data$steps, data$date, sum, na.rm=TRUE)

#Build a data frame of total steps
totalsteps<-data.frame(date=dates,totalsteps=as.numeric(totalsteps))

#Make a histogram of total steps
require(RColorBrewer)
png("plot1efficient.png", width=480, height=480, units="px")
hist(totalsteps$totalsteps, breaks=10, 
     main="Total Daily Steps", 
     xlab="Total Daily Steps", ylab="Number of Days", 
     col=brewer.pal(10,"PRGn"), 
     ylim=c(0,20), xlim=c(0,25000)
     )
dev.off()
```

![Total Steps](https://github.com/carahubbell/ReproducibleResearch/blob/master/plot1totalsteps.png)

```R
#Calculate and show the mean and median steps by day, ignoring NA values

meansteps<-tapply(data$steps, data$date, mean, na.rm=TRUE)
meansteps<-data.frame(date=dates,meansteps=as.numeric(meansteps))
View(meansteps, "Mean Steps By Day")

medsteps<-tapply(data$steps, data$date, median, na.rm=TRUE)
medsteps<-data.frame(date=dates,medsteps=as.numeric(medsteps))
View(medsteps, "Median Steps By Day")
```

### What is the average daily activity pattern?

```R
#Build a time series plot of each 5-minute interval
timeMeanSteps<-tapply(data$steps, data$interval, mean, na.rm=TRUE)

png("plot2timemean.png", width=480, height=480, units="px")
plot(timeMeanSteps,type="l",
     main="Mean Steps By Time Interval",
     xlab="Time Interval", ylab="Mean Steps"
     )
dev.off()
```

![Time Series Plot](https://github.com/carahubbell/ReproducibleResearch/blob/master/plot2timemean.png)

```R
#Find the time interval with the max mean steps
intervals<-unique(data$interval)
timeMeanSteps<-data.frame(interval=intervals,meansteps=as.numeric(timeMeanSteps))

x<-which.max(timeMeanSteps$meansteps)
y<-timeMeanSteps[x,]$interval

#Calculate clock time
strftime(as.POSIXct("1970-01-01") + as.difftime(y, units="mins"),"%H:%M")
```

### Imputing missing values

In our dataset, there are a number of days/intervals where there are missing values (coded as ```NA```). The presence of missing days may introduce bias into some calculations or summaries of the data.

```R
#Calculate total number of missing values in dataset
sum(is.na(data$steps))

#Create a copy of data so we can replace NA values
complete<-data

#Replace NA values with mean steps for corresponding interval
for(i in 1:length(complete$steps)){
  if(is.na(complete$steps[i])){
    complete$steps[i]<-timeMeanSteps[which(complete$interval[i] == timeMeanSteps$interval),]$meansteps
  }
}

#Make a histogram of total steps in data versus complete
totalstepscomplete<-tapply(complete$steps, complete$date, sum)
totalstepscomplete<-data.frame(date=dates,totalsteps=as.numeric(totalstepscomplete))

png("plot3totalcomplete.png", width=480, height=480, units="px")

par(mfrow=c(1,2), cex=.5)

hist(totalsteps$totalsteps, breaks=10, 
     main="Total Daily Steps (Actual)",
     xlab="Total Daily Steps", ylab="Number of Days", 
     col=brewer.pal(9,"BuPu"), 
     ylim=c(0,25), xlim=c(0,25000)
)

hist(totalstepscomplete$totalsteps, breaks=10, 
     main="Total Daily Steps (NAs Replaced)", 
     xlab="Total Daily Steps", ylab="Number of Days", 
     col=brewer.pal(9,"BuPu"), 
     ylim=c(0,25), xlim=c(0,25000)
)

dev.off()
```

![Total Steps No NA](https://github.com/carahubbell/ReproducibleResearch/blob/master/plot3totalcomplete.png)

```R
#Calculate and report new mean and median
meanstepscomplete<-tapply(complete$steps, complete$date, mean)
meanstepscomplete<-data.frame(date=dates,meansteps=as.numeric(meanstepscomplete))
View(meanstepscomplete, "Mean Steps By Day (NAs Replaced")

medstepscomplete<-tapply(complete$steps, complete$date, median)
medstepscomplete<-data.frame(date=dates,medsteps=as.numeric(medstepscomplete))
View(medstepscomplete, "Median Steps By Day (NAs Replaced")
```

Discussion about effect of NAs and replacements

### Are there differences in activity patterns between weekdays and weekends?

To compare the category of day (weekdays versus weekends), we will use the ```complete``` dataset which has all NA values replaced by the mean steps of the corresponding time interval.

```R
#Create a new factor variable for weekday/weekend
complete$day <- weekdays(complete$date)
complete$category[(complete$day == "Saturday" | complete$day == "Sunday")] <- "weekend"
complete$category[!(complete$day == "Saturday" | complete$day == "Sunday")] <- "weekday"

#Make a panel plot comparing categories of days
png("plot4daycategory.png", width=480, height=480, units="px")
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
  x <- aggregate(steps ~ interval, 
                 data = complete, 
                 subset = complete$category == type, 
                 FUN = mean)
  plot(x, type = "l", main = type)
}
dev.off()
```

![Weekdays vs Weekends](https://github.com/carahubbell/ReproducibleResearch/blob/master/plot4daycategory.png)



