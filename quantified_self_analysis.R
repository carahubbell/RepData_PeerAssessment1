#------------------------------------------------
# Reproducible Research
# Cara Hubbell
# Week 1 Project
# February 2016
#------------------------------------------------

#LOADING AND PROCESSING THE DATA

#Unzip the activity file. Be sure it is in your wd.
list.files() #to see what is in the wd
unzip("./activity.zip")
list.files() #to see what we unzipped
data<-read.csv("activity.csv")
View(data)

#Process the data for our analysis
str(data) #We see that the date variable is a factor variable

#Change date from a factor variable to a date variable
data$date<-as.Date(as.character(data$date), format="%Y-%m-%d")

#Get a list of unique dates
dates<-unique(data$date)

#------------------------------------------------

#MEAN AND TOTAL DAILY STEPS

#Calculate the total steps by day, ignoring NA values
totalsteps<-tapply(data$steps, data$date, sum, na.rm=TRUE)

#Build a data frame of total steps
totalsteps<-data.frame(date=dates,totalsteps=as.numeric(totalsteps))

#Make a (pretty) histogram of total steps
require(RColorBrewer)
png("plot1totalsteps.png", width=480, height=480, units="px")
hist(totalsteps$totalsteps, breaks=10, 
     main="Total Daily Steps", 
     xlab="Total Daily Steps", ylab="Number of Days", 
     col=brewer.pal(10,"PRGn"), 
     ylim=c(0,20), xlim=c(0,25000)
     )
dev.off()

#Calculate and show the mean and median steps by day, ignoring NA values
meansteps<-tapply(data$steps, data$date, mean, na.rm=TRUE)
meansteps<-data.frame(date=dates,meansteps=as.numeric(meansteps))
View(meansteps, "Mean Steps By Day")

medsteps<-tapply(data$steps, data$date, median, na.rm=TRUE)
medsteps<-data.frame(date=dates,medsteps=as.numeric(medsteps))
View(medsteps, "Median Steps By Day")

#------------------------------------------------

#AVERAGE DAILY ACTIVITY PATTERN

#Build a time series plot of each 5-minute interval
timeMeanSteps<-tapply(data$steps, data$interval, mean, na.rm=TRUE)
png("plot2timemean.png", width=480, height=480, units="px")
plot(timeMeanSteps,type="l",
     main="Mean Steps By Time Interval",
     xlab="Time Interval", ylab="Mean Steps"
     )
dev.off()

#Find the time interval with the max mean steps
intervals<-unique(data$interval)
timeMeanSteps<-data.frame(interval=intervals,meansteps=as.numeric(timeMeanSteps))

x<-which.max(timeMeanSteps$meansteps)
y<-timeMeanSteps[x,]$interval

#Calculate clock time
strftime(as.POSIXct("1970-01-01") + as.difftime(y, units="mins"),"%H:%M")

#------------------------------------------------

#DEALING WITH MISSING VALUES

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

#Make a (pretty) histogram of total steps in data versus complete
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

#Calculate and report new mean and median
meanstepscomplete<-tapply(complete$steps, complete$date, mean)
meanstepscomplete<-data.frame(date=dates,meansteps=as.numeric(meanstepscomplete))
View(meanstepscomplete, "Mean Steps By Day (NAs Replaced")

medstepscomplete<-tapply(complete$steps, complete$date, median)
medstepscomplete<-data.frame(date=dates,medsteps=as.numeric(medstepscomplete))
View(medstepscomplete, "Median Steps By Day (NAs Replaced")

#What is the impact of replacing NAs?

#------------------------------------------------

#DIFFERENCES IN ACTIVITY ON WEEKDAYS VS WEEKENDS

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