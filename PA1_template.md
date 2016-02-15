# Reproducible Research: Peer Assessment 1

####Load packages that will be used

library(dplyr)
library(lubridate)
library(knitr)

# Loading and preprocessing the data
####Load the CSV file using "read.csv"

fbdata<-read.csv("activity.csv")

####Arrange the date as year-month-day

fbdata$date<-ymd(fbdata$date)

# What is mean total number of steps taken per day?
####Add the steps by day filtering na and create a histogram

stp_day<-fbdata %>%
filter(!is.na(steps)) %>%
group_by(date) %>%
summarize(steps=sum(steps)) %>%
print

####Load ggplot2 to create the histogram using geom_histogram

library(ggplot2)
ggplot(stp_day, aes(x=steps)) +
geom_histogram(fill="blue",binwidth=1000) +
labs(title="Steps per Day",x="Number of Steps",y="Frequency")

####Calcualte the mean and median for the steps taken each day

stpmean <- mean(stp_day$steps)
stpmedian<-median(stp_day$steps)

# What is the average daily activity pattern?
####Calculate the mean for each interval for all days and create a plot using geom_line with ggplot.

stpint<-fbdata %>%
filter(!is.na(steps)) %>%
group_by(interval) %>%
summarize(steps=mean(steps)) %>%
print

ggplot(stpint, aes(x=interval, y= steps)) +
geom_line(color="blue") +
labs(title="Steps in Intervals", x="Intervals", y="Steps")

####Calculating interval with the most steps

stpint[which.max(stpint$steps),]

# Imputing missing values
####Calculate and report all the missing values from the data set, fbdata.  Fill all the values with the mean for the given 5-minute interval.

sum(is.na(fbdata$steps))

fbdata_na<-fbdata
meanint<- tapply(fbdata_na$steps, fbdata_na$interval, mean, na.rm=TRUE, simplify=TRUE)
sumnas<-is.na(fbdata_na$steps)

fbdata_na$steps[sumnas]<-meanint[as.character(fbdata_na$interval[sumnas])]

####Make sure there are no missing values.
sum(is.na(fbdata_na$steps))

####Calculate the number of steps taken in each of the 5-minute interval and use ggplot to create a histogram for the full data set.
complete_data<-fbdata_na %>%
filter(!is.na(steps)) %>%
group_by(date) %>%
summarize(steps=sum(steps)) %>%
print



ggplot(complete_data, aes(x=steps)) +
geom_histogram(fill="red",binwidth=1000) +
labs(title="Steps per Day w/o NA",x="Number of Steps per Day",y="Frequency")

####Calculate the new mean and median with the added values.

stpmean_full<-mean(complete_data$steps, na.rm=TRUE)
stpmedian_full<-median(complete_data$steps, na.rm=TRUE)

# Are there differences in activity patterns between weekdays and weekends?

fbdata_na<-mutate(fbdata_na, weektype=ifelse(weekdays(fbdata_na$date)=="Saturday"|weekdays(fbdata_na$date)=="Sunday", "weekend", "weekday"))
fbdata$weektype<-as.factor(fbdata_na$weektype)
head(fbdata_na)

fbdata_na_interval<-fbdata_na %>%
group_by(interval, weektype) %>%
summarize(steps=mean(steps))

plots<-ggplot(fbdata_na_interval, aes(x=interval, y= steps, color=weektype)) +
geom_line() +
facet_wrap(~weektype, ncol=1, nrow=2)

print(plots)
