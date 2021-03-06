# read the data file
rf <- read.csv("activity.csv", header = TRUE)

# mean of the total mumber of steps per day

#read only the complete cases
rf2 <- na.omit(rf)

# get sum of the steps as per date
steps_per_day <- aggregate(steps~date, rf2, sum)

# create histogram of total number of steps in a day
hist(steps_per_day$steps, col=1, main="Total number of steps per day")

# get mean and mean 
mean(steps_per_day$steps)
median(steps_per_day$steps)

# get mean of steps in an interval
mean_per_interval <- aggregate(steps~interval, rf2, mean)

# create line plot
plot(mean_per_interval$interval, mean_per_interval$steps, type="l", col=1,
     main = "Average number of steps averaged over all days")

# find the row of maximum average number of steps
mean_per_interval[which.max(mean_per_interval$steps),]

# Imputing missing values

# get rows with NA's
rf_NA <- rf[!complete.cases(rf),]
# get number of rows
nrow(rf_NA)

# perform the imputation by replacing NA by the mean of 5-minute interval
for (i in 1:nrow(rf)){
  if(is.na(rf$steps[i])){
    interval_value <- rf$interval[i]
    row_id <- which(mean_per_interval$interval == interval_value)
    steps_value <- mean_per_interval$steps[row_id]
    rf$steps[i] <- steps_value
  }
}


# aggregate steps as per date to get total number of steps
steps_per_day_imputed <- aggregate(steps ~ date, rf, sum)

# create histogram
hist(steps_per_day_imputed$steps, col=1, main="Imputed histogram of total number of steps per day")

# get mean and median of total number of steps per day
mean(steps_per_day_imputed$steps)
median(steps_per_day_imputed$steps)

# imputation of the data had changed the median slightly, but 
# the mean remains the same

# Differences in weekdays and weekends

# convert date from string to date class
rf$date <- as.Date(rf$date)

# add column to find the weekdays
rf$day <- weekdays(rf$date)

# add new column called day type
rf$day_type <- c("weekday")

# if say is saturday or sunday, make weekend
# I use Rstudio in Korean so I have to use "配夸老" and "老夸老" instead of 
# "sunday" and "saturday"
for(i in 1:nrow(rf)){
  if (rf$day[i]=="配夸老" || rf$day[i]=="老夸老"){
    rf$day_type[i] <- "weekend"
  }
}

# convert day-type to factor
rf$day_type<- as.factor(rf$day_type)

# aggregate the value
mean_per_interval_imputed <- aggregate(steps ~ interval+day_type, rf, mean)

# get the ggplot from library
library(ggplot2)

# create plot
qplot(interval, steps, data=mean_per_interval_imputed, facets = .~ day_type, geom=c("line"), xlab="Interval", 
      ylab="Number of steps", main="")
# there is difference between weekdays and weekend