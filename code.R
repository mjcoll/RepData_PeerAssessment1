# Read and format the raw activity data
data <- read.csv("activity/activity.csv", na.strings = NA, colClasses = c("numeric", "character", "numeric"))
data$date <- as.Date(data$date, format="%Y-%m-%d")

## Question: What is mean total number of steps taken per day?

# Per the instructions, ignore the NA data
no_na_data <- na.omit(data)

# Calculate and plot the total number of steps
total_steps_by_day <- aggregate(no_na_data$steps, by = list(no_na_data$date), FUN = sum)
names(total_steps_by_day) <- c("Date", "TotalSteps")
hist(total_steps_by_day$TotalSteps, main = "Histogram of Total Steps", xlab = "Total Steps", ylab = "Frequency")

# Calculate the mean and median number of steps
mean_steps <- mean(total_steps_by_day$TotalSteps)
median_steps <- median(total_steps_by_day$TotalSteps)

## Question: What is the average daily activity pattern?

# Calculate and plot the average steps for each time interval
avg_steps_by_int <- aggregate(no_na_data$steps, by = list(no_na_data$interval), FUN = mean)
names(avg_steps_by_int) <- c("Interval", "AverageSteps")
plot(avg_steps_by_int$Interval, avg_steps_by_int$AverageSteps, type = "l", main = "Average Steps by Time Interval", xlab = "Interval (min)", ylab = "Average Steps")

# Report the time interval with the maximum average steps
max_row <- order(avg_steps_by_int$AverageSteps, decreasing = TRUE)[1]
avg_steps_by_int[max_row, 1]

## Task: Imputing missing values

# Calculate the number of missing values
missing_vector <- !complete.cases(data)
sum(missing_vector)

# Devise a strategy for filling in missing data
# My strategy is to use the average steps by time interval data as a substitute

# Merge the activity data with the average steps by time interval
mrg_avg_steps_by_int <- merge(data, avg_steps_by_int, by.x = "interval", by.y = "Interval")

# Loop through merged data set and replace NA values with the average for that time interval
for(i in seq_along(mrg_avg_steps_by_int$steps)) {
    if(is.na(mrg_avg_steps_by_int[i,2])) {
        mrg_avg_steps_by_int[i,2] <- mrg_avg_steps_by_int[i,4]
    }
}

# Remove the column that was added during the merge, return the substituted dataset
sub_activity_data <- mrg_avg_steps_by_int[,1:3]

# Calculate and plot the total number of steps
sub_total_steps_by_day <- aggregate(sub_activity_data$steps, by = list(sub_activity_data$date), FUN = sum)
names(sub_total_steps_by_day) <- c("Date", "TotalSteps")
hist(sub_total_steps_by_day$TotalSteps, main = "Histogram of Total Steps with NA Substitution", xlab = "Total Steps", ylab = "Frequency")
# Main difference between this and original hist is there are more days with values, so frequency, especially in middle is higher

# Calculate the mean and median number of steps
sub_mean_steps <- mean(sub_total_steps_by_day$TotalSteps) # The mean steps do not change
sub_median_steps <- median(sub_total_steps_by_day$TotalSteps) # The median steps increase slightly and are now equal to the mean


## Question: Are there differences in activity patterns between weekdays and weekends?
# Use the substituted dataset for this task

# create a factor variable for weekday or weekend
sub_factor_data <- cbind(sub_activity_data, weekdays(sub_activity_data$date))
names(sub_factor_data) <- c("interval", "steps", "date", "day")
sub_factor_data$day <- sub("Monday|Tuesday|Wednesday|Thursday|Friday", "Weekday", sub_factor_data$day)
sub_factor_data$day <- sub("Saturday|Sunday", "Weekend", sub_factor_data$day)
sub_factor_data$day <- factor(sub_factor_data$day)

# Create a panel plot with a time series of 5 minute intervals on x axis and average number of steps, averaged across weekdays and weekends
avg_steps_by_int_day <- aggregate(sub_factor_data$steps, by = list(sub_factor_data$interval, sub_factor_data$day), FUN = mean)
names(avg_steps_by_int_day) <- c("Interval", "Day", "AverageSteps")

library(lattice)
print(xyplot(avg_steps_by_int_day$AverageSteps ~ avg_steps_by_int_day$Interval | avg_steps_by_int_day$Day, type = "l", layout = c(1,2), xlab = "Interval", ylab = "Number of Steps", main = "Average Steps vs. Time Interval by Day Type"))