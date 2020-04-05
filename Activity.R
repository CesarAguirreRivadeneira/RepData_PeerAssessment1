#Load Data

activity <- read.csv("activity.csv")
#Transform column date to a valid Date
activity <- transform(activity, date = as.Date(date, "%Y-%m-%d"))
#Total number of steps each day
activityDay <- aggregate(steps ~ date, activity, sum)
hist(activityDay$steps,breaks = 10,xlab = "Steps",main = "Histogram of steps by date")

activityDayMean <- aggregate(steps ~ date, activity, mean)
activityDayMedian <- aggregate(steps ~ date, activity, median)
par(mar=c(5, 4, 4, 8.1), xpd=TRUE)
with(activityDayMean,plot(date,steps,type="l",col="red",main="Mean and median steps"))
with(activityDayMedian,lines(date,steps,type="l",col="blue"))
#In the Rmarkdown the margin is -0.3
legend("topright", inset=c(-0.55,0),lty = c(1, 1, 1), col = c("red", "blue"), legend = c("Mean", "Median"))
