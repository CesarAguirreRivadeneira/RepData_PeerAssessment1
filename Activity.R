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

#5 minutes interval average
activityIntervalMean <- aggregate(steps ~ interval, activity, mean)
with(activityIntervalMean,plot(interval,steps,type="l",col="red",main="Average steps across all days"))

activityIntervalMean[activityIntervalMean$steps == max(activityIntervalMean$steps),1]

#calculate missing values

sum(is.na(activity$steps))

#Impute NA values with average of each 5 minute interval
activityImp <-activity
for(i in 1:nrow(activityImp)){
  interval <-activityImp[i,3]
  if(is.na(activityImp[i,1]))
    activityImp[i,1] <- activityIntervalMean[activityIntervalMean$interval==interval,2]
}

#Histogram
activityDay <- aggregate(steps ~ date, activityImp, sum)
hist(activityDay$steps,breaks = 10,xlab = "Steps",main = "Histogram of steps by date(Imputed)")
#Mean and Median
activityDayMeanImp <- aggregate(steps ~ date, activityImp, mean)
activityDayMedianImp <- aggregate(steps ~ date, activityImp, median)
par(mar=c(5, 4, 4, 10), xpd=TRUE)
with(activityDayMean,plot(date,steps,type="l",col="red",main="Mean and median steps(Imputed)"))
with(activityDayMedian,lines(date,steps,type="l",col="blue"))
with(activityDayMeanImp,lines(date,steps,type="l",col="green"))
with(activityDayMedianImp,lines(date,steps,type="l",col="yellow"))
#In the Rmarkdown the margin is -0.3
legend("topright", inset=c(-0.47,0),lty = c(1, 1, 1), col = c("red", "blue","green","yellow")
       , legend = c("Mean", "Median","Mean(Imputed)","Median(Imputed)"))
#Impact in total

par(mar=c(5, 4, 4, 8.1), xpd=TRUE)
activityDayImp <- aggregate(steps ~ date, activity, sum)
with(activityDay,plot(date,steps,type="l",col="red",main="Total steps(Imputed)"))
with(activityDayImp,lines(date,steps,type="l",col="blue"))
legend("topright", inset=c(-0.55,0),lty = c(1, 1, 1), col = c("red", "blue","green","yellow")
       , legend = c("Original", "Imputed"))

#create factor variable
activity$wday <-factor(weekdays(activity$date),
       levels = c("lunes", "martes", "miercoles", "jueves","viernes","sabado","domingo"),
       labels = c("weekday","weekday","weekday","weekday","weekday","weekend","weekend"))

#Plot average steps by weekday

activityWday <- aggregate(steps ~ interval+wday, activity, mean)

library(lattice) 
with(activityWday, 
  xyplot(steps~interval|wday,
            main="Average steps by weekday",
            xlab="Interval",
         type= "l",layout=c(1,2))
)



#par(mfrow = c(2,1), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
#with(activityWday, {
#  plot(Interval, Ozone, main = "Ozone and Wind")
#  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
#  plot(Temp, Ozone, main = "Ozone and Temperature")
#  mtext("Ozone and Weather in New York City", outer = TRUE)
#})



