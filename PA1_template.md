---
title: "PA1_template.Rmd"
output: 
    html_document:
        keep_md: true
---



First things first, we want to load and clean up the data. This will assume that you have downloaded and unzipped the file from the Course Page, and that it is set in your current working directory, with the default name "activity.csv".
The code has quite simply read in the csv table, including the header, and has then converted the date field in to actual date format, rather than factors.
At this time I have also included the code to install and activate the ggPlot2 package, as all plots in this file will be done using ggPlot2.


```r
steps<-read.csv(file="activity.csv",header = TRUE, na.strings = "NA")
steps$date=as.Date(as.character(steps$date))

library(ggplot2)
```

We start by looking at the total number of steps on each day. Therefore we must begin by calculating the total:


```r
dailysteps<-tapply(steps$steps, steps$date,sum, na.rm=TRUE)
df.daily<-data.frame(date=unique(steps$date), total=dailysteps)
```

We first examine a histogram of the total number of steps per day, as well as the mean and median number of steps in a given day:


```r
ggplot(data=df.daily, aes(total))+geom_histogram()+labs(title="Histogram of daily steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
totalmean<-mean(dailysteps)
totalmedian<-median(dailysteps)

totalmean
```

```
## [1] 9354.23
```

```r
totalmedian
```

```
## [1] 10395
```

Next we examine the average pattern of activity on a single day, as opposed to the average total daily activity.
Again, this requires us to first perform our basic calculations:


```r
intervalmean<-tapply(steps$steps,steps$interval, mean, na.rm=TRUE)
df.results<-data.frame(interval=unique(steps$interval),mean=intervalmean)
```

Our examination starts with a plot of the average value for each interval.
Then we want to know at what time interval in the day the maximum number of steps occurs on average.


```r
ggplot(data=df.results,aes(interval,mean))+geom_line()+labs(
    title="Average number of steps in each interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
timemax<-df.results[df.results$mean==max(df.results$mean),1]
timemax
```

```
## [1] 835
```

Examining the data, its immediately obvious that there are lots of missing values.
We can easily determine exactly how many as follows:


```r
missingnum<-sum(is.na(steps$steps))
missingnum
```

```
## [1] 2304
```

We would like to replace the missing values and see what impact this has on the data.
We will replace any NA values with the average value for that interval as previously calculated:


```r
stepsnew<-steps
for (i in 1 : length(steps$steps)){
    if (is.na(stepsnew$steps[i])==TRUE){
        stepsnew$steps[i]=df.results[df.results$interval==stepsnew$interval[i],2]
    }
}
```

Now we want to re-examine the same results as in part 1 using this new data: 
*the histogram of daily total steps
*the mean number of daily steps
*the median number of daily steps

The methodology used is exactly the same, but using the stepsnew data we just created.


```r
newdailysteps<-tapply(stepsnew$steps, stepsnew$date,sum, na.rm=TRUE)
df.daily2<-data.frame(date=unique(stepsnew$date), total=newdailysteps)

ggplot(data=df.daily2, aes(total))+geom_histogram()+labs(title="New histogram of daily steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
newtotalmean<-mean(newdailysteps)
newtotalmedian<-median(newdailysteps)

newtotalmean
```

```
## [1] 10766.19
```

```r
newtotalmedian
```

```
## [1] 10766.19
```

We can see that both values have increased, and in fact return the same value.
This is unusual, but we should remember that we replaced all the missing values with the average values.
This means that each days total will be pulled towards the total of the averages.
Indeed a quick check reveals that the daily mean and median values are almost exactly the same as the total of the individual interval means:


```r
sum(intervalmean)
```

```
## [1] 10766.19
```

Finally we are asked to examine the difference between weekday and weekend information. 
To do this we first need to add a factor variable to our data indicating which category a given observation is in:


```r
for (i in 1:length(stepsnew$steps)){
    
    if (weekdays(stepsnew$date[i])=="Saturday"|weekdays(stepsnew$date[i])=="Sunday"){
        stepsnew$weekend[i]="Weekend"
    }
    else{ 
        stepsnew$weekend[i]="Weekday"
    }
}

stepsnew$weekend<-as.factor(stepsnew$weekend)
```

Now we will create data similar to question 2, by splitting the data out in to each level, and using the same methodology to calcaulate means for each time interval:


```r
weekdays<-stepsnew[stepsnew$weekend=="Weekday",]
weekends<-stepsnew[stepsnew$weekend=="Weekend",]

weekdayintervalmean<-tapply(weekdays$steps,weekdays$interval, mean, na.rm=TRUE)
weekendintervalmean<-tapply(weekends$steps,weekends$interval, mean, na.rm=TRUE)

df.weekdays<-data.frame(interval=unique(weekdays$interval),mean=weekdayintervalmean)
df.weekends<-data.frame(interval=unique(weekends$interval),mean=weekendintervalmean)
```

Now that we have the data we need, we can create the panel plot that we need to compare the averages in each dataset:

```r
p1<-ggplot(data=df.weekdays,aes(interval,mean))+geom_line()+labs(
    title="Average number of steps in each interval on weekdays")

p2<-ggplot(data=df.weekends,aes(interval,mean))+geom_line()+labs(
    title="Average number of steps in each interval on weekends")

library(grid)
pushViewport(viewport(layout=grid.layout(2,1)))
print(p1,vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(p2,vp=viewport(layout.pos.row=2, layout.pos.col=1))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

