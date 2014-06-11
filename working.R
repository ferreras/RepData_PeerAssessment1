library(lattice)

fn <- function(){
    Sys.setlocale("LC_TIME","English")
    
    par(mfrow=c(2,1))
    ## Loading and preprocessing the data
    ## Load the data (i.e. read.csv()
    ## Process/transform the data (if necessary) into a format suitable for your analysis
    ## Antonio Perhaps automatic download?
    
    unzip("./activity.zip")
    mT <<- read.csv(file="./activity.csv")
    steps <<- matrix(mT$steps, nrow=288, ncol=61)
    dates <<- as.Date(mT$date[288*c(1:61)])
    intervals <<- mT$interval[1:288]
    
    ## What is mean total number of steps taken per day?
    ##For this part of the assignment, you can ignore the missing values in the dataset.
    ## Make a histogram of the total number of steps taken each day
    ## Calculate and report the mean and median total number of steps taken per day
    
    TotalStepsPerDay <<- apply(steps,2,sum, na.rm=TRUE)

    ## What is the average daily activity pattern?
    
    #    hist(meanStepsPerDay)
    hist(TotalStepsPerDay,breaks=20, col="red", xlab="Steps", main="Total Steps per Day")
    
    meanTotalPearDay <<- (mean(TotalStepsPerDay))
    medianTotalPerDay <<- (mean(TotalStepsPerDay))
                
    ## Imputing missing values
    ## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    TotalDaysMissing = sum(apply(steps,2, function(x){return(sum(is.na(x))!=0)}))

    
    ## Devise a strategy for filling in all of the missing values in the dataset. 
    ##The strategy does not need to be sophisticated. 
    ## For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    medianPerInterval <- apply(steps, 1, median,na.rm=TRUE)
        
    # Create a new dataset that is equal to the original dataset but with the missing data filled in.
    new_steps <<- apply(steps, 2, function(x) {return(ifelse(is.na(x),medianPerInterval,x))})
    
    # Make a histogram of the total number of steps taken each day and 
    # Calculate and report the mean and median total number of steps taken per day. 
    # Do these values differ from the estimates from the first part of the assignment? 
    # What is the impact of imputing missing data on the estimates of the total daily number of steps?
    NewTotalStepsPerDay <<- apply(new_steps,2,sum, na.rm=TRUE)
    
    hist(NewTotalStepsPerDay,breaks=20, col="blue", xlab="Steps", main="Total Steps per Day")

    ## Are there differences in activity patterns between weekdays and weekends?

#    hist(NewTotalStepsPerDay[weekdays(dates,abbreviate=TRUE) %in% c("Mon","Tue","Wed","Thu","Fri")],
#         breaks=20, col="blue", xlab="Steps", main="Total Steps per WEEKDAYS")
    
#    hist(NewTotalStepsPerDay[weekdays(dates,abbreviate=TRUE) %in% c("Sat","Sun")],
#         breaks=20, col="blue", xlab="Steps", main="Total Steps per WEEKENDS")
    
#    boxplot(NewTotalStepsPerDay[weekdays(dates,abbreviate=TRUE) %in% c("Mon","Tue","Wed","Thu","Fri")], 
#            NewTotalStepsPerDay[weekdays(dates,abbreviate=TRUE) %in% c("Sat","Sun")])
    
#    boxplot(log(NewTotalStepsPerDay[weekdays(dates,abbreviate=TRUE) %in% c("Mon","Tue","Wed","Thu","Fri")]), 
#            log(NewTotalStepsPerDay[weekdays(dates,abbreviate=TRUE) %in% c("Sat","Sun")]))

#    par(mfrow=c(1,2))
    daytype <<- factor(x=weekdays(dates,abbreviate=TRUE) %in% c("Sat","Sun"),
                       levels=c(TRUE,FALSE), labels=c("WeekEnd", "WeekDay"))
    
    MeanPerIntervalWeekDays <- apply(new_steps[,daytype=="WeekDay"], 1, mean)
    MeanPerIntervalWeekEnds <- apply(new_steps[,daytype=="WeekEnd"], 1, mean)
    # Make a panel plot

    print("hola")
    xyplot(daytype ~ MeanPerIntervalWeekDays)
#    plot(intervals, MeanPerIntervalWeekDays, type="l")
#    plot(intervals, MeanPerIntervalWeekEnds, type="l")
    
    
    Sys.setlocale("LC_TIME","Spanish_Spain.1252")
}