# Peer Assesment 1
## Loading and preprocessing the data

Libraries required for this excercise:

```r
#Libraries required .
library(ggplot2)

# activity.csv should be in the current working directory. 
```

The first thing done was to load data into R and get the day, year and month for each date in data set. After this, below operations where done:

* Aggregate the steps taken per day, month and year.
* Get the mean for the whole data set.
* Get the median for the whole data set.



```r
# Read Data
l_data <- read.csv( file       = "activity.csv"
                  , header     = TRUE
                  , sep        = ","
                  , na.strings = "NA"
                  , colClasses = c("numeric", "Date", "numeric")
                  )

# Get day
l_data$day <- format( x      = l_data$date
                    , format =  "%d"	
                    )

# Take month and year
l_data$monthYr <- format( x      = l_data$date
                        , format =  "%B %Y"	
                        )

# Aggregate data
l_sum_p_day <- aggregate( formula = steps ~ day + monthYr
                        , data    = l_data
                        , FUN     = sum
                        , na.rm   = TRUE
                        )

# Get mean for whole dataset	
l_mean_all <- mean( x     = l_sum_p_day$steps
                  , na.rm = TRUE
                  ) 

# Get median for whole dataset	
l_median_all <- median( x     = l_sum_p_day$steps
                      , na.rm = TRUE
                      ) 
```

## What is mean total number of steps taken per day?

After data has been processed, plots were created layer by layer.


```r
# Initialize ggplot object
l_histogram <- ggplot( data = l_sum_p_day
                     , aes( y     = steps 
                          , x     = day
                          , fill  = monthYr
                          )
                     )

# Create and Customize histogram
l_histogram <- ( l_histogram + geom_histogram( stat = "identity"  ) 
                             + facet_grid( . ~ monthYr ) 
                             + labs( x =  "Days", y = "Steps", title = "Number of Steps Taken Per Day" ) 
                             + guides( fill = FALSE )
               ) 
```

![plot of chunk histogramPlot](figure/histogramPlot-1.png) 


The mean and median of the total number of steps taken per day are 10766.19 and 10765 respectively.

## What is the average daily activity pattern?

Data processing:


```r
# Aggregate Data
l_avg_p_int <- aggregate( formula = steps ~ interval
                        , data    = l_data
                        , FUN     = mean
                        , na.rm   = TRUE
                        )
```

Plot creation:


```r
# Initialize ggplot object
l_timeSeries <- ggplot( data = l_avg_p_int
                      , aes( y        = steps
                           , x        = interval
                           , colour   = steps
                           )
                      )

# Create and Customize Time Series
l_timeSeries <- ( l_timeSeries + geom_line( )
                               + labs( x =  "Intervals", y = "Steps", title = "Average Steps per Interval" ) 
                               + scale_colour_gradient( low = "Red", high = "Blue" )
                               + theme(legend.position="none")
                )
```

![plot of chunk timeSeriesPlot](figure/timeSeriesPlot-1.png) 

Getting the interval with the highest step average:

```r
# Get Max Value
l_max   <- max( l_avg_p_int$steps, na.rm = TRUE )

# Get Max Value Index
l_index <- which.max( x = l_avg_p_int$steps )

# Get Interval with Max Value
l_inter <- l_avg_p_int[ l_index, "interval" ]
```
Interval 835 is the interval with the highest step average ( 206.1698113 ).

## Imputing missing values


```r
# Get all rows who have NA steps
l_na       <- is.na( x = l_data$steps )

# Get the count of those rows
l_na_count <- sum( l_na )
```

The total number of NA values in the dataset is 2304.


```r
# Create new data set
l_data2 <- l_data

# Get unique intervals who have NA
l_unique_interval <- unique( l_data$interval [ l_na ] )

# Loop through those unique intervals and set value. Median per interval calculated before was
# used to fill in the values.
for( i in l_unique_interval )
{
	l_data2$steps[ l_na & l_data2$interval == i ] <- l_avg_p_int$steps[ l_avg_p_int$interval == i ]
	l_data2$flag[ l_na & l_data2$interval == i ] <- "UPDATED"
}


# Aggregate data
l_sum_p_day2 <- aggregate( formula = steps ~ day + monthYr
                         , data    = l_data2
                         , FUN     = sum
                         , na.rm   = TRUE
                         )

# Get mean for whole dataset	
l_mean_all2 <- mean( x     = l_sum_p_day2$steps
                   , na.rm = TRUE
                   ) 

# Get median for whole dataset	
l_median_all2 <- median( x     = l_sum_p_day2$steps
                       , na.rm = TRUE
                       ) 
```


```r
# Override existing l_histogram
l_histogram <- l_histogram + geom_histogram( data       = l_sum_p_day2
                                           , aes( y     = steps 
                                                , x     = day
                                                , fill  = monthYr
                                                )
                                           , stat = "identity"
                                           , na.rm = TRUE
                                           )
```

![plot of chunk histogramPlot2](figure/histogramPlot2-1.png) 
The mean and median of the total number of steps taken per day are 10766.19 and 10766.19 respectively.

## Are there differences in activity patterns between weekdays and weekends?


```r
# Set label weekend or weekday for date
l_data2$week <- ifelse( weekdays(l_data2$date) %in% c( "Saturday", "Sunday" )
                      , "Weekend"
                      , "Weekday" 
                      )

l_avg_week_int <- aggregate( formula = steps ~ interval + week
                           , data    = l_data2
                           , FUN     = mean
                           , na.rm   = TRUE
                           )

# Create ggplot object
l_timeSeries2 <- ggplot( data = l_avg_week_int
                       , aes( y        = steps
                            , x        = interval
                            , color    = week
                            )
                       )

# Create and Customize Time Series
l_timeSeries2 <- ( l_timeSeries2 + geom_line( aes( colour = week ) )
                                 + labs( x     = "Intervals"
                                       , y     = "Steps"
                                       , title = "Average Steps per Interval per Weekday" 
                                       ) 
                                 + theme( legend.position="none" )
                                 + facet_grid( . ~ week )
                 )
```
![plot of chunk weekPlot](figure/weekPlot-1.png) 
