
## Data Wizard Take Home Exercise
## Author: Giacomo Dalla Chiara
## Assignment Part 1

rm(list=ls()) #clean the console
library(ggplot2) #package used for plotting
#install.packages("lme4", repos='http://cran.us.r-project.org')
library(lme4) #package for estimating mixed-effect models



## --- Synthetic data generation

# Generating a synthetic dataset can help better understanding how different models behave under certain conditions (e.g. extreme values)
# Fix a seed number for random data generation to ensure replication
set.seed(61)

# Fix the total size of the sample to generate
n <- 1000

# Generate synthetic job dataset
job <- data.frame("driver_id"=sample(1:50, n, replace=TRUE),
                  "time_of_day"=sample(0:23, n, replace=TRUE), 
                  "pick_up_date"=sample(seq(as.Date("2023/02/13"), as.Date("2023/02/19"), by="day"), n, replace=T),
                  "earnings_pay"=rexp(n, rate = 0.045)) 

# Adding day of the week
job$day_of_week <- weekdays(job$pick_up_date) 

# Adding special events (e.g. super bowl)
job$special_event <- 0
job[c(1,50,467,835,456,23,788),"special_event"] <- 1
job[c(1,50,467,835,456,23,788),"earnings_pay"] <- job[c(1,50,467,835,456,23,788),"earnings_pay"]+400 #I'm adding $400 to earnings obtained during special events
job$special_event <- as.factor(job$special_event)

str(job)



## --- V0) Non-parametric model: mean

# First, I compute the hourly pay, from the trips data
hour_pay <- aggregate(job$earnings_pay, list(job$time_of_day, job$day_of_week, job$driver_id), FUN=sum)
names(hour_pay) <- c("time_of_day", "day_of_week", "driver_id", "hourly_earnings")

# Use the aggregate() function to compute the mean pay for each combination of day and time
avg_pay <- aggregate(hour_pay$hourly_earnings, list(hour_pay$time_of_day, hour_pay$day_of_week), FUN=mean)
names(avg_pay) <- c("time_of_day", "day_of_week", "value_mean")

# I then use ggplot to create a heatmap of the mean hourly earnings
avg_pay$lab_mean <- paste0("$",as.character(floor(avg_pay$value_mean)))
avg_pay$time_of_day <- as.factor(avg_pay$time_of_day)
avg_pay$day_of_week <- as.factor(avg_pay$day_of_week)
levels(avg_pay$day_of_week) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
hm_mean <- ggplot(avg_pay, aes(time_of_day, day_of_week)) +
  geom_tile(aes(fill = value_mean)) +
  geom_text(aes(label = lab_mean), size=3) +
  scale_fill_gradient(low = "white", high = "#1b98e0")
hm_mean



## --- V1) Non-parametric model: median

# Add column to the avg_pay dataset reporting the median hourly pay
avg_pay$value_med <- aggregate(hour_pay$hourly_earnings, list(hour_pay$time_of_day, hour_pay$day_of_week), FUN=median)$x

# Same as above, I use ggplot to plot a heatmap of avrage hourly earnings 
avg_pay$lab_med <- paste0("$",as.character(floor(avg_pay$value_med)))
avg_pay$time_of_day <- as.factor(avg_pay$time_of_day)
hm_med <- ggplot(avg_pay, aes(time_of_day, day_of_week)) +
  geom_tile(aes(fill = value_med)) +
  geom_text(aes(label = lab_med)) +
  scale_fill_gradient(low = "white", high = "#1b98e0")
hm_med

# Together with the point estimate for the average hourly pay, we should report the confidence interval (CIs)
# Add column to avg_pay containing sample size
avg_pay$sample_size <- aggregate(hour_pay$hourly_earnings, list(hour_pay$time_of_day, hour_pay$day_of_week), FUN=length)$x

# Display the point estimates with small sample size using a rule-based method
avg_pay[avg_pay$sample_size<5,]

# Compute confidence interval for the median hourly pay for Tuesday 11 am
wilcox.test(hour_pay[hour_pay$time_of_day==12 & hour_pay$day_of_week=="Tuesday","hourly_earnings"], alternative="two.sided", correct=TRUE, conf.int=TRUE, conf.level=0.95)$conf.int



## --- V2) Parametric model: linear regression
tmp <- hour_pay # I define a new temporary dataset tmp
tmp$day_of_week <- as.factor(tmp$day_of_week)
tmp$time_of_day <- as.factor(tmp$time_of_day)

# I estimate the regression parameters using the lm() function
modv2 <- lm(hourly_earnings~day_of_week+time_of_day, data=tmp)
summary(modv2)

# Add column to avg_pay where special_event==0
#avg_pay$special_event <- as.factor(0)

# Predict values using estimated regression model
avg_pay$value_regr <- predict(modv2, avg_pay)

# I use ggplot to visualize the new results
avg_pay$lab_regr <- paste0("$",as.character(floor(avg_pay$value_regr)))
hm_regr <- ggplot(avg_pay, aes(time_of_day, day_of_week)) +
  geom_tile(aes(fill = value_regr)) +
  geom_text(aes(label = lab_regr)) +
  scale_fill_gradient(low = "white", high = "#1b98e0")
options(repr.plot.width=11, repr.plot.height=4)
hm_regr

# Run model diagnostic to identify potential outliers and extreme values
plot(modv2)



## --- V3) Parametric model: mixed effect model
# A fault of the regression model in V2 is that it assumes that observations are independetly generated
# This is not true, since observations (trips) performed by the same drivers across multiple weeks might be correlated, and not independent
# NOTE: the following code does not work, as it assumes that we have data for multiple weeks, which we did not generate. It is meant only as an example of more complex analysis that could be done

tmp <- hour_pay # I define a new temporary dataset tmp
tmp$day_of_week <- as.factor(tmp$day_of_week)
tmp$time_of_day <- as.factor(tmp$time_of_day)
tmp$driver_id <- as.factor(tmp$driver_id)

# I estimate the regression parameters using the lmer() function
modv3 <- lmer(hourly_earnings ~ day_of_week + 
                time_of_day + 
                (1|driver_id), data=tmp)
summary(modv3)



