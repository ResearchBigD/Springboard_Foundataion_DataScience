print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("Please run Walmart_Functions.R before this script")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

print("You are running this Walmart_model.R at")
Sys.time()

setwd("C:/Users/Prabhat_nonadmin/Desktop/BigData/R_Programming/WorkingDirectory/Capstone_Walmart/Workplace")



# load libraries
library(lubridate)
library(tidyr)
library(dplyr)
library(timeDate)
library(ggplot2)
library(stringr)
library(rpart)
library(rpart.plot)

#Load training file provided by Walmart
Walmart.train <- read.csv("train.csv")
# data.frame':	4617600 obs. of  4 variables date as factor

#Convert date from factor to date in ymd format -->
Walmart.train$date <- ymd(Walmart.train$date)

#Created a new column to track, if shopping is being done from Mon to Thu or Fri to Sun
Walmart.train$IsMonToThu <- as.integer(isWeekday(Walmart.train$date, wday = 1:4))

#Created a new colun to check, if shopping is being done within 10 days of holidays
# Holidays checked are - 
# Super Bowl
# Labor Day
# Halloween
# Thanksgiving
# Christmas

#Walmart.train$HolidayShopping <- isHolidayShoppingDay(Walmart.train$date)
# Above code is commented, as it takes 12 hours to process the traing date
# I saved the file from last processing and used it for further anlaysis

Walmart.train <- read.csv("Walmart_HolidayShopping.csv")

Walmart.train$IsFriToSun <- NULL

#Convert date from factor to date in ymd format -->
Walmart.train$date <- ymd(Walmart.train$date)




#Copy date variable to split further into year, month and day
Walmart.train$date2 <- Walmart.train$date
Walmart.train <- Walmart.train %>% separate(date2, c("year", "month", "day"))

# Convert year, month and day to integer
Walmart.train$year <- as.integer(Walmart.train$year)
Walmart.train$month <- as.integer(Walmart.train$month)
Walmart.train$day <- as.integer(Walmart.train$day)

################# Add date variables #######
#Number days from Sunday as 1 to Saturday at 7
Walmart.train$day_7 <- as.integer(wday(Walmart.train$date))

#Number days from Jan 1st
Walmart.train$day_365 <- as.integer(strftime(Walmart.train$date, format = "%j"))

# Date as numeric
Walmart.train$date_numeric <- as.integer(Walmart.train$date)

############################################

#Removed outliers
# Item #5 in store #37 has a high unit sales value. Analyze +/- few days and see if the value is indeed an outlier
Walmart.train %>% filter((item_nbr == 5) & (store_nbr == 37) & (year == 2012) & (month == 11) & (day > 10) & (day <20))
#Decided to update it by average of sales on previous day and the next day
Walmart.train$units[1580534] <- as.integer(29)

# Item #5 in store #37 has another high unit sales value. Analyze +/- few days and see if the value is indeed an outlier
Walmart.train %>% filter((item_nbr == 5) & (store_nbr == 37) & (year == 2013) & (month == 11) & (day > 15) & (day <25))
#Decided to update it by average of sales on previous day and the next day
Walmart.train$units[3190034] <- as.integer(25)

# Item #93 in store #22 has a high unit sales value. Analyze +/- few days and see if the value is indeed an outlier
Walmart.train %>% filter((item_nbr == 93) & (store_nbr == 22) & (year == 2012) & (month == 03) & (day > 0) & (day <07))
#Decided to update it by average of sales on previous day and the next day
Walmart.train$units[305232] <- as.integer(14)

#write.csv(Walmart.train, "Walmart_outliersRemoved.csv")

# Cluster by weekday/weekend, holiday/regular or few months/entire year
# Can store be classified in 6 clusters
# 1 - Stores, where most customer by on weekdays
# 2 - Stores, where most customer by on weekends
# 3 - Stores, where most customer by on holidays
# 4 - Stores, where most customer by on non holidays
# 5 - Stores, where most customer by on few months of severe weather
# 6 - Stores, where most customer by on all months of the year
str(Walmart.train)

# Do not remove it 
#Walmart.train$X <- NULL

# We input - unit sales, Mon to Thu, Holiday, month
Cluster.train_1 <- Walmart.train %>% select(4,5,6,10)

# K means classification 
z <- Cluster.train_1
mean_z <- apply(z, 2, mean)
sd_z <- apply(z, 2, sd)
scale.Walmart.train <- scale(z, mean_z, sd_z )

k <- 6
set.seed(1)

KMC6 <- kmeans(scale.Walmart.train, centers = k, iter.max = 1000)
table(Walmart.train$store_nbr, KMC6$cluster)
plot(Walmart.train[c("store_nbr", "item_nbr")], col = KMC6$cluster)
# No meaningfull results

###################################
# Load weather data 
Weather <- read.csv("weather.csv")
Weather$date <- ymd(Weather$date)

# Converting M and T given in snowfall and preciptotal as 0 
Weather$snowfall <- gsub(pattern = "M", replacement = "0.0", Weather$snowfall)
Weather$snowfall <- gsub(pattern = "T", replacement = "0.0", Weather$snowfall)
Weather$preciptotal <- gsub(pattern = "M|T", replacement = "0", Weather$preciptotal)

Weather$snowfall <- as.numeric(Weather$snowfall)
Weather$preciptotal <- as.numeric(Weather$preciptotal)

Weather$date2 <- Weather$date
Weather <- Weather %>% separate(date2, c("year", "month", "day"))
Weather$year <- as.integer(Weather$year)
Weather$month <- as.integer(Weather$month)
Weather$day <- as.integer(Weather$day)

################################

## Flags for severe snow, mild snow, severe rain and severe weather

# Created a logical variable for severe snow
Weather$severeSnow <- isSevereSnow(Weather)

# Created a logical variable for mild snow
Weather$mildSnow <- isMildSnow(Weather)

# Created a logical variable for servere rain
Weather$severeRain <- isSevereRain(Weather)

# Created a logical variable for server weather
Weather$severeWeather <- isSevereWeather(Weather)

############################
# Variables to understand buying pattern before and after severe snow

# Customers may by 1,2 or 3 before severe snow
Weather <- func_Days3BeforeSevereSnow(Weather)

# Customers may by 1,2 or 3 after severe snow
Weather <- func_Days3AfterSevereSnow(Weather)

# Customers may buy today, if no severe snow is there today and tomorrow has higher snow Higher the number, more sales
Weather <- func_LessSnowThanTomorrow(Weather)

# Customers may buy today, if no severe snow is there today and snow is lesser than yesterday. Higher the number, more sales
Weather <- func_LessSnowThanYesterday(Weather)

#If there are continuous days of severe snow , people may tend to buy in advance or later
# Higer the number, more could be the sale
Weather <- func_ContinousDayOfSevereSnow_AfterToday(Weather)
Weather <- func_ContinousDayOfSevereSnow_BeforeToday(Weather)

############################
# Variables to understand buying pattern before and after severe rain

# Customers may by 1,2 or 3 before severe snow
Weather <- func_Days3BeforeSevereRain(Weather)

# Customers may by 1,2 or 3 after severe snow
Weather <- func_Days3AfterSevereRain(Weather)

# Customers may buy today, if no severe snow is there today and tomorrow has higher snow Higher the number, more sales
Weather <- func_LessRainThanTomorrow(Weather)

# Customers may buy today, if no severe snow is there today and snow is lesser than yesterday. Higher the number, more sales
Weather <- func_LessRainThanYesterday(Weather)

#If there are continuous days of severe snow , people may tend to buy in advance or later
# Higer the number, more could be the sale
Weather <- func_ContinousDayOfSevereRain_AfterToday(Weather)
Weather <- func_ContinousDayOfSevereRain_BeforeToday(Weather)

#############################################

# Join trainig, key and weather data -->
#Remove the columns, which are already present in training data
Weather_copy <- Weather
Weather_copy$year <- NULL
Weather_copy$month <- NULL
Weather_copy$day <- NULL

Key <- read.csv("key.csv")
Walmart.train_full <- left_join(Walmart.train, Key, by = "store_nbr")

Walmart.train_full <- left_join(Walmart.train_full, Weather_copy, by = c("station_nbr", "date" ))


#write.csv(Walmart.train_full, "Walmart.train_full.csv")

######################

# Round 1
# 1/3 - Split data
# 2/3 - Linear
# 3/3 - CART

# Round 2
# 1/3 - Remove all zeros 
# 2/3 - Linear
# 3/3 - CART

# Round 3
# Cluster weather stations and then stores
# Run linear for each cluster of stores
# Run CART for each cluster of store
# Linear regression

# Round 4 - Remove all zero sales
# Round 5 - Remove non event data

# Do it by item, by store, by item/store

# Round 1
# 1/3 - Split data


Wal_Weather_1 <- Walmart.train_full

Wal_Weather_1_Train <- subset(Wal_Weather_1, (year == 2012 | year == 2013))
Wal_Weather_1_Validate <- subset(Wal_Weather_1, (year == 2014))

# Round 1
# 2/3 - Linear


lm_1 <- lm(units ~ store_nbr + item_nbr + IsMonToThu +  HolidayShopping + month + day + day_7 + day_365 + date_numeric + snowfall + preciptotal + severeSnow + mildSnow + severeRain + severeWeather + Days3BeforeSevereSnow + Days3AfterSevereSnow + LessSnowThanTomorrow + LessSnowThanYesterday + ContinousDayOfSevereSnow_AfterToday + ContinousDayOfSevereSnow_BeforeToday + Days3BeforeSevereRain + Days3AfterSevereRain + LessRainThanTomorrow + LessRainThanYesterday + ContinousDayOfSevereRain_AfterToday + ContinousDayOfSevereRain_BeforeToday, data = Wal_Weather_1_Train)

summary(lm_1)
# Residual standard error: 9.883 on 3365714 degrees of freedom
# Multiple R-squared:  0.008454,	Adjusted R-squared:  0.008446 
# F-statistic:  1063 on 27 and 3365714 DF,  p-value: < 2.2e-16

lm_2 <- lm(units ~ store_nbr + item_nbr + IsMonToThu + month + day + day_7 + day_365 + date_numeric + preciptotal + severeSnow + mildSnow + Days3BeforeSevereSnow + Days3AfterSevereSnow + LessSnowThanTomorrow, data = Wal_Weather_1_Train)

summary(lm_2)
# Residual standard error: 9.883 on 3365727 degrees of freedom
# Multiple R-squared:  0.00845,	Adjusted R-squared:  0.008446 
# F-statistic:  2049 on 14 and 3365727 DF,  p-value: < 2.2e-16

lm_3 <- lm(units ~ store_nbr + item_nbr + IsMonToThu + month + day + day_7 + day_365 + date_numeric + preciptotal + severeSnow + mildSnow + Days3BeforeSevereSnow + Days3AfterSevereSnow, data = Wal_Weather_1_Train)

summary(lm_3)
# Residual standard error: 9.883 on 3365728 degrees of freedom
# Multiple R-squared:  0.008449,	Adjusted R-squared:  0.008445 
# F-statistic:  2206 on 13 and 3365728 DF,  p-value: < 2.2e-16

lm_4 <- lm(units ~ store_nbr + item_nbr + IsMonToThu + month + day + day_7 + day_365 + date_numeric + severeSnow + mildSnow + Days3BeforeSevereSnow + Days3AfterSevereSnow, data = Wal_Weather_1_Train)

summary(lm_4)
# Residual standard error: 9.883 on 3365729 degrees of freedom
# Multiple R-squared:  0.008448,	Adjusted R-squared:  0.008444 
# F-statistic:  2390 on 12 and 3365729 DF,  p-value: < 2.2e-16

lm_5 <- lm(units ~ store_nbr + item_nbr + IsMonToThu + month + day + day_7 + day_365 + date_numeric + mildSnow + Days3BeforeSevereSnow + Days3AfterSevereSnow, data = Wal_Weather_1_Train)

summary(lm_5)
# Residual standard error: 9.883 on 3365730 degrees of freedom
# Multiple R-squared:  0.008445,	Adjusted R-squared:  0.008442 
# F-statistic:  2606 on 11 and 3365730 DF,  p-value: < 2.2e-16

lm_6 <- lm(units ~ store_nbr + item_nbr + IsMonToThu + day + day_7 + date_numeric + mildSnow + Days3BeforeSevereSnow + Days3AfterSevereSnow, data = Wal_Weather_1_Train)

summary(lm_)
# Residual standard error: 9.883 on 3365732 degrees of freedom
# Multiple R-squared:  0.00844,	Adjusted R-squared:  0.008438 
# F-statistic:  3183 on 9 and 3365732 DF,  p-value: < 2.2e-16

# Round 1
# 3/3 - CART


CART_1 <- rpart(units ~ store_nbr + item_nbr + IsMonToThu +  HolidayShopping + month + day + day_7 + day_365 + date_numeric + snowfall + preciptotal + severeSnow + mildSnow + severeRain + severeWeather + Days3BeforeSevereSnow + Days3AfterSevereSnow + LessSnowThanTomorrow + LessSnowThanYesterday + ContinousDayOfSevereSnow_AfterToday + ContinousDayOfSevereSnow_BeforeToday + Days3BeforeSevereRain + Days3AfterSevereRain + LessRainThanTomorrow + LessRainThanYesterday + ContinousDayOfSevereRain_AfterToday + ContinousDayOfSevereRain_BeforeToday, data = Wal_Weather_1_Train)

prp(CART_1)
# Only item_nbr and store_nbr are used

CART_2 <- rpart(units ~ store_nbr + item_nbr + IsMonToThu + month + day + day_7 + day_365 + date_numeric + preciptotal + severeSnow + mildSnow + Days3BeforeSevereSnow + Days3AfterSevereSnow + LessSnowThanTomorrow, data = Wal_Weather_1_Train)

prp(CART_2)
# Only item_nbr and store_nbr are used

#############################
# Round 2
# 1/3 - Remove all zeros 
Wal_Weather_2_Train <- Wal_Weather_1_Train %>% filter(units>0)
Wal_Weather_2_Validate <- Wal_Weather_1_Validate %>% filter(units>0)

# 2/3 - Linear
lm_2_1 <- lm(units ~ store_nbr + item_nbr + IsMonToThu +  HolidayShopping + month + day + day_7 + day_365 + date_numeric + snowfall + preciptotal + severeSnow + mildSnow + severeRain + severeWeather + Days3BeforeSevereSnow + Days3AfterSevereSnow + LessSnowThanTomorrow + LessSnowThanYesterday + ContinousDayOfSevereSnow_AfterToday + ContinousDayOfSevereSnow_BeforeToday + Days3BeforeSevereRain + Days3AfterSevereRain + LessRainThanTomorrow + LessRainThanYesterday + ContinousDayOfSevereRain_AfterToday + ContinousDayOfSevereRain_BeforeToday, data = Wal_Weather_2_Train)

summary(lm_2_1)
# Has improved 10 times than the same model with records including 0s
# Residual standard error: 44.91 on 88698 degrees of freedom
# Multiple R-squared:  0.08083,	Adjusted R-squared:  0.08055 
# F-statistic: 288.9 on 27 and 88698 DF,  p-value: < 2.2e-16

lm_2_2 <- lm(units ~ store_nbr + item_nbr + IsMonToThu +  HolidayShopping + month + day + day_7 + day_365 + date_numeric + preciptotal + severeSnow + mildSnow + severeRain + severeWeather + Days3BeforeSevereSnow + Days3AfterSevereSnow + Days3BeforeSevereRain + Days3AfterSevereRain + LessRainThanTomorrow, data = Wal_Weather_2_Train)

summary(lm_2_2)
# Residual standard error: 44.91 on 88706 degrees of freedom
# Multiple R-squared:  0.08072,	Adjusted R-squared:  0.08053 
# F-statistic:   410 on 19 and 88706 DF,  p-value: < 2.2e-16

lm_2_3 <- lm(units ~ store_nbr + item_nbr + IsMonToThu +  HolidayShopping + month + day + day_7 + day_365 + date_numeric + preciptotal + severeSnow + mildSnow + Days3BeforeSevereSnow + Days3AfterSevereSnow + Days3BeforeSevereRain + Days3AfterSevereRain, data = Wal_Weather_2_Train)

summary(lm_2_3)
# Residual standard error: 44.91 on 88709 degrees of freedom
# Multiple R-squared:  0.08066,	Adjusted R-squared:  0.0805 
# F-statistic: 486.5 on 16 and 88709 DF,  p-value: < 2.2e-16

lm_2_4 <- lm(units ~ store_nbr + item_nbr + day_7 + day_365, data = Wal_Weather_2_Train)

summary(lm_2_4)
# Residual standard error: 45.38 on 88721 degrees of freedom
# Multiple R-squared:  0.0612,	Adjusted R-squared:  0.06116 
# F-statistic:  1446 on 4 and 88721 DF,  p-value: < 2.2e-16


# 3/3 - CART
CART_2_4 <- rpart(units ~ store_nbr + item_nbr + day_7 + day_365, data = Wal_Weather_2_Train)

prp(CART_2_4)
# Only item # and store #