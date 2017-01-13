print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("Please run Walmart_Functions.R before this script")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

print("You are running this Walmart_model.R at")
Sys.time()
#Data Analysis - Total sales of items at each store
# This is to identify items with large qty, stores with larger qty, the items being sold 
# in more stores and the stores selling more items

# Get total units sold by each item at each store. It may get max of 111*45 observations
Walmart.train.GroupByStore <- Walmart.train  %>% group_by(item_nbr, store_nbr)  %>% summarise(unit_sold = sum(units))

ggplot(subset(Walmart.train.GroupByStore, (unit_sold > 0)), aes(x = factor(store_nbr), y = factor(item_nbr), size = unit_sold, col = unit_sold, label = item_nbr)) + geom_point() + geom_text(nudge_y = 3) + ggtitle("Total units sold at each store for the items 1 to 111")
# Item # 25 is store 16 has very high value

# For each item, find count of store, it sells in
countOfStoreSellingTheItem <- subset(Walmart.train.GroupByStore, unit_sold > 0)  %>% group_by(item_nbr)  %>% summarise(sellingStore = n())  %>% arrange(sellingStore)

posn.b <- position_jitter(0.1, 0)
ggplot(countOfStoreSellingTheItem, aes(x = factor(sellingStore), y = factor(item_nbr), label = item_nbr)) + geom_jitter(size = 2) + geom_text(nudge_x = 0.2) + ggtitle("For each item, find count of store, it sells in")

# Majority of the items are being sold only in one store
# 79 items in only 1 store
# 13/92 in 2
# 5/97 in 3
# 2/99 in 4
# 4/103 in 5
# 2/105 in 6
# 1/106 in 7
# 1/107 in 8
# 1/108 in 16
# 1/109 in 17
# 1/110 in 18
# 1/111 in 29

# For each store, find number of unique item

countOfItemsSoldInStore <- subset(Walmart.train.GroupByStore, unit_sold > 0)  %>% group_by(store_nbr)  %>% summarise(uniqItemsSold = n())  %>% arrange(uniqItemsSold)

ggplot(countOfItemsSoldInStore, aes(x = factor(uniqItemsSold), y = factor(store_nbr), label = store_nbr)) + geom_point(size = 2) + geom_text(nudge_x = 0.2) + ggtitle("For each store, find number of unique item") 

# Review of items, which are sold in more than 10 stores
countOfStoreSellingTheItem  %>% arrange(desc(sellingStore))
item93 <- subset(Walmart.train, item_nbr == 93 & units > 0)
item5 <- subset(Walmart.train, item_nbr == 5 & units > 0)
item9 <- subset(Walmart.train, item_nbr == 9 & units > 0)
item45 <- subset(Walmart.train, item_nbr == 45 & units > 0)

ggplot(item93, aes(y = units, x = date, color = factor(store_nbr), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of Item # 93 in various store on time series")

ggplot(item5, aes(y = units, x = date, color = factor(store_nbr), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5)+ ggtitle("Sales of Item # 5 in various store on time series")

ggplot(item45, aes(y = units, x = date, color = factor(store_nbr), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5)+ ggtitle("Sales of Item # 45 in various store on time series")

# Review if Weekday or Weekend has any pattern in sales
ggplot(item93, aes(y = units, x = date, color = factor(IsMonToThu), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of Item #93 on Mon to Thu and Fri to Sun")

ggplot(item5, aes(y = units, x = date, color = factor(IsMonToThu), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of Item #5 on Mon to Thu and Fri to Sun")

ggplot(item9, aes(y = units, x = date, color = factor(IsMonToThu), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of Item #9 on Mon to Thu and Fri to Sun")

ggplot(item45, aes(y = units, x = date, color = factor(IsMonToThu), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + geom_smooth(size = 4) + ggtitle("Sales of Item #45 on Mon to Thu and Fri to Sun")

# Review sales pattern for holiday and non holiday

ggplot(item93, aes(y = units, x = date, color = factor(HolidayShopping), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of Item #93 within 10 days of Holidays and other regular days")

ggplot(item5, aes(y = units, x = date, color = factor(HolidayShopping), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of Item #5 within 10 days of Holidays and other regular days")

ggplot(item9, aes(y = units, x = date, color = factor(HolidayShopping), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of Item #9 within 10 days of Holidays and other regular days")

ggplot(item45, aes(y = units, x = date, color = factor(HolidayShopping), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of Item #45 within 10 days of Holidays and other regular days")

# Review stores
# Identify stores with highest number of items or items with outlier observations
countOfItemsSoldInStore  %>% arrange(desc(uniqItemsSold))
store1 <- subset(Walmart.train, store_nbr == 1 & units > 0)
store1_full <- subset(Walmart.train, store_nbr == 1)

store22 <- subset(Walmart.train, store_nbr == 22 & units > 0)
store37 <- subset(Walmart.train, store_nbr == 37 & units > 0)
store31 <- subset(Walmart.train, store_nbr == 31 & units > 0)

# Review sales pattern of each store for all items

ggplot(store1, aes(y = units, x = date, color = factor(item_nbr), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of each item in Store # 1")

ggplot(store1_full, aes(y = units, x = date, color = factor(item_nbr))) + geom_line() + ggtitle("Sales including 0 of each item in Store # 1")

ggplot(store22, aes(y = units, x = date, color = factor(item_nbr), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of each item in Store # 22")
# Qty for item 93 appears as outlier
# Review pattern of item 93 alone
ggplot(subset(store22, item_nbr == 93), aes(y = units, x = date, color = factor(item_nbr), label = units)) + geom_smooth(col = "blue", size = 2) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of item # 93 in Store # 22")

ggplot(store37, aes(y = units, x = date, color = factor(item_nbr), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of each item in Store # 37")
# Qty for item 5 appears as outlier
# Review pattern of item 5 alone
ggplot(subset(store37, item_nbr == 5), aes(y = units, x = date, color = factor(item_nbr), label = units)) + geom_smooth(col = "blue", size = 2) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of item #5 in Store # 37")

ggplot(store31, aes(y = units, x = date, color = factor(item_nbr), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of each item in Store # 31")
# Qty for item 45 appears as outlier
# Review pattern of item 45 alone
ggplot(subset(store31, item_nbr == 45), aes(y = units, x = date, color = factor(item_nbr), label = units)) + geom_smooth(col = "blue", size = 2) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Sales of item #45 in Store # 31")

# Review store sales for weekday Vs weekend and holiday Vs non holiday shopping

ggplot(store1, aes(y = units, x = date, color = factor(IsMonToThu), label = item_nbr)) + geom_smooth(size = 2) + geom_line() + geom_text(nudge_x = 5)  + ggtitle("Sales pattern in store for MOn to Thu Vs Fri to Sun at Store # 1")

ggplot(store22, aes(y = units, x = date, color = factor(IsMonToThu), label = item_nbr)) + geom_smooth(size = 2) + geom_line() + geom_text(nudge_x = 5)  + ggtitle("Sales pattern in store for MOn to Thu Vs Fri to Sun at Store # 22")

ggplot(store37, aes(y = units, x = date, color = factor(IsMonToThu), label = item_nbr)) + geom_smooth(size = 2) + geom_line() + geom_text(nudge_x = 5)  + ggtitle("Sales pattern in store for MOn to Thu Vs Fri to Sun at Store # 37")

ggplot(store31, aes(y = units, x = date, color = factor(IsMonToThu), label = item_nbr)) + geom_smooth(size = 2) + geom_line() + geom_text(nudge_x = 5)  + ggtitle("Sales pattern in store for MOn to Thu Vs Fri to Sun at Store # 31")

ggplot(store1, aes(y = units, x = date, color = factor(HolidayShopping), label = item_nbr)) + geom_text(nudge_x = 5)  + geom_smooth(size = 2) + geom_line() + ggtitle("Sales pattern for 10 days of holidays and regaular days in store #1")

ggplot(store22, aes(y = units, x = date, color = factor(HolidayShopping), label = item_nbr)) + geom_text(nudge_x = 5)  + geom_smooth(size = 2) + geom_line() + ggtitle("Sales pattern for 10 days of holidays and regaular days in store #22")

ggplot(store37, aes(y = units, x = date, color = factor(HolidayShopping), label = item_nbr)) + geom_text(nudge_x = 5)  + geom_smooth(size = 2) + geom_line() + ggtitle("Sales pattern for 10 days of holidays and regaular days in store #37")

ggplot(store31, aes(y = units, x = date, color = factor(HolidayShopping), label = item_nbr)) + geom_text(nudge_x = 5)  + geom_smooth(size = 2) + geom_line() + ggtitle("Sales pattern for 10 days of holidays and regaular days in store #31")

# Review items with highest quantity

highestSellingItems <- subset(Walmart.train, units > 0) %>% group_by(item_nbr) %>% summarise(total_units = sum(units)) %>% arrange(desc(total_units))

head(highestSellingItems)

ggplot(highestSellingItems, aes(y = total_units, x = item_nbr, size = total_units, label = item_nbr)) + geom_point() + geom_text(nudge_x = 5)

ggplot(item45, aes(y = units, x = factor(store_nbr))) + geom_boxplot() + ggtitle("item45")

ggplot(item45, aes(y = units, x = factor(store_nbr), size = units, label = units)) + geom_jitter(alpha = 0.2) + geom_text(nudge_y = 5) + stat_summary(fun.y = "median", colour = "red", size = 2, geom = "point") + ggtitle("Unit sales in each store and red dot is median for item # 45")

ggplot(item9, aes(y = units, x = factor(store_nbr))) + geom_boxplot() + ggtitle("item9")

ggplot(item5, aes(y = units, x = factor(store_nbr))) + geom_boxplot() + ggtitle("item5")

item44 <- subset(Walmart.train, item_nbr == 44 & units > 0)
ggplot(item44, aes(y = units, x = factor(store_nbr))) + geom_boxplot() + ggtitle("item44")

# Review all items

ggplot(subset(Walmart.train, item_nbr == 5), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item5")

ggplot(subset(Walmart.train, item_nbr == 6), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item6")

ggplot(subset(Walmart.train, item_nbr == 8), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item8")

ggplot(subset(Walmart.train, item_nbr == 9), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item9")

ggplot(subset(Walmart.train, item_nbr == 15), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item15")

ggplot(subset(Walmart.train, item_nbr == 16), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item16")

ggplot(subset(Walmart.train, item_nbr == 23), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item23")

ggplot(subset(Walmart.train, item_nbr == 25), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item25")

ggplot(subset(Walmart.train, item_nbr == 28), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item28")

ggplot(subset(Walmart.train, item_nbr == 44), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item44")

ggplot(subset(Walmart.train, item_nbr == 45), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item45")

ggplot(subset(Walmart.train, item_nbr == 93), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item93")

# Summarize item sales by month to understand the pattern of sales -->
Walmart.train_GroupByMonth <- Walmart.train  %>% group_by(item_nbr, store_nbr, year, month)  %>% summarise(total_units_month = sum(units))

# Review each item by month and store -->
Walmart.train_GroupByMonth_Item93 <- Walmart.train_GroupByMonth  %>% filter(item_nbr == 93) 

ggplot(subset(Walmart.train_GroupByMonth_Item93, (store_nbr >= 1 & store_nbr <5 | store_nbr == 22)), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item93") + facet_grid(.~store_nbr)

ggplot(subset(Walmart.train_GroupByMonth_Item93, (store_nbr >= 5 & store_nbr <10 | store_nbr == 22)), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item93") + facet_grid(.~store_nbr)

ggplot(subset(Walmart.train_GroupByMonth_Item93, (store_nbr >= 10 & store_nbr <15 | store_nbr == 22)), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item93") + facet_grid(.~store_nbr)

ggplot(subset(Walmart.train_GroupByMonth_Item93, (store_nbr >= 15 & store_nbr <20 | store_nbr == 22)), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item93") + facet_grid(.~store_nbr)

ggplot(subset(Walmart.train_GroupByMonth_Item93, (store_nbr >= 20 & store_nbr <25 | store_nbr == 22)), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item93") + facet_grid(.~store_nbr)

ggplot(subset(Walmart.train_GroupByMonth_Item93, (store_nbr >= 25 & store_nbr <30 | store_nbr == 22)), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item93") + facet_grid(.~store_nbr)

ggplot(subset(Walmart.train_GroupByMonth_Item93, (store_nbr >= 30 & store_nbr <35 | store_nbr == 22)), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item93") + facet_grid(.~store_nbr)

ggplot(subset(Walmart.train_GroupByMonth_Item93, (store_nbr >= 35 & store_nbr <40 | store_nbr == 22)), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item93") + facet_grid(.~store_nbr)

ggplot(subset(Walmart.train_GroupByMonth_Item93, (store_nbr >= 40 & store_nbr <=45 | store_nbr == 22)), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item93") + facet_grid(.~store_nbr)

Walmart.train_GroupByMonth_Item45 <- Walmart.train_GroupByMonth  %>% filter(item_nbr == 45) 
ggplot(subset(Walmart.train_GroupByMonth_Item45, (store_nbr %in% c(3,6,9,15,23))), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item45") + facet_grid(.~store_nbr)

ggplot(subset(Walmart.train_GroupByMonth_Item45, (store_nbr %in% c(15,25,26,28,29))), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item45") + facet_grid(.~store_nbr)

ggplot(subset(Walmart.train_GroupByMonth_Item45, (store_nbr %in% c(15,31,32,34,37))), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item45") + facet_grid(.~store_nbr)

ggplot(subset(Walmart.train_GroupByMonth_Item45, (store_nbr %in% c(15,38,40,42))), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item45") + facet_grid(.~store_nbr)

Walmart.train_GroupByMonth_Item6 <- Walmart.train_GroupByMonth  %>% filter(item_nbr == 6)

ggplot(subset(Walmart.train_GroupByMonth_Item6 , (store_nbr %in% c(24))), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item6") + facet_grid(.~store_nbr)

Walmart.train_GroupByMonth_Item8 <- Walmart.train_GroupByMonth  %>% filter(item_nbr == 8)

ggplot(subset(Walmart.train_GroupByMonth_Item8 , (store_nbr %in% c(29))), aes(y = total_units_month, x = month, color = factor(year), label = total_units_month)) + geom_line() + geom_text() + ggtitle("MonthOverMonth_Item8") + facet_grid(.~store_nbr)



#Analyse weather data -->

ggplot(subset(Weather, snowfall > 0), aes(y = snowfall, x = factor(station_nbr), label = snowfall)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("snowfall>0")
# No outliers observed

ggplot(subset(Weather, snowfall > 2), aes(y = snowfall, x = factor(station_nbr), label = snowfall)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("snowfall > 2")

posn.a <- position_jitter(0.1, 0)

ggplot(subset(Weather), aes(y = snowfall, x = factor(month), col = factor(station_nbr), size = snowfall, label = station_nbr)) + geom_jitter(position = posn.a, alpha = 0.5) + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_hline(yintercept=2, linetype="dashed", color = "red", size=2) +facet_grid(.~year) + ggtitle("Snowfall by month")
#Snowfall has been from Nov to Mar

#Review station # 4, it had snow once in the entire duration. Forcast could have caused panic buying
ggplot(subset(Weather, (month == 2 & year == 2014 & station_nbr == 2)), aes(y = snowfall, x = day, size = snowfall, label = date)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_hline(yintercept=2, linetype="dashed", color = "red", size=2) + ggtitle("Snowfall by day for store # 4")
#Station_nbr 4 has once snow in the entire duration  on 2014-01-23 and 24
View(Key %>% arrange(station_nbr))
#Station_nbr 4 serves store # 8
View(countOfItemsSoldInStore  %>% arrange((store_nbr)))
# Store # 8 sales 6 items
# Review sales of store # 8 in 2014-01
ggplot(subset(Walmart.train, (month == 2 & year == 2014 & store_nbr == 19 & units >0)), aes(y = units, x = day, color = factor(item_nbr), label = date)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_vline(xintercept = c(5,13,14,18,19), linetype="dashed", color = "red", size=2) 

# Didn't see advance buying, saw a dip on the day of snow, observed a lift for following few days

# Reviewing store = 2, it had snow from Jan to Mar/Apr and and Nov/Dec
# 2012-01
ggplot(subset(Weather, (month == 1 & year == 2012 & station_nbr == 2)), aes(y = snowfall, x = day, label = date)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_hline(yintercept=2, linetype="dashed", color = "red", size=2) + ggtitle("Snowfall by day for Station # 2 for 2012-01")
#Station_nbr 2 has snow
#Mild on 2012-01-06
#Severe on 2012-01-12
#Mild 2 inch - 2012-01-16
#Mild 2012-01-19, 20
#Severe on 2012-01-21

View(Key %>% arrange(station_nbr))
#Station_nbr 2 serves store # 16
View(countOfItemsSoldInStore  %>% arrange((store_nbr)))
# Store # 16 sales 8 items
# Review sales of store # 16 in 2012-01
ggplot(subset(Walmart.train, (month == 1 & year == 2012 & store_nbr == 16 & units >0)), aes(y = units, x = day, color = factor(item_nbr), label = units)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_vline(xintercept =12, linetype="dashed", color = "red", size=2) + geom_vline(xintercept =21, linetype="dashed", color = "red", size=2) + geom_vline(xintercept =6, linetype="dashed", color = "blue", size=2) + geom_vline(xintercept =16, linetype="dashed", color = "blue", size=2) + geom_vline(xintercept =19, linetype="dashed", color = "blue", size=2) + geom_vline(xintercept =20, linetype="dashed", color = "blue", size=2) + ggtitle("Sales during snowfall by day for store # 16/Station # 2 for 2012-01")
# There are advanced lift, drop and post event lift happening. Continuous bad weather has more prominent impact
# We should have features
# Buy3DaysBeforeSevereSnow
# Buy2DaysBeforeSevereSnow
# Buy1DayBeforeSevereSnow
# Snow on the day - If less, people will go out to buy
# Buy1DayAfterSevereSnow
# Buy2DayAfterSevereSnow
# Buy3DayAfterSevereSnow
# LessSnowThanTommorow - I will go out for shopping today if it is less. Higher the number, more sales
# SnowForNext2Days
# AvgSnowForNext2Days
# SnowForNext3Days
# AvgSnowForNext3Days
# SnowForPast2Days
# AvgSnowForPast2Days
# SnowForPast3Days
# AvgSnowForPastt3Days
# LessSnowThanYesterday - If it warmer today since yesterday and tomorrow, I may go and buy today

# Review sales of store # 16 in 2012-02
# 2012-02
ggplot(subset(Weather, (month == 2 & year == 2012 & station_nbr == 2)), aes(y = snowfall, x = day, label = date)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_hline(yintercept=2, linetype="dashed", color = "red", size=2) + ggtitle("Snowfall by day for store # 16/Station # 2 for 2012-02")
# Mild - 11 and 24
# Severe - 29
ggplot(subset(Walmart.train, (month == 2 & year == 2012 & store_nbr == 16 & units >0)), aes(y = units, x = day, color = factor(item_nbr), label = units)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_vline(xintercept =29, linetype="dashed", color = "red", size=2) + geom_vline(xintercept =11, linetype="dashed", color = "blue", size=2) + geom_vline(xintercept =24, linetype="dashed", color = "blue", size=2) + ggtitle("Sales during snowfall by day for store # 16/Station # 2 for 2012-02")
# No major pattern for mild or severe

# Review sales of store # 16 in 2012-03
# 2012-03
ggplot(subset(Weather, (month == 3 & year == 2012 & station_nbr == 2)), aes(y = snowfall, x = day, label = date)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_hline(yintercept=2, linetype="dashed", color = "red", size=2) + ggtitle("Snowfall by day for store # 16/Station # 2 for 2012-03")
# Severe = 01
# Mild - 2,3,31

ggplot(subset(Walmart.train, (month == 3 & year == 2012 & store_nbr == 16 & units >0)), aes(y = units, x = day, color = factor(item_nbr), label = units)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_vline(xintercept =1, linetype="dashed", color = "red", size=2) + geom_vline(xintercept =2, linetype="dashed", color = "blue", size=2) + geom_vline(xintercept =3, linetype="dashed", color = "blue", size=2) + geom_vline(xintercept =31, linetype="dashed", color = "blue", size=2) + ggtitle("Sales during snowfall by day for store # 16/Station # 2 for 2012-03")
# forward and backward buying, dip on severe snow

# Review sales of store # 16 in 2012-04
# 2012-04
ggplot(subset(Weather, (month == 4 & year == 2012 & station_nbr == 2)), aes(y = preciptotal, x = day, label = date)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_hline(yintercept=1, linetype="dashed", color = "red", size=2) + ggtitle("Rain by day for store # 16/Station # 2 for 2012-04")
# Mild rain - 1,4,11, 12, 27
# Severe rain - 22,23

ggplot(subset(Walmart.train, (month == 4 & year == 2012 & store_nbr == 16 & units >0)), aes(y = units, x = day, color = factor(item_nbr), label = units)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_vline(xintercept =22, linetype="dashed", color = "red", size=2) + geom_vline(xintercept =23, linetype="dashed", color = "red", size=2) + geom_vline(xintercept =1, linetype="dashed", color = "blue", size=2) + geom_vline(xintercept =4, linetype="dashed", color = "blue", size=2) + geom_vline(xintercept =11, linetype="dashed", color = "blue", size=2) + geom_vline(xintercept =12, linetype="dashed", color = "blue", size=2) + geom_vline(xintercept =27, linetype="dashed", color = "blue", size=2) + ggtitle("Sales during rain by day for store # 16/Station # 2 for 2012-04")
# Mild rain does not stop people from buying
# Post even sales gets delayed, if it is weekday, it picked up in weekend

# We should have features
# Buy3DaysBeforeSevereRain
# Buy2DaysBeforeSevereRain
# Buy1DayBeforeSevereRain
# Rain on the day - If less, people will go out to buy
# Buy1DayAfterSevereRain
# Buy2DayAfterSevereRain
# Buy3DayAfterSevereRain
# LessRainThanTommorow - I will go out for shopping today if it is less. Higher the number, more sales
# SevereRainForNext2Days
# AvgRainForNext2Days
# SevereRainForNext3Days
# AvgRainForNext3Days
# SevereRainForPast2Days
# AvgRainForPast2Days
# SevereRainForPast3Days
# AvgRainForPastt3Days
# LessRainThanYesterday - If it less rain today since yesterday and tomorrow, I may go and buy today

# As test data is from training data, as sequence # of date may create a pattern
# Also create another count of 365 days to see pattern in 60 days, 180 days and comparion year by year

#Also cluster weather data by putting all weather paraments and count of 365 days, month


# Review sales of store # 16 in 2012-06
# 2012-06
ggplot(subset(Weather, (month == 6 & year == 2012 & station_nbr == 2)), aes(y = preciptotal, x = day, label = date)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_hline(yintercept=1, linetype="dashed", color = "red", size=2) + ggtitle("Rain by day for store # 16/Station # 2 for 2012-06")
# Mild rain - 3,4,5,6,7,8,9,12,13,22,23,25,26,29 # 
# Severe rain - 2

ggplot(subset(Walmart.train, (month == 6 & year == 2012 & store_nbr == 16 & units >0)), aes(y = units, x = day, color = factor(item_nbr), label = units)) + geom_line() + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_vline(xintercept =c(3,4,5,6,7,8,9,12,13,22,23,25,26,29), linetype="dashed", color = "blue", size=2) + geom_vline(xintercept =c(2), linetype="dashed", color = "red", size=2) + ggtitle("Sales during rain by day for store # 16/Station # 2 for 2012-06")
# Continous rain does trigger lower sales
# month 08 - Post event buying is stronger 
# month 09 - 12 - Nothing significant

ggplot(subset(Weather, preciptotal > 0), aes(y = preciptotal, x = factor(station_nbr), label = preciptotal)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("Rain>0")

ggplot(subset(Weather, preciptotal > 1), aes(y = preciptotal, x = factor(station_nbr), label = preciptotal)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("Rain > 1")

ggplot(subset(Weather), aes(y = preciptotal, x = factor(month), col = factor(station_nbr), size = preciptotal, label = station_nbr)) + geom_jitter(position = posn.a, alpha = 0.5) + geom_text(nudge_x = 0.4, nudge_y = 0.4) + geom_hline(yintercept=1, linetype="dashed", color = "blue", size=2) +facet_grid(.~year) + ggtitle("Rain by month")
#Rain has been there for all 12 months. There could be some pattern by each station #

ggplot(subset(Weather, (year == 2012 & station_nbr >= 1 & station_nbr <6)), aes(y = preciptotal, x = factor(month), size = preciptotal)) + geom_jitter(position = posn.a, alpha = 0.5)+ geom_hline(yintercept=1, linetype="dashed", color = "blue", size=2) +facet_grid(.~station_nbr) + ggtitle("Rain by month in 2012 for station number 1 to 5")

ggplot(subset(Weather, (year == 2012 & station_nbr >= 6 & station_nbr <11)), aes(y = preciptotal, x = factor(month), size = preciptotal)) + geom_jitter(position = posn.a, alpha = 0.5)+ geom_hline(yintercept=1, linetype="dashed", color = "blue", size=2) +facet_grid(.~station_nbr) + ggtitle("Rain by month in 2012 for station number 6 to 10")

ggplot(subset(Weather, (year == 2012 & station_nbr >= 11 & station_nbr <16)), aes(y = preciptotal, x = factor(month), size = preciptotal)) + geom_jitter(position = posn.a, alpha = 0.5)+ geom_hline(yintercept=1, linetype="dashed", color = "blue", size=2) +facet_grid(.~station_nbr) + ggtitle("Rain by month in 2012 for station number 11 to 15")

ggplot(subset(Weather, (year == 2012 & station_nbr >= 16 & station_nbr <=20)), aes(y = preciptotal, x = factor(month), size = preciptotal)) + geom_jitter(position = posn.a, alpha = 0.5)+ geom_hline(yintercept=1, linetype="dashed", color = "blue", size=2) +facet_grid(.~station_nbr) + ggtitle("Rain by month in 2012 for station number 16 to 20")
# Do people behave differently, if severe weather is for longer duration like 3 days or over the weekend or my last 5 days were great, but next 5 days are going to be very cold

ggplot(Weather, aes(y = factor(station_nbr), x = date, color = factor(severeWeather), shape = factor(severeWeather))) + geom_point(size = 5) 

ggplot(Weather, aes(y = factor(station_nbr), x = date, color = factor(severeWeather))) + geom_point() + facet_grid(.~severeWeather)

View(Weather %>% filter(date >= ymd("2013-04-01"), severeWeather == 1))
View(Walmart.train_full %>% filter(date == ymd("2014-10-23") & station_nbr %in% c(1,2,15,16)))

############ Add new variables to weather here

#############################################

########### Junk test section ##############


###########################################





# We should have features
# Buy3DaysBeforeSevereRain
# Buy2DaysBeforeSevereRain
# Buy1DayBeforeSevereRain
# Rain on the day - If less, people will go out to buy
# Buy1DayAfterSevereRain
# Buy2DayAfterSevereRain
# Buy3DayAfterSevereRain
# LessRainThanTommorow - I will go out for shopping today if it is less. Higher the number, more sales
# SevereRainForNext2Days
# AvgRainForNext2Days
# SevereRainForNext3Days
# AvgRainForNext3Days
# SevereRainForPast2Days
# AvgRainForPast2Days
# SevereRainForPast3Days
# AvgRainForPastt3Days
# LessRainThanYesterday - If it less rain today since yesterday and tomorrow, I may go and buy today

# As test data is from training data, as sequence # of date may create a pattern
# Also create another count of 365 days to see pattern in 60 days, 180 days and comparion year by year

#Also cluster weather data by putting all weather paraments and count of 365 days, month



