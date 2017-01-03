## ----echo=FALSE----------------------------------------------------------
library(lubridate)
library(tidyr)
library(dplyr)
library(timeDate)
library(ggplot2)

## ----echo=FALSE----------------------------------------------------------
Walmart.train <- read.csv("train.csv")

## ----echo=FALSE, results='hide'------------------------------------------
str(Walmart.train)

## ----echo=FALSE, results='hide'------------------------------------------
Walmart.train$date <- ymd(Walmart.train$date)

## ----echo=FALSE, results='hide'------------------------------------------
str(Walmart.train)

## ----echo=FALSE, results='hide'------------------------------------------
summary(Walmart.train$date)

## ----echo=FALSE----------------------------------------------------------
Walmart.train$IsMonToThu <- as.integer(isWeekday(Walmart.train$date, wday = 1:4))
Walmart.train$IsFriToSun <- as.integer(isWeekend(Walmart.train$date, wday = 1:4))

## isHolidayShoppingDay takes 12 hours to execute. Hence I saved the file and used it
## ----echo=FALSE, eval= FALSE---------------------------------------------
## Walmart.train$HolidayShopping <- isHolidayShoppingDay(Walmart.train$date)
## Walmart.train <- read.csv("Walmart_HolidayShopping.csv")
## Walmart.train$date <- ymd(Walmart.train$date)

## ----echo= FALSE---------------------------------------------------------
Walmart.train.GroupByStore <- Walmart.train  %>% group_by(item_nbr, store_nbr)  %>% summarise(unit_sold = sum(units))

ggplot(subset(Walmart.train.GroupByStore, item_nbr == 1), aes(x = store_nbr, y = unit_sold)) + geom_line() + ggtitle("Units sold of the item by each store")
ggplot(subset(Walmart.train.GroupByStore, item_nbr == 2), aes(x = store_nbr, y = unit_sold)) + geom_line() + ggtitle("Units sold of the item by each store")


## ----echo=FALSE----------------------------------------------------------
ggplot(subset(Walmart.train.GroupByStore, (item_nbr > 0 & item_nbr <= 10 & unit_sold > 0)), aes(x = store_nbr, y = factor(item_nbr), size = unit_sold)) + geom_point(shape = 1) + ggtitle("Units sold of the item by each store")

ggplot(subset(Walmart.train.GroupByStore, (item_nbr > 10 & item_nbr <= 20 & unit_sold > 0)), aes(x = store_nbr, y = factor(item_nbr), size = unit_sold)) + geom_point(shape = 1) + ggtitle("Units sold of the item by each store")

ggplot(subset(Walmart.train.GroupByStore, (item_nbr > 20 & item_nbr <= 30 & unit_sold > 0)), aes(x = store_nbr, y = factor(item_nbr), size = unit_sold)) + geom_point(shape = 1) + ggtitle("Units sold of the item by each store")

ggplot(subset(Walmart.train.GroupByStore, (item_nbr > 30 & item_nbr <= 40 & unit_sold > 0)), aes(x = store_nbr, y = factor(item_nbr), size = unit_sold)) + geom_point(shape = 1) + ggtitle("Units sold of the item by each store")

ggplot(subset(Walmart.train.GroupByStore, (item_nbr > 40 & item_nbr <= 50 & unit_sold > 0)), aes(x = store_nbr, y = factor(item_nbr), size = unit_sold)) + geom_point(shape = 1) + ggtitle("Units sold of the item by each store")

ggplot(subset(Walmart.train.GroupByStore, (item_nbr > 50 & item_nbr <= 60 & unit_sold > 0)), aes(x = store_nbr, y = factor(item_nbr), size = unit_sold)) + geom_point(shape = 1) + ggtitle("Units sold of the item by each store")

ggplot(subset(Walmart.train.GroupByStore, (item_nbr > 60 & item_nbr <= 70 & unit_sold > 0)), aes(x = store_nbr, y = factor(item_nbr), size = unit_sold)) + geom_point(shape = 1) + ggtitle("Units sold of the item by each store")

ggplot(subset(Walmart.train.GroupByStore, (item_nbr > 70 & item_nbr <= 80 & unit_sold > 0)), aes(x = store_nbr, y = factor(item_nbr), size = unit_sold)) + geom_point(shape = 1) + ggtitle("Units sold of the item by each store")

ggplot(subset(Walmart.train.GroupByStore, (item_nbr > 80 & item_nbr <= 90 & unit_sold > 0)), aes(x = store_nbr, y = factor(item_nbr), size = unit_sold)) + geom_point(shape = 1) + ggtitle("Units sold of the item by each store")

ggplot(subset(Walmart.train.GroupByStore, (item_nbr > 90 & item_nbr <= 100 & unit_sold > 0)), aes(x = store_nbr, y = factor(item_nbr), size = unit_sold)) + geom_point(shape = 1) + ggtitle("Units sold of the item by each store")

ggplot(subset(Walmart.train.GroupByStore, (item_nbr > 100 & item_nbr <= 111 & unit_sold > 0)), aes(x = store_nbr, y = factor(item_nbr), size = unit_sold)) + geom_point(shape = 1) + ggtitle("Units sold of the item by each store")

countOfStoreSellingTheItem <- subset(Walmart.train.GroupByStore, unit_sold > 0)  %>% group_by(item_nbr)  %>% summarise(sellingStore = n())  %>% arrange(sellingStore)

ggplot(countOfStoreSellingTheItem, aes(x = factor(sellingStore), y = factor(item_nbr))) + geom_jitter(shape = 1, size = 2)

countOfItemsSoldInStore <- subset(Walmart.train.GroupByStore, unit_sold > 0)  %>% group_by(store_nbr)  %>% summarise(uniqItemsSold = n())  %>% arrange(uniqItemsSold)

ggplot(countOfItemsSoldInStore, aes(x = factor(uniqItemsSold), y = factor(store_nbr))) + geom_point(shape = 1, size = 2)





## ----echo=FALSE----------------------------------------------------------
countOfStoreSellingTheItem  %>% arrange(desc(sellingStore))
item93 <- subset(Walmart.train, item_nbr == 93 & units > 0)
item5 <- subset(Walmart.train, item_nbr == 5 & units > 0)
item9 <- subset(Walmart.train, item_nbr == 9 & units > 0)
item45 <- subset(Walmart.train, item_nbr == 45 & units > 0)

ggplot(item93, aes(y = units, x = date, color = factor(store_nbr), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Item93")
ggplot(item5, aes(y = units, x = date, color = factor(store_nbr), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5)+ ggtitle("Item5")
ggplot(item9, aes(y = units, x = date, color = factor(store_nbr), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5)+ ggtitle("Item9")
ggplot(item45, aes(y = units, x = date, color = factor(store_nbr), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5)+ ggtitle("Item45")

## ----echo=FALSE----------------------------------------------------------
ggplot(item93, aes(y = units, x = date, color = factor(IsMonToThu), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Item93")
ggplot(item5, aes(y = units, x = date, color = factor(IsMonToThu), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Item5")
ggplot(item9, aes(y = units, x = date, color = factor(IsMonToThu), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Item9")
ggplot(item45, aes(y = units, x = date, color = factor(IsMonToThu), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Item45")


## ----echo=FALSE----------------------------------------------------------
ggplot(item93, aes(y = units, x = date, color = factor(HolidayShopping), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Item93")

ggplot(item5, aes(y = units, x = date, color = factor(HolidayShopping), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Item5")

ggplot(item9, aes(y = units, x = date, color = factor(HolidayShopping), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Item9")

ggplot(item45, aes(y = units, x = date, color = factor(HolidayShopping), label = store_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("Item45")



## ----echo=FALSE----------------------------------------------------------
countOfItemsSoldInStore  %>% arrange(desc(uniqItemsSold))
store1 <- subset(Walmart.train, store_nbr == 1 & units > 0)
store1_full <- subset(Walmart.train, store_nbr == 1)

store22 <- subset(Walmart.train, store_nbr == 22 & units > 0)
store37 <- subset(Walmart.train, store_nbr == 37 & units > 0)
store31 <- subset(Walmart.train, store_nbr == 31 & units > 0)

## ----echo=FALSE----------------------------------------------------------
ggplot(store1, aes(y = units, x = date, color = factor(item_nbr), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("store1")

ggplot(store1_full, aes(y = units, x = date, color = factor(item_nbr))) + geom_line() + ggtitle("store1_full")

ggplot(store22, aes(y = units, x = date, color = factor(item_nbr), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("store22")

ggplot(subset(store22, item_nbr == 93), aes(y = units, x = date, color = factor(item_nbr), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("store22")

ggplot(store37, aes(y = units, x = date, color = factor(item_nbr), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("store37")

ggplot(subset(store37, item_nbr == 5), aes(y = units, x = date, color = factor(item_nbr), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("store37")

ggplot(store31, aes(y = units, x = date, color = factor(item_nbr), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("store31")

ggplot(subset(store31, item_nbr == 45), aes(y = units, x = date, color = factor(item_nbr), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5) + ggtitle("store31")


## ----echo=FALSE----------------------------------------------------------
ggplot(store1, aes(y = units, x = date, color = factor(IsMonToThu), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5)  + ggtitle("store1")

ggplot(store22, aes(y = units, x = date, color = factor(IsMonToThu), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5)  + ggtitle("store22")
ggplot(store37, aes(y = units, x = date, color = factor(IsMonToThu), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5)  + ggtitle("store37")
ggplot(store31, aes(y = units, x = date, color = factor(IsMonToThu), label = item_nbr)) + geom_line() + geom_text(nudge_x = 5)  + ggtitle("store31")

ggplot(store1, aes(y = units, x = date, color = factor(HolidayShopping), label = item_nbr)) + geom_text(nudge_x = 5)  + geom_line() + ggtitle("store1")

ggplot(store22, aes(y = units, x = date, color = factor(HolidayShopping), label = item_nbr)) + geom_text(nudge_x = 5)  + geom_line() + ggtitle("store22")

ggplot(store37, aes(y = units, x = date, color = factor(HolidayShopping), label = item_nbr)) + geom_text(nudge_x = 5)  + geom_line() + ggtitle("store37")

ggplot(store31, aes(y = units, x = date, color = factor(HolidayShopping), label = item_nbr)) + geom_text(nudge_x = 5)  + geom_line() + ggtitle("store31")


## ----echo=FALSE----------------------------------------------------------
highestSellingItems <- subset(Walmart.train, units > 0) %>% group_by(item_nbr) %>% summarise(total_units = sum(units)) %>% arrange(desc(total_units))
head(highestSellingItems)
ggplot(highestSellingItems, aes(y = total_units, x = item_nbr, size = total_units, label = item_nbr)) + geom_point() + geom_text(nudge_x = 5)

ggplot(item45, aes(y = units, x = factor(store_nbr))) + geom_boxplot() + ggtitle("item45")

ggplot(item9, aes(y = units, x = factor(store_nbr))) + geom_boxplot() + ggtitle("item9")
ggplot(item5, aes(y = units, x = factor(store_nbr))) + geom_boxplot() + ggtitle("item5")

item44 <- subset(Walmart.train, item_nbr == 44 & units > 0)
ggplot(item44, aes(y = units, x = factor(store_nbr))) + geom_boxplot() + ggtitle("item44")





## ----echo=FALSE----------------------------------------------------------
ggplot(subset(Walmart.train, item_nbr == 5), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item5")
ggplot(subset(Walmart.train, item_nbr == 6), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item6")
ggplot(subset(Walmart.train, item_nbr == 8), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item8")
ggplot(subset(Walmart.train, item_nbr == 9), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item9")
ggplot(subset(Walmart.train, item_nbr == 15), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item15")
ggplot(subset(Walmart.train, item_nbr == 16), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item16")
ggplot(subset(Walmart.train, item_nbr == 23), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item23")
ggplot(subset(Walmart.train, item_nbr == 25), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item25")
ggplot(subset(Walmart.train, item_nbr == 28), aes(y = units, x = factor(store_nbr), label = units)) + geom_boxplot() + geom_text(nudge_x = .5) + ggtitle("item28")


