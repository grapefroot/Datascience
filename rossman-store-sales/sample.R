library(data.table)
library(zoo)

library(forecast)
library(ggplot2)
library(gridExtra)

test = fread("./Datascience/rossman-store-sales/test.csv")
train = fread("./Datascience/rossman-store-sales/train.csv")
store = fread("./Datascience/rossman-store-sales/store.csv")

## Take a look on the data
str(train)
str(test)
str(store)

##transform data
train[, Date := as.Date(Date)]
test[, Date := as.Date(Date)]

#order by date
train = train[order(Date)]
test = test[order(Date)]

test[is.na(test)] = 1

train[, lapply(.SD, function(x)
  length(unique(x)))]
test[, lapply(.SD, function(x)
  length(unique(x)))]

#All test stores are in the train
sum(unique(test$Store) %in% unique(train$Store))

#259 from train are not into the train
sum(!(unique(train$Store) %in% unique(test$Store)))

#percentage of open stores in train and test
table(train$Open) / nrow(train)
table(test$Open) / nrow(test)

#percentage of promotions in train and test
table(train$Promo) / nrow(train)
table(test$Promo) / nrow(test)

#percentage of school holidays in train and test.
table(train$SchoolHoliday) / nrow(train)
table(test$SchoolHoliday) / nrow(test)
#major difference observed

#percentage of state holidays in train and test.
table(train$StateHoliday) / nrow(train)
table(test$StateHoliday) / nrow(test)

plot(train$Date, type = "l")
plot(test$Date, type = "l")

all(table(test$Date) == 856)

hist(train$Sales, 100)

maxhist = hist(aggregate(train[Sales != 0]$Sales,
                         by = list(train[Sales != 0]$Store), max)$x, 100,
               main = "Max sales per store when stores were not closed")

meanhist = hist(aggregate(train[Sales != 0]$Sales,
                          by = list(train[Sales != 0]$Store), mean)$x, 100,
                main = "Mean sales per store when stores were not closed")

minhist = hist(aggregate(train[Sales != 0]$Sales,
                         by = list(train[Sales != 0]$Store), min)$x, 100,
               main = "Min sales per store when stores were not closed")

medianhist = hist(aggregate(train[Sales != 0]$Sales,
                            by = list(train[Sales != 0]$Store), median)$x, 100,
                  main = "Median sales per store when stores were not closed")

hist(train$Customers, 100)

maxhist = hist(aggregate(train[Sales != 0]$Customers,
                         by = list(train[Sales != 0]$Store), max)$x, 100,
               main = "Max customers per store when stores were not closed")

meanhist = hist(aggregate(train[Sales != 0]$Customers,
                          by = list(train[Sales != 0]$Store), mean)$x, 100,
                main = "Mean customers per store when stores were not closed")

minhist = hist(aggregate(train[Sales != 0]$Customers,
                         by = list(train[Sales != 0]$Store), min)$x, 100,
               main = "Min customers per store when stores were not closed")

medianhist = hist(aggregate(train[Sales != 0]$Customers,
                            by = list(train[Sales != 0]$Store), median)$x, 100,
                  main = "Median customers per store when stores were not closed")

ggplot(data = train[Sales != 0], aes(x = factor(SchoolHoliday), y = Sales)) +
  geom_jitter(alpha = 0.05) +
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

ggplot(data = train[train$Sales != 0 & train$Customers != 0],
       aes(x = log(Customers), y = log(Sales))) +
  geom_point(alpha = 0.05) +
  geom_smooth()

#how promos affect sales
ggplot(data = train[train$Sales != 0 & train$Customers != 0],
       aes(x = factor(Promo),y = Sales)) +
  geom_jitter(alpha = 0.05) +
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

#how promos affect customers
ggplot(data = train[train$Sales != 0 & train$Customers != 0],
       aes(x = factor(Promo),y = Customers)) +
  geom_jitter(alpha = 0.05) +
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

with(train[train$Sales != 0 &
             train$Promo == 0], mean(Sales / Customers))

with(train[train$Sales != 0 &
             train$Promo == 1], mean(Sales / Customers))
#so after promos people tend to spend more but amount of customers are not greatly affected

table(
  ifelse(train$Sales != 0, "Sales > 0", "Sales = 0"),
  ifelse(train$Promo, "Promo", "No Promo")
)

table(
  ifelse(train$Open == 1, "Opened", "Closed"),
  ifelse(train$Sales > 0, "Sales > 0", "Sales = 0")
)

train[train$Open == 1 & train$Sales == 0]

zerosPerStore = sort(tapply(train$Sales, list(train$Store), function(x)
  sum(x == 0)))
hist(zerosPerStore, 100)

tail(zerosPerStore, 10)

plot(train[Store == 311, Sales], ylab = "Sales", xlab = "Days", main = "Store 972")

ggplot(train[Store == 85], aes(x = Date, y = Sales, color = factor(DayOfWeek == 7), shape = factor(DayOfWeek == 7)))+
  geom_point(size=3)+ggtitle("Sale of store 85(True if sunday)")


ggplot(train[Store == 262], aes(x = Date, y = Sales, color = factor(DayOfWeek == 7), shape = factor(DayOfWeek == 7)))+
  geom_point(size=3)+ggtitle("Sale of store 85(True if sunday)")

ggplot(train[Store != 0], aes(x = factor(DayOfWeek), y = Sales))+
  geom_jitter(alpha = 0.1)+
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

table(store$StoreType)
nrow(store)

table(data.frame(Assortment = store$Assortment, StoreType = store$StoreType))

hist(store$CompetitionDistance, 100)