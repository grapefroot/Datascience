library(data.table)
library(zoo)

library(forecast)
library(ggplot2)
library(gridExtra)

test = fread("./test.csv")
train = fread("./train.csv")
store = fread("./store.csv")

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

train[, lapply(.SD, function(x) length(unique(x)))]
test[, lapply(.SD, function(x) length(unique(x)))]

#All test stores are in the train
sum(unique(test$Store) %in% unique(train$Store))

#259 from train are not into the train
sum(!(unique(train$Store) %in% unique(test$Store)))

#percentage of open stores in train and test
table(train$Open)/nrow(train)
table(test$Open)/nrow(test)

#percentage of promotions in train and test
table(train$Promo)/nrow(train)
table(test$Promo)/nrow(test)

#percentage of school holidays in train and test.
table(train$SchoolHoliday)/nrow(train)
table(test$SchoolHoliday)/nrow(test)
#major difference observed

#percentage of state holidays in train and test.
table(train$StateHoliday)/nrow(train)
table(test$StateHoliday)/nrow(test)

plot(train$Date, type="l")
plot(test$Date, type="l")

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

