install.packages("tidyverse")
library(tidyverse)

### Case 1: Sales Forecasting using Linear Regression

## 1. Data Preparation
##### Read in data and check variables
newyork_taxi <- read.csv("C:/Users/prate/OneDrive/Desktop/CU Boulder/MSBA-SEM-2/Advanced Data Analytics/Group Project/data.csv")
str(newyork_taxi)
##### Change the format of date variable
date_split_newyork_taxi <- separate(newyork_taxi,"tpep_pickup_datetime", into = c("pick_up_date", "pick_up_time"), sep = " ")#seperate pick up date and pickup time
date_split_newyork_taxi$pick_up_date <- as.Date(date_split_newyork_taxi$pick_up_date, format="%m/%d/%Y") #converted string to date format 

date_split_newyork_taxi$pick_up_time <- strptime(date_split_newyork_taxi$pick_up_time, format="%H%M%S")#converted string to time format 
str(date_split_newyork_taxi)

#
model1 <- lm(tip_amount  ~ fare_amount + PULocationID + payment_type + extra  + RatecodeID, data=newyork_taxi)
summary(model1)

ggplot(data= model1) +
  geom_point(aes(x = tip_amount, y = fare_amount))

ggplot(data= model1) +
  geom_histogram(aes(x = tip_amount, y = fare_amount))
