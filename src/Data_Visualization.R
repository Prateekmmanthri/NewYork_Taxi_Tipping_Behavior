library(tidyverse)

### Case 1: Sales Forecasting using Linear Regression

## 1. Data Preparation
##### Read in data and check variables
newyork_taxi <- read.csv("C:/Users/prate/OneDrive/Desktop/CU Boulder/MSBA - SEM 2/Advanced Data Analytics/Group Project/data.csv")
ncol(newyork_taxi)
nrow(newyork_taxi)
str(newyork_taxi)

library(readr)
library(tidyr)
library(dplyr)
library(caret)
library(ggplot2)
library(lubridate)

new_york_taxi_data <- read.csv("C:/Users/prate/OneDrive/Desktop/CU Boulder/MSBA-SEM-2/Advanced Data Analytics/Group Project/data.csv")


## Section 1: Data Cleaning ##
# Convert string columns to datetime objects
new_york_taxi_data$tpep_pickup_datetime <- ymd_hms(new_york_taxi_data$tpep_pickup_datetime)
new_york_taxi_data$tpep_dropoff_datetime <- ymd_hms(new_york_taxi_data$tpep_dropoff_datetime)

# Calculate the difference between datetime columns and round off minutes
new_york_taxi_data$duration <- as.numeric(round(difftime(new_york_taxi_data$tpep_dropoff_datetime, new_york_taxi_data$tpep_pickup_datetime, units = "mins"), 0))

# Adding trip day to the dataframe
new_york_taxi_data$trip_day <- format(new_york_taxi_data$tpep_pickup_datetime, "%A")

# Filter outliers using key features
filtered_taxi_data <- new_york_taxi_data |>
  filter(trip_distance > 0, fare_amount > 0, total_amount > 0, duration > 2 & duration < 120, tip_amount > 1 & tip_amount <= 30)

# Filtering only 12 days of data from Dec 31st, 2019 till Jan 11th, 2020
filtered_taxi_data <- filtered_taxi_data %>% filter(tpep_pickup_datetime < ymd_hms("2020-01-21 00:00:00") & tpep_pickup_datetime > ymd_hms("2019-12-31 00:00:00"))

# Dropping irrelevant columns
filtered_taxi_data <- filtered_taxi_data %>% select(-VendorID, -RatecodeID, -PULocationID, -DOLocationID, -store_and_fwd_flag)

# Below function and plot show the total number of trips by weekday and time period
# Function to categorize time period
categorize_time_period <- function(pickup_hour) {
  if (pickup_hour >= 5 & pickup_hour < 12) {
    return("Morning")
  } else if (pickup_hour >= 12 & pickup_hour < 17) {
    return("Afternoon")
  } else if (pickup_hour >= 17 & pickup_hour < 22) {
    return("Evening")
  } else {
    return("Night")
  }
}

# Dropping null values
filtered_taxi_data <- filtered_taxi_data %>% drop_na()

# Section 2: Feature Engineering
# Add 'hour' and 'time_period' columns to visualize the stats with trips and days of the week
filtered_taxi_data <- filtered_taxi_data %>%
  mutate(pickup_hour = hour(tpep_pickup_datetime),
         dropoff_hour = hour(tpep_dropoff_datetime),
         time_period = sapply(pickup_hour, categorize_time_period))

trip_counts <- filtered_taxi_data %>%
  group_by(trip_day, time_period) %>%
  summarise(trip_count = n())

# Section 3: Visualize the insights
# Total number of trips on every day segmented on the time period
ggplot(trip_counts, aes(x = trip_day, y = trip_count, fill = time_period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Number of Trips by Weekday and Time Period",
       x = "Weekday",
       y = "Total Number of Trips") +
  theme_minimal()

# Design a histogram and apply log on the tip_amount to normalize outliers and unskew the data
ggplot(filtered_taxi_data) + geom_histogram(aes(x=log(tip_amount+1)))

# Create a density plot of tip_amount categorized by trip day
ggplot(filtered_taxi_data) + geom_density(aes(x = tip_amount, fill=trip_day), alpha = 0.25)

# Finding out unique values of each column
# unique_values <- lapply(cleaned_taxi_data, unique)

# Section 4: Building model
# Building Predictive Model using Linear Regression with Tip as the outcome variable
set.seed(123)


##### Transformation to log sales (adding 1, because log(0) is undefined)
filtered_taxi_data$logtip_amount <- log(filtered_taxi_data$tip_amount+1)
ggplot(data=filtered_taxi_data) +
  geom_histogram(aes(x = logtip_amount))



trainIndex <- createDataPartition(filtered_taxi_data$logtip_amount, p = 0.8, list = FALSE)
trainData <- filtered_taxi_data[trainIndex, ]
testData <- filtered_taxi_data[-trainIndex, ]



trainData$log_trip_distance <- log(trainData$trip_distance + 1)
trainData$log_fare_amount <- log(trainData$fare_amount + 1)



#model <- lm(logtip_amount ~ trip_distance + fare_amount + passenger_count + duration + trip_day + pickup_hour + dropoff_hour + congestion_surcharge + time_period, data = trainData)
model <- lm(logtip_amount ~ log_trip_distance + log_fare_amount + passenger_count + duration + trip_day + pickup_hour + dropoff_hour + congestion_surcharge + time_period, data = trainData)
summary(model)
summary(model)

residuals2 <- residuals(model)
trainData$residuals2 <- residuals2 
ggplot(data=trainData) +
  geom_histogram(aes(x = residuals2))
ggplot(data = trainData) +
  geom_point(aes(x = model$fitted.values, y = residuals2)) +
  geom_hline(yintercept = 0, linetype = "dashed") 

plot(jitter(model$fitted.values), jitter(residuals2), main="Residuals vs Fitted Values", 
     xlab="Fitted Values", ylab="Residuals")



library(ggplot2)
ggplot(data = your_data, aes(x = model$fitted.values, y = residuals2)) +
  geom_point(alpha = 0.2) +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

subset_data <- sample(1:nrow(your_data), size = 1000)  # sample 1000 points
plot(model$fitted.values[subset_data], residuals2[subset_data], 
     main="Residuals vs Fitted Values (Subset)", 
     xlab="Fitted Values", ylab="Residuals", pch=20)

ggplot(data = your_data, aes(x = model$fitted.values, y = residuals2)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values with Smoothing",
       x = "Fitted Values",
       y = "Residuals")
predictions <- predict(model, newData=testData)

print(predictions)

# Calculate residuals
residuals <- trainData$tip_amount - predictions

# View the residuals
residuals

# Create a residual plot
plot(predictions, residuals, 
     xlab = "Predicted Values", 
     ylab = "Residuals", 
     main = "Residual Plot")
abline(h = 0, col = "red", lwd = 2)

ggplot(data = trainData) +
  geom_point(aes(x = model$fitted.values, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed") 

# Section 5: Write the output to CSV file
# write_csv(filtered_taxi_data, "Documents/Personal Documents/R-Projects/New York Fares/interm_data_clean.csv")


# Function to categorize durations with descriptive labels
categorize_duration <- function(duration_minutes) {
  if (duration_minutes >= 2 & duration_minutes < 15) {
    return("Short (2-15 minutes)")
  } else if (duration_minutes >= 15 & duration_minutes < 30) {
    return("Medium (15-30 minutes)")
  } else if (duration_minutes >= 30 & duration_minutes < 60) {
    return("Long (30-60 minutes)")
  } else if (duration_minutes >= 60 & duration_minutes <= 120) {
    return("Very Long (60-120 minutes)")
  } else {
    return(NA)  # Return NA for durations outside the expected range
  }
}

# Apply the categorize_duration function to create a new 'duration_category' column
filtered_taxi_data$duration_category <- sapply(filtered_taxi_data$duration, categorize_duration)

# Remove rows with NA in duration_category (optional)
filtered_taxi_data_no_na <- filtered_taxi_data[!is.na(filtered_taxi_data$duration_category), ]

# Reorder the duration_category factor levels
filtered_taxi_data_no_na$duration_category <- factor(filtered_taxi_data_no_na$duration_category, 
                                                     levels = c("Short (2-15 minutes)", 
                                                                "Medium (15-30 minutes)", 
                                                                "Long (30-60 minutes)", 
                                                                "Very Long (60-120 minutes)"))

# Create the boxplot with reordered duration categories
ggplot(data = filtered_taxi_data_no_na) +
  geom_boxplot(aes(x = duration_category, y = log(tip_amount))) +
  labs(x = "Trip Duration Category", y = "Log(Tip Amount)", title = "Boxplot of Tip Amount by Trip Duration Category") +
  theme_minimal()


# Function to categorize distances
categorize_distance <- function(distance_miles) {
  if (distance_miles > 0 & distance_miles <= 5) {
    return("<= 5 miles")
  } else if (distance_miles > 5 & distance_miles <= 15) {
    return("<= 15 miles")
  } else if (distance_miles > 15 & distance_miles <= 30) {
    return("<= 30 miles")
  } else if (distance_miles > 30 & distance_miles <= 50) {
    return("<= 50 miles")
  } else {
    return(NA)  # Return NA for distances outside the expected range
  }
}


# Apply the categorize_distance function to create a new column
filtered_taxi_data$distance_category <- sapply(filtered_taxi_data$trip_distance, categorize_distance)

# Reorder the distance_category factor levels
filtered_taxi_data$distance_category <- factor(filtered_taxi_data$distance_category, 
                                               levels = c("<= 5 miles", "<= 15 miles", "<= 30 miles", "<= 50 miles"))

# Create the boxplot with reordered distance categories
ggplot(data = filtered_taxi_data) +
  geom_boxplot(aes(x = distance_category, y = log(tip_amount))) +
  labs(x = "Trip Distance Category", y = "Log(Tip Amount)", title = "Boxplot of Tip Amount by Trip Distance Category") +
  theme_minimal()

# Remove rows where distance_category is NA
filtered_taxi_data_no_na <- filtered_taxi_data[!is.na(filtered_taxi_data$distance_category), ]

# Create the boxplot without NA categories
ggplot(data = filtered_taxi_data_no_na) +
  geom_boxplot(aes(x = distance_category, y = log(tip_amount))) +
  labs(x = "Trip Distance Category", y = "Log(Tip Amount)", title = "Boxplot of Tip Amount by Trip Distance Category") +
  theme_minimal()


