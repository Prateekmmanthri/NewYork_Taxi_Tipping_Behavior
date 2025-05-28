**Optimizing Tip Strategies for NYC Taxi Drivers**

**Overview**
This project explores tipping behavior in NYC taxi rides, using data analysis and modeling in R to understand the key drivers behind generous tips. We analyzed trip-level data and built models to uncover patterns, with a goal of helping drivers or platforms improve earnings via data-backed strategies.


**Dataset**
We used a publicly available NYC Taxi dataset.
🔗 Dataset Link ( [https://drive.google.com/drive/folders/1TXteiPRH5vBXshtPPFoJ75QAbp5tkld9?usp=drive_link](https://drive.google.com/drive/folders/1TXteiPRH5vBXshtPPFoJ75QAbp5tkld9?usp=sharing) )


**Objectives**
1. Identify factors that influence tip amounts
2. Visualize passenger tipping behavior across various ride characteristics
3. Build a linear regression model to predict tipping likelihood


**Tools & Technologies**
1. Language: R
2. Libraries: tidyverse, lubridate, ggplot2, caret
3. Platform: RStudio


**Key Insights**
1. Longer durations and distances → higher tips
   Median tip increases with ride length and distance.

2. Card payments lead to better tips
   Compared to cash, passengers paying by card tip more frequently and generously.

3. Rush hour patterns affect tipping
   Morning and evening rush hours have higher trip volumes and more diverse tipping behavior.

4. Passenger count has minimal effect
   Tipping seems relatively independent of how many people are in the car.


**Modeling Approach**
We used a linear regression model predicting log(tip_amount) as the target variable with features such as:

1. trip_distance, fare_amount, passenger_count
2. pickup/dropoff hours
3. trip duration, day of week
4. congestion surcharge
5. payment method, time period category

Residual and distribution plots were used to validate assumptions and detect variance patterns.


**Notes**
1. The dataset is not hosted in the repo to save space — use the provided Google Drive link.
2. The .RDataTmp, .Rhistory, and .Rproj files help preserve project context when opened in RStudio.

