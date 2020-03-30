#Step 1. Load all the required libraries and data.

# Loading the tidyverse, ggmap and viridis for extra colours and tree for regression tree.
library(tidyverse)
library(ggmap)
library(viridis)
library(tree)
library(lubridate)
library(randomForest)

# Reading in the taxi data
taxi <- read_csv("datasets/taxi.csv")

# Taking a look at the first couple of rows in taxi
head(taxi)

#Step 2. Clean the data renaming location variables, eliminating joureys with 0 tips or fares.
#Create the "total" variable with the logarithmic value of fare + tips
taxi <- taxi %>%
  rename("lat"="pickup_latitude", "long"="pickup_longitude")%>%
  filter(fare_amount >0|tip_amount>0) %>%
  mutate(total=log(fare_amount+tip_amount))

#Step 3. We focus on trips starting in manhattan where the biggest amount are: lat: 40.70-40.83/long:-74.025-73.93

taxi <- taxi  %>% 
  filter(between(lat, 40.70,40.83),between(long, -74.025,-73.93))

#Step 4. We draw a map from manhattan with the density of journeys and their start locations.

# Retrieving a stored map object which originally was created by
# manhattan <- get_map("manhattan", zoom = 12, color = "bw")
manhattan <- readRDS("datasets/manhattan.rds")

#Drawing a density map with the number of journey start locations
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  geom_bin2d(data=taxi, aes(x=long,y=lat), bins=60, alpha = 0.6) +
  labs(x='Longitude', y='Latitude', fill='Journeys')

#Step 5. We create a regression tree to predict the total fare with lat and long being the predictors.

# Fitting a tree to lat and long
fitted_tree <- tree(total ~ lat+long, data=taxi)

# Draw a diagram of the tree structure
plot(fitted_tree)
text(fitted_tree)

#Step 6. We add three new variables for a new prediction tree: hour, week dat and month.

# Generate the three new time variables
taxi <- taxi %>% 
  mutate(hour=hour(taxi$pickup_datetime),
         wday=wday(taxi$pickup_datetime, label=TRUE),
         month=month(taxi$pickup_datetime,label=TRUE))

# Fitting a tree with total as the outcome and lat, long, hour, wday, and month as predictors
fitted_tree <- tree(total ~ lat+long+hour+wday+month, taxi)

# draw a diagram of the tree structure
plot(fitted_tree)
text(fitted_tree)

# Summarizing the performance of the tree
summary(fitted_tree)

#Step 7.The results of the regression tree didn't change so we are going to try with a Random Forest model.

# Fitting a random forest
fitted_forest <- randomForest(total ~ lat+long+hour+wday+month, taxi,ntree=80,sampsize=10000)

# Printing the fitted_forest object
print(fitted_forest)

#Step 8.Fitted_forest has a slightly lower error so we are going to produce the predictions.

# Extracting the prediction from fitted_forest
taxi$pred_total <- fitted_forest$predicted

# Plotting the predicted mean trip prices from according to the random forest
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  stat_summary_2d(data=taxi, aes(x=long,y=lat,z=pred_total), bins=60, alpha = 0.6, fun=mean) +
  labs(x='Longitude', y='Latitude', fill='Mean trip prices')

#Step 9. Compare the map with the predicted fares with a new map showing the mean fares according to the data.

# Function that returns the mean *if* there are 15 or more datapoints
mean_if_enough_data <- function(x) { 
  ifelse( length(x) >= 15, mean(x), NA) 
}

# Plotting the mean trip prices from the data
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  stat_summary_2d(data=taxi, aes(x=long,y=lat,z=total), bins=60, alpha = 0.6, fun=mean_if_enough_data) +
  labs(x='Longitude', y='Latitude', fill='Mean trip prices')
