#Step 1. Load the required packages and data

# Loading in required libraries
library(tidyverse)
library(lubridate)

# Reading in the Nobel Prize data
nobel <- read_csv("datasets/nobel.csv")

# Taking a look at the first couple of winners
head(nobel)

#Step 2. Check which sex and country is the most commonly represented.

# Counting the number of (possibly shared) Nobel Prizes handed
# out between 1901 and 2016
nobel %>% count()

# Counting the number of prizes won by male and female recipients.
nobel %>%count(sex)

# Counting the number of prizes won by different nationalities.
nobel %>%
  count(birth_country) %>%
  arrange(desc(n)) %>%
  head(20)

#Step 3. Check when the USA started to dominate in the Nobel Prize Charts and make a plot.

# Calculating the proportion of USA born winners per decade
prop_usa_winners <- nobel %>% 
  mutate(usa_born_winner = birth_country == "United States of America",decade = floor(year / 10) * 10) %>%
  group_by(decade) %>%
  summarize(proportion = mean(usa_born_winner, na.rm = TRUE))

# Display the proportions of USA born winners per decade
prop_usa_winners

# Setting the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=4)

# Plotting USA born winners
ggplot(prop_usa_winners, aes(x = decade, y = proportion)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent, limits = 0:1, expand = c(0, 0))

#Step 4. Check the ratio of men in each category (physics, medicine, literature, ...)

# Calculating the proportion of female laureates per decade
prop_female_winners <- nobel %>%
  mutate(female_winner = sex == "Female", decade = floor(year / 10) * 10) %>%
  group_by(decade, category) %>%
  summarize(proportion = mean(female_winner, na.rm = TRUE))

# Plotting the proportion of female laureates per decade
ggplot(prop_female_winners, aes(x = decade, y = proportion, color = category)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent, limits = 0:1, expand = c(0, 0))

#Step 5. Who and in what category was the first woman to receive a Nobel Prize.

# Picking out the first woman to win a Nobel Prize
nobel %>%
  filter(sex == "Female") %>%
  top_n(1, desc(year))

#Step 6. Who are the few that have gotten more than one nobel prize.

# Selecting the laureates that have received 2 or more prizes.
nobel %>%
  count(full_name) %>%
  filter(n > 1)

#Step 7. Plot the age of the Nobel Prize winners and a second one separated by categories.

# Calculating the age of Nobel Prize winners
nobel_age <- nobel %>%
  mutate(age = year - year(birth_date))

# Plotting the age of Nobel Prize winners
ggplot(nobel_age, aes(x = age, y = year)) +
  geom_point() +
  geom_smooth()

# Same plot as above, but faceted by the category of the Nobel Prize
ggplot(nobel_age, aes(x = age, y = year)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ category)

#Step 8. Oldest and youngest winner

# The oldest winner of a Nobel Prize as of 2016
nobel_age %>% top_n(1, age)

# The youngest winner of a Nobel Prize as of 2016
nobel_age %>% top_n(1, desc(age))

# The name of the youngest winner of the Nobel Prize as of 2016
youngest_winner <- "Malala Yousafzai"
