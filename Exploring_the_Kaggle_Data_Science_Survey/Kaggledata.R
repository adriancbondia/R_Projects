#Step 1. Load the necessary packages and data.
library(tidyverse)
responses <- read_csv("datasets/kagglesurvey.csv")

#Step 2. Examinate the data format and how the multiple items are separated.
# Print the first 10 rows
head(responses)
# Print the first respondent's tools and languages
responses[1,2]

#Step 3. Separate each of the different tools from the "WorkToolSelect" column and create a new one called "tools".
# Add a new column, and unnest the new column
tools <- responses  %>% 
  mutate(work_tools = str_split(WorkToolsSelect,","))  %>% 
  unnest(work_tools)

# View the first 6 rows of tools
head(tools,6)

#Step 4. Count how many times each of the tool is being used and store it in the "tool_count" variable.

# Group the data by work_tools, summarise the counts, and arrange in descending order
tool_count <- tools  %>% 
  group_by(work_tools) %>%
  summarize(count = n())%>%
  arrange(desc(count))

# Print the first 6 results
head(tool_count,6)

#Step 5. Visualize how popular are each of the tools and show which ones are the most popular.

# Create a bar chart of the work_tools column, most counts on the far right
ggplot(tool_count, aes(x=fct_reorder(work_tools, count),y=count),fct_reorder) + 
  geom_bar(stat = "identity")

#Step 6. Create a new column to check how often R, Python, both or none are being used.

# Create a new column called language preference
debate_tools <- responses  %>% 
  mutate(language_preference = case_when(
    str_detect(WorkToolsSelect, "R")& str_detect(WorkToolsSelect, "Python", negate = TRUE) ~ "R",
    str_detect(WorkToolsSelect, "Python")& str_detect(WorkToolsSelect, "R", negate = TRUE) ~ "Python",
    str_detect(WorkToolsSelect, "R") & str_detect(WorkToolsSelect, "Python") ~ "both",
    TRUE ~ "neither"
  ))

# Print the first 6 rows
head(debate_tools, 6)

#Step 7. Show a bar chart with the amount of responses for R, Python or both.

# Group by language preference, calculate number of responses, and remove "neither"
debate_plot <- debate_tools  %>% 
  group_by(language_preference)  %>% 
  summarize(count = n())  %>% 
  filter(language_preference != "neither")

# Create a bar chart
ggplot(debate_plot, aes(x=language_preference,y=count)) + 
  geom_bar(stat = "identity")

#Step 8. Create a new variable with the language recomendation for new learners and show a graphic with the result.

# Creating a new data frame
recommendations <- debate_tools

# Grouping by language_preference and then LanguageRecommendationSelect
recommendations <- recommendations  %>% 
  group_by(language_preference, LanguageRecommendationSelect)  %>% 
  summarise(n = n())

# Removing empty responses and include the top recommendations
recommendations <- recommendations %>%
  filter(row_number() <= 4)

# Creating a faceted bar plot
ggplot(recommendations, aes(x = LanguageRecommendationSelect, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~language_preference)

