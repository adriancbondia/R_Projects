#Load necessary packages and data

library(tidyverse)
responses <- read_csv("datasets/kagglesurvey.csv")

# Print the first 10 rows
head(responses)

# Print the first respondent's tools and languages
responses[1,2]

# Add a new column, and unnest the new column
tools <- responses  %>% 
    mutate(work_tools = str_split(WorkToolsSelect,","))  %>% 
    unnest(work_tools)

#In this section we are going to group the data by tools and order the counts in a descending order.

tool_count <- tools  %>% 
    group_by(work_tools) %>%
    summarize(count = n())%>%
    arrange(desc(count))
   

# Create a bar chart of the work_tools column, most counts on the far right
ggplot(tool_count, aes(x=fct_reorder(work_tools, count),y=count),fct_reorder) + 
    geom_bar(stat = "identity")
    
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

# Group by language preference, calculate number of responses, and remove "neither"
debate_plot <- debate_tools  %>% 
   group_by(language_preference)  %>% 
   summarize(count = n())  %>% 
    filter(language_preference != "neither")

# Create a bar chart
ggplot(debate_plot, aes(x=language_preference,y=count)) + 
    geom_bar(stat = "identity")
    
    # Group by, summarise, arrange, mutate, and filter
recommendations <- debate_tools  %>% 
    group_by(language_preference, LanguageRecommendationSelect)%>%
    summarise(count = n())%>%
    arrange(language_preference,desc(count))%>%
    filter(row_number()<=4)
    
    # Create a faceted bar plot
ggplot(recommendations, aes(x = LanguageRecommendationSelect, y = n)) +
    geom_bar(stat = "identity") +
    facet_wrap(~language_preference)
    
    
