#Load necessary packages and data

library(tidyverse)

responses <- read_csv("datasets/kagglesurvey.csv")

#1.We create the variable Tools which is a copy of the dataset with an extra column called "work_tools" with all the different Data Science Tools from the column WorkToolSelect.
tools <- responses  %>% 
    mutate(work_tools = str_split(WorkToolsSelect,","))  %>% 
    unnest(work_tools)
    
#2.We create the variable "tool_count" with the different tools and the number of users arranged in descending order.
tool_count <- tools  %>% 
    group_by(work_tools) %>%
    summarize(count = n())%>%
    arrange(desc(count))

#Graphic with the different tools and the number of users in ascending order.
ggplot(tool_count, aes(x=fct_reorder(work_tools, count),y=count),fct_reorder) + 
    geom_bar(stat = "identity")

#3.We calculate the amount of users for Python, R and both and we store the data in the "language_preference" column.
debate_tools <- responses  %>% 
   mutate(language_preference = case_when(
       str_detect(WorkToolsSelect, "R")& str_detect(WorkToolsSelect, "Python", negate = TRUE) ~ "R",
       str_detect(WorkToolsSelect, "Python")& str_detect(WorkToolsSelect, "R", negate = TRUE) ~ "Python",
       str_detect(WorkToolsSelect, "R") & str_detect(WorkToolsSelect, "Python") ~ "both",
       TRUE ~ "neither"       
   ))

#4. Group by language preference, calculate number of responses, and remove "neither"
debate_plot <- debate_tools  %>% 
   group_by(language_preference)  %>% 
   summarize(count = n())  %>% 
    filter(language_preference != "neither")

# Create a bar chart
ggplot(debate_plot, aes(x=language_preference,y=count)) + 
    geom_bar(stat = "identity")

