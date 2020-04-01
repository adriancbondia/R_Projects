#Step 1. Load the necessary packages and data

library(tidyverse)
library(lubridate)
library(ggridges)

ba_dates <- read_csv("datasets/breath_alcohol_datetimes.csv")

#Step 2. Modify the date column and create a new column with the day of the week. Show a bar chart with the days of the week.

#Change DateTime column to America/Chicago with force_tz
ba_dates <- ba_dates %>% mutate(DateTime = force_tz(DateTime,"America/Chicago"))

#Create a wkday column in the ba_dates 
ba_dates <- ba_dates %>% mutate(wkday = DateTime %>% wday(label=T))

#Create a bar chart of # tests by day of week
ggplot(data = ba_dates, aes(x = wkday)) + 
  geom_bar()

#Step 3. Analyze the hour of the day in which the tests are made during Friday, Saturday and Sunday.

#Create hour variable
ba_dates <- ba_dates %>% mutate(hr = hour(DateTime))

#Create weekend data frame including Friday, Saturday and Sunday
weekend <- ba_dates %>% filter(wkday %in% c("Fri", "Sat", "Sun"))

#Show side-by side bar charts counting hour of the day of tests for each weekend day
ggplot(data = weekend) + 
  geom_bar(aes(x = hr)) + 
  facet_grid(.~wkday) + 
  scale_x_continuous(breaks = 1:12*2-1) # for ease of readability

#Step 4. Explore the pattern of alcohol tests per day and visualize the result with a line plot

#Create a date column rounded to the nearest day. as.Date() is for the plot later
ba_dates <- ba_dates %>% mutate(date = as.Date(round_date(DateTime, unit="day"))) 

#Count number of tests per date 
ba_summary <- ba_dates %>% count(date)

#Pipe the result from above into ggplot() using geom_line to create a time series plot.
ba_summary %>% 
  ggplot() + 
  geom_line(aes(x = date, y = n), alpha = .7) + # change alpha for readability 
  scale_x_date("6 months") + 
  theme(axis.text.x = element_text(angle = 30)) # make x-axis more readable

#Step 5. Analize the Iowa State football Schedule 

#Load the football dataset 
isu_fb <- read_csv("datasets/isu_football.csv")

#Create Date as a date format variable 
isu_fb <- isu_fb %>% mutate(Date = parse_date(Date, format = '%b %d, %Y'))

#Filter ba_summary
ba_fb <- ba_summary %>% filter(date  %in% isu_fb$Date)

#Arrange ba_fb by number of tests from high to low and print first six rows 
ba_fb %>% arrange(desc(n)) %>% head(6)

#Step 5. Analize teh effect of a home/away match and a result of win/lose.

#Join ba_summary to isu_fb 
isu_fb2 <- isu_fb %>% left_join(ba_summary, by = c("Date"="date"))

#Change nas to 0s 
isu_fb2 <- isu_fb2 %>% mutate(n = ifelse(is.na(n), 0, n))

#Draw a plotwith the result
isu_fb2 %>% ggplot(aes(n, fill=Home))+geom_bar()+facet_grid(.~Res)

#Step 6. Compare basketball season results (Novembrer-March) with football season results (Septemeber-November). 

#Create a mo and a yr column in ba_dates
ba_dates <- ba_dates %>% mutate(mo = month(date, label=T), yr = year(date))

#Make bar chart by mo and color by year 
ba_dates %>% ggplot(aes(mo, fill=as.factor(yr)))+geom_bar()

#Step 7. Compare the VEISHEA (weeklog festival) weeks with the non-VEISHEA weeks.

# In 2013, VEISHEA was held from April 15-21. In 2014, it was held from April 7-13. 
v13 <- interval(make_date(2013, 4, 15) , make_date(2013, 4, 21), tzone = "America/Chicago")
v14 <- interval(make_date(2014, 4, 7) , make_date(2014, 4, 13), tzone = "America/Chicago")

#Other comparable VEISHEA weeks in 2015-2017
v15 <- interval(make_date(2015, 4, 13) , make_date(2015, 4, 19), tzone = "America/Chicago")
v16 <- interval(make_date(2016, 4, 11) , make_date(2016, 4, 17), tzone = "America/Chicago")
v17 <- interval(make_date(2017, 4, 10) , make_date(2017, 4, 16), tzone = "America/Chicago")

#Filter ba_dates for only the 5 veishea intervals
veishea <- ba_dates %>% filter(date %within% v13 | date  %within% v14 | 
                                 date  %within% v15 | date  %within% v16 | date  %within% v17)

#Count up years 
veishea %>% count(yr)

#Step 8. Check the results of the breathanalyzer tests. 

#Take a mean of res1, res2

ba_dates <- ba_dates %>% mutate(res=(Res1+Res2)/2)

#Make ridgeline plot 
ggplot(data = ba_dates, aes(x = res, y = hr, group = hr)) +
  # some style choices made already
  geom_density_ridges(alpha = 0.7, fill = "steelblue", bandwidth = .01, rel_min_height = 0.0001) + 
  scale_y_continuous(breaks = 0:23)

#Step 9.Perform a second ridgeline plot fixing the values under 0.

#Create a zero indicator variable 
ba_dates <- ba_dates %>% mutate(zero = res == 0)

#Tabulate the data by the zero column
ba_dates %>% count(zero)

#Redo ridge with no 0s
ba_dates %>% filter(!zero) %>% 
  ggplot(aes(x = res, y = hr, group = hr)) +
  geom_density_ridges(alpha = 0.7, fill = "steelblue", bandwidth = .01, rel_min_height = 0.005) + 
  scale_y_continuous(breaks = 0:23)

#Step 10. Check the data to filter all the results with an alcohol concentration of 0.31 or higher.

#Filter the ba_dates data to contain only those with the most dangerous result
danger <- ba_dates %>% filter(res>=.31)

#Print danger
print(danger)
