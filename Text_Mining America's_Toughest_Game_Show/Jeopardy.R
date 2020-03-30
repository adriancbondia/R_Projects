#Step 1. Load all the required packages and data

library(readr)
library(dplyr)
library(tm)
library(wordcloud)
jeopardy <- read_csv("datasets/jeopardy.csv")

#Step 2. Check the disposition of the differnt variables of the data set.

glimpse(jeopardy)
head(jeopardy,6)

#Step 3. Transform the data categories in a more workable body of text.

categories <- jeopardy %>% 
  filter(round == "Jeopardy!") %>% 
  select(category)

categories_source <- VectorSource(categories)
categories_corp <- VCorpus(categories_source)

#Step 4. Clean the categories

clean_corp <- tm_map(categories_corp, content_transformer(tolower))
clean_corp <- tm_map(clean_corp, removePunctuation)
clean_corp <- tm_map(clean_corp, stripWhitespace)
clean_corp <- tm_map(clean_corp, removeWords, stopwords("english"))

categories_tdm <- TermDocumentMatrix(clean_corp)

#Step 5. Word frequency. Turn TDM into a matrix and rank the most frequent words.

categories_m <- as.matrix(categories_tdm)
term_frequency <- sort(rowSums(categories_m), decreasing = TRUE)
barplot(term_frequency[1:12], las = 2)

#Step 6. Remove words that don't have a significant weight.

cleaner_corp <- tm_map(clean_corp, removeWords, c("time", "new", "first", "lets"))
cleaner_tdm <- TermDocumentMatrix(cleaner_corp)
categories_m <- as.matrix(cleaner_tdm)
term_frequency <- sort(rowSums(categories_m), decreasing = TRUE)
barplot(term_frequency[1:12], las = 2)

#Step 7. Transform the messy code into one line functions to clean and extract the frequency of each term.

speed_clean <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  return(corpus)}


freq_terms <- function(list){
  source <- VectorSource(list)
  corpus <- VCorpus(source)
  clean_corpus <- speed_clean(corpus)
  tdm <- TermDocumentMatrix(clean_corpus)
  matrix <- as.matrix(tdm)
  term_frequency <- sort(rowSums(matrix), decreasing = TRUE)
  return(term_frequency)}

#Step 8. Check the final answers for the last round ("Final Jeopardy!")

answers <- jeopardy %>% 
  filter(round == "Final Jeopardy!") %>% 
  select(answer)
ans_frequency <- freq_terms(answers)
ans_names <- names(ans_frequency)
wordcloud(ans_names, ans_frequency, max.words = 40)
