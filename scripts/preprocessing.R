# Libraries
library(tidyverse)
library(stopwords)
library(SnowballC)
library(tidytext)

# Importing data
df <- read.csv("data/nys_2001-2020_cleaned.csv")

# Tokenization, removing stopwords and stemming
stop_words <- data.frame(word = stopwords(language = "da", source = "snowball")) # stopwords list

tokens <- tibble(df) %>%
  unnest_tokens(word, speech) %>% #tokenization
  anti_join(stop_words, by = "word") %>% #removing stopwords 
  count(year, word, sort = T) #frequency count

tokens$stemmed <- wordStem(tokens$word, language = "danish") #stemming
