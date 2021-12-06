library(tidyverse)
library(SentimentAnalysis) # for sentiment analysis

dk_sentiment <- read.csv("utils/2a_fullform_headword_polarity.csv", encoding = "UTF-8", header = FALSE, sep = "\t")
names(dk_sentiment) <- c("word_form", "headword", "homograph_number", "POS", "DDO_headword_ID", "polarity_label_headword")
# Source: Det Danske Sprog- og Litteraturselskab (DSL, Society for Danish Language and Literature) and Center for Sprogteknologi, Københavns Universitet (CST, Centre for Language Technology, University of Copenhagen)
# Source origin: https://github.com/dsldk/danish-sentiment-lexicon

# Use only word_form, headword and polarity
dk_sentiment_min <- dk_sentiment %>% 
  select(word_form, headword, polarity_label_headword) %>% 
  rename(polarity = polarity_label_headword)
dk_sentiment_headword <- select(dk_sentiment_min, headword, polarity) %>% 
  .[!duplicated(.),]
dk_sentiment_word_form <- select(dk_sentiment_min, word_form, polarity) %>% 
  .[!duplicated(.),]

tokens <- readRDS("data/tokens.rds")
tokens$polarity = 0
tokens$headword = tokens$stemmed
for (i_word in unique(tokens$word)) {
  if(i_word %in% dk_sentiment_headword$headword){
    tokens <- tokens %>%
      rowwise() %>% 
      mutate(polarity = ifelse(word == i_word, dk_sentiment_headword[dk_sentiment_headword$headword == i_word,]$polarity[1], polarity))
  }
  if(i_word %in% dk_sentiment_word_form$word_from){
    tokens <- tokens %>%
      rowwise() %>% 
      mutate(polarity = ifelse(word == i_word, dk_sentiment_word_form[dk_sentiment_word_form$word_from == i_word,]$polarity[1], polarity))
  }
}
for (i_word in unique(tokens$stemmed)) {
  if(i_word %in% dk_sentiment_headword$headword){
    tokens <- tokens %>%
      rowwise() %>% 
      mutate(polarity = ifelse(stemmed == i_word, dk_sentiment_headword[dk_sentiment_headword$headword == i_word,]$polarity[1], polarity))
  }
  if(i_word %in% dk_sentiment_word_form$word_from){
    tokens <- tokens %>%
      rowwise() %>% 
      mutate(polarity = ifelse(stemmed == i_word, dk_sentiment_word_form[dk_sentiment_word_form$word_from == i_word,]$polarity[1], polarity))
  }
}

# Does not work, as dk_sentiment words are not unique
# tokens <- tokens %>%
#   left_join(dk_sentiment_min, by=c("stemmed" = "word_form")) %>%
#   left_join(dk_sentiment_min, by=c("word" = "word_form")) %>%
#   rowwise() %>%
#   mutate(polarity.x = as.numeric(ifelse(is.numeric(polarity.x), polarity.x, polarity.y)),
#          headword.x = ifelse(!is.na(headword.x), headword.x, headword.y)) %>%
#   select(-c(polarity.y, headword.y)) %>%
#   rename(polarity = polarity.x, headword = headword.x) %>%
#   left_join(dk_sentiment_min, by=c("stemmed" = "headword")) %>%
#   select(-c(word_form)) %>%
#   mutate(polarity.x = as.numeric(ifelse(is.numeric(polarity.x), polarity.x, polarity.y))) %>%
#   select(-c(polarity.y)) %>%
#   left_join(dk_sentiment_min, by=c("word" = "headword")) %>%
#   select(-c(word_form)) %>%
#   mutate(polarity = as.numeric(ifelse(is.numeric(polarity), polarity, polarity.x))) %>%
#   select(-c(polarity.x)) %>%
#   mutate(polarity = as.numeric(ifelse(!is.na(polarity), polarity, 0)))

tokens <- tokens %>%
  mutate(sentiment = analyzeSentiment(word, removeStopwords = FALSE, language = "danish")[["SentimentGI"]])

# Add sentiment labels
tokens <- tokens %>%
  mutate(sentiment = ifelse(polarity>= 2, "Positiv",
                                 ifelse(polarity <= -2, "Negativ", "Neutral"))) %>%
  mutate(sentiment_true = ifelse(polarity > 0, "Positiv",
                                 ifelse(polarity < 0, "Negativ", "Neutral")))

saveRDS(tokens,"data/tokens.rds")
