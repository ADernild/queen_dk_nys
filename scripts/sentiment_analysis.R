library(tidyverse)
library(tidytext)

# Get sentiments from a sentiment library ----
dk_sentiment <- read.csv("https://raw.githubusercontent.com/dsldk/danish-sentiment-lexicon/main/2a_fullform_headword_polarity.csv", encoding = "UTF-8", header = FALSE, sep = "\t")
names(dk_sentiment) <- c("word_form", "headword", "homograph_number", "POS", "DDO_headword_ID", "polarity_label_headword")
# Source: Det Danske Sprog- og Litteraturselskab (DSL, Society for Danish Language and Literature) and Center for Sprogteknologi, Københavns Universitet (CST, Centre for Language Technology, University of Copenhagen)
# Source origin: https://github.com/dsldk/danish-sentiment-lexicon

## Formatting sentiment library ----
# Use only word_form, headword and polarity
dk_sentiment_min <- dk_sentiment %>% 
  select(word_form, headword, polarity_label_headword) %>% 
  rename(polarity = polarity_label_headword)
dk_sentiment_headword <- select(dk_sentiment_min, headword, polarity) %>% 
  .[!duplicated(.),]
dk_sentiment_word_form <- select(dk_sentiment_min, word_form, polarity) %>% 
  .[!duplicated(.),]

# Tokens ----
tokens <- readRDS("data/tokens.rds")

## Set new token columns ----
tokens$polarity = 0
tokens$headword = tokens$stemmed

## Add polarity/sentiment to tokens ----
### ... Through stemming loop ----
# Note: Stemming and word mostly finds the same words, but will find cases the
#       other does not. Sometimes the sentiment are different. The word level is most
#       correct and is thus ran last.
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

### ... Through lemma loop ----
# Note: lemmatized words can contain multiple words and needs to be split
# Unique instances
uniqe_lemma <- unique(tokens$lemma)
# Loop to split
for(row in uniqe_lemma){
  # Detect multiple words
  if(grepl(",", row, fixed = TRUE)){
    # Split strings
    row_strings <- strsplit(row, ",")
    # Add each string as a new word
    for (string in row_strings) {
      uniqe_lemma <- c(uniqe_lemma, string)
    }
    # Remove instance of multiple words
    uniqe_lemma <- uniqe_lemma[uniqe_lemma!=row]
  }
}

# Filter redundant words
uniqe_lemma <- unique(uniqe_lemma)

# Loop unique lemmatized words
for (i_word in uniqe_lemma) {
  if(i_word %in% dk_sentiment_headword$headword){
    tokens <- tokens %>%
      rowwise() %>% 
      mutate(polarity = ifelse(i_word %in% unlist(strsplit(lemma, ",")), dk_sentiment_headword[dk_sentiment_headword$headword == i_word,]$polarity[1], polarity))
  }
  if(i_word %in% dk_sentiment_word_form$word_from){
    tokens <- tokens %>%
      rowwise() %>% 
      mutate(polarity = ifelse(i_word %in% unlist(strsplit(lemma, ",")), dk_sentiment_word_form[dk_sentiment_word_form$word_from == i_word,]$polarity[1], polarity))
  }
}

### ... Through word loop ----
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


year_tokens <- tokens %>%
  group_by(stemmed, year) %>%
  summarise(n_stem_year = sum(n_in_year))

tokens <- tokens %>%
  left_join(year_tokens, by=c("year", "stemmed")) %>% 
  arrange(desc(n_stem_total), word, desc(n_lemma_total),
          desc(n_total), desc(year),
          desc(n_in_year)) # arrange by largest n_hword_total

## Add sentiment labels ----
tokens <- tokens %>%
  mutate(sentiment = ifelse(polarity>= 2, "Positive",
                                 ifelse(polarity <= -2, "Negative", "Neutral"))) %>%
  mutate(sentiment_true = ifelse(polarity > 0, "Positive",
                                 ifelse(polarity < 0, "Negative", "Neutral")))

## Save Token ----
saveRDS(tokens,"data/tokens.rds")

# Calculate sentiment of each year ----
sentiments <- tokens %>% 
  rowwise() %>% 
  mutate(polarity_pos = as.numeric(ifelse(polarity > 0, polarity, 0)),
         polarity_neg = as.numeric(ifelse(polarity < 0, polarity, 0)),
         n_in_year_pos = as.numeric(ifelse(polarity > 0, n_in_year, 0)),
         n_in_year_neg = as.numeric(ifelse(polarity < 0, n_in_year, 0))) %>% 
  group_by(year) %>%
  summarise(sentiment = sum(n_in_year*polarity),
            sentiment_pos = sum(n_in_year*polarity_pos),
            sentiment_neg = sum(n_in_year*polarity_neg),
            average_sentiment = mean(n_in_year*polarity),
            n_pos = sum(n_in_year_pos),
            n_neg = sum(n_in_year_neg)
  ) %>% 
  mutate(n_words = n_pos+n_neg) %>% 
  rowwise() %>% 
  mutate(sentiment_label = ifelse(sentiment>0, "Positive", "Negative"))

## Save sentiments ----
saveRDS(sentiments, "data/sentiments.rds")

