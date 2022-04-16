library(tidyverse)
library(tidytext)

print("sentiment_analysis.R")

article_lib <- readRDS("data/article_library.rds") # File containing UUID, Article name

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

# Check if sentiment exists ----
if(file.exists("data/sentiments.rds")){
  sentiments <- readRDS("data/sentiments.rds")
  old_tokens <- tokens %>% 
    filter(!is.na(polarity))
  tokens <- tokens %>% 
    filter(is.na(polarity))
}

if(nrow(tokens)>0){
    
  ## Set new token columns ----
  tokens$polarity = 0
  
  tokens$headword = tokens$stemmed
  
  ## Add polarity/sentiment to tokens ----
  tokens <- tokens %>%
    rowwise() %>% 
    ### ... Through stemmed ----
    mutate(polarity = ifelse(stemmed  %in% dk_sentiment_word_form$word_from,
                           dk_sentiment_word_form[dk_sentiment_word_form$word_from == stemmed,]$polarity[1],
                           ifelse(stemmed %in% dk_sentiment_headword$headword,
                                  dk_sentiment_headword[dk_sentiment_headword$headword == stemmed,]$polarity[1],
                                  ### ... Through lemma ----
                                  ifelse(any(unlist(strsplit(lemma, ",")) %in% dk_sentiment_headword$headword),
                                         dk_sentiment_headword[dk_sentiment_headword$headword == unlist(strsplit(lemma, ","))[min(which(unlist(strsplit(lemma, ",")) %in% dk_sentiment_headword$headword))],]$polarity[1],
                                         ifelse(any(unlist(strsplit(lemma, ",")) %in% dk_sentiment_word_form$word_from),
                                                dk_sentiment_word_form[dk_sentiment_word_form$word_from == unlist(strsplit(lemma, ","))[min(which(unlist(strsplit(lemma, ",")) %in% dk_sentiment_word_form$word_from))],]$polarity[1],
                                                ### ... Through word ----
                                                ifelse(word %in% dk_sentiment_headword$headword,
                                                       dk_sentiment_headword[dk_sentiment_headword$headword == word,]$polarity[1],
                                                       ifelse(word  %in% dk_sentiment_word_form$word_from,
                                                              dk_sentiment_word_form[dk_sentiment_word_form$word_from == word,]$polarity[1],
                                                              polarity
                                                              )
                                                       )
                                                )
                                         )
                                  )
                           )
           )
  
  if(exists("old_tokens")){
    new_tokens <- tokens
    tokens <- tokens %>%
      full_join(old_tokens) %>% 
      select(!n_stem)
  }
  
  
  ## Summarise tokens ----
  n_tokens <- tokens %>%
    group_by(stemmed, uuid) %>%
    summarise(n_stem = sum(n_in))
  
  tokens <- tokens %>%
    left_join(n_tokens, by=c("uuid", "stemmed")) %>% 
    arrange(desc(n_stem_total), word, desc(n_lemma_total),
            desc(n_total), desc(uuid),
            desc(n_in)) # arrange by largest n_hword_total
  
  ## Add sentiment labels ----
  tokens <- tokens %>%
    mutate(sentiment = ifelse(polarity>= 2, "Positive",
                                   ifelse(polarity <= -2, "Negative", "Neutral"))) %>%
    mutate(sentiment_true = ifelse(polarity > 0, "Positive",
                                   ifelse(polarity < 0, "Negative", "Neutral")))
  
  ## Save Token ----
  saveRDS(tokens,"data/tokens.rds")
  write.csv(tokens, "data/tokens.csv", row.names = F, fileEncoding = "UTF-8")
  
  # Calculate sentiment of each instance ----
  if(exists("sentiments")){
    new_sentiments <- new_tokens %>% 
      filter(!(uuid %in% unique(sentiments$uuid))) %>% 
      rowwise() %>% 
      mutate(polarity_pos = as.numeric(ifelse(polarity > 0, polarity, 0)),
             polarity_neg = as.numeric(ifelse(polarity < 0, polarity, 0)),
             n_in_pos = as.numeric(ifelse(polarity > 0, n_in, 0)),
             n_in_neg = as.numeric(ifelse(polarity < 0, n_in, 0))) %>% 
      group_by(uuid) %>%
      summarise(sentiment = sum(n_in*polarity),
                sentiment_pos = sum(n_in*polarity_pos),
                sentiment_neg = sum(n_in*polarity_neg),
                average_sentiment = mean(n_in*polarity),
                n_pos = sum(n_in_pos),
                n_neg = sum(n_in_neg)
      ) %>% 
      mutate(n_words = n_pos+n_neg) %>% 
      rowwise() %>% 
      mutate(sentiment_label = as.character(ifelse(sentiment>0, "Positive", "Negative"))) %>% 
      mutate(title = paste(article_lib$title[min(which(uuid == article_lib$uuid))], " (", uuid, ")", sep = ""),
             date_updated_at = article_lib$date_updated_at[min(which(uuid == article_lib$uuid))],
             date_published_at = article_lib$date_published_at[min(which(uuid == article_lib$uuid))]
      )
    sentiments <- sentiments %>% 
      mutate(sentiment_label = as.character(sentiment_label))
    new_sentiments <- new_sentiments %>% 
      mutate(sentiment_label = as.character(sentiment_label))
    sentiments <- new_sentiments %>% 
      full_join(sentiments)
  } else{
    sentiments <- tokens %>% 
      rowwise() %>% 
      mutate(polarity_pos = as.numeric(ifelse(polarity > 0, polarity, 0)),
             polarity_neg = as.numeric(ifelse(polarity < 0, polarity, 0)),
             n_in_pos = as.numeric(ifelse(polarity > 0, n_in, 0)),
             n_in_neg = as.numeric(ifelse(polarity < 0, n_in, 0))) %>% 
      group_by(uuid) %>%
      summarise(sentiment = sum(n_in*polarity),
                sentiment_pos = sum(n_in*polarity_pos),
                sentiment_neg = sum(n_in*polarity_neg),
                average_sentiment = mean(n_in*polarity),
                n_pos = sum(n_in_pos),
                n_neg = sum(n_in_neg)
      ) %>% 
      mutate(n_words = n_pos+n_neg) %>% 
      rowwise() %>% 
      mutate(sentiment_label = ifelse(sentiment>0, "Positive", "Negative"))
    
    # sentiments <- readRDS("data/sentiments.rds") # Sentiment for year
    sentiments <- sentiments %>% 
      mutate(title = paste(article_lib$title[min(which(uuid == article_lib$uuid))], " (", uuid, ")", sep = ""),
             date_updated_at = article_lib$date_updated_at[min(which(uuid == article_lib$uuid))],
             date_published_at = article_lib$date_published_at[min(which(uuid == article_lib$uuid))]
      )
  }
  
  ## Save sentiments ----
  saveRDS(sentiments, "data/sentiments.rds")
  write.csv(sentiments, "data/sentiments.csv", row.names = F, fileEncoding = "UTF-8")
}

# Unsetting ----
rm(list=ls())
detach("package:tidyverse", unload=TRUE)
detach("package:tidytext", unload=TRUE)
