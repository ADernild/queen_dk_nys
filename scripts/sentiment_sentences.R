## Library for lemmatizing sentences
library(udpipe)
library(tidytext)
library(stringr)
library(dplyr)
library(stringi)
library(SnowballC)

print("sentiment_sentences.R")

# sentences
# df <- read.csv("data/sentences.csv")
df <- readRDS("data/sentences.rds")
stop_words <- readRDS("utils/stopwords.rds")$word

tokens <- readRDS("data/tokens.rds") %>%  # All tokens, filtered
  filter(polarity != 0)

# Functions
firstup <- function(x) { # Source: https://stackoverflow.com/questions/18509527/first-letter-to-upper-case
  if(is.null(x) || x==F && !is.numeric(x)){
    return("")
  } else if(is.numeric(x)){
    return(x)
  }
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

add_sentiment_labels <- function(word_list, word_list_comma, uuid){
  # Function to
  #   1. Add sentiment labels as html
  #   2. Add commas at specific positions, after text has been wrapped by HTML
  #   3. Add with polarity by word
  
  # Do stuff
  # Get tokens
  tokens <- tokens[tokens$uuid == uuid,][tokens[tokens$uuid == uuid,]$word %in% word_list,]
  for (i in 1:length(word_list)) {
    org_word <- word_list[i]
    if(org_word %in% tokens$word){
      tokens_istance <- tokens[tokens$word == org_word,][1,]
      if(tokens_istance$polarity>0){
        word_list[i] <- paste("<span class='sentiment_pos pol", tokens_istance$polarity, "' title='Polarity of: \"", tokens_istance$polarity, "\"'>", ifelse(i==1,firstup(org_word),org_word), "</span>", sep="")
      }
      else{
        word_list[i] <- paste("<span class='sentiment_neg pol", tokens_istance$polarity, "' title='Polarity of: \"", tokens_istance$polarity, "\"'>", ifelse(i==1,firstup(org_word),org_word), "</span>", sep="")
      }
    } else if(wordStem(org_word, language = "danish") %in% tokens$stemmed){
      tokens_istance <- tokens[tokens$stemmed == wordStem(org_word, language = "danish"),][1,]
      if(tokens_istance$polarity>0){
        word_list[i] <- paste("<span class='sentiment_pos pol", tokens_istance$polarity, "' title='Polarity of: \"", tokens_istance$polarity, "\"'>", ifelse(i==1,firstup(org_word),org_word), "</span>", sep="")
      }
      else{
        word_list[i] <- paste("<span class='sentiment_neg pol", tokens_istance$polarity, "' title='Polarity of: \"", tokens_istance$polarity, "\"'>", ifelse(i==1,firstup(org_word),org_word), "</span>", sep="")
      }
    } else if(org_word %in% stop_words){
      word_list[i] <- paste("<span class='stopword' title='Stopword'>", ifelse(i==1,firstup(org_word),org_word), "</span>", sep="")
    }
    
    if(!is.na(org_word) && !is.na(word_list_comma[i]) && nchar(org_word) != nchar(word_list_comma[i])){
      word_list[i] <- paste(word_list[i], ",", sep="")
    }
  }
  return(word_list)
}

df_da <- df %>%
# df_da <- df[1:3,] %>% 
  mutate(sentences = str_trim(sentences),
         sentences_full = str_trim(sentences_full)) %>% 
  mutate(word_list = str_split(sentences, " "),
         word_list_comma = str_split(sentences_full, " ")) %>% 
  rowwise() %>% 
  mutate(polarity = as.numeric(
    ifelse(is.na(polarity),
           sum(tokens[tokens$uuid %in% uuid,][tokens[tokens$uuid %in% uuid,]$word %in% word_list,]$polarity),
           polarity))) %>% 
  # mutate(`sentences_sentiment` = as.character(
  # ifelse(sentences_sentiment == "",
  mutate(`sentences_sentiment` = as.character(
    firstup(
      paste0(collapse = " ",
        unlist(
          add_sentiment_labels(word_list, word_list_comma, uuid)
  ))))) %>%
  # ))), sentences_sentiment))) %>%
  select(!c(word_list, word_list_comma))

write.csv(df_da, "data/sentences.csv", row.names = F, fileEncoding = "UTF-8")
saveRDS(df_da, "data/sentences.rds")

# Cleaned sentences ----
cleaned_sentences <- readRDS("data/sentences_cleaned.rds")
cleaned_sentences <- cleaned_sentences %>% 
  mutate(polarity = as.numeric(
    ifelse("polarity" %in% names(cleaned_sentences),
           polarity,
           ""))) %>% 
  mutate(polarity = as.numeric(
    ifelse(is.na(polarity),
           sum(df_da[df_da$uuid %in% uuid,]$polarity),
           polarity)),
    content = paste0(collapse = ". ",
                     unlist(df_da[df_da$uuid %in% uuid,]$sentences_sentiment)
    ))

# Save Cleaned sentences ----
saveRDS(cleaned_sentences, "data/sentences_cleaned.rds")
write.csv(cleaned_sentences, "data/sentences_cleaned.csv", row.names = F, fileEncoding = "UTF-8")


# Unsetting ----
rm(list=ls())
detach("package:udpipe", unload=TRUE)
detach("package:tidytext", unload=TRUE)
detach("package:stringr", unload=TRUE)
detach("package:dplyr", unload=TRUE)
detach("package:stringi", unload=TRUE)
detach("package:SnowballC", unload=TRUE)
