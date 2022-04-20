## Library for lemmatizing sentences
library(udpipe)
library(tidytext)
library(stringr)
library(dplyr)

print("sentiment_sentences.R")

# Validate if update is needed ----
new_entries <- readRDS("data/new_entries.rds")

if(new_entries>0){
  # sentences
  # df <- read.csv("data/sentences.csv")
  df <- readRDS("data/sentences.rds")
  
  tokens <- readRDS("data/tokens.rds") %>%  # All tokens, filtered
    filter(polarity != 0)
  
  
  # Check if sentiment exists ----
  # if(!is.null(df$polarity)){
  #   old_df <- df
  #   df_da <- df %>% 
  #     filter(is.null(polarity)) %>% 
  #     rowwise() %>% 
  #     mutate(polarity = sum(tokens[tokens$uuid %in% uuid,][tokens[tokens$uuid %in% uuid,]$word %in% str_split(sentences, " ")[[1]],]$polarity)) %>% 
  #     group_by(uuid, sentences, sentences_full, polarity) %>% 
  #     full_join(old_df)
  # } else{
    df_da <- df %>% 
      rowwise() %>% 
      mutate(polarity = sum(tokens[tokens$uuid %in% uuid,][tokens[tokens$uuid %in% uuid,]$word %in% str_split(sentences, " ")[[1]],]$polarity))
  # }
  
    # mutate(sentiment = sum(tokens[tokens$uuid %in% "00b26860-e4ce-4f5c-a728-830bf8a4c056",][tokens[tokens$uuid %in% "00b26860-e4ce-4f5c-a728-830bf8a4c056",]$word %in% str_split(" når jeg laver fejl på mine heste så lærer jeg af det så jeg ikke laver dem i næste runde siger hun", " ")[[1]],]$polarity))

  # # Lemmatizing sentences
  # df_lemmatized <- udpipe(df$sentences, "danish")
  # 
  # # Correcting sentence_id to match rownumber for sentences in df
  # df_lemmatized$sentence_id <- as.numeric(str_replace(df_lemmatized$doc_id, "doc", ""))
  # 
  # sentence_sentiment <- function(df, df_lemmatized){
  #   dk_sentiment <- read.csv("https://raw.githubusercontent.com/dsldk/danish-sentiment-lexicon/main/2a_fullform_headword_polarity.csv", encoding = "UTF-8", header = FALSE, sep = "\t")
  #   names(dk_sentiment) <- c("word_form", "headword", "homograph_number", "POS", "DDO_headword_ID", "polarity")
  #   dk_sentiment <- dk_sentiment %>% 
  #     select(headword, word_form, polarity) %>% 
  #     unique()
  #   df_match <- df_lemmatized %>% 
  #     inner_join(dk_sentiment, by=c("lemma" = "headword")) %>% 
  #     inner_join(dk_sentiment, by=c("lemma" = "word_form")) %>% 
  #     inner_join(dk_sentiment, by=c("token" = "headword")) %>% 
  #     inner_join(dk_sentiment, by=c("token" = "word_form")) %>% 
  #     summarise(
  #       sentence_id = sentence_id,
  #       value = (polarity.x + polarity.y + polarity.x.x + polarity.y.y)/4
  #     ) %>% 
  #     group_by(sentence_id) %>% 
  #     summarise(
  #       sentence_id = sentence_id,
  #       polarity = mean(value)
  #     )
  #   df$polarity <- 0
  #   df$polarity[df_match$sentence_id] <- df_match$polarity
  #   return(df)
  # }
  # 
  # df_da <- sentence_sentiment(df, df_lemmatized)
  
  write.csv(df_da, "data/sentences.csv", row.names = F, fileEncoding = "UTF-8")
  saveRDS(df_da, "data/sentences.rds")
}