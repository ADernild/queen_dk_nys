## Library for lemmatizing sentences
library(udpipe)
library(tidytext)
library(stringr)
library(dplyr)

setwd("../")

# sentences
df <- read.csv("data/nys_sentences.csv")
df_eng <- read.csv("data/nys_sentences_eng.csv")

# Lemmatizing sentences
df_lemmatized <- udpipe(df$sentences, "danish")
df_eng_lemmatized <- udpipe(df_eng$sentences, "english")

# Correcting sentence_id to match rownumber for sentences in df
df_lemmatized$sentence_id <- as.numeric(str_replace(df_lemmatized$doc_id, "doc", ""))
df_eng_lemmatized$sentence_id <- as.numeric(str_replace(df_eng_lemmatized$doc_id, "doc", ""))

sentence_sentiment <- function(df, df_lemmatized, lang="en"){
  if(lang == "en"){
    afinn <- get_sentiments("afinn")
    df_match <- df_lemmatized %>% 
      inner_join(afinn, by=c("lemma" = "word")) %>% 
      inner_join(afinn, by=c("token" = "word")) %>% 
      summarise(
        sentence_id = sentence_id,
        value = (value.x + value.y)/2
      ) %>% 
      group_by(sentence_id) %>% 
      summarise(
        sentence_id = sentence_id,
        polarity = mean(value)
      )
    df$polarity <- 0
    df$polarity[df_match$sentence_id] <- df_match$polarity
    return(df)
  }else if(lang == "da"){
    dk_sentiment <- read.csv("https://raw.githubusercontent.com/dsldk/danish-sentiment-lexicon/main/2a_fullform_headword_polarity.csv", encoding = "UTF-8", header = FALSE, sep = "\t")
    names(dk_sentiment) <- c("word_form", "headword", "homograph_number", "POS", "DDO_headword_ID", "polarity")
    dk_sentiment <- dk_sentiment %>% 
      select(headword, word_form, polarity) %>% 
      unique()
    df_match <- df_lemmatized %>% 
      inner_join(dk_sentiment, by=c("lemma" = "headword")) %>% 
      inner_join(dk_sentiment, by=c("lemma" = "word_form")) %>% 
      inner_join(dk_sentiment, by=c("token" = "headword")) %>% 
      inner_join(dk_sentiment, by=c("token" = "word_form")) %>% 
      summarise(
        sentence_id = sentence_id,
        value = (polarity.x + polarity.y + polarity.x.x + polarity.y.y)/4
      ) %>% 
      group_by(sentence_id) %>% 
      summarise(
        sentence_id = sentence_id,
        polarity = mean(value)
      )
    df$polarity <- 0
    df$polarity[df_match$sentence_id] <- df_match$polarity
    return(df)
  }
}

df_eng <- sentence_sentiment(df_eng, df_eng_lemmatized, lang="en")
df_da <- sentence_sentiment(df, df_lemmatized, lang = "da")


write.csv(df_da, "data/nys_sentences.csv", row.names = F)
write.csv(df_eng, "data/nys_sentences_eng.csv", row.names = F)
