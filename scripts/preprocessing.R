# Libraries
library(dplyr)
library(SnowballC)
library(tidytext)
library(udpipe)

print("preprocessing.R")

# Importing cleanned speaches ----
# df <- read.csv("data/sentences_cleaned.csv", encoding = "utf-8")
df <- readRDS("data/sentences_cleaned.rds")
article_lib <- readRDS("data/article_library.rds") # File containing UUID, Article name

# Tokenization ----
# Removing stopwords and stemming

## Stopwords ----
stop_words <- readRDS("utils/stopwords.rds")

## Load and tokens ----
if(file.exists("data/tokens.rds")){
  old_tokens <- readRDS("data/tokens.rds")
  
  tokens <- tibble(df) %>% 
    filter(!(uuid %in% unique(old_tokens$uuid)))
} else{
  tokens <- tibble(df)
}

if(nrow(tokens)>0){
  ## Filter stopwords ----
  ### ... and count occurrences by uuid ----
  tokens <- tokens %>% 
    unnest_tokens(word, content) %>% #tokenization
    anti_join(stop_words, by = "word") %>% #removing stopwords 
    count(uuid, word, sort = T) # frequency count each uuid
  
  # Stemming ----
  tokens$stemmed <- wordStem(tokens$word, language = "danish") # stemming
  # tokens$stemmed_hunspell <- hunspell::hunspell_stem(tokens$word, dict = dictionary('da_DK')) # Dictionary based stemming
  # hunspell::hunspell_stem(tokens$word, dict = dictionary('da_DK'))
  
  
  # All tokens, filtered
  # tokens <- readRDS("data/tokens.rds")
  tokens <- tokens %>% 
    rowwise() %>% 
    mutate(title = paste(article_lib$title[min(which(uuid == article_lib$uuid))], " (", uuid, ")", sep = ""),
           date_updated_at = article_lib$date_updated_at[min(which(uuid == article_lib$uuid))],
           date_published_at = article_lib$date_published_at[min(which(uuid == article_lib$uuid))]
    )
  
  # # Lemmatization ----
  # model <- udpipe_download_model(language = "danish")
  # if(!model$download_failed){
  #   ud <- udpipe_load_model(model)
  #   
  #   ## Tokenise, Tag and Dependency Parsing Annotation. Output is in CONLL-U format.
  #   txt <- df$content
  #   # txt <- paste(df$content, collapse = ' ')
  #   names(txt) <- df$uuid
  #   lemmi <- udpipe(txt, object = ud)
  #   lemmi <- udpipe(data.frame(doc_id = names(txt), text = txt, stringsAsFactors = FALSE), 
  #               object = ud)
  #   lemmi <- udpipe(strsplit(txt, "[[:space:][:punct:][:digit:]]+"), 
  #               object = ud)
  #   lemmi <- lemmi[colSums(!is.na(lemmi)) > 0]
  # }
  # 
  # saveRDS(lemmi, "data/lemma.rds")
  # lemma <- readRDS("data/lemma.rds")
  # 
  # ## Implement legitimized references into tokens ----
  # tokens <- tokens %>%
  #   rowwise() %>% 
  #   mutate(lemma = paste(unique(lemmi[lemmi$token == word,]$lemma), collapse=","))
  # 
  # ## Count total occurrences of limmitized words ----
  # total_tokens <- tokens %>%
  #   group_by(lemma) %>%
  #   summarise(n_lemma_total = sum(n_in))
  # 
  # tokens <- tokens %>%
  #   left_join(total_tokens, by="lemma") %>% 
  #   arrange(desc(n_lemma_total), word, desc(n_stem_total), desc(n_total)) # arrange
  
  # Combine token lists ----
  if(exists("old_tokens")){
    tokens <- tokens %>% 
      rename(n_in = n) %>% 
      full_join(old_tokens) %>% 
      select(!c(n_total, n_stem_total, n_stem)) %>% 
      rename(n = n_in)
  }
  
  ### ... and count total occurrences ----
  total_tokens <- tokens %>%
    group_by(word) %>%
    summarise(n_total = sum(n))
  
  ### ... and count occurrences in subject ----
  tokens <- tokens %>% 
    left_join(total_tokens, by="word") %>% 
    rename(n_in = n) %>% 
    arrange(desc(n_total), word) # arrange
  
  ## Count total occurrences of stemmed words ----
  # Count total usage of stemmed values
  total_tokens <- tokens %>%
    group_by(stemmed) %>%
    summarise(n_stem_total = sum(n_in))
  
  tokens <- tokens %>%
    left_join(total_tokens, by="stemmed") %>% 
    arrange(desc(n_stem_total), word, desc(n_total)) # arrange
  
  if(exists("old_tokens")){
    tokens <- tokens %>%
      left_join(old_tokens)
  }
  
  # Save tokens ----
  saveRDS(tokens,"data/tokens.rds")
  write.csv(tokens, "data/tokens.csv", row.names = F, fileEncoding = "UTF-8")
}

# Unsetting ----
rm(list=ls())
detach("package:dplyr", unload=TRUE)
detach("package:tidytext", unload=TRUE)
detach("package:udpipe", unload=TRUE)
