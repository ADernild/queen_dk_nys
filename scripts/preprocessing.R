# Libraries
library(tidyverse)
library(stopwords)
library(SnowballC)
library(tidytext)
library(udpipe)

# Importing cleanned speaches ----
# df <- read.csv("data/sentences_cleaned.csv", encoding = "utf-8")
df <- readRDS("data/sentences_cleaned.rds")
article_lib <- readRDS("data/article_library.rds") # File containing UUID, Article name

# Tokenization ----
# Removing stopwords and stemming

## Stopwords ----
### ... from stopwords defined by Bertel Torp ----
berteltorp_stopwords <- read.table("https://gist.githubusercontent.com/berteltorp/0cf8a0c7afea7f25ed754f24cfc2467b/raw/305d8e3930cc419e909d49d4b489c9773f75b2d6/stopord.txt", encoding = "UTF-8") # Stopwords defined by Bertel Torp https://gist.github.com/berteltorp

### ... from stopwords defined by snowball ----
snowball_stopwords <- stopwords(language = "da", source = "snowball") %>%  # Stopwords defined by snowball (Richard Boulton & Olly Betts) http://snowball.tartarus.org/algorithms/danish/stop.txt
  as.data.frame()
names(snowball_stopwords) <- "V1"

### ... from stopwords defined by Max Festersen Hansen & Alexander Ibsen Dernild ----
#custom_stop_words <- read.table("utils/custom_stopwords.txt", encoding = "UTF-8") # Custom stopwords defined by Max F.H. & Alexander I.D.

## Combining stopwords ----
stop_words <- full_join(berteltorp_stopwords, snowball_stopwords, by = "V1") %>% # Combine lists
  # full_join(custom_stop_words, by = "V1") %>% 
  arrange(V1) %>%  # Sort alphabetically
  rename(word = V1)

## Filter stopwords ----
### ... and count occurrences by uuid ----
tokens <- tibble(df) %>%
  unnest_tokens(word, content) %>% #tokenization
  anti_join(stop_words, by = "word") %>% #removing stopwords 
  count(uuid, word, sort = T) # frequency count each uuid

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
### ... and count total occurrences ----
total_tokens <- tokens %>%
  group_by(word) %>%
  summarise(n_total = sum(n))

tokens <- tokens %>% 
  left_join(total_tokens, by="word") %>% 
  rename(n_in = n) %>% 
  arrange(desc(n_total), word) # arrange

# Stemming ----
tokens$stemmed <- wordStem(tokens$word, language = "danish") # stemming
# tokens$stemmed_hunspell <- hunspell::hunspell_stem(tokens$word, dict = dictionary('da_DK')) # Dictionary based stemming
# hunspell::hunspell_stem(tokens$word, dict = dictionary('da_DK'))

## Count total occurrences of stemmed words ----
# Count total usage of stemmed values
total_tokens <- tokens %>%
  group_by(stemmed) %>%
  summarise(n_stem_total = sum(n_in))

tokens <- tokens %>%
  left_join(total_tokens, by="stemmed") %>% 
  arrange(desc(n_stem_total), word, desc(n_total)) # arrange

}

# tokens <- readRDS("data/tokens.rds") # All tokens, filtered
tokens <- tokens %>% 
  mutate(title = paste(article_lib$title[min(which(uuid == article_lib$uuid))], " (", uuid, ")", sep = ""),
         date_updated_at = article_lib$date_updated_at[min(which(uuid == article_lib$uuid))],
         date_published_at = article_lib$date_published_at[min(which(uuid == article_lib$uuid))]
  )

# Save tokens ----
saveRDS(tokens,"data/tokens.rds")
