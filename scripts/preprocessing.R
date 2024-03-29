# Libraries
library(tidyverse)
library(stopwords)
library(SnowballC)
library(tidytext)
library(udpipe)
# library(hunspell)

setwd("../")

# Importing cleanned speaches ----
last_year <- as.integer(format(Sys.Date(), "%Y")) - 1 # Last year i.e., the year of the lastest speech
df <- read.csv(paste0("data/nys_1972-", last_year, "_cleaned.csv"), encoding = "UTF-8")


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
custom_stop_words <- read.table("utils/custom_stopwords.txt", encoding = "UTF-8") # Custom stopwords defined by Max F.H. & Alexander I.D.

## Combining stopwords ----
stop_words <- full_join(berteltorp_stopwords, snowball_stopwords, by = "V1") %>% # Combine lists
  full_join(custom_stop_words, by = "V1") %>% 
  arrange(V1) %>%  # Sort alphabetically
  rename(word = V1)

## Filter stopwords ----
### ... and count occurrences by year ----
tokens <- tibble(df) %>%
  unnest_tokens(word, speech) %>% #tokenization
  anti_join(stop_words, by = "word") %>% #removing stopwords 
  count(year, word, sort = T) # frequency count each year

### ... and count total occurrences ----
total_tokens <- tokens %>%
  group_by(word) %>%
  summarise(n_total = sum(n))

tokens <- tokens %>% 
  left_join(total_tokens, by="word") %>% 
  rename(n_in_year = n) %>% 
  arrange(desc(n_total), word, desc(year), desc(n_in_year)) # arrange by largest n_total, word alphabetically, largest year and lastly largest n_in_year.

# Stemming ----
tokens$stemmed <- wordStem(tokens$word, language = "danish") # stemming
# tokens$stemmed_hunspell <- hunspell::hunspell_stem(tokens$word, dict = dictionary('da_DK')) # Dictionary based stemming
# hunspell::hunspell_stem(tokens$word, dict = dictionary('da_DK'))

## Count total occurrences of stemmed words ----
# Count total usage of stemmed values
total_tokens <- tokens %>%
  group_by(stemmed) %>%
  summarise(n_stem_total = sum(n_in_year))

tokens <- tokens %>%
  left_join(total_tokens, by="stemmed") %>% 
  arrange(desc(n_stem_total), word, desc(n_total), desc(year), desc(n_in_year)) # arrange by largest n_total, word alphabetically, largest year and lastly largest n_in_year.

# Lemmatization ----
lemmi <- udpipe(df$speech, "danish")
lemmi <- lemmi[colSums(!is.na(lemmi)) > 0]

saveRDS(lemmi, "data/lemma.rds")

## Implement legitimized references into tokens ----
tokens <- tokens %>%
  rowwise() %>% 
  mutate(lemma = paste(unique(lemmi[lemmi$token == word,]$lemma), collapse=","))

## Count total occurrences of limmitized words ----
total_tokens <- tokens %>%
  group_by(lemma) %>%
  summarise(n_lemma_total = sum(n_in_year))

tokens <- tokens %>%
  left_join(total_tokens, by="lemma") %>% 
  arrange(desc(n_lemma_total), word, desc(n_stem_total), desc(n_total), desc(year), desc(n_in_year)) # arrange by largest n_lemma_total, word alphabetically, largest n_stem_total, largest year and lastly largest n_in_year.


# Save tokens ----
saveRDS(tokens,"data/tokens.rds")

# English speeches with a function ------
df_en <- read.csv(paste0("data/nys_2010-", last_year, "_eng_cleaned.csv"), encoding = "UTF-8")

## English stopwords ----
stop_words_en <- read.csv("utils/custom_stopwords_en.txt", header=F) %>% 
  rbind(data.frame(V1 = stopwords::stopwords(language = "en", source = "snowball"))) %>% 
  rename(word = V1)

preprocess <- function(df, stop_words, lang) {
  tokens <- tibble(df_en) %>% 
    unnest_tokens(word, speech) %>% 
    anti_join(stop_words, by = "word") %>% 
    count(year, word, sort = T)
  
  total_tokens <- tokens %>% 
    group_by(word) %>% 
    summarise(n_total = sum(n))
  
  tokens <- tokens %>% 
    left_join(total_tokens, by = "word") %>%
    rename(n_in_year = n) %>% 
    arrange(desc(n_total), word, desc(year), desc(n_in_year))
  
  tokens$stemmed <- wordStem(tokens$word, language = lang)

  total_tokens <- tokens %>%
    group_by(stemmed) %>%
    summarise(n_stem_total = sum(n_in_year))

  tokens <- tokens %>%
    left_join(total_tokens, by="stemmed") %>%
    arrange(desc(n_stem_total), word, desc(n_total), desc(year), desc(n_in_year)) # arrange by largest n_total, word alphabetically, largest year and lastly largest n_in_year.

  # Lemmatization ----
  lemmi <- udpipe(df$speech, lang)
  lemmi <- lemmi[colSums(!is.na(lemmi)) > 0]

  ## Implement legitimized references into tokens ----
  tokens <- tokens %>%
    rowwise() %>%
    mutate(lemma = paste(unique(lemmi[lemmi$token == word,]$lemma), collapse=","))

  ## Count total occurrences of limmitized words ----
  total_tokens <- tokens %>%
    group_by(lemma) %>%
    summarise(n_lemma_total = sum(n_in_year))

  tokens <- tokens %>%
    left_join(total_tokens, by="lemma") %>%
    arrange(desc(n_lemma_total), word, desc(n_stem_total), desc(n_total), desc(year), desc(n_in_year)) # arrange by largest n_lemma_total, word alphabetically, largest n_stem_total, largest year and lastly largest n_in_year.
  
  list(tokens = tokens, lemma = lemmi)
}

preprocessed_en <- preprocess(df_en, stop_words_en, "english")
saveRDS(preprocessed_en$tokens, "data/tokens_en.rds")
saveRDS(preprocessed_en$lemma, "data/lemma_en.rds")
