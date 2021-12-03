# Libraries
library(tidyverse)
library(stopwords)
library(SnowballC)
library(tidytext)

# Importing data
df <- read.csv("data/nys_2001-2020_cleaned.csv", encoding = "UTF-8")

# Tokenization, removing stopwords and stemming
berteltorp_stopwords <- read.table("utils/stopord.txt", encoding = "UTF-8") # Stopwords defined by Bertel Torp https://gist.github.com/berteltorp

snowball_stopwords <- stopwords(language = "da", source = "snowball") %>%  # Stopwords defined by snowball (Richard Boulton & Olly Betts) http://snowball.tartarus.org/algorithms/danish/stop.txt
  as.data.frame()
names(snowball_stopwords) <- "V1"

costom_stop_words <- read.table("utils/custom_stopwords.txt", encoding = "UTF-8") # Custom stopwords defined by Max F.H. & Alexander I.D.

stop_words <- full_join(berteltorp_stopwords, snowball_stopwords, by = "V1") %>% # Combine lists
  full_join(costom_stop_words, by = "V1") %>% 
  arrange() %>%  # Sort alphabetically
  rename(word = V1)

tokens <- tibble(df) %>%
  unnest_tokens(word, speech) %>% #tokenization
  anti_join(stop_words, by = "word") %>% #removing stopwords 
  count(year, word, sort = T) # frequency count each year

total_tokens <- tokens %>%
  count(word, sort = T ) # total frequency count

tokens <- tokens %>% 
  rename(n_in_year = n) %>% 
  rowwise() %>% 
  mutate(n_total = total_tokens$n[which(total_tokens$word == word)]) %>% # Add total token usage to each instance of the word in n_total
  arrange(desc(n_total), word, desc(year), desc(n_in_year)) # arrange by largest n_total, word alphabetically, largest year and lastly largest n_in_year.

#unique(tokens$word)

tokens$stemmed <- wordStem(tokens$word, language = "danish") #stemming
tokens$stemmed_hunspell <- hunspell::hunspell_stem(tokens$word, dict = dictionary('da_DK')) # Dictionary based stemming

# Count total usage of stemmed values


# Define master word for stemmed tokens

saveRDS(tokens,"data/tokens.rds")

# hunspell::hunspell_stem(tokens$word, dict = dictionary('da_DK'))
