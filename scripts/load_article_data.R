print("Setting libraries...")

library(plyr) # For data manipulation
library(dplyr) # For data manipulation
library(tidyr) # For data manipulation
library(DT) # For table visualizations
library(plotly) # for interactive plots
library(LDAvis) # For topic models
library(stm) # for stm models
library(highcharter) # for plot display
library(leaflet)
library(stringr)
library(wordcloud2) # Two create wordclouds
library(colorBlindness) # For colors

print("Loading article data...")

# Load and format article data ---------------------------------------------
## Load data ---------------------------------------------------------------
tokens <<- readRDS("data/tokens.rds") # All tokens, filtered
lda_model <<- readRDS("data/lda_model.rds") # LDA model
stm_model_da <<- readRDS("data/stm_model.rds") # STM model
thoughts <<- readRDS("data/thoughts.rds") # sentences belonging to topics (topic proportion 45%)
# lemma <- readRDS("data/lemma.rds") # All lematized values unfiltered
sentiment <<- readRDS("data/sentiments.rds") # Sentiment for year
# countries <- readRDS("data/country_speech.rds") # Countries, country code, lat, lon and # mentions
# geojson <- readRDS("data/countries.rds") # Library containing geographic information
article_lib <<- readRDS("data/article_library.rds") # File containing UUID, Article name
# article_lib <<- readRDS("data/article_library.rds") %>%
#   .[1,]
cleaned_sentences <<- readRDS("data/sentences_cleaned.rds")


## Formatting data ---------------------------------------------------------
print("Formatting data...")

### Words ----
n_dist_t_headword <<- nrow(distinct(tokens, stemmed)) # Number of distinct headwords
words_all <<-  unique(tokens$word) %>% sort()
words_tokens_all <<- tokens %>%
  mutate(wordisnum = as.integer(suppressWarnings(ifelse(!is.na(as.numeric(stemmed)),1,0)))) %>% 
  arrange(wordisnum, stemmed) %>% 
  .$stemmed %>% 
  unique()
words_count_unique <<- length(words_all)
most_common <<- max(tokens$n_stem_total)
most_common_any_year <<- max(tokens$n_stem)
number_of_rarity <<- length(unique(arrange(tokens, desc(n_stem_total))$n_stem_total))
named_id <- article_lib$uuid
names(named_id) <- article_lib$title
named_id <<- named_id
topic_frame <<- data.frame(topic = names(thoughts$index)) %>% 
  mutate(uuid = thoughts$uuid[topic],
         docs = thoughts$docs[topic]) %>% 
  rowwise() %>% 
  mutate(doc_len = length(docs))
n_unique_sentences <<- sum(topic_frame$doc_len)
sections <<- article_lib$section %>% 
  unique() %>% 
  sort()
authors <<- article_lib$authors %>% 
  str_split(", ") %>% 
  unlist() %>% 
  unique() %>% 
  .[. != ""] %>% 
  sort()
authors <<-  c("Not set", authors)
locations <<- article_lib$location %>% 
  unique() %>% 
  sort()
