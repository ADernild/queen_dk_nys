## Library for lemmatizing sentences
library(udpipe)

# Sentiment lookup table
dk_sentiment <- read.csv("https://raw.githubusercontent.com/dsldk/danish-sentiment-lexicon/main/2a_fullform_headword_polarity.csv", encoding = "UTF-8", header = FALSE, sep = "\t")
names(dk_sentiment) <- c("word_form", "headword", "homograph_number", "POS", "DDO_headword_ID", "polarity_label_headword")

# sentences
df <- read.csv("data/nys_sentences.csv")

# Lemmatizing sentences
df_lemmatized <- udpipe(df$sentences, "danish")

# Correcting sentence_id to match rownumber for sentences in df
df_lemmatized$sentence_id <- as.numeric(str_replace(df_lemmatized$doc_id, "doc", ""))

# Selecting needed columns in lookup table and deleting duplicates
dk_sentiment_min <- dk_sentiment %>% 
  select(headword, word_form, polarity_label_headword) %>% 
  unique()

# Limiting lemmatized sentences to headwords that are in lookup table
df_matched_headword <- df_lemmatized[df_lemmatized$lemma %in% dk_sentiment_min$headword,]

# Limiting lemmatized sentences to word_form that are in lookup table
df_matched_wordform <- df_lemmatized[df_lemmatized$lemma %in% dk_sentiment_min$word_form,]

# Limiting lookup table to words in sentences, and setting polarity of words with multiple polarities to mean
dk_sentiment_min_headword <- dk_sentiment_min[dk_sentiment_min$headword %in% df_lemmatized$lemma,] %>% 
  group_by(headword) %>% 
  summarise(
    polarity = mean(polarity_label_headword)
  )

# Limiting lookup table to words in sentences, and setting polarity of words with multiple polarities to mean
dk_sentiment_min_wordform <- dk_sentiment_min[dk_sentiment_min$word_form %in% df_lemmatized$lemma,] %>% 
  group_by(word_form) %>% 
  summarise(
    polarity = mean(polarity_label_headword)
  )


# Initiating empty list
polarity_list <- c()

# looping through words in sentences that are matched and appending their polarity to polarity list
for (row in 1:nrow(df_matched_headword)) {
  polarity_list <- c(polarity_list, dk_sentiment_min_headword$polarity[dk_sentiment_min_headword$headword == df_matched_headword$lemma[row]])
}

# Making polarity list a column in lemmatized words from sentences
df_matched_headword$polarity <- polarity_list

# Initiating empty list
polarity_list <- c()

# looping through words in sentences that are matched and appending their polarity to polarity list
for (row in 1:nrow(df_matched_wordform)) {
  polarity_list <- c(polarity_list, dk_sentiment_min_wordform$polarity[dk_sentiment_min_wordform$word_form == df_matched_wordform$lemma[row]])
}

# Making polarity list a column in lemmatized words from sentences
df_matched_wordform$polarity <- polarity_list

# Calculating mean polarity of headwords for each sentence
polarity_headword <- df_matched_headword %>% 
  group_by(sentence_id) %>% 
  summarise(
    polarity = mean(polarity)
  )

# Calculating mean polarity of word forms for each sentence
polarity_wordform <- df_matched_wordform %>% 
  group_by(sentence_id) %>% 
  summarise(
    polarity = mean(polarity)
  )

# Joining the respective polarity scores
polarity <- full_join(polarity_headword, polarity_wordform)

# In cases where a sentences has headword and wordform polarity polarity is set to mean
polarity <- polarity %>% 
  group_by(sentence_id) %>% 
  summarise(
    polarity = mean(polarity)
  )

# Creating new column in sentences
df$polarity <- 0

# Adding mean polarity to sentences with matched words
df$polarity[polarity$sentence_id] <- polarity$polarity

write.csv(df, "data/nys_sentences.csv", row.names = F)
