# Libraries
library(stringr)
library(dplyr)

# Cleaning function
clean_speech <- function(x) {
  x %>%
    str_replace_all('<.*?>|&([a-z0-9]+|#[0-9]{1,6}|#x[0-9a-f]{1,6});', " ") %>% # Replace html whitespace code with space
    str_replace_all("\\\n", " ") %>% # removes linebreaks
    str_replace_all("[^[:alnum:]]", " ") %>% # removes special characters
    str_to_lower() %>% # Converts to lower case
    gsub(pattern = '[[:digit:]]+', replacement =  '') %>%  # removes numbers
    # gsub(pattern = "\\W*\\b\\w\\b\\W*", replacement = "") %>%  # Remove single character 
    str_squish() # Removes leading, trailing and middle whitespace
}

clean_sentences <- function(x) {
  x %>%
    str_replace_all('<.*?>|&([a-z0-9]+|#[0-9]{1,6}|#x[0-9a-f]{1,6});', " ") %>% # Replace html whitespace code with space
    str_replace_all("\\\n", " ") %>% # removes linebreaks
    str_replace_all("[^[:alnum:].']", " ") %>% # removes special characters except .,
    gsub(pattern = '[[:digit:]]+', replacement =  '') %>%  # removes numbers
    # gsub(pattern = "\\W*\\b\\w\\b\\W*", replacement = "") %>%  # Remove single character 
    str_to_lower() %>% # Converts to lower case
    str_squish() # Removes leading, trailing and middle whitespace
}

clean_sentences_less <- function(x) {
  x %>%
    str_replace_all('<.*?>|&([a-z0-9]+|#[0-9]{1,6}|#x[0-9a-f]{1,6});', " ") %>% # Replace html whitespace code with space
    str_replace_all("\\\n", " ") %>% # removes linebreaks
    str_replace_all("[^[:alnum:].,']", " ") %>% # removes special characters except .,
    gsub(pattern = '[[:digit:]]+', replacement =  '') %>%  # removes numbers
    # gsub(pattern = "\\W*\\b\\w\\b\\W*", replacement = "") %>%  # Remove single character 
    str_to_lower() %>% # Converts to lower case
    str_squish() # Removes leading, trailing and middle whitespace
}

# function for making long data.frame i.e., a row per sentence
unnest_sentences <- function(x) {
  y <- data.frame(uuid=c(), sentences=c(), sentences_full = c())
  for (i in 1:length(x$sentence)) {
    y <- rbind(y, data.frame(uuid=rep(x$X1[i], length(x$sentence[[i]])), sentences=x$sentence[[i]], sentences_full=x$sentence_full[[i]]))
  }
  y
}

# Importing data
df <- readRDS("data/article_library.rds")

# Cleaning sentences i.e., leaving in the . (dots) for later separation
sentences <- data.frame(cbind(df$uuid, clean_sentences(df$content), clean_sentences_less(df$content)))

# Grouping speaches by id and separating into sentences by . (dots)
sentences <- sentences %>% 
  group_by(X1) %>% 
  summarize(
    sentence = strsplit(X2, "[.]"),
    sentence_full = strsplit(X3, "[.]")
  )

sentences <- unnest_sentences(sentences)
saveRDS(sentences, "data/sentences.rds")
write.csv(sentences, "data/sentences.csv", row.names = F, fileEncoding = "UTF-8")

# Cleaning speech of each year
df$content <- clean_speech(df$content)
saveRDS(df, "data/sentences_cleaned.rds")
write.csv(df, "data/sentences_cleaned.csv", row.names = F, fileEncoding = "UTF-8")
