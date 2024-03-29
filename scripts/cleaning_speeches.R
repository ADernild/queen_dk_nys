# Libraries
library(stringr)
library(dplyr)

setwd("../")

# Cleaning function
clean_speech <- function(x) {
  x %>%
    str_replace_all("\\\n", " ") %>% # removes linebreaks
    str_replace_all("[^[:alnum:]]", " ") %>% # removes special characters
    str_to_lower() %>% # Converts to lower case
    str_squish() # Removes leading, trailing and middle whitespace
}

clean_sentences <- function(x) {
  x %>%
    str_replace_all("\\\n", " ") %>% # removes linebreaks
    str_replace_all("[^[:alnum:].']", " ") %>% # removes special characters except .,
    str_to_lower() %>% # Converts to lower case
    str_squish() # Removes leading, trailing and middle whitespace
}

clean_sentences_less <- function(x) {
  x %>%
    str_replace_all("\\\n", " ") %>% # removes linebreaks
    str_replace_all("[^[:alnum:].,']", " ") %>% # removes special characters except .,
    str_to_lower() %>% # Converts to lower case
    str_squish() # Removes leading, trailing and middle whitespace
}

new_year_en <- function(x){
  x %>% 
    str_replace_all("new year", "newyear")
}

# Importing data
last_year <- as.integer(format(Sys.Date(), "%Y")) - 1 # Last year i.e., the year of the lastest speech
df <- read.csv(paste0("data/new_year_speeches_1972-", last_year, ".csv"), encoding = "UTF-8")
df_2 <- read.csv(paste0("data/new_year_speeches_eng_2010-", last_year, ".csv"), encoding = "UTF-8")
# Cleaning sentences i.e., leaving in the . (dots) for later separation
sentences <- data.frame(cbind(df$year, clean_sentences(df$speech), clean_sentences_less(df$speech)))
sentences_eng <- data.frame(cbind(df_2$year, new_year_en(clean_sentences(df_2$speech)), clean_sentences_less(df_2$speech)))
# Grouping speaches by year and separating into sentences by . (dots)
sentences <- sentences %>% 
  group_by(X1) %>% 
  summarize(
    sentence = strsplit(X2, "[.]"),
    sentence_full = strsplit(X3, "[.]")
  )

sentences_eng <- sentences_eng %>% 
  group_by(X1) %>% 
  summarize(
    sentence = strsplit(X2, "[.]"),
    sentence_full = strsplit(X3, "[.]")
  )



# function for making long data.frame i.e., a row per sentence
unnest_sentences <- function(x) {
  y <- data.frame(years=c(), sentences=c(), sentences_full = c())
  for (i in 1:length(x$sentence)) {
    y <- rbind(y, data.frame(years=rep(x$X1[i], length(x$sentence[[i]])), sentences=x$sentence[[i]], sentences_full=x$sentence_full[[i]]))
  }
  y
}

sentences <- unnest_sentences(sentences)
write.csv(sentences, "data/nys_sentences.csv", row.names = F, fileEncoding = "UTF-8")

sentences_eng <- unnest_sentences(sentences_eng)
write.csv(sentences_eng, "data/nys_sentences_eng.csv", row.names = F, fileEncoding = "UTF-8")
# Cleaning speech of each year
df$speech <- clean_speech(df$speech)
write.csv(df, paste0("data/nys_1972-", last_year, "_cleaned.csv"), row.names = F, fileEncoding = "UTF-8")

# Cleaning speech of each year
df_2$speech <- clean_speech(df_2$speech) %>% new_year_en()
write.csv(df_2, paste0("data/nys_2010-", last_year, "_eng_cleaned.csv"), row.names = F, fileEncoding = "UTF-8")
