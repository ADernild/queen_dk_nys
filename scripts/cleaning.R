
# Libraries
library(stringr)
library(dplyr)

print("cleaning.R")

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

# # Importing data ----
df <- readRDS("data/article_library.rds")
# if(file.exists("data/sentences_cleaned.rds")){
#   sentences_cleaned <- readRDS("data/sentences_cleaned.rds")
#   ## Load senteces if it exists ----
#   old_sentences <- readRDS("data/sentences.rds")
#   if(nrow(old_sentences) != nrow(df)){
#     # Cleaning sentences i.e., leaving in the . (dots) for later separation
#     sentences <- data.frame(cbind(df$uuid, clean_sentences(df$content), clean_sentences_less(df$content)))
#     
#     # Grouping speaches by id and separating into sentences by . (dots)
#     sentences <- sentences %>% 
#       filter(!(X1 %in% old_sentences$uuid)) %>% 
#       group_by(X1) %>% 
#       summarize(
#         sentence = strsplit(X2, "[.]"),
#         sentence_full = strsplit(X3, "[.]")
#       )
#     
#     sentences <- unnest_sentences(sentences)
#     
#     sentences$polarity <- 0
#     sentences <- sentences %>% 
#       full_join(old_sentences)
#     
#     # Cleaning speech of each year
#     new_df_row <- df %>% 
#       filter(!(uuid %in% sentences_cleaned$uuid)) %>% 
#       rowwise() %>% 
#       mutate(content = clean_speech(content))
#     
#     df <- sentences_cleaned %>% 
#       full_join(new_df_row)
#   } else{
#     sentences <- old_sentences
#   }
# } else {
  # Create, if not exists ----
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
  
  # Cleaning speech of each year
  df$content <- clean_speech(df$content)
# }

# Save sentences ----
saveRDS(sentences, "data/sentences.rds")
write.csv(sentences, "data/sentences.csv", row.names = F, fileEncoding = "UTF-8")

saveRDS(df, "data/sentences_cleaned.rds")
write.csv(df, "data/sentences_cleaned.csv", row.names = F, fileEncoding = "UTF-8")

# Unsetting ----
rm(list=ls())
detach("package:stringr", unload=TRUE)
detach("package:dplyr", unload=TRUE)
